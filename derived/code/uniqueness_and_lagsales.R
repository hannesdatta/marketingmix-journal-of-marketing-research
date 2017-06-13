#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|

# Goal: Define uniqueness measures per SKU and date

require(data.table)
require(compiler)

#################################################################
### LOAD RAW DATA AND PREPARE DATA FILES INTO datlist_final
#################################################################

# Load merged and categorized data sets
load('..\\temp\\categorized.RData')
load('..\\temp\\attributes.RData')

# Remove 'non-used' parts of the dataset (e.g., GfK 2012, if GfK 2015 data is being used)
for (i in 1:length(datlist_final)) {
	datlist_final[[i]]<-datlist_final[[i]][which(used==T)]
	}


#################################################################
### EXECUTING CREATION OF ATTRIBUTE-BASED MEASURES
#################################################################

source('proc_functions.R')

# Put all data in a list (per category, and country)
# Creates all attribute-based measures
skus_by_date_list <- NULL


for (i in 1:length(datlist_final)) {
	cat('=============================================================\n')
	cat(names(datlist_final)[i],fill=T)
	cat('=============================================================\n')
	assign("last.warning", NULL, envir = baseenv())
	
	attrib = grep('attr_',colnames(attribs[[i]]),value=T)
	
	setkey(datlist_final[[i]], category, country, brand, model, date)
	setkey(attribs[[i]], country, brand, model)
	
	skus_by_date = datlist_final[[i]][, list(t_sales_units=sum(sales_units), 
											 t_value_sales = sum(sales_units*as.numeric(price_lc)),
											 t_value_sales_usd = sum(sales_units*as.numeric(price_usd)),
											 t_numdist = sum(sales_units*numeric_distribution)/sum(sales_units),
											 t_wdist = sum(sales_units*weighted_distribution)/sum(sales_units)), 
											 by=c('category', 'country', 'market_id', 'brand', 'model', 'date')]
	
	# If a product is not sold, kick it out from this list
	skus_by_date <- skus_by_date[!t_sales_units==0]
	
	skus_by_date <- merge(skus_by_date, attribs[[i]], by=c('country','market_id', 'brand','model'),all.x=T,all.y=F)[order(country, date, brand, model)]
		
	##############################################################################################
	# Create weighted past sales metric for every SKU (used for weighing the promotion metrics)  #
	##############################################################################################
		cat('Compute past sales metric for re-weighing.\n')
		
		# metrics that are aggregated to a brand-level will be
		# weighted with each SKU's lagged sales (t-2, t-1, t).
		
		# question: are NAs counted as ZEROS, or as NA's (?)
		# -> I decide they will be treated as NAs.
	
		t_lags = c(-2,-1, 0)
		t_lag_names = paste0('lag_date', gsub('[-]', 'min', as.character(t_lags)))
		
		tmpdates <- skus_by_date[, list(N=.N),by=c('date')][,N:=NULL]
		
		lags=sapply(t_lags, function(t_lag, x) {
			m.date=as.POSIXlt(x)
			m.date$mon=m.date$mon+t_lag
			return(as.character(m.date))
			}, x=tmpdates$date)
		
		for (t_lag in seq(along=t_lags)) {
			tmpdates[, t_lag_names[t_lag] := c(as.Date(lags[,t_lag])),with=F]
			}
		
		# Merge this to skus_by_date
		setkey(tmpdates, date)
		setkey(skus_by_date, date)
		suppressWarnings(skus_by_date[, t_lag_names := NULL, with=F])
		skus_by_date <- tmpdates[skus_by_date]
		
		setkey(skus_by_date, 'category', 'country','brand', 'model', 'date')
		
		sales_lag_names = paste0('lag_sales', gsub('[-]', 'min', as.character(t_lags)))
		for (t_lag in seq(along=t_lags)) {
			eval(parse(text=paste0('skus_by_date[,  sales_lag_names[t_lag] := t_sales_units[match(', t_lag_names[t_lag],', date)],by=c(\'category\', \'country\',\'brand\', \'model\'),with=F]')))
			}
		eval(parse(text=paste0('skus_by_date[, t_wsales_units := rowSums(data.table(',paste(sales_lag_names,collapse=','),'), na.rm=T)]')))
		skus_by_date[, t_wsales_units:=t_wsales_units/length(t_lags)]
		
		# remove unnecessary columns
		skus_by_date[, c(t_lag_names, sales_lag_names) := NULL, with=F]
		rm(tmpdates)
		
		# -> tmpskutable is the complete sales data, combined with a SKU's attributes
		
		# assert that there are only 1 sku per unique key
		if(all(skus_by_date[, list(.N),by=c('country','category', 'date', 'sku_id')]$N==1)==F) stop('problem with unique key/SKUs')

	#######################################
	# Calculation of similarity measures  #
	#######################################
	
	cat('Calculating non-metric and metric similarity measures\n')
	
	for (coln in attrib) {
		colclass=eval(parse(text=paste0('class(skus_by_date$',coln,')')))
		print(coln)
		# compute weighted and non-weighted similarity metrics

		newcolid1=paste0('similarity_', gsub('attr_', '', coln))
		skus_by_date[, tmp_wattr := t_sales_units]
			
		if (colclass=='factor') {
		# for non-metric variables
		
		# define weighting variable, here: sales units in the current period
			
		# execute coding
			skus_by_date[, ':=' (n_sku_byattr = .N, w_sku_byattr = sum(tmp_wattr)), by=c('country', 'date', coln)]
			skus_by_date[, ':=' (n_sku_byattr_own = .N, w_sku_byattr_own = sum(tmp_wattr)), by=c('country', 'brand', 'date', coln)] 
			# the difference of n_sku_... is both will be equal to the amount of products with an attribute sold by the competition
			# the difference of the next set will be equal to the amount of sales incurred by the competition, which is the denominator
			
			skus_by_date[, ':=' (n_sku = .N, w_sku = sum(tmp_wattr)), by=c('country', 'date')]
			skus_by_date[, ':=' (n_sku_own = .N, w_sku_own = sum(tmp_wattr)), by=c('country', 'brand', 'date')]
			
			# add similarity metrics
			skus_by_date[, ':=' (similarity_nw = (n_sku_byattr-n_sku_byattr_own)/(n_sku-n_sku_own),
								 similarity_w = (w_sku_byattr-w_sku_byattr_own)/(w_sku-w_sku_own))	] # relative distance with weights equal to 1
			
			skus_by_date[is.na(similarity_nw), similarity_nw:=0] # if this measure is NaN, then it's the only SKU that has a particular attribute. Its similarity should therefore be 0.
			skus_by_date[is.na(similarity_w), similarity_w:=0] # if this measure is NaN, then it's the only SKU that has a particular attribute. Its similarity should therefore be 0.
			
			# clean up skus_by_date
			skus_by_date[, grep('n_sku|w_sku|tmp_wattr', colnames(skus_by_date),value=T) := NULL, with=F]
			
			setnames(skus_by_date, c('similarity_nw','similarity_w'), paste(c('nw','w'), newcolid1,sep='_'))
			}
			
		if (colclass=='numeric') {
			# compute similarity: METRIC 1 (relative distance)
	
			#attrval<-c(2,1,1, 1,1,1, 1,1,1)
			#w=c(2,1,1,1,1,1,1,1,1)
			#brand=c('a','a','a','b','b','b','c','c','c')
			#			skus_by_date[, GRP := .GRP, by=c('country', 'date')]
			#length(unique(skus_by_date$GRP))
			
			#attrval=skus_by_date[GRP==1, get(coln)]
			#w=skus_by_date[GRP==1]$tmp_wattr
			#brand=skus_by_date[GRP==1]$brand
			#df = data.table(attrval,w,brand)
			
			
			metric_similarity <- function(attrval, w, brand) {
				# note: weights will be automatically normalized to 1 in what follows.
				
				# If all attributes are the same, similarity is 1.
				if (length(unique(attrval))==1) return(rep(1, length(attrval)))
				
				valrange = max(attrval)-min(attrval)
				brand_index = match(brand,unique(brand))
				brand_index_list <- lapply(unique(brand_index), function(x) !brand_index%in%x)
				
					1-sapply(1:length(attrval), function(i) {
						# sum over all j not belonging to SKUs
						bindx=brand_index_list[[brand_index[i]]]
						attrval_j <- attrval[bindx]
						w_j <- w[bindx]
						sum(w_j*(abs(attrval[i]-attrval_j)))/(valrange*sum(w_j))
						})
					
					}
										
			metric_similarity <- cmpfun(metric_similarity)
				
			#	system.time({for (i in 1:100) metric_similarity(attrval,w,brand)})
			
			skus_by_date[, ':=' (similarity_nw = relative_dist(get(coln)),
								 similarity_w = metric_similarity(attrval=get(coln), w=tmp_wattr, brand=brand)), by=c('country', 'date')]
								 
			setnames(skus_by_date, c('similarity_nw','similarity_w'), paste(c('nw','w'), newcolid1,sep='_'))
			}
			
		}
		
	# Aggregate to uniqueness per SKU
		simcols = grep('^nw_similarity_',colnames(skus_by_date),value=T)
		eval(parse(text=paste0('skus_by_date[,nw_sku_unique:=1-rowMeans(data.frame(', paste(simcols,collapse=','),'))]')))
		simcols = grep('^w_similarity_',colnames(skus_by_date),value=T)
		eval(parse(text=paste0('skus_by_date[,w_sku_unique:=1-rowMeans(data.frame(', paste(simcols,collapse=','),'))]')))
	
	###################################
	# Calculation of novelty measure  #
	###################################

	# Definition: Number of SKUs introduced within the last three months, expressed as percentage of unique SKUs sold in the current period

		tmp_novelty = datlist_final[[i]][, list(first_date = min(date), last_date = max(date)),by = c('category','country','brand','model')]
		skus_by_date <- merge(skus_by_date, tmp_novelty ,by=c('category', 'country','brand','model'),all.x=T)

		uniq_date <- skus_by_date[, list(N=.N),by='date']
		uniq_date[, N:=NULL]
		uniq_date[, date_lag3 := sapply(date, function(x) {m<-as.POSIXlt(x); m$mon=m$mon-3; return(as.character(as.Date(m)))})]
		uniq_date[, date_lag3 := as.Date(date_lag3)]
		uniq_date[, date_lag1 := sapply(date, function(x) {m<-as.POSIXlt(x); m$mon=m$mon-1; return(as.character(as.Date(m)))})]
		uniq_date[, date_lag1 := as.Date(date_lag1)]
		setkey(skus_by_date, date)
		setkey(uniq_date, date)
		
		skus_by_date[uniq_date, ':=' (date_lag3 = i.date_lag3, date_lag1 = i.date_lag1)]
		rm(uniq_date)
	
	
	skus_by_date_list[[i]] <- skus_by_date
	
	print(warnings())
	}
	
names(skus_by_date_list) <- names(datlist_final)

print(warnings())

save(skus_by_date_list, file='..\\temp\\uniqueness_and_lagsales.RData')
	
