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

# Check number of categories in file
rbindlist(lapply(datlist_final, function(x) x[, list(N=.N),by=c('catname','country')]))

# Remove 'non-used' parts of the dataset (e.g., GfK 2012, if GfK 2015 data is being used)
datlist_final<-lapply(datlist_final, function(x) {
	tmp=x[which(used==T)][, category:=catname]
	if (!nrow(tmp)==0) return(tmp)})
# delete empty categories

datlist_final <- datlist_final[!unlist(lapply(datlist_final, is.null))]


# Check number of categories in file
rbindlist(lapply(datlist_final, function(x) x[, list(N=.N),by=c('category','country')]))

# Extract first availability dates from the data

  # get string vector of first activity dates
  months=data.table(char_month=unique(unlist(lapply(datlist_final, function(x) unique(x$firstactivity)))))
  months[, char_month:=gsub('[-]', ' 20', char_month)]
  months[, year:=sapply(char_month, function(x) strsplit(x, ' ')[[1]][2])]
  months[, month:=sapply(char_month, function(x) tolower(substr(strsplit(x, ' ')[[1]][1],1,3)))]
  
  raw_months<-c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')
  months[, month_no := as.character(formatC(match(month, raw_months), width=2,flag=0))]
  
  months[, firstactivity_date:=as.Date(paste0(year,'-', month_no, '-01'))]
  
  months[char_month=='BEFORE 2001', firstactivity_date:=as.Date('2000-12-01')]
  months<-months[!is.na(char_month)]

  
#################################################################
### EXECUTING CREATION OF VARIABLES
#################################################################

source('proc_functions.R')

# Put all data in a list (per category, and country)
# Creates all attribute-based measures
skus_by_date_list <- NULL
novelty_list <- NULL


for (i in 1:length(datlist_final)) {
	cat('=============================================================\n')
	cat(names(datlist_final)[i],fill=T)
	cat('=============================================================\n')
	assign("last.warning", NULL, envir = baseenv())
	

	setkey(datlist_final[[i]], category, country, brand, model, date)

	skus_by_date = datlist_final[[i]][, list(t_sales_units=sum(sales_units), 
											 t_value_sales = sum(sales_units*as.numeric(price_lc)),
											 t_value_sales_usd = sum(sales_units*as.numeric(price_usd)),
											 t_numdist = sum(sales_units*numeric_distribution)/sum(sales_units),
											 t_wdist = sum(sales_units*weighted_distribution)/sum(sales_units)), 
											 by=c('category', 'country', 'market_id', 'brand', 'model', 'date')]
	
	# If a product is not sold, kick it out from this list
	skus_by_date <- skus_by_date[!t_sales_units==0]
	
	###########################################################################################################
	# Create weighted past sales metric for every SKU (used for weighing the price and distribution metrics)  #
	###########################################################################################################
	
		cat('Compute past sales metric for re-weighing.\n')
		
		# metrics that are aggregated to a brand-level will be
		# weighted with each SKU's lagged sales (t-2, t-1, t).
		
		# question: are NAs counted as ZEROS, or as NA's:
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
			tmpdates[, t_lag_names[t_lag] := c(as.Date(lags[,t_lag]))]
			}
		
		# Merge this to skus_by_date
		setkey(tmpdates, date)
		setkey(skus_by_date, date)
		suppressWarnings(skus_by_date[, t_lag_names := NULL])
		skus_by_date <- tmpdates[skus_by_date]
		
		setkey(skus_by_date, 'category', 'country','brand', 'model', 'date')
		
		sales_lag_names = paste0('lag_sales', gsub('[-]', 'min', as.character(t_lags)))
		for (t_lag in seq(along=t_lags)) {
			eval(parse(text=paste0('skus_by_date[,  sales_lag_names[t_lag] := t_sales_units[match(', t_lag_names[t_lag],', date)],by=c(\'category\', \'country\',\'brand\', \'model\')]')))
			}
		eval(parse(text=paste0('skus_by_date[, t_wsales_units := rowSums(data.table(',paste(sales_lag_names,collapse=','),'), na.rm=T)]')))
		skus_by_date[, t_wsales_units:=t_wsales_units/length(t_lags)]
		
		# remove unnecessary columns
		skus_by_date[, c(t_lag_names, sales_lag_names) := NULL]
		rm(tmpdates)
		

	###################################
	# Calculation of novelty measure  #
	###################################

	# Definition: Number of SKUs introduced within the last X months, expressed as percentage of unique SKUs sold in the current period
		setkey(months, char_month)
		setkey(datlist_final[[i]], firstactivity)
		datlist_final[[i]][months, firstactivity_date:=i.firstactivity_date]
		
		tmp_novelty = datlist_final[[i]][, list(first_datecoded = min(firstactivity_date), first_datedata = min(date), last_date = max(date)),by = c('category','country','brand','model')]
    tmp_novelty[is.na(first_datecoded), first_date:=first_datedata]
    tmp_novelty[first_datecoded>first_datedata, first_date:=first_datedata]
    tmp_novelty[first_datecoded<=first_datedata, first_date:=first_datecoded]
    
    novelty_list[[i]]<-tmp_novelty
    
		skus_by_date <- merge(skus_by_date, tmp_novelty ,by=c('category', 'country','brand','model'),all.x=T)

		uniq_date <- skus_by_date[, list(N=.N),by='date']
		uniq_date[, N:=NULL]
		
		for (lags in c(1,3,6,12)) {
			uniq_date[, paste0('date_lag', lags) := as.Date(sapply(date, function(x) {m<-as.POSIXlt(x); m$mon=m$mon-lags; return(as.character(as.Date(m)))}))]
			}
		
		skus_by_date=merge(skus_by_date, uniq_date, by = c('date'))
		rm(uniq_date)
	
	skus_by_date_list[[i]] <- skus_by_date
	
	print(warnings())
	}
	
names(skus_by_date_list) <- names(datlist_final)
names(novelty_list) <- names(datlist_final)

print(warnings())

  # verify first_date field has been properly coded
  all=rbindlist(novelty_list)
  all[,diff:=as.numeric(difftime(first_datedata,first_datecoded, units='weeks'))]
  
  # hard-coded first activity dates BEFORE first sales observation
  val1=nrow(all[first_datecoded<first_datedata])/nrow(all)
  # hard-coded first activity dates EQUAL to first sales observation
  val2=nrow(all[first_datecoded==first_datedata])/nrow(all)
  # hard-coded first activity dates AFTER first sales observation
  val3=nrow(all[first_datecoded>first_datedata])/nrow(all)
  # hard-coded first activity MISSING
  val4=nrow(all[is.na(first_datecoded)])/nrow(all)
  #asserts
  stopifnot(val1+val2+val3+val4==1)
  stopifnot(nrow(all[is.na(first_date)])==0)
  
# save
save(skus_by_date_list, novelty_list, file='..\\temp\\uniqueness_and_lagsales.RData')
