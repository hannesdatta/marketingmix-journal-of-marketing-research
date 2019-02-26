#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|

# Goals: 
# - Create variables at the brand level
# - Determine beginning and end of observation periods per brand
# - Aggregate small brands to a common "allothers" brand

require(data.table)

# -> skus_by_date_list: sales data on a SKU-level by date
  load('..//temp//uniqueness_and_lagsales.RData') 

# -> brand_selection: selection of brands
	load('..//temp//select.RData') 

# -> indicators (economic indicators)
	load('..//temp//exch_cpi.RData')
	Rcpp::sourceCpp('runsum.cpp')
	
# initialize object to store final data
	all_data <- NULL
	
	source('proc_functions.R')
	
# run the preparation
for (i in 1:length(skus_by_date_list)) {
	cat('=============================================================\n')
	cat(names(skus_by_date_list)[i],fill=T)
	cat('=============================================================\n')
	
	skus_by_date <- skus_by_date_list[[i]]
	
	#####################################
	#### Aggregation to brand-level #####
	#####################################

	cat('Aggregation to brand-level\n')
	
	# Which brands have been selected for modeling? Add the info here, plus brand names to be used for aggregation (e.g., for collapsing some brands)
	skus_by_date[, category:=names(skus_by_date_list)[i]]
	setkey(skus_by_date, category, country, brand)
	skus_by_date[brand_selection, ':=' (selected_brand=i.selected_brand, n_brands_selected=i.n_brands_selected, brand_rename=i.brand_rename)]
	
	setnames(skus_by_date, 'brand', 'brand_orig')
	setnames(skus_by_date, 'brand_rename', 'brand')
	
	setkey(skus_by_date, category, country, brand, brand_orig, date)
	
	# keep only markets for which we have at least 2 brands selected
	skus_by_date = skus_by_date[n_brands_selected>1]
	
	# Compute novelty of product assortment
  	idvars=c('market_id','category','country','brand','brand_orig', 'selected_brand', 'model')
  	tmp = skus_by_date[, list(first_date=min(first_date,na.rm=T)),by=idvars]
  	alldates=seq(from=min(tmp$first_date,na.rm=T), to=as.Date('2015-12-01'), by = '1 month')
  	
  	emptydf=data.table(first_date=alldates)
  	for (coln in colnames(tmp)[!colnames(tmp)%in%c('first_date')]) {
  	  emptydf[, (coln):=NA]
  	}
  	
  	tmp=rbind(tmp, emptydf)
  	
  	tmp[, id := .GRP,by=idvars]
  	tmp[, value:=1]
  	novelty=dcast(tmp, id+first_date~., drop=F, fill=0)
  	setnames(novelty, '.', 'novel')
  	
  	setkey(novelty)
  	setkey(tmp, id)
  	
  	cmds=paste(sapply(idvars, function(cols) paste0(cols, '=i.',cols)),collapse=',')
  	eval(parse(text=paste0("novelty[tmp, ':='(", cmds, ")]")))
  	
  	novelty=novelty[!id==max(id)]
  	setorder(novelty, id, first_date)
  	novelty[, novel_sum:=cumsum(cumsum(novel)),by=idvars]
	
	# Compute average prices, distribution, etc. using current and lagged sales
  	skus_by_date[, id:=.GRP,by=idvars]
  	
    tmp=dcast(skus_by_date, id+date~., value.var=c('t_sales_units','t_value_sales', 't_value_sales_usd', 't_wdist', 't_price','t_price_usd'),drop=F)
  	
    setkey(tmp, id)
  	setkey(skus_by_date, id)
  	cmds=paste(sapply(idvars, function(cols) paste0(cols, '=i.',cols)),collapse=',')
  	eval(parse(text=paste0("tmp[skus_by_date, ':='(", cmds, ")]")))
  
  	tmp[is.na(t_sales_units), t_sales_units:=0]
  	tmp[t_sales_units<0, t_sales_units:=0]
  	
  	for (.var in c('t_price', 't_price_usd', 't_wdist')) {
  	  tmp[, paste0(.var, '_filled'):=na_forwarding(get(.var)), by=idvars]
  	  }
  
  	# add rolling sum of unit sales
    tmp[, t_sales_units_rolled := run_sum(t_sales_units, n=3),by=idvars]
  	tmp[is.na(t_sales_units_rolled), t_sales_units_rolled := 0]
  	tmp[, t_noweights:=1]
  	
  	aggkey=c(setdiff(idvars,c('model','brand_orig')),'date')
  	
  	setkeyv(tmp, c(idvars, 'date'))
  	setkeyv(novelty, c(idvars, 'first_date'))
  	
  	tmp[novelty, novelty_sum:=i.novel_sum]
	
  # Aggregate data
	merged_attr_sales = tmp[, list( usales=sum(t_sales_units,na.rm=T),
	                                         vsales = sum(t_value_sales,na.rm=T), 
	                                         vsalesd = sum(t_value_sales_usd,na.rm=T),
	                                         
	                                         llen = length(unique(model[t_sales_units>0])),
	                                         
	                                         wspr=weigh_by_w(t_price_filled, t_sales_units,na.rm=T),
	                                         wpspr=weigh_by_w(t_price_filled, t_sales_units_rolled,na.rm=T),
	                                         nwpr= weigh_by_w(t_price_filled, t_noweights,na.rm=T),
	                                         
	                                         wsprd=weigh_by_w(t_price_usd_filled, t_sales_units, na.rm=T),
	                                         wpsprd=weigh_by_w(t_price_usd_filled, t_sales_units_rolled, na.rm=T),
	                                         nwprd = weigh_by_w(t_price_usd_filled, t_noweights,na.rm=T),
	                                         
	                                         wswdst = weigh_by_w(t_wdist_filled,t_sales_units,na.rm=T),
	                                         wpswdst = weigh_by_w(t_wdist_filled,t_sales_units_rolled,na.rm=T),
	                                          
	                                         nov1 = length(unique(model[t_sales_units>0&novelty_sum%in%1])),
	                                         nov3 = length(unique(model[t_sales_units>0&novelty_sum%in%1:3])),
	                                         nov6 = length(unique(model[t_sales_units>0&novelty_sum%in%1:6])),
  	                                       nov12 = length(unique(model[t_sales_units>0&novelty_sum%in%1:12]))
	                                
	                                 ),
	by=aggkey]

	# transform novelty variables to shares; if llen = 0, set novelty share to 0.
	novvars <- grep('nov[0-9].*', colnames(merged_attr_sales),value=T)
	for (.var in novvars) merged_attr_sales[, (paste0(.var,'sh')) := ifelse(llen==0, 0, (get(.var)/llen)*100)]
	
	# The novelty data is censored at the beginning of a category's observation period, except in 
	# the tablets category (this is monitored at the start of the category)
	tmp_catdates <- merged_attr_sales[,list(sumsales=sum(usales)),by=c('market_id','date')][sumsales>0, list(date=unique(date)), by='market_id']
	setorder(tmp_catdates, market_id,date)
	tmp_catdates[, Ncens:=1:.N, by = c('market_id')]
	
	setkey(tmp_catdates, market_id, date)
	setkey(merged_attr_sales, market_id, date)
	merged_attr_sales[tmp_catdates, Ncens:=i.Ncens]
	
	catname=names(skus_by_date_list)[i]
	
	for (lags in c(1,3,6,12)) merged_attr_sales[Ncens%in%1:lags&!catname=='tablets', (paste0('nov', lags)):=-999]

	# Add indicators for which category-date observations should be included
  	setkey(merged_attr_sales, category, country, date)
  	merged_attr_sales[time_selection, selected_t_cat := i.selected_t_cat]
	
	# Merge CPIs
		cpi=indicators[type=='cpi']
		setkey(cpi, date, country)
	
		merged_attr_sales[, selected_brand := unique(selected_brand[!is.na(selected_brand)]), by = c('brand')]
		
		setkey(merged_attr_sales, date, country)
		merged_attr_sales[cpi, cpi:=i.value]
		
		
	# Convert some columns to factors
		merged_attr_sales[, ':=' (brand=as.factor(brand), country=as.factor(country), category=as.factor(category))]
		
	# Sort the data
		setorder(merged_attr_sales,country,brand,date)

	##############################
	# Interpolation of missings  #
	##############################
	
		# define columns to interpolate if missings occur inbetween subsequent observations
		all_cols=colnames(merged_attr_sales)
		interp_cols = all_cols[!all_cols%in%c('category', 'country', 'market_id', 'brand', 'date', 'selected_t_cat', 'selected_brand')]
		
		# set some columns to NA before interpolating
		for (.var in grep('spr', all_cols,value=T)) {
		  eval(parse(text=paste0('merged_attr_sales[which(', .var,'<=0), ', .var, ':=NA]')))
		}
		
		# set sales columns to NA if they are below 0.
		for (.var in grep('sales$|salesd$|dst$', all_cols, value=T)) {
		  eval(parse(text=paste0('merged_attr_sales[, ', .var, ' := ifelse(', .var, '<0, NA, ', .var,')]')))
		}
		
		setorder(merged_attr_sales,market_id,category,country,brand,date)
		
		# interpolate variables
		dframe_interp<-cbind(merged_attr_sales[, c('date', 'country', 'category','selected_t_cat', 'selected_brand'),with=F], 
		                     merged_attr_sales[, lapply(.SD, nafill),by=c('category','country','market_id', 'brand'),.SDcols=interp_cols])
			
		dframe_noninterp <- merged_attr_sales[,colnames(dframe_interp),with=F]
			
		# compare whether values were interpolated, or not
			unequal = rowSums(!is.na(dframe_interp))-rowSums(!is.na(dframe_noninterp))
		# add an indicator which rows contain at least one interpolated value
			dframe_interp[, interpolated:= !unequal==0]
			
		# add market share metrics
			dframe_interp[,':=' (usalessh = as.numeric(usales/sum(usales,na.rm=T)),
		           						 vsalessh = as.numeric(vsales/sum(vsales,na.rm=T))), by=c('country', 'date')]
	
	# store data 
	all_data[[i]]<-list(data=split(dframe_interp, dframe_interp$country))
	
	# remove objects
		rm(merged_attr_sales,skus_by_date)
	
	# clear memory
		gc()
		cat('\n')
}

# rename
names(all_data)<-names(skus_by_date_list)

# Initialize count variables to assign unique IDs to brands and markets
	cnt_brand=0
	cnt_market=0

# prepare final (cleaned) data sets / longest consecutive stretch selection
for (i in 1:length(all_data)) {
	print(i)
	cat('Flagging missing observations per brand\n')
	all_data[[i]]$data_cleaned <- NULL
	
	for(j in seq(along=all_data[[i]]$data)) {
		# Get data
			paneldata = all_data[[i]]$data[[j]]
		
		# Correct monetary variables with a country's CPI
			paneldata[, ':=' (rvsales = vsales/cpi, rwspr = wspr/cpi, 
			                  rwpspr = wpspr/cpi, 
			                  rwsprd = wsprd/cpi, 
			                  rwpsprd = wpsprd/cpi)]
		
		# Investigate which part of the data set is complete and can be used for model estimation
			tmp <- split(paneldata, as.character(paneldata$brand))
		
			tmp <- lapply(tmp, function(dframe) {
				keyvars = c('market_id', 'category', 'country','brand','date')

				# include 0-sales periods inbetween non-zero sales				
				if(0) {
				dframe[, usales0:=usales]
				dframe[1:.N<first(which(usales>0)), usales0:=NA]
				dframe[1:.N>last(which(usales>0)), usales0:=NA]
				}
				
				#if(0){
				# exclude 0-sales periods inbetween non-zero sales				
				  dframe[, usales0:=usales]
				  dframe[usales==0, usales0:=NA]
				#}
				
				all_cols=colnames(dframe)
				
				.availabilitycheck1 = setdiff(all_cols,c(keyvars, 'cpi', 'interpolated', 'selected_t_cat', 'selected_brand'))
				.availabilitycheck2 = NULL 
				
				# determine max consecutive observations
				.zoo = zoo(dframe)
				
				.excl <- NULL
				
				.out = try(na.contiguous(.zoo),silent=T)
				
				if (class(.out)=='try-error') {
					if (grepl('all times contain an NA', .out[1])) { 
						
						# check out which time series has only NAs, rerun
						.excl = names(which(colSums(is.na(.zoo))==length(unique(dframe$date))))
						.zoo = zoo(dframe[, setdiff(c(keyvars, .availabilitycheck1, .availabilitycheck2), .excl),with=F])
						.out = try(na.contiguous(.zoo),silent=T)
						}
					}

				.zoo = dframe
				
				# tag longest consecutive stretch
				suppressWarnings(.zoo[, selected_t_brand:=!1:.N %in% as.numeric(attr(.out, 'na.action'))])
				.zoo <- .zoo[, c(keyvars, .availabilitycheck1, .availabilitycheck2, 'selected_t_brand', 'selected_t_cat', 'selected_brand'),with=F]
				.zoo[, usales0:=NULL]
				return(.zoo)
				})
			
		.zoo <- rbindlist(tmp)
		
		# clean category/country/brand names
		if (nrow(.zoo)>0){
			.zoo[, category:=as.character(gsub('[-]|[/]','', category))]
			.zoo[, brand:=as.character(gsub(' |[-]|[/]|[(]|[)]|[.]|[&]','', brand))]
			.zoo[, ':=' (category=tolower(category),country=tolower(country),brand=tolower(brand))]

			#.zoo[which(!is.na(usales) & selected_t_brand==T & selected_brand == T & !selected_t_cat %in% c(NA, F)), selected:=T, by=c('category', 'country', 'brand')]
			#.zoo[is.na(selected), selected:=F, by=c('category', 'country', 'brand')]
			
		}
		
		novvars= grep('nov[0-9]+', colnames(.zoo),value=T)
		for (.var in novvars) .zoo[get(.var)==-999, (.var):=NA]
		
		all_data[[i]]$data_cleaned[[j]] <- .zoo
		}
}	

save(all_data, file='../temp/brand_metrics.RData')	

