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

# -> time_selection: selection of category-date observations that will be used (e.g., due to "late" take-off of category)
	load('..//temp//select.RData') 

# -> indicators (economic indicators)
	load('..//temp//exch_cpi.RData')
	Rcpp::sourceCpp('runsum.cpp')
	
# initialize object to store final data
	all_data <- NULL
	
# run the preparation
for (i in 1:length(skus_by_date_list)) {
	cat('=============================================================\n')
	cat(names(skus_by_date_list)[i],fill=T)
	cat('=============================================================\n')
	
	#####################################
	#### Aggregation to brand-level #####
	#####################################

	cat('Aggregation to brand-level\n')
	
	# Aggregate remaining measures to the brand level
	source('proc_functions.R')

	
	
	# Merge time and date observations to the data
	skus_by_date <- skus_by_date_list[[i]]
	skus_by_date[, category:=names(skus_by_date_list)[i]]
	setkey(skus_by_date, category, country, brand)
	skus_by_date[brand_selection, ':=' (selected_brand=i.selected_brand, n_brands_selected=i.n_brands_selected, brand_rename=i.brand_rename)]
	
	setnames(skus_by_date, 'brand', 'brand_orig')
	setnames(skus_by_date, 'brand_rename', 'brand')
	
	setkey(skus_by_date, category, country, brand, date)
	
	skus_by_date = skus_by_date[n_brands_selected>1]
	
	# compute average prices using the correct lagging specification
	idkey=c('market_id','brand_orig','brand', 'model')
  skus_by_date[, market_brand_model:=.GRP,by=idkey]
	
  tmp=dcast(skus_by_date, market_brand_model+date~., value.var=c('t_sales_units','t_value_sales', 't_value_sales_usd', 't_wdist'),drop=F)
	
	setkey(tmp, market_brand_model)
	setkey(skus_by_date, market_brand_model)
	tmp[skus_by_date, ':=' (market_id=i.market_id, brand_orig=i.brand_orig, 
	                        brand=i.brand, model=i.model)]
	tmp[is.na(t_sales_units), t_sales_units:=0]
	
	tmp[, ':=' (avgprice = t_value_sales/t_sales_units,
	            avgprice_usd = t_value_sales_usd/t_sales_units)]
	
	for (.var in c('avgprice', 'avgprice_usd', 't_wdist')) {
	  tmp[, paste0(.var, '_filled'):=na_forwarding(get(.var)), by=idkey]
	  }

	# add rolling sum of unit sales
  tmp[, t_sales_units_rolled := run_sum(t_sales_units, n=3),by=idkey]
	tmp[is.na(t_sales_units_rolled), t_sales_units_rolled := 0]
	tmp[, t_noweights:=1]
	
	merged_attr_sales2 = tmp[, list( usales=sum(t_sales_units,na.rm=T),
	                                         vsales = sum(t_value_sales,na.rm=T), 
	                                         vsalesd = sum(t_value_sales_usd,na.rm=T),
	                                         
	                                         llen = length(unique(model[t_sales_units>0])),
	                                         
	                                         wspr=weigh_by_w(avgprice_filled, t_sales_units,na.rm=T),
	                                         wpspr=weigh_by_w(avgprice_filled, t_sales_units_rolled,na.rm=T),
	                                         nwpr= weigh_by_w(avgprice_filled, t_noweights,na.rm=T),
	                                         
	                                         wsprd=weigh_by_w(avgprice_usd_filled, t_sales_units, na.rm=T),
	                                         wpsprd=weigh_by_w(avgprice_usd_filled, t_sales_units_rolled, na.rm=T),
	                                         nwprd = weigh_by_w(avgprice_usd_filled, t_noweights,na.rm=T),
	                                         
	                                         wswdst = weigh_by_w(t_wdist_filled,t_sales_units,na.rm=T),
	                                         wpswdst = weigh_by_w(t_wdist_filled,t_sales_units_rolled,na.rm=T)),
	by=c('market_id', 'date', 'brand')]

	merged_attr_sales1 = skus_by_date[, list(nov1= length(unique(model[which(first_date>date_lag1 & first_date <= date)])),
											 nov3= length(unique(model[which(first_date>date_lag3 & first_date <= date)])),
											 nov6= length(unique(model[which(first_date>date_lag6 & first_date <= date)])),
											 nov12= length(unique(model[which(first_date>date_lag12 & first_date <= date)]))
											 ), by=c('category', 'country', 'market_id', 'date', 'brand', 'selected_brand')][order(category, country,brand,date)]
	# any missing obs?
	
	merged_attr_sales=merge(merged_attr_sales1, merged_attr_sales2, by = c('market_id','date','brand'),all.x=T,all.y=F)
	
	if(0){dates<-data.table(date=unique(merged_attr_sales2$date))
	setorder(dates,date)
	dates[,order:=1:.N]
	setkey(dates,date)
	setkey(merged_attr_sales2, date)
	merged_attr_sales2[dates, dateorder:=i.order]
	
	merged_attr_sales2[, datediff:=c(NA, dateorder[-1]-dateorder[-.N]), by=c('market_id','brand')]
	setorder(merged_attr_sales2, market_id, brand,date)
	}
	
	# novelty data is censored at the beginning of a category's observation period, except in 
	# the tablets category (this is monitored at the start of the category)
	tmp_catdates <- merged_attr_sales[!is.na(usales), list(date=unique(date)), by='market_id']
	setorder(tmp_catdates, market_id,date)
	tmp_catdates[, Ncens:=1:.N, by = c('market_id')]
	
	setkey(tmp_catdates, market_id, date)
	setkey(merged_attr_sales, market_id, date)
	merged_attr_sales[tmp_catdates, Ncens:=i.Ncens]
	
	catname=names(skus_by_date_list)[i]
	
	for (lags in c(1,3,6,12)) merged_attr_sales[Ncens%in%1:lags&!catname=='tablets', (paste0('nov', lags)):=-999]
	#merged_attr_sales[, Ncens:=NULL]

	# Add indicators for category-sales observations
	setkey(merged_attr_sales, category, country, date)
	merged_attr_sales[time_selection, selected_t_cat := i.selected_t_cat]
	
	# Merge CPIs
		cpi=indicators[type=='cpi']
		setkey(cpi, date, country)
		
	# Make an empty dataset from beginning date till end date, merge
		all_dates = seq(from=base::as.Date('2004-01-01'),to=base::as.Date('2014-12-01'),by='1 month')
		tmp <- split(merged_attr_sales, as.character(merged_attr_sales$country))
		selected_attr_sales=lapply(tmp, function(dframe) {
									# create 'empty' data frame (with full set of dates
									empty_df=data.table(date=rep(all_dates,each=length(unique(dframe$brand))), 
														  category=as.character(dframe$category)[1],
														  country=unique(dframe$country)[1],
														  market_id=unique(dframe$market_id)[1],
														  brand=rep(unique(dframe$brand), length(all_dates))
														  )
									
									res=merge(empty_df, dframe, by=c('category', 'country', 'market_id', 'brand', 'date'),all.x=T)
									setkey(res, 'date', 'country')
									
									res[, selected_brand := unique(selected_brand[!is.na(selected_brand)]), by = c('brand')]
									
									# add cpi's
									res[cpi, cpi:=i.value]
									# order
									setorder(res,country,brand,date)
									# convert some columns to factors
									res[, ':=' (brand=as.factor(brand), country=as.factor(country), category=as.factor(category))]
									return(res)
									})
		rm(tmp)
		
	##############################
	# Interpolation of missings  #
	##############################
	
		# define colums to interpolate (maximum fill currently set to two observations
		all_cols=unique(unlist(lapply(selected_attr_sales, colnames)))
		interp_cols = all_cols[!all_cols%in%c('category', 'country', 'market_id', 'brand', 'date', 'selected_t_cat', 'selected_brand')]
		
		selected_attr_sales <- lapply(selected_attr_sales, function(dframe) {
			setorder(dframe, country,brand,date)
			
			# set some columns to NA before interpolating
			for (.var in grep('spr', all_cols,value=T)) {
				eval(parse(text=paste0('dframe[which(', .var,'<=0), ', .var, ':=NA]')))
				}
			
			# set sales columns to NA if they are below 0.
			for (.var in c('usales', 'vsales', 'vsalesd', 'wswdst', 'wpswdst')) {
				eval(parse(text=paste0('dframe[, ', .var, ' := ifelse(', .var, '<0, NA, ', .var,')]')))
				}
											
			# interpolate variables
			dframe_interp<-cbind(dframe[, c('date', 'category','selected_t_cat', 'selected_brand'),with=F], 
								 dframe[, lapply(.SD, nafill),by=c('country','market_id', 'brand'),.SDcols=interp_cols])
			
			dframe_noninterp <- dframe[,colnames(dframe_interp),with=F]
			
			# compare whether values were interpolated, or not
			unequal = rowSums(!is.na(dframe_interp))-rowSums(!is.na(dframe_noninterp))
			# add an indicator which rows contain at least one interpolated value
			dframe_interp[, interpolated:= !unequal==0]
			
			# add market share metrics
			dframe_interp[,':=' (usalessh = as.numeric(usales/sum(usales,na.rm=T)),
								 vsalessh = as.numeric(vsales/sum(vsales,na.rm=T))), by=c('country', 'date')]
	
			return(dframe_interp)
		})
	
	rm(all_cols)
		
	# store data 
	all_data[[i]]<-list(data=selected_attr_sales)
	
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

# prepare final (cleaned) data sets
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
				all_cols=colnames(paneldata)
				
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
				return(.zoo)
				})
			
		.zoo <- rbindlist(tmp)
		
		# clean category/country/brand names
		if (nrow(.zoo)>0){
			.zoo[, category:=as.character(gsub('[-]|[/]','', category))]
			.zoo[, brand:=as.character(gsub(' |[-]|[/]|[(]|[)]|[.]|[&]','', brand))]
			.zoo[, ':=' (category=tolower(category),country=tolower(country),brand=tolower(brand))]

			.zoo[which(!is.na(usales) & selected_t_brand==T & selected_brand == T & !selected_t_cat %in% c(NA, F)), selected:=T, by=c('category', 'country', 'brand')]
			.zoo[is.na(selected), selected:=F, by=c('category', 'country', 'brand')]
			
		}
		
		novvars= grep('nov[0-9]+', colnames(.zoo),value=T)
		for (.var in novvars) .zoo[get(.var)==-999, (.var):=NA]
		
		all_data[[i]]$data_cleaned[[j]] <- .zoo
		}
	}	

# Load GDP per capita, and put into data sets
	load('..\\temp\\gdppercap.RData')
	
# Prepare CSV file with data
	brand_panel=rbindlist(lapply(all_data, function(x) rbindlist(x$data_cleaned)))
	setorder(brand_panel, market_id, category,country,brand,date)

	fwrite(brand_panel, file = '..\\output\\datasets.csv', row.names=F)
	
# Save complete data as .RData
	save(all_data, gdppercap, file =  '..\\output\\datasets.RData')

	
# In which categories does the first sales NOT correspond with selected t in category
	tmp=brand_panel[, list(first_sales=min(date[!is.na(usales)]), first_tcat=min(date[selected_t_cat==T],na.rm=T)), by = c('market_id', 'category', 'country')]
	tmp[!first_sales==first_tcat]

	
