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
# - Create assortment variables at the brand level
# - Determine beginning and end of observation periods per brand
# - Aggregate small brands to a common "allothers" brand


require(data.table)


# -> skus_by_date_list: sales data on a SKU-level by date
	load('..//temp//uniqueness_and_lagsales.RData') 

# -> brand_selection: selection of brands

# -> time_selection: selection of category-date observations that will be used (e.g., due to "late" take-off of category)
	load('..//temp//select_periods_and_brands.RData') 

# -> indicators (economic indicators)
	load('..//temp//exch_cpi.RData')

# -> attrib
	load('..//temp//attributes.RData')

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
	skus_by_date[brand_selection, ':=' (selected_brand=i.selected_brand, brand_rename=i.brand_rename)]
	
	setnames(skus_by_date, 'brand', 'brand_orig')
	setnames(skus_by_date, 'brand_rename', 'brand')
	
	setkey(skus_by_date, category, country, brand, date)
	 
	merged_attr_sales = skus_by_date[, list( usales=sum(t_sales_units),
											 vsales = sum(t_value_sales), 
											 vsalesd = sum(t_value_sales_usd),
											
											 llen = length(unique(sku_id)), 
																						
											 wspr=weigh_by_w(t_value_sales/t_sales_units, t_sales_units),
											 wpspr=weigh_by_w(t_value_sales/t_sales_units, t_wsales_units),
											 
											 wsprd=weigh_by_w(t_value_sales_usd/t_sales_units, t_sales_units),
											 wpsprd=weigh_by_w(t_value_sales_usd/t_sales_units, t_wsales_units),
											 
											 wsun = weigh_by_w(w_sku_unique*100,t_sales_units),
											 wpsun = weigh_by_w(w_sku_unique*100,t_wsales_units),
											 
											 wsunnw = weigh_by_w(nw_sku_unique*100,t_sales_units),
											 wpsunnw = weigh_by_w(nw_sku_unique*100,t_wsales_units),
											 
										     #wsnumdist = weigh_by_w(t_numdist,t_sales_units),
											 #wpsnumdist = weigh_by_w(t_numdist,t_wsales_units),
											 
											 wswdst = weigh_by_w(t_wdist,t_sales_units),
											 wpswdst = weigh_by_w(t_wdist,t_wsales_units),
											 
											 nov1= length(unique(sku_id[which(first_date>date_lag1 & first_date <= date)])),
											 nov3= length(unique(sku_id[which(first_date>date_lag3 & first_date <= date)]))
											 ), by=c('category', 'country', 'market_id', 'date', 'brand', 'selected_brand')][order(category, country,brand,date)]
	
		# novelty variables must be set to missing for the first three months.
		merged_attr_sales[, N:=1:.N, by=c('category', 'country','brand')]
		merged_attr_sales[N%in%1:3, nov3:=NA]
		merged_attr_sales[N%in%1, nov1:=NA]
		merged_attr_sales[, N:=NULL]
		
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
	all_data[[i]]<-list(data=selected_attr_sales, skus = attribs[[i]])
	
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
			paneldata[, ':=' (rvsales = vsales/cpi, rwspr = wspr/cpi, rwpspr = wpspr/cpi, rwsprd = wsprd/cpi, rwpsprd = wpsprd/cpi)]
		
		# Investigate which part of the data set is complete and can be used for model estimation
			tmp <- split(paneldata, as.character(paneldata$brand))
		
			tmp <- lapply(tmp, function(dframe) {
				keyvars = c('country','brand','date','category')
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
			#.zoo[, brand_id:=cnt_brand+.GRP, by = c('brand')]
			#.zoo[, market_id:=cnt_market+1, by = c('brand')]
			
			#cnt_brand = cnt_brand+length(unique(.zoo$brand))
			#cnt_market = cnt_market + 1
			
			.zoo[which(!is.na(usales) & selected_t_brand==T & selected_brand == T & !selected_t_cat %in% c(NA, F)), selected:=T, by=c('category', 'country', 'brand')]
			.zoo[is.na(selected), selected:=F, by=c('category', 'country', 'brand')]
			
			}
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

