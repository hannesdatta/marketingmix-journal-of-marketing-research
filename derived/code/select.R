#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|


# Goals: Select...

# (a) the time periods to use for each market, and
# (b) which brands will be included in each category

# Load packages
library(data.table)

# Load data
	load('..\\temp\\uniqueness_and_lagsales.RData')

dir.create('../output')

# Assert: How many categories are passed on from the previous step? must be 174 (182-4-4)!
#stopifnot(nrow(rbindlist(lapply(skus_by_date_list, function(x) x[, list(N=.N), by=c('category','country')])))==174)
stopifnot(nrow(rbindlist(lapply(skus_by_date_list, function(x) x[, list(N=.N), by=c('category','country')])))==196-4-4)

##################################################
### Determine start and end of modeling period ###
##################################################
	
	# Implement threshold (for cutoff / turned on/off, see below)
	
	tmp_sales = rbindlist(lapply(skus_by_date_list, function(x) x[,list(total_sales=sum(t_sales_units, na.rm=T)),by=c('category', 'country','date')]))

	tmp_sales[, max_sales := max(total_sales),by=c('category','country')]
	tmp_sales[, indic_max_sales := total_sales==max_sales,by=c('category','country')]
	tmp_sales[, indic_above_threshold := total_sales>.05*max_sales,by=c('category','country')]
	
	tmp_sales[, indic_above_threshold := T] # turn off threshold (set it to "above" for all markets)
	
	table(tmp_sales$indic_above_threshold)
	setorderv(tmp_sales, c('category','country','date'), order=1L)
	
	# determine first and last observations
	tmp_sales[, first:=cumsum(cumsum(indic_above_threshold))==1,by=c('category','country')]
	tmp_sales[, last:=rev(cumsum(cumsum(rev(indic_above_threshold)))==1),by=c('category','country')]
	tmp_sales[, selected_t_cat:=cumsum(first+last)==1|last==1,by=c('category','country')]
	
	setkey(tmp_sales,category,country,date)
	
	# assemble one dataset from all categories (keep only columns common across all data sets)
	dat<-rbindlist(lapply(skus_by_date_list, function(x) x[,grep('category|country|brand|model|date|^t_.*|unique|[_]date', colnames(skus_by_date_list[[2]]),value=T),with=F]))
	setkey(dat, category, country, date)
	
	# add time selection
	dat[tmp_sales, selected_t_cat:=i.selected_t_cat]
	
	dat[, year := year(date)]
	
	months = dat[, list(.N), by = c('date')]
	setorder(months, date)
	months[, month := as.numeric(as.factor(as.character(date)))]
	setkey(months, date)
	setkey(dat, date)
	dat[months, month := i.month]
	
	min_share =.01
	brand_id = 'brand'
	time_id = 'month'
	
####################
# Brand selection  #
####################
	
	sel_applied=c('main') #, '8years')
	
	selection = lapply(sel_applied, function(i) {
	# selection criteria: all brands with at least X% market share in X consec. years (at X*12 months)
	# with average yearly share of at least x%. 

	
	if(i=='main') {
	  consec_min = 5
	  consec_min_tablets = 4
	}
	
  if(i=='8years') {
    consec_min = 8
    consec_min_tablets = consec_min
  }
	  
	# aggregate brand sales for selected time periods (determined on a category-level earlier)
	
	# establish consecutive number of years (first, for all brands, then for the combined 'allother' composite brand)

	# for first iteration
	
  sel_brands<-function(tmp_brands, brand_id) {
  	tmp_brands[!category == 'tablets', consec_min := consec_min*12]
  	tmp_brands[category == 'tablets', consec_min := consec_min_tablets*12]
  	
  	setorderv(tmp_brands, c('category', 'country',time_id, 'brand_sales'),order=-1)
  	tmp_brands[, obs_difference := as.numeric(0)]
  	setorderv(tmp_brands, c('category', 'country', brand_id, time_id))
  	tmp_brands[, obs_difference := c(diff(get(time_id)),1), by=c('category','country',brand_id)]
  	tmp_brands[, cumsum_obsdiff := cumsum(obs_difference-1), by=c('category','country',brand_id)]
  	tmp_brands[, obs := sum(time_periods[obs_difference==1 | !obs_difference==1&.I==.N]), by=c('category','country',brand_id, 'cumsum_obsdiff')]
  	
  	tmp_brands_select = tmp_brands[, list(brand_sales=sum(brand_sales), consec_fulfilled = any(obs >= consec_min), tot_obs=max(obs)),by=c('category','country',brand_id)]
  	tmp_brands_select[is.na(consec_fulfilled), consec_fulfilled:=F]
  	setorderv(tmp_brands_select, c('category', 'country',brand_id))
  
  	# calculate market shares by brand
  	tmp_brands_select[, marketshare := brand_sales / sum(brand_sales), by=c('category','country')]
  	tmp_brands_select[, ms_fulfilled := marketshare >= min_share]
  	tmp_brands_select[, selected_brand := !grepl('unknown|local|unbranded', get(brand_id), ignore.case=T) & consec_fulfilled == T & ms_fulfilled==T, by = c('category','country',brand_id)]
  
  	# mark markets with only 1 brand (excluding the composite allother)
  	tmp_brands_select[, n_brands_selected := length(unique(get(brand_id)[selected_brand==T&!grepl('ALLOTHER', get(brand_id))])), by = c('category','country')]
  
  	return(tmp_brands_select)
  	
	}

  tmp_brands <- dat[which(selected_t_cat==T), list(brand_sales = sum(t_sales_units), time_periods = length(unique(date))), by=c('category','country',brand_id,time_id)]

  tmp_brands_select=sel_brands(tmp_brands, brand_id='brand')
  
  # rename "non-selected brands" as composite "allothers"
  tmp_brands_select[which(selected_brand==F), brand_rename:='ALLOTHERS']
  tmp_brands_select[which(selected_brand==T), brand_rename:=brand]
  
  setkey(tmp_brands_select, category,country,brand)
  setkey(dat, category,country,brand)
  dat[tmp_brands_select, brand_rename:=i.brand_rename]
  
	# apply selection rule for "allother" brand as well!
  tmp_brands2 <- dat[which(selected_t_cat==T), list(brand_sales = sum(t_sales_units), time_periods = length(unique(date))), by=c('category','country','brand_rename',time_id)]
  
  tmp_brands_select2=sel_brands(tmp_brands2, brand_id='brand_rename')
  
  # merge inclusion to allother brands from above
  setkey(tmp_brands_select, category,country,brand_rename)
  setkey(tmp_brands_select2, category, country, brand_rename)
  
  # update brand selection
  tmp_brands_select[tmp_brands_select2, selected_brand_upd:=i.selected_brand]

  tmp_brands_select[, composite_included:=any(brand_rename=='ALLOTHERS'&selected_brand_upd==T),by=c('category','country')]
  
  setkey(tmp_brands_select, category, country, brand)
	
	brand_selection <-  tmp_brands_select[, list(selected_brand = any(selected_brand_upd), n_brands_selected=unique(n_brands_selected), composite_included=unique(composite_included)), by=c('category','country','brand', 'brand_rename')]
	
	time_selection = tmp_sales[, c('category','country','date','selected_t_cat'),with=F]
	setkey(time_selection,category,country,date)
	
	return(list(brand_selection=brand_selection, time_selection=time_selection,
	            tmp_brands_select=tmp_brands_select, tmp_brands_select2=tmp_brands_select2))
	
	})
	
	names(selection) <- sel_applied
	
	
	save(selection, file='..//temp//select.RData')

	
	
	##########
	# report #
	##########
	
	for (i in names(selection)) {
  	#print(i)
  	tmp=selection[[i]]$tmp_brands_select[, list(marketshare_covered_by_selection=sum(brand_sales[which(selected_brand==T)])/sum(brand_sales),
  	                             marketshare_covered_by_selection_incl_composite=sum(brand_sales[which(selected_brand_upd==T)])/sum(brand_sales),
  								 n_brands_total=length(unique(brand)),
  								 n_brands_selected=length(unique(brand[which(selected_brand==T)])),
  								 n_brands_collapsed_to_allothers=length(unique(brand[which(brand_rename=='ALLOTHERS')])),
  								 composite_included=any(composite_included==T),
  								 n_brands_not_included=length(unique(brand[which(selected_brand==F)]))
  	               ), by=c('category','country')]
  	setorder(tmp, marketshare_covered_by_selection)
  
  	tmp[,selected_market:=marketshare_covered_by_selection>0&n_brands_selected>1]
  	# How many of the brands which are selected are still in the top 7 at the end of the sample
  	options(width=800)
  	options(max.print=10000)
  
    sink(paste0('../output/brand_selection_', i,'.txt'))
  	
  	{
  	cat(paste0('\nBrand selection rule\n====================================================================================\n\n'))
  	cat(paste0('Number of markets at this point of data preparation: ', nrow(tmp),'\n'))
  	cat(paste0('Number of selected markets (category-country combinations): ', nrow(tmp[selected_market==T]),'\n'))
  	
  	cat(paste0('Number of total market-brands: ', sum(tmp[selected_market==T]$n_brands_total),'\n'))
  	cat(paste0('Number of selected market-brands (excluding composite brands): ', sum(tmp[selected_market==T]$n_brands_selected),'\n'))
  	cat(paste0('Number of selected unique brands (excluding composite brands): ', length(unique(selection[[i]]$tmp_brands_select[n_brands_selected>1&selected_brand==T&!grepl('ALLOTHERS',brand_rename)]$brand)),'\n'))
  	
  	cat('\n')
  	
  	cat(paste0('Market share coverage for included brands in selected markets (1 = 100%):\n'))
  	print(summary(tmp[selected_market==T]$marketshare_covered_by_selection))
  	cat('\n')
  	
  	cat(paste0('Market share coverage for included brands in selected markets (1 = 100%), including composite brands:\n'))
  	print(summary(tmp[selected_market==T]$marketshare_covered_by_selection_incl_composite))
  	cat('\n')
  	
  	cat(paste0('Distribution of number of selected brands across all categories:'))
  	print(table(tmp[selected_market==T]$n_brands_selected))
  	cat('\n')
  	
  	cat(paste0('Summary of number of selected brands across all categories:'))
  	print(summary(tmp[selected_market==T]$n_brands_selected))
  	cat('\n')
  	
  	cat('Excluded markets:\n')
  	for (i in 1:nrow(tmp[selected_market==F])) {
  		with(tmp[selected_market==F][i,], cat(paste0(category, ' - ', country, '\n')))
  		}
  
  	setorder(tmp, marketshare_covered_by_selection)
  	cat(paste0('\nAll included markets (', nrow(tmp[selected_market==T]),'):\n'))
  	print(data.frame(tmp[selected_market==T]))
  
  	cat(paste0('\nMarkets for which composite brand is excluded (', nrow(tmp[composite_included==F&selected_market==T]),'):\n'))
  	print(data.frame(tmp[composite_included==F&selected_market==T]))
  	
  	cat(paste0('\nAll excluded markets (', nrow(tmp[selected_market==F]),'):\n'))
  	print(data.frame(tmp[selected_market==F]))
  	
  	}

	  sink()
	}
