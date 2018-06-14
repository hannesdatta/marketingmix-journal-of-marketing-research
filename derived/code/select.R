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
require(data.table)

# Load data
	load('..\\temp\\uniqueness_and_lagsales.RData')

# Assert: How many categories are passed on from the previous step? must be 174 (182-4-4)!
stopifnot(nrow(rbindlist(lapply(skus_by_date_list, function(x) x[, list(N=.N), by=c('category','country')])))==174)

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
	
#############
# Auditing  #
#############

#tmp = dat[category=='camera_slr'&country=='INDIA']

####################
# Brand selection  #
####################
	
	dat[, year := year(date)]
	
	months = dat[, list(.N), by = c('date')]
	setorder(months, date)
	months[, month := as.numeric(as.factor(as.character(date)))]
	setkey(months, date)
	setkey(dat, date)
	dat[months, month := i.month]
	
	
	# selection criteria: all brands with at least X% market share in X consec. years (at X*12 months)
	# with average yearly share of at least x%. 

	min_share = .01
	
	brand_id = 'brand'
	time_id = 'month'
	
	# aggregate brand sales for selected time periods (determined on a category-level earlier)
	tmp_brands <- dat[which(selected_t_cat==T), list(brand_sales = sum(t_sales_units), time_periods = length(unique(date))), by=c('category','country',brand_id,time_id)]
	
	tmp_brands[!category == 'tablets', consec_min := 5*12] #5
	tmp_brands[category == 'tablets', consec_min := 4*12] # 4
	
	# establish consecutive number of years
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
	
	tmp_brands_select[, selected_brand := consec_fulfilled == T & ms_fulfilled==T, by = c('category','country','brand')]

	# kick out markets with only 1 brand
	tmp_brands_select[, n_brands_selected := length(unique(get(brand_id)[selected_brand==T])), by = c('category','country')]

	#tmp_brands_select[n_brands_selected==1, selected_brand:=F]
	
	# rename "non-selected brands" as composite "allothers"
	tmp_brands_select[which(selected_brand==F), brand_rename:='ALLOTHERS']
	tmp_brands_select[which(selected_brand==T), brand_rename:=brand]
	setkey(tmp_brands_select, category, country, brand)
	
	brand_selection <-  tmp_brands_select[, list(selected_brand = any(selected_brand), n_brands_selected=unique(n_brands_selected)), by=c('category','country','brand', 'brand_rename')]
	
	#brand_selection[, n_brands := length(unique(brand_rename)), by = c('category','country')]
	#brand_selection[, n_brands_selected := length(unique(brand_rename[selected_brand==T])), by = c('category','country')]
	
	time_selection = tmp_sales[, c('category','country','date','selected_t_cat'),with=F]
	setkey(time_selection,category,country,date)
	
	save(brand_selection, time_selection, file='..//temp//select.RData')

	##########
	# report #
	##########
	
	tmp=tmp_brands_select[, list(marketshare_covered_by_selection=sum(brand_sales[which(selected_brand==T)])/sum(brand_sales),
								 n_brands_total=length(unique(brand)),
								 n_brands_selected=length(unique(brand[which(selected_brand==T)])),
								 n_brands_collapsed_to_allothers=length(unique(brand[which(brand_rename=='ALLOTHERS')])),
								 n_brands_not_included=length(unique(brand[which(selected_brand==F)]))
	               ), by=c('category','country')]
	setorder(tmp, marketshare_covered_by_selection)

	tmp[,selected_market:=marketshare_covered_by_selection>0&n_brands_selected>1]
	# How many of the brands which are selected are still in the top 7 at the end of the sample
	options(width=800)
	options(max.print=10000)
	sink('../output/brand_selection.txt')
	
	
	{
	cat(paste0('\nBrand selection rule\n====================================================================================\n\n'))
	cat(paste0('Number of markets at this point of data preparation: ', nrow(tmp),'\n'))
	cat(paste0('Number of selected markets (category-country combinations): ', nrow(tmp[selected_market==T]),'\n'))
	
	cat(paste0('Number of selected market-brands: ', sum(tmp[selected_market==T]$n_brands_selected),'\n'))
	cat(paste0('Number of selected unique brands: ', length(unique(tmp_brands_select[n_brands_selected>1&selected_brand==T]$brand)),'\n'))
	
	cat('\n')
	
	cat(paste0('Market share coverage for included brands in selected markets (1 = 100%):\n'))
	print(summary(tmp[selected_market==T]$marketshare_covered_by_selection))
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
	cat('\nAll included markets:\n')
	print(data.frame(tmp[selected_market==T]))

	cat('\nAll excluded markets:\n')
	print(data.frame(tmp[selected_market==F]))
	
	}

	sink()


#############################################################################################
# Plot market cutoffs (i.e., which periods are selected, compared to overall category sales #
#############################################################################################
	
if(0){	
	# overview
	tmp=tmp_sales[, list(N=.N, N_sel=length(which(selected==T))),by=c('category', 'country')]
	
	
	bymarket <- split(tmp_sales, paste(tmp_sales$category,tmp_sales$country,sep='_'))
	
	fpath = '../audit/markets_cutoff/'
	dir.create(fpath)
	unlink(paste0(fpath,'*.png'))

	for (i in 1:length(bymarket)) {
		x=bymarket[[i]]
		plotname =  paste(unique(x$category), unique(x$country),sep=' - ')
		anydiff = any(x$n_diff)
		if (anydiff==T) fn=paste('cutoff_',plotname,'.png',sep='')
		if (anydiff==F) fn=paste('nocutoff', plotname,'.png',sep='')
		
		png(paste(fpath,fn,sep=''), res=150, units='in', height=8, width=16)

		with(x, plot(date, total_sales,type='l', main = plotname))
		abline(v=x$date[which(x$first==T)],col='red')
		abline(v=x$date[which(x$last==T)],col='red')
		dev.off()
		}

}	

