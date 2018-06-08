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
# (a) the time period for a category, and
# (b) which brands will be included in this study

# Rules: 
# (a) We select the first (last) month in which category sales are higher than x% of the maximum sales in any given month.
# (b) We select all brands that are in the top x (1, 2, 3, 4, 5...) for x consecutive years (in months).
#     All remaining brands are modeled as a composite "allothers" brand.

# "For each of these categories, the top X brands are considered, provided they obtain an average
# share of at least x% in x consecutive years."



# Load data
	load('..\\temp\\uniqueness_and_lagsales.RData')

# Load packages
	require(data.table)
	
# Apply category names
	for (i in 1:length(skus_by_date_list)) {
		skus_by_date_list[[i]][, category:=names(skus_by_date_list)[i]]
		}
	
##################################################
### Determine start and end of modeling period ###
##################################################
	
	tmp_sales = rbindlist(lapply(skus_by_date_list, function(x) x[,list(total_sales=sum(t_sales_units, na.rm=T)),by=c('category', 'country','date')]))

	tmp_sales[, max_sales := max(total_sales),by=c('category','country')]
	tmp_sales[, indic_max_sales := total_sales==max_sales,by=c('category','country')]
	tmp_sales[, indic_above_threshold := total_sales>.05*max_sales,by=c('category','country')]
	table(tmp_sales$indic_above_threshold)
	setorderv(tmp_sales, c('category','country','date'), order=1L)
	
	# select observations
	tmp_sales[, first:=cumsum(cumsum(indic_above_threshold))==1,by=c('category','country')]
	tmp_sales[, last:=rev(cumsum(cumsum(rev(indic_above_threshold)))==1),by=c('category','country')]
	tmp_sales[, selected_t_cat:=cumsum(first+last)==1|last==1,by=c('category','country')]
	
	setkey(tmp_sales,category,country,date)
	
	# keep only names common across all data sets
	dat<-rbindlist(lapply(skus_by_date_list, function(x) x[,grep('category|country|brand|model|date|^t_.*|unique|[_]date', colnames(skus_by_date_list[[2]]),value=T),with=F]))
	setkey(dat, category,country,date)
	
	dat[tmp_sales, selected_t_cat:=i.selected_t_cat]
	
#############
# Auditing  #
#############

#tmp = dat[category=='camera_slr'&country=='SINGAPORE']


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
	
	
	min_share = .01
	
	# selection criteria: all brands with at least X% market share in X consec. years (at X*12 months)
	# with average share of at least x%. 
	
	# @hannes, make sure we don't use zeros for brands which are not yet in the market
	
	brand_id = 'brand'
	time_id = 'month'
	exclude_categories = c('minioven', 'tumbledryers')
	
	# aggregate brand sales for selected time periods (determined on a category-level earlier)
	tmp_brands <- dat[which(selected_t_cat==T), list(brand_sales = sum(t_sales_units), time_periods = length(unique(date))), by=c('category','country',brand_id,time_id)]
	if (length(exclude_categories)>0) tmp_brands = tmp_brands[!category%in%exclude_categories]
	
	tmp_brands[!category == 'tablets', consec_min := 5*12]
	tmp_brands[category == 'tablets', consec_min := 4*12]
	
	# establish consecutive number of years
	setorderv(tmp_brands, c('category', 'country',time_id, 'brand_sales'),order=-1)
	tmp_brands[, obs_difference := as.numeric(0)]
	setorderv(tmp_brands, c('category', 'country', brand_id, time_id))
	tmp_brands[, obs_difference := c(diff(get(time_id)),1), by=c('category','country',brand_id)]
	tmp_brands[, cumsum_obsdiff := cumsum(obs_difference-1), by=c('category','country',brand_id)]
	tmp_brands[, obs := sum(time_periods[obs_difference==1 | !obs_difference==1&.I==.N]), by=c('category','country',brand_id, 'cumsum_obsdiff')]
	
	tmp_brands_select = tmp_brands[, list(brand_sales=sum(brand_sales), consec_fulfilled = any(obs >= consec_min)),by=c('category','country',brand_id)]
	tmp_brands_select[is.na(consec_fulfilled), consec_fulfilled:=F]
	setorderv(tmp_brands_select, c('category', 'country',brand_id))

	# calculate market shares by brand
	tmp_brands_select[, marketshare := brand_sales / sum(brand_sales), by=c('category','country')]
	tmp_brands_select[, ms_fulfilled := marketshare >= min_share]
	
	tmp_brands_select[, selected_brand := consec_fulfilled == T & ms_fulfilled==T, by = c('category','country','brand')]
	
	# rename "non-selected brands" as composite "allothers"
	tmp_brands_select[which(selected_brand==F), brand_rename:='ALLOTHERS']
	tmp_brands_select[which(selected_brand==T), brand_rename:=brand]
	setkey(tmp_brands_select, category, country, brand)
	
	brand_selection <-  tmp_brands_select[, list(selected_brand = any(selected_brand)), by=c('category','country','brand', 'brand_rename')]
	
	# kick out markets with only 1 brand
	brand_selection[, n_brands := length(unique(brand_rename)), by = c('category','country')]
	
	
	time_selection = tmp_sales[, c('category','country','date','selected_t_cat'),with=F]
	setkey(time_selection,category,country,date)
	
	save(brand_selection, time_selection, file='..//temp//select.RData')

	##########
	# report #
	##########
	
	tmp=tmp_brands_select[, list(marketshare_tot=sum(brand_sales[which(selected_brand==T)])/sum(brand_sales),
								 marketshare_brands=sum(brand_sales[which(selected_brand==T & !brand_rename == 'ALLOTHERS')])/sum(brand_sales),
							     marketshare_brands_consec=sum(brand_sales[which(selected_brand==T & !brand_rename == 'ALLOTHERS')])/sum(brand_sales[which(consec_fulfilled == T)]),
							     n_brands=length(unique(brand_rename[which(selected_brand==T & !brand_rename == 'ALLOTHERS')])),
								 n_brands_compos=length(unique(brand[which(selected_brand==T & brand_rename == 'ALLOTHERS')])),
								 n_brands_exclud=length(unique(brand[which(selected_brand==F)]))
								 ), by=c('category','country')]
	setorder(tmp, marketshare_tot)
	tmp
	
	# How many of the brands which are selected are still in the top 7 at the end of the sample
	sink('../output/brand_selection.txt')
	
	{
	cat(paste0('\nBrand selection rule\n====================================================================================\n\n'))
	cat(paste0('Number of markets: ', nrow(tmp),'\n'))
	cat(paste0('Number of selected markets (category-country combinations): ', nrow(tmp[marketshare_tot>0]),'\n'))
	
	#cat(paste0('Number of brands in total: ', ,'\n'))
	cat(paste0('Number of selected individual brands: ', sum(tmp[marketshare_tot>0]$n_brands),'\n'))
	cat(paste0('Number of selected composite brands: ', nrow(tmp[marketshare_tot>0 & n_brands_compos>0]),'\n'))
	cat(paste0('Number of excluded composite brands: ', nrow(tmp[marketshare_tot>0 & n_brands_compos==0]),'\n'))
	cat('\n')
	
	cat(paste0('Market share coverage for individual brands in the complete market (1 = 100%):\n'))
	print(summary(tmp[marketshare_tot>0]$marketshare_brands))
	cat('\n')
	
	cat(paste0('Market share coverage for all included brands, including the composite brand (1 = 100%):\n'))
	print(summary(tmp[marketshare_tot>0]$marketshare_tot))
	
	cat(paste0('Market share coverage for individual brands in the market fulfilling min. consec. years criteria (1 = 100%):\n'))
	print(summary(tmp[marketshare_tot>0]$marketshare_brands_consec))
	cat('\n')
	
	cat(paste0('Distribution of number of selected brands across all categories (excluding composite brand):'))
	print(table(tmp[marketshare_tot>0]$n_brands))
	cat('\n')
	
	cat(paste0('Summary of number of selected brands across all categories:'))
	print(summary(tmp[marketshare_tot>0]$n_brands))
	cat('\n')
	
	cat('Excluded markets:\n')
	for (i in 1:nrow(tmp[marketshare_tot==0])) {
		with(tmp[marketshare_tot==0][i,], cat(paste0(category, ' - ', country, '\n')))
		}

	setorder(tmp, marketshare_brands)
	cat('\nAll included markets:\n')
	print(data.frame(tmp[marketshare_tot>0]))

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

