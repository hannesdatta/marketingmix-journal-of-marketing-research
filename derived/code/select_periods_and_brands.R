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
# (a) We select the first (last) month in which category sales are higher than 5% of the maximum sales in any given month.
# (b) We select all brands that are in the top five for three consecutive years (36 months).
#     All remaining brands are modeled as a composite "other" brand.


#For each of these categories, the top three
#brands are considered, provided they obtain an average
#share of at least 5% over the sampling period.

#-->
#For each of these categories, the top five
#brands are considered, provided they obtain an average
#share of at least 5% in three consecutive years.



load('..\\temp\\uniqueness_and_lagsales.RData')
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
	tmp_sales[, selected_t:=cumsum(first+last)==1|last==1,by=c('category','country')]
	
	setkey(tmp_sales,category,country,date)
	
	# keep only names common across all data sets
	dat<-rbindlist(lapply(skus_by_date_list, function(x) x[,grep('category|country|brand|model|date|^t_.*|unique|[_]date', colnames(skus_by_date_list[[2]]),value=T),with=F]))
	setkey(dat, category,country,date)
	
	dat[tmp_sales, selected_t:=i.selected_t]
	
#############
# Auditing  #
#############

#tmp = dat[category=='camera_slr'&country=='SINGAPORE']


####################
# Brand selection  #
####################

	dat[, year := year(date)]
	tmp_brands <- dat[which(selected_t==T), list(brand_sales = sum(t_sales_units), time_periods = length(unique(date))), by=c('category','country','brand','year')]
	tmp_brands[, marketshare :=  brand_sales / sum(brand_sales), by=c('category','country','year')]
	
	setorderv(tmp_brands, c('category', 'country','year', 'brand_sales'),order=-1)
	tmp_brands[, sales_rank:=1:(.N),by=c('category', 'country', 'year')]
	

	# selection criteria: all brands for three consecutive years (at least 5*12 months), with at least 1% market share in three consec. years
	top_n = 1E6
	top_min_marketshare = .01 
	consec_years = 5
	
	tmp_brands[, top_n_TRUE := sales_rank%in%(1:top_n) & marketshare >= top_min_marketshare, by=c('category', 'country', 'year')]
	
	tmp_brands[, consec_obs_TRUE := as.numeric(0)]
	setorder(tmp_brands, category, country, brand, year)
	tmp_brands[which(top_n_TRUE==T), consec_obs_TRUE := c(diff(year),1), by=c('category','country','brand')]
		# checks whether there are at least three consecutive years with a top position in the data
	
	tmp_brands[, selected_brand := sum(consec_obs_TRUE)>=consec_years & sum(time_periods[consec_obs_TRUE==1])>=consec_years*12,by=c('category','country','brand')]
	tmp_brands[is.na(selected_brand), selected_brand:=F]
	
	setorderv(tmp_brands, c('category', 'country','brand'))
	
	tmp=tmp_brands[, list(marketshare=sum(brand_sales[which(selected_brand==T)])/sum(brand_sales), n_brands=length(unique(brand[which(selected_brand==T)]))), by=c('category','country')]
	setorder(tmp, marketshare)
	tmp
	
	#test=tmp_brands[category=='tv_gen1_crtv'&country=='JAPAN']
	#setorder(test,year,brand)
	
	
	# How many of the brands which are selected are still in the top 7 at the end of the sample
	{
	cat(paste0('\nSelection rule: All brands which are in the top ', top_n,' for at least ', consec_years, ' consecutive years\n====================================================================================\n\n'))
	cat(paste0('Number of markets: ', nrow(tmp),'\n'))
	cat(paste0('Number of selected markets: ', nrow(tmp[n_brands>0]),'\n'))
	#cat(paste0('Number of brands in total: ', ,'\n'))
	cat(paste0('Number of selected brands: ', sum(tmp[n_brands>0]$n_brands),'\n'))
	cat('\n')
	cat(paste0('Market share coverage in selection (1 = 100%):\n'))
	print(summary(tmp[n_brands>0]$marketshare))
	cat(paste0('Distribution of number of selected brands across all categories:'))
	print(table(tmp[n_brands>0]$n_brands))
	cat(paste0('Summary of number of selected brands across all categories:'))
	print(summary(tmp[n_brands>0]$n_brands))
	
	#cat(paste0('\nNumber (percentage) of brands still in top 7 in the last two years of the data set: ', nrow(tmp_brands[selected==T & rank_recent %in% 1:7]),'\n'))
	#cat(paste0('...in %: ', nrow(tmp_brands[selected==T & rank_recent %in% 1:topb])/nrow(tmp_brands[selected==T]),'\n'))
	}

	brand_selection <- tmp_brands[, list(selected_brand = any(selected_brand)), by=c('category','country','brand')]
	#setnames(brand_selection,'brand','brand_orig')
	brand_selection[which(selected_brand==F), brand_rename:='ALLOTHERS']
	brand_selection[which(!selected_brand==F), brand_rename:=brand]
	setkey(brand_selection,category,country,brand)
	
	# I have to put in the time periods of modeling, too!
	time_selection = tmp_sales[, c('category','country','date','selected_t'),with=F]
	setkey(time_selection,category,country,date)
	
	save(brand_selection, time_selection, file='..//temp//select_periods_and_brands.RData')


#######################
# Plot market cutoffs #
#######################
	
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

