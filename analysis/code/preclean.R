#     _____    __   _  __                               _                 _       
#    / ____|  / _| | |/ /       /\                     | |               (_)      
#   | |  __  | |_  | ' /       /  \     _ __     __ _  | |  _   _   ___   _   ___ 
#   | | |_ | |  _| |  <       / /\ \   | '_ \   / _` | | | | | | | / __| | | / __|
#   | |__| | | |   | . \     / ____ \  | | | | | (_| | | | | |_| | \__ \ | | \__ \
#    \_____| |_|   |_|\_\   /_/    \_\ |_| |_|  \__,_| |_|  \__, | |___/ |_| |___/
#                                                            __/ |                
#                                                           |___/                 
#
#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|

# Purpose: Preclean data

### LOAD DATA SETS
library(data.table)

ds <- list.files('../../derived/output/', pattern='data.*csv')


### Stack data in data.table
for (fn in ds) {
	brand_panel=fread(paste0('../../derived/output/', fn))
	brand_panel[, ':=' (date = as.Date(date))]
	
	
	# lag variables
	for (.var in c('rwpspr', 'wpswdst','llen',grep('nov[0-9]+sh', colnames(brand_panel),value=T), 'usales')) {
	  brand_panel[, paste0('lag', .var) := c(NA, get(.var)[-.N]), by = c('market_id', 'brand')]
	}
	
	# merge development indicators
	dev_indicators <- fread('../../derived/output/dev_indicators.csv')
	brand_panel <- merge(brand_panel, dev_indicators, by = c('year', 'country'), all.x=T)
	
	# set 2010 values
	tmp=copy(dev_indicators[year==2010])
	setnames(tmp, paste0(colnames(tmp), '2010'))
	brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('country2010'), all.x=T)
	
	if (!grepl('[_]agg', fn)) {
	  
	  # calculate growth rates in markets
  	growth = brand_panel[, list(sales=sum(usales,na.rm=T)), by = c('market_id', 'category', 'country', 'date')]
  	growth[, year:=year(date)]
  	growth[, months_with_sales:=length(which(sales>0)), by = c('market_id', 'year')]
  	
  	growth[, N:=1:.N,by = c('market_id')]
  	windowsize=3
  	suppressWarnings(growth[, catvolatility_range:=sapply(N, function(x) max(sales[N<x&(N>=(x-windowsize))])-min(sales[N<x&(N>=(x-windowsize))])), by = c('market_id')])
  	growth[, catvolatility_sd:=sapply(N, function(x) sd(sales[N<x&(N>=(x-windowsize))])), by = c('market_id')]
  	growth[N<=windowsize, ':=' (catvolatility_range=NA, catvolatility_sd=NA)]
  	
  	setkey(brand_panel, market_id, date)
  	setkey(growth, market_id, date)
  	brand_panel[growth, ':=' (catvolatility_range=i.catvolatility_range, catvolatility_sd=i.catvolatility_sd)]
  	
  	# remove incomplete years
  	growth = growth[months_with_sales==12]
  	growth[, year_to_year_growth := sales/c(NA, sales[-.N]), by = c('market_id')]
  	tmp=growth[, list(sales_first=sum(sales[year==min(year)]),
  	              sales_last=sum(sales[year==max(year)]),
  	              nyears = max(year)-min(year)+1,
  	              sd_growth=sd(year_to_year_growth, na.rm=T)), by = c('market_id', 'category', 'country')]
  	tmp[, growth := (sales_last/sales_first)^(1/nyears)]
  	setorder(tmp, growth)
  	
    setkey(tmp, market_id)
    setkey(brand_panel, market_id)
    brand_panel[tmp, ':=' (market_growth = i.growth, market_sdgrowth = i.sd_growth)]
    
    # calculate (for the complete sample) market shares and price indices
    overall_metrics = brand_panel[, list(sumunits=sum(usales, na.rm=T),
                                         sumvalue=sum(rvsales, na.rm=T)), by = c('market_id', 'brand')]
    overall_metrics[, ':=' (overall_ms = sumunits/sum(sumunits,na.rm=T),
                            overall_avglocalrpr = sumvalue/sumunits), by = c('market_id')]
    
    # rank market shares
    setorderv(overall_metrics, c('market_id', 'overall_ms'), order=-1L)
    overall_metrics[, rank_ms:=.N-rank(overall_ms)+1, by = c('market_id')]
    
    # compute price indices (devided by mean)
    overall_metrics[, ':=' (overall_rprindex = overall_avglocalrpr/max(overall_avglocalrpr),
  					      overall_rprindexavg = overall_avglocalrpr/mean(overall_avglocalrpr)),
  						  by = c('market_id')]
    
    
    setkey(brand_panel, market_id, brand)
    setkey(overall_metrics, market_id, brand)
    
    brand_panel[overall_metrics, ':=' (brand_ms=i.overall_ms, brand_prindex_max = i.overall_rprindex,
                                       brand_prindex_mean = i.overall_rprindexavg)]
    
    concentration=overall_metrics[, list(market_herf = sum(overall_ms^2), 
                      market_c3=sum(overall_ms[rank_ms<=3])/sum(overall_ms),
                      market_c5=sum(overall_ms[rank_ms<=5])/sum(overall_ms)), by = c('market_id')]
    brand_panel <- merge(brand_panel, concentration, by = c('market_id'), all.x=T, all.y=F)
     
	}
	
  # keep selected brands and ALLOTHER brands
  brand_panel <- brand_panel[selected==T]
  
  brand_panel[, usales_incr:=ifelse(usales==0, usales+.01, usales)]
  
  if (!grepl('[_]agg', fn)) {
  	brand_panel[, usalessh := usales_incr/sum(usales_incr,na.rm=T), by=c('category', 'country', 'date')]
  	brand_panel[, vsalessh := vsales/sum(vsales,na.rm=T), by=c('category', 'country', 'date')]
  	brand_panel[, month_no := as.numeric(as.factor(date))]
  	
  	brand_panel[brand=='allothers', brand := paste0('allothers', .GRP), by = c('category', 'country')]
  }

	# add %-indicator to separate data in moving windows
	brand_panel[, N:=.GRP, by = c('market_id', 'date')]
	brand_panel[, share_obs:=(N-min(N)+1) / (max(N)-min(N)+1), by = c('market_id')]
	brand_panel[, N:=NULL]
	
	
	tmp=brand_panel[selected_t_cat==T&selected_brand==T, list(usales=sum(usales,na.rm=T)),by=c('market_id','date')]
	tmp[, percentile_obs:=(1:.N)/.N, by = c('market_id')]
	
	setkey(tmp, market_id, date)
	setkey(brand_panel, market_id, date)
	brand_panel[tmp, percentile_obs:=i.percentile_obs]
	setorder(brand_panel, market_id, brand, date)
	
		
#	elast[, brand:=my_capitalize(brand)]
	
	# Save file
	dir.create('../temp')
	fwrite(brand_panel, paste0('../temp/', gsub('datasets', 'preclean', fn)))
}
sink('../temp/preclean.txt')
cat('done\n')
sink()