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
	brand_panel[, quarter := quarter(date)]
	brand_panel[, year:=year(date)]
	
	attr= grep('^attr',colnames(brand_panel),value=T)
	attrnew=attr
	
	
	attrnew = gsub('[<][=]','smeqth', attrnew)
	attrnew = gsub('[>][=]','greqth', attrnew)
	attrnew = gsub('[>]','grth', attrnew)
	attrnew = gsub('[<]','smth', attrnew)
	attrnew = gsub('[/]','_', attrnew)
	attrnew = gsub('[/]','_', attrnew)
	attrnew = gsub('[-]','', attrnew)
	attrnew = gsub('[ ]','', attrnew)
	attrnew = tolower(attrnew)
	
	for (i in seq(along=attrnew)) setnames(brand_panel,attr[i], attrnew[i])
	
	
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
    
    
    # Load brand-country classifications
    brands_countries <- fread('../../../../data/brands_countries/brands_countries.tsv')
    setkey(brands_countries, brand)
    
    brand_panel[, ncountries:=length(unique(country)), by = c('brand')]
    brand_panel[, globalbrand:=ncountries>2 & !brand=='unbranded']
    
    brand_panel[, ncat_in_country:=length(unique(category)), by = c('brand','country')]
    brand_panel[, ncountry_in_category:=length(unique(country)), by = c('brand','category')]
    
    
    # merge country of origins for brands
    brands_countries <- brands_countries[!brand=='']
    brands_countries[country_cleaned=='', country_cleaned:=NA]
    
    setkey(brand_panel, brand)
    setkey(brands_countries, brand)
    
    brand_panel[brands_countries, country_of_origin:=i.country_cleaned]
    
    
    # Domestic brands / local_to_market
    brand_panel[, local_to_market:=as.numeric(country_of_origin==country & ncountries==1)]#
    brand_panel[, local_multip_market:=as.numeric(country_of_origin==country&ncountries>1)]
    
    # Brand equity
    #brand_panel[, brandz:=0]
    #brandz_brands<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone','lenovo', 'haier', 'midea', 'hisense')
    #brand_panel[brand%in%brandz_brands, brandz:=1]
    
    brand_panel[, brandz:=0]
    brandz_brands_globalonly<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone')
    brand_panel[brand%in%brandz_brands_globalonly, brandz:=1]
    
    #brand_panel[, brandz_chinaonly:=0]
    #brand_panel[brand%in%c('lenovo', 'haier', 'midea', 'hisense'), brandz_chinaonly:=1]
    
    #brand_panel[, brandz_global_alltime:=0]
    #brandz_brands_all<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone', 'canon', 'motorola')
    #brand_panel[brand%in%brandz_brands_all, brandz_global_alltime:=1]
    
    brand_panel[, brandz_financial500:=0]
    brandz_brands_fin<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone', 'canon', 'motorola',
                         'fujifilm', 'huawei', 'lenovo', 'lg', 'panasonic', 'philips', 'toshiba', 'zte')
    brand_panel[brand%in%brandz_brands_fin, brandz_financial500:=1]
    
    brand_panel[, interbrand:=0]
    
    interbrand_brands <- c('apple', 'dell', 'ge', 'hp', 'ibm', 'nokia', 'samsung', 'siemens', 'sony', 'canon', 'motorola', 'blackberry', 'htc', 'huawei', 'kodak', 'lg', 'panasonic', 'philips') 
    brand_panel[brand%in%interbrand_brands, interbrand:=1]
    
    brand_panel[, brandequity_interbr_brandz := ifelse(interbrand+brandz_financial500+brandz>0,1,0)]
    
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

	# Define broad country/categories classifications
	brand_panel[grepl('camera', category), cat_class := 'cam']
	brand_panel[grepl('phones|tablets', category), cat_class := 'ph']
	brand_panel[grepl('desktoppc|laptop', category), cat_class := 'cp']
	brand_panel[grepl('tv[_]|dvd', category), cat_class := 'tvdvd']
	brand_panel[grepl('washing|cooling|microwave', category), cat_class := 'wte']
	
	brand_panel[, appliance:=0]
	brand_panel[grepl('washing|cooling|microwave', category), appliance:=1]
	
	brand_panel[country %in% c('australia', 'hong kong', 'japan', 'new zealand', 'singapore', 'south korea', 'taiwan'), country_class := 'hinc']
	brand_panel[is.na(country_class), country_class := 'linc']
	
	# add %-indicator to separate data in moving windows
	brand_panel[, N:=.GRP, by = c('market_id', 'date')]
	brand_panel[, share_obs:=(N-min(N)+1) / (max(N)-min(N)+1), by = c('market_id')]
	brand_panel[, N:=NULL]
	
	# preprocess brand_panel attr
	for (var in grep('^attr', colnames(brand_panel),value=T)) {
	  if (min(brand_panel[, var,with=F],na.rm=T)==0&max(brand_panel[, var,with=F],na.rm=T)<=1) {
	    brand_panel[, (paste0(var)):=get(var)*100+1]
	  }  
	}
	
	# preprocess brand_panel attr
	for (var in grep('^attr', colnames(brand_panel),value=T)) {
	  if (min(brand_panel[, var,with=F],na.rm=T)==0&max(brand_panel[, var,with=F],na.rm=T)>1) {
	    brand_panel[, (paste0(var)):=get(var)+1]
	  }  
	}
	
	tmp=brand_panel[selected_t_cat==T&selected_brand==T, list(usales=sum(usales,na.rm=T)),by=c('market_id','date')]
	tmp[, percentile_obs:=(1:.N)/.N, by = c('market_id')]
	
	setkey(tmp, market_id, date)
	setkey(brand_panel, market_id, date)
	brand_panel[tmp, percentile_obs:=i.percentile_obs]
	setorder(brand_panel, market_id, brand, date)
	
	brand_panel[, hedon := category %in% c('tablets', 'phones_smart', 'phones_mobile', 'camera_slr', 'camera_compact', 'dvd', 'tv_gen1_crtv', 'tv_gen2_lcd')]
	
	# elast asian devel vs. not
	#elast[, region_of_origin:=country_of_origin]
	#elast[region_of_origin=='asian'& country_class=='hinc', region_of_origin := 'asian-high']
	#elast[region_of_origin=='asian'& country_class=='linc', region_of_origin := 'asian-low']
	
	brand_panel[, developed:=0]
	brand_panel[country%in%c('australia', 'singapore', 'japan', 'new zealand', 'hong kong', 'south korea', 'taiwan'), developed := 1]
	
	brand_panel[, emerging:=1-developed]
	

	brand_panel[, worldbank := '']
	brand_panel[country%in%c('india','indonesia', 'vietnam', 'philippines'), worldbank:='lowermid']
	brand_panel[country%in%c('china', 'malaysia','thailand'), worldbank:='uppermid']
	brand_panel[worldbank=='', worldbank:='high']
	
	# add world bank classifications
	brand_panel[, wb_lowermid:=country%in%c('india','indonesia', 'vietnam', 'philippines')]
	brand_panel[, wb_uppermid:=country%in%c('china', 'malaysia','thailand')]
		
#	elast[, brand:=my_capitalize(brand)]
	
	# Save file
	dir.create('../temp')
	fwrite(brand_panel, paste0('../temp/', gsub('datasets', 'preclean', fn)))
}
sink('../temp/preclean.txt')
cat('done\n')
sink()