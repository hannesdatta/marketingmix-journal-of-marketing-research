load('../temp/brand_metrics.RData')
library(data.table)

unlink('../temp/dataset*')

for (seldat in names(all_datasets)) {

	all_data <- all_datasets[[seldat]]

	####################
	# BRAND-LEVEL DATA #
	####################
	
	# Prepare flat CSV file with data
	brand_panel=rbindlist(lapply(all_data, function(x) if(!is.null(x)) return(rbindlist(x$data_cleaned))),fill=T)

	# Create brand IDs
	brand_panel[, brand_id:=.GRP, by = c('market_id', 'brand')]
	
	# Add quarter and year variables
	brand_panel[, quarter := quarter(date)]
	brand_panel[, year:=year(date)]
	
	##############
	# Attributes #
	##############
	
	# Rename
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
	
	# Scale shares between 0 and 100
	for (var in grep('^attr', colnames(brand_panel),value=T)) {
	  if (min(brand_panel[, var,with=F],na.rm=T)==0&max(brand_panel[, var,with=F],na.rm=T)<=1) {
	    brand_panel[, (paste0(var)):=get(var)*100+1]
	  }  
	}
	
	# Add 1 for variables that contain 0s
	for (var in grep('^attr', colnames(brand_panel),value=T)) {
	  if (min(brand_panel[, var,with=F],na.rm=T)==0&max(brand_panel[, var,with=F],na.rm=T)>1) {
	    brand_panel[, (paste0(var)):=get(var)+1]
	  }  
	}
	
	
	###################################
	# Define category-level meta data #
	###################################
	
	brand_panel[grepl('camera', category), cat_class := 'cam']
	brand_panel[grepl('phones|tablets', category), cat_class := 'ph']
	brand_panel[grepl('desktoppc|laptop', category), cat_class := 'cp']
	brand_panel[grepl('tv[_]|dvd', category), cat_class := 'tvdvd']
	brand_panel[grepl('washing|cooling|microwave', category), cat_class := 'wte']
	
	brand_panel[, appliance:=0]
	brand_panel[grepl('washing|cooling|microwave', category), appliance:=1]
	
	brand_panel[, hedon := category %in% c('tablets', 'phones_smart', 'phones_mobile', 'camera_slr', 'camera_compact', 'dvd', 'tv_gen1_crtv', 'tv_gen2_lcd')]
	
	##################################
	# Define country-level meta data #
	##################################
	
	brand_panel[country %in% c('australia', 'hong kong', 'japan', 'new zealand', 'singapore', 'south korea', 'taiwan'), country_class := 'hinc']
	brand_panel[is.na(country_class), country_class := 'linc']
	
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
	
	
	################################
	# Define brand-level meta data #
	################################
	
	# Load brand-country classifications
	brands_countries <- fread('../../../../data/country_of_origin/country_of_origin.tsv')
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
	
	brand_panel[, asian_brand:=0]
	brand_panel[country_of_origin%in%c('south korea', 'japan','taiwan', 'thailand', 'indonesia', 'philippines',
	                             'india', 'singapore', 'malaysia', 'vietnam', 'cambodja', 'pakistan', 
	                             'hong kong'), asian_brand:=1]
	
	
	# country classifications
	western=c('australia', 'canada','finland','france', 'germany','great britain', 
	          'italy','luxembourg', 'netherlands','new zealand', 'spain', 'sweden',
	          'switzerland', 'turkey', 'usa')
	
	brand_panel[, western_brand:=as.numeric(country_of_origin%in%western)]
	
	jbcountries=c('japan', 'usa','switzerland','germany','sweden') #'japan', 
	
	brand_panel[, `brand_from_jp-us-ch-ge-sw`:=as.numeric(country_of_origin%in%jbcountries)]
	
	brand_panel[, other_brand:=1-asian_brand-western_brand]
	
	
	# Brand equity metrics
	brand_panel[, brandz:=0]
	brandz_brands_globalonly<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone')
	brand_panel[brand%in%brandz_brands_globalonly, brandz:=1]
	
	brand_panel[, brandz_financial500:=0]
	brandz_brands_fin<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone', 'canon', 'motorola',
	                     'fujifilm', 'huawei', 'lenovo', 'lg', 'panasonic', 'philips', 'toshiba', 'zte')
	brand_panel[brand%in%brandz_brands_fin, brandz_financial500:=1]
	
	brand_panel[, interbrand:=0]
	
	interbrand_brands <- c('apple', 'dell', 'ge', 'hp', 'ibm', 'nokia', 'samsung', 'siemens', 'sony', 'canon', 'motorola', 'blackberry', 'htc', 'huawei', 'kodak', 'lg', 'panasonic', 'philips') 
	brand_panel[brand%in%interbrand_brands, interbrand:=1]
	
	brand_panel[, brandequity_interbr_brandz := ifelse(interbrand+brandz_financial500+brandz>0,1,0)]
	
	################################
	# INTERNALLY GENERATED METRICS #
	################################
	
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
	growth = growth[months_with_sales==12][, list(sales=sum(sales,na.rm=T)),by=c('market_id','category','country','year')]
	
	growth[, year_to_year_growth := sales/c(NA, sales[-.N]), by = c('market_id')]
	tmp=growth[, list(sales_first=sum(sales[year==min(year)]),
	                  sales_last=sum(sales[year==max(year)]),
	                  nyears = max(year)-min(year)+1,
	                  sd_growth=sd(year_to_year_growth, na.rm=T),
	                  mean_growth=mean(year_to_year_growth,na.rm=T)), by = c('market_id', 'category', 'country')]
	tmp[, growthn := (sales_last/sales_first)^(1/nyears)]
	tmp[, growth := (sales_last/sales_first)^(1/(nyears-1))]
	
	setorder(tmp, growth)
	
	setkey(tmp, market_id)
	setkey(brand_panel, market_id)
	brand_panel[tmp, ':=' (market_growth = i.growth, 
	                       market_growthn = i.growthn,
	                       market_sdgrowth = i.sd_growth,
	                       market_meangrowth= i.mean_growth)]
	
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
	
	###############
	# FINAL TOUCH #
	###############
	
	# Order data
	setorder(brand_panel, market_id, category,country,brand,date)

	brand_panel[which(!is.na(usales) & selected_t_brand==T & selected_brand == T), prelim_selected:=T, by=c('category', 'country', 'brand')]
	brand_panel[is.na(prelim_selected), prelim_selected:=F, by=c('category', 'country', 'brand')]

	# For which markets do we only see 1 brand?
	tmp = brand_panel[prelim_selected==T,list(sumsales=sum(usales, na.rm=T), nbrand=length(unique(brand))),by=c('market_id','category', 'country','date')]
	setorder(tmp, category, country,date)

	tmp[, above:=as.numeric(1:.N%in%first(which(nbrand>1))), by=c('category','country')]
	tmp[, below:=as.numeric(1:.N%in%(1+last(which(nbrand>1))))*(-1), by=c('category','country')]
	tmp[, obs:=cumsum(above+below),by=c('category','country')]

	dir.create('../audit')
	
	time_select_dir = paste0('/time-selection_', seldat)
	
	dir.create(paste0('../audit', time_select_dir))
	unlink(paste0(paste0('../audit', time_select_dir), '/*'))

	for (i in unique(tmp$market_id)) {
	  fn=paste0(unique(tmp[market_id==i]$category), ' - ', unique(tmp[market_id==i]$country), ' (', i, ')', ifelse(any(tmp[market_id==i]$nbrand==1),' - affected',''))
	  
	  png(paste0('../audit', time_select_dir, '/', fn,'.png'), res=200, units='in', height=8, width=16)
	  
	  with(tmp[market_id==i], plot(x=date, y=sumsales, type='l', main = fn, xlab='date',ylab='sum of sales'))
	  with(tmp[market_id==i], abline(v=date[which(above==1)],col='red'))
	  with(tmp[market_id==i], abline(v=date[which(below==c(-1))],col='red'))
	  
	  dev.off()
	  
	}

	# update time selection
	setkey(tmp, market_id,date)
	setkey(brand_panel, market_id,date)

	brand_panel[tmp, tselect:=i.obs==1]

	brand_panel[,selected_t_cat_1brand:=selected_t_cat]
	brand_panel[tselect==F, selected_t_cat_1brand:=F]
	brand_panel[,tselect:=NULL]

	brand_panel[,selected:=ifelse(prelim_selected==T&selected_t_cat_1brand==T, T, F)]

	# Prepare CSV file with data
	fwrite(brand_panel, file = paste0('..\\temp\\datasets_', seldat, '.csv'), row.names=F)

	# Load GDP per capita, and put into data sets
	#load('..\\temp\\gdppercap.RData')
	#  gdppercap[, lngdppercap:=log(gdppercap)]
	#  gdppercap[, dlngdppercap:=lngdppercap-c(NA,lngdppercap[-.N]),by=c('country')]
 
	brand_panel[, year:=year(date)]
	#brand_panel = merge(brand_panel, gdppercap, by = c('country', 'year'), all.x=T)
	
	# Save complete data as .RData
	#save(all_data, gdppercap, brand_panel, file =  '..\\output\\datasets.RData')

	# In which categories does the first sales NOT correspond with selected t in category
	tmp=brand_panel[, list(first_sales=min(date[!is.na(usales)]), first_tcat=min(date[selected==T],na.rm=T),
	                       last_tcat=max(date[selected==T],na.rm=T)), by = c('market_id', 'category', 'country')]
	tmp[!first_sales==first_tcat]

	
	}

sink('../temp/datasets.txt')
cat('done prepping datasets: ', paste0(names(all_datasets), collapse=', '))
sink()
