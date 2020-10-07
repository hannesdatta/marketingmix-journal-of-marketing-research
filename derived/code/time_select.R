load('../temp/brand_metrics.RData')
library(data.table)

holidays <- fread('../../../../data/holidays/holidays.csv')

holidays[country=='AU', country:='australia']
holidays[country=='CN', country:='china']
holidays[country=='HK', country:='hong kong']
holidays[country=='IN', country:='india']
holidays[country=='ID', country:='indonesia']
holidays[country=='JP', country:='japan']
holidays[country=='MY', country:='malaysia']
holidays[country=='NZ', country:='new zealand']
holidays[country=='PH', country:='philippines']
holidays[country=='SG', country:='singapore']
holidays[country=='KO', country:='south korea']
holidays[country=='TW', country:='taiwan']
holidays[country=='TH', country:='thailand']
holidays[country=='VN', country:='vietnam']

holidays[, month:=as.Date(paste0(substr(date, 1,7),'-01'))]

holiday_period= holidays[public==T, list(N=.N),by=c('country', 'month')]


for (seldat in names(all_datasets)) {

	all_data <- all_datasets[[seldat]]

	####################
	# BRAND-LEVEL DATA #
	####################
	
	# Prepare flat CSV file with data
	brand_panel=rbindlist(lapply(all_data, function(x) if(!is.null(x)) return(rbindlist(x$data_cleaned))),fill=T)

	# Create brand IDs
	brand_panel[, brand_id:=.GRP, by = c('market_id', 'brand')]
	
	# Merge holiday data
	setkey(brand_panel, country, date)
	setkey(holiday_period, country, month)
	brand_panel[holiday_period, npublicholidays:=i.N]
	brand_panel[is.na(npublicholidays), npublicholidays:=0]

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
	
	################
	# Brand equity #
	################
	
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
	fwrite(brand_panel, file = paste0('..\\output\\datasets_', seldat, '.csv'), row.names=F)

	# Load GDP per capita, and put into data sets
	load('..\\temp\\gdppercap.RData')
  gdppercap[, lngdppercap:=log(gdppercap)]
  gdppercap[, dlngdppercap:=lngdppercap-c(NA,lngdppercap[-.N]),by=c('country')]
 
  brand_panel[, year:=year(date)]
	brand_panel = merge(brand_panel, gdppercap, by = c('country', 'year'), all.x=T)
	
	# Save complete data as .RData
	#save(all_data, gdppercap, brand_panel, file =  '..\\output\\datasets.RData')

	# In which categories does the first sales NOT correspond with selected t in category
	tmp=brand_panel[, list(first_sales=min(date[!is.na(usales)]), first_tcat=min(date[selected==T],na.rm=T),
	                       last_tcat=max(date[selected==T],na.rm=T)), by = c('market_id', 'category', 'country')]
	tmp[!first_sales==first_tcat]

	
	#######################
	# CATEGORY-LEVEL DATA #
	#######################
	
	# Prepare flat CSV file with data
	brand_panel_agg=rbindlist(lapply(all_data, function(x) if(!is.null(x)) return(rbindlist(x$data_cleaned_agg))),fill=T)
	setorder(brand_panel_agg, market_id, category,country,brand,date)
	
	brand_panel_agg[which(!is.na(usales) & selected_t_brand==T & selected_brand == T), prelim_selected:=T, by=c('category', 'country', 'brand')]
	brand_panel_agg[is.na(prelim_selected), prelim_selected:=F, by=c('category', 'country', 'brand')]
	
	brand_panel_agg=merge(brand_panel_agg, tmp, by=c('market_id', 'category','country'), all.x=T)
	
	# Add holidays
	
	setkey(brand_panel_agg, country, date)
	setkey(holiday_period, country, month)
	brand_panel_agg[holiday_period, npublicholidays:=i.N]
	brand_panel_agg[is.na(npublicholidays), npublicholidays:=0]
	
	
	# Apply same time selection as for the brand-level data
	
	brand_panel_agg[,selected:=ifelse(prelim_selected==T&date>=first_tcat&date<=last_tcat, T, F)]
	brand_panel_agg[, ':=' (first_sales=NULL, first_tcat=NULL, last_tcat=NULL)]
	
	brand_panel_agg[, year:=year(date)]
	brand_panel_agg = merge(brand_panel_agg, gdppercap, by = c('country', 'year'), all.x=T)
	
	# Prepare CSV file with data
	fwrite(brand_panel_agg, file = paste0('..\\output\\datasets_', seldat, '_agg.csv'), row.names=F)
	
	}

sink('../output/datasets.txt')
cat('done prepping datasets: ', paste0(names(all_datasets), collapse=', '))
sink()
