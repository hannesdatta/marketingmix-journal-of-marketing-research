load('../temp/brand_metrics.RData')
library(data.table)

for (seldat in names(all_datasets)) {

	all_data <- all_datasets[[seldat]]

	####################
	# BRAND-LEVEL DATA #
	####################
	
	# Prepare flat CSV file with data
	brand_panel=rbindlist(lapply(all_data, function(x) if(!is.null(x)) return(rbindlist(x$data_cleaned))),fill=T)
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
