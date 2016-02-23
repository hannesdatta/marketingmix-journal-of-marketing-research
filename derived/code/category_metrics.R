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
# - Create category-level data set

require(data.table)


# -> skus_by_date_list: sales data on a SKU-level by date
	load('..//temp//brand_metrics.RData') 

for (i in 1:length(all_data)) {

	all_data[[i]]$data_category <- NULL
	
	for (j in 1:length(all_data[[i]]$data_cleaned)) {
	
		dt <- all_data[[i]]$data_cleaned[[j]]
	
		##########################################
		## CREATE LAG SALES MEASURE FOR WEIGHING #
		##########################################
		
			# Create a brand-level measure of past sales in periods -2,-1,0 for re-weighing of the 'unweighted' metrics
			
			t_lags = c(-2,-1, 0)
			t_lag_names = paste0('lag_date', gsub('[-]', 'min', as.character(t_lags)))
		
			tmpdates <- dt[, list(N=.N),by=c('date')][,N:=NULL]
		
			lags=sapply(t_lags, function(t_lag, x) {
				m.date=as.POSIXlt(x)
				m.date$mon=m.date$mon+t_lag
				return(as.character(m.date))
				}, x=tmpdates$date)
		
			for (t_lag in seq(along=t_lags)) {
				tmpdates[, t_lag_names[t_lag] := c(as.Date(lags[,t_lag])),with=F]
				}
		
			# Merge this to skus_by_date
			setkey(tmpdates, date)
			setkey(dt, date)
			suppressWarnings(dt[, t_lag_names := NULL, with=F])
			dt <- tmpdates[dt]
		
		setkey(dt, 'category', 'country','brand', 'date')
		
		sales_lag_names = paste0('lag_sales', gsub('[-]', 'min', as.character(t_lags)))
		for (t_lag in seq(along=t_lags)) {
			eval(parse(text=paste0('dt[,  sales_lag_names[t_lag] := unitsales[match(', t_lag_names[t_lag],', date)],by=c(\'category\', \'country\',\'brand\'),with=F]')))
			}
			
		eval(parse(text=paste0('dt[, t_wsales_units := rowSums(data.table(',paste(sales_lag_names,collapse=','),'), na.rm=T)]')))
		dt[, t_wsales_units:=t_wsales_units/length(t_lags)]
		
		# remove unnecessary columns
		dt[, c(t_lag_names, sales_lag_names) := NULL, with=F]
		rm(tmpdates)
	
		# assert that there are only 1 sku per unique key
		if(all(dt[, list(.N),by=c('country','category', 'date', 'brand')]$N==1)==F) stop('problem with unique key/SKUs')
		
		
		###################################
		# Aggregate data to category-level #
		####################################

			weigh_by_w <- function(x, w, ...) {
				if (sum(w, ...)==0) w = rep(1, length(x))
				sum(x*w, ...)/sum(w, ...)
				}
			setorder(dt, category, country, brand, date)
			
			makelag = function(x, lags = 1) {
				c(rep(NA,lags),x)[1:length(x)]
				}

			dt[, ':=' (lunitsales = makelag(unitsales),
					   lvaluesales = makelag(valuesales),
					   lvaluesales_usd = makelag(valuesales_usd)
						), by = c('brand')]
			
			
			
			dt_aggr = dt[which(selected==T), list(
								
								unitsales=sum(unitsales, na.rm=T),
								valuesales = sum(valuesales, na.rm=T), 
								valuesales_usd = sum(valuesales_usd, na.rm=T),
								
								lunitsales=sum(lunitsales, na.rm=T),
								lvaluesales = sum(lvaluesales, na.rm=T), 
								lvaluesales_usd = sum(lvaluesales_usd, na.rm=T),
																
								llength = sum(llength, na.rm=T),
								
								rwsprice=weigh_by_w(rvaluesales/unitsales, unitsales, na.rm=T),
								rwpsprice=weigh_by_w(rvaluesales/unitsales, t_wsales_units, na.rm=T),
								 
								wsunique = weigh_by_w(wsunique,unitsales, na.rm=T),
								wpsunique = weigh_by_w(wsunique,t_wsales_units, na.rm=T),
								 
								wsuniquenoweight = weigh_by_w(wsuniquenoweight,unitsales, na.rm=T),
								wpsuniquenoweight = weigh_by_w(wsuniquenoweight,t_wsales_units, na.rm=T),
								 
								wsnumdist = weigh_by_w(wsnumdist,unitsales, na.rm=T),
								wpsnumdist = weigh_by_w(wsnumdist,t_wsales_units, na.rm=T),
								 
								wswdist = weigh_by_w(wswdist,unitsales, na.rm=T),
								wpswdist = weigh_by_w(wswdist,t_wsales_units, na.rm=T),
								
								novel= sum(novel, na.rm=T),
								
								n_brands = length(which(!is.na(unitsales)))
								 
								 ), by=c('category', 'country', 'market_id', 'date')][order(category, country,date)]

		all_data[[i]]$data_category[[j]] <- dt_aggr
	
		}
	}
	
	
# Load GDP per capita, and put into data sets
	load('..\\temp\\gdppercap.RData')

save(all_data, gdppercap, file =  '..\\output\\datasets.RData')
