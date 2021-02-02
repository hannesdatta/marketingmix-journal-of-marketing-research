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
	setorder(brand_panel, market_id, brand, date)
	
	# lag variables
	for (.var in c('rwpspr', 'wpswdst','llen', 'usales')) {
	  brand_panel[, paste0('lag', .var) := c(NA, get(.var)[-.N]), by = c('market_id', 'brand')]
	}
	
	
	# keep selected brands and ALLOTHER brands
  brand_panel <- brand_panel[selected==T]
  
  brand_panel[selected_brand==T, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  
  
  brand_panel[, usales_incr:=ifelse(usales==0, usales+.01, usales)]
  
	brand_panel[, usalessh := usales_incr/sum(usales_incr,na.rm=T), by=c('category', 'country', 'date')]
	brand_panel[, vsalessh := vsales/sum(vsales,na.rm=T), by=c('category', 'country', 'date')]
	brand_panel[, month_no := as.numeric(as.factor(date))]
  	
	brand_panel[brand=='allothers', brand := paste0('allothers', .GRP), by = c('category', 'country')]

	# add %-indicator to separate data in moving windows
	brand_panel[, N:=.GRP, by = c('market_id', 'date')]
	brand_panel[, share_obs:=(N-min(N)+1) / (max(N)-min(N)+1), by = c('market_id')]
	brand_panel[, N:=NULL]
	
	tmp=brand_panel[selected_t_cat==T&selected_brand==T, list(usales=sum(usales,na.rm=T)),by=c('market_id','date')]
	tmp[, percentile_obs:=(1:.N)/.N, by = c('market_id')]
	
	setkey(tmp, market_id, date)
	setkey(brand_panel, market_id, date)
	brand_panel[tmp, percentile_obs:=i.percentile_obs]
	
	
	# Beginning and end
	
	# per brand, aggregate to quartlery level
	
	thres=.05 # threshold of max sales
	tmp = brand_panel[, list(usales=sum(usales,na.rm=T)),by=c('brand_id','date')]
	tmp[, max_sales:=max(usales, na.rm=T),by=c('brand_id')]
	tmp = tmp[, list(first_crossing=date[which(usales>=thres*max_sales)[1]],
	                 last_crossing=date[rev(which(usales>thres*max_sales))[1]]),by=c('brand_id')]
	
	setkey(tmp, 'brand_id')
	setkey(brand_panel, 'brand_id')
	brand_panel[tmp, timewindow:=date>=i.first_crossing & date<=i.last_crossing]
  brand_panel[, obs48 := sum(timewindow)>=48, by = c('brand_id')]
  
	setorder(brand_panel, market_id, brand, date)

	# Save file
	dir.create('../temp')
	
	fwrite(brand_panel, paste0('../temp/', gsub('datasets', 'preclean', fn)))
}

sink('../temp/preclean.txt')
cat('done\n')
sink()