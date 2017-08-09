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
require(data.table)

### Stack data in data.table
	brand_panel=fread('../../derived/output/datasets.csv')
	brand_panel[, ':=' (date = as.Date(date))]
	brand_panel <- brand_panel[selected==T]
	
	brand_panel[, nov3sh := (nov3/llen)*100]
	brand_panel[, nov1sh := (nov1/llen)*100]
	brand_panel[, quarter := quarter(date)]

	brand_panel[, usalessh := usales/sum(usales,na.rm=T), by=c('category', 'country', 'date')]
	brand_panel[, vsalessh := vsales/sum(vsales,na.rm=T), by=c('category', 'country', 'date')]
	brand_panel[, month_no := as.numeric(as.factor(date))]

	brand_panel[brand=='allothers', brand := paste0('allothers', .GRP), by = c('category', 'country')]
	
	# Define broad country/categories classifications
	
	brand_panel[grepl('camera', category), cat_class := 'cam']
	brand_panel[grepl('phones|tablets', category), cat_class := 'ph']
	brand_panel[grepl('desktoppc|laptop', category), cat_class := 'cp']
	brand_panel[grepl('tv[_]|dvd', category), cat_class := 'tvdvd']
	brand_panel[grepl('washing|cooling|microwave', category), cat_class := 'wte']
	
	brand_panel[country %in% c('australia', 'hong kong', 'japan', 'new zealand', 'singapore', 'south korea', 'taiwan'), country_class := 'hinc']
	brand_panel[is.na(country_class), country_class := 'linc']
	
	# add %-indicator to separate data in moving windows
	brand_panel[, N:=.GRP, by = c('market_id', 'date')]
	brand_panel[, share_obs:=(N-min(N)+1) / (max(N)-min(N)+1), by = c('market_id')]
	brand_panel[, N:=NULL]
	
	# Save file
	fwrite(brand_panel, '../temp/preclean.csv')
	