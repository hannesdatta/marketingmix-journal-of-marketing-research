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

### LOAD DATA SETS
library(data.table)

## Load panel data
	brand_panel=fread('../temp/preclean.csv')
	brand_panel[, ':=' (date = as.Date(date))]

## 
  brand_panel[, ':=' (first_date=min(date[!is.na(usales)]), last_date=max(date[!is.na(usales)])),by=c('category','country')]
  
  tmp=	brand_panel[, list(marketshare = unique(overall_ms), first_date_cat = unique(first_date), last_date_cat=unique(last_date), first_date_brand=min(date[!is.na(usales)]), last_date_brand=max(date[!is.na(usales)])),by=c('category','country','brand')]
  
  setorderv(tmp, c('country', 'category', 'marketshare'), order=-1L)
  
  tmp[, topX:=1:.N, by = c('country','category')]
	
  setcolorder(tmp, c('country','category','topX', 'brand', 'marketshare','first_date_cat','last_date_cat', 'first_date_brand', 'last_date_brand'))
  fwrite(tmp, '../temp/brands_for_advertising.csv',row.names=F) 
  

## Obs per market
	tmp=brand_panel[, list(obs=length(unique(date[!is.na(nov6)]))),by=c('market_id','category','country')]
	setorder(tmp, obs)

	
# export for hongkong
	fwrite(tmp[country=='hong kong'], file='../temp/brands_for_adv_hongkong.csv')
	
