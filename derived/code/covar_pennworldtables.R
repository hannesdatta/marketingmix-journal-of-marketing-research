#    _____            _                                                                   _     _                 
#   |  __ \          | |                                                                 | |   (_)                
#   | |  | |   __ _  | |_    __ _     _ __    _ __    ___   _ __     ___   _ __    __ _  | |_   _    ___    _ __  
#   | |  | |  / _` | | __|  / _` |   | '_ \  | '__|  / _ \ | '_ \   / _ \ | '__|  / _` | | __| | |  / _ \  | '_ \ 
#   | |__| | | (_| | | |_  | (_| |   | |_) | | |    |  __/ | |_) | |  __/ | |    | (_| | | |_  | | | (_) | | | | |
#   |_____/   \__,_|  \__|  \__,_|   | .__/  |_|     \___| | .__/   \___| |_|     \__,_|  \__| |_|  \___/  |_| |_|
#                                    | |                   | |                                                    
#                                    |_|                   |_|                                                    
#
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

library(data.table)

library(xlsx)


###################################################
# Time-varying Penn State World Tables indicators #
###################################################

tmp <- fread('../../../../Data/penn_worldtables/pwt90.csv')

# keep countries
tmp <- tmp[grepl('australia|china$|hong|india|indonesia|malaysia|taiwan|thailand|philippines|zealand|japan|korea|singa|viet', country, ignore.case=T)]

#length(table(tmp$country))

tmp[, country_recode := tolower(country)]
table(tmp$country_recode)
tmp[grepl('hong', country, ignore.case=T), country_recode:='hong kong']
tmp[grepl('korea',country, ignore.case=T), country_recode:='south korea']
tmp[grepl('viet',country, ignore.case=T), country_recode:='vietnam']

table(tmp$country_recode)
tmp[, i_outlier:=NULL]

# select variables
vars <- c('rgdpe','rgdpo','pop','emp','avh', 'hc')

tmp <- tmp[, c('country_recode','year',vars),with=F]

tmp <- tmp[year%in%2003:2014]

# growth rates
for (.v in grep('^rgdp', colnames(tmp), value=T)) tmp[, paste0('growth', .v):=100*(get(.v)-c(NA,get(.v)[-.N]))/c(NA,get(.v)[-.N]), by = c('country_recode')]

# per capita
for (.v in grep('^rgdp', colnames(tmp), value=T)) tmp[, paste0('percapita', .v):=get(.v)/pop]

tmp <- tmp[year>=2004]

setnames(tmp, 'country_recode', 'country')
fwrite(tmp, '../output/penn_indicators.csv')
