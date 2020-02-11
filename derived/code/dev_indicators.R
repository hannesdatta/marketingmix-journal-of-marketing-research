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


#######################################
# Time-varying Development indicators #
#######################################

tmp <- data.table(read.xlsx('../../../../Data/dev_indicators/development_data.xlsx',1, startRow=2))

tmp = melt(tmp, id.vars=c('Year', 'Measure'))

setnames(tmp, c('year','measure','country','value'))

# cleaning
tmp[, measure:=tolower(gsub("[^[:alnum:]]", '', measure))]
tmp[, country:=tolower(gsub("[^[:alnum:]]", '', country))]

# recast
dev = dcast(tmp, year+country~measure)

dev[grepl('philippines', country), country:='philippines']


table(dev$country)

dev = dev[!is.na(year)]
dev[, `NA`:=NULL]

fwrite(dev, '../output/dev_indicators.csv')
