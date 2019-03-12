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
library(stringr)

dir.create('../temp')

########################################
# Human development index              #
# provided by UN Development Programme #
# http://hdr.undp.org/en/data          #
########################################

# Get content of all zip files
	file_path = '..\\..\\..\\..\\Data\\hdi\\'

	dat <- fread(paste0(file_path, 'Human Development Index (HDI).csv'),skip=1)
  setnames(dat, tolower(colnames(dat)))
  
	# retain HDI in 2010
  dat <- melt(dat[, c('country','2010'),with=F], id.var=c('country'), value.var=c('2010'))
  
	dat[, country:=tolower(country)]
	setnames(dat, 'value','hdi2010')
  dat[, variable:=NULL]
  dat[, hdi2010:=as.numeric(hdi2010)]
  dat[, country:=str_trim(country)]
  
  dat[country=='hong kong, china (sar)', country:='hong kong']
  dat[country=='viet nam', country:='vietnam']
  dat[country=='korea (republic of)', country:='south korea']
  
  # add Taiwan (2007 data: https://en.wikipedia.org/w/index.php?title=List_of_countries_by_Human_Development_Index_(2009)&oldid=817625828)
  dat<-rbindlist(list(dat, data.frame('taiwan', 0.943))) # not reliable data probably
  
  dat[country%in%c('australia', 'japan', 'hong kong', 'new zealand', 'thailand', 'indonesia','vietnam', 'taiwan','singapore', 'south korea', 'china', 'india', 'philippines', 'malaysia')]
  
  fwrite(dat, '../temp/hdi.csv')

	