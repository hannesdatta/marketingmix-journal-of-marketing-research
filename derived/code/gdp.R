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

require(data.table)
require(RODBC)

##########################
# GDP per capita 		 #
# provided by World Bank #
##########################


# Get content of all zip files
	file_path = '..\\..\\..\\Data\\gdp\\'

	dat <- fread(paste0(file_path, 'Popular indicators_Data.csv'))
	setnames(dat, "﻿Series Name", 'seriesname')
	tmp <- melt(dat, id.vars = c('seriesname', 'Series Code', 'Country Name', 'Country Code'))
	tmp[, year := as.numeric(substr(variable, 1,4))]
	setnames(tmp, 'Series Code', 'seriescode')
	setnames(tmp, 'Country Name', 'country')
	setnames(tmp, 'Country Code', 'countrycode')
	tmp[, country := tolower(country)]

	countries <- c('australia', 'singapore', 'hong kong sar, china', 'taiwan', 'korea, rep.', 'japan', 'india','china', 'indonesia', 'malaysia', 'vietnam', 'philippines', 'thailand', 'new zealand')

	tmp = tmp[country %in% countries & seriescode == 'NY.GDP.PCAP.CD']

	worldbank=tmp[, c('country','year', 'value'),with=F]
	setnames(worldbank, 'value', 'gdppercap')
	setorder(worldbank, country, year)
	
	
# Get Taiwan data
	taiwan <- fread(paste0(file_path,'TWGDPPEAA - Time Series Data.csv'), skip=6)[,1:2,with=F]
	setnames(taiwan, c('date_untransformed', 'gdppercap'))
	# build yearly average
	
	taiwan[, date_tmp := unlist(lapply(strsplit(as.character(date_untransformed), "/"),function(x) paste0(x[1],'/01/',x[3])))]
	# convert to date column
	taiwan[, date := as.Date(date_tmp, format = '%m/%d/%Y')]
	taiwan[, ':=' (date_tmp=NULL,date_untransformed=NULL)]
	taiwan[, year:=year(date)]
	
	# make yearly average
	taiwan = taiwan[, list(gdppercap=mean(gdppercap)),by=c('year')]
	
	taiwan = taiwan[, list(country='taiwan', year=year, gdppercap=gdppercap)]
	
# Merge
	gdppercap <- rbind(worldbank, taiwan)
	gdppercap[, gdppercap := as.numeric(gdppercap)]
# Retain only values from observation period (2004-2014)

	gdppercap<- gdppercap[year%in%2004:2014]

# Rename countries
	gdppercap[country=='hong kong sar, china', country:='hong kong']
	gdppercap[country=='korea, rep.', country:='south korea']

# Save
	save(gdppercap, file='..\\temp\\gdppercap.RData')

	