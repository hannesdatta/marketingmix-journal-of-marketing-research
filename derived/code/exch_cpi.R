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

############################################
# CPI 							   	 	   #
# provided by Thomson Reuters (Datastream) #
############################################


# Get content of all zip files
file_path = '..\\..\\..\\..\\Data\\datastream\\'
file_list <- list.files(file_path, full.names=T,pattern='xlsx')

indicators <- NULL
		for (i in 1:length(file_list)) {
			print(i)
			xlsfile=paste0(file_list[i])
			out <- read.xlsx(xlsfile,1)
				
			if (grepl('CONPRCF|CPBANGF', out[4,2])) {
				# reference price
				# for India, CPI for Industrial workers in Bangalore is taken, because series for urban CPI was only available starting in January 2011.

				out$type = 'cpi'
				}
			if (grepl('[A-Z][A-Z]USDSP', out[4,2])) {
				# exchange rate price
				out$type = 'exchange'
				}			
			colnames(out)[1:2] <- c('date_untransformed', 'value')
			out$countrycode = substr(out[4,2],1,2)
			out$value = as.numeric(levels(out$value)[out$value])
			out$date_untransformed = as.numeric(levels(out$date_untransformed)[out$date_untransformed])
			
			out <- out[6:nrow(out),]
			indicators[[i]]<-out
			
			}
			
raw_indicators<-rbindlist(indicators)


# Interpolation to monthly data
	# set all dates to the beginning of the month
	raw_indicators[, date_tmp := as.Date(date_untransformed, origin="1899/12/30")]
	raw_indicators[, date_tmp := date_tmp-mday(date_tmp)+1]
	# convert to date column
	raw_indicators[, date := as.Date(date_tmp, format = '%m/%d/%Y')]
	raw_indicators[, ':=' (date_tmp=NULL,date_untransformed=NULL)]
	setkey(raw_indicators, date, type, countrycode)
	
	source('proc_functions.R')
	
	dates = data.table(data.frame(date=seq(from=min(raw_indicators$date),to=max(raw_indicators$date),by='1 month')),key='date')
	
	# make empty data set
	tmp = split(raw_indicators, paste(raw_indicators$type, raw_indicators$countrycode,sep='_'))
	indicators=rbindlist(lapply(tmp, function(x) {
		setkey(x, date)
		ret=x[dates]
		ret[, ':=' (type = unique(type[!is.na(type)]), countrycode= unique(countrycode[!is.na(countrycode)]))]
		return(ret)
		}))
	
	indicators=indicators[order(type,countrycode,date)]
	
	# interpolate
	indicators[, interp_value := nafill(value, maxgap=4),by=c('type','countrycode')]
	
	indicators[is.na(value) & !is.na(interp_value), interpolated:=T]
	indicators[!is.na(value), interpolated:=F]
	indicators[is.na(value) & is.na(interp_value), interpolated:=NA]
	
	indicators[, value:=NULL]
	setnames(indicators, 'interp_value', 'value')
	

	indicators[countrycode=='AU', country:='AUSTRALIA']
	indicators[countrycode=='CH', country:='CHINA']
	indicators[countrycode=='HK', country:='HONG KONG']
	indicators[countrycode=='ID', country:='INDONESIA']
	indicators[countrycode=='IN', country:='INDIA']
	indicators[countrycode=='JP', country:='JAPAN']
	indicators[countrycode=='KO', country:='SOUTH KOREA']
	indicators[countrycode=='MY', country:='MALAYSIA']
	indicators[countrycode=='NZ', country:='NEW ZEALAND']
	indicators[countrycode=='PH', country:='PHILIPPINES']
	indicators[countrycode=='SP', country:='SINGAPORE']
	indicators[countrycode=='TH', country:='THAILAND']
	indicators[countrycode=='TW', country:='TAIWAN']
	indicators[countrycode=='VI', country:='VIETNAM']

	indicators[, countrycode:=NULL]

	# normalize CPIs (1 = value on 2004-01-01)
	indicators[type=='cpi', value := value/value[date=='2004-01-01'], by = c('country')]
		
	library(lattice)
	dir.create('../audit/')
	png('../audit/cpi.png', res=150, units='in', height=8, width=12)
	print(xyplot(value~date|country, data=data.table(indicators)[type=='cpi']))
	dev.off()
	
# save
	dir.create('../temp/')
	save(indicators, file='..\\temp\\exch_cpi.RData')
