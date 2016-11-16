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

### CHINESE ADVERTISING DATA ###
#source(paste0(Sys.getenv('uvt_setup_gfk'), 'setup.R'))

require(reshape2)
require(RODBC)
require(data.table)

# ADVERTISING DATA FOR CHINA
	file_path = '..\\..\\..\\..\\Data\\advertising\\China\\'

	file_list <- list.files(file_path, recursive=T,include.dirs=T,pattern='.xls')
	adv_data <- NULL
		for (i in 1:length(file_list)) {
			print(i)
			xlsfile=paste0(file_path, file_list[i])
			channel <- odbcConnectExcel2007(xlsfile)
			tbls <- sqlTables(channel) 
			adv_data[[i]] = data.table(fileid=file_list[i],sqlFetch(channel, tbls[which(tolower(tbls$TABLE_NAME)=='revised$'),]$TABLE_NAME))
			setnames(adv_data[[i]],tolower(gsub(' |[(]|[)]|[-]','',colnames(adv_data[[i]]))))
			odbcClose(channel)
			}
	adv_data<-rbindlist(adv_data)


	trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
	adv_data[, fileid:=gsub('( - China)','',fileid)]
	adv_data[, fileid:=gsub('( -China)','',fileid)]
	adv_data[, fileid:=gsub('(Top 7)','',fileid)]
	adv_data[, fileid:=gsub('(  )',' ',fileid)]
	adv_data[, fileid:=gsub('(.xls)','',fileid)]
	adv_data[, fileid:=tolower(trim(fileid))]
 

	# Turn into quarterly data
	#Time series
	#require(zoo)
	#year=c(2009,2010,2010,2010,2010,2011,2011)
	#month=c(10,1,4,7,10,1,4)
	#X=1:7
	#tartmonth=7
	#endyear=2012
	#endmonth=12
	if (0){
	make_series <- function(X, year, month) { # Linear interpolation; deprecated
		cat('.')
		# interpolate!!!
		given_ts = base::as.Date(apply(data.frame(year,month),1, function(x) {
						base::as.Date(paste0(x[1],'-',x[2],'-01'))
						}))
		complete_ts = seq(from=base::as.Date(paste0(year[1],'-',month[1],'-01')),
					   to=base::as.Date(paste0(rev(year)[1],'-',as.character(rev(as.numeric(month))[1]+2),'-01')), by=c('1 month'))
		.seq = union(given_ts, complete_ts)
		.seq=.seq[order(.seq)]
		res = zoo(x=X[match(.seq, given_ts)], .seq)
		if (length(X)>1) res <- na.fill(res, "extend") # check whether this is a good one!
		return(list(date=.seq, X=res))
		}
	}
	require(zoo)
	make_series <- function(X, year, month) { # Update: Harald's devide-by-three rule; provided on 17 April 2015 via E-Mail
		cat('.')
		# interpolate!!!
		given_ts = base::as.Date(apply(data.frame(year,month),1, function(x) {
						base::as.Date(paste0(x[1],'-',x[2],'-01'))
						}))
		complete_ts = seq(from=base::as.Date(paste0(year[1],'-',month[1],'-01')),
					   to=base::as.Date(paste0(rev(year)[1],'-',as.character(rev(as.numeric(month))[1]+2),'-01')), by=c('1 month'))
		.seq = union(given_ts, complete_ts)
		.seq=.seq[order(.seq)]
		res = zoo(x=X[match(.seq, given_ts)], .seq)
		res[length(res)]<-rev(X)[1]
		if (length(X)>1) res <- na.approx(res, method="constant") # check whether this is a good one!
		res<-res/3
		return(list(date=.seq, X=res))
		}

	adv_data[, ':=' (year = (substr(quarter,1,4)), month=as.character((as.numeric(substr(quarter,6,6))-1)*3+1))]
	adv_data<-adv_data[!is.na(adspendrmb)&!is.na(country)]
	adv_data[, GRP:=.GRP, by=c('fileid','country','advertisers')]
	adv_china = adv_data[, list(date=make_series(adspendrmb,year,month)$date, adspent=make_series(adspendrmb,year,month)$X) , by=c('fileid','country','advertisers')]
	adv_china[, date:=base::as.Date(date)]
	#detach(package:zoo)

	# -> I have to merge these carefully and rename company names, etc.

	#dir.create(paste0(dirs$svn,'derived\\temp\\'))
	
	save(adv_china, file=paste0('..\\temp\\adv_china.RData'))

	
### DEPRECATED ###

#    __  __                                                                                                                                                     
#   |  \/  |                                                                                                                                                    
#   | \  / |   ___   _ __    __ _    ___                                                                                                                        
#   | |\/| |  / _ \ | '__|  / _` |  / _ \                                                                                                                       
#   | |  | | |  __/ | |    | (_| | |  __/                                                                                                                       
#   |_|  |_|  \___| |_|     \__, |  \___|                                                                                                                       
#     _____   _       _      __/ |                                  _                         _     _         _                         _           _           
#    / ____| | |     (_)    |___/                                  | |                       | |   (_)       (_)                       | |         | |          
#   | |      | |__    _   _ __     ___   ___    ___      __ _    __| | __   __   ___   _ __  | |_   _   ___   _   _ __     __ _      __| |   __ _  | |_    __ _ 
#   | |      | '_ \  | | | '_ \   / _ \ / __|  / _ \    / _` |  / _` | \ \ / /  / _ \ | '__| | __| | | / __| | | | '_ \   / _` |    / _` |  / _` | | __|  / _` |
#   | |____  | | | | | | | | | | |  __/ \__ \ |  __/   | (_| | | (_| |  \ V /  |  __/ | |    | |_  | | \__ \ | | | | | | | (_| |   | (_| | | (_| | | |_  | (_| |
#    \_____| |_| |_| |_| |_| |_|  \___| |___/  \___|    \__,_|  \__,_|   \_/    \___| |_|     \__| |_| |___/ |_| |_| |_|  \__, |    \__,_|  \__,_|  \__|  \__,_|
#                                                                                                                          __/ |                                
#                                                                                                                         |___/                                 

# (currently deprecated)

if (0) {
# Prepare chinese advertising data
	# --> launched via make.bat
	
load('../temp/adv_china.RData')
setnames(adv_china, 'advertisers', 'brand')
setcolorder(adv_china, c('fileid', 'country', 'brand', 'date', 'adspent'))
#adv_china <- data.table(data.frame(adv_china)) # take a clean copy


# per category
	adv_china[,':=' (brand=as.character(brand), country=as.character(country))]
# Refrigerators
	adv_china[brand=='RONSHEN',brand:='RONGSHENG']
	adv_china[brand=='RONGSHEN',brand:='RONGSHENG']
	adv_china[brand=='FRESTEC',brand:='FRESTECH']
	adv_china[fileid=='refrigerators',catname:='refrigerators']
	
	datlist_final$refrigerators<-merge(datlist_final$refrigerators, adv_china[which(catname=='refrigerators'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
	
# DVD
	adv_china[fileid=="dvd players",catname:='dvd']
	datlist_final$dvd<-merge(datlist_final$dvd, adv_china[which(catname=='dvd'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	# AOKA   not matched
# TV
	adv_china[fileid=="television sets",catname:='tv']
	datlist_final$tv<-merge(datlist_final$tv, adv_china[which(catname=='tv'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
# Washing machines
	adv_china[fileid=="dryers and washing machines",catname:='washing']
	datlist_final$washing<-merge(datlist_final$washing, adv_china[which(catname=='washing'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
# Microwave
	adv_china[fileid=="microwave ovens",catname:='microwave']
	datlist_final$microwave<-merge(datlist_final$microwave, adv_china[which(catname=='microwave'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
	#  brands: NATIONAL and BANQIU not matched.
	# BANQIU
# Desktop PC
	adv_china[fileid=="computers",catname:='desktoppc']
	datlist_final$desktoppc<-merge(datlist_final$desktoppc, adv_china[which(catname=='desktoppc'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
	# not matched: E REN E BOOK
# Laptop
	adv_china[fileid=="computers",catname:='laptop']
	datlist_final$laptop<-merge(datlist_final$laptop, adv_china[which(catname=='laptop'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
# Tablets
	adv_china[fileid=="computers",catname:='tablets']
	datlist_final$tablets<-merge(datlist_final$tablets, adv_china[which(catname=='tablets'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
	# not matched: E REN E BOOK
# Adv. cameras
	adv_china[fileid=="digital-cameras",catname:='camera_adv']
	datlist_final$camera_adv<-merge(datlist_final$camera_adv, adv_china[which(catname=='camera_adv'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
# Std. cameras
	adv_china[fileid=="digital-cameras",catname:='camera_std']
	datlist_final$camera_std<-merge(datlist_final$camera_std, adv_china[which(catname=='camera_std'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
# Smart phones
	adv_china[fileid=="phones (mobile phones and smartphones)",catname:='phones_smart']
	datlist_final$phones_smart<-merge(datlist_final$phones_smart, adv_china[which(catname=='phones_smart'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
	 #NIECHE not matched
# Regular phones
	adv_china[fileid=="phones (mobile phones and smartphones)",catname:='phones_mobile']
	datlist_final$phones_mobile<-merge(datlist_final$phones_mobile, adv_china[which(catname=='phones_mobile'),c('country','brand','date','adspent'),with=F],by=c('country','brand','date'),all.x=T)
	
	 #NIECHE not matched
	 

}

