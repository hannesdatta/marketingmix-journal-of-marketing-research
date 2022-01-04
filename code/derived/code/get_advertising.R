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
library(reshape2)
library(xlsx)
library(data.table)
library(zoo)

################################
# ADVERTISING DATA FOR CHINA   #
################################

	file_path = '../../../Data/advertising/China/'

	file_list <- list.files(file_path, recursive=T,include.dirs=T,pattern='.xls')
	
	adv_data <- NULL
		for (i in 1:length(file_list)) {
			print(i)
			xlsfile=paste0(file_path, file_list[i])
			tmp <- read.xlsx(xlsfile, sheetName='Revised')
			adv_data[[i]] = cbind(fileid=file_list[i],data.table(tmp))
			setnames(adv_data[[i]],tolower(gsub(' |[(]|[)]|[-]|[.]','',colnames(adv_data[[i]]))))
			}
	adv_data<-rbindlist(adv_data)

  # Data cleaning
	
	trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
	}
	
	adv_data[, fileid:=gsub('( - China)','',fileid, ignore.case=T)]
	adv_data[, fileid:=gsub('( -China)','',fileid, ignore.case=T)]
	adv_data[, fileid:=gsub('(Top 7)','',fileid, ignore.case=T)]
	adv_data[, fileid:=gsub('(  )',' ',fileid, ignore.case=T)]
	adv_data[, fileid:=gsub('(.xls)','',fileid, ignore.case=T)]
	adv_data[, fileid:=tolower(trim(fileid))]
	
	make_series <- function(X, year, month) { # Update: Harald's devide-by-three rule; provided on 17 April 2015 via E-Mail
	  cat('.')
	  # interpolate!!!
	  given_ts = base::as.Date(apply(data.frame(year,month),1, function(x) {
	    base::as.Date(paste0(x[1],'-',x[2],'-01'), origin = '1970-01-01')
	  }), origin = '1970-01-01')
	  complete_ts = seq(from=base::as.Date(paste0(year[1],'-',month[1],'-01'), origin = '1970-01-01'),
	                    to=base::as.Date(paste0(rev(year)[1],'-',as.character(rev(as.numeric(month))[1]+2),'-01'), origin = '1970-01-01'), by=c('1 month'))
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
	adv_china[, date:=base::as.Date(date, origin = '1970-01-01')]
	
	setnames(adv_china, 'fileid', 'category')
	setnames(adv_china, 'advertisers', 'brand')
	
	adv_china[brand=='FRESTEC',brand:='FRESTECH']
	
	
####################################
# ADVERTISING DATA FOR HONG KONG   #
####################################
	
adv_hk <- data.table(read.xlsx('..\\..\\..\\Data\\advertising\\HongKong\\Massey1087-040101-141231.xls', sheetName='Details', startRow=2, stringsAsFactors=F))
setnames(adv_hk, tolower(gsub(' |[(]|[)]|[-]|[.]','',colnames(adv_hk))))

adv_hk[, country:=toupper(country)]

setnames(adv_hk, 'cost','adspent')

adv_hk[, date:=as.Date(paste0(year,'-', month,'-01'))]
adv_hk[, ':=' (year=NULL, month=NULL)]

adv_hk[brand=='AK', brand:='AK MOBILE']
adv_hk[brand=='CHINA MOBILE TELECOM', brand:='CHINA MOBILE']
adv_hk[brand=='DBTEL INTERNATIONAL', brand:='DBTEL']
adv_hk[brand=='FRESTEC', brand:='FRESTECH']
adv_hk[brand=='HP COMPAQ', brand:='HP']
adv_hk[brand=='KONICA MINOLTA', brand:='KONICA-MINOLTA']
adv_hk[brand=='POLVVISION', brand:='POLYVISION']
adv_hk[brand=='TSINGHUA TONGFANG', brand:='TSINGHUA TONGFA']
adv_hk[brand=='VERTU ASCENT', brand:='VERTU']

adv_hk[,category_combined := paste0(category,'-',subcategory,'-',details)]



#############################
###### DATA CLEANING  #######
#############################

# Restructure data

adv_china[, adspent:=as.numeric(adspent)]

cnames=c('country','brand','date','adspent')


adv_merged = NULL

adv_merged$cooling = rbind(adv_china[category=='refrigerators', cnames,with=F],
                           adv_hk[category_combined=='HOME ELECTRICAL APPLIANCES-Refrigerator-Un-Defined', cnames,with=F])

adv_merged$dvd = rbind(adv_china[category=='dvd players', cnames,with=F],
                       adv_hk[category_combined=='HOME ELECTRICAL APPLIANCES-Video Player-Un-Defined', cnames,with=F])

tvs = rbind(adv_china[category=='television sets', cnames,with=F],
            adv_hk[category_combined=='HOME ELECTRICAL APPLIANCES-TV & Accessories-Un-Defined', cnames,with=F])

adv_merged$tv_gen1_crtv <- copy(tvs)
adv_merged$tv_gen2_ptv <- copy(tvs)
adv_merged$tv_gen3_lcd_only <- copy(tvs)

washdry = rbind(adv_china[category=='dryers and washing machines', cnames,with=F],
                adv_hk[category_combined=='HOME ELECTRICAL APPLIANCES-Washing Machine/Dryer-Un-Defined', cnames,with=F])

adv_merged$washing = copy(washdry)

adv_merged$tumbledryers = copy(washdry)


adv_merged$microwave = rbind(adv_china[category=='microwave ovens', cnames,with=F],
                             adv_hk[category_combined=='HOME ELECTRICAL APPLIANCES-Electrical Kitchen Appliances-Microwave Oven', cnames,with=F])

adv_merged$desktoppc = rbind(adv_china[category=='computers', cnames,with=F],
                             adv_hk[category_combined=='IT Product & Service-Computer-Desktop Computer', cnames,with=F])
adv_merged$laptop = rbind(adv_china[category=='computers', cnames,with=F],
                          adv_hk[category_combined=='IT Product & Service-Computer-Notebook Computer', cnames,with=F])

adv_merged$tablets = rbind(adv_china[category=='computers', cnames,with=F],
                           adv_hk[category_combined=='IT Product & Service-Computer-Tablets', cnames,with=F])

cams = rbind(adv_china[category=='digital-cameras', cnames,with=F],
             adv_hk[category_combined=='IT Product & Service-Digital Product-Digital Camera'|category_combined=='IT Product & Service-Digital Product-Digital Single Lens Reflective Camera', cnames,with=F])

adv_merged$camera_adv = copy(cams)
adv_merged$camera_std = copy(cams)

phones =  rbind(adv_china[category=='phones (mobile phones and smartphones)', cnames,with=F],
                adv_hk[category_combined=='POST & COMMUNICATION-Communication Product-Mobile Phone', cnames,with=F])

adv_merged$phones_smart = copy(phones)
adv_merged$phones_mobile = copy(phones)

for (i in seq(along=adv_merged)) {
  adv_merged[[i]][,category:=names(adv_merged)[i]]
}

tmp = rbindlist(adv_merged)

adv_merged = tmp[, list(adspent=sum(adspent,na.rm=T)),by=c('category','country','brand','date')]
# sum up adv spending

# fill in zero ad spending
tmp=dcast.data.table(adv_merged,category+country+brand~date, value.var='adspent')
tmp2=melt(tmp, id.vars=c('category','country','brand'))
setnames(tmp2, 'variable','date')
setnames(tmp2, 'value','adspent')

setkey(tmp2, category,country,brand,date)

tmp2[, date:=as.Date(date)]

tmp2[, first_date:=min(date[adspent>0],na.rm=T),by=c('category','country')]
tmp2[, last_date:=max(date[adspent>0],na.rm=T),by=c('category','country')]

tmp2[is.na(adspent)&date>=first_date&date<=last_date, adspent:=0]

setcolorder(tmp2, c('category','country','brand','date','adspent'))
tmp2[, first_date:=NULL]
tmp2[, last_date:=NULL]

fwrite(tmp2, file=paste0('..\\temp\\advertising.csv'))

