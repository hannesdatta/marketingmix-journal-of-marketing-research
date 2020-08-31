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


#if (1==1) quit()

### LOAD DATA SETS
library(data.table)
library(bit64)
library(parallel)
library(marketingtools)
library(car)
library(devtools)

#install_github('https://github.com/hannesdatta/dynamac', ref = 'firstdiff_nolags')

# try out new dynamac distribution
#devtools::install_github("andyphilips/dynamac")
library(dynamac)


dir.create('../output')

## Load panel data
	brand_panel=fread('../temp/preclean_main.csv')
	brand_panel[, ':=' (date = as.Date(date))]

# define markets to run analysis on 
	markets <- brand_panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
  length(analysis_markets)
 
# Define additional variables
  brand_panel[selected==T, trend:=.GRP,by=c('market_id', 'date')]
  brand_panel[selected==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id')]
 

  for (q in 1:3) {  
    brand_panel[, paste0('quarter', q):=0]
    brand_panel[quarter==q, paste0('quarter', q):=1]
  }  
    
  vars=c('rwpspr', 'llen', 'wpswdst', 'usales', 'lagusales')
  for (var in vars) {
    brand_panel[, anyzero:=as.numeric(any(get(var)==0),na.rm=T),by=c('market_id', 'brand')]
    brand_panel[is.na(anyzero), anyzero:=0]
    brand_panel[, paste0('ln', var):=log(get(var)+anyzero), by = c('market_id', 'brand')]
    brand_panel[, paste0('dln', var):= get(paste0('ln', var))-c(NA, get(paste0('ln', var))[-.N]), by = c('market_id', 'brand')]
  }
  
  # competitive mmix
  for (v in c('lnrwpspr', 'lnllen', 'lnwpswdst')) {
    brand_panel[, paste0('sum_', v):=sum(get(v),na.rm=T), by = c('market_id', 'date')]
    brand_panel[, paste0('N_', v):=length(which(!is.na(get(v)))), by = c('market_id', 'date')]
    brand_panel[, paste0('comp_', v):=(get(paste0('sum_', v))-get(v))/(get(paste0('N_', v))-1)]
    brand_panel[, paste0('sum_', v):=NULL]
    brand_panel[, paste0('N_', v):=NULL]
    
    }
  
  # Define copula terms
  for (var in c('rwpspr', 'llen', 'wpswdst')) {
    brand_panel[, paste0('cop_', var):=make_copula(get(paste0('ln', var))), by = c('market_id','brand')]
    brand_panel[, paste0('dcop_', var):=get(paste0('cop_', var))-c(NA,get(paste0('cop_', var))[-.N]), by = c('market_id', 'brand')]
  }
  
  # run shapiro-wilk tests to assess non-normality of untransformed inputs to the copula function
  if(0){
  vars=c('lnrwpspr', 'lnwpsllen', 'lnwpswdst')
  cop=lapply(vars, function(var) {
    out=panel[, list(shap_pval = shapiro.test(get(var))$p), by = c('category', 'country', 'market_id')]
    out[, variable:=var]
    #
    #cop_terms[, value := make_copula(value), by = c('country','category','brand','variable')]
    return(out)
  })
    
  cop=rbindlist(cop)
  
  cop[, list(nonnormal_share=length(which(shap_pval<.1))/.N), by = c('variable')]
  }
  
  brand_panel <- brand_panel[!grepl('allothers|unbranded',brand,ignore.case=T)]
  
#################################
# PRELIMINARY UNIT ROOT TESTING #
#################################

source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R')
if(0){
# checking order of UR for brand sales
vars = c('lnusales','lnrwpspr','lnllen','lnwpswdst')
tmp2=brand_panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlags=6, pval=.1)$order),by=c('market_id', 'brand'), .SDcols=vars]
tmp2[lnusales>1] 
any(tmp2[,-1,with=F]>1)
# looks good, too!
}
##########################
# DYNAMAC ARDL PROCEDURE #
##########################

source('proc_analysis_agg.R')
source('proc_analysis.R')
source('proc_analysis_brand.R')
source('proc_ardl.R')
source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R')
  
###########################
# CLUSTER ESTIMATION      #
###########################

if(0) {
require(parallel)
cl<-makePSOCKcluster(7)
clusterExport(cl, c('panel', 'brand_panel'))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(timeSeries))
void<-clusterEvalQ(cl, source('proc_analysis_agg.R'))
void<-clusterEvalQ(cl, source('proc_analysis_brand.R'))
void<-clusterEvalQ(cl, source('proc_analysis.R'))
void<-clusterEvalQ(cl, source('proc_ardl.R'))
void<-clusterEvalQ(cl, source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R'))
rm(void)

bids <- unique(brand_panel$brand_id)[1:150]

#res=clusterApply(cl, unique(panel$market_id), function(mid) try(analyze_market(mid, quarters=T), silent=T))
res=clusterApply(cl, bids, function(bid) {
  out=try(analyze_brand(bid, quarters=T), silent=T)
  if (class(out)=='try-error') return('error') else return(out)
})
my_results=unlist(res)


cases = data.table(brand_id = bids,
                   boundstest = gsub(' [(].*', '', my_results))
cases[, case:=as.character(NA)]
cases[is.na(case)&grepl('stationarity', boundstest), case:='stationarity']
cases[is.na(case)&grepl('no cointegration$', boundstest), case:='nocointegration']
cases[is.na(case)&grepl('cointegration$', boundstest), case:='cointegration']

cases[, list(.N),by=c('case')]

}

  
  
##################################
# COMPLETE MODEL ESTIMATION      #
##################################
source('proc_salesresponse.R')

res=sapply(unique(brand_panel$brand_id)[1:20], function(i) {
  cat('\n\n NEW MODEL \n\n')
  print(i)
  cat('\n\n======================= \n\n')
  return(try(newfkt(i), silent=T))
  })

##########################
### CLUSTER ESTIMATION ###
##########################

  require(parallel)
  cl<-makePSOCKcluster(7)
  clusterExport(cl, c('brand_panel'))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(timeSeries))
  void<-clusterEvalQ(cl, source('proc_analysis_agg.R'))
  void<-clusterEvalQ(cl, source('proc_analysis_brand.R'))
  void<-clusterEvalQ(cl, source('proc_analysis.R'))
  void<-clusterEvalQ(cl, source('proc_ardl.R'))
  void<-clusterEvalQ(cl, source('proc_salesresponse.R'))
  
  void<-clusterEvalQ(cl, source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R'))
  rm(void)
  
  bids <- unique(brand_panel$brand_id)
  
  #res=clusterApply(cl, unique(panel$market_id), function(mid) try(analyze_market(mid, quarters=T), silent=T))
  res=clusterApply(cl, bids, function(bid) {
    out=try(newfkt(bid), silent=T)
    if (class(out)=='try-error') return('error') else return(out)
  })
  
  results_salesresponse <- res
  
  save(results_salesresponse, file = '../output/results_salesresponse.RData')
  
  
  checks=unlist(lapply(results_salesresponse, class))
  table(checks)
  which(checks!='list')
  
  errs <- bids[which(!checks=='list')]
  errs <- c(1611,1884,1885,476,169,575,1077,978,1364,731,485,1245,1647,1648,1649,1650,1651,1652,1653,1665,1482,1485,1486,1487,1488,1498,369,371,1787,1789,1790,1791,1792,1793,1794,1800,1803,1805,1254,1259,1261,1265,213,216,220,222,231,233,234,1390,613,614,615,616,617,618,619,621,622,623,629,865,866,869,874,876,877,1103,1105,1107,1109,1111,1112,1114,1118,1014,41,497,498,500,504,507,82,90,98,660,901,917,398,400,1696,1833,1149,1163,1289,1295,1030,1032,531,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,666,667,668,669,670,671,672,673,279,280,281,282,283,284,285,286,287,288,289,290,291,1539,1540,1541,1542,1543,1544,1545,1546,1547,1548,1549,1550,1551,1552,1553,1554,1555,404,405,406,407,408,409,410,411,412,413,414,415,416,417,1700,1701,1702,1703,1704,1705,1706,1707,1708,1709,1839,1840,1842,1843,1844,1845,1166,1167,1168,1169,1170,1171,1172,1173,1174,1300,1302,1303,1304,1305,1306,1307,1308,1309,1411,1412,1035,1038,1039,1042,919,920,921,540,541,542,543,546,547,548,549,119,120,121,122,123,124,126,127,128,129,130,131,132,133,676,677,678,679,680,681,682,293,294,295,296,297,298,300,301,302,303,304,306,1557,1558,1559,1560,1562,1563,1564,1567,1568,1570,1572,1573,419,420,421,422,423,424,425,427,428,430,431,432,1711,1712,1714,1715,1718,1719,1847,1848,1850,1852,1853,1176,1177,1178,1180,1181,1182,1183,1184,1310,1312,1314,1315,1317,1318,1319,1320,1414,1415,551,552,553,554,557,558,559,560,1578,1417,1418,1856,1322,1323,1326,1328,1334,1336,320,434,435,437,439,441,443,445,447,1189,135,136,137,139,140,141,145,146,564)

  newfkt(errs[10])
  
  # -> error cases

  #sapply(errs, newfkt)
  
  #> 
  
  #elast = rbindlist(lapply(res[checks=='list'], function(x) x$elasticities))
  #summary(elast)
  elast = rbindlist(lapply(res[which(checks=='list')], function(x) x$elasticities))
  
  
  elast[, lapply(.SD, mean),by=c('varname')] # positive price?!
  
  # percent positive
  elast[, lapply(.SD, function(x) length(which(x>0))/length(x)),by=c('varname')] # positive price?!
  
  #outliers?
  summary(elast)
  
  
  
  #fwrite(elast, '../temp/save_elast.csv')
  