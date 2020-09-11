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
  
  brand_panel <- brand_panel[!grepl('allothers|unbranded|^local|^super|^amazon',brand,ignore.case=T)]
  
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

res=lapply(unique(brand_panel$brand_id)[1:20], function(i) {
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
  length(bids)
  
  results_salesresponse=clusterApply(cl, bids, function(bid) try(newfkt(bid), silent=T))
  
  elast <- rbindlist(lapply(results_salesresponse[checks=='list'], function(x) x$elasticities))
  
  save(results_salesresponse, file = '../output/results_salesresponse.RData')
  
  results_salesresponse2 <- lapply(results_salesresponse, function(x) x$elasticities)
  
  save(results_salesresponse2, file = '../output/results_salesresponse2.RData')
  
  checks=unlist(lapply(results_salesresponse, class))
  table(checks)
  which(checks!='list')
  
  # just one AC remains!
  table(unlist(lapply(results_salesresponse, function(x) x$m_lagstructure$autocorrelation)))
  
  
  #summary(elast)
  elast = rbindlist(lapply(results_salesresponse[which(checks=='list')], function(x) x$elasticities))
  
  
  elast[, lapply(.SD, mean),by=c('varname')] # positive price?!
  
  # ctry
  setkey(elast, brand_id)
  setkey(brand_panel, brand_id)
  elast[brand_panel, ':=' (developed=i.developed, country=i.country, category=i.category)]
  
  summary(lm(elast6~1+developed, data=elast[grepl('pr', varname,ignore.case=T)]))
  summary(lm(elast6~1+developed, data=elast[grepl('pr', varname,ignore.case=T)], weights=1/elast6_sd))
  summary(lm(elast12~1+developed, data=elast[grepl('pr', varname,ignore.case=T)], weights=1/elast12_sd))
  
  
  summary(lm(elast6~1+developed, data=elast[grepl('dst', varname,ignore.case=T)]))
  summary(lm(elast6~1+developed, data=elast[grepl('dst', varname,ignore.case=T)], weights=1/elast6_sd))
  summary(lm(elast12~1+developed, data=elast[grepl('dst', varname,ignore.case=T)], weights=1/elast12_sd))
 
  summary(lm(elast6~1+developed, data=elast[grepl('llen', varname,ignore.case=T)]))
  summary(lm(elast6~1+developed, data=elast[grepl('llen', varname,ignore.case=T)], weights=1/elast6_sd))
  summary(lm(elast12~1+developed, data=elast[grepl('llen', varname,ignore.case=T)], weights=1/elast12_sd))
  
  
  out=newfkt(1697, withcontrols=F, withattributes=T)
  
  
  # percent positive
  elast[, lapply(.SD, function(x) length(which(x>0))/length(x)),by=c('varname')] # positive price?!
  
  hist(elast[grepl('pr', varname,ignore.case=T)]$elast6)
  hist(elast[grepl('llen', varname,ignore.case=T)]$elast6)
  hist(elast[grepl('dst', varname,ignore.case=T)]$elast6)
  
  
  #outliers?
  summary(elast)
  
  
  
  #fwrite(elast, '../temp/save_elast.csv')
  
  
  
  