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

  bids <- unique(brand_panel$brand_id)
  length(bids)
  
res=lapply(unique(brand_panel$brand_id), function(i) {
  cat('\n\n NEW MODEL \n\n')
  print(i)
  cat('\n\n======================= \n\n')
  return(newfkt(
    i,
    withcontrols = T,
    withattributes = T,
    shockperiods = 12,
    ndraws = 1000,
    covar = 'yes',
    autocorrel_lags = c(1,2,3,1)
  ))
  })


# correct 
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
  
  
  results_salesresponse = clusterApply(cl, bids, function(bid) {
    #sink(file='log.txt',append=T)
    #print(bid)
    #sink()
    #print(bid)
    
    ret=try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes'
    ),
    silent = T)
    return(ret)}
  )
  
  results_salesresponse_max3 = clusterApply(cl, bids, function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      autocorrel_lags = c(1,2,3,1)
    ),
    silent = T)
  )
 
  
  
  results_salesresponse_max1 = clusterApply(cl, bids, function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      autocorrel_lags = c(1)
    ),
    silent = T)
  )
 
  results_salesresponse_max3_kickoutns = clusterApply(cl, bids, function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      autocorrel_lags = c(1,2,3,1), 
      kickout_ns_controls=T
    ),
    silent = T)
  )
  
  
  # favorite
  results_salesresponse_max3_kickoutns_urcontrols = clusterApply(cl, bids, function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      #autocorrel_lags = c(1,2,3,1), 
      kickout_ns_controls=T,
      control_ur=T 
      
    ),
    silent = T)
  )
  
  
  results_salesresponse_max3_kickoutns_urcontrols_ctrnobounds = clusterApply(cl, bids, function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      autocorrel_lags = c(1,2,3,1), 
      kickout_ns_controls=T,
      control_ur=T,
      controls_in_bounds =F
      
    ),
    silent = T)
  )
  
  # favorite
  results_salesresponse_max12_kickoutns_urcontrols = clusterApply(cl, bids[1:50], function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      #autocorrel_lags = c(1,2,3,1), 
      kickout_ns_controls=T,
      control_ur=T 
      
    ),
    silent = T)
  )
  
  # favorite
  results_salesresponse_max3_kickoutns_urcontrols_p05 = clusterApply(cl, bids, function(bid)
    try(newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      autocorrel_lags = c(1,2,3,1), 
      kickout_ns_controls=T,
      control_ur=T,
      pval=.05
      
    ),
    silent = T)
  )
  comp <- rbindlist(lapply(results_salesresponse_max12_kickoutns_urcontrols_p05, function(x) x$elasticities))
  #comp <- rbindlist(lapply(results_salesresponse_max3_kickoutns_urcontrols_ctrnobounds, function(x) x$elasticities))
  comp2 <- rbindlist(lapply(results_salesresponse_max12_kickoutns_urcontrols, function(x) x$elasticities))
  #comp2 <- rbindlist(lapply(results_salesresponse_max3_kickoutns_urcontrols, function(x) x$elasticities))
  comp[, z:=elast6/elast6_sd]
  comp2[, z:=elast6/elast6_sd]
  summary(abs(comp$z))
  summary(abs(comp2$z))
  
  
  
  for (bid in bids[200:250]) {
    print(bid)
    newfkt(
      bid,
      withcontrols = T,
      withattributes = T,
      shockperiods = 12,
      ndraws = 1000,
      covar = 'yes',
      autocorrel_lags = c(3, 2, 1), 
      kickout_ns_controls=T,
      control_ur=T,
      pval=.05
      
    )
  }
  
  
  
  save(results_salesresponse, results_salesresponse_max1, results_salesresponse_max3,
       results_salesresponse_max3_kickoutns,
       results_salesresponse_max12_kickoutns_urcontrols,
    #   results_salesresponse_max3_kickoutns_urcontrols,
    #   results_salesresponse_max3_kickoutns_urcontrols_ctrnobounds,
    #   results_salesresponse_max12_kickoutns_urcontrols,
       
       file = '../output/results_salesresponse_comparison2.RData')
  
 # save(results_salesresponse, file = '../output/results_salesresponse_se.RData')
  
  if(0){
  
    
    # Extract model names from .RData file
    lscall=ls(envir=.GlobalEnv)
    models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')
    
    # Extract elasticities
    for (model_name in models) {
      print(model_name)
      results_brands=eval(parse(text=model_name))
      
        checks=unlist(lapply(results_brands, class))
    table(checks)
    which(checks!='list')
  
   elast <- rbindlist(lapply(results_brands[checks=='list'], function(x) data.table(modeltype=x$m_final_type, x$elasticities)))
  
  #library(xlsx)
  #write.xlsx(elast, file = '../output/elasticities_revision_se.xlsx', row.names=FALSE)
  fwrite(elast, file = paste0('../output/elasticities_revision_', gsub('results[_]','', model_name), '.csv'), row.names=FALSE)
    }
  
  hist(elast[grepl('rwps',varname, ignore.case=T)]$elast6,breaks=100)
  mean(elast[grepl('rwps',varname, ignore.case=T)]$elast6,breaks=100)
  median(elast[grepl('rwps',varname, ignore.case=T)]$elast6,breaks=100)
  
  
  hist(elast[grepl('rwps',varname, ignore.case=T)]$elast12,breaks=100)
  mean(elast[grepl('rwps',varname, ignore.case=T)]$elast12,breaks=100)
  median(elast[grepl('rwps',varname, ignore.case=T)]$elast12,breaks=100)
  
  
  # just one AC remains!
  table(unlist(lapply(results_salesresponse, function(x) x$m_autocorrelation)))
  
  # percent positive
  elast[, lapply(.SD, function(x) length(which(x>0))/length(x)),by=c('varname')] # positive price?!
  
  hist(elast[grepl('pr', varname,ignore.case=T)]$elast6)
  hist(elast[grepl('llen', varname,ignore.case=T)]$elast6)
  hist(elast[grepl('dst', varname,ignore.case=T)]$elast6)
  
  }
  
  
  
  out=newfkt(1023, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar='yes', kickout_ns_controls=T,
             control_ur=T, controls_in_bounds=F)
  
  out2=newfkt(1023, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar='yes', kickout_ns_controls=T,
              control_ur=T, controls_in_bounds=T)
  
  out$elasticities
  out2$elasticities
  
  out=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar='yes', kickout_ns_controls=T)
  out2=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar='yes', kickout_ns_controls=F)
  out$elasticities
  out2$elasticities
  
  out=newfkt(151, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar='yes', kickout_ns_controls=T)
  out2=newfkt(151, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar='yes', kickout_ns_controls=F)
  out$elasticities
  out2$elasticities
  
  out2=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar = 'yes')
  out3=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=NA, ndraws=1000, covar = 'yes')
  
  out4=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=NA, ndraws=1000, covar = 'base')
  
  out4=newfkt(151, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar = 'yes')
  out4=newfkt(bids[5], withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar = 'yes')
  
  
  out4=newfkt(bids[2], withcontrols=T, withattributes=T, 
              shockperiods=12, ndraws=1000, covar = 'yes')
  
  out5=newfkt(bids[2], withcontrols=F, withattributes=T, 
              shockperiods=12, ndraws=1000, covar = 'yes',
              autocorrel_lags=c(1,2,3,1))
  
  # Case 1: max
  out5=newfkt(bids[1], withcontrols=T, withattributes=T, 
              controls_in_bounds = F, control_ur = F,
              shockperiods=12, ndraws=1000, covar = 'yes',
              autocorrel_lags=c(1,2,3,1))
  # Case 1: max
  out5=newfkt(bids[1], withcontrols=T, withattributes=T, 
              controls_in_bounds = T,
              shockperiods=12, ndraws=1000, covar = 'yes',
              autocorrel_lags=c(1,2,3,1))
  
  # with controls in EC but do not vary AC structure
  # without controls in EC, but add later as controls
   #a in levels always
   #b in differences depending on UR outcome
  
  out=newfkt(
    1023,
    withcontrols = T,
    withattributes = T,
    shockperiods = 12,
    ndraws = 1000,
    covar = 'yes',
    autocorrel_lags = c(1,2,3,1)
  )
  
  
  
  out$elasticities
  out2$elasticities
  out3$elasticities

  xout=newfkt(151, withcontrols=T, withattributes=T, shockperiods=12, ndraws=5)
  xout2=newfkt(151, withcontrols=T, withattributes=T, shockperiods=12, ndraws=1000, covar = 'yes')
  xout3=newfkt(151, withcontrols=T, withattributes=T, shockperiods=NA, ndraws=1000, covar = 'yes')
  
  xout$elasticities
  xout2$elasticities
  xout3$elasticities
  
  
   out2=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=12:14, ndraws=5)
  
  
  out=newfkt(151, withcontrols=T, withattributes=T, shockperiods=NA, ndraws=5)
  out2=newfkt(151, withcontrols=T, withattributes=T, shockperiods=12:14, ndraws=5)
  out$elasticities
  out2$elasticities
  
  
  out=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=NA, ndraws=5)
  out2=newfkt(1697, withcontrols=T, withattributes=T, shockperiods=12:14, ndraws=5)
  out$elasticities
  out2$elasticities
  
  
  out=newfkt(bids[1], withcontrols=T, withattributes=T, shockperiods=NA, ndraws=5)
  out2=newfkt(bids[1], withcontrols=T, withattributes=T, shockperiods=12:14, ndraws=5)
  out$elasticities
  out2$elasticities
  
  out=newfkt(1740, withcontrols=T, withattributes=T)
 
  out=newfkt(151, withcontrols=T, withattributes=T)
  
  
  
  
  #outliers?
  summary(elast)
  
  
  
  #fwrite(elast, '../temp/save_elast.csv')
  
  
  
  