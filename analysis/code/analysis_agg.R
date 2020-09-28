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
  for (var in c('lnrwpspr', 'lnllen', 'lnwpswdst')) {
    brand_panel[, paste0('cop_', var):=make_copula(get(paste0(var))), by = c('market_id','brand')]
    brand_panel[, paste0('cop_d.1.', var):=make_copula(dshift(get(paste0(var)))), by = c('market_id','brand')]
    
    #brand_panel[, paste0('dcop_', var):=get(paste0('cop_', var))-c(NA,get(paste0('cop_', var))[-.N]), by = c('market_id', 'brand')]
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
source('proc_salesresponse.R')
  
  
  
##################################
# COMPLETE MODEL ESTIMATION      #
##################################

bids <- unique(brand_panel$brand_id)
length(bids)


##########################
### CLUSTER ESTIMATION ###
##########################

  require(parallel)
  cl<-makePSOCKcluster(7)
  clusterExport(cl, c('brand_panel'))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(timeSeries))
  clusterEvalQ(cl, library(marketingtools))
  void<-clusterEvalQ(cl, source('proc_analysis_agg.R'))
  void<-clusterEvalQ(cl, source('proc_analysis_brand.R'))
  void<-clusterEvalQ(cl, source('proc_analysis.R'))
  void<-clusterEvalQ(cl, source('proc_ardl.R'))
  void<-clusterEvalQ(cl, source('proc_salesresponse.R'))
  void<-clusterEvalQ(cl, source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R'))
  rm(void)
  
  
  bids <- unique(brand_panel$brand_id) #[1:255]
  length(bids)
  
 # set.seed(1234)
 # bids = sample(bids, 40)


  
  results_salesresponse_max3_p10_cop = parLapplyLB(cl, bids, function(bid)
    try(model_configure(
      bid,
      withcontrols = T,
      withattributes = T,
      autocorrel_lags = c(3, 2, 1), 
      kickout_ns_controls=T,
      control_ur=T,
      pval=.1,
      with_copulas=T
      
    ),
    silent = T)
  )
  
  estimated_markets <- rbindlist(lapply(results_salesresponse_max3_p10_cop, function(x) x$paneldimension[,-c('date'),with=F][1]))
  estimated_markets[, ordered:=1:.N,by=c('market_id')]
  estimated_markets[, index:=1:.N]
  
  # ESTIMATE SUR
  sur_res = parLapplyLB(cl, split(results_salesresponse_max3_p10_cop, estimated_markets$market_id), function(focal_models) {
    mid=focal_models[[1]]$paneldimension$market_id[1]
    
    res=suppressWarnings(try(model_sur(focal_models), silent=T))
  
    list(market_id=mid, results=res)
  })
  
 #sur_res2 <- lapply(unique(estimated_markets$market_id), function(mid) list(market_id=mid, results=suppressWarnings(try(model_sur(results_salesresponse_max3_p10_cop[which(estimated_markets$market_id==mid)]),silent=T))))
 # 
 # model_sur(focal_models)
  
  # WRITE RESULTS OF SUR TO MAIN RESULT SET
  for (i in seq(along=sur_res)) {
    print(i)
    mid = unlist(lapply(sur_res, function(x) x$market_id))[i]
    for (j in estimated_markets[market_id==mid]$ordered) {
      print(paste0(' ', j))
      results_salesresponse_max3_p10_cop[[estimated_markets[market_id==mid]$index[j]]]$sur <- list(coefs=sur_res[[i]]$results$coefs[[j]],
                                                                                             varcovar=sur_res[[i]]$results$varcovar[[j]])
    }
  }
  
  # EXECUTE SIMULATIONS
  
  #for (i in seq(along=results_salesresponse_max3_p10_cop)) {
  #  print(i)
  #  model_simulate(results_salesresponse_max3_p10_cop[[i]])
  #}
  
  
  sims = parLapplyLB(cl, results_salesresponse_max3_p10_cop, function(focal_model)
    try(model_simulate(focal_model),
    silent = T)
  )
  
  # write sims to final final
  for (i in seq(along=sims)) {
    results_salesresponse_max3_p10_cop[[i]]$elasticities <- sims[[i]]$elasticities
  }
  
  summary(rbindlist(lapply(sims,function(x) x$elasticities)))
  
  
  
  
  
  
  #save(results_salesresponse_max3_p10_cop,
  #     
  #     file = '../output/results_salesresponse_comparison_upd2_sur.RData')
  
 
  
  