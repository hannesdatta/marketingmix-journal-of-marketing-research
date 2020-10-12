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
rm(list = ls())

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
  brand_panel <- brand_panel[!brand=='mobistar']
  
# define markets to run analysis on 
	markets <- brand_panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
  length(analysis_markets)
 
# Define additional variables
  setorder(brand_panel, market_id, brand, date)
  brand_panel[selected==T, trend:=.GRP,by=c('market_id', 'brand','date')]
  brand_panel[selected==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id', 'brand')]
  brand_panel[selected==T, trend:=log(trend),by=c('market_id', 'brand')]
  
  
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
  }
  
  # run shapiro-wilk tests to assess non-normality of untransformed inputs to the copula function
  if(0){
  vars=c('lnrwpspr', 'lnwpsllen', 'lnwpswdst')
  cop=lapply(vars, function(var) {
    out=panel[, list(shap_pval = shapiro.test(get(var))$p), by = c('category', 'country', 'market_id')]
    out[, variable:=var]
    return(out)
  })
    
  cop=rbindlist(cop)
  
  cop[, list(nonnormal_share=length(which(shap_pval<.1))/.N), by = c('variable')]
  }
  
  brand_panel <- brand_panel[!grepl('allothers|unbranded|^local|^super|^amazon',brand,ignore.case=T)]
  
##########################
# DYNAMAC ARDL PROCEDURE #
##########################

  init <- function() {
    library(data.table)
    library(timeSeries)
    library(marketingtools)
    source('proc_analysis_marketshare.R')
    source('proc_analysis_ardl.R')
    source('proc_analysis_ardlbounds.R')
    source('proc_analysis_sales.R')
    source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R')
    }
  
init()

  
  
##################################
# COMPLETE MODEL ESTIMATION      #
##################################

get_elasticities <- function(results_model, simulate = T) {
  
    estimated_markets <- rbindlist(lapply(results_model, function(x) x$paneldimension[,-c('date'),with=F][1]))
    estimated_markets[, ordered:=1:.N,by=c('market_id')]
    estimated_markets[, index:=1:.N]
    
    # ESTIMATE SUR
    split_by_market = split(results_model, estimated_markets$market_id)
    
    cat(paste0('Estimating SUR for ', length(split_by_market), ' markets...\n'))
    
    sur_res = parLapplyLB(cl, split_by_market, function(focal_models) {
      mid=focal_models[[1]]$paneldimension$market_id[1]
      cat(mid,fill=T)
      #if(0){
      res=suppressWarnings(try(model_sur(focal_models), silent=T))
      if(class(res)=='try-error') res=suppressWarnings(try(model_sur(focal_models, maxiter=1), silent=T))
      #}
      #res=suppressWarnings(try(model_sur(focal_models, maxiter=1), silent=T))
      
      list(market_id=mid, results=res)
    })
   
    # verify errors in SUR estimation
    cat('Reporting on errors in SUR estimation...\n')
    sur_errs <- which(unlist(lapply(sur_res, function(x) class(x$results)))=='try-error')
    print(sur_errs)
    
   # WRITE RESULTS OF SUR TO MAIN RESULT SET
    for (i in seq(along=sur_res)) {
      mid = unlist(lapply(sur_res, function(x) x$market_id))[i]
      for (j in estimated_markets[market_id==mid]$ordered) {
        ck=try(sur_res[[i]]$results$coefs[[j]],silent=T)
        if (class(ck)!='try-error') {
          results_model[[estimated_markets[market_id==mid]$index[j]]]$sur <- list(coefs=sur_res[[i]]$results$coefs[[j]],
                                                                                               varcovar=sur_res[[i]]$results$varcovar[[j]])
        }
      }
    }
    
    if (simulate==T) {
      # EXECUTE SIMULATIONS
      cat('Start the simulations...\n')
      
      sims = parLapplyLB(cl, results_model, function(focal_model) {
        if (is.null(focal_model$sur)) return(NULL)
        try(model_simulate(focal_model),
        silent = T)
      }
      )
      
      # write sims to final final
      for (i in seq(along=sims)) {
        results_model[[i]]$elasticities <- sims[[i]]$elasticities
      }
    }
    cat('Done.\n')
    return(results_model)
  }
  
  
##########################
### CLUSTER ESTIMATION ###
##########################

require(parallel)

# try whether cluster exists
ncpu=7
try(stopCluster(cl),silent=T)

cl<-makePSOCKcluster(ncpu)
clusterExport(cl,c('brand_panel', 'init'))
void<-clusterEvalQ(cl, init())
init()


bids <- unique(brand_panel$brand_id)
length(bids)

results_salesresponse_max3_p10_cop = parLapplyLB(cl, bids, function(bid)
    try(model_configure(
      bid,
      withcontrols = T,
      withattributes = T,
      autocorrel_lags = c(3,2,1), 
      kickout_ns_controls=T,
      control_ur=T,
      pval=.1,
      with_copulas=T,
      return_models=F
      
    ),
    silent = T)
  )
#save(results_salesresponse_max3_p10_cop, file = '../output/results_sales_notfinal.RData')

load('../output/results_sales_notfinal.RData')

table(unlist(lapply(results_salesresponse_max3_p10_cop, class)))


results2 = get_elasticities(results_salesresponse_max3_p10_cop, simulate = F)


results_salesresponse_max3_p10_cop_sur_full <- results

#save(results_salesresponse_max3_p10_cop_sur, file = '../output/results_sales.RData')

#elast_sum = rbindlist(lapply(results,function(x) x$elasticities))
#summary(elast_sum)

results_salesresponse_max3_p10_cop_sur <- lapply(results, function(x) {x$dt=NULL;x$model_matrix=NULL;return(x)})
  
save(results_salesresponse_max3_p10_cop_sur, file = '../output/results_sales.RData')

save(results_salesresponse_max3_p10_cop_sur_full, file = '../output/results_sales_full.RData')
