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
  
  setorder(brand_panel, market_id, brand, date)
  brand_panel[selected==T&timewindow==T, trend:=as.double(.GRP),by=c('market_id', 'brand','date')]
  brand_panel[selected==T&timewindow==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id', 'brand')]
  brand_panel[selected==T&timewindow==T, lntrend:=log(trend),by=c('market_id', 'brand')]
  
  
  for (q in 1:3) {  
    brand_panel[, paste0('quarter', q):=0]
    brand_panel[quarter==q, paste0('quarter', q):=1]
  }  
    
  vars=c('rwpspr', 'llen', 'wpswdst', 'usales', 'lagusales')#, grep('comp[_]', colnames(brand_panel),value=T))
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
    brand_panel[, paste0('dcomp_',v):=get(paste0('comp_', v))-c(NA, get(paste0('comp_', v))[-.N]), by = c('market_id', 'brand')]
    }
  
  
  length(unique(brand_panel[selected==T]$brand_id))
  length(unique(brand_panel[selected==T & timewindow==T &obs48==T]$brand_id))
  nrow(brand_panel[selected==T])
  nrow(brand_panel[selected==T & timewindow==T &obs48==T])
  
  brand_panel <- brand_panel[selected==T & timewindow==T &obs48==T] # at least 48 obs for estimation
  
  length(unique(brand_panel[selected==T]$brand_id))
  
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
  
  brand_panel[, lngdp := log(gdppercapita)]
  brand_panel[, lnholiday := log(npublicholidays+1)]
  
##########################
# DYNAMAC ARDL PROCEDURE #
##########################

  init <- function() {
    library(data.table)
    library(timeSeries)
    library(marketingtools)
    library(car)
    source('proc_analysis_marketshare.R')
    source('proc_analysis_ardl.R')
    source('proc_analysis_ardlbounds.R')
    source('proc_analysis_sales.R')
    source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R')
    source('proc_simplified.R')
  }
  
init()



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


bids <- unique(brand_panel$brand_id) #[1:50]
length(bids)

init()


results_ec_restricted_nocop = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_diffs='^comp[_]', 
                controls_laglevels = '',
                controls_curr = 'quarter[1-3]|lnholiday|^trend',
                controls_cop = '',
                pval = .1, kickout_ns_copula = T),
      silent = T)
)

results_ec_restricted_alwayscop = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_diffs='^comp[_]', 
                controls_laglevels = '',
                controls_curr = 'quarter[1-3]|lnholiday|^trend',
                controls_cop = '^cop[_]d[.]1[.]',
                pval = .1, kickout_ns_copula = F),
      silent = T)
)

results_ec_restricted_sigcop = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_diffs='^comp[_]', 
                controls_laglevels = '',
                controls_curr = 'quarter[1-3]|lnholiday|^trend',
                controls_cop = '^cop[_]d[.]1[.]',
                pval = .1, kickout_ns_copula = T),
      silent = T)
)


results_ec_restricted_trend = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_diffs='^comp[_]', 
                controls_laglevels = '',
                controls_curr = 'quarter[1-3]|lnholiday|^trend',
                controls_cop = '^cop[_]d[.]1[.]',
                pval = .1, kickout_ns_copula = T),
      silent = T)
)

results_ec_restricted_lntrend = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_diffs='^comp[_]', 
                controls_laglevels = '',
                controls_curr = 'quarter[1-3]|lnholiday|^lntrend',
                controls_cop = '^cop[_]d[.]1[.]',
                pval = .1, kickout_ns_copula = T),
      silent = T)
)


# Function scans global environment for occurence of regular expression (`regex`), 
# and saves all objects in `filename`.
save_by_regex <- function(regex, filename) {
  lscall = ls(envir=.GlobalEnv)
  stuff_to_save = grep(regex, lscall, value=T)
  if (length(stuff_to_save)>0) {
    cat('saving...\n')
    cat(paste0('(', paste0(stuff_to_save, collapse=', '), ')\n'))
    save(list=stuff_to_save , file = filename)
    cat('...done.\n') } else {
      cat('No objects to save. Verify regular expression.\n')
    }
}

save_by_regex('^results[_]', filename = '../output/results_simplified.RData')