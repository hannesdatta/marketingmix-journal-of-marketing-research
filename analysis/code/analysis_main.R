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


rm(list = ls())

### LOAD DATA SETS

library(parallel)
library(devtools)
library(zoo)

init <- function() {
  library(data.table)
  library(bit64)
  library(timeSeries)
  library(marketingtools)
  library(car)
  source('proc_auxilary.R')
  source('proc_analysis_main.R')
}

init()


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
 
# view number of included brands  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  
# Define trend
  setorder(brand_panel, market_id, brand, date)
  brand_panel[selected==T&timewindow==T, trend:=as.double(.GRP),by=c('market_id', 'brand','date')]
  brand_panel[selected==T&timewindow==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id', 'brand')]
  brand_panel[selected==T&timewindow==T, lntrend:=log(trend),by=c('market_id', 'brand')]
  
# Define quarterly dummies
  for (q in 1:3) {  
    brand_panel[, paste0('quarter', q):=0]
    brand_panel[quarter==q, paste0('quarter', q):=1]
  }  
    
# Define focal marketing mix instruments
  vars=c('rwpspr', 'llen', 'wpswdst', 'usales', 'lagusales', 'radv')
  for (var in vars) {
    brand_panel[, anyzero:=as.numeric(any(get(var)==0),na.rm=T),by=c('market_id', 'brand')]
    brand_panel[is.na(anyzero), anyzero:=0]
    brand_panel[, paste0('ln', var):=log(get(var)+anyzero), by = c('market_id', 'brand')]
    brand_panel[, paste0('dln', var):= get(paste0('ln', var))-c(NA, get(paste0('ln', var))[-.N]), by = c('market_id', 'brand')]
    brand_panel[, paste0('d', var):= get(paste0('', var))-c(NA, get(paste0('', var))[-.N]), by = c('market_id', 'brand')]
  }
  
  
# Define competitive marketing-mix
  for (v in c('rwpspr', 'llen', 'wpswdst', 'radv')) { 
    setorder(brand_panel, market_id, category, country, brand, date)
    
    # includes current period
    brand_panel[, rollmean_salescp:=c(NA, NA, rollmean(usales, k = 3)), 
                by = c('market_id','brand_id')]
    brand_panel[, rollmean_salescp:=ifelse(1:.N%in%1:2, rollmean_salescp[3], rollmean_salescp), 
                by = c('market_id','brand_id')]
    
    # past-three period sales
    brand_panel[, rollmean_sales:=c(NA, rollmean_salescp[-.N]), by = c('market_id','brand_id')]
    
    # excluding current period
    brand_panel[, paste0('numerator_', v):=sum(rollmean_sales*get(v),na.rm=T), by = c('market_id', 'date')]
    brand_panel[, paste0('denominator_', v):=sum(rollmean_sales, na.rm=T), by = c('market_id', 'date')]
    
    brand_panel[, paste0('comp_', v):=(get(paste0('numerator_', v))-rollmean_sales*get(v))/(get(paste0('denominator_', v))-rollmean_sales)]
    brand_panel[, paste0('comp_ln', v):=log(get(paste0('comp_', v)))]
    
    
    brand_panel[, paste0('dcomp_',v):=get(paste0('comp_', v))-c(NA, get(paste0('comp_', v))[-.N]), by = c('market_id', 'brand_id')]
    brand_panel[, paste0('dcomp_ln',v):=get(paste0('comp_ln', v))-c(NA, get(paste0('comp_ln', v))[-.N]), by = c('market_id', 'brand_id')]
    
    brand_panel[, paste0('numerator_', v):=NULL]
    brand_panel[, paste0('denominator_', v):=NULL]
    
    }
 

# Retain only brands/time periods for estimation
  brand_panel <- brand_panel[selected==T & timewindow==T &obs48==T] 

  
# Define copula terms
  for (var in c('rwpspr', 'llen', 'wpswdst', 'radv')) {
    brand_panel[, paste0('cop_', var):=make_copula(get(paste0(var))), by = c('market_id','brand')]
  }
  
# Assess share of normality
  norm_tests <- rbindlist(lapply(c('rwpspr','llen','wpswdst'), function(.v) brand_panel[selected==T&timewindow==T&obs48==T&!is.na(get(.v))&!grepl('allother',brand), list(shapiro_p_val=shapiro.test(get(.v))$p), by = c('category','country','brand')][,variable:=.v]))
  norm_tests[, list(length(which(shapiro_p_val<=.1))/.N),by=c('variable')]
  norm_tests[, list(length(which(shapiro_p_val<=.1))/.N)]
  
# Remaining control variables  
  brand_panel[, lnholiday := log(npublicholidays+1)]
  brand_panel[, holiday := npublicholidays]
 
# Remove "all other" (composite rest brand) from data 
  brand_panel <- brand_panel[!grepl('alloth',brand, ignore.case=T)]


##########################
### CLUSTER ESTIMATION ###
##########################

# build parallel computation environment
  ncpu=7 # number of CPUs
  try(stopCluster(cl),silent=T) # verify cluster isn't running yet

  cl<-makePSOCKcluster(ncpu) # create cluster
  clusterExport(cl,c('brand_panel', 'init')) # export data
  void<-clusterEvalQ(cl, init()) # load packages

# get brand IDs to estimate models
  bids <- unique(brand_panel$brand_id)
  length(bids)

  init()


estim_model <- function(model_name, fun, ...) {
  prototype=F
  if (!file.exists(paste0('../output/', model_name, '.RData'))|prototype==T) {
    cat(paste0('Estimating ', model_name, '...\n'))
    results_model = parLapplyLB(cl, bids, function(bid)
      try(eval(parse(text=paste0(fun, '(bid, ...)'))), silent=T)
    )
    eval(parse(text=paste0(model_name, "<<-results_model")))
    
    if (!prototype) eval(parse(text=paste0('save(', model_name, ', file = \"../output/', model_name, '.RData\")')))
    rm(results_model)
  } else {
    cat(paste0('Model ', model_name, ' already exists!\n'))
  }
}

############################
### DEFINITION OF MODELS ###
############################


## MAIN MODEL
estim_model('results_ec_main', 'estimate_ec')

#----

## MAIN MODEL w/ PRODUCT ATTRIBUTES
estim_model('results_ec_main_attributes', 'estimate_ec', controls_curr = 'quarter[1-3]|^holiday|^trend|^attr')

## MAIN MODEL W/ LONG-TERM COMPETITIVE EFFECTS
estim_model('results_ec_unrestrictedcompetition', 'estimate_ec', controls_laglevels = '^comp[_](rwpspr|llen|wpswdst)$')

## MAIN MODEL WITH LOG TREND (INSTEAD OF LINEAR TREND)
estim_model('results_ec_lntrend', 'estimate_ec', controls_curr = 'quarter[1-3]|lnholiday|^lntrend')

#----

## MODELS FOR NESTED F-TESTS
## (all estimated without Copula terms)

### BECHMARK MODELS

estim_model('results_ec_main_nc', 'estimate_ec', controls_cop = NULL)

estim_model('results_ec_noocmmixnc', 'estimate_ec', vars = NULL, controls_cop = NULL, controls_diff = NULL)

### REMOVING FOCAL AND COMPETITIVE MARKETING MIX INSTRUMENTS ITERATIVELY

estim_model('results_ec_remocprnc', 'estimate_ec', vars = c('llen','wpswdst'), controls_diffs='^comp[_](llen|wpswdst)$', 
            controls_cop = NULL)

estim_model('results_ec_remocllennc', 'estimate_ec', vars = c('rwpspr','wpswdst'), controls_diffs='^comp[_](rwpspr|wpswdst)$', 
            controls_cop = NULL)

estim_model('results_ec_remocdstnc', 'estimate_ec', vars = c('rwpspr','llen'), controls_diffs='^comp[_](rwpspr|llen)$', 
            controls_cop = NULL)

### ADDING FOCAL AND COMPETITIVE MARKETING MIX INSTRUMENTS ITERATIVELY

estim_model('results_ec_onlyocprnc', 'estimate_ec', vars = c('rwpspr'), controls_cop = NULL,
            controls_diff = '^comp[_](rwpspr)$')

estim_model('results_ec_onlyocllennc', 'estimate_ec', vars = c('llen'), controls_cop = NULL,
            controls_diff = '^comp[_](llen)$')

estim_model('results_ec_onlyocdstnc', 'estimate_ec', vars = c('wpswdst'), controls_cop = NULL,
            controls_diff = '^comp[_](wpswdst)$')

#----

## LINEAR SALES RESPONSE MODEL, CORRESPONDING TO THE MAIN MODEL
estim_model('results_salesresponse_linear', 'estimate_salesresponse')

## LINEAR SALES RESPONSE MODEL, CORRESPONDING TO THE MAIN MODEL (WITHOUT LAGED DV)
estim_model('results_salesresponse_linear_noldv', 'estimate_salesresponse', withlagdv=F)

## LINEAR SALES RESPONSE MODEL, ONLY W/ A LAGGED DEPENDENT VARIABLE
estim_model('results_salesresponse_linear_onlyldv', 'estimate_salesresponse', withlagdv=T, vars=NULL, controls=NULL)


###################################
# FUNCTION TO ESTIMATE SUR MODELS #
###################################

do_sur <- function(results_model = results_ec_main) {
  
  estimated_markets <- rbindlist(lapply(results_model, function(x) x$paneldimension[,-c('date'),with=F][1]))
  estimated_markets[, ordered:=1:.N,by=c('market_id')]
  estimated_markets[, index:=1:.N]
  
  # ESTIMATE SUR
  split_by_market = split(results_model, estimated_markets$market_id)
  
  cat(paste0('Estimating SUR for ', length(split_by_market), ' markets...\n'))
  
  sur_res = parLapplyLB(cl, split_by_market, function(focal_models) {
    mid=focal_models[[1]]$paneldimension$market_id[1]
    cat(mid,fill=T)
    
    res=suppressWarnings(try(model_sur(focal_models), silent=T))
    if(class(res)=='try-error') res=suppressWarnings(try(model_sur(focal_models, maxiter=1), silent=T))
    
    
    list(market_id=mid, results=res)
  })
  
  table(unlist(lapply(sur_res,function(x) class(x$results))))
  which(unlist(lapply(sur_res,function(x) class(x$results)))=='try-error')
  
  cat(paste0('Wrangling data...',fill=T))
  
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
  
  
  cat('Calculating elasticities', fill=T)
  results_with_sur_models = parLapplyLB(cl, results_model, function(x) {
    try(process_sur(x), silent=T)
  })
  
  # remove models
  ret_model <- lapply(results_with_sur_models, function(x) {
    x$model_matrix <- NULL
    x$paneldimension <- NULL
    x$dt <- NULL
    x$elast_sur$country=x$elast$country
    x$elast_sur$category=x$elast$category
    x$elast_sur$brand=x$elast$brand
    x$elast_sur$brand_id=x$elast$brand_id
    
    
    x$elast <- x$elast_sur
    
    x
  })
  
  
  rm(results_with_sur_models)
  rm(results_model)

  return(ret_model)
}

## Execute SUR computation

# Load models
if (!file.exists('../output/results_ec_main_sur.RData')) {
  load('../output/results_ec_main.RData')
  results_ec_main_sur <- do_sur(results_ec_main)
  save(results_ec_main_sur, file = '../output/results_ec_main_sur.RData')
}

surs <- c('results_ec_main_attributes', 'results_ec_unrestrictedcompetition')

for (s in surs) {
  cat(paste0('Calculating SUR for ', s), fill=T)
  if (!file.exists(paste0('../output/', s, '_sur.RData'))) {
    load(paste0('../output/', s, '.RData'))
    eval(parse(text=paste0(s, '_sur <- do_sur(', s, ')')))
    cat('starting to save...', fill=T)
    eval(parse(text=paste0('save(', s, '_sur, file = \'../output/', s, '_sur.RData\')'))) 
  }
}


######################################
# Robustness w/ advertising spending #
######################################

brand_panel[, has_adv:=sum(radv>0)>=12,by=c('category','country','market_id','brand_id')]
  
china_hk_selection = brand_panel[country%in%c('china','hong kong')&
                                   has_adv==T & date<ifelse(country=='china', '2012-09-01', '2014-12-01'), 
                                 list(obs= length(unique(date))),by=c('category','country', 'market_id', 'brand_id')]

china_hk_selection = china_hk_selection[, selected:=obs>ifelse(category=='tablet',4*12,5*12)]
china_hk = unique(china_hk_selection[selected==T]$brand_id)

length(china_hk)

# Locate markets to estimate model on
table(sapply(china_hk, function(bid) use_ts(brand_panel[brand_id==bid]$adv)))
brand_panel[brand_id%in%china_hk, list(length(unique(brand))),by=c('category','country')]

old_bids = bids

bids = china_hk
estim_model('results_ec_chinahk_withadv', 'estimate_ec', vars = c('rwpspr','llen','wpswdst', 'radv'),
            controls_diffs='^comp[_](rwpspr|llen|wpswdst|radv)$', 
            controls_cop = '^cop[_](rwpspr|llen|wpswdst|radv)$')

estim_model('results_ec_chinahk_withoutadv', 'estimate_ec')

# Estimate SUR
surs <- c('results_ec_chinahk_withadv', 'results_ec_chinahk_withoutadv')

for (s in surs) {
  cat(paste0('Calculating SUR for ', s), fill=T)
  if (!file.exists(paste0('../output/', s, '_sur.RData'))) {
    load(paste0('../output/', s, '.RData'))
    eval(parse(text=paste0(s, '_sur <- do_sur(', s, ')')))
    eval(parse(text=paste0('save(', s, '_sur, file = \'../output/', s, '_sur.RData\')'))) 
  }
}

bids <- old_bids

###################################################
# Robustness w/ first and last x% of observations #
###################################################

save_brandpanel = copy(brand_panel)

brand_sel = brand_panel[, list(obs = sum(usales>0,na.rm=T)), by = c('category','country','brand', 'brand_id')]
brand_sel[, selected:=obs>=8*12]

selected_bids = brand_sel[selected==T]$brand_id
length(selected_bids)

perc_select=.6
brand_panel[!is.na(usales), percentile_obs:=(1:.N)/.N, by = c('category','country','brand')]
brand_panel = brand_panel[percentile_obs<=perc_select & brand_id %in% selected_bids]

# First x%

clusterExport(cl,c('brand_panel'))
bids = unique(brand_panel$brand_id)
length(bids)

estim_model('results_ec_first60', 'estimate_ec')
if (!exists('results_ec_first60')) load('../output/results_ec_first60.RData')

if (!file.exists('../output/results_ec_first60_sur.RData')) {
  results_ec_first60_sur <- do_sur(results_ec_first60)
  save(results_ec_first60_sur, file = '../output/results_ec_first60_sur.RData')
}


# Last x%
brand_panel = save_brandpanel

brand_panel[!is.na(usales), percentile_obs:=(1:.N)/.N, by = c('category','country','brand')]
brand_panel = brand_panel[percentile_obs>=(1-perc_select) & brand_id %in% selected_bids]

clusterExport(cl,c('brand_panel'))
bids = unique(brand_panel$brand_id)
length(bids)

estim_model('results_ec_last60', 'estimate_ec')
if (!exists('results_ec_last60')) load('../output/results_ec_last60.RData')

if (!file.exists('../output/results_ec_last60_sur.RData')) {
  results_ec_last60_sur <- do_sur(results_ec_last60)
  save(results_ec_last60_sur, file = '../output/results_ec_last60_sur.RData')
}

# Generate file to indicate the script has completed.

sink('../output/results_main.txt')
cat('Done.')
sink()
