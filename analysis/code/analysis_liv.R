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
  #source('proc_analysis_main.R')
  source('proc_liv.R')
  
  # Load library
  library(mvtnorm)
  library(stargazer)
  library(data.table)
}

init()


dir.create('../output')
#unlink('../output/')

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
 
  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  
  # total u sales
  alls=sum(brand_panel$usales,na.rm=T)
  cov=sum(brand_panel[selected==T&timewindow==T&obs48]$usales,na.rm=T)
  cov/alls # --> report in paper
  
  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand')][!grepl('allothers',brand)] # -> uniq brands
  
  
# Define additional variables
  setorder(brand_panel, market_id, brand, date)
  brand_panel[selected==T&timewindow==T, trend:=as.double(.GRP),by=c('market_id', 'brand','date')]
  brand_panel[selected==T&timewindow==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id', 'brand')]
  brand_panel[selected==T&timewindow==T, lntrend:=log(trend),by=c('market_id', 'brand')]
  
  for (q in 1:3) {  
    brand_panel[, paste0('quarter', q):=0]
    brand_panel[quarter==q, paste0('quarter', q):=1]
  }  
    
  vars=c('rwpspr', 'rwcpspr', 'rnwpr', 'llen', 'wpswdst', 'wcpswdst', 'nwwdst', 'usales', 'lagusales', 'radv')#, 'adv')
  for (var in vars) {
    brand_panel[, anyzero:=as.numeric(any(get(var)==0),na.rm=T),by=c('market_id', 'brand')]
    brand_panel[is.na(anyzero), anyzero:=0]
    brand_panel[, paste0('ln', var):=log(get(var)+anyzero), by = c('market_id', 'brand')]
    brand_panel[, paste0('dln', var):= get(paste0('ln', var))-c(NA, get(paste0('ln', var))[-.N]), by = c('market_id', 'brand')]
    brand_panel[, paste0('d', var):= get(paste0('', var))-c(NA, get(paste0('', var))[-.N]), by = c('market_id', 'brand')]
  }
  
  
  # competitive mmix
  for (v in c('rwpspr', 'rwcpspr', 'rnwpr', 'llen', 'wpswdst','wcpswdst', 'nwwdst', 'radv')) { #}, 'lnadv')) {
    setorder(brand_panel, market_id, category, country, brand, date)
    
    # includes current period
    brand_panel[, rollmean_salescp:=c(NA, NA, rollmean(usales, k = 3)), 
                by = c('market_id','brand_id')]
    brand_panel[, rollmean_salescp:=ifelse(1:.N%in%1:2, rollmean_salescp[3], rollmean_salescp), 
                by = c('market_id','brand_id')]
    
    # past-three period sales
    brand_panel[, rollmean_sales:=c(NA, rollmean_salescp[-.N]), by = c('market_id','brand_id')]
    
    # including the current period
    brand_panel[, paste0('numerator_', v):=sum(rollmean_salescp*get(v),na.rm=T), by = c('market_id', 'date')]
    brand_panel[, paste0('denominator_', v):=sum(rollmean_salescp, na.rm=T), by = c('market_id', 'date')]
    
    brand_panel[, paste0('cpcomp_', v):=(get(paste0('numerator_', v))-rollmean_salescp*get(v))/(get(paste0('denominator_', v))-rollmean_salescp)]
    brand_panel[, paste0('cpcomp_ln', v):=log(get(paste0('cpcomp_', v)))]
    
    brand_panel[, paste0('dcpcomp_',v):=get(paste0('cpcomp_', v))-c(NA, get(paste0('cpcomp_', v))[-.N]), by = c('market_id', 'brand_id')]
    brand_panel[, paste0('dcpcomp_ln',v):=get(paste0('cpcomp_ln', v))-c(NA, get(paste0('cpcomp_ln', v))[-.N]), by = c('market_id', 'brand_id')]
    
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
  
  length(unique(brand_panel[selected==T]$brand_id))
  length(unique(brand_panel[selected==T & timewindow==T &obs48==T]$brand_id))
  nrow(brand_panel[selected==T])
  nrow(brand_panel[selected==T & timewindow==T &obs48==T])
  
  brand_panel[, list(.N),by=c('category','country','brand')] # [!grepl('alloth',brand)]
  brand_panel[, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
  brand_panel[selected==T & timewindow == T & obs48 == T, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
  # we lose:
  brand_panel[selected==T & timewindow == T & obs48 == F, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
  brand_panel[selected==T & timewindow == T & obs48 == F, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)][, list(.N, avgN=mean(N)),by=c('category')]
  
  rem_obs=nrow(brand_panel[selected==T & timewindow == F & obs48 == T])
  keep_obs=nrow(brand_panel[selected==T & timewindow == T & obs48 == T])
  
 # (rem_obs)/keep_obs
  
  
  
#  , list(.N),by=c('category','country','brand')][!grepl('alloth',brand)][, list(.N, avgN=mean(N)),by=c('category')]
  
  
  brand_panel <- brand_panel[selected==T & timewindow==T &obs48==T] # at least 48 obs for estimation
  
  length(unique(brand_panel[selected==T]$brand_id))
  
  brand_panel[, list(.N),by=c('brand','category','country')][!grepl('allother',brand)]
  brand_panel[, list(.N),by=c('brand','category','country')][!grepl('allother',brand)][, list(.N),by=c('brand')] # --> uniq brand
  
  
  # Define copula terms
  for (var in c('rwpspr', 'rwcpspr', 'rnwpr', 'llen', 'wpswdst', 'wcpswdst', 'nwwdst', 'radv')) {
    brand_panel[, paste0('cop_', var):=make_copula(get(paste0(var))), by = c('market_id','brand')]
  }
  
  #brand_panel[, lngdp := log(gdppercapita)]
  brand_panel[, lnholiday := log(npublicholidays+1)]
  brand_panel[, holiday := npublicholidays]
  
  brand_panel <- brand_panel[!grepl('alloth',brand, ignore.case=T)]


  list1= brand_panel[, list(.N),by=c('category','country', 'market_id')]
  #list2=brand_panel_raw[, list(.N),by=c('category','country', 'market_id')]
  #list2[!market_id%in%list1$market_id]
  # we're losing another two tablet categories


##################
### ESTIMATION ###
##################
  
  # Prototype for one brand
  id = 100
 
  ###################################
  # PREPARE DATA FOR LIV ESTIMATION #
  ###################################
  
estimate_liv <- function(id, iter.max=1E3, n_classes=2) {
  liv_data = get_liv_data(id)
  
  # Estimate without LIV (to get starting parameters)
  m<- lm(I(dusales) ~ 1 + 
           dcomp_llen + dcomp_rwpspr + dcomp_wpswdst + lagusales + lagllen + lagrwpspr + lagwpswdst +
           trend + quarter1 + quarter2 + quarter3 + holiday + I(llen-lagllen) + I(rwpspr-lagrwpspr) + I(wpswdst-lagwpswdst), data = liv_data$df_rescaled)
  
  # Compare w/ Gaussian Copula model
  m_cop<- lm(I(dusales) ~ 1 + 
           dcomp_llen + dcomp_rwpspr + dcomp_wpswdst + lagusales + lagllen + lagrwpspr + lagwpswdst +
           trend + quarter1 + quarter2 + quarter3 + holiday + I(llen-lagllen) + I(rwpspr-lagrwpspr) + I(wpswdst-lagwpswdst) + 
             cop_llen + cop_rwpspr + cop_wpswdst, data = liv_data$df_untransformed)
  
  
  
  # Estimate on untransformed data to compare
  m_unscaled<- lm(I(dusales) ~ 1 + 
           dcomp_llen + dcomp_rwpspr + dcomp_wpswdst + lagusales + lagllen + lagrwpspr + lagwpswdst +
           trend + quarter1 + quarter2 + quarter3 + holiday + I(llen-lagllen) + I(rwpspr-lagrwpspr) + I(wpswdst-lagwpswdst), data = liv_data$df_untransformed)
  
  stargazer(m, m_cop, m_unscaled, type='text')
  
  # Assemble starting values for replication with own LL function
  
  pars = c(m$coefficients[!grepl('I[(]', names(m$coefficients))],
           m$coefficients[grepl('I[(]', names(m$coefficients))],0)
  control_pars = list(iter.max = iter.max, eval.max = 1E3, trace = 10)
  
  nlminb  = nlminb(start = pars, objective = ols_ll, df = liv_data$df_rescaled, control = control_pars)
  nlminb0  = nlminb(start = double(17), objective = ols_ll, df = liv_data$df_rescaled, control = control_pars)
  
  # Compare parameters
  cbind(nlminb$par,nlminb0$par, c(m$coefficients, NA))
  # --> equivalent; own ll function is correct
  
  # Recover & compare parameters and elasticities in transformed and untransformed OLS
  
  # Obtain untransformed means
  mean_llen = mean(liv_data$df_untransformed$llen)
  mean_pr =  mean(liv_data$df_untransformed$rwpspr)
  mean_dst = mean(liv_data$df_untransformed$wpswdst)
  mean_sales = mean(liv_data$df_untransformed$lagusales)
  
  ## Unscaled model
  # retr coef
  retr_coef=m_unscaled$coefficients[grep('I[(]', names(m_unscaled$coefficients))]
  retr_coef
  # retr elast
  retr_elast=m_unscaled$coefficients[grep('I[(]', names(m_unscaled$coefficients))]*c(mean_llen, mean_pr, mean_dst) / mean_sales
  retr_elast
  
  ## Scaled model
  # retr coef
  gamma =  m$coefficients[grep('I[(]', names(m$coefficients))]
  gamma
  
  retransform =  (liv_data$rescale_values[grepl('dusales', names(liv_data$rescale_values))] * gamma) / liv_data$rescale_values[c('llen','rwpspr','wpswdst')]
  
  retransform_elast = retransform*c(mean_llen, mean_pr, mean_dst)/mean_sales
  
  
  # assert *parameters* and *elasticities* are correctly recovered
  stopifnot(all(abs(retransform-retr_coef)<.001))
  stopifnot(all(abs(retransform_elast-retr_elast)<.001))
  
  #################
  # Extend to LIV #
  #################
  cat('\nEstimating LIV\n')
  
  # Configure starting parameters
  levels=n_classes #2
  endog=3
  pars = c(rep(0, levels*endog),
           rep(0, (levels-1)*endog),
           diag(1+endog)[upper.tri(diag(1+endog),diag=T)],
           # 1,0,1,0,0,1,0,0,0,1,
           m$coefficients[grepl('I[(]', names(m$coefficients))],#0,0,0,
           m$coefficients[!grepl('I[(]', names(m$coefficients))])
  nlminb_ols  = nlminb(start = pars, objective = liv_llik, levels=levels, no_liv=T, df=liv_data$df_rescaled, control = control_pars)

  nlminb_liv  = nlminb(start = pars, objective = liv_llik, levels=levels, no_liv=F, df=liv_data$df_rescaled, control = control_pars)

  # Compare parameters
  cbind(OLS_LL=nlminb$par,OLS_zero_LL=nlminb0$par, OLS_LM=c(m$coefficients, NA), OLS_LL_IN_LIV = c(nlminb_ols$par[23:35], nlminb_ols$par[20:22], NA),LIV=c(nlminb_liv$par[23:35], nlminb_liv$par[20:22], NA))
  
  #############################
  # Retrieve new elasticities #
  #############################
  
  # Retrieve final values of the optimizer (mapped to names)
  res <- liv_llik(nlminb_liv$par, return_llik=F, levels=levels, df = liv_data$df_rescaled)
  
  # Transform coefficients back to *untransformed variable space*
  gamma_shortterm = (liv_data$rescale_values[grepl('dusales', names(liv_data$rescale_values))] * res$gamma) / liv_data$rescale_values[c('llen','rwpspr','wpswdst')]
  
  ## Compare short-term elasticities

  # was:
  retr_elast
  
  # now is:
  elast_shortterm = (gamma_shortterm * c(mean_llen, mean_pr, mean_dst))/mean_sales
  
  # get copula values
  elast_st_copula = (m_cop$coefficients[grepl('I[(]', names(m_cop$coefficients))]* c(mean_llen, mean_pr, mean_dst))/mean_sales
  
  ## Compare long-term elasticities
  
  # was:
  step1=m_unscaled$coefficients[grepl('^lagllen|^lagrwpspr|^lagwpswdst', names(m_unscaled$coefficients))]/-(m_unscaled$coefficients[names(m_unscaled$coefficients)=='lagusales'])
  retr_elastlt = (step1 * c(mean_llen, mean_pr, mean_dst))/mean_sales
  
  # now is:
  gamma = res$beta[c('lagllen','lagrwpspr','lagwpswdst')]
  gamma_transformed = (liv_data$rescale_values[grepl('dusales', names(liv_data$rescale_values))] * gamma) / liv_data$rescale_values[c('lagllen','lagrwpspr','lagwpswdst')]
  
  lagusales_transformed = (liv_data$rescale_values[grepl('dusales', names(liv_data$rescale_values))] * res$beta['lagusales']) / liv_data$rescale_values[c('lagusales')]
  
  step1=gamma_transformed/-lagusales_transformed
  elast_longterm = (step1 * c(mean_llen, mean_pr, mean_dst))/mean_sales
  
  retr_elastlt
  elast_longterm
  
  
  # get copula LT elast
  elast_lt_copula = (m_cop$coefficients[c('lagllen','lagrwpspr','lagwpswdst')]/-m_cop$coefficients['lagusales'])*c(mean_llen, mean_pr, mean_dst)/mean_sales
                  
  
  
  elasticities = cbind(OLS_shortterm = retr_elast,
                       COP_shortterm = elast_st_copula,
                       LIV_shortterm = elast_shortterm,
                       OLS_longterm = retr_elastlt,
                       COP_longterm = elast_lt_copula,
                       LIV_longterm = elast_longterm)
  
  # IDs
  ids = brand_panel[brand_id==id][1, c('brand','category','country', 'brand_id'),with=T]
  
  result=cbind(ids,elasticities)
  result[, variable := c('llen','rwpspr','wpswdst')]
  setcolorder(result, c('brand','brand_id','category','country','variable'))   
  
  cat('\n')
  print(result)
  cat('\n')
  return(result)
  }

set.seed(1234)  
bids = sample(unique(brand_panel[!is.na(llen)]$brand_id), 100)


init()
#test=estimate_liv(bids[1], iter.max=1)
#test2=estimate_liv(100)




cl <- makePSOCKcluster(6)
clusterExport(cl,c('brand_panel', 'init'))

void<-clusterEvalQ(cl, init())

clusterExport(cl, 'estimate_liv')


all_prototype =parLapplyLB(cl, bids, function(bid) {
  try(estimate_liv(bid, iter.max=1), silent=T)
})

all_prototype3 =parLapplyLB(cl, bids, function(bid) {
  try(estimate_liv(bid, iter.max=1, n_classes=3), silent=T)
})


all = parLapplyLB(cl, bids, function(bid) {
  try(estimate_liv(bid, iter.max = 1E3), silent = T)
})

all3 = parLapplyLB(cl, bids, function(bid) {
  try(estimate_liv(bid, iter.max = 1E3, n_classes=3), silent = T)
})

liv_elast2 = rbindlist(all[unlist(lapply(all,function(x) !'try-error'%in%class(x)))])
liv_elast3 = rbindlist(all3[unlist(lapply(all3,function(x) !'try-error'%in%class(x)))])

with(liv_elast2[variable=='llen'], plot(LIV_longterm, COP_longterm,type='p'))
with(liv_elast2[variable=='llen'], cor(LIV_longterm, COP_longterm,use='pairwise'))

with(liv_elast2[variable=='rwpspr'], plot(LIV_longterm, COP_longterm,type='p'))
with(liv_elast2[variable=='rwpspr'], cor(LIV_longterm, COP_longterm,use='pairwise'))


with(liv_elast2[variable=='wpswdst'], plot(LIV_longterm, COP_longterm,type='p'))
with(liv_elast2[variable=='wpswdst'], cor(LIV_longterm, COP_longterm,use='pairwise'))

#
# - magnitude large
# - standard errors?
# 
# 
# 
# --> Copula


# starting points class mean OF boundary points
# Constrain class means 







summary(liv_elast2)
liv_elast[LIV_longterm>10]




liv_elast2[, lapply(.SD, mean), by=c('variable'), .SDcols=grep('longterm',colnames(liv_elast),value=T)]
liv_elast2[, list(.N),by=c('variable')]

liv_elast3[LIV_longterm<100, lapply(.SD, mean), by=c('variable'), .SDcols=grep('longterm',colnames(liv_elast),value=T)]
liv_elast3[LIV_longterm<100, list(.N),by=c('variable')]




# compare elasticities

tmp = melt(liv_elast2, id.vars=c('brand','brand_id','category','country','variable'))
setnames(tmp, 'variable.1', 'measure')

tmp <- tmp[grepl('longterm',measure)&grepl('cop|liv',measure,ignore.case=T)]

tmp[, ols:=ifelse(grepl('ols',measure, ignore.case=T), 1,0)]
tmp[, copula:=ifelse(grepl('cop',measure, ignore.case=T), 1,0)]
tmp[, liv:=ifelse(grepl('liv',measure, ignore.case=T), 1,0)]



#summary(lm(value~1+copula + as.factor(brand), data = tmp[variable==.v]))



mods <- lapply(unique(tmp$variable), function(.v) lm(value~1+liv+copula, data=tmp[variable==.v]))
stargazer(mods, type = 'text', column.labels = c('line length','price','distribution'))

# Nice!

# What about WLS


cor(cbind(liv_elast2$COP_longterm, liv_elast2$LIV_longterm, liv_elast2$OLS_longterm))


# Extensions

# - Debug 4 cases
# - Make function more efficient / speed up (RCPP?)
# - Hessian berekenen
# - Testen for 3 & 4 classes

# --> Hessian!
