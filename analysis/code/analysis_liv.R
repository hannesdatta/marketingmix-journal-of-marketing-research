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
  
  # Build data
  dt = data.table(brand_panel[brand_id==id])
  setorder(dt, category,country,brand,date)
  dv = 'usales'
  dt[, ldv := c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  dt[, ddv := get(dv)-c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  
  vars = c('rwpspr','llen','wpswdst')
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  
  controls_diffs='^comp[_](rwpspr|llen|wpswdst)$'
  controls_curr = 'quarter[1-3]|^holiday|^trend'
  pval = .1
  
  # Configure model type and calibrate lag structure
  control_vars = sapply(c(diffs=controls_diffs,curr=controls_curr), function(ctrls) if (nchar(ctrls)>0) grep(ctrls, colnames(dt),value=T))
  control_vars = lapply(control_vars, function(control_var) control_var[unlist(lapply(dt[, control_var, with=F], use_ts))])
  
  dt[, trend:=trend-mean(trend,na.rm=T)]
    
  vars_delta = paste0('d', c(vars, control_vars$diffs))
  vars_lags = paste0('lag', c(vars, control_vars$lags))
  vars_curr = control_vars$curr
    
  dt[, intercept:=1]
  df <- dt[, c('dusales', 'intercept','rwpspr','llen','wpswdst', 'lagrwpspr', 'lagllen','lagwpswdst',
                       'lagusales', 'dcomp_rwpspr', 'dcomp_llen','dcomp_wpswdst',
                       'trend','quarter1','quarter2','quarter3','holiday',
               'cop_rwpspr','cop_llen','cop_wpswdst'), with=F]
  
  df <- df[complete.cases(df),]
  
  # Rescale for better convergence
  rescale_values <- sapply(grep('intercept', colnames(df), value=T, invert=T), function(x) max(abs(drop(unlist(df[, get(x)])))))
  #rescale_values <- sapply(grep('intercept', colnames(df), value=T, invert=T), function(x) 1)
  for (.v in grep('intercept', colnames(df), value=T, invert=T)) df[, (.v) := get(.v)/rescale_values[.v]]
  
  # Load library
  library(mvtnorm)
  
  # Likelihood function
  llik <- function(pars, endogenous_variables = c('llen','rwpspr', 'wpswdst'), levels = 2, return_llik = T, no_liv = F) {
    
    # Covariance matrix of focal equation and endogenous regressors: 1 + endogenous variables (but then only lower diag)
    istart = 1
    
    .mapping = seq(from=istart, length.out=levels * length(endogenous_variables))
    .lambdas = matrix(pars[.mapping],ncol=length(endogenous_variables))
    lambdas = apply(.lambdas, 2, function(x) cumsum(c(x[1], exp(x[-1]))))
    
    istart = max(.mapping) + 1
    
    .mapping = seq(from=istart,length.out=(levels-1)*length(endogenous_variables))
    .prob = matrix(pars[.mapping],ncol=length(endogenous_variables))
    
    prob = apply(rbind(0, .prob), 2, function(x) exp(x)/sum(exp(x)))
    
    if (length(.mapping)>0) istart = max(.mapping) + 1
    
    # Covariance/correlation structure
    .uchol = diag(length(endogenous_variables) + 1)
    cov_nparameters = .5 * (length(endogenous_variables)+1)*(length(endogenous_variables)+2)
    .mapping = seq(from=istart, length.out = cov_nparameters)
    .uchol[upper.tri(.uchol,diag=T)] <- pars[.mapping]
    sigma <- crossprod(.uchol)
    
    # coefficients of endogenous variables
    istart = max(.mapping) + 1
    
    .mapping = seq(from=istart, length.out=length(endogenous_variables))
    gamma = pars[.mapping]
    
    #remaining parameters
    istart = max(.mapping) + 1
    .mapping = seq(from=istart, length.out=length(pars)-istart+1)
    betas = pars[.mapping]
  
    if (no_liv==T) {
      
      
      y_pred = df$intercept * betas[1] + 
        betas[2] * df$dcomp_llen + betas[3] * df$dcomp_rwpspr + betas[4] * df$dcomp_wpswdst +
        betas[5] * df$lagusales + 
        betas[6] * df$lagllen + betas[7] * df$lagrwpspr + betas[8] * df$lagwpswdst +
        betas[9] * df$trend + betas[10] * df$quarter1 + betas[11] * df$quarter2 + betas[12] * df$quarter3 +
        betas[13] * df$holiday + 
        gamma[1] * df$llen - gamma[1] * df$lagllen +
        gamma[2] * df$rwpspr - gamma[2] * df$lagrwpspr +
        gamma[3] * df$wpswdst - gamma[3] * df$lagwpswdst 
      
      se = exp(.uchol[1])
      
      
   #   se = sqrt(sigma[1,1])
      
      return(-sum(dnorm(df$dusales-y_pred, mean = 0, sd = se , log=T)))
    }
    
    iterating_lambdas = as.matrix(expand.grid(split(drop(lambdas), rep(1:length(endogenous_variables),each=levels))))
    
    iterating_probabilities = apply(expand.grid(split(drop(prob), rep(1:length(endogenous_variables),each=levels))),1,prod)
    
    liks = sapply(seq(from=1, to=length(iterating_probabilities)), function(l) {
      .lambda= iterating_lambdas[l,]
      
      y_pred = df$intercept * betas[1] + 
        betas[2] * df$dcomp_llen + betas[3] * df$dcomp_rwpspr + betas[4] * df$dcomp_wpswdst +
        betas[5] * df$lagusales + 
        betas[6] * df$lagllen + betas[7] * df$lagrwpspr + betas[8] * df$lagwpswdst +
        betas[9] * df$trend + betas[10] * df$quarter1 + betas[11] * df$quarter2 + betas[12] * df$quarter3 +
        betas[13] * df$holiday + 
        gamma[1] * .lambda[1] - gamma[1] * df$lagllen +
        gamma[2] * df$rwpspr - gamma[2] * df$lagrwpspr +
        gamma[3] * df$wpswdst - gamma[3] * df$lagwpswdst 
      #gamma[2] * .lambda[2] - gamma[2] * df$lagrwpspr +
      #  gamma[3] * .lambda[3] - gamma[3] * df$lagwpswdst 
      
      
      #.tmp = cbind(df$dusales-y_pred, cbind(df$llen, df$rwpspr, df$wpswdst) - matrix(rep(.lambda,length(y_pred)),ncol=length(endogenous_variables), byrow=T))
      .tmp = cbind(df$dusales-y_pred, (cbind(df$llen, df$rwpspr, df$wpswdst) - matrix(rep(.lambda,length(y_pred)),ncol=length(endogenous_variables), byrow=T))[,1])
      
      
      #dmvnorm(.tmp, mean=rep(0, each=length(endogenous_variables)+1), sigma=sigma, log=T)
      dmvnorm(.tmp, mean=rep(0, each=2), sigma=sigma[1:2,1:2], log=T)
      
    })
    
    # likelihood (L, see Biernacki et al. 2000, formula 2.2) (?)
    lprob = matrix(rep(log(iterating_probabilities),each=nrow(df)),ncol=length(iterating_probabilities))
    lprob_liks = lprob + liks
    
    # complete likelihood (CL, see Biernacki et al. 2000, formula 2.3) (?)
    max.AB = apply(lprob_liks,1,max)
    llik_min_maxab = apply(liks, 2, function(x) exp(x- max.AB))
    
    prob_times_exp = sapply(seq(from=1, to=length(iterating_probabilities)), function(i) iterating_probabilities[i]*llik_min_maxab[,i])
    llik_lse = -sum(max.AB + log(rowSums(prob_times_exp)))
    
    if (no_liv==F) {
      if (return_llik==T) {
        return(llik_lse) } else {
          return(list(neg_llik=llik_lse, sigma=sigma, lambdas=lambdas, prob=prob, gamma=gamma))
        }
    }
    
    
  }
  
  
  
  ownll <-  function(pars) {
    betas = pars[1:13]
    gamma = pars[14:16]
    se = exp(pars[17])
    
      y_pred = df$intercept * betas[1] + 
        betas[2] * df$dcomp_llen + betas[3] * df$dcomp_rwpspr + betas[4] * df$dcomp_wpswdst +
        betas[5] * df$lagusales + 
        betas[6] * df$lagllen + betas[7] * df$lagrwpspr + betas[8] * df$lagwpswdst +
        betas[9] * df$trend + betas[10] * df$quarter1 + betas[11] * df$quarter2 + betas[12] * df$quarter3 +
        betas[13] * df$holiday + 
        gamma[1] * df$llen - gamma[1] * df$lagllen +
        gamma[2] * df$rwpspr - gamma[2] * df$lagrwpspr +
        gamma[3] * df$wpswdst - gamma[3] * df$lagwpswdst 
      
    return(-sum(dnorm(df$dusales-y_pred, mean = 0, sd = se , log=T)))
    }
    
      
  # Estimate without LIV (to get starting parameters)
  m<- lm(I(dusales) ~ 1 + 
           dcomp_llen + dcomp_rwpspr + dcomp_wpswdst + lagusales + lagllen + lagrwpspr + lagwpswdst +
           trend + quarter1 + quarter2 + quarter3 + holiday + I(llen-lagllen) + I(rwpspr-lagrwpspr) + I(wpswdst-lagwpswdst), data = df) #+ cop_llen + cop_rwpspr + cop_wpswdst
  summary(m)
  
  pars = c(m$coefficients[!grepl('I[(]', names(m$coefficients))],
           m$coefficients[grepl('I[(]', names(m$coefficients))],0)
           
  nlminb  = nlminb(
    start = pars,
    objective = ownll, control = list(iter.max = 1000, eval.max = 1000)
  )
  
  nlminb0  = nlminb(
    start = double(17),
    objective = ownll, control = list(iter.max = 1000, eval.max = 1000, rel.tol=1E-12, abs.tol=1E-12)
  )
  cbind(nlminb$par,nlminb0$par, c(m$coefficients, NA))
  
  
  
  
  
  # Configure starting parameters
  levels=2
  endog=3
  pars = c(rep(0, levels*endog),
           rep(0, (levels-1)*endog),
           diag(1+endog)[upper.tri(diag(1+endog),diag=T)],
           # 1,0,1,0,0,1,0,0,0,1,
           m$coefficients[grepl('I[(]', names(m$coefficients))],#0,0,0,
           m$coefficients[!grepl('I[(]', names(m$coefficients))])
  nlminb_full  = nlminb(
    start = pars,
    objective = llik, levels= 2, no_liv=T, control = list(iter.max = 1000, eval.max = 1000)
  )
  
  nlminb_liv  = nlminb(
    start = pars,
    objective = llik, levels= 2, no_liv=F, control = list(iter.max = 1000, eval.max = 1000)
  )
  
  cbind(nlminb$par,nlminb0$par, c(m$coefficients, NA), c(nlminb_full$par[23:35], nlminb_full$par[20:22]),c(nlminb_liv$par[23:35], nlminb_liv$par[20:22]))
  
  
  # Retrieve final values (mapped to names)
  res <- llik(nlminb$par, return_llik=F, levels=levels)
  
  nlminb$par
  m$coefficients
  
  
  
  # Transform gamma back to untransformed variable space
  gamma_transformed = (rescale_values[grepl('dusales', names(rescale_values))] * res$gamma) / rescale_values[c('llen','rwpspr','wpswdst')]
  
  # Obtain elasticities
  mean_llen = mean(df$llen*rescale_values['llen'])
  mean_pr = mean(df$rwpspr*rescale_values['rwpspr'])
  mean_dst = mean(df$wpswdst*rescale_values['wpswdst'])
  mean_sales = mean(df$lagusales*rescale_values['lagusales'])
  
  (gamma_transformed * c(mean_llen, mean_pr, mean_dst))/mean_sales
  
  
  # elast from linear model
  m$coefficients[grepl('I[(](llen|rwpspr|wpswdst)', names(m$coefficients))] * c(mean_llen, mean_pr, mean_dst) / mean_sales
  
  
  
  # also compare (without endogeneity?)
  
  # what about SEs?
  
  
  
  
  # retransform values by rescale value
  
  
  set.seed(1234)
  
  llik(pars)
  
  # scaling required to spped up estimation
  
  nlminb  = nlminb(
    start = pars,
    objective = llik, levels= 3, control = list(iter.max = 1000, eval.max = 1000)
  )
  
  # gamma?
  res=llik(nlminb$par, return_llik=F, levels=2)
  
  (rescale_values[grepl('dusales', names(rescale_values))] * res$gamma) / rescale_values[grepl('^(rwpspr|llen|wpswdst)$', names(rescale_values))]
  
  # rescaling
  
  
  # normality test
  shapiro.test(df$llen)
  shapiro.test(df$rwpspr)
  shapiro.test(df$wpswdst)
  
  
  
  
  
  # best fit?
  
  
  # 
  
  m<- lm(I(dusales) ~ 1 + I(llen-lagllen) + I(rwpspr-lagrwpspr) + I(wpswdst-lagwpswdst) +
           dcomp_llen + dcomp_rwpspr + dcomp_wpswdst + lagusales + lagllen + lagrwpspr + lagwpswdst +
           trend + I(quarter1) + quarter2 + quarter3 + holiday , data = df) #+ cop_llen + cop_rwpspr + cop_wpswdst
  summary(m)
  
  
  mean(df$dusales)
  mean(df$rwpspr)
  
  1.85 * (.4405551/.01193864) # 68!
  
  1.85 * (.4405551/.01193864) # 68!
  
  1.333e+02 * (139.0254/760.1626)
  
  
  
  
  levels=2
  endog=3
  pars = c(rep(0, levels*endog),
           rep(0, (levels-1)*endog),
           diag(1+endog)[upper.tri(diag(1+endog),diag=T)],
           # 1,0,1,0,0,1,0,0,0,1,
           m$coefficients[grepl('I[(]', names(m$coefficients))],#0,0,0,
           m$coefficients[!grepl('I[(]', names(m$coefficients))])
  nlminb  = nlminb(
    start = pars,
    objective = llik, levels= 2, control = list(iter.max = 1000, eval.max = 1000)
  )
  
  # gamma?
  llik(nlminb$par, return_llik=F, levels=2)
  
  # calculate actual elasticities
  # scaling
  
  
  
  
  #s    rnorm(23+13) #rep(0,23+13)
  llik(pars, levels=3)
  
  # build in endogeneity
  
  
  
  
    if (length(c(vars,control_vars$diff))==0) vars_delta=NULL
    if (length(c(vars,control_vars$lags))==0) vars_lags=NULL
    
    if (is.null(controls_cop)) {
      vars_cop = NULL 
    } else {
      if (nchar(controls_cop)==0) {
        vars_cop=NULL
      } else {
        vars_cop = grep(controls_cop, colnames(dt), value=T)
        vars_cop = vars_cop[unlist(lapply(dt[, vars_cop, with=F], use_ts))]
      }
    }
    #if (is.null(controls_cop)) vars_cop=NULL
    
    
    for (v in vars_lags) dt[, (v):=c(NA, get(gsub('^lag','',v))[-.N])]
    
    my_form = update.formula(ddv~1, as.formula(paste0('.~.+1+', paste0(c(vars_delta, vars_cop, 'ldv', switch(length(vars_lags)>0, paste0('I(-', vars_lags,')')), vars_curr), collapse='+'))))
    
    dt[, percentile_obs:=(1:.N)/.N]
    
    
    dt[, estim_set:=T]
    
    m = lm(my_form, data= dt) #, subset = estim_set==T)
    
    #identifiers = unique(dt[,c('market_id', 'category','country', 'brand', 'brand_id' ),with=F], by=c('brand_id'))
    #setkey(identifiers, market_id, brand)
    
    # kickout coefs with NA values (e.g., attributes that are not identified)
    kickoutcoef = NULL
    if (any(is.na(m$coefficients))) kickoutcoef = c(kickoutcoef, names(m$coefficients[is.na(m$coefficients)]))
    
    # kickout ns. copulas
    if (!is.null(vars_cop) & kickout_ns_copula == T) {
      tmpres = data.table(variable=rownames(summary(m)$coefficients), summary(m)$coefficients)[grepl(controls_cop, variable)]
      setnames(tmpres, c('variable','est','se','t','p'))
      tmpres = tmpres[p>pval]
      
      if (nrow(tmpres)>0) kickoutcoef = c(kickoutcoef, tmpres$variable)
    }
    
    if (length(kickoutcoef>0)) {
      my_form =update.formula(my_form, as.formula(paste0('.~.-', paste0(kickoutcoef, collapse='-'))))
      m2 = lm(my_form, data= dt, subset = estim_set==T)
    } else {m2=m}
    
    
    # block holdouts
    
    kfolds=10
    
    folds = split(1:nrow(dt), cut(1:nrow(dt), kfolds))
    iter<-0
    preds_new=rbindlist(lapply(folds, function(pset) {
      iter<<-iter+1
      obs<<-setdiff(1:nrow(dt), c(pset)) #, max(pset)+1))
      newpset = seq(from=min(pset)+1, to=max(pset)-1)
      m3=update(m2, .~.,subset=obs)
      
      combobs = union(newpset, obs)
      combobs = combobs[order(combobs)]
      res=data.table(dt[combobs, c('date', 'ddv','ldv',dv),with=F], kfold=iter,ddv_hat=predict(m3, newdata=dt[combobs,]))
      res[, estim_set:=combobs%in%obs]
      return(res)
    }))
    
    pred_new3 = merge(dt[, c('category','country','brand','date'),with=F],
                      preds_new,
                      by=c('date'),all.x=F)
    
    
    # within-predictions   
    predictions = cbind(dt[, c('category','country','brand','date', 'estim_set', dv, 'ldv','ddv'),with=F],
                        ddv_hat=predict(m2, newdata=dt))
    predictions[, paste0(dv,'_hat') := ldv + ddv_hat]
    
    
    
    if (!is.null(m2$na.action)) identifiers = dt[-m2$na.action,c('market_id', 'category','country', 'brand', 'brand_id' , 'date'),with=F]
    if (is.null(m2$na.action)) identifiers = dt[,c('market_id', 'category','country', 'brand', 'brand_id' , 'date'),with=F]
    
    setkey(identifiers, market_id, date, brand)
    
    # for linear model, need to multiply elasticities by mean X / mean sales.
    means = unlist(dt[, lapply(.SD, mean,na.rm=T), .SDcols=c(dv,vars)])
    medians = unlist(dt[, lapply(.SD, median,na.rm=T), .SDcols=c(dv,vars)])
    multipliers_means <- rep(1, length(vars))
    if (!grepl('^ln|^log', dv)) multipliers_means <- sapply(vars, function(.v) means[.v]/means[dv])
    multipliers_medians <- rep(1, length(vars))
    if (!grepl('^ln|^log', dv)) multipliers_medians <- sapply(vars, function(.v) medians[.v]/medians[dv])
    
    names(multipliers_means)<-vars
    names(multipliers_medians)<-vars
    
    if (length(vars)>0) {
      elast2 = rbindlist(lapply(vars, function(v) {
        st=deltaMethod(m2, paste0(multipliers_means[v],'*(d', v, ')'))
        
        if (paste0('I(-lag', v, ')') %in% names(m2$coefficients)) {
          lt=deltaMethod(m2, paste0(multipliers_means[v],'*((`I(-lag', v, ')`)/(ldv))')) } else {
            lt=deltaMethod(m2, paste0(multipliers_means[v],'*((0)/(ldv))'))
          }
        # at the median
        st_median=deltaMethod(m2, paste0(multipliers_medians[v],'*(d', v, ')'))
        
        if (paste0('I(-lag', v, ')') %in% names(m2$coefficients)) {
          lt_median=deltaMethod(m2, paste0(multipliers_medians[v],'*((`I(-lag', v, ')`)/(ldv))')) } else {
            lt_median=deltaMethod(m2, paste0(multipliers_medians[v],'*((0)/(ldv))'))
          }
        
        data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
                   elastlt = lt$Estimate, elastlt_se = lt$SE, 
                   
                   elastmedian = st_median$Estimate, elastmedian_se = st_median$SE,
                   elastmedianlt = lt_median$Estimate, elastmedianlt_se = lt_median$SE,
                   beta = m2$coefficients[paste0('d',v)], 
                   carryover = m2$coefficients['ldv'])
        
      }))
      
      uniq_identifiers = copy(identifiers)[1][, date:=NULL]
      elast2=cbind(uniq_identifiers,elast2)
      .v=vif(m2)
      vif2=cbind(uniq_identifiers, data.table(variable=names(.v), vif=.v))
    } else {
      elast2=NULL
      vif2=NULL
    }
    
    return(list(elast=elast2, model=m2, vif=vif2,
                paneldimension = identifiers,
                model_matrix = m2$model,
                dt=dt,
                orig_results = m$coefficients, kicked_out_coefs = kickoutcoef,
                predictions=predictions, predictions_kfold = pred_new3, #,#predictions_kfold=pred_new3, 
                r2_within_dv= summary(m2)$r.squared))
    
  }
  
  