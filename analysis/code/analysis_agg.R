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
	panel=fread('../temp/preclean_main_agg.csv')
	panel[, ':=' (date = as.Date(date))]
	setorder(panel, market_id, date)
	
	brand_panel=fread('../temp/preclean_main.csv')
	brand_panel[, ':=' (date = as.Date(date))]

	# generate brand specific weights
	weights=brand_panel[, list(upsales=sum(upsales)),by=c('market_id','brand')]
	weights[, weight:=upsales/sum(upsales), by = c('market_id')]
	

# define markets to run analysis on 
	markets <- panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
  length(analysis_markets)
 
# Define additional variables
  
  panel[selected==T, trend:=1:.N,by=c('market_id')]

  for (q in 1:3) {  
    panel[, paste0('quarter', q):=0]
    panel[quarter==q, paste0('quarter', q):=1]
  }

 brand_panel[selected==T, trend:=.GRP,by=c('market_id', 'date')]
 brand_panel[selected==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id')]
 
  
  for (q in 1:3) {  
    brand_panel[, paste0('quarter', q):=0]
    brand_panel[quarter==q, paste0('quarter', q):=1]
  }  
  
 for (m in 1:11) {  
   brand_panel[, paste0('month', m):=0]
   brand_panel[month(date)==m, paste0('month', m):=1]
 }  
 
 
  vars=c('rwpspr', 'wpsllen', 'wpswdst', 'usales', 'lagusales', 'noofbrands')
  for (var in vars) {
    panel[, anyzero:=as.numeric(any(get(var)==0),na.rm=T),by=c('market_id')]
    panel[is.na(anyzero), anyzero:=0]
    panel[, paste0('ln', var):=log(get(var)+anyzero), by = 'market_id']
    panel[, paste0('dln', var):= get(paste0('ln', var))-c(NA, get(paste0('ln', var))[-.N]), by = c('market_id')]
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
  for (var in c('rwpspr', 'wpsllen', 'wpswdst')) {
    panel[, paste0('cop_', var):=make_copula(get(paste0('ln', var))), by = c('market_id')]
    panel[, paste0('dcop_', var):=get(paste0('cop_', var))-c(NA,get(paste0('cop_', var))[-.N]), by = c('market_id')]
  }
  
  # Define copula terms
  for (var in c('rwpspr', 'llen', 'wpswdst')) {
    brand_panel[, paste0('cop_', var):=make_copula(get(paste0('ln', var))), by = c('market_id','brand')]
    brand_panel[, paste0('dcop_', var):=get(paste0('cop_', var))-c(NA,get(paste0('cop_', var))[-.N]), by = c('market_id', 'brand')]
  }
  
  # run shapiro-wilk tests to assess non-normality of untransformed inputs to the copula function
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
  
  panel[, lntrend:=log(trend)]
  
#################################
# PRELIMINARY UNIT ROOT TESTING #
#################################

source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R')

# checking order of UR for category sales
vars = c('lnusales','lnrwpspr','lnwpsllen','lnwpswdst')
tmp=panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlags=6, pval=.1)$order),by=c('market_id'), .SDcols=vars]
tmp[lnusales>1] 
any(tmp[,-1,with=F]>1)
# looks good!
colMeans(tmp)

# checking order of UR for brand sales
vars = c('lnusales','lnrwpspr','lnllen','lnwpswdst')
tmp2=brand_panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlags=6, pval=.1)$order),by=c('market_id', 'brand'), .SDcols=vars]
tmp2[lnusales>1] 
any(tmp2[,-1,with=F]>1)
# looks good, too!

##########################
# DYNAMAC ARDL PROCEDURE #
##########################

mid = 1
bid = 1


source('proc_analysis_agg.R')
source('proc_analysis.R')
source('proc_analysis_brand.R')
source('proc_ardl.R')

# select a few markets for Marnik and Hannes to investigate
#out=sapply(unique(panel$market_id)[1:5], function(mid) try(analyze_market(mid, quarters=T), silent=T))


out=sapply(unique(brand_panel$brand_id)[1:18], function(bid) try(analyze_brand(bid, quarters=T), silent=T), simplify=F)

out[[17]]

unique(brand_panel$brand_id)[1:18][grepl('stationarity in DV',unlist(out))]




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

newfkt <- function(id) {

  # 1.0 Conduct bounds test
  dv='lnusales'
  dt=data.table(brand_panel[brand_id==id])
  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  quarter_vars = c('quarter1','quarter2','quarter3')
  
  res <- analyze_brand(id, quarters=T)

  mtype = as.character(NA)
  if(grepl('stationarity in DV',gsub(' [(].*', '', res))) mtype = 'ardl-levels'
  if(grepl('cointegration$', gsub(' [(].*', '', res))) mtype = 'ardl-ec'
  if(grepl('no cointegration$', gsub(' [(].*', '', res))) mtype = 'ardl-firstdiff'
  
  cat(paste0('\n\nFinal result of bounds procedure: ', mtype,'\n\n'))

  # 2.0 Determine lag structure on the basis of `mtype`
  m<-ardl(type=mtype, dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
          adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6, controls = NULL)
  
  cat('\nResult of lag structure:\n')
  print(summary(m$model))
  
  # 3.0 Reestimate model, adding extra covariates
  
  # Public holidays
  dt[, lnhol:=log(npublicholidays+1)]
  
  # Attributes
  retain_attr=unlist(lapply(dt[, grep('^attr',colnames(dt),value=T),with=F], function(x) !all(is.na(x))))
  attr_vars = names(retain_attr[which(retain_attr==T)])

  # Reassemble model matrix and data
  df = m$model$model$model
  identifiers = dt[-m$model$model$na.action,c('market_id','date', 'brand'),with=F]
  setkey(identifiers, market_id, date, brand)

  # Define controls
  controls = c('lnhol', 'comp_lnrwpspr', 'comp_lnllen','comp_lnwpswdst', attr_vars)
  newdf <- dt[-m$model$model$na.action, controls,with=F]
  finaldf = cbind(df, newdf)

  # Add copulas (pending)
  
  # Reestimate
  m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
  cat('\nResult of final model:\n')
  print(summary(m2))
  
  # 4.0 Extract estimation data set and manipulate for simulation

  sim_dat <- function(m, shockvar=NULL, shockperiod=20, Nperiods=30, shockvalue=log(1.01)) {
    
    dv <- m$model[,1]
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    
    # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
    Xsim <- colMeans(m$model[,-1])
    
    # Set lags of main effect variables (i.e. not the DV) to the same (first) value
    for (.var in grep('l[.][0-9][.]', names(Xsim),value=T))
      blankvar = gsub('l[.][0-9][.]','',.var)
      Xsim[.var] <- Xsim[grep(paste0('^', blankvar,'|l[.][0-9][.]',blankvar), names(Xsim), value=T)[1]]
    
    # set lagged DVs to NA (0 in case it's first differenced)
    lagdvcol = grep(paste0('l[.][0-9][.]', dvblank, '|ld[.][0-9][.]', dvblank, '|l[.][0-9][.]d', dvblank), names(Xsim),value=T)
    Xsim[lagdvcol] <- NA
    Xsim[lagdvcol[grepl('l[.][0-9][.]d[.]|ld[.][0-9][.]|l[.][0-9][.]d', lagdvcol)]] <- 0
    
    # set all variables in differences to 0
    Xsim[grep('^d[.][0-9][.]', names(Xsim))] <- 0
    
    # stretch out
    Xsim_mat <- matrix(rep(Xsim,Nperiods),byrow=T, ncol=length(Xsim))
    colnames(Xsim_mat)<-names(Xsim)
    
    # set lagged DV in the first period to mean observed DV 
    meansales <- mean(dt$lnusales)
    #indiff=F
    #if (grepl('^d[.]', dvname)) indiff=T
    
    Xsim_mat[1, grep(paste0('l[.][0-9][.]', dvblank), names(Xsim),value=T)] <- meansales
    
    # Extract coefficients from model for simulation
    #shockperiod = 25
    #shockvar = vars[3]
    #shockvalue = 0 #1.1
    
    # extend lagged dependent variables
    relevant_vars=grep(paste0('l[.][0-9].', dvblank), colnames(Xsim_mat),value=T)
    for (s in seq(from=1, length.out=length(relevant_vars))) Xsim_mat[seq(from=s, to=1), relevant_vars[s]] <- meansales
    
    if ('trendvar'%in%names(Xsim)) Xsim_mat[, 'trendvar']<-1:Nperiods
  
    # collect variables pertaining to the variable that will be shocked
    relevant_vars=grep(paste0('(d[.][0-9].){0,1}', shockvar), colnames(Xsim_mat),value=T) 
    relevant_vars = grep('comp[_]', relevant_vars, invert=T,value=T)
    
    if (is.null(shockvar)|shockvar=='base') {
      # no shock
    } else {
      
      relevant_vars_levels = grep('^d[.]', relevant_vars, invert=T,value=T)
      lags = as.numeric(gsub("\\D", "", relevant_vars_levels))
      lags[is.na(lags)] <- 0
      
      for (s in seq(from=1, length.out=length(relevant_vars_levels))) Xsim_mat[shockperiod + lags[s], relevant_vars_levels[s]] <- Xsim_mat[shockperiod, relevant_vars_levels[1]]+shockvalue
      
      relevant_vars_diffs = grep('^d[.]', relevant_vars, invert=F,value=T)
      lags = as.numeric(gsub("\\D", "", relevant_vars_diffs))
      lags[is.na(lags)] <- 0
      
      for (s in seq(from=1, length.out=length(relevant_vars_diffs))) Xsim_mat[shockperiod + lags[s]-1, relevant_vars_diffs[s]] <- Xsim_mat[shockperiod, relevant_vars_diffs[1]]+shockvalue
      
      }
    
    return(list(data=Xsim_mat, shockvar = shockvar, shockperiod=shockperiod))
  }
    
  # 4.1 Generate scenarios
  sim_ds = lapply(c('base', vars), function(x) sim_dat(m=m2, shockvar=x, shockvalue = log(1.01), Nperiods = 48))
  
  ## reset copulas to zero
  #if(length(which(grepl('cop[_]', colnames(dset))))>0) coefs[which(grepl('cop[_]', colnames(dset)))]<-0

  #View(sim_ds[[2]]$data)
  
  #Xsim_mat=sim_ds[[1]]$data
  
  
  library(MASS)

  # 5.0 Predict/simulate

  my_predict <- function(m, Xsim_mat, L = 100) {
    
    # Draw coefficients
    coefs =m$coefficients
    Sigma=vcov(m)
    #Sigma = matrix(rep(0, prod(dim(Sigma))), ncol=ncol(Sigma))
    set.seed(1234)
    draws=mvrnorm(n=L, mu=coefs, Sigma = Sigma)
    
    Nperiods=nrow(Xsim_mat)
    
    predlevels <- matrix(rep(NA,Nperiods*L),ncol=L)
    
    preddiff <- matrix(rep(NA,Nperiods*L),ncol=L)
    pred <- matrix(rep(NA,Nperiods*L),ncol=L)
    
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    #Xsim <- colMeans(m$model[,-1])
    
    Xsim_mat_L <- rep(cbind(1,Xsim_mat), L)
    dim(Xsim_mat_L) <- c(nrow(Xsim_mat), ncol(Xsim_mat)+1,L)
    colnames(Xsim_mat_L) <- c('intercept', colnames(Xsim_mat))
    
   # dvmean = Xsim[grep(paste0('^l[.][0-9][.]', dvblank,'$|^', dvblank,'$'), colnames(m$model$model$model[,-1]),value=T)][1]
    
    #if(is.na(dvmean)) dvmean=mean(eval(parse(text=paste0('dt$`',dvblank,'`'))))
    
    meansales <- mean(dt$lnusales)
  
    indiff=F
    if (grepl('^d[.]|^d', dvname)) indiff=T
      
    for (p in seq(from=1, to=Nperiods)) {
      
      #pred[p] = t(coefs)%*%cbind(c(1,Xsim_mat[p,]))
      pred[p,] =  rowSums(draws*t(Xsim_mat_L[p,,])) #draws%*%Xsim_mat_L[p,,]
      
      if(indiff==T) {
        # in differences
        if (p==1) {
          predlevels[p,] = meansales + pred[p,]
          preddiff[p,] <- pred[p,]
        }
        if (p>1) {
          predlevels[p,] = predlevels[p-1,]+pred[p,]
          preddiff[p,] <- pred[p,]
        }
        
      } else {
        # in levels
        predlevels[p,] = pred[p,]
        
      }
      
      # filling lag DVs in levels
      relevant_vars=grep(paste0('l[.][0-9][.]', dvblank, '|^', dvblank), colnames(Xsim_mat),value=T)
      lags = as.numeric(gsub("\\D", "", relevant_vars))
      lags[is.na(lags)] <- 0
      
      for (nextp in seq(from=1, length.out=length(relevant_vars))) if (p+lags[nextp]<=Nperiods) Xsim_mat_L[p+lags[nextp], relevant_vars[nextp],] <- predlevels[p,]
      
      # filling lag DVs in differences
      relevant_vars=grep(paste0('ld[.][0-9][.]', dvblank, '|^', dvblank, '|l[.][0-9][.]d', dvblank), colnames(Xsim_mat),value=T)
      lags = as.numeric(gsub("\\D", "", relevant_vars))
      lags[is.na(lags)] <- 0
      
      for (nextp in seq(from=1, length.out=length(relevant_vars))) if (p+lags[nextp]<=Nperiods) Xsim_mat_L[p+lags[nextp], relevant_vars[nextp],] <- preddiff[p,]
      
    }
    
    return(list(data=Xsim_mat, means = cbind(ylevels_pred=rowMeans(predlevels), y_pred = rowMeans(pred)),
                simulated_levels = predlevels))
    
   # par(mfrow=c(3,1))
    #plot(pred, type='l')
    #plot(predlevels, type='l')
    
    #plot(m$model$model$model[,1], type='l')
    }
    
  
  # 5.1 Execute simulation

  res=lapply(sim_ds, function(x) my_predict(m2, x$data))
  #View(res[[2]])

  # 5.2 Extract elasticities
    
  varnames=unlist(lapply(sim_ds, function(x) x$shockvar))
  
  elast=rbindlist(lapply(varnames[-1], function(v) {
    #cat(v,fill=T)
    i=match(v,varnames)
    shockp=unlist(lapply(sim_ds, function(x) x$shockperiod))[i]
    
    outc=res[[i]]$simulated_levels-res[[1]]$simulated_levels
    
    plot(rowMeans(res[[i]]$simulated_levels), type='l', ylab = 'log usales', xlab = 'simulation period', main = paste0('Shock in ', v))
    lines(rowMeans(res[[1]]$simulated_levels), type='l',lty=2)
    
    elast6 = colSums(outc[shockp:(shockp+6-1),])
    elast12 = colSums(outc[shockp:(shockp+12-1),])
    elast24 = colSums(outc[shockp:(shockp+24-1),])
    
    #print(mean(elast6))
    #print(mean(elast12))
    #print(mean(elast24))
    
    return(data.frame(brand_id = id, varname=v, 
                 elast6 = mean(elast6),
                 elast12 = mean(elast12),
                 elast24 = mean(elast24),
                 elast6_sd = sd(elast6),
                 elast12_sd = sd(elast12),
                 elast24_sd = sd(elast24)))
      
      #plot(outc, type='l', main=varname)
    }))

  print(elast)
  
list(ardl_bounds = m, model_type = mtype, final_model = m2, simulation_data = sim_ds, elasticities = elast)
}
  
#  plot(dt$dlnusales,type='l')
#  plot(dt$lnusales,type='l')



id=unique(brand_panel$brand_id)[26]



out <- newfkt(id)



res=sapply(unique(brand_panel$brand_id)[1:20], function(i) {
  cat('\n\n NEW MODEL \n\n')
  print(i)
  cat('\n\n======================= \n\n')
  newfkt(i)
  })

