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


###########################
# CLUSTER ESTIMATION      #
###########################

# Roadmap:

# - determine "case"
# - 
# - competing marketing mix? / first differences normaal
# - attribute: levels erin steken, not differnces



# Save cases DONE
# Prototype model estimation, depending on the case
# Determine exact configuration of lag structure and models-to-be-estimated
# Estimate + simulate elasticities

# How to combine final case w/ exact model? E.g., simulations? Needed?


#######################################################
# Save configuration (extracted from model)
# Estimate model and simulate in another function
# + add Copulas
#######################################################



# To do:

# "retrieve" resulting model, reestimate
quarter_vars = c('quarter1','quarter2','quarter3')

unique(brand_panel$brand_id)[1:18][grepl('stationarity in DV',unlist(out))]
# -> 1736 1739  951  952  953  958

#
unique(brand_panel$brand_id)[1:18][grepl('cointegration$',unlist(out))]
# -> 1737 1738 1740 1741 1742  956  957

unique(brand_panel$brand_id)[1:18][grepl('no cointegration$',unlist(out))]
# ->  1741 1742  956  957




# simulation
dv='lnusales'
id=1742

dt=data.table(brand_panel[brand_id==id])
vars = c('lnrwpspr','lnllen','lnwpswdst') 

# Marnik: can there be any LT effect in case of no cointegration?
# Include vars as controls already when testing? (but assume they are there, either in levels or diffs?)

# re-estimate model
controls = NULL # c('comp_lnrwpspr', 'comp_lnllen','comp_lnwpswdst', 'npublicholidays')


res <- analyze_brand(id, quarters=T)
mtype = as.character(NA)
if(grepl('stationarity in DV',res)) mtype = 'ardl=levels'
if(grepl('cointegration$',res)) mtype = 'ardl-ec'
if(grepl('no cointegration$',res)) mtype = 'ardl-firstdiff'

  # LEVELS
# -> 1736 1739  951  952  953  958

#
unique(brand_panel$brand_id)[1:18][grepl('cointegration$',unlist(out))]
# -> 1737 1738 1740 1741 1742  956  957

unique(brand_panel$brand_id)[1:18][grepl('no cointegration$',unlist(out))]
# ->  1741 1742  956  957


m<-ardl(type='ardl-levels', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
       adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6, controls = controls)
summary(m)



dv='lnusales'
m<-ardl(type='ardl-ec', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6, controls = controls)
summary(m)



# first diffs
dv='lnusales'
m<-ardl(type='ardl-firstdiff', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6, controls = controls)
summary(m)



# Extract data set

sim_dat <- function(m, shockvar=NULL, shockperiod=20, Nperiods=30, shockvalue=log(1.1)) {
  
  dv <- m$model$model$model[,1]
  dvname = colnames(m$model$model$model)[1]
  dvblank=gsub('.*[.]|^d','', dvname)
  
  
  # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
  Xsim <- colMeans(m$model$model$model[,-1])
  
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
  

sim_ds = lapply(c('base', vars), function(x) sim_dat(m=m, shockvar=x, shockvalue = log(1.10), Nperiods = 48))
 
View(sim_ds[[2]]$data)
  
Xsim_mat=sim_ds[[1]]$data
  
  
  
my_predict <- function(m, Xsim_mat) {
  
  # Draw coefficients
  coefs =m$model$model$coefficients
  Nperiods=nrow(Xsim_mat)
  predlevels <- rep(NA,Nperiods)
  preddiff <- rep(NA,Nperiods)
  pred <- rep(NA,Nperiods)
  
  dvname = colnames(m$model$model$model)[1]
  dvblank=gsub('.*[.]|^d','', dvname)
  
  Xsim <- colMeans(m$model$model$model[,-1])
  
 # dvmean = Xsim[grep(paste0('^l[.][0-9][.]', dvblank,'$|^', dvblank,'$'), colnames(m$model$model$model[,-1]),value=T)][1]
  
  #if(is.na(dvmean)) dvmean=mean(eval(parse(text=paste0('dt$`',dvblank,'`'))))
  
  meansales <- mean(dt$lnusales)

  indiff=F
  if (grepl('^d[.]|^d', dvname)) indiff=T
    
  for (p in seq(from=1, to=Nperiods)) {
    
    pred[p] = t(coefs)%*%cbind(c(1,Xsim_mat[p,]))
    
    if(indiff==T) {
      # in differences
      if (p==1) {
        predlevels[p] = meansales + pred[p]
        preddiff[p] <- pred[p]
      }
      if (p>1) {
        predlevels[p] = predlevels[p-1]+pred[p]
        preddiff[p] <- pred[p]
      }
      
    } else {
      # in levels
      predlevels[p] = pred[p]
      
    }
    
    # filling lag DVs in levels
    relevant_vars=grep(paste0('l[.][0-9][.]', dvblank, '|^', dvblank), colnames(Xsim_mat),value=T)
    lags = as.numeric(gsub("\\D", "", relevant_vars))
    lags[is.na(lags)] <- 0
    
    for (nextp in seq(from=1, length.out=length(relevant_vars))) if (p+lags[nextp]<=Nperiods) Xsim_mat[p+lags[nextp], relevant_vars[nextp]] <- predlevels[p]
    
    # filling lag DVs in differences
    relevant_vars=grep(paste0('ld[.][0-9][.]', dvblank, '|^', dvblank, '|l[.][0-9][.]d', dvblank), colnames(Xsim_mat),value=T)
    lags = as.numeric(gsub("\\D", "", relevant_vars))
    lags[is.na(lags)] <- 0
    
    for (nextp in seq(from=1, length.out=length(relevant_vars))) if (p+lags[nextp]<=Nperiods) Xsim_mat[p+lags[nextp], relevant_vars[nextp]] <- preddiff[p]
    
  }
  
  return(cbind(ylevels_pred=predlevels, y_pred = pred, Xsim_mat))
  
 # par(mfrow=c(3,1))
  #plot(pred, type='l')
  #plot(predlevels, type='l')
  
  #plot(m$model$model$model[,1], type='l')
  }
  
  
  # Create data sets across various circumstances
  
res=lapply(sim_ds, function(x) my_predict(m, x$data))
  
#View(res[[2]])


  for (i in 2:4){
    varname=unlist(lapply(sim_ds, function(x) x$shockvar))[i]
    shockp=unlist(lapply(sim_ds, function(x) x$shockperiod))[i]
    cat(varname,fill=T)
    plot(res[[i]][,'ylevels_pred'], type='l', ylab = 'log usales', xlab = 'simulation period', main = paste0('Shock in ', varname))
    lines(res[[1]][,'ylevels_pred'], type='l',lty=2)
    outc=res[[i]][,'ylevels_pred']-res[[1]][,'ylevels_pred']
    print(sum(outc[shockp:(shockp+6-1)]))
    print(sum(outc[shockp:(shockp+12-1)]))
    print(sum(outc[shockp:(shockp+24-1)]))
    
    #plot(outc, type='l', main=varname)
  }
  
  plot(dt$dlnusales,type='l')
  plot(dt$lnusales,type='l')
  

  
  
  
  
  
  

colnames(m$model$model$model)



# simulation
dv='dlnusales'
dt=data.table(brand_panel[brand_id==id])

m<-ardl(type='ardl-levels', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6, controls = controls)
m$tested_model_specs

summary(m)


dv='lnusales'
m<-ardl(type="ardl-ec", dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6)#, controls = controls)

summary(m)

dv='lnusales'
m<-ardl(type="ardl-firstdiff", dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1, maxpq=6)#, controls = controls)

summary(m)
m$tested_model_specs

# is this discrepancy because of my ardl function?

# THIS IS A NO-COINTEGRATION MODEL

# diff DV, rest is the same
shockvariable='lnrwpspr'
sims <- lapply(c(0,log(1.1)), function(simvalue) {
               set.seed(1234)
               dynardl(dlnusales ~ 1 + lnrwpspr + lnllen + lnwpswdst + quarter1 + quarter2 + 
                 quarter3, data = dt, lags = list(dlnusales=1:2),
               diffs = NULL, lagdiffs = NULL, ec=F,
               levels= c( "lnrwpspr" , "lnllen"   , "lnwpswdst" ,"quarter1",  "quarter2",  "quarter3" ), 
               trend = F, noLDV=F,
               simulate = T, shockvar=eval(parse(text=paste0('"', shockvariable, '"'))), 
               range=48, time = 20, shockval=simvalue, 
               burnin=12, sig=90)})

simmis = data.frame(do.call('cbind', lapply(sims, function(x) x$simulation$central)))
colnames(simmis) <-c('base','sim')
plot(simmis[,2],type='l',col='red')
lines(simmis[,1],type='l')

# it shows it may grow in general
plot(cumsum(simmis[,2]),type='l',col='red')
lines(cumsum(simmis[,1]),type='l')

simmis$cumsum_base=cumsum(simmis$base)
simmis$cumsum_sim=cumsum(simmis$sim)

simmis$diffcum <- with(simmis, cumsum_sim-cumsum_base)
simmis$diff <- with(simmis, sim-base)

# AUC





summary(test)
# --> replicates model

# now in first-diffs, automatic, w/ EC

test = dynardl(lnusales ~ 1 + lnrwpspr + lnllen + lnwpswdst + quarter1 + quarter2 + 
                 quarter3, data = dt, lags = list(dlnusales=1:2),
               diffs = NULL, lagdiffs = NULL, #list(lnusales=1:2), #NULL,
               levels= c( "lnrwpspr" , "lnllen"   , "lnwpswdst" ,"quarter1",  "quarter2",  "quarter3" ), 
               ec = T, trend = F, noLDV=T)#,
               #simulate = T, shockvar=eval(parse(text=paste0('"', shockvariable, '"'))), 
               #range=48, time = 10, shockval=0,
               #shockvalue, 
               #burnin=12, sig=90)
summary(test)

sim1=test$simulation

baseline=test$simulation



dynardl.simulation.plot(test, type='area', response='diffs')

dynardl.simulation.plot(test, type='area', response='levels') #diffs



# check controls


# simulation
shockvariable = vars[3]
shockvalue = log(1.1)

msim = dynardl(m$tested_model_specs$formula, data = dt, lags = m$tested_model_specs$lagstructure[[m$mchoice]]$lags,
               diffs = m$tested_model_specs$diffs, lagdiffs = m$tested_model_specs$lagstructure[[m$mchoice]]$lagdiff,
               levels= m$tested_model_specs$levels, ec = m$tested_model_specs$ec, trend = m$tested_model_specs$trend,
               noLDV = ifelse(m$type=='ardl-firstdiff',T,F),
               simulate = T, shockvar=eval(parse(text=paste0('"', shockvariable, '"'))), 
               range=48, time = 10, shockval=shockvalue, burnin=12, sig=90)
summary(msim)

dynardl.simulation.plot(msim, type='area', response='diffs')

dynardl.simulation.plot(msim, type='area', response='levels') #diffs

dynardl.simulation.plot(msim, type='area', response='levels') #diffs

msim$simulation[,c('time','central')]

#ardl-firstdiff
#ardl-levels




# NEXT STEPS
# FINAL MODEL




# what about: cannot remove autocorrel
emarkets=unique(brand_panel$brand_id)[which(grepl('cannot remove autocorrel',res))]
o2 = sapply(emarkets, analyze_brand, quarters=T, simplify = F)

bid = 18

unique(brand_panel$brand_id)[which(grepl('cannot remove autocorrel',res))]

m<-analyze_brand(1642, quarters=T)


unique(panel$market_id)[grepl('error', out, ignore.case=T)]






  ### Case: DV is stationary ###
  m=dynardl(lnusales~1+lnrwpspr+lnwpsllen+lnwpswdst, data = dat, levels=c('lnrwpspr'), diffs=c('lnwpsllen', 'lnwpswdst'),
            lags=list('lnusales'=1), simulate=T, shockvar='lnwpsllen', range=100, shockval=log(1.10), burnin=48, sig=90)
  
  dynardl.simulation.plot(m, type='area', response='levels', start.period=9)
  
  
  
# try some simulations
mx<-dynardl(formula, data=dat, lags = lag_list,
            diffs = c("lnrwpspr", 'lnwpsllen', 'lnwpswdst'), 
            lagdiffs = NULL, ec = TRUE, trend = use_trend, simulate = T, shockvar='lnwpsllen',
            range=48, shockval=log(1.10), burnin=48, sig=90)

dynardl.simulation.plot(mx, type='area', response='levels')













for (m in 1:12) panel[,paste0('month',m):=ifelse(month(date)==m, 1,0)]
panel[, monthno:=as.numeric(as.character(as.factor(month(date))))]






#####################################
# Estimation of OLS: category sales #
#####################################

  plevel=.1
  res = NULL
  for (market in analysis_markets) {
      print(market)
    dat=panel[selected==T&market_id==market]
      
      form=dlnusales ~ 1+dlnrwpspr+dlnwpsllen+dlnwpswdst+quarter1+quarter2+quarter3+dlnlagusales+dcop_rwpspr+dcop_wpsllen+dcop_wpswdst+dlngdppercap+lntrend
      
      #if(length(unique(dat$noofbrands))>4) form=update.formula(form, ~ . + dlnnoofbrands)
        # use GDP per cap?
      if (unique(dat$country)=='new zealand') form=update.formula(form, .~.-dlngdppercap)
        
      m<-lm(form, data=dat)
    
    # Check insig. copula terms
      coefs = data.table(variable=names(m$coefficients),summary(m)$coefficients)
      setnames(coefs, c('variable', 'est', 'se', 'z', 'p'))
      nscop=coefs[grepl('dcop[_]', variable)&p>plevel]$variable
      
    if (length(nscop>0)) form=update.formula(form,as.formula(paste0('.~.-',paste0(nscop, collapse='-'))))
    
      
      m<-lm(form, data=dat)
      
    if(0){
     # m<-lm(lnusales ~ 1+lnrwpspr+lnwpsllen+lnwpswdst+quarter1+quarter2+quarter3+lnlag_usales, data=dat)
     m<-lm(lnusales ~ 1+lnrwpspr+lnwpsllen+lnwpswdst+quarter1+quarter2+quarter3+lngdppercap+lnlagusales, data=dat)
    }
      
    
    
      coefs=data.table(summary(m)$coefficients)
      coefs[, market_id:=market]
      coefs[, vifs:=c(NA, vif(m))]
    
      coefs[, variable:=names(m$coefficients)]
      setnames(coefs, c('est', 'se','z', 'p', 'market_id', 'vif', 'variable'))
      coefs[, category:=unique(dat$category)]
      coefs[, country:=unique(dat$country)]
      
    setcolorder(coefs, c('market_id', 'category','country','variable', 'vif'))
    coefs[, rsq:=summary(m)$r.squared]
    
    res[[market]] <- coefs
    
  }
  res=rbindlist(res)
  
  # FIT:
  rsq=res[, list(rsq=unique(rsq)),by=c('market_id')]
  summary(rsq)
  
  # summary
  res[, list(N=.N,mean=mean(est), min=min(est), max= max(est), pos_sig = length(which(est>0&p<=.1))/.N,
      neg_sig = length(which(est<0&p<=.1))/.N,
      max_vif=max(vif), mean_vif=mean(vif)), by= c('variable')]
  
  # VIF problems
  res[vif>50]
  
# expand to elasticities
  elast=res[grep('pspr|llen|wdst', variable)]
  elast=elast[!grep('[pspr|llen|wdst]_]cop', variable)]
  
  weights=weights[market_id%in%elast$market_id]
  
  elast=merge(weights, elast, by = c('market_id'), allow.cartesian=T)
  elast[, elast:=est*weight]
  
  tmp=res[grep('usales', variable), est, by = c('market_id')]
  setkey(tmp, market_id)
  
  # summary stats of lambda
  summary(1+tmp$est)
  
  setkey(elast, market_id)
  
  elast[tmp, lambda:=1+i.est]
  
  elast[, elast_lt:=est*weight/(1-lambda)]
  
  # summary
  elast[!grepl('cop', variable), list(N=.N,mean_st=mean(elast), min_st=min(elast), max_st= max(elast), 
                                      mean_lt=mean(elast_lt), min_lt= min(elast_lt), max_lt=max(elast_lt), pos_sig = length(which(est>0&p<=.1))/.N,
             neg_sig = length(which(est<0&p<=.1))/.N,
             max_vif=max(vif), mean_vif=mean(vif)), by= c('variable')]
  
  
  #### HLM
  # integrate characteristics little earlier
  
  # load chars
  be <- fread('../output/elast_results_main.csv')
  setkey(be, market_id, brand)
  
  characteristics =   brand_panel[, lapply(.SD, mean, na.rm=T), by=c('market_id', 'brand'), .SDcols=c( 'emerging', 'herf', 'market_growth', 'appliance','gini', 'local_to_market', 'gdppercap2010')] #''
  
  
  elast=merge(elast, characteristics, by = c('market_id', 'brand'), all.x=T)
  
  setkey(elast, market_id, brand)
  elast[be, sbbe:=i.sbbe_std]
  elast[, sbbe_mc:=sbbe-mean(sbbe,na.rm=T)]
  
  
  elast=elast[!grepl('cop', variable)]
  
  elast<-elast[!grepl('alloth', brand)]
  
  form=elast~1+emerging+sbbe+I(emerging*sbbe_mc)+log(herf)+log(market_growth)+appliance+log(gini)+local_to_market
  
   spldata=split(elast, elast$variable)
   spldata=spldata[c(1,3,2)]
  models=lapply(spldata, function(x) lm(form, data=x, weights=1/se))
  names(models)=unlist(lapply(spldata, function(x) unique(x$variable)))
  
  library(stargazer)
  
  stargazer(models, type='text', column.labels=names(models))
  
  .
  # VIF problems
 # res[vif>50]
  
  # regress covariates
#  elast=res[grepl('rwpspr|llen|wdst', variable)]
  
  be <- fread('../output/elast_results_main.csv')
  setkey(be, market_id, brand)
  
  characteristics =   brand_panel[, lapply(.SD, mean, na.rm=T), by=c('market_id', 'brand'), .SDcols=c( 'emerging', 'herf', 'market_growth', 'appliance','gini', 'local_to_market', 'gdppercap2010')] #''
  
 # elast=res[grepl('rwpspr|llen|wdst', variable)]
  #elast=res[grepl('dln(rwpspr|llen|wpswdst)', variable)]
  elast=res[grepl('long.*(rwpspr|llen|wpswdst)', variable)]
  
  elast[, include:=T]
  perc=.01
  elast[est<quantile(est, perc/2), include:=F, by = c('variable')]
  elast[est>quantile(est, 1-(perc/2)), include:=F, by = c('variable')]
  
  
  elast[grepl('rwpspr', variable), variable:='rwpspr']
  elast[grepl('llen', variable), variable:='llen']
  elast[grepl('wpswdst',variable), variable:='wpswdst']
  
  elast=merge(elast, characteristics, by = c('market_id', 'brand'), all.x=T)
  
  setkey(elast, market_id, brand)
  elast[be, sbbe:=i.sbbe_std]
  elast[, sbbe_mc:=sbbe-mean(sbbe,na.rm=T)]
  
  setkey(be, market_id, brand, variable)
  setkey(elast, market_id, brand, variable)
  elast[be, oldelast:=i.elast]
  elast[be, oldelast_lt:=i.elastlt]
  
  
  elast<-elast[!grepl('alloth', brand)]
  elast=elast[!is.na(local_to_market)&!tolower(brand)%in%c('unbranded')]
  
  be[, market_brand:=paste0(market_id,'-',brand)]
  elast[, market_brand:=paste0(market_id,'-',brand)]
 
   with(elast, cor(est,oldelast, use='pairwise'))
  with(elast, cor(est,oldelast_lt, use='pairwise'))
  
  elast=elast[include==T]
  
  library(lme4)
  
  form=est~1+(1 | category) + (1 | country) + (1|brand) + I(-log(gdppercap2010))+sbbe+I(-log(gdppercap2010)*sbbe_mc)+log(herf)+log(market_growth)+appliance+log(gini)+local_to_market
  form=est~1+(1 | category) + (1 | country) + (1|brand) + I(emerging)+sbbe+I(emerging*sbbe_mc)+log(herf)+log(market_growth)+appliance+log(gini)+local_to_market
  
  spldata=split(elast, elast$variable)
  spldata=spldata[c(2,3,1)]
  models=lapply(spldata, function(x) lmer(form, data=x, weights=1/se))
  names(models)=unlist(lapply(spldata, function(x) unique(x$variable)))
  
  library(stargazer)
  
  stargazer(models, type='text', column.labels=names(models))
  
  
  
  
  tmp=elast[, list(mean_elast=sum((1/se)*est)/sum(1/se)), by = c('variable', 'emerging')]
  tmp[, ctr:='emerging']
  tmp[emerging==0, ctr:='developed']
  dcast(tmp, variable~ctr, value.var='mean_elast')
  