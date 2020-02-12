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


plot(panel[market_id==33]$lnusales, type='l')
plot(diff(panel[market_id==33]$lnusales), type='l')

# checking order of UR for brand sales
vars = c('lnusales','lnrwpspr','lnllen','lnwpswdst')
tmp2=brand_panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlags=6, pval=.1)$order),by=c('market_id', 'brand'), .SDcols=vars]
tmp2[lnusales>1] 
any(tmp2[,-1,with=F]>1)
# looks good, too!

brand_panel[market_id==1]$lnusales

colMeans(tmp2)





  
##########################
# DYNAMAC ARDL PROCEDURE #
##########################

mid = 1

source('proc_analysis_agg.R')
source('proc_analysis.R')
source('proc_analysis_brand.R')


out=sapply(unique(panel$market_id)[1:20], function(mid) try(analyze_market(mid, quarters=T), silent=T))

brand_panel[, brand_id:=.GRP, by = c('market_id', 'brand')]

out=sapply(unique(brand_panel$brand_id)[1:20], function(bid) try(analyze_brand(bid, quarters=T), silent=T))



require(parallel)
cl<-makePSOCKcluster(4)
clusterExport(cl, c('panel'))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(timeSeries))
clusterEvalQ(cl, source('proc_analysis_agg.R'))
clusterEvalQ(cl, source('proc_analysis.R'))
clusterEvalQ(cl, source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R'))

unique(panel$market_id)[grepl('now: inconclusive', unlist(res))]
unique(panel$market_id)[grepl('cannot remove', unlist(res))]

res=clusterApply(cl, unique(panel$market_id), function(mid) try(analyze_market(mid, quarters=T), silent=T))




unique(panel$market_id)[grepl('error', out, ignore.case=T)]






### Case: DV is stationary ###
m=dynardl(lnusales~1+lnrwpspr+lnwpsllen+lnwpswdst, data = dat, levels=c('lnrwpspr'), diffs=c('lnwpsllen', 'lnwpswdst'),
          lags=list('lnusales'=1))

autocorrel_test=dynardl.auto.correlated(m, object.out=T)

# no autocorrelation
  
  difflist = lapply(sapply(1:12, function(x) 1:x), function(x) {
    obj=lapply(c(vars), function(...) x)
    names(obj) = c(vars)
    return(obj)
  })
  
  difflist=c(list(NULL), difflist)
  
  models=lapply(difflist, function(lagdiff) {
    mx<-dynardl(formula, data=dat, levels = vars,
                lags=list('dlnusales'=1),
                lagdiffs=lagdiff, ec = FALSE, simulate = FALSE, trend = use_trend)
    list(bic=BIC(mx$model), model=mx)
  })
  
  # lowest BIC
  bics=unlist(lapply(models, function(x) x$bic))
  
  m=models[[which(bics==min(bics))]]$model

  
  ### Case: DV is stationary ###
  m=dynardl(lnusales~1+lnrwpspr+lnwpsllen+lnwpswdst, data = dat, levels=c('lnrwpspr'), diffs=c('lnwpsllen', 'lnwpswdst'),
            lags=list('lnusales'=1), simulate=T, shockvar='lnwpsllen', range=100, shockval=log(1.10), burnin=48, sig=90)
  
  dynardl.simulation.plot(m, type='area', response='levels', start.period=9)
  

# to do    
# "latent" structural break tests
# add seasonal components to ADS (currently NA)
# add quarterly dummies


# all w/ unit root


# try some simulations
mx<-dynardl(formula, data=dat, lags = lag_list,
            diffs = c("lnrwpspr", 'lnwpsllen', 'lnwpswdst'), 
            lagdiffs = NULL, ec = TRUE, simulate = T, trend = use_trend, shockvar='lnwpsllen',
            range=48, shockval=log(1.10), burnin=48, sig=90)

dynardl.simulation.plot(mx, type='area', response='levels')


# Bounds test of cointegration
boundstest=pssbounds(m, object.out = T)
boundstest_result = 'inconclusive'
if (boundstest$fstat < boundstest$`ftest.I0.p10`) boundstest_result='no cointegration'
if (boundstest$fstat > boundstest$`ftest.I1.p10`) boundstest_result='cointegration'

if (boundstest_result=='no cointegration') {
  
  
  formula=as.formula(paste0('d', dv,'~1+', paste(vars, collapse='+')))
  
  
  
  difflist = lapply(sapply(1:12, function(x) 1:x), function(x) {
    obj=lapply(c(vars), function(...) x)
    names(obj) = c(vars)
    return(obj)
  })
  
  difflist=c(list(NULL), difflist)
  
  models=lapply(difflist, function(lagdiff) {
    mx<-dynardl(formula, data=dat, levels = vars,
                lags=list('dlnusales'=1),
                lagdiffs=lagdiff, ec = FALSE, simulate = FALSE, trend = use_trend)
    list(bic=BIC(mx$model), model=mx)
  })
  
  # lowest BIC
  bics=unlist(lapply(models, function(x) x$bic))
  
  m=models[[which(bics==min(bics))]]$model
  
  autocorrelation=autocorrel_test$bg$p.value<.1
  print(paste0('g) Is there autocorrelation in the residuals? ', ifelse(autocorrelation==T, 'yes', 'no')))
  
  #m<-dynardl(dlnusales~1+lnrwpspr, data=dat, levels = c('lnrwpspr'),
  #           lags=list('dlnusales'=1),
  #           #lagdiffs=list('lnusales'=1), #, 'lnrwpspr'=1)
  #           ec = FALSE, simulate = FALSE, trend = use_trend)
  #dynardl.auto.correlated(m)
  
  # equivalent
  #summary(lm(dlnusales~1+lnrwpspr+I(lshift(dlnusales,1)),data=dat))
  
          
}


# how to simulate + interpret! --discuss



# breusch
pssbounds(m)

m<-dynardl(lnusales~1+lnrwpspr, data=dat, lags = list("lnusales"=1, "lnrwpspr" = 1),
           diffs = c("lnrwpspr"), 
           lagdiffs = list('lnusales'=1, 'lnrwpspr' = 1), ec = FALSE, simulate = FALSE, trend = FALSE)

#########################################################
# Testing time series properties of category-level data #
#########################################################
  
vars = c('lnusales','lnrwpspr','lnwpsllen','lnwpswdst')

# checking order of UR
tmp4=panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlag=6, pval=.1)$order),by=c('market_id'), .SDcols=vars]
tmp4[lnusales>1] # several cases!

plot(panel[market_id==33]$lnusales, type='l')
plot(diff(panel[market_id==33]$lnusales), type='l')




# checking order of UR
vars = c('lnusales','lnrwpspr','lnllen','lnwpswdst')
tmp3=brand_panel[, lapply(.SD, function(x) marketingtools::adf_enders(x, season=NULL, maxlag=6, pval = .1)['order']),by=c('market_id'), .SDcols=vars]
tmp3[lnusales>1] # several cases!
colMeans(tmp3)




for (m in 1:12) panel[,paste0('month',m):=ifelse(month(date)==m, 1,0)]
panel[, monthno:=as.numeric(as.character(as.factor(month(date))))]


dt=panel[market_id==9]

summary(CADFtest(lnusales~1+month1+month2+month3+month4+month5+month6+month7+month8+month9+month10+month11,data=dt, type='trend', criterion='BIC', max.lag.y=11))

summary(CADFtest(lnusales~1+as.factor(monthno),data=dt, type='trend', criterion='AIC', max.lag.y=13))






#I(2) in brand_level data?
vars = c('lnusales') #,'lnrwpspr','lnllen','lnwpswdst')
tmp2=brand_panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlag=6, pval=.1)['order']),by=c('market_id', 'brand'), .SDcols=vars]
tmp2[lnusales>1] # several cases!

plot(brand_panel[market_id==9&brand=='hp']$lnusales, type='l')
plot(diff(brand_panel[market_id==9&brand=='hp']$lnusales), type='l')


dt=brand_panel[market_id==9&brand=='apple']

dt[, trend:=1:.N]


+ldshift(lnusales,2)+
  ldshift(lnusales,3)+ldshift(lnusales,4)+ldshift(lnusales,5)+ldshift(lnusales,6)+ldshift(lnusales,7)+
  ldshift(lnusales,8)+ldshift(lnusales,9)+ldshift(lnusales,10)+ldshift(lnusales,11)+ldshift(lnusales,12)


dt=panel[market_id==9]

adf <- function(lags, lagy=T, trend=T, cons=T, dv='lnusales', maxlag=lags) {
  lagterms=''
  if (lags>0) lagterms=paste0('+', paste(paste0('ldshift(',dv,',', 1:lags,')'), collapse='+'))
                              
  form=as.formula(paste0('I(dshift(', dv, '))~', ifelse(cons==T, '1', '-1'), ifelse(lagy==T,paste0('+lshift(', dv, ',1)'),''),ifelse(trend==T,'+(trend)',''), lagterms))
  subs = with(dt, !is.na(ldshift(get(dv), maxlags)))
  
  m<-lm(form, dt, subset=subs)
  list(model=m, bic=BIC(m))
}

adf_optim <- function(lags, lagy=T, trend=T, cons=T, dv = 'lnusales') {
  
  models=lapply(lags, function(lag) adf(lag, lagy=lagy, trend=trend, cons=cons, dv = dv, maxlag=max(lags)))
  bics=unlist(lapply(models, function(x) x$bic))
  return(models[[which(bics==min(bics))]])
}
  
adf(0)

m=adf_optim(0:6, lagy=T, trend=T)$model
summary(m)



linearHypothesis(m, c('lshift(lnusales, 1) = 0', 'trend = 0'))


adf_enders_all(dt$lnusales)





summary(m)
# is gamma  = 0?
m@teststat[1]>m@cval[1,3]



# no unit root
summary(adf_optim(0:6, lagy=F, trend=F)$model)


summary(adf_optim(0:6, lagy=F, trend=T)$model)
summary(adf_optim(0:6, lagy=T, trend=F)$model)
summary(adf_optim(0:6, lagy=F, trend=F)$model)

summary(adf_optim(0:6, lagy=T, trend=F, cons=F)$model)


summary(adf_optim(0:6, lagy=T, trend=F)$model)

summary(adf_optim(0:6, lagy=F, trend=F)$model)

dt[, lagdv:=dshift(lnusales)]

summary(adf_optim(0:6, lagy=T, trend=T, dv = 'lagdv')$model)

summary(adf_optim(0:6, lagy=F, trend=T, dv = 'lagdv')$model)
summary(adf_optim(0:6, lagy=T, trend=F, dv = 'lagdv')$model)
summary(adf_optim(0:6, lagy=F, trend=F, dv = 'lagdv')$model)


bics=lapply(0:6, adf)
unlist(lapply(bics, function(x) x$bic))

summary(adf(4, lagy=F)$model)

summary(adf(4, lagy=T, trend=F)$model)

summary(adf(4, lagy=F, trend=F)$model)

adf_enders(dt$lnusales, season=NULL,maxlag=12,pval=.1)

adf_enders_single(dt$lnusales, season=NULL, maxlag=12, pval=.1)

adf.test(dt$lnusales, season=NULL, maxlag=12, pval=.1)


vars = c('lnusales','lnrwpspr','lnwpsllen','lnwpswdst')
# null: stationarity
# alternative: rejection of stationarity --> non-stationarity / evolving
tmp=panel[, lapply(.SD, function(x) adf_enders(x[!is.na(x)], maxlag=12, pval=.1, season=NULL)['ur']),by=c('market_id'), .SDcols=vars]
tmp[, sum_mmix:=lnrwpspr+lnwpsllen+lnwpswdst]
out=tmp[, list(.N), by=c('lnusales', 'sum_mmix')]
setorder(out, lnusales, sum_mmix)

adf_enders(1:100+rnorm(100),season=NULL,maxlag=12, pval=.1)

adf_enders(1:1000+rnorm(1000),season=NULL,maxlag=12, pval=.1)

adf.test(1:1000+rnorm(1000))
adf.test(rep(1,1000)+rnorm(1000))

with(panel[market_id==1], ca.jo(data.frame(lnusales, lnrwpspr),type="trace", K=2, ecdet="none", spec="longrun"))

# could do panel unit root tests instead
library(plm)

.var=vars[1]
ur_data = dcast(panel,date~market_id, value.var=.var)
ur_data$date = NULL
ur_data = ur_data[, !all(is.na(ur_data))]

# Hadri: purtest(ur_data, test='hadri', exo='intercept')
purtest(ur_data, test='hadri', exo='trend')

# Hadri: purtest(ur_data, test='hadri', exo='intercept')
purtest(ur_data, test='ips', exo='trend')

#####################
# Estimation of OLS #
#####################
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
  
  
  
  LLCdemean = purtest(newdat, test='levinlin',exo='trend',lags='AIC', pmax=12)
  
  
   Fok, JMR