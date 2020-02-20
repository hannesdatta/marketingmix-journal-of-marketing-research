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



out=sapply(unique(brand_panel$brand_id)[1:10], function(bid) try(analyze_brand(bid, quarters=T), silent=T), simplify=F)

# "retrieve" resulting model, reestimate

# simulation
dv='lnusales'
dt=data.table(brand_panel[brand_id==1])
vars = c('lnrwpspr','lnllen','lnwpswdst')
m<-ardl(type='ardl-ec', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1)

shockvariable = vars[1]
shockvalue = log(1.1)

msim = dynardl(m$tested_model_specs$formula, data = dt, lags = m$tested_model_specs$lagstructure[[m$mchoice]]$lags,
        diffs = m$tested_model_specs$diffs, lagdiffs = m$tested_model_specs$lagstructure[[m$mchoice]]$lagdiff,
        levels= m$tested_model_specs$levels, ec = m$tested_model_specs$ec, trend = m$tested_model_specs$trend,
        simulate = T, shockvar="lnrwpspr", range=48, time = 10, shockval=shockvalue, burnin=12, sig=90)

dynardl.simulation.plot(msim, type='area', response='levels') #diffs

#ardl-firstdiff
#ardl-levels




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


#res=clusterApply(cl, unique(panel$market_id), function(mid) try(analyze_market(mid, quarters=T), silent=T))
res=clusterApply(cl, unique(brand_panel$brand_id), function(bid) {
  out=try(analyze_brand(bid, quarters=T), silent=T)
  if (class(out)=='try-error') return('error') else return(out)
})
my_results=unlist(res)


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
  