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
  
  #panel[, nbrandswpsllen:=noofbrands*wpsllen]
  
  vars=c('rwpspr', 'wpsllen', 'wpswdst', 'usales', 'lagusales', 'noofbrands')
  for (var in vars) {
    panel[, anyzero:=as.numeric(any(get(var)==0),na.rm=T),by=c('market_id')]
    panel[is.na(anyzero), anyzero:=0]
    panel[, paste0('ln', var):=log(get(var)+anyzero), by = 'market_id']
    panel[, paste0('dln', var):= get(paste0('ln', var))-c(NA, get(paste0('ln', var))[-.N]), by = c('market_id')]
  }

# Define copula terms
  for (var in c('rwpspr', 'wpsllen', 'wpswdst')) {
    panel[, paste0('cop_', var):=make_copula(get(paste0('ln', var))), by = c('market_id')]
    panel[, paste0('dcop_', var):=get(paste0('cop_', var))-c(NA,get(paste0('cop_', var))[-.N]), by = c('market_id')]
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
  
  
#####################
# Estimation of OLS #
#####################
  plevel=.1
  res = NULL
  for (market in analysis_markets) {
      print(market)
    dat=panel[selected==T&market_id==market]
      
      form=dlnusales ~ 1+dlnrwpspr+dlnwpsllen+dlnwpswdst+quarter1+quarter2+quarter3+dlnlagusales+dcop_rwpspr+dcop_wpsllen+dcop_wpswdst+dlngdppercap
      
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
      m<-lm(lnusales ~ 1+lnrwpspr+lnwpsllen+lnwpswdst+quarter1+quarter2+quarter3+lnlag_usales, data=dat)
     m<-lm(lnusales ~ 1+lnrwpspr+lnwpsllen+lnwpswdst+quarter1+quarter2+quarter3+lngdppercap+lnlag_usales, data=dat)
    }
      
    
    
      coefs=data.table(summary(m)$coefficients)
      coefs[, market_id:=market]
      coefs[, vifs:=c(NA, vif(m))]
    
      coefs[, variable:=names(m$coefficients)]
      setnames(coefs, c('est', 'se','z', 'p', 'market_id', 'vif', 'variable'))
    
    setcolorder(coefs, c('market_id', 'variable', 'vif'))
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
  
 
  