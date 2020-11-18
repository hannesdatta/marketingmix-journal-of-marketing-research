library(MASS)
library(car)

# Configure model type and calibrate lag structure
simple_loglog <- function(id, withcontrols=T, withattributes=T, 
                   autocorrel_lags= c(3,6,9,12,15,18,1),
                   control_ur = F,
                   return_models = F, return_simulations = F, shockperiods=NA,
                   ndraws=5,
                   kickout_ns_controls = F, pval = .1,
                   with_copulas= F) {
  
  # 1.0 Conduct bounds test
  dv='lnusales'
  dt=data.table(brand_panel[brand_id==id])
  
  brands_in_market <- unique(brand_panel[market_id%in%unique(brand_panel[brand_id==id]$market_id)]$brand)
  
  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  
  dt[, lngdp := log(gdppercapita)]
  dt[, lnholiday := log(npublicholidays+1)]
  
  #if (length(brands_in_market)>2) 
  #if (length(brands_in_market)<=2) control_vars = c('lngdp', 'lnholiday')
  
  control_vars = c('lngdp', 'lnholiday', grep('^comp[_]', colnames(dt),value=T))
  
  if(withcontrols==F) control_vars = NULL
  control_vars = control_vars[unlist(lapply(dt[, control_vars, with=F], use_ts))]
  
  quarter_vars = c('quarter1','quarter2','quarter3')
  
  dt[, lntrend:=log(1:.N)]
  dt[, lntrend:=lntrend-mean(lntrend,na.rm=T)]
  dt[, laglnusales:=c(NA, lnusales[-.N])]
  
  # LOG-LOG
    
    # lm model
    my_form =update.formula(lnusales~1, as.formula(paste0('.~.+1+', paste0(c(vars,quarter_vars, control_vars, 'lntrend', 'laglnusales'), collapse='+'))))
    
    m1 = lm(my_form, data= dt)
    
    identifiers = dt[-m1$na.action,c('market_id', 'category','country', 'brand', 'brand_id' , 'date'),with=F]
    setkey(identifiers, market_id, date, brand)
    
    if (withattributes==T) {
      # Add brand attributes
      retain_attr=unlist(lapply(dt[-m1$na.action, grep('^attr',colnames(dt),value=T),with=F], function(x) !all(is.na(x))))
      attr_vars = names(retain_attr[which(retain_attr==T)])
      attr_vars = attr_vars[unlist(lapply(dt[-m1$na.action, attr_vars, with=F], use_ts))]
      
      if(length(attr_vars)>0) my_form =update.formula(my_form, as.formula(paste0('.~.+', paste0(c(attr_vars), collapse='+'))))
    
      m1 = lm(my_form, data= dt)
      if (any(is.na(m1$coefficients))) {
        kickoutcoef = names(m1$coefficients[is.na(m1$coefficients)])
        my_form =update.formula(my_form, as.formula(paste0('.~.-', paste0(kickoutcoef, collapse='-'))))
        m1 = lm(my_form, data= dt)
        
      }
    }
    
     
    # elasticities
    
    elast1 = rbindlist(lapply(vars, function(v) {
      st=deltaMethod(m1, paste0('(', v, ')'))
      lt=deltaMethod(m1, paste0('(', v, ')/(1-laglnusales)'))
      
      data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
                 elastlt = lt$Estimate, elastlt_se = lt$SE, beta = m1$coefficients[v], carryover = m1$coefficients['laglnusales'])
      
    }))
   
  
  
  # EC
    
    ctrls = NULL #c('lntrend')
    if ('lngdp' %in% control_vars) ctrls=c(ctrls, 'lngdp')
    if ('lnholiday' %in% control_vars) ctrls=c(ctrls, 'lnholiday')
    vars_delta = paste0('d', setdiff(c(vars, control_vars), ctrls))
    vars_lags = c(paste0('lag', setdiff(c(vars, control_vars), ctrls)))
    
    
    for (v in vars_lags) dt[, (v):=c(NA, get(gsub('^lag','',v))[-.N])]
    
    my_form = update.formula(dlnusales~1, as.formula(paste0('.~.+1+', paste0(c(vars_delta, 'laglnusales', paste0('I(-', vars_lags,')'), ctrls), collapse='+'))))
  
  m2 = lm(my_form, data= dt)
  
  identifiers2 = dt[-m2$na.action,c('market_id', 'category','country', 'brand', 'brand_id' , 'date'),with=F]
  setkey(identifiers2, market_id, date, brand)
  
  if (withattributes==T) {
    # Add brand attributes
    retain_attr=unlist(lapply(dt[-m2$na.action, grep('^attr',colnames(dt),value=T),with=F], function(x) !all(is.na(x))))
    attr_vars = names(retain_attr[which(retain_attr==T)])
    attr_vars = attr_vars[unlist(lapply(dt[-m2$na.action, attr_vars, with=F], use_ts))]
    
    if(length(attr_vars)>0) my_form =update.formula(my_form, as.formula(paste0('.~.+', paste0(c(attr_vars), collapse='+'))))
    
    m2 = lm(my_form, data= dt)
    
    if (any(is.na(m2$coefficients))) {
      kickoutcoef = names(m2$coefficients[is.na(m2$coefficients)])
      my_form =update.formula(my_form, as.formula(paste0('.~.-', paste0(kickoutcoef, collapse='-'))))
      m2 = lm(my_form, data= dt)
      
    }
  }
  
  elast2 = rbindlist(lapply(vars, function(v) {
    st=deltaMethod(m2, paste0('(d', v, ')'))
    lt=deltaMethod(m2, paste0('(`I(-lag', v, ')`)/(laglnusales)'))
    
    data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
               elastlt = lt$Estimate, elastlt_se = lt$SE, beta = m2$coefficients[paste0('d',v)], carryover = m2$coefficients['laglnusales'])
    
  }))
  
  
  
 elast1=cbind(identifiers[1],elast1)[,date:=NULL]
 elast2=cbind(identifiers2[1],elast2)[,date:=NULL]
 
 .v=vif(m1)
 vif1=cbind(identifiers[1], data.table(variable=names(.v), vif=.v))[, date:=NULL]
 
 .v=vif(m2)
 vif2=cbind(identifiers2[1], data.table(variable=names(.v), vif=.v))[, date:=NULL]
 
 
return(list(elast_loglog=elast1, elast_ec = elast2, model_loglog=m1, model_ec=m2, vif_loglog=vif1, vif_ec=vif2))
}

