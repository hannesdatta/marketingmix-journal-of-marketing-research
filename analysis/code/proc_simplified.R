library(MASS)
library(car)

# Configure model type and calibrate lag structure
simple_loglog <- function(id, controls='quarter[1-3]|^attr[_]|lngdp|lnholiday|^comp[_]|^trend') {
  
  # 1.0 Conduct bounds test
  dv='lnusales'
  dt=data.table(brand_panel[brand_id==id])
  
  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  
  #brands_in_market <- unique(brand_panel[market_id%in%unique(brand_panel[brand_id==id]$market_id)]$brand)
  
  #if (length(brands_in_market)>2) 
  #if (length(brands_in_market)<=2) control_vars = c('lngdp', 'lnholiday')
  
  
  control_vars = grep(controls, colnames(dt),value=T)
  
  if(length(control_vars)==0|controls=='') control_vars = NULL
  
  control_vars = control_vars[unlist(lapply(dt[, control_vars, with=F], use_ts))]
  
  #dt[, lntrend:=log(1:.N)]
  dt[, lntrend:=lntrend-mean(lntrend,na.rm=T)]
  dt[, trend:=trend-mean(trend,na.rm=T)]
  
  # LOG-LOG
    
    # lm model
    my_form =update.formula(lnusales~1, as.formula(paste0('.~.+1+', paste0(c(vars, control_vars, 'lnlagusales'), collapse='+'))))
    
    m1 = lm(my_form, data= dt)
    
    identifiers = unique(dt[,c('market_id', 'category','country', 'brand', 'brand_id' ),with=F], by=c('brand_id'))
    setkey(identifiers, market_id, brand)
    
    if (any(is.na(m1$coefficients))) {
      kickoutcoef = names(m1$coefficients[is.na(m1$coefficients)])
      my_form =update.formula(my_form, as.formula(paste0('.~.-', paste0(kickoutcoef, collapse='-'))))
      m1 = lm(my_form, data= dt)
    }
      
    if (0) {#(any(grepl('^attr[_]', control_vars))) {
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
      lt=deltaMethod(m1, paste0('(', v, ')/(1-lnlagusales)'))
      
      data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
                 elastlt = lt$Estimate, elastlt_se = lt$SE, beta = m1$coefficients[v], carryover = m1$coefficients['lnlagusales'])
      
    }))
   
 elast1=cbind(identifiers[1],elast1)
 
 .v=vif(m1)
 vif1=cbind(identifiers[1], data.table(variable=names(.v), vif=.v))
 

 
return(list(elast=elast1, model=m1, vif=vif1))
}



# Configure model type and calibrate lag structure
simple_ec <- function(id, controls_diffs='^comp[_]', #
                          controls_laglevels = '',
                          controls_curr = 'quarter[1-3]|lnholiday',
                          controls_cop = '^cop[_]d[.]1[.]', pval = .1) {
  
  # 1.0 Conduct bounds test
  dv='lnusales'
  dt=data.table(brand_panel[brand_id==id])
  
  brands_in_market <- unique(brand_panel[market_id%in%unique(brand_panel[brand_id==id]$market_id)]$brand)
  
  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  
  control_vars = sapply(c(diffs=controls_diffs,lags=controls_laglevels,curr=controls_curr), function(ctrls) if (nchar(ctrls)>0) grep(ctrls, colnames(dt),value=T))
  control_vars = lapply(control_vars, function(control_var) control_var[unlist(lapply(dt[, control_var, with=F], use_ts))])
  
  dt[, lntrend:=lntrend-mean(lntrend,na.rm=T)]
  dt[, trend:=trend-mean(trend,na.rm=T)]
 
  
  vars_delta = paste0('d', c(vars, control_vars$diffs))
  vars_lags = paste0('lag', c(vars, control_vars$lags))
  vars_curr = control_vars$curr

  if (!is.null(controls_cop)) {
    vars_cop = grep(controls_cop, colnames(dt), value=T)
    vars_cop = vars_cop[unlist(lapply(dt[, vars_cop, with=F], use_ts))]
  } else {
    vars_cop = NULL
  }
  
  for (v in vars_lags) dt[, (v):=c(NA, get(gsub('^lag','',v))[-.N])]
  
  my_form = update.formula(dlnusales~1, as.formula(paste0('.~.+1+', paste0(c(vars_delta, vars_cop, 'lnlagusales', paste0('I(-', vars_lags,')'), vars_curr), collapse='+'))))
  
  m = lm(my_form, data= dt)
  
  identifiers = unique(dt[,c('market_id', 'category','country', 'brand', 'brand_id' ),with=F], by=c('brand_id'))
  setkey(identifiers, market_id, brand)
  
  # kickout coefs with NA values (e.g., attributes that are not identified)
  kickoutcoef = NULL
  if (any(is.na(m$coefficients))) kickoutcoef = c(kickoutcoef, names(m$coefficients[is.na(m$coefficients)]))
  
  # kickout ns. copulas
  tmpres = data.table(variable=names(m$coefficients), summary(m)$coefficients)[grepl(controls_cop, variable)]
  setnames(tmpres, c('variable','est','se','t','p'))
  tmpres = tmpres[p>pval]
  
  if (nrow(tmpres)>0) kickoutcoef = c(kickoutcoef, tmpres$variable)
  
  if (length(kickoutcoef>0)) {
    my_form =update.formula(my_form, as.formula(paste0('.~.-', paste0(kickoutcoef, collapse='-'))))
    m2 = lm(my_form, data= dt)
  } else {m2=m}
  
  elast2 = rbindlist(lapply(vars, function(v) {
    st=deltaMethod(m2, paste0('(d', v, ')'))
    lt=deltaMethod(m2, paste0('(`I(-lag', v, ')`)/(lnlagusales)'))
    
    data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
               elastlt = lt$Estimate, elastlt_se = lt$SE, beta = m2$coefficients[paste0('d',v)], 
               carryover = m2$coefficients['lnlagusales'],
               orig_results = m$coefficients, kicked_out_coefs = kickoutcoef)
    
  }))
  
  elast2=cbind(identifiers[1],elast2)
  .v=vif(m2)
  vif2=cbind(identifiers[1], data.table(variable=names(.v), vif=.v))
  
  
  return(list(elast=elast2, model=m2, vif=vif2))
  
}


