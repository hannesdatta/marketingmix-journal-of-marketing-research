library(MASS)
library(car)

# Configure model type and calibrate lag structure
estimate_salesresponse <- function(id, vars = c('rwpspr','llen','wpswdst'),
                      controls='(^comp[_](rwpspr|llen|wpswdst)$)|quarter[1-3]|^holiday|^trend|(^cop[_](rwpspr|llen|wpswdst)$)', 
                      pval = .1,
                      kickout_ns_copula = T, withlagdv=T, dv = 'usales') {
  
  
  dt=data.table(brand_panel[brand_id==id])
  setorder(dt, category,country,brand,date)
  dt[, ldv := c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  dt[, ddv := get(dv)-c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  
  brands_in_market <- unique(brand_panel[market_id%in%unique(brand_panel[brand_id==id]$market_id)]$brand)
  
  print(vars)
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  print(vars)
  
  control_vars = unlist(sapply(controls, function(ctrls) if (nchar(ctrls)>0) grep(ctrls, colnames(dt),value=T)))
  control_vars = unlist(lapply(control_vars, function(control_var) control_var[unlist(lapply(dt[, control_var, with=F], use_ts))]))
  print(control_vars)
  
  dt[, lntrend:=lntrend-mean(lntrend,na.rm=T)]
  dt[, trend:=trend-mean(trend,na.rm=T)]
  
  my_form = update.formula(as.formula(paste0(dv,'~1')), as.formula(paste0('.~.+1+', paste0(c(vars, switch(withlagdv, 'ldv', NULL), control_vars), collapse='+'))))
  
  dt[, estim_set:=T]
  
  dt[, percentile_obs:=(1:.N)/.N]
  
  m = lm(my_form, data= dt, subset = estim_set==T)
  
  # kickout coefs with NA values (e.g., attributes that are not identified)
  kickoutcoef = NULL
  if (any(is.na(m$coefficients))) kickoutcoef = c(kickoutcoef, names(m$coefficients[is.na(m$coefficients)]))
  
  # kickout ns. copulas
  vars_cop=grep('^cop[_]', control_vars, value=T)
  
  if (!is.null(vars_cop) & kickout_ns_copula == T) {
    tmpres = data.table(variable=rownames(summary(m)$coefficients), summary(m)$coefficients)[grepl('^cop[_]', variable)]
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
    res=data.table(dt[combobs, c('date', dv, 'ddv','ldv'),with=F], kfold=iter,dv_hat=predict(m3, newdata=dt[combobs,]))
    res[, estim_set:=combobs%in%obs]
    res[, df.residual := m3$df.residual]
    res[, residual.se := summary(m3)$sigma]
    res[, SSR:=sum(m3$residuals^2)]
    
    return(res)
    
  }))
  
  pred_new3 = merge(dt[, c('category','country','brand','date'),with=F],
                    preds_new,
                    by=c('date'),all.x=F)
  
  
  predictions = cbind(dt[, c('category','country','brand','date', 'estim_set', dv),with=F],
                      dv_hat=predict(m2, newdata=dt))
  predictions[, df.residual := m2$df.residual]
  predictions[, residual.se := summary(m2)$sigma]
  predictions[, SSR:=sum(m2$residuals^2)]
  
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
      st=deltaMethod(m2, paste0(multipliers_means[v],'*(', v, ')'))
      st_median=deltaMethod(m2, paste0(multipliers_medians[v],'*(', v, ')'))
      
      if (v%in% names(m2$coefficients)) {
        lt=try(deltaMethod(m2, paste0(multipliers_means[v],'*((', v, ')/(1-ldv))')), silent=T)
        lt_median=try(deltaMethod(m2, paste0(multipliers_medians[v],'*((', v, ')/(1-ldv))')), silent=T)
        
        if ('try-error'%in%class(lt)) lt = deltaMethod(m2, paste0(multipliers_means[v],'*((', v, ')/(1-0))'))
        if ('try-error'%in%class(lt_median)) lt_median = deltaMethod(m2, paste0(multipliers_medians[v],'*((', v, ')/(1-0))'))
      }
      
      
      if ('ldv'%in%names(m2$coefficients)) lnlagcoef = m2$coefficients['ldv'] else lnlagcoef = NA
      
      data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
                 elastlt = lt$Estimate, elastlt_se = lt$SE, 
                 
                 elastmedian = st_median$Estimate, elastmedian_se = st_median$SE,
                 elastmedianlt = lt_median$Estimate, elastmedianlt_se = lt_median$SE,
                 
                 beta = m2$coefficients[v], 
                 carryover = lnlagcoef)
      
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
              predictions=predictions, predictions_kfold = pred_new3, r2_within_dv= summary(m2)$r.squared))
  
}

# Configure model type and calibrate lag structure
estimate_ec <- function(id, vars = c('rwpspr','llen','wpswdst'),
                          controls_diffs='^comp[_](rwpspr|llen|wpswdst)$', 
                          controls_laglevels = '',
                          controls_curr = 'quarter[1-3]|^holiday|^trend',
                          controls_cop = '^cop[_](rwpspr|llen|wpswdst)$', 
                          pval = .1,
                          kickout_ns_copula = T, dv = 'usales') {
  
  dt=data.table(brand_panel[brand_id==id])
  setorder(dt, category,country,brand,date)
  dt[, ldv := c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  dt[, ddv := get(dv)-c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  
  brands_in_market <- unique(brand_panel[market_id%in%unique(brand_panel[brand_id==id]$market_id)]$brand)
  
  
  print(vars)
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  print(vars)
  
  control_vars = sapply(c(diffs=controls_diffs,lags=controls_laglevels,curr=controls_curr), function(ctrls) if (nchar(ctrls)>0) grep(ctrls, colnames(dt),value=T))
  control_vars = lapply(control_vars, function(control_var) control_var[unlist(lapply(dt[, control_var, with=F], use_ts))])
  print(control_vars)
  
  dt[, lntrend:=lntrend-mean(lntrend,na.rm=T)]
  dt[, trend:=trend-mean(trend,na.rm=T)]
 
  
  vars_delta = paste0('d', c(vars, control_vars$diffs))
  vars_lags = paste0('lag', c(vars, control_vars$lags))
  vars_curr = control_vars$curr
  
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
    res=data.table(dt[combobs, c('date', 'ddv','ldv',dv),with=F], kfold=iter,
                   ddv_hat=predict(m3, newdata=dt[combobs,]))
    res[, estim_set:=combobs%in%obs]
    
    res[, df.residual := m3$df.residual]
    res[, residual.se := summary(m3)$sigma]
    
    #plot(res[newpset]$ddv, res[newpset]$ddv_hat, type='p')
    #plot(res$usales,type='l')
    #lines(res$ldv+res$ddv_hat,type='l',lty=2)
    
    #plot(res$ddv_hat,type='l')
    #lines(res$ddv,type='l',lty=2)
    
    return(res)
    
  }))

  pred_new3 = merge(dt[, c('category','country','brand','date'),with=F],
                    preds_new,
                    by=c('date'),all.x=F)
  
 
  # within-predictions   
  predictions = cbind(dt[, c('category','country','brand','date', 'estim_set', dv, 'ldv','ddv'),with=F],
                      ddv_hat=predict(m2, newdata=dt))
  predictions[, paste0(dv,'_hat') := ldv + ddv_hat]
  
  predictions[, df.residual := m2$df.residual]
  predictions[, residual.se := summary(m2)$sigma]
  
  
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


process_sur <- function(mod) {
  vars=as.character(mod$elast$variable)
  
  vcov = mod$sur$varcovar
  
  coefs=mod$sur$coefs$coef
  names(coefs)<-mod$sur$coefs$variable
  names(coefs) <- gsub('[.]', '', names(coefs))
  colnames(vcov) <- names(coefs)
  rownames(vcov) <- names(coefs)
  
  dv = colnames(mod$predictions)
  dv = dv[which(dv=='estim_set')+1]
  
  means = unlist(mod$dt[, lapply(.SD, mean,na.rm=T), .SDcols=c(dv,vars)])
  medians = unlist(mod$dt[, lapply(.SD, median,na.rm=T), .SDcols=c(dv,vars)])
  multipliers_means <- rep(1, length(vars))
  if (!grepl('^ln|^log', dv)) multipliers_means <- sapply(vars, function(.v) means[.v]/means[dv])
  names(multipliers_means)<-vars
  multipliers_medians <- rep(1, length(vars))
  if (!grepl('^ln|^log', dv)) multipliers_medians <- sapply(vars, function(.v) medians[.v]/medians[dv])
  names(multipliers_medians)<-vars
  
  
  
  
  elast_sur = rbindlist(lapply(vars, function(v) {
    dvar= grep(paste0('d',v), names(coefs),value=T)
    lvar= grep(paste0('lag',v), names(coefs),value=T)
    lagu= grep(paste0('ld'), names(coefs),value=T)
    
    st=deltaMethod(coefs, paste0(multipliers_means[v],'*(', dvar, ')'),  vcov.=vcov)
    lt=deltaMethod(coefs, paste0(multipliers_means[v],'*((`', lvar, '`)/(', lagu, '))'),  vcov.=vcov)
    
    stmed=deltaMethod(coefs, paste0(multipliers_medians[v],'*(', dvar, ')'),  vcov.=vcov)
    ltmed=deltaMethod(coefs, paste0(multipliers_medians[v],'*((`', lvar, '`)/(', lagu, '))'),  vcov.=vcov)
    
    data.frame(variable=v, elast = st$Estimate, elast_se=st$SE,
               elastlt = lt$Estimate, elastlt_se = lt$SE, 
               elastmedian = stmed$Estimate, elastmedian_se = stmed$SE,
               elastmedianlt = ltmed$Estimate, elastmedianlt_se = ltmed$SE,
               beta = coefs[dvar], 
               carryover = coefs[lagu])
    
  }))
  
  mod$elast_sur = elast_sur
  return(mod)
}


# Estimate models jointly
model_sur <- function(focal_models, maxiter = 5000) {
  
  single_eqs <- lapply(seq(along=focal_models), function(x) {
    tmp = focal_models[[x]]$model_matrix
    #add intercept
    newtmp = cbind(tmp[,1], 1, tmp[,-1])
    names(newtmp)[2]<-'(Intercept)'
    brid=paste0('bid', gsub('[ ]', '0', prettyNum(x,width=3)), '_')
    names(newtmp) <- paste0(brid, names(newtmp))
    names(newtmp) <- c('y', names(newtmp)[-1])
    
    newret = as.matrix(newtmp)
    
    #colnames(newtmp) <- names(newtmp)
    return(data.frame(newret))
  })
  #colnames(single_eqs[[1]])
  #single_eqs[[1]][1:2,]
  
  #if (length(single_eqs)==2) {
  #  single_eqs = lapply(single_eqs, function(x) {
  #  cols <- grep('comp[_]', colnames(x),value=T)
  #  if (length(cols)>0) return(x[, -match(cols,colnames(x))])
  #  return(x)
  #})}
  
  stacked_eq = rbindlist(single_eqs,fill=T)
  
  
  index_list = rbindlist(lapply(seq(along=focal_models), function(x) {
    focal_models[[x]]$paneldimension 
  }))
  index_list[, ordered:=1:.N]
  setorder(index_list, date, market_id, brand)
  index_list[, period:=.GRP,by=c('date')]
  setorder(index_list, ordered)
  
  
  for (resetvar in colnames(stacked_eq)) {
    vals = stacked_eq[,resetvar, with=F]
    stacked_eq[, (resetvar) := ifelse(is.na(get(resetvar)), 0, get(resetvar))]
  }
  
  # separate DVs from IVs for estimation
  X = stacked_eq[,!colnames(stacked_eq)%in%'y',with=F]
  Y = data.frame(stacked_eq[,'y',with=F])
  
  index=data.frame(date=index_list$period,brand=index_list$brand)
  
  #dcast(index, date~brand)
  if(0) {
    # remove rows where only 1 brand is observed
    nbrands_eval <- aggregate(index$brand, by=list(index$date),FUN = length)
    # first and last stretch of at least two brands
    nbrands_eval$larger_two <- nbrands_eval$x>=4
    
    complete_eqs <- nbrands_eval$`Group.1`[which(nbrands_eval$larger_two==T)]
    
    # kick out incomplete ('diffed') vars, i.e., their first observation (as it is NA) by brand
    X=as.matrix(X[complete_eqs,])
    Y=as.matrix(Y[complete_eqs,])
    index=data.frame(index[complete_eqs,])
  }
  
  #################
  # Estimate SURs #
  #################
  
  # rescale X matrix (normalize to 1?) - strongly recommended for these models to help conversion
  rescale = TRUE
  
  # save ranges before rescaling
  #res$ranges=list(before=data.table(original_variable=colnames(X), min = colMins(X), max=colMaxs(X))[!grepl('[_]sq|[_]cop[_]|[_]dum', original_variable)])
  
  if (rescale==T) rescale_values = apply(X, 2, function(x) max(abs(x)))
  if (rescale==F) rescale_values = apply(X, 2, function(x) 1)
  
  div_matrix <- matrix(rep(rescale_values, nrow(X)), byrow=TRUE, ncol=length(rescale_values))
  X=X/div_matrix
  
  estmethod = "FGLS"
  #maxiter=5000
  
  # remove var from itersur
  #tmp=sapply(1:ncol(X), function(rem) try(itersur(X=as.matrix(X[,-rem]),Y=as.matrix(Y),index=index, method = estmethod, maxiter=maxiter),silent=T))
  
  #unlist(lapply(tmp,class))
  
  
  #colnames(X)[which(unlist(lapply(tmp,class))=='character')]
  
 # !all(order(index[, 2], index[, 1]) == 1:nrow(X))
  
  
  m<-itersur(X=as.matrix(X),Y=as.matrix(Y),index=index, method = estmethod, maxiter=maxiter) #, use_ginv=F
  if (m@iterations==maxiter&maxiter>1) stop("Reached max. number of iterations with SUR. Likely did not converge.")
  
  # compare parameter estimates
  if (rescale == T) {
    retr_coefs <- coef(m)$coef
    mvarcovar=m@varcovar
    
    rescale_after = rescale_values #[keep_vars]
    retr_coefs[seq(length.out=length(rescale_after))] = retr_coefs[seq(length.out=length(rescale_after))] / rescale_after
    
    for (ch in seq(length.out=length(rescale_after))) {
      mvarcovar[ch,] <- mvarcovar[ch,] / rescale_after[ch]
      mvarcovar[,ch] <- mvarcovar[,ch] / rescale_after[ch]
    }
    
    m@coefficients[,2:3] <- cbind(retr_coefs, sqrt(diag(mvarcovar)))
    m@varcovar <- mvarcovar
  }
  
  m@coefficients$type = gsub('[_].*', '', m@coefficients$variable)
  m@coefficients$varname = gsub('bid[0-9]{1,3}[_]', '', m@coefficients$variable)
  m@coefficients$coef_index = 1:nrow(m@coefficients)
  
  coefs = lapply(split(m@coefficients$coef_index, m@coefficients$type), function(x) {
    m@coefficients[x,]
  })
  
  
  varcovar = lapply(split(m@coefficients$coef_index, m@coefficients$type), function(x) {
    m@varcovar[x,x]
  })
  
  return(list(coefs=coefs, varcovar=varcovar))
  
}

