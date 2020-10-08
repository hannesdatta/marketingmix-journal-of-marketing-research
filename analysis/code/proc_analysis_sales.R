library(MASS)

# Configure model type and calibrate lag structure
model_configure <- function(id, withcontrols=T, controls_in_bounds = T, withattributes=T, 
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
  
  control_vars = c('lngdp', 'lnholiday', grep('comp[_]', colnames(dt),value=T))
  
  if(withcontrols==F) control_vars = NULL
  control_vars = control_vars[unlist(lapply(dt[, control_vars, with=F], use_ts))]
  
  quarter_vars = c('quarter1','quarter2','quarter3')
  
  if (controls_in_bounds==T) {
    controls_ardl = control_vars
  } else {
  
    controls_ardl = NULL}
  
  m_ardlbounds <- analyze_ardlbounds(id, quarters=T, xs = vars, controls = controls_ardl, dat = dt,
                                autocorrel_lags=autocorrel_lags, pval = pval)
  
  cat(paste0('\n\nOutput of the bounds procedure: ', m_ardlbounds))
  
  mtype = as.character(NA)
  if(grepl('stationarity in DV',gsub(' [(].*', '', m_ardlbounds))) mtype = 'ardl-levels'
  if(grepl('cointegration$', gsub(' [(].*', '', m_ardlbounds))) mtype = 'ardl-ec'
  if(grepl('no cointegration$', gsub(' [(].*', '', m_ardlbounds))) mtype = 'ardl-firstdiff'
  
  cat(paste0('\n\nFinal result of bounds procedure: ', mtype,'\n\n'))
  

  # 2.0 Determine lag structure on the basis of `mtype`
  #autocorrel_lags= c(3,6,9,12,15,18,1)
  for (maxpq in autocorrel_lags) {
    m<-try(ardl(type=mtype, dt = dt, dv = dv, vars = unique(c(vars, quarter_vars)), exclude_cointegration = NULL,
            adf_tests= NULL, maxlag = 6, pval = pval, maxpq = maxpq, controls = controls_ardl),silent=T)
    if (class(m)=='ardl_procedure') if (m$autocorrelation==F) break
  }
 
  if (maxpq == 1) autocorrel_lags <- unique(c(1, autocorrel_lags))
  
  cat('\nResult of lag structure:\n')
  print(summary(m$model))
  
  # 3.0 Reestimate model, adding extra covariates
  
  # Collect model matrix and data
  df = m$model$model$model
  
  # Add brand attributes
  retain_attr=unlist(lapply(dt[-m$model$model$na.action, grep('^attr',colnames(dt),value=T),with=F], function(x) !all(is.na(x))))
  attr_vars = names(retain_attr[which(retain_attr==T)])
  attr_vars = attr_vars[unlist(lapply(dt[-m$model$model$na.action, attr_vars, with=F], use_ts))]
  
  # kick out NA vars -> check why it can't be identified
  
  identifiers = dt[-m$model$model$na.action,c('market_id', 'category','country', 'brand', 'brand_id' , 'date'),with=F]
  setkey(identifiers, market_id, date, brand)
  
  # Define controls
  if (withattributes==T) controls = c(attr_vars)
  if (withattributes==F) controls = NULL
  
  if (withcontrols==T & controls_in_bounds ==F) controls = c(controls, control_vars)
  
  control_adf_tests=rbindlist(lapply(c(controls), function(.var) {
    print(.var)
    return(data.frame(variable=.var, cbind(t(adf_enders(unlist(dt[, .var,with=F]), maxlag=6, pval=pval)))))
  }))
  
  if (control_ur == T & length(controls)>0) {
    controls_levels = as.character(control_adf_tests[ur==0]$variable)
    controls_differences = as.character(control_adf_tests[ur==1]$variable)
  
    for (.ctr in controls_differences) if (!is.null(.ctr)) dt[, paste0('d.1.', .ctr):=dshift(get(.ctr),1)]
  
    controls = c(controls_levels)
    if (length(controls_differences)>0) controls = c(controls, paste0('d.1.', controls_differences))
  }
  
  if (length(controls)>0) {
    newdf <- dt[-m$model$model$na.action, controls,with=F]
    finaldf = cbind(df, newdf)
  } else {
    finaldf = df
  }
  
  #####
  ## Add copulas (pending)
  if (with_copulas==T) {
    cop_terms = sapply(vars, function(.v) {
      tmp1=grep(paste0('^',.v,'$'),colnames(df),value=T)
      if (length(tmp1)>0) return(tmp1)
      tmp1=grep(paste0('^d.1.',.v,'$'),colnames(df),value=T)
      if (length(tmp1)>0) return(tmp1)
      return(NULL)
    })
    
    if (length(cop_terms)>0) {
      newdf <- dt[-m$model$model$na.action, paste0('cop_', cop_terms),with=F]
      finaldf = cbind(finaldf, newdf)
    }
  }

  #####
  
  # Reestimate
  m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
  
  na_coefs = names(m2$coefficients)[is.na(m2$coefficients)]
  if (length(na_coefs)>0) {
    finaldf =  finaldf[, -match(na_coefs, colnames(finaldf))]
    m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
  }
  
  # Kick out ns. controls
  if (kickout_ns_controls==T) {
    #nscoefs = names(m2$coefficients)[summary(m2)$coefficients[,4]>.1]
    #nscoefs = names(m2$coefficients)[abs(summary(m2)$coefficients[,3])<1]
    nscoefs = names(m2$coefficients)[abs(summary(m2)$coefficients[,3])<1]
    
    exclude_from_kickout = c('(Intercept)', grep(paste0(c(vars,'lnusales'), collapse = '|'), colnames(finaldf),value=T))
    exclude_from_kickout = grep('comp[_]|cop[_]', exclude_from_kickout, value=T, invert=T)
    
    nscoefs = nscoefs[!nscoefs%in%exclude_from_kickout]
    
    if (length(nscoefs)>0) {
      finaldf =  finaldf[, -match(nscoefs, colnames(finaldf))]
      cat('Before removal\n\n')
      print(summary(m2))
      
      m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
     
      cat('\nRemoval of n.s. coefficients:\n')
      cat(paste0(nscoefs,collapse=', '))
      cat('\n\n')
      
    }
      
  }
            
  cat('\nResult of final model:\n')
  print(summary(m2))
  
  retobj = list(m_final_type = mtype,
              m_autocorrelation = m$autocorrelation,
              model_matrix = m2$model, 
              paneldimension = identifiers, 
              adf_tests=cbind(identifiers[,!colnames(identifiers)%in%'date',with=F][1], m$adf_tests),
              dt=dt)
  
  
  if(return_models==T) {
    retobj$m_ardlbounds = m_ardlbounds
    retobj$m_lagstructure = m
    retobj$m_final = m2
  }
  
  return(retobj)
  
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
  
  # save ranges after rescaling
  #if (rescale==T) res$ranges$after=data.table(original_variable=colnames(X), min = colMins(X), max=colMaxs(X))[!grepl('[_]sq|[_]cop[_]|[_]dum', original_variable)]
  if(0){
  # flatten correlation
  tmp=data.table(melt(cor(X)))
  
  uniqvar = unique(tmp$Var2)
  tmp[, Var1index:=match(Var1,uniqvar)]
  tmp[, Var2index:=match(Var2,uniqvar)]
  tmp <- tmp[Var1index<Var2index]
  
  tmp[abs(value)>.9]
  tmp[,abscor:=abs(value)]
  ct=tmp[Var1=='bid001_d.1.comp_lnllen'|Var2=='bid001_d.1.comp_lnllen']
  setorder(ct, abscor)
  
  }
  
  estmethod = "FGLS"
  #maxiter=5000
  
  # remove var from itersur
  #tmp=sapply(1:ncol(X), function(rem) try(itersur(X=as.matrix(X[,-rem]),Y=as.matrix(Y),index=index, method = estmethod, maxiter=maxiter),silent=T))
  
  #unlist(lapply(tmp,class))
  

  #colnames(X)[which(unlist(lapply(tmp,class))=='character')]
  
  
  
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

# Simulate elasticities on the basis of SUR estimates
model_simulate <- function(focal_model, shockperiods = 12, covar = 'yes', ndraws=1000, return_simulations=F) {

  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  
  vars = vars[sapply(vars, function(x) any(grepl(paste0('[.]|^',x), colnames(focal_model$model_matrix))))]
  
  # 4.0 Extract estimation data set and manipulate for simulation
  
  sim_dat <- function(m, shockvar=NULL, shockperiods=20, shockvalue=log(1.01), reset_terms = NULL) { 
    
    dv <- m$model[,1]
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    
    # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
    Xsim_mat <- as.matrix(m$model[,-1])
    #colnames(Xsim_mat) <- colnames(m$model)
    
    if (!is.null(reset_terms)) {
      reset_vars <- grep(reset_terms, colnames(Xsim_mat),value=T)
      
      for (r in seq(along=reset_vars)) {
        Xsim_mat[,reset_vars[r]]<-0
      }
    }
    
    # set lagged DVs (or any derivative of it) to NA; except in period 1
    lagdvcol = grep(paste0('l[.][0-9][.]', dvblank, '|ld[.][0-9][.]', dvblank, '|l[.][0-9][.]d', dvblank), colnames(Xsim_mat),value=T)
    
    lags = as.numeric(gsub("\\D", "", lagdvcol))
    lags[is.na(lags)] <- 0
    
    for (s in seq(from=1, length.out=length(lagdvcol))) {
      Xsim_mat[-c(1:lags[s]), lagdvcol[s]] <- NA
    }
   
    # lagged differences
    lagdvcol = grep(paste0('ld[.][0-9][.]', dvblank, '|l[.][0-9][.]d', dvblank), colnames(Xsim_mat),value=T)
    lags = as.numeric(gsub("\\D", "", lagdvcol))
    lags[is.na(lags)] <- 0
    
    for (s in seq(from=1, length.out=length(lagdvcol))) Xsim_mat[1:lags[s], lagdvcol[s]] <- 0
    

    # collect variables pertaining to the variable that will be shocked
    relevant_vars=grep(paste0('(d[.][0-9].){0,1}', shockvar), colnames(Xsim_mat),value=T) 
    relevant_vars = grep('comp[_]', relevant_vars, invert=T,value=T)
    
    Xsim_mat_copy = Xsim_mat
    
    retobj=lapply(shockperiods, function(shockperiod) {
      Xsim_mat = Xsim_mat_copy
      if (is.null(shockvar)|shockvar=='base') {
        # no shock
        } else {
        
        relevant_vars_levels = grep('^d[.]|^ld[.]', relevant_vars, invert=T,value=T)
        lags = as.numeric(gsub("\\D", "", relevant_vars_levels))
        lags[is.na(lags)] <- 0
        
        for (s in seq(from=1, length.out=length(relevant_vars_levels))) Xsim_mat[shockperiod + lags[s], relevant_vars_levels[s]] <- Xsim_mat[shockperiod + lags[s], relevant_vars_levels[s]]+shockvalue
        
        relevant_vars_diffs = grep('^d[.]|^ld[.]', relevant_vars, invert=F,value=T)
        lags = as.numeric(gsub("\\D", "", relevant_vars_diffs))
        lags[is.na(lags)] <- 0
        lags[grepl('^d[.]', relevant_vars_diffs)] <- 0
        
        for (s in seq(from=1, length.out=length(relevant_vars_diffs))) {
          Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]]+shockvalue
          #antishock
          if (length(relevant_vars_levels)>0) Xsim_mat[shockperiod + lags[s] + 1, relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s] + 1, relevant_vars_diffs[s]] - shockvalue
          
        }
      }
    
    return(list(data=Xsim_mat, shockvar = shockvar, shockperiod=shockperiod))
    })
  retobj
  }
  
  # 4.1 Generate scenarios
  
  if (any(is.na(shockperiods))) shockperiods = 13:(nrow(df)-12)
  
  sim_ds = lapply(c('base', vars), function(x) sim_dat(m=list(model=focal_model$model_matrix), shockvar=x, shockvalue = log(1.01), shockperiod=shockperiods))
  
  # 5.0 Predict/simulate
 
  my_predict <- function(coefs, Sigma, m, Xsim_mat, L = 100, covar='yes', shockvar=NULL, dynamic = T, dt) {
    
    # Draw coefficients
    #coefs =m$coefficients
    #Sigma=vcov(m)
    
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    if (is.na(shockvar)) shockvar = NULL
    # only use variance/covariance of shockable variables
    if (covar=='no') Sigma = matrix(rep(0, prod(dim(Sigma))), ncol=ncol(Sigma))
    
    if (covar=='base' & is.null(shockvar)) Sigma = matrix(rep(0, prod(dim(Sigma))), ncol=ncol(Sigma))
    
    #if (covar=='partly') {
    # keep=grep(paste0(dvblank,'|',shockvar),names(coefs))
    # 
    #   Sigma[-keep,-keep] <-0
    #    
    #}
    set.seed(1234)
    draws=mvrnorm(n=L, mu=coefs, Sigma = Sigma)
    
      
    #draws[,1] <- coefs[1]
    
    Nperiods=nrow(Xsim_mat)
    
    
    predlevels <- matrix(rep(NA,Nperiods*L),ncol=L)
    
    preddiff <- matrix(rep(NA,Nperiods*L),ncol=L)
    pred <- matrix(rep(NA,Nperiods*L),ncol=L)
    
    
    #Xsim <- colMeans(m$model[,-1])
    
    Xsim_mat_L <- rep(cbind(1,Xsim_mat), L)
    dim(Xsim_mat_L) <- c(nrow(Xsim_mat), ncol(Xsim_mat)+1,L)
    colnames(Xsim_mat_L) <- c('intercept', colnames(Xsim_mat))
    
    # dvmean = Xsim[grep(paste0('^l[.][0-9][.]', dvblank,'$|^', dvblank,'$'), colnames(m$model$model$model[,-1]),value=T)][1]
    
    #if(is.na(dvmean)) dvmean=mean(eval(parse(text=paste0('dt$`',dvblank,'`'))))
    
    meansales <- mean(dt[!is.na(lnusales)]$lnusales[1:3])
    
    indiff=F
    if (grepl('^d[.]|^d', dvname)) indiff=T
    
    for (p in seq(from=1, to=Nperiods)) {
      
      #pred[p] = t(coefs)%*%cbind(c(1,Xsim_mat[p,]))
      pred[p,] =  rowSums(draws*t(Xsim_mat_L[p,,])) #draws%*%Xsim_mat_L[p,,]
      
      if (dynamic == T) {
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
      
    }
    
    if (dynamic==F) {
      
      if(indiff==T) {
        predlevels = meansales + apply(pred, 2, cumsum)
        preddiff <- pred
      } else {
        predlevels = pred
        
        }
        
      }
  
  
    return(list(data=Xsim_mat_L, means = cbind(ylevels_pred=rowMeans(predlevels), y_pred = rowMeans(pred)),
                simulated_levels = predlevels,
                simulated_dv = pred))
    
    # par(mfrow=c(3,1))
    #plot(pred, type='l')
    #plot(predlevels, type='l')
    
    #plot(m$model$model$model[,1], type='l')
  }
  
  
  # 5.1 Execute simulation
  res=lapply(1:length(sim_ds), function(i) lapply(sim_ds[[i]], function(simset) my_predict(coefs=focal_model$sur$coefs$coef,
                                                                                          Sigma=focal_model$sur$varcovar,
                                                                                          m=list(model=focal_model$model_matrix),
                                                                                          Xsim_mat = simset$data, 
                                                                                          L = ndraws, 
                                                                                          covar=covar, 
                                                                                          dynamic = T, 
                                                                                          dt=focal_model$dt,
                                                                                          shockvar=c(NA, vars)[i])))
  #coefs=focal_model$sur$coef
  
  # 5.2 Extract elasticities
  varnames=unlist(lapply(sim_ds, function(x) x[[1]]$shockvar))
  
  elast=rbindlist(lapply(varnames[-1], function(v) {
    #cat(v,fill=T)
    i=match(v,varnames)
    
    outc=lapply(seq(along=shockperiods), function(shockindex) {
      
      shockp=shockperiods[shockindex]
      
      outc=res[[i]][[shockindex]]$simulated_levels-res[[1]][[shockindex]]$simulated_levels
      
      elast1 = 100*outc[shockp,]
      elast6 = 100*colSums(outc[shockp:(shockp+6-1),])
      elast12 = 100*colSums(outc[shockp:(shockp+12-1),])
      elastlt12 = 100 * outc[(shockp+12-1),]
      o=as.matrix(c(elast1,elast6,elast12,elastlt12))
      dim(o) <-c(length(elast1),4)
      o
      })
    aa=unlist(outc)
    dim(aa) <- c(dim(outc[[1]]), length(shockperiods))
    
    outp=apply(aa,2,rowMeans)             
    
    return(data.frame(varname=v, 
                      Ndraws=dim(outp)[1],
                      Nperiods=length(shockperiods),
                      elast1 = mean(outp[,1]),
                      elast6 = mean(outp[,2]),
                      elast12 = mean(outp[,3]),
                      elastlt12 = mean(outp[,4]),
                      elast1_sd = sd(outp[,1]),
                      elast6_sd = sd(outp[,2]),
                      elast12_sd = sd(outp[,3]),
                      elastlt12_sd = sd(outp[,4])))
                      
  }))
  
  elast$brand_id = unique(focal_model$paneldimension$brand_id)
  print(elast)
  
  ### FIRST: first check face avlidity of mean parameter estimates
  ### NEXT: uncertainity; alleen errors, conditional forecast vs. unconditional; weer parameter uncertainty
  rm(sim_ds)
  rm(res)
  
  retobj = list(elasticities = elast)
  
  if(return_simulations==T) {
    retobj$simulation_data_raw = sim_ds
    retobj$simulation_data = lapply(res, function(x) x$data)
    retobj$simulated_dv = lapply(res, function(x) x$simulated_dv)
    retobj$simulated_levels = lapply(res, function(x) x$simulated_levels)
  }
  
  retobj
}



#  plot(dt$dlnusales,type='l')
#  plot(dt$lnusales,type='l')

