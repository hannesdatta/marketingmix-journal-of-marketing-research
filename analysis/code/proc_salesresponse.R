
newfkt <- function(id, withcontrols=T, controls_in_bounds = T, withattributes=T, 
                   autocorrel_lags= c(3,6,9,12,15,18,1),
                   control_ur = F,
                   return_models = F, return_simulations = F, shockperiods=NA,
                   ndraws=5, covar = 'no',
                   kickout_ns_controls = F, pval = .1) {
  
  # 1.0 Conduct bounds test
  dv='lnusales'
  dt=data.table(brand_panel[brand_id==id])
  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  
  dt[, lngdp := log(gdppercapita)]
  dt[, lnholiday := log(npublicholidays+1)]
  
  control_vars = c('lngdp', 'lnholiday', grep('comp[_]', colnames(dt),value=T))
  if(withcontrols==F) control_vars = NULL
  control_vars = control_vars[unlist(lapply(dt[, control_vars, with=F], use_ts))]
  
  quarter_vars = c('quarter1','quarter2','quarter3')
  
  if (controls_in_bounds==T) {
    controls_ardl = control_vars
  } else {
  
    controls_ardl = NULL}
  
  m_ardlbounds <- analyze_brand(id, quarters=T, xs = vars, controls = controls_ardl, dat = dt,
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
  
  # Reassemble model matrix and data
  df = m$model$model$model
  
  # Attributes
  retain_attr=unlist(lapply(dt[-m$model$model$na.action, grep('^attr',colnames(dt),value=T),with=F], function(x) !all(is.na(x))))
  attr_vars = names(retain_attr[which(retain_attr==T)])
  attr_vars = attr_vars[unlist(lapply(dt[-m$model$model$na.action, attr_vars, with=F], use_ts))]
  
  # kick out NA vars -> check why it can't be identified
  
  identifiers = dt[-m$model$model$na.action,c('market_id','date', 'brand'),with=F]
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
 # .v=vars[1]
  
 # .res=grep(paste0('^',.v,'$'),colnames(df),value=T)
 # .res=grep(paste0('^d.1.',.v,'$'),colnames(df),value=T)
  
 # c('d.1.'
    
 # grep(paste0(vars,collapse='|'), colnames(df),value=T)
  
  
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
    exclude_from_kickout = grep('comp[_]', exclude_from_kickout, value=T, invert=T)
    
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
  
  # 4.0 Extract estimation data set and manipulate for simulation
  
  sim_dat <- function(m, shockvar=NULL, shockperiod=20, Nperiods=48, shockvalue=log(1.01)) {
    
    dv <- m$model[,1]
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    
    # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
    Xsim <- colMeans(m$model[,-1])
    
    # Set lags of main effect variables (i.e. not the DV) to the same (first) value
    for (.var in grep('l[.][0-9][.]', names(Xsim),value=T)) {
      blankvar = gsub('l[.][0-9][.]','',.var)
      Xsim[.var] <- Xsim[grep(paste0('^', blankvar,'|l[.][0-9][.]',blankvar), names(Xsim), value=T)[1]]
    }
    
    # set lagged DVs to NA (0 in case it's first differenced)
    lagdvcol = grep(paste0('l[.][0-9][.]', dvblank, '|ld[.][0-9][.]', dvblank, '|l[.][0-9][.]d', dvblank), names(Xsim),value=T)
    Xsim[lagdvcol] <- NA
    Xsim[lagdvcol[grepl('l[.][0-9][.]d[.]|ld[.][0-9][.]|l[.][0-9][.]d', lagdvcol)]] <- 0
    
    # set all variables in differences to 0
    Xsim[grep('^d[.][0-9][.]|^ld[.][0-9][.]', names(Xsim))] <- 0
    
    # stretch out
    Xsim_mat <- matrix(rep(Xsim,Nperiods),byrow=T, ncol=length(Xsim))
    colnames(Xsim_mat)<-names(Xsim)
    
    # set lagged DV in the first period to mean observed DV 
    meansales <- mean(dt[!is.na(lnusales)]$lnusales[1:3])
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
    
    if ('trendvar'%in%names(Xsim)) Xsim_mat[, 'trendvar']<-1:Nperiods # check whether this shouldn't have been in logs?!
    
    # collect variables pertaining to the variable that will be shocked
    relevant_vars=grep(paste0('(d[.][0-9].){0,1}', shockvar), colnames(Xsim_mat),value=T) 
    relevant_vars = grep('comp[_]', relevant_vars, invert=T,value=T)
    
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
      
      #for (s in seq(from=1, length.out=length(relevant_vars_diffs))) Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]]+shockvalue
      
      for (s in seq(from=1, length.out=length(relevant_vars_diffs))) {
        Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]]+shockvalue
        #antishock
        if (length(relevant_vars_levels)>0) Xsim_mat[shockperiod + lags[s] + 1, relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s] + 1, relevant_vars_diffs[s]] - shockvalue
        
      }
    }
    
    return(list(data=Xsim_mat, shockvar = shockvar, shockperiod=shockperiod))
  }
  
  
  
  sim_dat2 <- function(m, shockvar=NULL, shockperiods=20, shockvalue=log(1.01)) {
    
    dv <- m$model[,1]
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    
    # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
    Xsim_mat <- as.matrix(m$model[,-1])
    #colnames(Xsim_mat) <- colnames(m$model)
    
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
  
  sim_ds = lapply(c('base', vars), function(x) sim_dat2(m=m2, shockvar=x, shockvalue = log(1.01), shockperiod=shockperiods))
 # sim_ds = lapply(c('base', vars), function(x) sim_dat(m=m2, shockvar=x, shockvalue = log(1.01), shockperiod=12))
  

  ## reset copulas to zero
  #if(length(which(grepl('cop[_]', colnames(dset))))>0) coefs[which(grepl('cop[_]', colnames(dset)))]<-0
  
  #View(sim_ds[[2]]$data)
  #Xsim_mat=sim_ds[[1]]$data
   
  
  
  library(MASS)
  
  # 5.0 Predict/simulate
 
  my_predict <- function(m, Xsim_mat, L = 100, covar=c('no'), use_residerror = F, shockvar=NULL, dynamic = T) {
    
    # Draw coefficients
    coefs =m$coefficients
    Sigma=vcov(m)
    
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
    
    set.seed(6312)
    residerror = matrix(rnorm(L*Nperiods, mean = 0, sd = summary(m)$sigma),ncol=L)
    
    
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
      
      if (use_residerror ==T) pred[p,] <- pred[p,] + residerror[p,]
      
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
  
 res=lapply(1:length(sim_ds), function(i) lapply(sim_ds[[i]], function(simset) my_predict(m2, simset$data, L = ndraws, covar=covar, 
                                                     use_residerror = F, dynamic = T, shockvar=c(NA, vars)[i])))
 

 #for 1:1000
 
  # for shock period in second year -- last year-1:
    
  #  predictions
    
 #   four elasticities: 1, 6, 12, 24
    
 # mean across 36 shock periods
  
#mean and SD over draws 
    
  
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
    
    return(data.frame(brand_id = id, varname=v, 
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
  
  print(elast)
  
  ### FIRST: first check face avlidity of mean parameter estimates
  ### NEXT: uncertainity; alleen errors, conditional forecast vs. unconditional; weer parameter uncertainty
  rm(sim_ds)
  rm(res)
  
  retobj = list(elasticities = elast,
                m_final_type = mtype,
                m_autocorrelation = m$autocorrelation)
  
  if(return_models==T) {
    retobj$m_ardlbounds = m_ardlbounds
    retobj$m_lagstructure = m
    retobj$m_final = m2
  }
  
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

