
newfkt <- function(id, withcontrols=T, withattributes=T) {
  
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
  
  m_ardlbounds <- analyze_brand(id, quarters=T, xs = vars, controls = control_vars, dat = dt)
  
  cat(paste0('\n\nOutput of the bounds procedure: ', m_ardlbounds))
  
  mtype = as.character(NA)
  if(grepl('stationarity in DV',gsub(' [(].*', '', m_ardlbounds))) mtype = 'ardl-levels'
  if(grepl('cointegration$', gsub(' [(].*', '', m_ardlbounds))) mtype = 'ardl-ec'
  if(grepl('no cointegration$', gsub(' [(].*', '', m_ardlbounds))) mtype = 'ardl-firstdiff'
  
  cat(paste0('\n\nFinal result of bounds procedure: ', mtype,'\n\n'))
  

  # 2.0 Determine lag structure on the basis of `mtype`
  autocorrel_lags= c(3,6,9,12,15,18,1)
  for (maxpq in autocorrel_lags) {
    m<-try(ardl(type=mtype, dt = dt, dv = dv, vars = unique(c(vars, quarter_vars)), exclude_cointegration = NULL,
            adf_tests= NULL, maxlag = 6, pval = .1, maxpq = maxpq, controls = control_vars),silent=T)
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
  
  if (length(controls)>0) {
    newdf <- dt[-m$model$model$na.action, controls,with=F]
    finaldf = cbind(df, newdf)
  } else {
    finaldf = df
  }
  
  #####
  ## Add copulas (pending)
  #####
  
  # Reestimate
  m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
  
  na_coefs = names(m2$coefficients)[is.na(m2$coefficients)]
  if (length(na_coefs)>0) {
    finaldf =  finaldf[, -match(na_coefs, colnames(finaldf))]
    m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
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
  
  
  
  sim_dat2 <- function(m, shockvar=NULL, shockperiod=20, shockvalue=log(1.01)) {
    
    dv <- m$model[,1]
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    
    # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
    Xsim_mat <- as.matrix(m$model[,-1])
    #colnames(Xsim_mat) <- colnames(m$model)
    
    # set lagged DVs (or any derivative of it) to NA; except in period 1
    lagdvcol = grep(paste0('l[.][0-9][.]', dvblank, '|ld[.][0-9][.]', dvblank, '|l[.][0-9][.]d', dvblank), colnames(Xsim_mat),value=T)
    Xsim_mat[-1, lagdvcol] <- NA
    
    lagdvcol = grep(paste0('ld[.][0-9][.]', dvblank, '|l[.][0-9][.]d', dvblank), colnames(Xsim_mat),value=T)
    Xsim_mat[1, lagdvcol] <- 0
    Xsim_mat[-1, lagdvcol] <- NA
    
    Nperiods = nrow(Xsim_mat)
    # stretch out
    
    # set lagged DV in the first period to mean observed DV 
    #meansales <- mean(dt[!is.na(lnusales)]$lnusales[1:3])
    
    #Xsim_mat[1, grep(paste0('l[.][0-9][.]', dvblank), names(Xsim),value=T)] <- meansales
    
    
    # Extract coefficients from model for simulation
    #shockperiod = 25
    #shockvar = vars[3]
    #shockvalue = 0 #1.1
    
    # extend lagged dependent variables / check whether this is still needed!!!
    #relevant_vars=grep(paste0('l[.][0-9].', dvblank), colnames(Xsim_mat),value=T)
    #for (s in seq(from=1, length.out=length(relevant_vars))) Xsim_mat[seq(from=s, to=1), relevant_vars[s]] <- meansales
    
    #if ('trendvar'%in%colnames(Xsim_mat)) Xsim_mat[, 'trendvar']<-1:Nperiods
    
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
      
      for (s in seq(from=1, length.out=length(relevant_vars_diffs))) {
        Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]]+shockvalue
        #antishock
        if (length(relevant_vars_levels)>0) Xsim_mat[shockperiod + lags[s] + 1, relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s] + 1, relevant_vars_diffs[s]] - shockvalue
        
      }
    }
    
    return(list(data=Xsim_mat, shockvar = shockvar, shockperiod=shockperiod))
  }
  
  # 4.1 Generate scenarios
  sim_ds = lapply(c('base', vars), function(x) sim_dat2(m=m2, shockvar=x, shockvalue = log(1.01), shockperiod=12))
 # sim_ds = lapply(c('base', vars), function(x) sim_dat(m=m2, shockvar=x, shockvalue = log(1.01), shockperiod=12))
  
  #View(sim_ds[[1]]$data)
  
       
  
  ## reset copulas to zero
  #if(length(which(grepl('cop[_]', colnames(dset))))>0) coefs[which(grepl('cop[_]', colnames(dset)))]<-0
  
  #View(sim_ds[[2]]$data)
  #Xsim_mat=sim_ds[[1]]$data
  
  Xsim_mat=as.matrix(m2$model[,-1])
  
  
  out=my_predict(m2, Xsim_mat, L=1000, covar='no',use_residerror = T, shockvar=NA, dynamic = F)
  
  plot(out$simulated_dv[,120],type='l')
  plot(out$simulated_levels[,150],type='l')
  
  
  
  library(MASS)
  
  # 5.0 Predict/simulate
  #mclone=m2
  #mclone$coefficients <- c()
  
  my_predict <- function(m, Xsim_mat, L = 100, covar=c('no'), use_residerror = F, shockvar=NULL, dynamic = T) {
    
    # Draw coefficients
    coefs =m$coefficients
    Sigma=vcov(m)
    
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    if (is.na(shockvar)) shockvar = NULL
    # only use variance/covariance of shockable variables
    if (covar=='no') Sigma = matrix(rep(0, prod(dim(Sigma))), ncol=ncol(Sigma))
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
  
 res=lapply(1:length(sim_ds), function(i) my_predict(m2, sim_ds[[i]]$data, L = 1000, covar='no', 
                                                     use_residerror = F, dynamic = T, shockvar=c(NA, vars)[i]))
   #View(res[[2]])
  #plot(res[[1]]$simulated_dv[,3],type='l')
# plot(res[[1]]$simulated_levels[,100],type='l')
 
 
 
 # Marnik/Nijs: - simulate from the mean, then use rsidual standard error to be added to each prediction
 # liefst: unconditional simulation of y (like I have done it now)
 
 # varying shock period: 
  # hoe de variance te bepalen; 
 
 #for 1:1000
 
  # for shock period in second year -- last year-1:
    
  #  predictions
    
 #   four elasticities: 1, 6, 12, 24
    
 # mean across 36 shock periods
  
#mean and SD over draws 
    
 
  # 

#plot(res[[1]]$simulated_levels[,5],type='l')
  
 # plot(cumsum(res[[1]]$simulated_dv[,9]),type='l')
  
  #lines(cumsum(res[[2]]$simulated_dv[,9]),type='l', lty=2)
  
  
  #plot(res[[1]]$simulated_levels[,9],type='l')
  #plot(res[[1]]$simulated_levels[,2],type='l')
  
  #plot(res[[2]]$simulated_levels[,1]-res[[1]]$simulated_levels[,1],type='l')
  #plot(res[[2]]$simulated_levels[,2]-res[[1]]$simulated_levels[,2],type='l')
  
  #plot(res[[1]]$simulated_levels[,2],type='l')
  
  
  #plot(res[[1]]$simulated_levels[,1],type='l')
  #lines(res[[2]]$simulated_levels[,1],type='l')
  
  #res[[2]]$simulated_levels[,1]- res[[1]]$simulated_levels[,1]

  
  
  # 5.2 Extract elasticities
  
  varnames=unlist(lapply(sim_ds, function(x) x$shockvar))
  
  elast=rbindlist(lapply(varnames[-1], function(v) {
    #cat(v,fill=T)
    i=match(v,varnames)
    shockp=unlist(lapply(sim_ds, function(x) x$shockperiod))[i]
    
    outc=res[[i]]$simulated_levels-res[[1]]$simulated_levels
    
    plot(rowMeans(res[[i]]$simulated_levels), type='l', ylab = 'log usales', xlab = 'simulation period', main = paste0('Shock in ', v))
    lines(rowMeans(res[[1]]$simulated_levels), type='l',lty=2)
    
    elast1 = 100*outc[shockp,]
    elast6 = 100*colMeans(outc[shockp:(shockp+6-1),])
    elast12 = 100*colMeans(outc[shockp:(shockp+12-1),])
    elast24 = 100*colMeans(outc[shockp:(shockp+24-1),])
    
    return(data.frame(brand_id = id, varname=v, 
                      elast1 = mean(elast1),
                      elast6 = mean(elast6),
                      elast12 = mean(elast12),
                      elast24 = mean(elast24),
                      elast1_sd = sd(elast1),
                      elast6_sd = sd(elast6),
                      elast12_sd = sd(elast12),
                      elast24_sd = sd(elast24)))
    
  }))
  
  print(elast)
  
  ### FIRST: first check face avlidity of mean parameter estimates
  ### NEXT: uncertainity; alleen errors, conditional forecast vs. unconditional; weer parameter uncertainty
   
  
  list(m_ardlbounds = m_ardlbounds, 
       m_lagstructure = m,
       m_final = m2,
       m_final_type = mtype,
       #simulation_data_raw = sim_ds, simulation_data = lapply(res, function(x) x$data), 
       #simulated_dv = lapply(res, function(x) x$simulated_dv), 
       #simulated_levels = lapply(res, function(x) x$simulated_levels) ,
       elasticities = elast)
}



#  plot(dt$dlnusales,type='l')
#  plot(dt$lnusales,type='l')

