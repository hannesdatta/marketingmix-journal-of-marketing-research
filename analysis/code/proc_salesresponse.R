
newfkt <- function(id, withcontrols=T) {
  
  # 1.0 Conduct bounds test
  dv='lnusales'
  dt=data.table(brand_panel[brand_id==id])
  vars = c('lnrwpspr','lnllen','lnwpswdst') 
  
  dt[, lngdp := log(gdppercapita)]
  dt[, lnholiday := log(npublicholidays+1)]
  
  control_vars = c('lngdp', 'lnholiday', grep('comp[_]', colnames(dt),value=T))
  if(withcontrols==F) control_vars = NULL
  control_vars = control_vars[unlist(lapply(dt[, control_vars, with=F], use_ts))]
  
  quarter_vars = c('quarter1','quarter2','quarter3')
  
  res <- analyze_brand(id, quarters=T, xs = vars, controls = control_vars, dat = dt)
  
  cat(paste0('\n\nOutput of the bounds procedure: ', res))
  
  mtype = as.character(NA)
  if(grepl('stationarity in DV',gsub(' [(].*', '', res))) mtype = 'ardl-levels'
  if(grepl('cointegration$', gsub(' [(].*', '', res))) mtype = 'ardl-ec'
  if(grepl('no cointegration$', gsub(' [(].*', '', res))) mtype = 'ardl-firstdiff'
  
  cat(paste0('\n\nFinal result of bounds procedure: ', mtype,'\n\n'))
  

  # 2.0 Determine lag structure on the basis of `mtype`
  autocorrel_lags= c(3,6,9,12,15,18)
  for (maxpq in autocorrel_lags) {
    m<-ardl(type=mtype, dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
            adf_tests= NULL, maxlag = 6, pval = .1, maxpq = maxpq, controls = control_vars)
    if (class(m)=='ardl_procedure') break
  }
 
  cat('\nResult of lag structure:\n')
  print(summary(m$model))
  
  # 3.0 Reestimate model, adding extra covariates
  
  # Attributes
  retain_attr=unlist(lapply(dt[, grep('^attr',colnames(dt),value=T),with=F], function(x) !all(is.na(x))))
  attr_vars = names(retain_attr[which(retain_attr==T)])
  attr_vars = attr_vars[unlist(lapply(dt[, attr_vars, with=F], use_ts))]
  
  # Reassemble model matrix and data
  df = m$model$model$model
  identifiers = dt[-m$model$model$na.action,c('market_id','date', 'brand'),with=F]
  setkey(identifiers, market_id, date, brand)
  
  # Define controls
  controls = c(attr_vars)
  newdf <- dt[-m$model$model$na.action, controls,with=F]
  finaldf = cbind(df, newdf)
  
  # Add copulas (pending)
  
  # Reestimate
  m2 <- lm(as.formula(paste0(colnames(finaldf)[1], '~1+', paste0(colnames(finaldf)[-1],collapse='+'))), data=finaldf)
  cat('\nResult of final model:\n')
  print(summary(m2))
  
  # 4.0 Extract estimation data set and manipulate for simulation
  
  sim_dat <- function(m, shockvar=NULL, shockperiod=20, Nperiods=30, shockvalue=log(1.01)) {
    
    dv <- m$model[,1]
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    
    # Xsim holds the "starting" values for the simulation data set (NOT the coefficients)
    Xsim <- colMeans(m$model[,-1])
    
    # Set lags of main effect variables (i.e. not the DV) to the same (first) value
    for (.var in grep('l[.][0-9][.]', names(Xsim),value=T))
      blankvar = gsub('l[.][0-9][.]','',.var)
    Xsim[.var] <- Xsim[grep(paste0('^', blankvar,'|l[.][0-9][.]',blankvar), names(Xsim), value=T)[1]]
    
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
      
      for (s in seq(from=1, length.out=length(relevant_vars_diffs))) Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]] <- Xsim_mat[shockperiod + lags[s], relevant_vars_diffs[s]]+shockvalue
      
    }
    
    return(list(data=Xsim_mat, shockvar = shockvar, shockperiod=shockperiod))
  }
  
  # 4.1 Generate scenarios
  sim_ds = lapply(c('base', vars), function(x) sim_dat(m=m2, shockvar=x, shockvalue = log(1.01), shockperiod=25, Nperiods = 48))
  
  ## reset copulas to zero
  #if(length(which(grepl('cop[_]', colnames(dset))))>0) coefs[which(grepl('cop[_]', colnames(dset)))]<-0
  
  #View(sim_ds[[2]]$data)
  #Xsim_mat=sim_ds[[2]]$data
  
  
  library(MASS)
  
  # 5.0 Predict/simulate
  
  my_predict <- function(m, Xsim_mat, L = 100) {
    
    # Draw coefficients
    coefs =m$coefficients
    Sigma=vcov(m)
    #Sigma = matrix(rep(0, prod(dim(Sigma))), ncol=ncol(Sigma))
    set.seed(1234)
    draws=mvrnorm(n=L, mu=coefs, Sigma = Sigma)
    
    Nperiods=nrow(Xsim_mat)
    
    predlevels <- matrix(rep(NA,Nperiods*L),ncol=L)
    
    preddiff <- matrix(rep(NA,Nperiods*L),ncol=L)
    pred <- matrix(rep(NA,Nperiods*L),ncol=L)
    
    dvname = colnames(m$model)[1]
    dvblank=gsub('.*[.]|^d','', dvname)
    
    #Xsim <- colMeans(m$model[,-1])
    
    Xsim_mat_L <- rep(cbind(1,Xsim_mat), L)
    dim(Xsim_mat_L) <- c(nrow(Xsim_mat), ncol(Xsim_mat)+1,L)
    colnames(Xsim_mat_L) <- c('intercept', colnames(Xsim_mat))
    
    # dvmean = Xsim[grep(paste0('^l[.][0-9][.]', dvblank,'$|^', dvblank,'$'), colnames(m$model$model$model[,-1]),value=T)][1]
    
    #if(is.na(dvmean)) dvmean=mean(eval(parse(text=paste0('dt$`',dvblank,'`'))))
    
    meansales <- mean(dt$lnusales)
    
    indiff=F
    if (grepl('^d[.]|^d', dvname)) indiff=T
    
    for (p in seq(from=1, to=Nperiods)) {
      
      #pred[p] = t(coefs)%*%cbind(c(1,Xsim_mat[p,]))
      pred[p,] =  rowSums(draws*t(Xsim_mat_L[p,,])) #draws%*%Xsim_mat_L[p,,]
      
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
    
    return(list(data=Xsim_mat_L, means = cbind(ylevels_pred=rowMeans(predlevels), y_pred = rowMeans(pred)),
                simulated_levels = predlevels,
                simulated_dv = pred))
    
    # par(mfrow=c(3,1))
    #plot(pred, type='l')
    #plot(predlevels, type='l')
    
    #plot(m$model$model$model[,1], type='l')
  }
  
  
  # 5.1 Execute simulation
  
  res=lapply(sim_ds, function(x) my_predict(m2, x$data))
  #View(res[[2]])
  
  # 5.2 Extract elasticities
  
  varnames=unlist(lapply(sim_ds, function(x) x$shockvar))
  
  elast=rbindlist(lapply(varnames[-1], function(v) {
    #cat(v,fill=T)
    i=match(v,varnames)
    shockp=unlist(lapply(sim_ds, function(x) x$shockperiod))[i]
    
    outc=res[[i]]$simulated_levels-res[[1]]$simulated_levels
    
    plot(rowMeans(res[[i]]$simulated_levels), type='l', ylab = 'log usales', xlab = 'simulation period', main = paste0('Shock in ', v))
    lines(rowMeans(res[[1]]$simulated_levels), type='l',lty=2)
    
    elast6 = colSums(outc[shockp:(shockp+6-1),])
    elast12 = colSums(outc[shockp:(shockp+12-1),])
    elast24 = colSums(outc[shockp:(shockp+24-1),])
    
    #print(mean(elast6))
    #print(mean(elast12))
    #print(mean(elast24))
    
    return(data.frame(brand_id = id, varname=v, 
                      elast6 = mean(elast6),
                      elast12 = mean(elast12),
                      elast24 = mean(elast24),
                      elast6_sd = sd(elast6),
                      elast12_sd = sd(elast12),
                      elast24_sd = sd(elast24)))
    
    #plot(outc, type='l', main=varname)
  }))
  
  print(elast)
  
  list(ardl_bounds = m, model_type = mtype, final_model = m2, simulation_data_raw = sim_ds, simulation_data = lapply(res, function(x) x$data), 
       simulated_dv = lapply(res, function(x) x$simulated_dv), 
       simulated_levels = lapply(res, function(x) x$simulated_levels) ,elasticities = elast)
}

#  plot(dt$dlnusales,type='l')
#  plot(dt$lnusales,type='l')

