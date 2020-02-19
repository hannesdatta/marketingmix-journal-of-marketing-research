# Model specification: 
run_ardl <- function(type='ardl-ec') {
  
  # case (e)
  if (type=='ardl-ec') {
    
    formula=as.formula(paste0(dv,'~1+', paste(c(vars, quarter_vars), collapse='+')))
    
    lags_list = lapply(c(dv, vars), function(x) 1)
    names(lags_list) = c(dv,vars)
    
    diffs = vars
    
    lagdiffs = list()
    
    levels = c(quarter_vars)
    
    lagdiffs_list = lapply(sapply(1:6, function(x) 1:x), function(x) {
      obj=lapply(c(dv, vars), function(...) x)
      names(obj) = c(dv,vars)
      return(obj)
      
    })
    ec=TRUE
    
    #lagdiffs_list=c(list(NULL), lagdiffs_list)
    
    possible_grid = expand.grid(0:3, 0:3, 0:3, 0:3)
    start_values=colMins(possible_grid)
    
    lagdiffs_list = apply(possible_grid, 1, function(x) {
      obj=list()
      for (xi in seq(along=x)) obj[[xi]]<-start_values[xi]:x[xi]
      names(obj) = c(dv,vars)
      obj=obj[!unlist(lapply(obj, function(x) all(x==0)))]
      obj=lapply(obj, function(x) x[!x==0])
      
      
    })
    
  }
  
  if (type=='ardl-levels') {
    # work in progress - not finished yet
    formula=as.formula(paste0(dv,'~1+', paste(c(vars, quarter_vars), collapse='+')))
    
    lags_list = lapply(c(dv, vars), function(x) 1)
    names(lags_list) = c(dv,vars)
    
    diffs = adf_tests[ur==1&!variable==dv]$variable
    
    lagdiffs = list()
    
    levels = c(quarter_vars)
    
    lagdiffs_list = lapply(sapply(1:6, function(x) 1:x), function(x) {
      obj=lapply(c(dv, vars), function(...) x)
      names(obj) = c(dv,vars)
      return(obj)
      
    })
    ec=TRUE
    
    
    
    
    lagdiffs_list=c(list(NULL), lagdiffs_list)
  }
  
  # Estimate models with varying lag terms
  models=lapply(lagdiffs_list, function(lagdiff) {
    mx<-dynardl(formula, data=dat, lags = lags_list, diffs = diffs,
                lagdiffs = lagdiff, levels=levels, ec = ec, simulate = FALSE, trend = use_trend)
    
    autocorrel_test=dynardl.auto.correlated(mx, object.out=T)
    autocorrel_test$bg$p.value
    
    list(bic=BIC(mx$model), model=mx, autocorrel_p=autocorrel_test$bg$p.value)
  })
  
  # choose the one with lowest BIC & no auto correlation
  bics=unlist(lapply(models, function(x) x$bic))
  autocorrel=unlist(lapply(models, function(x) x$autocorrel_p))
  
  if (length(which(autocorrel>.1))==0) return('cannot remove autocorrel')
  
  # choose model without autocorrel & highest BIC
  m.choice=match(min(bics[autocorrel>.1]), bics)
  
  #m=models[[which(bics==min(bics))]]$model
  m=models[[m.choice]]$model
  
  # check for autocorrelation
  #plot(m$model$residuals)
  
  autocorrel_test=dynardl.auto.correlated(m, object.out=T)
  autocorrel_test$bg$p.value
  
  autocorrelation=autocorrel_test$bg$p.value<.1
  
  boundstest=pssbounds(m, object.out = T)
  boundstest_result = 'inconclusive'
  if (boundstest$fstat < boundstest$`ftest.I0.p10`) boundstest_result='no cointegration'
  if (boundstest$fstat > boundstest$`ftest.I1.p10`) boundstest_result='cointegration'
  
  return(list(model=m, boundstest_result=boundstest_result, autocorrelation=autocorrelation, autocorrel_test=autocorrel_test,
              mchoice=m.choice))
  
}

