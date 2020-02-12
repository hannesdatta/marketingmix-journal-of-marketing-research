library(urca)
library(dynamac)
require(stats4)

analyze_brand <- function(bid, quarters=T) {
  dat=brand_panel[brand_id==bid]
  
  print(unique(dat$category))
  print(unique(dat$country))
  
  # plotting
  par(mfrow=c(2,2))
  dv='lnusales'
  
  vars = c('lnrwpspr','lnllen','lnwpswdst')
  
  #melted_panel
  
  #[, ts_selected := use_ts(value), by=c('country','category', 'brand', 'variable')]
  
  if (quarters==T) quarter_vars = c('quarter1','quarter2','quarter3')
  if (quarters==F) quarter_vars = NULL
  
  for (.var in c(dv,vars)) ts.plot(dat[, .var,with=F], main = .var)
  
  par(mfrow=c(1,1))
  
  ##############
  # UNIT ROOTS #
  ##############
  
  adf_tests=rbindlist(lapply(c(dv,vars), function(.var) {
    print(.var)
    return(data.frame(variable=.var, cbind(t(adf_enders(unlist(dat[, .var,with=F]), maxlag=6, pval=.1)))))
  }))
  
  print(adf_tests)
  
  
  dv_unitroot = adf_tests[variable==dv]$ur
  
  print(paste0('a) Is the DV non-stationary? ', ifelse(dv_unitroot==1, 'yes', 'no')))
  
  if(dv_unitroot==1) {
    
    # (b) Are all independent variables of order I(1) or lower?
    all_order1=all(adf_tests[-1,]$order<=1)
    
    print(paste0('a) All independent variables of order I(1) or lower? ', ifelse(all_order1==1, 'yes', 'no')))
    
    use_trend=as.logical(adf_tests[variable=='lnusales']$trend)
    
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
        
        possible_grid = expand.grid(1:3, 0:3, 0:3, 0:3)
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
    
    
    m<-run_ardl(type='ardl-ec')
    if ('cannot remove autocorrel'%in%m) return(m)
    
    boundstest_result=m$boundstest_result
    
     
    initial_bounds=boundstest_result
    
    if(boundstest_result=='inconclusive') {
       
      # exclude non-UR variables from lag_levels
      i0_vars=as.character(adf_tests[!variable==dv&ur==0]$variable)
      i1_vars=as.character(adf_tests[!variable==dv&ur==1]$variable)
      # combinations
      i1vars_comb = do.call('c', sapply(1:(length(i1_vars)-1), function(x) combn(i1_vars, x, simplify=F)))
      
      potential_exclusions = c(list(i0_vars), lapply(i1vars_comb, function(x) c(i0_vars,x)))
      
      if (length(i0_vars)>0) names(potential_exclusions)[1]<-'i0vars'
      if (length(i1vars_comb)>0) names(potential_exclusions)[-1]<-unlist(lapply(i1vars_comb, paste, collapse=','))
      
      #if(length(i0_vars)==0) return(paste0(mid,'; inconclusive: cannot remove stationary regressor'))
      
      #exclusions[]
      
      if (length(potential_exclusions)>0) {
        
        run_model <- function(excl_vars) {
          # Model specification
          formula=as.formula(paste0(dv,'~1+', paste(c(vars, quarter_vars), collapse='+')))
          
          lags_list = lapply(c(dv, vars[!vars%in%excl_vars]), function(x) 1)
          names(lags_list) = c(dv,vars[!vars%in%excl_vars])
          
          diffs = vars
          
          lagdiffs = list()
          
          levels = c(quarter_vars)
          
          possible_grid = expand.grid(1:3, 0:3, 0:3, 0:3)
          start_values=colMins(possible_grid)
          
          lagdiffs_list = apply(possible_grid, 1, function(x) {
            obj=list()
            for (xi in seq(along=x)) obj[[xi]]<-start_values[xi]:x[xi]
            names(obj) = c(dv,vars)
            obj=obj[!unlist(lapply(obj, function(x) all(x==0)))]
            obj=lapply(obj, function(x) x[!x==0])
            
            
          })
          
          models=lapply(lagdiffs_list, function(lagdiff) {
            mx<-dynardl(formula, data=dat, lags = lags_list, diffs = diffs,
                        lagdiffs = lagdiff, levels=levels, ec = TRUE, simulate = FALSE, trend = use_trend)
            autocorrel_test=dynardl.auto.correlated(mx, object.out=T)
            autocorrel_test$bg$p.value
            
            list(bic=BIC(mx$model), model=mx, autocorrel_p=autocorrel_test$bg$p.value)
          })
          
          # lowest BIC
          bics=unlist(lapply(models, function(x) x$bic))
          autocorrel=unlist(lapply(models, function(x) x$autocorrel_p))
          
          if (length(which(autocorrel>.1))==0) return('cannot remove autocorrel')
          
          # choose model without autocorrel & highest BIC
          m.choice=match(min(bics[autocorrel>.1]), bics)
          
          #m=models[[which(bics==min(bics))]]$model
          m=models[[m.choice]]$model
          
          # check for autocorrelation
          plot(m$model$residuals)
          
          autocorrel_test=dynardl.auto.correlated(m, object.out=T)
          autocorrel_test$bg$p.value
          
          
          autocorrelation=autocorrel_test$bg$p.value<.1
          
          boundstest=pssbounds(m, object.out = T)
          boundstest_result = 'inconclusive'
          if (boundstest$fstat < boundstest$`ftest.I0.p10`) boundstest_result='no cointegration'
          if (boundstest$fstat > boundstest$`ftest.I1.p10`) boundstest_result='cointegration'
          
          print(boundstest_result)
          return(boundstest_result)
        }
        ot=unlist(lapply(potential_exclusions[1], run_model))
        
        return(paste0('initially: ', initial_bounds, '; now: ', ot[1]))
        
        if (!ot[1]=='inconclusive') boundstest_result=ot[1]
        if (ot[1]=='inconclusive') {
          boundstest_result='no cointegration'
          if(all(ot[-1]=='cointegration')) boundstest_result='cointegration'
          
        }
      }
    }
    
    # was initially inconclusive, but now is conclusive
    
    return(boundstest_result)
    
    if (boundstest_result=='cointegration') {
      m<-run_ardl(type='ardl-ec')
      #re-estimate & move down?
      info_msg=paste0('g) Is there autocorrelation in the residuals? ', ifelse(m$autocorrelation==T, 'yes', 'no'), '; p = ', m$autocorrel_test$bg$p.value, '; model choice: ', m$m.choice, '.')
      return(paste0('OK; market ', mid, '. ', info_msg, 'cointegration; initial bounds: ', initial_bounds))
      
    }
    
    
    if (boundstest_result=='no cointegration') {
      
      # Model specification
      formula=as.formula(paste0('d', dv,'~1+', paste(c(vars, quarter_vars), collapse='+')))
      
      levels = c(quarter_vars, vars)
      
      possible_grid = expand.grid(1, 0:3, 0:3, 0:3)
      start_values=colMins(possible_grid)

      lags_list = apply(possible_grid, 1, function(x) {
        obj=list()
        for (xi in seq(along=x)) obj[[xi]]<-start_values[xi]:x[xi]
        names(obj) = c(paste0('d',dv),vars)
        obj=obj[!unlist(lapply(obj, function(x) all(x==0)))]
        obj=lapply(obj, function(x) x[!x==0])
        
      })
      
      diffs = list() 
      
      lagdiffs = list()
      
      # pick number of lags for Xs
      models=lapply(lags_list, function(lags) {
        mx<-dynardl(formula, data=dat, lags = lags, diffs = diffs,
                    lagdiffs = lagdiffs, levels=levels, ec = FALSE, simulate = FALSE, trend = use_trend)
        autocorrel_test=dynardl.auto.correlated(mx, object.out=T)
        autocorrel_test$bg$p.value
        
        list(bic=BIC(mx$model), model=mx, autocorrel_p=autocorrel_test$bg$p.value, lags = lags)
      })
      
      # choose model with best bic to determine lag numbers
      bics=unlist(lapply(models, function(x) x$bic))
      m.choice = which(bics==min(bics))
      
      
      possible_grid = expand.grid(1:6, unlist(models[[m.choice]]$lags)[-1])
      start_values=colMins(possible_grid)
      
      lags_list = apply(possible_grid, 1, function(x) {
        obj=list()
        for (xi in seq(along=x)) obj[[xi]]<-start_values[xi]:x[xi]
        names(obj) = names(models[[m.choice]]$lags)
        obj=obj[!unlist(lapply(obj, function(x) all(x==0)))]
        obj=lapply(obj, function(x) x[!x==0])
        
      })
      
      # pick number of lags for Xs
      models=lapply(lags_list, function(lags) {
        mx<-dynardl(formula, data=dat, lags = lags, diffs = diffs,
                    lagdiffs = lagdiffs, levels=levels, ec = FALSE, simulate = FALSE, trend = use_trend)
        autocorrel_test=dynardl.auto.correlated(mx, object.out=T)
        autocorrel_test$bg$p.value
        
        list(bic=BIC(mx$model), model=mx, autocorrel_p=autocorrel_test$bg$p.value, lags = lags)
      })
      
      # choose model with best bic to determine lag numbers
      bics=unlist(lapply(models, function(x) x$bic))
      autocorrel=unlist(lapply(models, function(x) x$autocorrel_p))
      
      if (length(which(autocorrel>.1))==0) return(paste0(mid, ': cannot remove autocorrel in first-differenced ARDL (no cointegration)'))
      
      
      # choose model without autocorrel & highest BIC
      m.choice = match(min(bics[autocorrel>.1]), bics)
      
      m=models[[m.choice]]$model
      
      # check for autocorrelation
      plot(m$model$residuals)
      
      autocorrel_test=dynardl.auto.correlated(m, object.out=T)
      autocorrel_test$bg$p.value
      
      
      autocorrelation=autocorrel_test$bg$p.value<.1
      
      
      info_msg=paste0('g) Is there autocorrelation in the residuals? ', ifelse(autocorrelation==T, 'yes', 'no'), '; p = ', autocorrel_test$bg$p.value, '; model choice: ', m.choice, '; bounds: ', boundstest_result, '; initial bounds: ', initial_bounds)
      
      return(paste0('OK ', mid, '. ', info_msg))
      
      
    }  
    
    
  }
  
  return('stationarity in DV')
  
  
}

