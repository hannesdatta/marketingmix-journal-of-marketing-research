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
    all_order1=1 #all(adf_tests[-1,]$order<=1)
    
    print(paste0('a) All independent variables of order I(1) or lower? ', ifelse(all_order1==1, 'yes', 'no')))
    
    use_trend=as.logical(adf_tests[variable=='lnusales']$trend)
    
    for (maxpq in c(3,6,9,12,15,18)) {
    m<-ardl(type='ardl-ec', dt = dat, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
            adf_tests= adf_tests, maxlag = 6, pval = .1, maxpq = maxpq)
    if (class(m)=='ardl_procedure') break
    }
    
    # chosen lag structure
    #cat('Chosen lag structure:\n')
    #m$tested_model_specs$lagstructure[[m$mchoice]]
    summary(m)
    
    if ('cannot remove autocorrel'%in%m) return(m)
    
    boundstest_result=m$boundstest_result
    
     
    initial_bounds=boundstest_result
    
    if(boundstest_result=='inconclusive') {
       
      # exclude non-UR variables from lag_levels
      i0_vars=as.character(adf_tests[!variable==dv&ur==0]$variable)
      i1_vars=as.character(adf_tests[!variable==dv&ur==1]$variable)
      # combinations
      if(length(i1_vars)>0) {
        i1vars_comb = do.call('c', sapply(1:max(1,length(i1_vars)-1), function(x) combn(i1_vars, x, simplify=F), simplify=F))
        i1vars_comb = lapply(i1vars_comb, function(x) if(all(i1_vars%in%x)) return(NULL) else x)
      } else {
        i1vars_comb = list(NULL)
      }
      i1vars_comb
      # check w/ marnik whether done correctly (NULL in case of only 1 var)
      
      potential_exclusions = c(list(i0_vars), lapply(i1vars_comb, function(x) if(is.null(x)) return(NULL) else return(c(i0_vars,x))))
      
      potential_exclusions=potential_exclusions[unlist(lapply(potential_exclusions, function(x) !is.null(x)))]
      
      if (length(i0_vars)>0) names(potential_exclusions)[1]<-'i0vars'
      if (length(i1vars_comb)>0) names(potential_exclusions)[-1]<-unlist(lapply(i1vars_comb, paste, collapse=','))
      
      #if(length(i0_vars)==0) return(paste0(mid,'; inconclusive: cannot remove stationary regressor'))
      
      #exclusions[]
      
      if (length(potential_exclusions)>0) {
        if(length(potential_exclusions)==1 & all(i0_vars%in%potential_exclusions[[1]])) {
        all_boundstests = 'no cointegration'
        } else {
        ot=lapply(potential_exclusions, function(x) {
          for (maxpq in c(3,6)) {
            me<-ardl(type='ardl-ec', dt = dat, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = x,
                     adf_tests= adf_tests, maxlag = 6, pval = .1, maxpq = maxpq)
            if (class(me)=='ardl_procedure') break
          }
          return(me)
        })
      
      cat(paste0('Running another series of bounds tests, by excluding the following variables (test result in brackets):\n'))
      
      print_excl=paste0('- ', unlist(lapply(potential_exclusions, function(x) paste0(x, collapse= ', '))))
      all_boundstests = unlist(lapply(ot, function(x) x$boundstest_result))
      fused_excl = paste(print_excl, all_boundstests, sep = ': ')
      cat(paste0(fused_excl, collapse='\n'))
      
        }
        
      return(paste0('initially: ', initial_bounds, '; now: ', paste0(all_boundstests, collapse='|')))
        #if (!ot[1]=='inconclusive') boundstest_result=ot[1]
        #if (ot[1]=='inconclusive') {
        #  boundstest_result='no cointegration'
        #  if(all(ot[-1]=='cointegration')) boundstest_result='cointegration'
          
        #}
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

#analyze_brand <- function(x,  ...) stop(paste0('error ', x))