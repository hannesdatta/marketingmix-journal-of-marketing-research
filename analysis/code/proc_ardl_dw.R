get_lagdiffs_list <- function(gridlist = list(), namesarg, constraints = NULL) {
  possible_grid <- expand.grid(gridlist) # 0:3, 0:3, 0:3, 0:3)
  start_values <- colMins(possible_grid)
  
  if (!is.null(constraints)) {
    for (constr in constraints)
      possible_grid <- possible_grid[with(possible_grid, eval(parse(text=constr))),]
    
  }
  
  lagdiffs_list <- apply(possible_grid, 1, function(x) {
    
    obj=mapply(function(arg, startval, gridval) {
      sapply(arg, function(x) startval:gridval, simplify=F)
    }, namesarg, start_values, x, SIMPLIFY=F, USE.NAMES=F)
    obj = do.call('c', obj)
    
    obj <- obj[!unlist(lapply(obj, function(x) all(x == 0)))]
    obj <- lapply(obj, function(x) x[!x == 0])
  })
  return(lagdiffs_list)
}


# see https://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot
assert <- function(expr, error) {
  if (!expr) stop(error, call. = FALSE)
}

dt <- brand_panel[brand_id==18]
maxlag <- 6
pval <- .1
adf_tests <- NULL
exclude_cointegration <- NULL

ardl <- function(type = "ardl-ec", dt, dv, vars, exclude_cointegration = NULL, controls = NULL, adf_tests = NULL, maxlag = 6, 
                 maxpq = 3, pval = .1, ...) {
  # coint: variables to be included in cointegration

  tmp <- apply(dt[, vars, with = F], 2, function(x) all(x %in% c(0, 1)))
  dummies <- names(tmp)[tmp == T]
  xvars <- setdiff(vars, dummies)
  xvars_without_cointegration <- setdiff(xvars, exclude_cointegration)

  assert("data.table" %in% class(dt), "dt needs to be of class dt.table")
  assert("data.table" %in% class(adf_tests) | is.null(adf_tests), "please supply ADF_tests as class dt.table")
  assert(!all(dt[, dv, with = F] %in% 0:1), "DV needs to be continuous, i.e., not a dummy variable")
  assert(all(exclude_cointegration %in% vars), "All variables specified in *exclude_cointegration* need to be part of *vars*")

  # run ADF tests if not done before
  if (is.null(adf_tests)) {
    adf_tests <- rbindlist(lapply(c(dv, xvars, controls), function(.var) {
      return(data.frame(variable = .var, cbind(t(adf_enders(unlist(dt[, .var, with = F]), maxlag = maxlag, pval = pval)))))
    }))
  }
  
  if (!is.null(controls)) {
    controls_diffs = as.character(adf_tests[variable%in%controls&ur==1]$variable)
    controls_levels = as.character(adf_tests[variable%in%controls&ur==0]$variable)
  }else{
    controls_diffs=NULL
    controls_levels=NULL
  }
  
    
  use_trend = as.logical(adf_tests[variable==dv]$trend)
  
  # Estimate ARDL in error correction form (case e, Philips 2018)
  if (type == "ardl-ec") {
    formula <- as.formula(paste0(dv, "~1+", paste(c(xvars, dummies, controls), collapse = "+")))
    lags_list <- lapply(c(dv, xvars_without_cointegration), function(x) 1)
    names(lags_list) <- c(dv, xvars_without_cointegration)
    diffs <- c(xvars, controls_diffs)
    levels <- c(dummies, controls_levels)
    ec <- TRUE
    tmp <- get_lagdiffs_list(rep(list(0:maxpq), 2), list(dv, xvars))
    lagstructure <- lapply(tmp, function(x) list(lags = lags_list, lagdiff = x))
  }

  if (type == "ardl-levels") {
    xvars_levels <- as.character(adf_tests[variable %in% xvars & order == 0]$variable)
    xvars_firstdiff <- setdiff(xvars, xvars_levels)

    formula <- as.formula(paste0(dv, "~1+", paste(c(xvars, dummies, controls), collapse = "+")))

    levels <- c(xvars_levels, dummies, controls_levels)
    diffs <- c(xvars_firstdiff, controls_diffs)
    
                                 # lags            # x variables in level
    tmp <- get_lagdiffs_list(list(1:maxpq, 0:maxpq), list(dv, xvars))
    lagstructure <- lapply(tmp, function(x) list(lags = x[names(x) %in% c(dv, xvars_levels)], 
                                                 lagdiff = x[names(x) %in% xvars_firstdiff]))
    ec <- FALSE
  }

  if (type == "ardl-firstdiff") {
    dv = paste0('d', dv)
    formula <- as.formula(paste0(paste0("", dv), "~1+", paste(c(xvars, dummies, controls), collapse = "+")))
    levels <- c(xvars, dummies, controls_levels)
    diffs <- c(xvars, controls_diffs)# NULL # xvars

    ec <- FALSE
    
                                  #DV  #lags #lags of differences
    tmp <- get_lagdiffs_list(list(p=1:maxpq, l=0:maxpq,  q=0:maxpq), list(dv, xvars, paste0("_", xvars)), constraints=list('l<q|l==0&q==0'))

    lagstructure <- lapply(tmp, function(x) {
      lagdiff <- x[names(x) %in% c(paste0("_", xvars))]
      names(lagdiff) <- gsub("[_]", "", names(lagdiff))

      out <- list(lags = x[names(x) %in% c(dv, paste0("", xvars))], lagdiff = lagdiff)
      return(out)
    })
  }

  # Estimate models with varying lag terms
  cnt=0
  models <- lapply(lagstructure, function(.lagstructure) {
    #cnt<<-cnt+1
    #print(.lagstructure)
    #print(cnt)
    log <- capture.output({
      mx <- try(dynardl(formula,
        data = dt, lags = .lagstructure$lags, diffs = c(diffs),
        lagdiffs = .lagstructure$lagdiff, levels = c(levels), ec = ec, simulate = FALSE, trend = use_trend#,
        #noLDV = ifelse(type=='ardl-firstdiff', T, F)
      ), silent=T)
  
      #if (class(mx)!='try-error') {
      autocorrel_test_bg <- try(dynardl.auto.correlated(mx, object.out = T),
                             silent=T)
      autocorrel_test_bg_p <- NA
      if (class(autocorrel_test_bg)!='try-error') autocorrel_test_bg_p = autocorrel_test$bg$p.value
      
      autocorrel_test_dw = try(dwtest(mx$model), silent = T)
      autocorrel_test_dw_p <- NA
      
      if (class(autocorrel_test_dw)!='try-error') autocorrel_test_dw_p = dwtest(mx$model)$p.value
      
    })

    list(bic = BIC(mx$model), model = mx, autocorrel_bg_p = autocorrel_test_bg_p,
         autocorrel_dw_p = autocorrel_test_dw_p)
  })

  # choose the one with lowest BIC & no auto correlation
  bics <- unlist(lapply(models, function(x) x$bic))
  autocorrel <- unlist(lapply(models, function(x) x$autocorrel_test_bg_p))

  if (length(which(autocorrel > pval)) == 0) {
    return("cannot remove autocorrel")
  }

   # choose model without autocorrel & highest BIC
  m.choice <- match(min(bics[autocorrel > pval]), bics)
  m <- models[[m.choice]]$model

  # check for autocorrelation
  # plot(m$model$residuals)

  autocorrelation <- models[[m.choice]]$autocorrel_p < pval

  res <- list(
    model = m, tested_model_specs = list(
      bic = bics, autocorrel = autocorrel, lagstructure = lagstructure,
      diffs = diffs, levels = levels, ec = ec, formula = formula,
      trend = use_trend
    ),
    autocorrelation = autocorrelation, mchoice = m.choice,
    adf_tests = adf_tests,
    type = type
  )

  if (ec == T & type == "ardl-ec") {
    log <- capture.output({
      boundstest <- pssbounds(m, object.out = T)
      boundstest_result <- "inconclusive"
      if (boundstest$fstat < boundstest$`ftest.I0.p10`) boundstest_result <- "no cointegration"
      if (boundstest$fstat > boundstest$`ftest.I1.p10`) boundstest_result <- "cointegration"
      res$boundstest_result <- boundstest_result
    })
    
    
  }
  class(res) <- 'ardl_procedure'
  return(res)
}

summary.ardl_procedure <- function(object, ...) {
  cat('ARDL Procedure\n=======================\n\n')
  cat('Model type: ', object$type,fill=T)
  cat('\nADF tests: ',fill=T)
  print(object$adf_tests)
  
  cat('\nModel estimates\n')
  cat(paste0('\nDependent variable: ', colnames(object$model$model$model)[1], '\n'))
  print(summary(object$model))
  cat('\n\n')
  
  cat(paste0('Remaining autocorrelation: ', object$autocorrelation))
  
  cat('\n\n')
  
  if(object$tested_model_specs$ec==T & object$type=='ardl-ec') {
    cat(paste0('Bounds test:\n\n'))
    pssbounds(object$model)
  
    cat(paste0('\n\nThe result of the bounds procedure is: ', object$boundstest_result), '\n\n=======================\n')
  }
}