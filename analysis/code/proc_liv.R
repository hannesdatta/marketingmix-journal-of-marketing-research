# Build data
get_liv_data <- function(id) {
  dt = data.table(brand_panel[brand_id==id])
  setorder(dt, category,country,brand,date)
  dv = 'usales'
  dt[, ldv := c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  dt[, ddv := get(dv)-c(NA,get(dv)[-.N]), by = c('category','country','brand')]
  
  vars = c('rwpspr','llen','wpswdst')
  vars = vars[unlist(lapply(dt[, vars, with=F], use_ts))]
  
  controls_diffs='^comp[_](rwpspr|llen|wpswdst)$'
  controls_curr = 'quarter[1-3]|^holiday|^trend'
  pval = .1
  
  # Configure model type and calibrate lag structure
  control_vars = sapply(c(diffs=controls_diffs,curr=controls_curr), function(ctrls) if (nchar(ctrls)>0) grep(ctrls, colnames(dt),value=T))
  control_vars = lapply(control_vars, function(control_var) control_var[unlist(lapply(dt[, control_var, with=F], use_ts))])
  
  dt[, trend:=trend-mean(trend,na.rm=T)]
  
  vars_delta = paste0('d', c(vars, control_vars$diffs))
  vars_lags = paste0('lag', c(vars, control_vars$lags))
  vars_curr = control_vars$curr
  
  dt[, intercept:=1]
  df <- dt[, c('dusales', 'intercept','rwpspr','llen','wpswdst', 'lagrwpspr', 'lagllen','lagwpswdst',
               'lagusales', 'dcomp_rwpspr', 'dcomp_llen','dcomp_wpswdst',
               'trend','quarter1','quarter2','quarter3','holiday',
               'cop_rwpspr','cop_llen','cop_wpswdst'), with=F]
  
  df <- df[complete.cases(df),]
  
  df_untransformed <- copy(df)
  
  # Rescale for better convergence
  rescale_values <- sapply(grep('intercept', colnames(df), value=T, invert=T), function(x) max(abs(drop(unlist(df[, get(x)])))))
  
  # set rescale values of focal mmix & lag values to same values
  rescale_values['lagllen']=rescale_values['llen']
  rescale_values['lagrwpspr']=rescale_values['rwpspr']
  rescale_values['lagwpswdst']=rescale_values['wpswdst']
  
  for (.v in grep('intercept', colnames(df), value=T, invert=T)) df[, (.v) := get(.v)/rescale_values[.v]]
  
  
  return(list(df_rescaled=df, df_untransformed=df_untransformed, rescale_values=rescale_values))
}


# Likelihood function
liv_llik <- function(pars, endogenous_variables = c('llen','rwpspr', 'wpswdst'), levels = 2, return_llik = T, no_liv = F, df) {
  
  # Covariance matrix of focal equation and endogenous regressors: 1 + endogenous variables (but then only lower diag)
  istart = 1
  
  .mapping = seq(from=istart, length.out=levels * length(endogenous_variables))
  .lambdas = matrix(pars[.mapping],ncol=length(endogenous_variables))
  lambdas = apply(.lambdas, 2, function(x) cumsum(c(x[1], exp(x[-1]))))
  
  istart = max(.mapping) + 1
  
  .mapping = seq(from=istart,length.out=(levels-1)*length(endogenous_variables))
  .prob = matrix(pars[.mapping],ncol=length(endogenous_variables))
  
  prob = apply(rbind(0, .prob), 2, function(x) exp(x)/sum(exp(x)))
  
  if (length(.mapping)>0) istart = max(.mapping) + 1
  
  # Covariance/correlation structure
  .uchol = diag(length(endogenous_variables) + 1)
  cov_nparameters = .5 * (length(endogenous_variables)+1)*(length(endogenous_variables)+2)
  .mapping = seq(from=istart, length.out = cov_nparameters)
  .uchol[upper.tri(.uchol,diag=T)] <- pars[.mapping]
  sigma <- crossprod(.uchol)
  
  # coefficients of endogenous variables
  istart = max(.mapping) + 1
  
  .mapping = seq(from=istart, length.out=length(endogenous_variables))
  gamma = pars[.mapping]
  
  #remaining parameters
  istart = max(.mapping) + 1
  .mapping = seq(from=istart, length.out=length(pars)-istart+1)
  betas = pars[.mapping]
  
  if (no_liv==T) {
    
    
    y_pred = df$intercept * betas[1] + 
      betas[2] * df$dcomp_llen + betas[3] * df$dcomp_rwpspr + betas[4] * df$dcomp_wpswdst +
      betas[5] * df$lagusales + 
      betas[6] * df$lagllen + betas[7] * df$lagrwpspr + betas[8] * df$lagwpswdst +
      betas[9] * df$trend + betas[10] * df$quarter1 + betas[11] * df$quarter2 + betas[12] * df$quarter3 +
      betas[13] * df$holiday + 
      gamma[1] * df$llen - gamma[1] * df$lagllen + # --> gamma[1] * df$dllen
      gamma[2] * df$rwpspr - gamma[2] * df$lagrwpspr +
      gamma[3] * df$wpswdst - gamma[3] * df$lagwpswdst 
    
    se = .uchol[1]^2
    
    return(-sum(dnorm(df$dusales-y_pred, mean = 0, sd = se , log=T)))
  }
  
  iterating_lambdas = as.matrix(expand.grid(split(drop(lambdas), rep(1:length(endogenous_variables),each=levels))))
  
  iterating_probabilities = apply(expand.grid(split(drop(prob), rep(1:length(endogenous_variables),each=levels))),1,prod)
  
  liks = sapply(seq(from=1, to=length(iterating_probabilities)), function(l) {
    .lambda= iterating_lambdas[l,]
    
    y_pred = df$intercept * betas[1] + 
      betas[2] * df$dcomp_llen + betas[3] * df$dcomp_rwpspr + betas[4] * df$dcomp_wpswdst +
      betas[5] * df$lagusales + 
      betas[6] * df$lagllen + betas[7] * df$lagrwpspr + betas[8] * df$lagwpswdst +
      betas[9] * df$trend + betas[10] * df$quarter1 + betas[11] * df$quarter2 + betas[12] * df$quarter3 +
      betas[13] * df$holiday + 
      gamma[1] * .lambda[1] - gamma[1] * df$lagllen +
      #gamma[2] * df$rwpspr - gamma[2] * df$lagrwpspr +
      #gamma[3] * df$wpswdst - gamma[3] * df$lagwpswdst 
      gamma[2] * .lambda[2] - gamma[2] * df$lagrwpspr +
      gamma[3] * .lambda[3] - gamma[3] * df$lagwpswdst 
    
    
    .tmp = cbind(df$dusales-y_pred, cbind(df$llen, df$rwpspr, df$wpswdst) - matrix(rep(.lambda,length(y_pred)),ncol=length(endogenous_variables), byrow=T))
    #.tmp = cbind(df$dusales-y_pred, (cbind(df$llen, df$rwpspr, df$wpswdst) - matrix(rep(.lambda,length(y_pred)),ncol=length(endogenous_variables), byrow=T))[,1])
    
    
    dmvnorm(.tmp, mean=rep(0, each=length(endogenous_variables)+1), sigma=sigma, log=T)
    #dmvnorm(.tmp, mean=rep(0, each=2), sigma=sigma[1:2,1:2], log=T)
    
  })
  
  # likelihood (L, see Biernacki et al. 2000, formula 2.2) (?)
  lprob = matrix(rep(log(iterating_probabilities),each=nrow(df)),ncol=length(iterating_probabilities))
  lprob_liks = lprob + liks
  
  # complete likelihood (CL, see Biernacki et al. 2000, formula 2.3) (?)
  max.AB = apply(lprob_liks,1,max)
  llik_min_maxab = apply(liks, 2, function(x) exp(x- max.AB))
  
  prob_times_exp = sapply(seq(from=1, to=length(iterating_probabilities)), function(i) iterating_probabilities[i]*llik_min_maxab[,i])
  llik_lse = -sum(max.AB + log(rowSums(prob_times_exp)))
  
  if (no_liv==F) {
    if (return_llik==T) {
      return(llik_lse) } else {
        return(list(neg_llik=llik_lse, sigma=sigma, lambdas=lambdas, prob=prob, gamma=gamma, betas=betas))
      }
  }
  
  
}



ols_ll <-  function(pars, df) {
  betas = pars[1:13]
  gamma = pars[14:16]
  se = exp(pars[17])
  
  y_pred = df$intercept * betas[1] + 
    betas[2] * df$dcomp_llen + betas[3] * df$dcomp_rwpspr + betas[4] * df$dcomp_wpswdst +
    betas[5] * df$lagusales + 
    betas[6] * df$lagllen + betas[7] * df$lagrwpspr + betas[8] * df$lagwpswdst +
    betas[9] * df$trend + betas[10] * df$quarter1 + betas[11] * df$quarter2 + betas[12] * df$quarter3 +
    betas[13] * df$holiday + 
    gamma[1] * df$llen - gamma[1] * df$lagllen +
    gamma[2] * df$rwpspr - gamma[2] * df$lagrwpspr +
    gamma[3] * df$wpswdst - gamma[3] * df$lagwpswdst 
  
  return(-sum(dnorm(df$dusales-y_pred, mean = 0, sd = se , log=T)))
}
