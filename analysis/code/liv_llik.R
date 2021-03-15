
library(mvtnorm)

map_pars <- function(pars, levels = 2, endogenous_variables = 1) {
  
  # Parameters
  
  # Means of latent instruments --> levels x endogeneous variables
  # Class probabilities of latent instruments --> (levels-1) x endogenous variables
  # Beta coefficients of effect of LIV on outcome variable
  
  # Covariance matrix of focal equation and endogenous regressors: 1 + endogenous variables (but then only lower diag)
  istart = 1
  
  .mapping = seq(from=istart, length.out=levels * endogenous_variables)
  .lambdas = matrix(pars[.mapping],ncol=endogenous_variables)
  lambdas = apply(.lambdas, 2, function(x) cumsum(c(x[1], exp(x[-1]))))
  
  istart = max(.mapping) + 1
  
  
  .mapping = seq(from=istart,length.out=(levels-1)*endogenous_variables)
  .prob = matrix(pars[.mapping],ncol=endogenous_variables)
  
  prob = apply(rbind(0, .prob), 2, function(x) exp(x)/sum(exp(x)))
  
  istart = max(.mapping) + 1
  
  # Covariance/correlation structure
  .uchol = diag(endogenous_variables + 1)
  cov_nparameters = .5 * (endogenous_variables+1)*(endogenous_variables+2)
  .mapping = seq(from=istart, length.out = cov_nparameters)
  .uchol[upper.tri(.uchol,diag=T)] <- pars[.mapping]
  sigma <- crossprod(.uchol)
  
  
  # coefficients of endogenous variables
  istart = max(.mapping) + 1
  
  .mapping = seq(from=istart, length.out=endogenous_variables)
  gamma = pars[.mapping]
  
  #remaining parameters
  istart = max(.mapping) + 1
  .mapping = seq(from=istart, length.out=length(pars)-istart+1)
  betas = pars[.mapping]
  
  return(list(betas=betas, gamma=gamma, 
              sigma = sigma,
              prob = prob,
              lambdas = lambdas,
              uchol=.uchol, prob_untransformed=.prob,
              lambdas_untransformed = .lambdas))
}

reps=100
set.seed(1234)
N=try(nrow(data[[1]]$data), silent=T)
if (class(N)=='try-error') N=120

simvals = matrix(runif(N*reps),ncol=100)


llik <- function(params, levels = 2, endogenous_variables = 2, data = list(y=y,X=cbind(rep(1,length(y))),
                                                                            endog=cbind(x,x))) {
  
  pars=map_pars(params, levels = levels, endogenous_variables = endogenous_variables)
  
  lambdas = pars$lambdas
  betas=pars$betas[1]
  
  gamma=pars$gamma
  
  prob=pars$prob
  varcov=pars$sigma

  iterating_lambdas = as.matrix(expand.grid(split(drop(lambdas), rep(1:endogenous_variables,each=levels))))
  
  iterating_probabilities = apply(expand.grid(split(drop(prob), rep(1:endogenous_variables,each=levels))),1,prod)
  
  
  
  liks = sapply(seq(from=1, to=length(iterating_probabilities)), function(l) {
    .lambda= iterating_lambdas[l,]
    y_pred = data$X%*%betas + drop(t(cbind(.lambda))%*%cbind(gamma))
    
    .tmp = cbind(y-y_pred, data$endog - matrix(rep(.lambda,nrow(data$endog)),ncol=endogenous_variables, byrow=T))
    
    dmvnorm(.tmp, mean=rep(0, each=endogenous_variables+1), sigma=varcov, log=T)
  })
  
  
  lprob = matrix(rep(log(iterating_probabilities),each=length(y)),ncol=length(iterating_probabilities))
  
  max.AB = apply(lprob+liks,1,max)
  
  llik_min_maxab = apply(liks, 2, function(x) exp(x- max.AB))
  
  prob_times_exp = sapply(seq(from=1, to=length(iterating_probabilities)), function(i) iterating_probabilities[i]*llik_min_maxab[,i])
  
  #llik_lse = sum(max.AB + log(prob[1] * exp(llik1 - max.AB) + prob[2] * exp(llik2-max.AB)))
  #llik_lse = sum(max.AB + log(prob[1] * llik_min_maxab[,1] + prob[2] * llik_min_maxab[,2]))
  
  llik_lse = sum(max.AB + log(rowSums(prob_times_exp)))
  
  
  #llik_lse = sum(max.AB + log(sum()
  
  #llik_lse = sum(max.AB + log(sum(sapply(seq(from=1, to=levels), function(i) prob[i]+llik_min_maxab[,i]))))
                                
  
  return(-llik_lse)
}

# Extensions: 
# - multiple endogenous variables
# - multiple classes
# - "exp" trick

# EC
# --
# Gaussian Copulas in EC model
# Use our data, similar model

# Step 1:
# -> regression model with lagged DV + copula
# -> regression model with 