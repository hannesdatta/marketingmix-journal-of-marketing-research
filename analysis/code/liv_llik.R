
library(mvtnorm)

map_pars <- function(pars) {
  beta0 = pars[1]
  beta1 = pars[2]
  
  # Covariance/correlation structure
  uchol = diag(2)
  uchol[1,1] <- pars[3]
  uchol[1,2] <- pars[4]
  uchol[2,2] <- pars[5]
  
  sigma <- crossprod(uchol)
  
  # class memberships
  prob <- exp(pars[6])/(1+exp(pars[6]))
  
  # instrument coefficients
  #lambdas <- double(2)
  #lambdas[1] <- pars[7]
  #lambdas[2] <- pars[7] + exp(pars[8])
  lambdas = pars[7:8]  
  return(list(beta0=beta0, beta1=beta1, uchol=uchol, sigma=sigma, prob=prob, lambdas=lambdas))
}



llik <- function (params) {
  
  pars=map_pars(params)
  
  lambdas = pars$lambdas
  beta0=pars$beta0
  beta1=pars$beta1
  prob=pars$prob
  varcov=pars$sigma
  
  y_pred1 = beta0 + beta1 * lambdas[1]
  y_pred2 = beta0 + beta1 * lambdas[2]
  
  llik1 = dmvnorm(cbind(y-y_pred1, x-lambdas[1]), mean=c(0,0), sigma=varcov, log=T)
  llik2 = dmvnorm(cbind(y-y_pred2, x-lambdas[2]), mean=c(0,0), sigma=varcov, log=T)
  
  max.AB = pmax(log(prob) + llik1, log(1-prob) + llik2)
  
  llik_lse = sum(max.AB + log(prob * exp(llik1 - max.AB) + (1-prob) * exp(llik2-max.AB)))
  
  return(-llik_lse)
  
}