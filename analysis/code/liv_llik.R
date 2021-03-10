
library(mvtnorm)

map_pars <- function(pars, levels = 2) {
  beta0 = pars[1]
  beta1 = pars[2]
  
  # Covariance/correlation structure
  uchol = diag(2)
  uchol[1,1] <- pars[3]
  uchol[1,2] <- pars[4]
  uchol[2,2] <- pars[5]
  
  sigma <- crossprod(uchol)
  
  # class memberships
  istart=6
  
  .prob = exp(c(0, pars[seq(from=istart,length.out=levels-1)]))
  prob = .prob/sum(.prob)
  
  istart = istart+levels-1
  
  .lambdas = pars[seq(from=istart, length.out=levels)] 
  lambdas = cumsum(c(.lambdas[1], exp(.lambdas[-1])))
  
  return(list(beta0=beta0, beta1=beta1, uchol=uchol, sigma=sigma, prob=prob, lambdas=lambdas))
}

reps=100
set.seed(1234)
N=try(nrow(data[[1]]$data), silent=T)
if (class(N)=='try-error') N=120

simvals = matrix(runif(N*reps),ncol=100)


llik <- function (params, levels = 2) {
  
  pars=map_pars(params, levels = levels)
  
  lambdas = pars$lambdas
  beta0=pars$beta0
  beta1=pars$beta1
  
  prob=pars$prob
  varcov=pars$sigma

  y_pred = sapply(lambdas, function(lambda) beta0 + beta1 * lambda)
  
  liks = sapply(seq(from=1, to=levels), function(l) {
    dmvnorm(cbind(y-y_pred[l], x-lambdas[l]), mean=c(0,0), sigma=varcov, log=T)
  })
  
  
  lprob = matrix(rep(log(prob),each=length(y)),ncol=levels)
  
  max.AB = apply(lprob+liks,1,max)
  
  llik_min_maxab = apply(liks, 2, function(x) exp(x- max.AB))
  
  prob_times_exp = sapply(seq(from=1, to=levels), function(i) prob[i]*llik_min_maxab[,i])
  
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