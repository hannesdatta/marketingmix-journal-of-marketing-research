
library(mvtnorm)

map_pars <- function(pars, reparametrization = FALSE) {
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
  
  lambdas = pars[7:8] 
  if (reparametrization==T) lambdas = c(pars[7], pars[7]+exp(pars[8]))  
  

  return(list(beta0=beta0, beta1=beta1, uchol=uchol, sigma=sigma, prob=prob, lambdas=lambdas))
}

reps=100
set.seed(1234)
N=try(nrow(data[[1]]$data), silent=T)
if (class(N)=='try-error') N=120

simvals = matrix(runif(N*reps),ncol=100)


llik <- function (params, sim=FALSE, reparametrization = FALSE) {
  
  pars=map_pars(params, reparametrization = reparametrization)
  
  lambdas = pars$lambdas
  beta0=pars$beta0
  beta1=pars$beta1
  
  prob=pars$prob
  varcov=pars$sigma

  
  if (sim==T) {
 
    y_y_pred = y - (beta0 + beta1 * ifelse(simvals < prob, lambdas[1], lambdas[2]))
    x_x_pred = x - (ifelse(simvals < prob, lambdas[1], lambdas[2]))
    
    llik = sapply(1:nrow(y_y_pred), function(i) log(mean(dmvnorm(cbind(y_y_pred[i,], x_x_pred[i,]), mean=c(0,0), sigma=varcov, log=F))))
    return(-sum(llik))
    
    #llik = sapply(1:reps, function(i) dmvnorm(cbind(y_y_pred[,i], x_x_pred[,i]), mean=c(0,0), sigma=varcov, log=T))
    #return(-sum(apply(llik, 1, function(x) log(mean(exp(x-max(x)))))))
    
   # sllik = colSums(llik)
    #return(-log(mean(exp(sllik-max(sllik)))))
    
    
    #-log(mean(exp(llik_vals-max(llik_vals))))
    
    
  }
  
  
  # first: move this to simulated maximum likelihood
  y_pred1 = beta0 + beta1 * lambdas[1]
  y_pred2 = beta0 + beta1 * lambdas[2]
  
  llik1 = dmvnorm(cbind(y-y_pred1, x-lambdas[1]), mean=c(0,0), sigma=varcov, log=T)
  llik2 = dmvnorm(cbind(y-y_pred2, x-lambdas[2]), mean=c(0,0), sigma=varcov, log=T)
  
  max.AB = pmax(log(prob) + llik1, log(1-prob) + llik2)
  
  llik_lse = sum(max.AB + log(prob * exp(llik1 - max.AB) + (1-prob) * exp(llik2-max.AB)))
  
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