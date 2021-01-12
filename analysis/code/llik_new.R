llik <- function (params, m.data.mvnorm, use.intercept, name.intercept, 
          name.endo.param) 
{
  if (use.intercept) {
    b00 <- params[name.intercept]
    a1 <- params[name.endo.param]
  }
  else {
    b00 <- 0
    a1 <- params[name.endo.param]
  }
  pt <- exp(params["theta5"])
  pt <- pt/(1 + pt)
  
  if(0){
  m.sigma <- matrix(c(params["theta6"], params["theta7"], params["theta7"], 
                      params["theta8"]), byrow = TRUE, ncol = 2, nrow = 2)
  s2e <- m.sigma[1, 1]
  s2v <- m.sigma[2, 2]
  sev <- m.sigma[1, 2]
  varcov <- matrix(0, 2, 2)
  varcov[1, 1] <- a1 * a1 * s2v + 2 * a1 * sev + s2e
  varcov[2, 1] <- a1 * s2v + sev
  varcov[1, 2] <- varcov[2, 1]
  varcov[2, 2] <- s2v
  #print(varcov)
  #stop('max')
  }
  
  # if(0){
  uchol = diag(2)
  uchol[1,1] <- params['theta6']
  uchol[1,2] <- params['theta7']
  uchol[2,2] <- params['theta8']
  
  varcov <- crossprod(uchol)
  #}
  #print(varcov)
  
  pi1 <- params["pi1"]
  pi2 <- params["pi2"]
  
  
  if(0){
    
  mu1 <- matrix(data = c(b00 + a1 * pi1, pi1), nrow = 2, ncol = 1)
  mu2 <- matrix(c(b00 + a1 * pi2, pi2), nrow = 2, ncol = 1)
  log.pdf1 <- dmvnorm(m.data.mvnorm, mean = mu1, sigma = varcov, 
                      log = TRUE)
  log.pdf2 <- dmvnorm(m.data.mvnorm, mean = mu2, sigma = varcov, 
                      log = TRUE)
  max.AB <- pmax(log(pt) + log.pdf1, log(1 - pt) + log.pdf2)
  logLL.lse <- sum(max.AB + log(pt * exp(log.pdf1 - max.AB) + 
                                  (1 - pt) * exp(log.pdf2 - max.AB)))
  
  }
  
 # if(0){
  lambdas = c(pi1, pi2)
  beta0 = b00
  beta1 = a1
  prob=pt
  
  
  y_pred1 = beta0 + beta1 * lambdas[1]
  y_pred2 = beta0 + beta1 * lambdas[2]
  
  #sigma = diag(2)
  
  llik1 = dmvnorm(cbind(y-y_pred1, x-lambdas[1]), mean=c(0,0), sigma=varcov, log=T)
  llik2 = dmvnorm(cbind(y-y_pred2, x-lambdas[2]), mean=c(0,0), sigma=varcov, log=T)
  
  #llik2 = dmvnorm(cbind(y, x),mean=matrix(c(y_pred2,lambdas[2]), nrow=2, ncol=1), sigma=varcov, log=T)
  
  #llik1 <- dmvnorm(m.data.mvnorm, mean = c(y_pred1, lambdas[1]), sigma = varcov, 
  #                    log = TRUE)
  #llik2 <- dmvnorm(m.data.mvnorm, mean = c(y_pred2, lambdas[2]), sigma = varcov, 
  #                    log = TRUE)
  
  #llik = dnorm(y-y_pred, log=T)
  #llik1 = dnorm(y-y_pred, log=T)
  #llik2 = dnorm(x-x_pred, log=T)
  max.AB = pmax(log(prob) + llik1, log(1-prob) + llik2)
  
  llik_lse = sum(max.AB + log(prob * exp(llik1 - max.AB) + (1-prob) * exp(llik2-max.AB)))
  #print(-sum(llik))
 # stop('error')
  return(-llik_lse)
  
#}
  
  
  return(-1 * logLL.lse)
}