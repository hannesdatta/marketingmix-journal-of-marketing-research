itersur <- function (X, Y, index, method = "FGLS", maxiter = 1000, 
          reltol = 10^-7, to.file = F, use_ginv=F) {
  if (!method %in% c("FGLS", "FGLS-Praise-Winsten")) 
    stop(paste("Invalid method selected: ", method))
  if (!class(X) == "matrix" | !class(Y) == "matrix") 
    stop("X and Y need to be matrices")
  if (!all(order(index[, 2], index[, 1]) == 1:nrow(X))) 
    stop("Data needs to be stacked by brands")
  colnames(index) <- c("date", "brand")
  X = as(X, "dgeMatrix")
  beta_ols = solve(crossprod(X), crossprod(X, Y))
  beta_hat = beta_ols
  obsperbrand = table(index$brand)
  max_t = max(obsperbrand)
  takeouts <- c(sapply(obsperbrand, function(x) seq(from = 1, 
                                                    to = max_t) %in% seq(from = 1, to = x)))
  I = Diagonal(max_t)
  empty_sigma <- sigma <- matrix(double(length(obsperbrand)^2), 
                                 ncol = length(obsperbrand))
  xsplit = split(data.frame(as.matrix(X)), index[, 2])
  ysplit = split(as.matrix(Y), index[, 2])
  xprime = X
  yprime = Y
  deltas <- NULL
  
  for (iter in 1:maxiter) {
    beta_old = beta_hat
    
    if (method == "FGLS-Praise-Winsten") {
      pred = X %*% beta_hat
      resid = Y - pred
      resid_by_brand = dcast(data.frame(index, resid = matrix(resid)), 
                             date ~ brand, value.var = "resid")
      rho_brand = apply(cbind(resid_by_brand[, -1]), 2, 
                        function(x) sum(x[-1] * x[-length(x)], na.rm = T)/sum(x^2, 
                                                                              na.rm = T))
      yprime = matrix(unlist(mapply(praise_winsten, ysplit, 
                                    as.list(rho_brand), SIMPLIFY = FALSE)), ncol = 1)
      xprime = do.call("rbind", mapply(function(x, 
                                                rho) apply(x, 2, praise_winsten, rho = rho), 
                                       xsplit, as.list(rho_brand), SIMPLIFY = FALSE, 
                                       USE.NAMES = FALSE))
      xprime = as(xprime, "dgeMatrix")
    }
    
    pred = xprime %*% beta_hat
    resid = yprime - pred
    resid_by_brand = dcast(data.frame(index, resid = matrix(resid)), 
                           date ~ brand, value.var = "resid")
    rhos = apply(cbind(resid_by_brand[, -1]), 2, function(x) sum(x[-1] * 
                                                                   x[-length(x)], na.rm = T)/sum(x^2, na.rm = T))
    sigma <- empty_sigma
    for (.i in 1:ncol(sigma)) {
      for (.j in 1:ncol(sigma)) {
        resids = cbind(resid_by_brand[, .i + 1], resid_by_brand[, 
                                                                .j + 1])
        compl.cases = complete.cases(resids)
        if (length(which(compl.cases == TRUE)) <= 1) {
          sigma[.i, .j] <- 0
        }
        else {
          tmax = nrow(resids)
          resids = resids[complete.cases(resids), ]
          sigma[.i, .j] <- (1/tmax) * sum(resids[, 1] * 
                                            resids[, 2])
        }
      }
    }
    
    if (use_ginv==T) {
      sigma_inv = try(solve(sigma),silent=T)
      if ((class(sigma_inv))=='try-error') sigma_inv = ginv(sigma)
    } else {
      sigma_inv = solve(sigma)
    }
    
    if (0) {
      inew = NULL
      for (.i in 1:ncol(sigma_inv)) {
        jnew = NULL
        for (.j in 1:ncol(sigma_inv)) {
          zeros = matrix(double(obsperbrand[.i] * obsperbrand[.j]), 
                         nrow = obsperbrand[.i], ncol = obsperbrand[.j])
          diag(zeros) <- sigma_inv[.i, .j]
          jnew = cbind(jnew, zeros)
        }
        inew = rbind(inew, jnew)
      }
      omega_inverse = inew
    }
    omega_inverse = kronecker(sigma_inv, I, make.dimnames = FALSE)[takeouts, 
                                                                   takeouts]
    inv_varcovar = crossprod(xprime, omega_inverse) %*% xprime
    if (use_ginv==T) {
      varcovar = try(solve(inv_varcovar),silent=T)
      if ((class(varcovar))=='try-error') varcovar = ginv(as.matrix(inv_varcovar))
    } else{
      varcovar = solve(inv_varcovar)
    }
    beta_hat = varcovar %*% (crossprod(xprime, omega_inverse) %*% 
                               yprime)
    delta = drop(t(beta_hat - beta_old) %*% inv_varcovar %*% 
                   (beta_hat - beta_old))
    cat("Iteration ", iter, " (Convergence Criteria: ", 
        delta, ").\n")
    if (to.file == T) {
      outbetahat = data.frame(matrix(beta_hat, ncol = length(beta_hat)))
      colnames(outbetahat) <- colnames(xprime)
      outbetahat$iteration = iter
      outbetahat$delta = delta
      appendfile = ifelse(iter == 1, F, T)
      write.table(outbetahat, "iter_out.csv", append = appendfile, 
                  col.names = !appendfile, row.names = F)
    }
    deltas = c(deltas, delta)
    if (delta < reltol) 
      break
  }
  res = new("itersur")
  pred = xprime %*% beta_hat
  resid = yprime - pred
  ses = sqrt(diag(varcovar))
  res@coefficients = data.frame(variable = colnames(X), coef = drop(as.matrix(beta_hat)), 
                                se = drop(as.matrix(ses)), ols = drop(as.matrix(beta_ols)), 
                                row.names = NULL)
  res@coefficients$z <- res@coefficients$coef/res@coefficients$se
  res@coefficients <- res@coefficients[, match(c("variable", 
                                                 "coef", "se", "z"), colnames(res@coefficients))]
  k = ncol(varcovar)
  N = length(resid)
  res@bic = N * log(sum(resid^2)/N) + (k * log(N))
  res@llik = -0.5 * N * log(sum(resid^2)/N)
  res@aic = 2 * k - 2 * res@llik
  res@predicted = as.numeric(pred)
  res@resid = as.numeric(resid)
  res@varcovar = as.matrix(varcovar)
  res@X <- as.matrix(xprime)
  res@y <- as.numeric(yprime)
  res@index <- index
  res@sigma <- as.matrix(sigma)
  res@iterations = iter
  res@delta = deltas
  res@method = method
  res@rho = rhos
  if (method == "FGLS-Praise-Winsten") 
    res@rho_hat = rho_brand
  else res@rho_hat = rep(0, length(obsperbrand))
  return(res)
}