library(REndo)

latentIV

REndo:::latentIV_LL





function (formula, data, start.params = c(), optimx.args = list(), 
          verbose = TRUE) 
{
  cl <- match.call()
  check_err_msg(checkinput_latentIV_data(data = data))
  check_err_msg(checkinput_latentIV_formula(formula = formula))
  check_err_msg(checkinput_latentIV_dataVSformula(formula = formula, 
                                                  data = data))
  check_err_msg(checkinput_latentIV_startparams(start.params = start.params, 
                                                formula = formula))
  check_err_msg(checkinput_latentIV_optimxargs(optimx.args = optimx.args))
  check_err_msg(checkinput_latentIV_verbose(verbose = verbose))
  F.formula <- as.Formula(formula)
  mf <- model.frame(formula = F.formula, data = data)
  vec.data.y <- model.response(mf)
  vec.data.endo <- model.part(object = F.formula, data = mf, 
                              lhs = 0, rhs = 1, drop = TRUE)
  m.data.mvnorm <- cbind(vec.data.y, vec.data.endo)
  use.intercept <- as.logical(attr(terms(F.formula), "intercept"))
  name.intercept <- "(Intercept)"
  if (is.null(start.params)) {
    if (verbose) 
      message("No start parameters were given. The linear model ", 
              deparse(formula(F.formula, lhs = 1, rhs = 1)), 
              " is fitted to derive them.")
    res.lm.start.param <- lm(F.formula, data)
    if (anyNA(coef(res.lm.start.param))) 
      stop("The start parameters could not be derived by fitting a linear model.", 
           call. = FALSE)
    start.vals.main.model <- coef(res.lm.start.param)
  }
  else {
    start.vals.main.model <- start.params
  }
  name.endo.param <- setdiff(names(start.vals.main.model), 
                             name.intercept)
  names.main.model <- if (use.intercept) 
    c(name.intercept, name.endo.param)
  else name.endo.param
  start.vals.main.model <- setNames(start.vals.main.model[names.main.model], 
                                    names.main.model)
  start.vals.support.params <- c(pi1 = mean(vec.data.endo), 
                                 pi2 = mean(vec.data.endo) + sd(vec.data.endo), theta5 = 0.5, 
                                 theta6 = 1, theta7 = 0.5, theta8 = 1)
  names.support.params <- names(start.vals.support.params)
  optimx.start.params <- c(start.vals.main.model, start.vals.support.params)
  if (verbose) {
    str.brakets <- paste0("(", paste(names(optimx.start.params), 
                                     "=", round(optimx.start.params, 3), collapse = ", ", 
                                     sep = ""), ")")
    message("The start parameters c", str.brakets, " are used for optimization.")
  }
  optimx.name.endo.param <- make.names(name.endo.param)
  optimx.name.intercept <- make.names(name.intercept)
  optimx.default.args <- alist(par = optimx.start.params, fn = latentIV_LL, 
                               m.data.mvnorm = m.data.mvnorm, use.intercept = use.intercept, 
                               name.intercept = name.intercept, name.endo.param = name.endo.param, 
                               method = "Nelder-Mead", hessian = TRUE, itnmax = 10000, 
                               control = list(trace = 0, dowarn = FALSE))
  optimx.call.args <- modifyList(optimx.default.args, val = optimx.args, 
                                 keep.null = FALSE)
  res.optimx <- tryCatch(expr = do.call(what = optimx, args = optimx.call.args), 
                         error = function(e) {
                           return(e)
                         })
  if (is(res.optimx, "error")) 
    stop("Failed to optimize the log-likelihood function with error '", 
         res.optimx$message, "'. Please revise your start parameter and data.", 
         call. = FALSE)
  optimx.estimated.params <- coef(res.optimx)[1, ]
  all.estimated.params <- setNames(optimx.estimated.params[c(make.names(names.main.model), 
                                                             names.support.params)], c(names.main.model, names.support.params))
  all.estimated.params["theta5"] <- exp(all.estimated.params["theta5"])/(1 + 
                                                                           exp(all.estimated.params["theta5"]))
  names.hessian <- names(all.estimated.params)
  m.hessian <- attr(res.optimx, "details")[, "nhatend"][[1]]
  if (length(m.hessian) == 1 & all(is.na(m.hessian))) {
    m.hessian <- matrix(data = NA_real_, nrow = length(names.hessian), 
                        ncol = length(names.hessian))
    warning("Hessian could not be derived. Setting all entries to NA.", 
            immediate. = TRUE)
  }
  rownames(m.hessian) <- colnames(m.hessian) <- names.hessian
  vec.diag <- rep(1, times = length(all.estimated.params))
  names(vec.diag) <- names(all.estimated.params)
  opt.t5 <- optimx.estimated.params["theta5"]
  vec.diag["theta5"] <- exp(opt.t5)/((exp(opt.t5) + 1)^2)
  m.delta.diag <- diag(vec.diag)
  rownames(m.delta.diag) <- colnames(m.delta.diag) <- names(vec.diag)
  if (use.intercept) 
    fitted <- all.estimated.params[[name.intercept]] * 1 + 
    all.estimated.params[[name.endo.param]] * vec.data.endo
  else fitted <- all.estimated.params[[name.endo.param]] * 
    vec.data.endo
  fitted <- as.vector(fitted)
  names(fitted) <- names(vec.data.y)
  residuals <- as.vector(vec.data.y - fitted)
  names(residuals) <- names(vec.data.y)
  res <- new_rendo_latent_IV(call = cl, F.formula = F.formula, 
                             mf = mf, start.params = optimx.start.params, coefficients = all.estimated.params, 
                             m.delta.diag = m.delta.diag, names.main.coefs = names.main.model, 
                             res.optimx = res.optimx, hessian = m.hessian, fitted.values = fitted, 
                             residuals = residuals)
  return(res)
}
<bytecode: 0x7ff13a2bdc10>
  <environment: namespace:REndo>
  > 