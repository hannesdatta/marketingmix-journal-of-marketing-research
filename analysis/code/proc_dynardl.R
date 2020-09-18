dynardl <- function (formula, data = list(), lags = list(), diffs = c(), 
          lagdiffs = list(), levels = c(), ec = FALSE, trend = FALSE, 
          constant = TRUE, modelout = FALSE, noLDV = FALSE, simulate = FALSE, 
          shockvar = list(), shockval = sd(data[[shockvar]], na.rm = T), 
          time = 10, qoi = "mean", forceset = NULL, range = 20, burnin = 20, 
          sims = 1000, sig = 95, expectedval = FALSE, fullsims = FALSE) 
{
  if (identical(data, list())) {
    temp.data.mat <- NULL
    for (i in 1:length(all.vars(formula))) {
      assign(paste(all.vars(formula)[i]), get(all.vars(formula)[i]))
      temp.data.mat <- cbind(temp.data.mat, get(all.vars(formula)[i]))
    }
    colnames(temp.data.mat) <- all.vars(formula)
    data <- data.frame(temp.data.mat)
  }
  if (length(diffs)) {
    if (isTRUE(is.list(diffs))) {
      stop("Declare differences as a vector of variables to be first-differenced (i.e. diffs = c('X1', ...))")
    }
    else {
      for (i in 1:length(diffs)) {
        if (!(diffs[i] %in% all.vars(formula))) {
          warning(paste(paste("Variable"), paste(diffs[i]), 
                        paste("in the diffs list not found in model formula. Not included in differences."), 
                        sep = " "))
        }
      }
    }
  }
  if (length(lags)) {
    if (!is.list(lags)) {
      stop("'lags' must be a list")
    }
    else {
      if (as.character(formula[[2]]) %in% names(lags)) {
        first.lag.ldv <- FALSE
        for (i in 1:length(names(lags))) {
          if (names(lags)[i] == as.character(formula[[2]])) {
            for (o in 1:length(lags[[i]])) {
              ifelse(lags[[i]][o] == 1, first.lag.ldv <- TRUE, 
                     first.lag.ldv <- first.lag.ldv)
            }
          }
        }
        if (first.lag.ldv == FALSE) {
          if (noLDV == TRUE) {
            warning(paste("ARDL model with no LDV specified. Only finite dynamics are possible through lags of X (which might affect simulations). Are you sure you want this?"))
          }
          else {
            stop("'lags' must include first lag of dependent variable (LDV) (or specify noLDV = TRUE)")
          }
        }
      }
      for (i in 1:length(names(lags))) {
        if (!(names(lags)[i] %in% all.vars(formula))) {
          warning(paste(paste("Variable"), paste(names(lags)[i]), 
                        paste("in the lags list not found in model formula. Not included in lagged levels."), 
                        sep = " "))
        }
      }
    }
  }
  if (length(lagdiffs)) {
    if (!is.list(lagdiffs)) {
      stop("'lagdiffs' must be a list")
    }
    else {
      for (i in 1:length(names(lagdiffs))) {
        if (!(names(lagdiffs)[i] %in% all.vars(formula))) {
          warning(paste(paste("Variable"), paste(names(lagdiffs)[i]), 
                        paste("in the lagdiffs list not found in model formula. Not included in lagged differences."), 
                        sep = " "))
        }
      }
    }
  }
  if (length(levels)) {
    if (isTRUE(is.list(levels))) {
      stop("Declare levels as a vector of variables to be included in levels (i.e. levels = c('X1', ...))")
    }
    if (as.character(formula[[2]]) %in% levels) {
      stop("Dependent variable cannot appear in contemporaneous levels in ARDL model.")
    }
    else {
      for (i in 1:length(levels)) {
        if (!(levels[i] %in% all.vars(formula))) {
          warning(paste(paste("Variable"), paste(levels[i]), 
                        paste("in the levels list not found in model formula. Not included in levels."), 
                        sep = " "))
        }
      }
    }
  }
  if (length(forceset)) {
    if (as.character(formula[[2]]) %in% names(forceset)) {
      stop("LDV cannot be forceset in dynamic simulation.")
    }
    else {
      for (i in 1:length(names(forceset))) {
        if (!(names(forceset)[i] %in% all.vars(formula))) {
          warning(paste(paste("Variable"), paste(names(forceset)[i]), 
                        paste("in the forceset list not found in model formula. Variable not forced."), 
                        sep = " "))
        }
      }
    }
  }
  shock.message <- NULL
  if (simulate == FALSE) {
    if (!(identical(deparse(substitute(shockvar)), "list()"))) {
      if (!(grepl("\"", deparse(substitute(shockvar))))) {
        stop("'shockvar' must be specified in quotation marks.")
      }
      else {
        if (length(shockvar) > 1) {
          stop("You specified more than one shockvar. Only specify one shockvar in a dynamic simulation.")
        }
        if (!(shockvar %in% all.vars(formula))) {
          warning("Your shockvar is not in the model formula, but since you are not simulating, I am ignoring it. Specify modeled variables for dynamic simulations.")
        }
        if (!(shockvar %in% diffs)) {
          if (!(shockvar %in% names(lagdiffs))) {
            if (!(shockvar %in% levels)) {
              if (!(shockvar %in% names(lags))) {
                warning("Your shockvar is not found in lags, lagdiffs, levels, or differences, but since you are not simulating, I am ignoring it. Specify modeled variables for dynamic simulations.")
              }
            }
          }
        }
      }
    }
  }
  else {
    if (identical(deparse(substitute(shockvar)), "list()")) {
      stop("'shockvar' must be specified for a dynamic simulation")
    }
    else {
      if (!(grepl("\"", deparse(substitute(shockvar))))) {
        stop("'shockvar' must be specified in quotation marks.")
      }
      else {
        if (length(shockvar) > 1) {
          stop("Only specify one shockvar in a dynamic simulation")
        }
        else {
          if (!(shockvar %in% all.vars(formula))) {
            stop(paste(paste("Variable"), paste(shockvar), 
                       paste("(the shockvar) not found in model formula. Shock variable required for dynamic simulation."), 
                       sep = " "))
          }
          else {
            if (!(shockvar %in% diffs)) {
              if (!(shockvar %in% names(lagdiffs))) {
                if (!(shockvar %in% levels)) {
                  if (!(shockvar %in% names(lags))) {
                    stop(paste(paste("Variable"), paste(shockvar), 
                               paste("(the shockvar) not found lags, lagdiffs, levels, or differences. Shock variable required for dynamic simulation."), 
                               sep = " "))
                  }
                }
              }
            }
          }
        }
      }
    }
    if (shockval == sd(data[[shockvar]], na.rm = T)) {
      shock.message <- paste(paste(shockvar), paste("shocked by one standard deviation of"), 
                             paste(shockvar), paste("by default."), sep = " ")
    }
    if (length(forceset)) {
      if (as.character(formula[[2]]) %in% names(forceset)) {
        stop("LDV cannot be forceset in dynamic simulation.")
      }
      else {
        for (i in 1:length(names(forceset))) {
          if (!(names(forceset)[i] %in% all.vars(formula))) {
            warning(paste(paste("Variable"), paste(names(forceset)[i]), 
                          paste("in the forceset list not found in model formula. Variable not forced."), 
                          sep = " "))
          }
        }
      }
    }
    if (time >= range) {
      stop("The range of simulation must be longer than shock time.")
    }
  }
  for (i in 2:length(all.vars(formula))) {
    if (!(all.vars(formula)[i] %in% diffs)) {
      if (!(all.vars(formula)[i] %in% names(lagdiffs))) {
        if (!(all.vars(formula)[i] %in% levels)) {
          if (!(all.vars(formula)[i] %in% names(lags))) {
            warning(paste(paste("Variable"), paste(all.vars(formula)[i]), 
                          paste("in the formula list not found lags, lagdiffs, levels, or differences. Variable ignored."), 
                          sep = " "))
          }
        }
      }
    }
  }
  dv <- dvnamelist <- NULL
  ldvs <- lnumdvs <- ldvnamelist <- ldvset <- NULL
  lddvs <- ldnumdvs <- lddvnamelist <- lddvset <- NULL
  dsiv <- lnumdsiv <- dsivnamelist <- dsivset <- NULL
  livs <- lnumivs <- livsnamelist <- livsset <- NULL
  lsiv <- lnumsiv <- lsivnamelist <- lsivset <- NULL
  divs <- lnumdivs <- divnamelist <- divset <- NULL
  ldsiv <- lnumldsiv <- ldsivnamelist <- ldsivset <- NULL
  ldivs <- lnumldivs <- ldivsnamelist <- ldivsset <- NULL
  siv <- sivnamelist <- sivset <- NULL
  ivs <- ivsnamelist <- ivsset <- NULL
  trendvar <- nocons <- trendset <- NULL
  ec.message <- constant.message <- trend.message <- NULL
  if (ec == "TRUE") {
    ec.message <- "Error correction (EC) specified; dependent variable to be run in differences."
    dv <- as.data.frame(dshift(as.matrix(data[[as.character(formula[[2]])]])))
    colnames(dv) <- dvnamelist <- paste("d", as.character(formula[[2]]), 
                                        sep = ".")
    assign(paste(dvnamelist), as.matrix(dv))
  }
  else {
    ec.message <- "Dependent variable to be run in levels."
    dv <- as.data.frame(as.matrix(data[as.character(formula[[2]])]))
    colnames(dv) <- dvnamelist <- as.character(formula[[2]])
    assign(paste(dvnamelist), as.matrix(dv))
  }
  if (length(lags)) {
    if (!(as.character(formula[[2]]) %in% names(lags))) {
      if (noLDV == TRUE) {
        warning(paste("ARDL model with no LDV specified. Only finite dynamics are possible through lags of X (which might affect simulations). Are you sure you want this?"))
        lnumdvs <- 0
      }
      else {
        lnumdvs <- 1
        warning("Lagged dependent variable added to model formula.")
        ldvs <- cbind(ldvs, lshift(as.matrix(data[as.character(formula[[2]])]), 
                                   l = 1))
        v.name <- paste("l", 1, as.character(formula[[2]]), 
                        sep = ".")
        ldvnamelist <- c(ldvnamelist, v.name)
        assign(paste(v.name), lshift(as.matrix(data[as.character(formula[[2]])]), 
                                     l = 1))
        if (constant == TRUE) {
          ldvset <- c(ldvset, mean(as.matrix(data[as.character(formula[[2]])]), 
                                   na.rm = T))
        }
        else {
          ldvset <- c(ldvset, 0)
        }
      }
    }
    for (i in 1:length(lags)) {
      if (names(lags)[[i]] == as.character(formula[[2]])) {
        lnumdvs <- lags[[i]]
        for (o in 1:length(lags[[i]])) {
          ldvs <- cbind(ldvs, lshift(as.matrix(data[[names(lags[i])]]), 
                                     l = lags[[i]][o]))
          v.name <- paste("l", lags[[i]][o], names(lags[i]), 
                          sep = ".")
          ldvnamelist <- cbind(ldvnamelist, v.name)
          assign(paste(v.name), lshift(as.matrix(data[[names(lags[i])]]), 
                                       l = lags[[i]][o]))
          if (constant == TRUE) {
            ldvset <- c(ldvset, mean(as.matrix(data[as.character(formula[[2]])]), 
                                     na.rm = T))
          }
          else {
            ldvset <- c(ldvset, 0)
          }
        }
      }
      else {
        if (names(lags)[[i]] %in% shockvar) {
          lnumsiv <- lags[[i]]
          for (o in 1:length(lags[[i]])) {
            lsiv <- cbind(lsiv, lshift(as.matrix(data[[names(lags[i])]]), 
                                       l = lags[[i]][o]))
            v.name <- paste("l", lags[[i]][o], names(lags[i]), 
                            sep = ".")
            lsivnamelist <- cbind(lsivnamelist, v.name)
            assign(paste(v.name), lshift(as.matrix(data[[names(lags[i])]]), 
                                         l = lags[[i]][o]))
            if (names(lags)[i] %in% names(forceset)) {
              lsivset <- c(lsivset, as.numeric(forceset[(names(forceset) == 
                                                           names(lags)[i])][1]))
            }
            else {
              lsivset <- c(lsivset, mean(data[[names(lags[i])]], 
                                         na.rm = T))
            }
          }
        }
        else {
          lnumivs <- c(lnumivs, lags[[i]])
          for (o in 1:length(lags[[i]])) {
            livs <- cbind(livs, lshift(as.matrix(data[[names(lags[i])]]), 
                                       l = lags[[i]][o]))
            v.name <- paste("l", lags[[i]][o], names(lags[i]), 
                            sep = ".")
            livsnamelist <- cbind(livsnamelist, v.name)
            assign(paste(v.name), lshift(as.matrix(data[[names(lags[i])]]), 
                                         l = lags[[i]][o]))
            if (names(lags)[i] %in% names(forceset)) {
              livsset <- c(livsset, as.numeric(forceset[(names(forceset) == 
                                                           names(lags)[i])][1]))
            }
            else {
              livsset <- c(livsset, mean(data[[names(lags[i])]], 
                                         na.rm = T))
            }
          }
        }
      }
    }
  }
  else {
    if (noLDV == TRUE) {
      warning(paste(paste("ARDL model with no LDV specified. Only finite dynamics are possible through lags of X (which might affect simulations). Are you sure you want this?")))
      lnumdvs <- 0
    }
    else {
      lnumdvs <- 1
      warning("Lagged dependent variable added to model formula.")
      ldvs <- cbind(ldvs, lshift(as.matrix(data[as.character(formula[[2]])]), 
                                 l = 1))
      v.name <- paste("l", 1, as.character(formula[[2]]), 
                      sep = ".")
      ldvnamelist <- c(ldvnamelist, v.name)
      assign(paste(v.name), lshift(as.matrix(data[as.character(formula[[2]])]), 
                                   l = 1))
      if (constant == TRUE) {
        ldvset <- c(ldvset, mean(as.matrix(data[as.character(formula[[2]])]), 
                                 na.rm = T))
      }
      else {
        ldvset <- c(ldvset, 0)
      }
    }
  }
  if (length(diffs)) {
    for (i in 1:length(diffs)) {
      if (diffs[i] %in% shockvar) {
        dsiv <- cbind(dsiv, dshift(as.matrix(data[diffs[i]])))
        v.name <- paste("d.1", diffs[i], sep = ".")
        dsivnamelist <- c(dsivnamelist, v.name)
        assign(paste(v.name), dshift(as.matrix(data[diffs[i]])))
        dsivset <- c(dsivset, 0)
      }
      else {
        divs <- cbind(divs, dshift(as.matrix(data[diffs[i]])))
        v.name <- paste("d.1", diffs[i], sep = ".")
        divnamelist <- c(divnamelist, v.name)
        assign(paste(v.name), dshift(as.matrix(data[diffs[i]])))
        divset <- c(divset, 0)
      }
    }
  }
  if (length(lagdiffs)) {
    for (i in 1:length(lagdiffs)) {
      if (names(lagdiffs)[[i]] == as.character(formula[[2]])) {
        ldnumdvs <- lagdiffs[[i]]
        for (o in 1:length(lagdiffs[[i]])) {
          lddvs <- cbind(lddvs, ldshift(as.matrix(data[[names(lagdiffs[i])]]), 
                                        l = lagdiffs[[i]][o]))
          v.name <- paste("ld", lagdiffs[[i]][o], names(lagdiffs[i]), 
                          sep = ".")
          lddvnamelist <- cbind(lddvnamelist, v.name)
          assign(paste(v.name), ldshift(as.matrix(data[[names(lagdiffs[i])]]), 
                                        l = lagdiffs[[i]][o]))
          lddvset <- c(lddvset, 0)
        }
      }
      else {
        if (names(lagdiffs)[[i]] %in% shockvar) {
          lnumldsiv <- lagdiffs[[i]]
          for (o in 1:length(lagdiffs[[i]])) {
            ldsiv <- cbind(ldsiv, ldshift(as.matrix(data[[names(lagdiffs[i])]]), 
                                          l = lagdiffs[[i]][o]))
            v.name <- paste("ld", lagdiffs[[i]][o], 
                            names(lagdiffs[i]), sep = ".")
            ldsivnamelist <- c(ldsivnamelist, v.name)
            assign(paste(v.name), ldshift(as.matrix(data[[names(lagdiffs[i])]]), 
                                          l = lagdiffs[[i]][o]))
            ldsivset <- c(ldsivset, 0)
          }
        }
        else {
          lnumldivs <- c(lnumldivs, lagdiffs[[i]])
          for (o in 1:length(lagdiffs[[i]])) {
            ldivs <- cbind(ldivs, ldshift(as.matrix(data[[names(lagdiffs[i])]]), 
                                          l = lagdiffs[[i]][o]))
            v.name <- paste("ld", lagdiffs[[i]][o], 
                            names(lagdiffs[i]), sep = ".")
            ldivsnamelist <- c(ldivsnamelist, v.name)
            assign(paste(v.name), ldshift(as.matrix(data[[names(lagdiffs[i])]]), 
                                          l = lagdiffs[[i]][o]))
            ldivsset <- c(ldivsset, 0)
          }
        }
      }
    }
  }
  if (length(levels)) {
    for (i in 1:length(levels)) {
      if (levels[i] %in% shockvar) {
        siv <- cbind(siv, as.matrix(data[levels[[i]]]))
        v.name <- levels[i]
        sivnamelist <- c(sivnamelist, v.name)
        assign(paste(v.name), as.matrix(data[levels[[i]]]))
        if (levels[i] %in% names(forceset)) {
          sivset <- c(sivset, as.numeric(forceset[levels[i]]))
        }
        else {
          sivset <- c(sivset, mean(as.matrix(data[levels[[i]]]), 
                                   na.rm = T))
        }
      }
      else {
        ivs <- cbind(ivs, as.matrix(data[levels[[i]]]))
        v.name <- levels[i]
        ivsnamelist <- c(ivsnamelist, v.name)
        assign(paste(v.name), as.matrix(data[levels[[i]]]))
        if (levels[i] %in% names(forceset)) {
          ivsset <- c(ivsset, as.numeric(forceset[levels[i]]))
        }
        else {
          ivsset <- c(ivsset, mean(as.matrix(data[levels[[i]]]), 
                                   na.rm = T))
        }
      }
    }
  }
  if (trend == TRUE) {
    trendvar <- seq(1, length(dv[, 1]), 1)
    trendset <- -burnin
    trend.message <- "Deterministic linear trend added to model formula."
  }
  if (constant == FALSE) {
    constant.message <- "Constant suppressed from model formula."
  }
  IVs <- as.data.frame(cbind(ldvs, lddvs, dsiv, lsiv, ldsiv, 
                             siv, ivs, divs, livs, ldivs))
  colnames(IVs) <- c(ldvnamelist, lddvnamelist, dsivnamelist, 
                     lsivnamelist, ldsivnamelist, sivnamelist, ivsnamelist, 
                     divnamelist, livsnamelist, ldivsnamelist)
  set <- setlist <- c(ldvset, lddvset, dsivset, lsivset, ldsivset, 
                      sivset, ivsset, divset, livsset, ldivsset, trendset)
  if (constant == TRUE) {
    set <- setlist <- c(1, setlist)
  }
  if (trend == TRUE) {
    IVs <- data.frame(IVs, trendvar)
    colnames(IVs) <- c(ldvnamelist, lddvnamelist, dsivnamelist, 
                       lsivnamelist, ldsivnamelist, sivnamelist, ivsnamelist, 
                       divnamelist, livsnamelist, ldivsnamelist, "trendvar")
    if (constant == FALSE) {
      res <- lm(as.formula(paste(paste(dvnamelist), "~", 
                                 paste(colnames(IVs), collapse = "+"), "- 1", 
                                 collapse = " ")))
    }
    else {
      res <- lm(as.formula(paste(paste(dvnamelist), "~", 
                                 paste(colnames(IVs), collapse = "+"), collapse = " ")))
    }
  }
  else {
    if (constant == FALSE) {
      res <- lm(as.formula(paste(paste(dvnamelist), "~", 
                                 paste(colnames(IVs), collapse = "+"), "- 1", 
                                 collapse = " ")))
    }
    else {
      res <- lm(as.formula(paste(paste(dvnamelist), "~", 
                                 paste(colnames(IVs), collapse = "+"), collapse = " ")))
    }
  }
  if (modelout == TRUE) {
    print(summary(res))
  }
  res$y <- dv
  res$y.name <- as.character(formula[[2]])
  res$EC <- ec
  res$simulate <- simulate
  res$ymean <- mean(as.matrix(data[[as.character(formula[[2]])]]), 
                    na.rm = T)
  res$ldv <- ifelse(noLDV == TRUE, FALSE, TRUE)
  if (ec == TRUE) {
    res$ymean.diff <- mean(dshift(as.matrix(data[[as.character(formula[[2]])]])), 
                           na.rm = T)
  }
  if (ec == TRUE) {
    if (max(lnumdvs, lnumsiv, lnumivs) > 1) {
      warning("Multiple lags included in implied co-integrated relationship: are you sure you want this?")
    }
  }
  print(ec.message)
  if (!(identical(NULL, trend.message))) {
    print(trend.message)
  }
  if (!(identical(NULL, constant.message))) {
    print(constant.message)
  }
  if (!(identical(NULL, shock.message))) {
    print(shock.message)
  }
  B <- coef(res)
  V <- vcov(res)
  sigma2 <- sigma(res)^2
  dfsig <- res$df.residual
  len <- res$rank
  if (simulate == TRUE) {
    sigl <- ((100 - sig)/2)/100
    sigu <- (100 - ((100 - sig)/2))/100
    brange <- range + burnin
    btime <- time + burnin
    meanpv <- meandpv <- rep(NA, brange)
    if (sig %in% c(75, 90, 95)) {
      d_PV_pctile <- PV_pctile <- matrix(rep(NA, brange * 
                                               6), ncol = 6)
      colnames(PV_pctile) <- c("ll95", "ll90", "ll75", 
                               "ul75", "ul90", "ul95")
      colnames(d_PV_pctile) <- c("d.ll95", "d.ll90", "d.ll75", 
                                 "d.ul75", "d.ul90", "d.ul95")
    }
    else {
      d_PV_pctile <- PV_pctile <- matrix(rep(NA, brange * 
                                               8), ncol = 8)
      colnames(PV_pctile) <- c("ll95", "ll90", "ll75", 
                               "ul75", "ul90", "ul95", "ll", "ul")
      colnames(d_PV_pctile) <- c("d.ll95", "d.ll90", "d.ll75", 
                                 "d.ul75", "d.ul90", "d.ul95", "d.ll", "d.ul")
    }
    if (fullsims == TRUE) {
      PV_all_sims <- matrix(rep(NA, brange * sims), ncol = brange)
    }
    PB <- mvrnorm(n = sims, mu = B, Sigma = V)
    Sigma2 <- (sigma2 * dfsig)/(rchisq(sims, dfsig))
    PV <- PB %*% set
    if (expectedval == TRUE) {
      for (i in 1:sims) {
        PV.error <- PV[i] + rnorm(1000, 0, sqrt(Sigma2[i]))
        PV[i] <- mean(PV.error)
      }
    }
    else {
      PV <- PV + rnorm(sims, 0, sqrt(Sigma2))
    }
    if (sig %in% c(75, 90, 95)) {
      if (ec == TRUE) {
        if (constant == FALSE) {
          PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975)) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                                                              na.rm = T)
        }
        else {
          PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975)) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                                                              na.rm = T)
        }
        d_PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975))
      }
      else {
        PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                         0.125, 0.875, 0.95, 0.975))
        d_PV_pctile[1, ] <- rep(NA, 6)
      }
    }
    else {
      if (ec == TRUE) {
        if (constant == FALSE) {
          PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975, sigl, sigu)) + 
            mean(as.matrix(data[[as.character(formula[[2]])]]), 
                 na.rm = T)
        }
        else {
          PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975, sigl, sigu)) + 
            mean(as.matrix(data[[as.character(formula[[2]])]]), 
                 na.rm = T)
        }
        d_PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975, sigl, sigu))
      }
      else {
        PV_pctile[1, ] <- quantile(PV, c(0.025, 0.05, 
                                         0.125, 0.875, 0.95, 0.975, sigl, sigu))
        d_PV_pctile[1, ] <- rep(NA, 8)
      }
    }
    if (fullsims == TRUE) {
      if (ec == TRUE) {
        if (constant == FALSE) {
          PV_all_sims[, 1] <- mean(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                              na.rm = T)
        }
        else {
          PV_all_sims[, 1] <- mean(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                              na.rm = T)
        }
      }
      else {
        PV_all_sims[, 1] <- PV
      }
    }
    if (ec == TRUE) {
      if (constant == FALSE) {
        if (qoi == "mean") {
          meanpv[1] <- mean(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                       na.rm = T)
          meandpv[1] <- mean(PV)
        }
        else {
          meanpv[1] <- median(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                         na.rm = T)
          meandpv[1] <- median(PV)
        }
      }
      else {
        if (qoi == "mean") {
          meanpv[1] <- mean(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                       na.rm = T)
          meandpv[1] <- mean(PV)
        }
        else {
          meanpv[1] <- median(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                         na.rm = T)
          meandpv[1] <- median(PV)
        }
      }
    }
    else {
      if (qoi == "mean") {
        meanpv[1] <- mean(PV)
        meandpv[1] <- NA
      }
      else {
        meanpv[1] <- median(PV)
        meandpv[1] <- NA
      }
    }
    print("dynardl estimating ...")
    flush.console()
    pb <- txtProgressBar(min = 0, max = brange, style = 3)
    for (p in 2:brange) {
      Sys.sleep(0.1)
      PV_lag <- PV
      row <- 1
      if (constant == TRUE) {
        row <- row + 1
      }
      if (max(lnumdvs) > 0) {
        for (lag in 1:length(lnumdvs)) {
          w <- p - lnumdvs[lag]
          if (w > 0) {
            set[row] <- meanpv[w]
          }
          row <- row + 1
        }
      }
      if (length(ldnumdvs)) {
        for (lag in 1:length(ldnumdvs)) {
          w <- p - ldnumdvs[lag]
          if (w > 1) {
            wm1 <- w - 1
            set[row] <- meanpv[w] - meanpv[wm1]
          }
          row <- row + 1
        }
      }
      if (p == btime) {
        if (length(dsivnamelist)) {
          for (var in 1:length(dsivnamelist)) {
            set[row] <- shockval
            row <- row + 1
          }
        }
        if (length(lsivnamelist)) {
          for (var in 1:length(lsivnamelist)) {
            row <- row + 1
          }
        }
        if (length(ldsivnamelist)) {
          for (var in 1:length(ldsivnamelist)) {
            row <- row + 1
          }
        }
        if (length(sivnamelist)) {
          for (var in 1:length(sivnamelist)) {
            set[row] <- sivset[1] + shockval
            row <- row + 1
          }
        }
      }
      else {
        if (p > btime) {
          if (length(dsivnamelist)) {
            for (var in 1:length(dsivnamelist)) {
              set[row] <- 0
              row <- row + 1
            }
          }
          if (length(lsivnamelist)) {
            for (lag in 1:length(lnumsiv)) {
              w <- p - btime
              if (w == lnumsiv[lag]) {
                set[row] <- lsivset[1] + shockval
              }
              row <- row + 1
            }
          }
          if (length(ldsivnamelist)) {
            for (lagd in 1:length(lnumldsiv)) {
              w <- p - btime
              if (w == lnumldsiv[lagd]) {
                set[row] <- shockval
              }
              else {
                set[row] <- 0
              }
              row <- row + 1
            }
          }
          if (length(sivnamelist)) {
            for (var in 1:length(sivnamelist)) {
              row <- row + 1
            }
          }
        }
      }
      if (trend == TRUE) {
        set[length(B)] <- p - burnin
      }
      PV <- PB %*% set
      if (expectedval == TRUE) {
        for (i in 1:sims) {
          PV.error <- PV[i] + rnorm(1000, 0, sqrt(Sigma2[i]))
          PV[i] <- mean(PV.error)
        }
      }
      else {
        PV <- PV + rnorm(sims, 0, sqrt(Sigma2))
      }
      if (sig %in% c(75, 90, 95)) {
        if (ec == TRUE) {
          if (noLDV == TRUE) {
            if (constant == FALSE) {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975)) + 
                mean(as.matrix(data[[as.character(formula[[2]])]]), 
                     na.rm = T)
            }
            else {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975)) + 
                mean(as.matrix(data[[as.character(formula[[2]])]]), 
                     na.rm = T)
            }
          }
          else {
            if (constant == FALSE) {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975)) + 
                set[1]
            }
            else {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975)) + 
                set[2]
            }
          }
          d_PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                             0.05, 0.125, 0.875, 0.95, 0.975))
        }
        else {
          PV_pctile[p, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975))
          d_PV <- PV - PV_lag
          d_PV_pctile[p, ] <- quantile(d_PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975))
        }
      }
      else {
        if (ec == TRUE) {
          if (noLDV == TRUE) {
            if (constant == FALSE) {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975, sigl, 
                                               sigu)) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                                             na.rm = T)
            }
            else {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975, sigl, 
                                               sigu)) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                                             na.rm = T)
            }
          }
          else {
            if (constant == FALSE) {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975, sigl, 
                                               sigu)) + set[1]
            }
            else {
              PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975, sigl, 
                                               sigu)) + set[2]
            }
          }
          d_PV_pctile[p, ] <- quantile(PV, c(0.025, 
                                             0.05, 0.125, 0.875, 0.95, 0.975, sigl, sigu))
        }
        else {
          PV_pctile[p, ] <- quantile(PV, c(0.025, 0.05, 
                                           0.125, 0.875, 0.95, 0.975, sigl, sigu))
          d_PV <- PV - PV_lag
          d_PV_pctile[p, ] <- quantile(d_PV, c(0.025, 
                                               0.05, 0.125, 0.875, 0.95, 0.975, sigl, sigu))
        }
      }
      if (fullsims == TRUE) {
        if (ec == TRUE) {
          if (noLDV == TRUE) {
            if (constant == FALSE) {
              PV_all_sims[, p] <- PV + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                            na.rm = T)
            }
            else {
              PV_all_sims[, p] <- PV + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                            na.rm = T)
            }
          }
          else {
            if (constant == FALSE) {
              PV_all_sims[, p] <- PV + set[1]
            }
            else {
              PV_all_sims[, p] <- PV + set[2]
            }
          }
        }
        else {
          PV_all_sims[, p] <- PV
        }
      }
      if (ec == TRUE) {
        if (constant == FALSE) {
          if (qoi == "mean") {
            if (noLDV == TRUE) {
              meanpv[p] <- mean(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                           na.rm = T)
            }
            else {
              meanpv[p] <- mean(PV) + set[1]
            }
            meandpv[p] <- mean(PV)
          }
          else {
            if (noLDV == TRUE) {
              meanpv[p] <- median(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                             na.rm = T)
            }
            else {
              meanpv[p] <- median(PV) + set[1]
            }
            meandpv[p] <- median(PV)
          }
        }
        else {
          if (qoi == "mean") {
            if (noLDV == TRUE) {
              meanpv[p] <- mean(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                           na.rm = T)
            }
            else {
              meanpv[p] <- mean(PV) + set[2]
            }
            meandpv[p] <- mean(PV)
          }
          else {
            if (noLDV == TRUE) {
              meanpv[p] <- median(PV) + mean(as.matrix(data[[as.character(formula[[2]])]]), 
                                             na.rm = T)
            }
            else {
              meanpv[p] <- median(PV) + set[2]
            }
            meandpv[p] <- median(PV)
          }
        }
      }
      else {
        if (qoi == "mean") {
          meanpv[p] <- mean(PV)
          meandpv[p] <- mean(d_PV)
        }
        else {
          meanpv[p] <- median(PV)
          meandpv[p] <- median(d_PV)
        }
      }
      setTxtProgressBar(pb, p)
    }
    close(pb)
    if (sig %in% c(75, 90, 95)) {
      sims <- matrix(rep(NA, range * 14), ncol = 14)
      sims[, 1] <- meanpv[(burnin + 1):brange]
      sims[, 2:7] <- PV_pctile[(burnin + 1):brange, ]
      sims[, 8] <- meandpv[(burnin + 1):brange]
      sims[, 9:14] <- d_PV_pctile[(burnin + 1):brange, 
                                  ]
      colnames(sims) <- c("central", "ll95", "ll90", "ll75", 
                          "ul75", "ul90", "ul95", "d.central", "d.ll95", 
                          "d.ll90", "d.ll75", "d.ul75", "d.ul90", "d.ul95")
    }
    else {
      sims <- matrix(rep(NA, range * 18), ncol = 18)
      sims[, 1] <- meanpv[(burnin + 1):brange]
      sims[, 2:9] <- PV_pctile[(burnin + 1):brange, ]
      sims[, 10] <- meandpv[(burnin + 1):brange]
      sims[, 11:18] <- d_PV_pctile[(burnin + 1):brange, 
                                   ]
      colnames(sims) <- c("central", "ll95", "ll90", "ll75", 
                          "ul75", "ul90", "ul95", paste("ll", sig, sep = ""), 
                          paste("ul", sig, sep = ""), "d.central", "d.ll95", 
                          "d.ll90", "d.ll75", "d.ul75", "d.ul90", "d.ul95", 
                          paste("d.ll", sig, sep = ""), paste("d.ul", 
                                                              sig, sep = ""))
    }
    sim.time <- seq(1, length(sims[, 1]))
    temp.names <- colnames(sims)
    sims <- cbind(sim.time, sims)
    colnames(sims) <- c("time", temp.names)
    z <- data.frame(sims)
    if (fullsims == TRUE) {
      all.sims <- data.frame(PV_all_sims[, (burnin + 1):brange])
      colnames(all.sims) <- paste("time", seq(1, ncol(all.sims), 
                                              1), sep = "")
      all.sims$central <- paste(qoi)
    }
    if (fullsims == TRUE) {
      out <- list(z, res, all.sims)
      names(out) <- c("simulation", "model", "rawsims")
      out$simulation$shocktime <- time
    }
    else {
      out <- list(z, res)
      names(out) <- c("simulation", "model")
      out$simulation$shocktime <- time
    }
  }
  else {
    out <- list(res)
    names(out) <- c("model")
  }
  class(out) <- "dynardl"
  out
}
