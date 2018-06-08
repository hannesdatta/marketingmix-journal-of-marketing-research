library(car)
library(reshape2)
library(Matrix)

###########################
### AUXILARY FUNCTIONS ####
###########################

# decide whether a time series is appropriate (per brand in each market)
	use_ts <- function(x) {
	      # exclude series from model estimation under certain conditions (see below)
	      # returns TRUE for valid series, and FALSE for invalid series

	      # invalid series if all values are NA
				if (all(is.na(x))) return(F)
				
				tmp = x
        .tmp=table(tmp)
        .tmp=.tmp[which(.tmp==max(.tmp))][1]
                
        # invalid series if it only contains one unique value (no variation)        
        if (length(unique(tmp))<=1) return(F)
				
        # invalid series if it has less than x months of non-zero observations
				if (length(which(!tmp==min(tmp, na.rm=T)))<12) return(F)
				
        return(T)
        }

# assigns a model to main R environment
	assign_model <- function(m, del=F) {
	  if (del==F) {
	    for (l in seq(along=m)) {
	      var = m[[l]]
	      eval(parse(text=paste0(names(m)[l], ' <<- var')))
	    }
	  }
	  if (del==T) {
	    for (l in seq(along=m)) suppressWarnings(eval(parse(text=paste0('rm(', names(m)[l],')'))))
	  }
	}

savemodels <- function(fname) {
	  lscall=ls(envir=.GlobalEnv)
	  stuff=setdiff(c(grep('^m[0-9]+', lscall,value=T),'analysis_markets', grep('results[_]', lscall, value=T)),'results_brands')
	  cat('saving...\n')
	  print(stuff)
	  cat('\ndone.\n')
	  save(list=stuff , file = fname)
	  }

######################
### MAIN ANALYSIS ####
######################
		
# Main analysis function (to be run seperately for each market_id i)
analyze_by_market <- function(i, setup_y, setup_x, setup_endogenous=NULL, trend = 'none', pval = .05, max.lag = 12, min.t = 36, 
                              estmethod = "FGLS", benchmarkb = NULL, use_quarters = TRUE, maxiter=1000,
                              attraction_model = "MNL", takediff = 'alwaysdiff', plusx = NULL, squared = F, lag_heterog=F,
                              carryover_zero=F) {
	
		cat('data preparation\n...')
		
		panel <<- brand_panel[market_id==i]
		
		for (var in plusx) {
		  panel[, (var):=get(var)+1]
		}
		
		# Create empty object holding the results of the model estimation process (elements will be added throughout the code)
		res = NULL
		
		# Return error if data has less than min.t observations
		if (length(unique(panel$date))<min.t) {
			res$error <- c(res$error, 'minimum_n')
			return(res)
			}
		
		res$specs = data.frame(category=unique(panel$category),
		                       country=unique(panel$country), 
		                       brand=unique(panel$brand), 
		                       market_id=as.numeric(unique(panel$market_id[1])),
		                       stringsAsFactors=F)
		res$market_id=as.numeric(unique(panel$market_id[1]))
		
		# create trend variable for all non-NA observations
		eval(parse(text=paste0('panel[!is.na(',setup_y,'), trend:=1:.N, by=c(\'category\', \'country\', \'brand\')]')))
	
			
		#########################################################################################################
		# Determine which series to include, and create object 'melted_panel' with all variables in long format #
		#########################################################################################################
		
		# Convert data to long format
		
	  # set ID vars of the data
	  id_vars= c('country','category','market_id', 'brand','date', 'quarter')
		
	  suppressWarnings(melted_panel <- melt(panel[,colnames(panel)%in%c(setup_y, setup_x, id_vars),with=F], id.vars=id_vars))
	  # Note: I surpress warnings here (the message shows that since  variable types are not the same, the procedure coerces them to a more fine grained measure).
	  #melted_panel[, list(ts_selected = use_ts(value)), by=c('country','category', 'brand', 'variable')]
	  
		# check whether variables qualify for estimation
		melted_panel[, ts_selected := use_ts(value), by=c('country','category', 'brand', 'variable')]
		
		# check whether lags of explanatory variables are included; verify these are kicked out!
		
		# kick out non-selected series and remove indicator column
		melted_panel <- melted_panel[ts_selected==T][, ts_selected:=NULL]
		
		# add copula terms to the data, if endogenous variables are specified
		if (length(setup_endogenous)>0) {
		  cop_terms = melted_panel[variable%in%setup_endogenous]
		
		  # run shapiro-wilk tests to assess non-normality of untransformed inputs to the copula function
		  res$cop_nonnormal = cop_terms[, list(shap_pval = shapiro.test(value)$p), by = c('category', 'country', 'brand', 'variable')]
		
		  cop_terms[, value := make_copula(value), by = c('country','category','brand','variable')]
		  
		  if (attraction_model=='MCI') cop_terms[, value := exp(value)]
		  
		  cop_terms[, variable := paste0('cop_', variable)]
		
		  # attach copula terms to data
		  melted_panel <- rbind(melted_panel, cop_terms)
		}
				
		# store summary of included variables for reporting
		incl_vars = as.character(unique(melted_panel$variable))
		res$variables = list(y=setup_y[setup_y%in%incl_vars], x=incl_vars[incl_vars %in% c(setup_x, paste0('cop_', setup_x))], endog = setup_endogenous)
		rm(incl_vars)
		
		##################################################################################
		# Determine benchmark brand for normalization (any brand with most observations) #
		##################################################################################
		
		tmp1 = panel[, list(obs=sum(selected_t_brand)),by=c('brand')]
		tmp2 = melted_panel[, list(nvars = length(unique(variable))), by=c('brand')]
		setkey(tmp1, brand)
		setkey(tmp2, brand)
		
		tmp <- tmp1[tmp2]
		setorderv(tmp, c('obs', 'nvars'), order=-1L)
		
		res$benchmark_brand = tmp[match(max(tmp$obs), obs)]$brand
		
		if (!is.null(benchmarkb)) res$benchmark_brand = benchmarkb
		
		rm(tmp, tmp1, tmp2)
		
		# assert that all variables are measured for the benchmark brand
		required_vars = grep(paste0(setup_endogenous,collapse='|'), unique(melted_panel$variable), value = TRUE, invert = TRUE)
		
		if (!all(required_vars %in% unique(melted_panel[brand==res$benchmark_brand]$variable))) {
			stop('Not all variables measured for selected benchmark brand.')
			}
		
		#############################################################
		# Transform variables to attraction model estimation system #
		#############################################################
		
		if (carryover_zero==F) {
		# add lagged dependent variable
		lagged_ms = melted_panel[variable==res$variables$y]
		lagged_ms[, value := makelag(value), by=c('brand')]
		lagged_ms[, variable:='lagunitsales_sh']
		melted_panel <- rbind(melted_panel, lagged_ms)
		rm(lagged_ms)
		}
		
		# create month variable from dates
		melted_panel[, month := as.numeric(as.factor(date))]
		
		# store melted panel in results
		res$melted_panel<-melted_panel
		
		# transform system to base-brand representation
		dt <- data.table(dcast(melted_panel, brand+month+quarter~variable, value.var=c('value')))
		
		if (carryover_zero==F) {
		  m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
		  m_form_heterog = as.formula(paste0('~ ', paste0(res$variables$x, collapse = '+'), ifelse(lag_heterog==T, ' + lagunitsales_sh', '')))
		}
		
		if (carryover_zero==T) {
		  m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ''))
		  m_form_heterog = as.formula(paste0('~ ', paste0(res$variables$x, collapse = '+'), ifelse(lag_heterog==T, '', '')))
		}
		
	  m_form_index = as.formula(~ brand + month)
		
	  # recall: MCI takes logs, MNL does not
		dtbb <- suppressWarnings(attraction_data(formula=m_form, 
								data = dt, 
								heterogenous = m_form_heterog, 
								index = m_form_index, 
								benchmark=res$benchmark_brand,
								model = attraction_model))

		
		# in the case of only two brands and a heterogenous carry-over parameter, remove lagged market share of base brand for identification purposes
		if (length(unique(dtbb@input$index$brand))==2 & lag_heterog==T & carryover_zero==F) {
			X = dtbb@X
			delcol = which(colnames(X)==paste0(dtbb@benchmark, '_lagunitsales_sh'))
			X <- X[, -delcol]
			dtbb@X <- X
			rm(X)
			}
		
				
		###########################
		# Conduct Unit Root Tests #
		###########################

		# Procedure to conduct equation-by-equation operations (e.g., differencing in case of non-stationarity)
		
		cat('performing unit root tests for transformed system\n...')
		dat_by_brand = split(data.frame(y=dtbb@y, dtbb@X, period = dtbb@period), dtbb@individ)

		# function to conduct Enders procedure by column
			column_enders = function(X) {
			adf=data.frame(t(apply(X, 2, adf_enders, maxlag=12,pval=pval, season=NULL)))
			adf$variable = rownames(adf)
			rownames(adf) <- NULL
			
			to_be_diffed = adf$variable[which(adf$ur==1)]
			for (variable in to_be_diffed) {
				X[,variable] = makediff(X[,variable])
				}
			
			return(list(adf=adf,data=X))
			}

    # execute procedures by brand
		eq_by_brand=lapply(seq(along=dat_by_brand), function(z) {
		  
		  ####################
		  # Enders procedure #
		  ####################
		  
		  # conduct Enders procedure on all variables (i.e., DVs (ratio of marketshares), 
		  # and explanatory variables (log X, and log(y focal), log(y benchmark)), 
		  # but not the trend and not the copula terms)
		  
			curr_brand = names(dat_by_brand)[z]
			#print(curr_brand)
			
			vars = setdiff(colnames(dat_by_brand[[z]])[!colSums(abs(dat_by_brand[[z]]))==0 & !grepl('[_]dum|trend', colnames(dat_by_brand[[z]]))],'period')
			
			# verify whether, for any given brand/base brand combination, the minimum N constraint for number of observations (result of use_ts) is given.
			delvars = vars[sapply(vars, function(xx) use_ts(ifelse(grepl(dtbb@benchmark, xx), -1, 1) * unlist(dat_by_brand[[z]][, xx])))==F]
			
			# also add copula and lagged values to deletion (doesn't make sense deleting the focal variable, but not its lagged term)
			delvars = union(delvars, intersect(gsub('[_]', '_lag', delvars),vars))
			delvars = union(delvars, intersect(gsub('[_]', '_cop_', delvars),vars))
			
			# if any variable does not satisfy the constraint, kick out those variables
			for (kickout in delvars) {
				dat_by_brand[[z]] = dat_by_brand[[z]][, -which(colnames(dat_by_brand[[z]])==kickout)]
				}
			vars = setdiff(vars,delvars)
			
			# conduct Enders procedure
			adf=data.frame(t(apply(dat_by_brand[[z]][vars], 2, adf_enders, maxlag=12,pval=pval, season=NULL)))
			adf$variable = rownames(adf)
			rownames(adf) <- NULL
			
			
			adf = rbind(data.table(variable=delvars), data.table(adf), fill=T)
			adf[, included:=T]
			adf[variable%in%delvars, included:=F]
			adf=data.frame(adf)
			
			##########################
			# differencing procedure #
			##########################
			
			# how to handle series with unit roots in the estimation process?
			# this procedure allows for three possibilities:
		  # (1) most conservative: take differences of ALL terms
			#     use: takediff = "alwaysdiff"
			#
			# (2) do not take any difference
			#     use: takediff = "none"
			#
			# (3) take the difference depending on the UR test outcome above
			#     use: takediff = "flexible"
			#

			out_matrix = dat_by_brand[[z]]

			adf = data.frame(data.table(adf)[variable=='y', trend_included:=F])
			
			ytrend=0
			
			
			if (takediff=='flexible') {
			  #		1) UR test outcome on Y; 
			  #		   o if trend --> add trend variable
			  #		   o if UR present --> take differences in Y
			  
			  #		2) UR test outcome for Xs:
			  #		   o if UR present in X and UR present in Y --> take difference of both Y and X (and the differences of the copula terms)
			  #		   o if UR present in X, but not in Y --> difference X, but take level copulas (as error term is still in levels)
			  
			  ydiff = data.table(adf)[variable=='y']$ur
			  ytrend = data.table(adf)[variable=='y']$trend
			  
			  	
				
				to_be_diffed = NULL
				
				if (ydiff==1) {
					# what about xs?
					ur_xs = data.table(adf)[!variable == 'y' & ur==1 & !grepl('cop_', variable)]$variable
					ur_xs_cop = sub ('[_]', '_cop_', ur_xs)
					ur_xs_cop = ur_xs_cop[ur_xs_cop%in%adf$variable]
					
					to_be_diffed = c('y', ur_xs, ur_xs_cop)
					
					} else {
					ur_xs = data.table(adf)[!variable == 'y' & ur==1 & !grepl('cop_', variable)]$variable
					to_be_diffed = c('y', ur_xs)
					
					}
				
			}
				
			
			if (takediff == 'alwaysdiff') {
			  to_be_diffed = data.table(adf)[included==T]$variable
		  }
			
			if (takediff == 'none') {
			  # nothing
			  to_be_diffed = NULL
			  
		  }
		
			# save which series are diffed, and which not
			adf = data.table(adf)[,diffed:=F]
			adf[, brand := curr_brand]
			adf = data.frame(adf[variable%in%to_be_diffed, diffed := T])
			
			
			# execute differencing
			for (variable in to_be_diffed) {
				out_matrix[,variable] = makediff(out_matrix[,variable])
				}
		
			if (trend=='always') ytrend=1
		  if (trend=='none') ytrend=0
		  if (trend=='ur') {
		    if (takediff=='flexible') NULL # has been set earlier
		    if (takediff=='always') ytrend=ytrend # if diffed model, adding trend boils down to trend from UR estimation
		    if (takediff=='none') ytrend=ifelse(ytrend==1|ydiff==1,1,0) # if not diffed model, trend either if process is UR or linearly evolving
		    
		  }
			
			# add trend; default is UR-based
			if (ytrend==1) {
			  if (attraction_model=='MNL') out_matrix <- cbind(out_matrix, trend = seq(from=1, to=nrow(out_matrix)))
			  if (attraction_model=='MCI') out_matrix <- cbind(out_matrix, trend = log(seq(from=1, to=nrow(out_matrix))))
			}
			
			# relabel
			if ('trend' %in% colnames(out_matrix)) {
			  colnames(out_matrix)[which(colnames(out_matrix)=='trend')] <- paste0(curr_brand, '_trend')
			  vars <- c(vars, paste0(curr_brand, '_trend'))
			  adf = data.frame(data.table(adf)[variable=='y', trend_included:=T])
			}
			
			
			# create indicator for rows with non-missing information (missings arise from first-differencing)
			complete_obs = complete.cases(out_matrix)
			
			res= list(adf=adf, 
			          transformed=cbind(out_matrix), 
			          complete_obs = complete_obs, 
			          original = dat_by_brand[[z]], 
			          brand = curr_brand)
		
			return(res)
			})

		# collect ADF test results
			adf_sur <- rbindlist(lapply(eq_by_brand, function(x) x$adf))
			adf_sur[, ':=' (market_id = unique(res$specs$market_id), category=unique(res$specs$category), country=unique(res$specs$country))]
			res$adf_sur <- adf_sur
	
		# collect transformed equations
			stacked_eq = rbindlist(lapply(eq_by_brand, function(x) x$transformed), fill=TRUE)
			stacked_eq[, period:=NULL]
			complete_eqs = unlist(lapply(eq_by_brand, function(x) x$complete_obs))
			
		# replace NAs for trends/copula/and any other "missing" value 
		# (i.e., values that may have been kicked out due to the use_ts constraint) to 0; 
		# note that "real" missing values are kicked out by complete_obs
			
			for (resetvar in colnames(stacked_eq)) {
				vals = stacked_eq[,resetvar, with=F]
				stacked_eq[, (resetvar) := ifelse(is.na(get(resetvar)), 0, get(resetvar))]
				}
		
		# separate DVs from IVs for estimation
			X = stacked_eq[,!colnames(stacked_eq)%in%'y',with=F]
			Y = data.frame(stacked_eq[,'y',with=F])
			index=data.frame(date=dtbb@period,brand=dtbb@individ)

		# remove all-zero columns from X (e.g., where a brand variable is removed earlier ("NA"), and contains zeros for all other brands).
			delvars = colnames(X)[which(colSums(abs(X))==0)]
			keepv = setdiff(colnames(X), delvars)
			X = X[, keepv, with=F]
	
		# kick out incomplete ('diffed') vars, i.e., their first observation (as it is NA) by brand
			X=as.matrix(X[complete_eqs,])
			Y=as.matrix(Y[complete_eqs,])
			index=data.frame(index[complete_eqs,])

		#####################	
		# quarterly dummies #
		#####################
		
		if (use_quarters==T) {
			# create brand indicators by quarter (q2, q3, q4)
			dummatrix = data.table(index)
			setnames(dummatrix, c('period', 'individ'))
			dummatrix[, order:=1:.N]
			quarters = dt[, list(N=.N), by = c('month','quarter')][, N:=NULL]
			setkey(dummatrix, period)
			setkey(quarters, month)
			dummatrix[quarters, quarter := i.quarter]
			setorder(dummatrix, order)
			
			# create quarterly dummies
			for (br in unique(dummatrix$individ)) {
					for (qu in 2:4) {
						dummatrix[individ==br, paste0('quarter', qu,'_', br) := ifelse(quarter==qu, 1, 0)]
						dummatrix[!individ==br, paste0('quarter', qu,'_', br) := 0]
						}
					}
					
			dummatrix[, ':=' (quarter=NULL, period=NULL, individ=NULL, order=NULL)]

			X=as.matrix(data.frame(X,dummatrix))
			rm(dummatrix, quarters)
		}
			
		#################
		# Estimate SURs #
		#################
	
		# rescale X matrix (normalize to 1?) - strongly recommended for these models to help conversion
		rescale = TRUE
		
		# save ranges before rescaling
		res$ranges=list(before=data.table(original_variable=colnames(X), min = colMins(X), max=colMaxs(X))[!grepl('[_]sq|[_]cop[_]|[_]dum', original_variable)])
		
		if (rescale==T) rescale_values = apply(X, 2, function(x) max(abs(x)))
		if (rescale==F) rescale_values = apply(X, 2, function(x) 1)
		
		div_matrix <- matrix(rep(rescale_values, nrow(X)), byrow=TRUE, ncol=length(rescale_values))
		X=X/div_matrix
		
		# save ranges after rescaling
		if (rescale==T) res$ranges$after=data.table(original_variable=colnames(X), min = colMins(X), max=colMaxs(X))[!grepl('[_]sq|[_]cop[_]|[_]dum', original_variable)]
		
		# Step 1: Determine significance of copula terms by endogenous regressor; retain those brand-variable terms that are significant
		copula_sign = lapply(setup_endogenous, function(regr) {
			exclude <- grep('.*[_]cop[_]', colnames(X), value=T)
			include <- grep(paste0('.*[_]cop[_]', regr), colnames(X), value=T)
			final_exclude <- setdiff(exclude,include)
			print(regr)
			
			m_interim<-itersur(X=as.matrix(X[,!colnames(X)%in%final_exclude]),Y=as.matrix(Y),index=index, method = estmethod, maxiter=maxiter)
			
			if (m_interim@iterations==maxiter) stop('error with iterations')
			res = data.table(m_interim@coefficients)
			res[, copula_check := regr]
			return(res)
			})
		
		copula_sign <- rbindlist(copula_sign)
		keep_vars = colnames(X)
		if (nrow(copula_sign)>0) {
			exclude <- copula_sign[grepl('.*[_]cop[_]', variable)][abs(z)<abs(qnorm(pval/2))]
			keep_vars <- colnames(X)[which(!colnames(X)%in%exclude$variable)]
			} 
		
		# Step 2: Determine significance of squared terms
		if(squared==T) {
		  
		  squared_sign = lapply(gsub('_sq', '', grep('_sq', setup_x, value=T)), function(regr) {
		    exclude <- grep('.*[_]sq.*', colnames(X), value=T)
		    include <- grep(paste0('.*[_]', regr, '[_].*'), colnames(X), value=T)
		    final_exclude <- setdiff(exclude,include)
		    final_include <- setdiff(keep_vars, final_exclude)
		    
		    m_interim<-itersur(X=as.matrix(X[,which(colnames(X)%in%final_include)]),Y=as.matrix(Y),index=index, method = estmethod, maxiter=maxiter)
		    
		    if (m_interim@iterations==maxiter) stop('error with iterations')
		    res = data.table(m_interim@coefficients)
		    res[, var := regr]
		    return(res)
		  })
		  
		  squared_sign <- rbindlist(squared_sign)
		  
		  if (nrow(squared_sign)>0) {
		    # retain only information on squared terms (e.g., remove copula, retain only when squares were estimated)
		    square_terms = squared_sign
		    square_terms = square_terms[, checkvar:=grepl(paste0('.*', unique(var),'.*'), variable), by = 'var'][checkvar==T&!grepl('.*[_]cop[_].*', variable)][,checkvar:=NULL]
		    # add normalization constants
		    normalization = data.table(variable=names(rescale_values), scale = rescale_values)
		    square_terms = merge(square_terms, normalization, by = c('variable'), all.x=T, all.y=F)
		    
		    square_terms[, original_variable := gsub('[_]sq','', variable)]
		    square_terms[, sq := ifelse(grepl('[_]sq', variable), 'sq', 'lin')]
		    
		    setnames(square_terms, 'variable', 'brand_variable')
		    
		    tmp = melt(square_terms, id.vars=c('brand_variable', 'var', 'original_variable', 'sq'))
		    
		    square_terms=data.table(dcast(tmp, original_variable + var ~ sq+variable, value.var='value'))
		    
		    # obtain range of observed variables as they have been used in the model estimation
		    range = data.table(original_variable=colnames(X), min = colMins(X), max=colMaxs(X))[!grepl('[_]sq', original_variable)]
		    
		    square_terms = merge(square_terms, range, by = c('original_variable'), all.x=T, all.y=F)
		    
		    # calculate inflection points
		    square_terms[, inflection_point := (-lin_coef*sq_scale)/(2*lin_scale^2*sq_coef), by = c('var','original_variable')]
		    square_terms[, inflection_inrange := inflection_point>=min & inflection_point<=max, by = c('var','original_variable')]
		    
		    # retain only rows with squared terms; decide which ones to exclude:
		    # a) insignificantsquared terms, and those outside of the inflection points
		    square_terms[, sq_included := !(abs(sq_z)<abs(qnorm(pval/2))|inflection_inrange==F)]
		    res$squared_terms <- square_terms
		    keep_vars <- keep_vars[which(!keep_vars%in%paste0(square_terms[sq_included==F]$original_variable, '_sq'))]
		    
		  } 
		}
		
		# Run final model
		cat('running SUR for selected variables\n')
		
		m<-itersur(X=as.matrix(X[,keep_vars]),Y=as.matrix(Y),index=index, method = estmethod, maxiter=maxiter)
		if (m@iterations==maxiter) stop('error with iterations')
	
	# back-scale
	if (rescale == T) {
		retr_coefs <- coef(m)$coef
		mvarcovar=m@varcovar

		rescale_after = rescale_values[keep_vars]
		retr_coefs[seq(length.out=length(rescale_after))] = retr_coefs[seq(length.out=length(rescale_after))] / rescale_after
		
		for (ch in seq(length.out=length(rescale_after))) {
			mvarcovar[ch,] <- mvarcovar[ch,] / rescale_after[ch]
			mvarcovar[,ch] <- mvarcovar[,ch] / rescale_after[ch]
			}
		
		m@coefficients[,2:3] <- cbind(retr_coefs, sqrt(diag(mvarcovar)))
		m@varcovar <- mvarcovar
		}
	
	# store model
	res$model <- m
	res$model@coefficients$brand <- unlist(lapply(strsplit(as.character(res$model@coefficients$variable), '_'), function(x) x[1]))
	res$model@coefficients$varname <- unlist(lapply(strsplit(as.character(res$model@coefficients$variable), '_'), function(x) paste(x[-1],collapse='_')))
	res$model@coefficients$market_id = unique(res$specs$market_id)
	res$model@coefficients$category = unique(res$specs$category)
	res$model@coefficients$country = unique(res$specs$country)
	
	
	# @fix this
	# legacy: R2s by model
	if(0){
	# Compute R2s by model
	pred = data.table(DVlevels=dtbb@y, brand=dtbb@individ, date=dtbb@period)
	pred[, DVlevelslag := c(NA, DVlevels[-.N]), by = c('brand')]
	setkey(pred, brand, date)
	
	pred2 = data.table(cbind(index, DV = as.matrix(Y), predicted = res$model@predicted, resid = res$model@resid))
	setkey(pred2, brand, date)
	
	pred <- merge(pred, pred2, by=c('brand', 'date'), all.x=F, all.y=T)
	adftmp = adf_sur[variable=='y']
	pred[, UR := adftmp$ur[match(brand, adftmp$brand)]]

	R2=rbindlist(lapply(split(pred, as.character(pred$brand)), function(x) {
		
			if (all(x$UR==0)) { # Model in levels
				return(
				#data.frame(brand=unique(x$brand), UR = unique(x$UR), R2 = 1-sum(x$resid^2)/sum((x$DV-mean(x$DV))^2))
				data.frame(brand=unique(x$brand), UR = unique(x$UR), R2levels = cor(x$predicted, x$DV)^2, R2 = cor(x$predicted, x$DV)^2)
				)
				}
			if (all(x$UR==1)) { # Model in differences
				#1-sum(x$resid^2)/sum((x$DV-mean(x$DV))^2)
				dvhat = x$predicted + x$DVlevelslag
				return(
				data.frame(brand=unique(x$brand), UR = unique(x$UR), R2levels = cor(dvhat, x$DVlevels)^2, R2 = cor(x$predicted, x$DV)^2)
				)

				}
			
			}))

	res$R2 <- R2
	rm(adftmp)
	}
	
	
	# Compute VIF values
	
		vifs = rbindlist(lapply(split(1:nrow(index), as.character(index$brand)), function(ind) {
			vifdf=as.matrix(X)[ind,]
		
			# drop zero columns
			vifdf = vifdf[,which(!colSums(vifdf)==0)]
			vify = unlist(Y)[ind]
			
		  .vifvars = colnames(vifdf)
		  df.cop = data.frame(as.matrix(cbind(y=vify, vifdf)))
		   
		  colnames(df.cop) <- c('y', .vifvars)
		  colnames(df.cop) <- gsub('@', '.', colnames(df.cop))
		  .vifvars= colnames(df.cop)[!grepl('cop_',  colnames(df.cop))] # remove copula terms
		  .vifvars= colnames(df.cop)[!grepl('cop_',  colnames(df.cop))] # remove benchmark brand terms
		  
		   
		  m<-eval(parse(text=paste0('lm(y~1+', paste(.vifvars[!.vifvars=='y' & !grepl('intercept|[_]dum', .vifvars)],collapse='+'),', data=df.cop)')))
		  data.frame(model_brand=as.character(index[ind[1],]$brand), vars = gsub('.', '@', names(vif(m)), fixed=T), vif=vif(m))
		 }))
		 
		 vifs[, brand := sapply(vars, function(x) strsplit(as.character(x), '_',fixed=T)[[1]][1])]
		 vifs[, variable := sapply(vars, function(x) paste(strsplit(as.character(x), '_',fixed=T)[[1]][-1],collapse='_'))]
		 vifs[, vars:=NULL]
		 setcolorder(vifs, c('model_brand', 'brand', 'variable', 'vif'))
		 
		res$vif <- data.table(data.frame(category=unique(res$specs$category), country=unique(res$specs$country), vifs))
		rm(vifs)
	
	
	########################
	# EXTRACT ELASTICITIES #
	########################

	# retrieve estimated coefficients
		coefs = data.table(res$model@coefficients)[, index:=1:.N][varname%in%setup_x]
		coefs[, lagged:=ifelse(grepl('^lag', varname), 'index_lagged','index_current')]
		setnames(coefs, 'variable', 'var_orig')
		setnames(coefs, 'varname', 'variable')
		coefs[,variable:=gsub('^lag', '', variable)]
		
	
		coefs = dcast(coefs, market_id+brand+variable~lagged, value.var=c('index'))
		if (!"index_lagged" %in% colnames(coefs)) coefs$index_lagged = NA
		
	# calculate means of explanatory variables
	means <- melted_panel[variable%in%coefs$variable, list(mean_x = mean(value,na.rm=T)), by = c('country','category','market_id','brand', 'variable')]
  # calculate average market share
	msaverage <- melted_panel[variable%in%setup_y, list(mean_ms = mean(value)),c('country','category','market_id','brand')]
	
	elasticities =	merge(coefs, means, by = c('market_id', 'brand','variable'))
	elasticities = merge(elasticities, msaverage, by = c('market_id', 'brand', 'category','country'))
	elasticities <- data.table(elasticities)
	
	elasticities[, Nbrands:=length(unique(brand))]
	
	if(lag_heterog==F) elasticities$index_lagms = match(paste0('hom_lagunitsales_sh'), res$model@coefficients$variable)
	if(lag_heterog==T) {
	  elasticities[Nbrands>2, index_lagms := match(paste0(brand,'_lagunitsales_sh'), res$model@coefficients$variable)]
	  elasticities[Nbrands==2, index_lagms := match(paste0(unique(brand[!brand==res$benchmark_brand]),'_lagunitsales_sh'), res$model@coefficients$variable)]
	}
	
	.tmpcoefs = res$model@coefficients$coef
  names(.tmpcoefs)<-paste0('x',1:length(.tmpcoefs))
  .varcovar = res$model@varcovar
  
  # check: can we actually calculate an LT elasticity if the carry over is negative??!?!
  
	if (attraction_model=="MNL") {
	  elasticities[!is.na(index_current), c('elast','elast_se') := deltaMethod(.tmpcoefs, paste0('((1-', mean_ms,')*',mean_x,'*x',index_current,')'),.varcovar), by = c('brand','variable')]
	  elasticities[!is.na(index_current), c('elastlt','elastlt_se') := deltaMethod(.tmpcoefs, paste0('((1-', mean_ms,')*',mean_x,')*((x',index_current,'+', ifelse(is.na(index_lagged),'0', paste0('x',index_lagged)),')/(1-', ifelse(is.na(index_lagms),'0', paste0('x',index_lagms)),'))'),.varcovar), by = c('brand','variable')]
	}
  
	if (attraction_model=="MCI") {
	  elasticities[!is.na(index_current), c('elast','elast_se') := deltaMethod(.tmpcoefs, paste0('((1-', mean_ms,')*x',index_current,')'),.varcovar), by = c('brand','variable')]
	  elasticities[!is.na(index_current), c('elastlt','elastlt_se') := deltaMethod(.tmpcoefs, paste0('(1-', mean_ms,')*((x',index_current,'+', ifelse(is.na(index_lagged),'0', paste0('x',index_lagged)),')/(1-', ifelse(is.na(index_lagms),'0', paste0('x',index_lagms)),'))'),.varcovar), by = c('brand','variable')]
	}
  
	# add elasticities to model output
	res$elast <- elasticities
	
	res$attraction_model <- attraction_model
	
	return(res)
	}
