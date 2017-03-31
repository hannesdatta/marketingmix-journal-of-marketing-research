require(car)
#require(CADFtest)
#require(plm)
#require(marketingtools)

###########################
### AUXILARY FUNCTIONS ####
###########################

# decide whether a time series is appropriate (per brand in each market)
	use_ts <- function(x) {
	            # exclude series from model estimation if...
                # (1) series has only one unique value (i.e., no variation at all)
                # (2) variation is the same in 95% of the cases.
                
				if (all(is.na(x))) return(F)
				
				tmp = x
                .tmp=table(tmp)
                .tmp=.tmp[which(.tmp==max(.tmp))][1]
                
                
                if (length(unique(tmp))<=1) return(F)
				# at least X months of non-zero observations
				if (length(which(!tmp==min(tmp)))<12) return(F)
				# if (.tmp/length(tmp) > .95) return(F)
                return(T)
                }

# Analysis steps:
# - Create log normalization
# - Run ADF panel unit root tests on log-normalized series
#   Correctly handle missing values!
# - Transform log normalizations [log(mkt_i/mkt_0)] to their first differences, if unit root is found
# - Create Gaussian Copulas: either on normalized [log(mkt_i/mkt_0)]
#   or first-differenced normalized series

require(reshape2)
require(Matrix)


# Analysis by country and category
analyze_by_market <- function(i, setup_y, setup_x, setup_endogenous=NULL, trend = 'all', pval = .05, max.lag = 12, min.t = 36) { # runs the analysis
	
		cat('data preparation\n...')
		
		panel <<- brand_panel[market_id==i]
		
		# Create empty result sets, to which elements will be added, depending on the analysis steps below
		res = NULL
		
		if (length(unique(panel$date))<min.t) {
			res$error <- c(res$error, 'minimum_n')
			return(res)
			}
		
		res$specs = data.frame(category=unique(panel$category),country=unique(panel$country), brand_id = unique(panel$brand_id),brand=unique(panel$brand), market_id=as.numeric(unique(panel$market_id[1])),stringsAsFactors=F)
			
		eval(parse(text=paste0('panel[!is.na(',setup_y,'), trend:=1:.N, by=c(\'category\', \'country\', \'brand\')]')))
		
		##########################################################################################
		# Determine which series to include, and create object 'melted_panel' with all variables #
		##########################################################################################
		
		# handle missing data (i.e., if a date is not available for one brand, but for others)
		# missing data within a brand cannot be handled with this procedure.

		# instruments
		id_vars= c('country','category','market_id', 'brand','date', 'quarter')
			
		suppressWarnings(melted_panel <- melt(panel[,colnames(panel)%in%c(setup_y, setup_x, id_vars),with=F], id.vars=id_vars))
		# I surpress warnings here, which would show that variable types are not the same (coercing it to a fine grained measure which is good enough).
		
		#########################################
		# Select variables for model estimation #
		#########################################

		# check whether variables qualify for estimation (e.g., minimum number of observations)
		melted_panel[, ts_selected := use_ts(value), by=c('country','category', 'brand', 'variable')]
		
		# kick out non-selected series and remove column
		melted_panel <- melted_panel[ts_selected==T][, ts_selected:=NULL]
		
		if (length(setup_endogenous)>0) {
		# add copula terms
		cop_terms = melted_panel[variable%in%setup_endogenous]
		
		res$cop_nonnormal = cop_terms[, list(shap_pval = shapiro.test(value)$p), by = c('category', 'country', 'brand', 'variable')]
		
		cop_terms[, value := make_copula(value), by = c('country','category','brand','variable')]
		cop_terms[, variable := paste0('cop_', variable)]
		
		melted_panel <- rbind(melted_panel, cop_terms)
		}
				
		# store included variables for reporting
		incl_vars = as.character(unique(melted_panel$variable))
		res$variables = list(y=setup_y[setup_y%in%incl_vars], x=incl_vars[incl_vars %in% c(setup_x, paste0('cop_', setup_x))], endog = setup_endogenous)
		rm(incl_vars)
		
		################################################################################
		# Determine benchmark brand for normalization (the one with most observations) #
		################################################################################
		
		tmp1 = panel[, list(obs=sum(selected_t_brand)),by=c('brand')]
		tmp2 = melted_panel[, list(nvars = length(unique(variable))), by=c('brand')]
		setkey(tmp1, brand)
		setkey(tmp2, brand)
		
		tmp <- tmp1[tmp2]
		setorderv(tmp, c('obs', 'nvars'), order=-1L)
		
		res$benchmark_brand = tmp[match(max(tmp$obs), obs)]$brand
		
		rm(tmp, tmp1, tmp2)
		
		# assert that all variables are measured for the benchmark brand
		required_vars = grep(paste0(setup_endogenous,collapse='|'), unique(melted_panel$variable), value = TRUE, invert = TRUE)
		
		if (!all(required_vars %in% unique(melted_panel[brand==res$benchmark_brand]$variable))) {
			stop('Not all variables measured for selected benchmark brand.')
			}
		
		############################################
		# Transform variables to estimation system #
		############################################
		
		# add lagged dependent variable
		lagged_ms = melted_panel[variable==res$variables$y]
		lagged_ms[, value := makelag(value), by=c('brand')]
		lagged_ms[, variable:='lagunitsales_sh']
		melted_panel <- rbind(melted_panel, lagged_ms)
		
		# create month variable from dates
		melted_panel[, month := as.numeric(as.factor(date))]
		res$melted_panel<-melted_panel
		
		# transform system to base-brand representation
		dt <- data.table(dcast(melted_panel, brand+month+quarter~variable, value.var=c('value')))
		
		m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
		m_form_heterog = as.formula(paste0('~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
		
		# no lag marketshares
		#m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ''))
		#m_form_heterog = as.formula(paste0('~ ', paste0(res$variables$x, collapse = '+'), ''))
		
		m_form_index = as.formula(~ brand + month)
		
		dtbb <- suppressWarnings(attraction_data(formula=m_form, 
								data = dt, 
								heterogenous = m_form_heterog, 
								index = m_form_index, 
								benchmark=res$benchmark_brand,
								model = 'MNL'))

		
		# if there are only two brands, remove lagged market share of base brand
		#if (length(unique(dtbb@input$index$brand))==2) {
		#	X = dtbb@X
		#	delcol = which(colnames(X)==paste0(dtbb@benchmark, '_lagunitsales_sh'))
		#	X <- X[, -delcol]
		#	dtbb@X <- X
		#	rm(X)
		#	}

		###########################
		# Conduct Unit Root Tests #
		###########################

		# Procedure to conduct equation-by-equation operations (e.g., differencing in case of non-stationarity, predicting endogenous regressors using ivreg2)
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


		eq_by_brand=lapply(seq(along=dat_by_brand), function(z) {
			# perform Enders procedure by brand
			# on all variables (i.e., DVs (ratio of marketshares), and explanatory variables (log X, and log(y focal), log(y benchmark)), but not the trend and not the copula terms)
			curr_brand = names(dat_by_brand)[z]
			vars = setdiff(colnames(dat_by_brand[[z]])[!colSums(dat_by_brand[[z]])==0 & !grepl('[_]dum|trend', colnames(dat_by_brand[[z]]))],'period')
			adf=data.frame(t(apply(dat_by_brand[[z]][vars], 2, adf_enders, maxlag=12,pval=pval, season=NULL)))
			adf$variable = rownames(adf)
			rownames(adf) <- NULL

			dat_by_brand[[z]][vars][,1]
			
	#		1) conduct UR op y; 
	#		   o if trend --> add trend variable
	#		   o if UR present --> take difference in y
	#		2) conduct UR op x:
	#		   o if UR presnet and UR present in Y --> difference of both  (difference of the copulas)
	#		   o if UR present and y does not have UR --> difference X, but take level copulas


			out_matrix = dat_by_brand[[z]]
			
			takediff= 'alwaysdiff'
			
			if (takediff=='flexible') {
				ydiff = data.table(adf)[variable=='y']$ur
				ytrend = data.table(adf)[variable=='y']$trend
				
				if (ytrend==1) out_matrix <- cbind(out_matrix, trend = seq(from=1, to=nrow(out_matrix)))
				
				if ('trend' %in% colnames(out_matrix)) {
					colnames(out_matrix)[which(colnames(out_matrix)=='trend')] <- paste0(curr_brand, '_trend')
					vars <- c(vars, paste0(curr_brand, '_trend'))
				}
					
					
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
			to_be_diffed = adf$variable
			}
			
			# execute differencing
			for (variable in to_be_diffed) {
				out_matrix[,variable] = makediff(out_matrix[,variable])
				}
			
			res= list(adf=adf, transformed=cbind(out_matrix), original = dat_by_brand[[z]], diffed_series = to_be_diffed, brand = curr_brand)#, ivreg2 = stata_data_out)
			return(res)
			})


		# summarize ADF results
			adf_sur <- rbindlist(lapply(eq_by_brand, function(x) data.frame(brand = x$brand, x$adf)))
			adf_sur[, ':=' (category=unique(res$specs$category), country=unique(res$specs$country))]
			res$adf_sur <- adf_sur
	
		# collect transformed equations
			stacked_eq = rbindlist(lapply(eq_by_brand, function(x) x$transformed), fill=TRUE)

			stacked_eq[, period:=NULL]
			
		# replace NAs for trends/copula to 0
			for (trendvar in grep('[_]trend|[_]cop', colnames(stacked_eq), value=T)) {
				vals = stacked_eq[,trendvar, with=F]
				stacked_eq[, (trendvar) := ifelse(is.na(get(trendvar)), 0, get(trendvar))]
				}
			
		# separate DVs from IVs
			X = stacked_eq[,!colnames(stacked_eq)%in%'y',with=F]
			Y = data.frame(stacked_eq[,'y',with=F])
			index=data.frame(date=dtbb@period,brand=dtbb@individ)

		# kick out incomplete ('diffed') vars, i.e., their first observation (as it is NA)
			complete = complete.cases(data.frame(X,Y,index))
			X=as.matrix(X[complete,])
			Y=as.matrix(Y[complete,])
			index=data.frame(index[complete,])

		#####################	
		# quarterly dummies #
		#####################
		
		# create dummy dataset
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
			
			
			
		if(0){
		# perform UR tests on untransformed series (i.e., before transforming them to MCI-base-brand representation)
		cat('performing unit root tests for untransformed system\n...')
			tmp=melted_panel[!grepl('trend|_cop|dv_', variable)] # not on trend and copula terms...
			tmp=split(tmp, paste0(tmp$category, tmp$country, tmp$brand, tmp$variable,'_'))

			adf_untransformed <- rbindlist(lapply(tmp, function(x) {
				res = adf_enders(x$value, maxlag=12, pval = pval, season=NULL)
				data.frame(category=unique(x$category), country=unique(x$country), brand=unique(x$brand), variable=unique(x$variable), rbind(res))
				}))

			res$adf_untransformed <- adf_untransformed
		}
		
		#################
		# Estimate SURs #
		#################

		# RUN 1
			
		cat('running SUR (run 1)\n')
		
		rescale = TRUE
		
		
		# rescale X
		if (rescale==T) {
			rescale_values = apply(X, 2, function(x) max(abs(x)))
			div_matrix <- matrix(rep(rescale_values, nrow(X)), byrow=TRUE, ncol=length(rescale_values))
			X=X/div_matrix
			}
			
		m<-itersur(X=as.matrix(X),Y=as.matrix(Y),index=index, method = "FGLS-Praise-Winsten", maxiter=1000)
		if (m@iterations==1000) stop('error with iterations')
	
	m@coefficients
	
	# check for insignificant copula terms
	pval_cop=.1
	insign_vars <- with(m@coefficients, which(grepl('[_]cop', variable)&abs(z)<abs(qnorm(pval_cop/2))))
	keep_vars = setdiff(1:nrow(m@coefficients), insign_vars)
	#keep_vars = 1:nrow(m@coefficients)
	
	m<-itersur(X=X[, keep_vars],Y=as.matrix(Y),index=index, method = "FGLS-Praise-Winsten", maxiter=1000)
	if (m@iterations==1000) stop('error with iterations')
	
	m@coefficients
	
	# backscale
	if (rescale == T) {
		#m@coefficients
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
	

	res$model <- m
	
	
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

	# Compute VIF values
		vifs = rbindlist(lapply(split(1:nrow(index), as.character(index$brand)), function(ind) {
			vifdf=as.matrix(Xs)[ind,]
		
			# drop zero columns
			vifdf = vifdf[,which(!colSums(vifdf)==0)]
			vify = unlist(Y)[ind]
			
		   .vifvars = colnames(vifdf)
		   df.cop = data.frame(as.matrix(cbind(y=vify, vifdf)))
		   
		   colnames(df.cop) <- c('y', .vifvars)
		   colnames(df.cop) <- gsub('@', '.', colnames(df.cop))
		   .vifvars= colnames(df.cop)[!grepl('cop_',  colnames(df.cop))] # remove copula terms
		   
		   m<-eval(parse(text=paste0('lm(y~1+', paste(.vifvars[!.vifvars=='y' & !grepl('intercept|[_]dum', .vifvars)],collapse='+'),', data=df.cop)')))
		   data.frame(model_brand=as.character(index[ind[1],]$brand), vars = gsub('.', '@', names(vif(m)), fixed=T), vif=vif(m))
		 }))
		 
		 vifs[, brand := sapply(vars, function(x) strsplit(as.character(x), '_',fixed=T)[[1]][1])]
		 vifs[, variable := sapply(vars, function(x) paste(strsplit(as.character(x), '_',fixed=T)[[1]][-1],collapse='_'))]
		 vifs[, vars:=NULL]
		 setcolorder(vifs, c('model_brand', 'brand', 'variable', 'vif'))
		 
		res$vif <- data.table(data.frame(category=unique(res$specs$category), country=unique(res$specs$country), vifs))
		rm(vifs)
	}
	
	res$model@coefficients$brand <- unlist(lapply(strsplit(as.character(res$model@coefficients$variable), '_'), function(x) x[1]))
	res$model@coefficients$varname <- unlist(lapply(strsplit(as.character(res$model@coefficients$variable), '_'), function(x) paste(x[-1],collapse='_')))
	
	
	########################
	# EXTRACT ELASTICITIES #
	########################

	# calculate means of explanatory variables
	means <- melted_panel[variable%in%setup_x, list(mean_var = mean(value)), by = c('country','category','market_id','brand', 'variable')]
	msaverage <- melted_panel[variable%in%setup_y, list(mean_ms = mean(value)),c('country','category','market_id','brand')]
		# note: does not add up to 1.
	coefs = data.table(res$model@coefficients)[varname%in%setup_x]
	setnames(coefs, 'variable', 'var_orig')
	setnames(coefs, 'varname', 'variable')
	
	elasticities <- merge(means, coefs, by=c('variable', 'brand'), all.x=T, all.y=F)
	elasticities <- merge(elasticities, msaverage, by=c('brand', 'category','country', 'market_id'))
	
		
	#if (attr_spec=="MNL") {
			elasticities[, elast := coef * (1-mean_ms) * mean_var]
			elasticities[, elast_se := se * (1-mean_ms) * mean_var]
	#		}
			
	#	if (attr_spec=="MCI") {
	#		elasticities[, elast := coef * (1-mean_ms)]
	#		elasticities[, elast_se := se * (1-mean_ms)]
	#		}
			
	# summary
	
	res$elast <- elasticities
	
	return(res)
	}
	
