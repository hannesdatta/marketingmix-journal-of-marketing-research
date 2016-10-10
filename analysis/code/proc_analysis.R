require(car)
require(CADFtest)
require(plm)
require(marketingtools)

###########################
### AUXILARY FUNCTIONS ####
###########################

# decide whether a time series is appropriate (per brand in each market)
	use_ts <- function(x) {
	            # exclude series from model estimation if...
                # (1) series has only one unique value (i.e., no variation at all)
                # (2) variation is the same in 95% of the cases.
                
				tmp = x
                .tmp=table(tmp)
                .tmp=.tmp[which(.tmp==max(.tmp))][1]
                
                if (all(is.na(tmp))) return(F)
                if (length(unique(tmp))<=1) return(F)
                if (.tmp/length(tmp) > .95) return(F)
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
analyze_by_market <- function(i, setup_y, setup_x, setup_xendog=NULL, setup_xendog_signcutoff=NULL, trend = 'all', pval = .05) { # runs the analysis
	
	cat('data preparation\n...')
	
	panel <<- brand_panel[market_id==i]
	panel <<- panel[selected==T]
	panel[, unitsales_sh := unitsales/sum(unitsales,na.rm=T), by=c('date')]
	#dcast(melt(panel, id.vars=c('date', 'brand'))[variable=='selected'], date ~ brand, value.var='value')
	
	#setup_x <- unique(c(setup_x, trend='trend'))
	
	# Create empty result sets, to which elements will be added, depending on the analysis steps below
	res = NULL
	
	if (length(unique(panel$date))<min.t) {
		res$error <- c(res$error, 'minimum_n')
		#stop('minimum n')
		return(res)
		}
	
	res$specs = data.frame(category=unique(panel$category),country=unique(panel$country), brand_id = unique(panel$brand_id),brand=unique(panel$brand), market_id=as.numeric(unique(panel$market_id[1])),stringsAsFactors=F)
		
	eval(parse(text=paste0('panel[!is.na(',setup_y,'), trend:=1:.N, by=c(\'category\', \'country\', \'brand\')]')))
	
	##########################################################################################
	# Determine which series to include, and create object 'melted_panel' with all variables #
	##########################################################################################
	
	# handle missing data (i.e., if a date is not available for one brand, but for others)
	# missing data within a brand cannot be handled with this procedure.

	# recode variables from wdist*100+1 to a 'finally' computed variable
	
	# dependent variable
	eval(parse(text=paste0('panel[, ', names(setup_y), ' := ', setup_y, ']')))
	
	# independent variables
		for (.selvar in seq(along=setup_x)) {		
			eval(parse(text=paste0('panel[, ', names(setup_x)[.selvar], ' := ', setup_x[.selvar], ']')))
			}

	id.vars= c('country','category','market_id', 'brand','date')
		
	suppressWarnings(melted_panel <- melt(panel[,colnames(panel)%in%c(names(setup_y), names(setup_x), id.vars),with=F], id.vars=id.vars))
	
	#########################################
	# Select variables for model estimation #
	#########################################

	# still implement 'forced' time series (e.g., to make sure constant values 
	# are carried along (e.g., pre-adoption variables), or that certain variables 
	# are not tested for unit roots (e.g., trends).
	
	melted_panel[, ts_selected := use_ts(value), by=c('country','category', 'brand', 'variable')]
	
	# kick out non-selected series
	melted_panel <- melted_panel[ts_selected==T]
	melted_panel[, ts_selected:=NULL]
	
	# store included variables for reporting
	incl_vars = as.character(unique(melted_panel$variable))
	res$variables = list(y=names(setup_y)[names(setup_y)%in%incl_vars], x=names(setup_x)[names(setup_x)%in%incl_vars])
	
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
	
	# assert that all variables are measured for the benchmark brand
	if (length(unique(melted_panel[brand==res$benchmark_brand]$variable)) < length(unique(melted_panel$variable))) {
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
	dt <- data.table(dcast(melted_panel, brand+month~variable, value.var=c('value')))
	
	m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
	m_form_heterog = as.formula(paste0('~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
	m_form_index = as.formula(~ brand + month)
	
	dtbb <- attraction_data(formula=m_form, 
							data = dt, 
							heterogenous = m_form_heterog, 
							index = m_form_index, 
							benchmark=res$benchmark_brand,
							model = 'MCI')

	if (any(!setup_xendog%in%names(setup_x))) stop('Endogenous variables MUST be listed in design matrix X, too.')

	###########################
	# Conduct Unit Root Tests #
	###########################

	# on all variables (i.e., DVs (ratio of marketshares), and explanatory variables (log X, and log(y focal), log(y benchmark)), but not the trend and not the copula terms)

	# perform Enders procedure by brand
	cat('performing unit root tests for transformed system\n...')
	dat_by_brand = split(data.frame(y=dtbb@y, dtbb@X), dtbb@individ)

	eq_by_brand=lapply(seq(along=dat_by_brand), function(z) {
		curr_brand = names(dat_by_brand)[z]
		vars = colnames(dat_by_brand[[z]])[!colSums(dat_by_brand[[z]])==0 & !grepl('[_]dum|trend', colnames(dat_by_brand[[z]]))]
		adf=data.frame(t(apply(dat_by_brand[[z]][vars], 2, adf_enders, maxlag=12,pval=pval, season=NULL)))
		adf$variable = rownames(adf)
		rownames(adf) <- NULL
		
		to_be_diffed = adf$variable[which(adf$ur==1)]
		
		out_matrix = dat_by_brand[[z]]
		for (variable in to_be_diffed) {
			out_matrix[,variable] = makediff(out_matrix[,variable])
			}
		
		# Activate / deactiviate trends (trend=='all')
			# add linear trend
			if (!trend=='none') out_matrix <- cbind(out_matrix, trend = seq(from=1, to=nrow(out_matrix)))
			if (trend=='ur') {
			   # keep trend only if UR test of DV indicates a trend
			   if (adf[which(adf$variable=='y'),]$trend == 0) out_matrix$trend <- NULL
			   } 
			
			if ('trend' %in% colnames(out_matrix)) colnames(out_matrix)[which(colnames(out_matrix)=='trend')] <- paste0(curr_brand, '_trend')
		
		# Copula terms
		find_var = grep(paste(setup_xendog,collapse='|'), vars, value=T)
		if (is.null(setup_xendog)) find_var=NULL
		cop_matrix = out_matrix[find_var]
		
		if (ncol(cop_matrix)>=1) {
			cop_matrix = apply(cop_matrix, 2, make_copula)
			colnames(cop_matrix) <- paste0(colnames(cop_matrix), '_cop')
			}
		
		res= list(adf=adf, transformed=cbind(out_matrix, cop_matrix), original = dat_by_brand[[z]], diffed_series = to_be_diffed, brand = curr_brand)
		return(res)
		})

	# summarize ADF results
		adf_sur <- rbindlist(lapply(eq_by_brand, function(x) data.frame(brand = x$brand, x$adf)))
		adf_sur[, ':=' (category=unique(res$specs$category), country=unique(res$specs$country))]
		res$adf_sur <- adf_sur

	# collect transformed equations
		stacked_eq = rbindlist(lapply(eq_by_brand, function(x) x$transformed), fill=TRUE)
		
	# replace NAs for trends/copula to 0
		for (trendvar in grep('[_]trend|[_]cop', colnames(stacked_eq), value=T)) {
			vals = stacked_eq[,trendvar, with=F]
			stacked_eq[,trendvar := ifelse(is.na(get(trendvar)), 0, get(trendvar)), with=F]
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
	
	# rescaling
	# divide variables by their absolute max
	#cat('running rescaling\n')
	#rescale_values = apply(X, 2, function(x) max(abs(x)))
	#div_matrix <- matrix(rep(rescale_values, nrow(X)), byrow=TRUE, ncol=length(rescale_values))
	#X=X/div_matrix


	# perform UR tests on untransformed series (i.e., before transforming them to MCI-base-brand representation)
		cat('performing unit root tests for untransformed system\n...')
		tmp=melted_panel[!grepl('trend|_cop|dv_', variable)] # not on trend and copula terms...
		tmp=split(tmp, paste0(tmp$category, tmp$country, tmp$brand, tmp$variable,'_'))

		adf_untransformed <- rbindlist(lapply(tmp, function(x) {
			res = adf_enders(x$value, maxlag=12, pval = pval, season=NULL)
			data.frame(category=unique(x$category), country=unique(x$country), brand=unique(x$brand), variable=unique(x$variable), rbind(res))
			}))

		res$adf_untransformed <- adf_untransformed

	#################
	# Estimate SURs #
	#################

	# two runs to kick out insignificant copula's
			
	cat('running SUR\n')

	m<-itersur(X=as.matrix(X),Y=as.matrix(Y),index=index, method = "FGLS-Praise-Winsten")

	# check for insignificant copula terms
	ins_coef = data.table(m@coefficients)
	insign_vars <- ins_coef[grepl('[_]cop', variable)&abs(z)<abs(qnorm(pval/2))]$variable
	
	Xs = data.table(X)
	for (var in insign_vars) Xs[, var:=NULL, with=F]
	
	m<-itersur(X=as.matrix(Xs),Y=as.matrix(Y),index=index, method = "FGLS-Praise-Winsten")

	res$model <- m

	# Rescale coefficients
#	retr_coefs <- coef(mest)$coef
	
#	mvarcovar=mest@varcovar

#	if (rescale==TRUE) {
#		cat('transforming back coefficients\n')
#		retr_coefs[seq(length.out=length(rescale_values))] = retr_coefs[seq(length.out=length(rescale_values))] / rescale_values
#		
#		for (ch in seq(length.out=length(rescale_values))) {
#			mvarcovar[ch,] <- mvarcovar[ch,] / rescale_values[ch]
#			mvarcovar[,ch] <- mvarcovar[,ch] / rescale_values[ch]
#			}
#	}

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

	#res$df = df
	#res$melted_panel <- melted_panel
	res$model@coefficients$brand <- unlist(lapply(strsplit(as.character(res$model@coefficients$variable), '_'), function(x) x[1]))
	res$model@coefficients$varname <- unlist(lapply(strsplit(as.character(res$model@coefficients$variable), '_'), function(x) paste(x[-1],collapse='_')))

	return(res)
	}
	


	### temp end here...
	
	# Next steps: "save" the results
	# Compute R2...
	
	# Compute short- and long-term elasticities
	
#	sur = list(coefficients=coef(m), bic=m@bic, predicted=m@predicted, resid=m@resid, dates_brands=m@index, X=m@X, y=m@y)
	
	
	#		colnames(surs[[lags]]$coefficients)[1]<-'orig_var'
			
			
			# Change reporting of coefficient estimates (split up variable names in brand and variable names)
	

	




