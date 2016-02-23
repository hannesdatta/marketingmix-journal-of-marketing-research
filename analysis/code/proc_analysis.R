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
                # (1) series has only one unique value (no variation at all)
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
	
	setup_x <- c(setup_x, trend='trend')
	
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
	incl_vars = unique(melted_panel$variable)
	res$variables = list(y=names(setup_y)[names(setup_y)%in%incl_vars], x=names(setup_x)[names(setup_x)%in%incl_vars])
	
	###################################################################################################
	# Determine benchmark brand for normalization (brand which is closest to the median market share) #
	###################################################################################################
	
	benchmark_brand = panel[, list(sales=sum(unitsales)),by=c('brand')]
	median_share = median(benchmark_brand$sales)
	benchmark_brand[, sales_min_median := sales - median_share]
	
	# pick smallest brand with median sales at a tie
	benchmark_brand[, tag := abs(sales_min_median) == min(abs(sales_min_median))]
	setorderv(benchmark_brand, 'sales', order=1L)
	benchmark_brand = benchmark_brand[tag==T]$brand[1]

	res$benchmark_brand <- benchmark_brand

	# assert that all variables are measured for the benchmark brand
	if (length(unique(melted_panel[brand==benchmark_brand]$variable)) < length(unique(melted_panel$variable))) {
		stop('Not all variables measured for selected benchmark brand.')
		}
		
	#################################
	# Transform variables into logs #
	#################################
	
	# take logarithm of variables
		melted_panel[, used_value := log(value)]
	
	# add dependent variable as the ratio of the focal brand to the benchmark brand
		y_melted_panel = melted_panel[variable %in% names(setup_y)]
		y_melted_panel[, variable:=paste0('dv_',variable)] # label as dependent variable
		y_melted_panel[, used_value := log(value/value[brand==benchmark_brand]), by=c('country','category','date','variable')]
	
		melted_panel = rbind(melted_panel, y_melted_panel)
		
		rm(y_melted_panel)
		
	if (any(!setup_xendog%in%names(setup_x))) stop('Endogenous variables MUST be listed in design matrix X, too.')

	###########################
	# Conduct Unit Root Tests #
	###########################

	# on all variables (i.e., DVs (ratio of marketshares), and explanatory variables (log X, and log(y focal), log(y benchmark)), but not the trend and not the copula terms)
	
	# Elders' and Kenny's procedure
	cat('performing unit root tests\n...')
	tmp=melted_panel[!grepl('trend|cop_', variable) & # not on trend and copula terms...
						    !(grepl('dv_', variable) & brand==benchmark_brand)] #... and not on ratio of benchmark to benchmark (well, this is zero anyways...)
	
	tmp=split(tmp, paste0(tmp$category, tmp$country, tmp$brand, tmp$variable,'_'))
	
	adf_sur <- lapply(tmp, function(x) {
		res = adf_enders(x$used_value, maxlag= max.lag, pval = pval, season=NULL)
		data.frame(category=unique(x$category), country=unique(x$country), brand=unique(x$brand), variable=unique(x$variable), rbind(res))
		})
	adf_sur=rbindlist(adf_sur)
	
	res$adf_sur <- adf_sur

	
	cat('performing unit root tests for untransformed variables\n...')
	tmp=melted_panel[!grepl('trend|cop_|dv_', variable)] # not on trend and copula terms...
	tmp=split(tmp, paste0(tmp$category, tmp$country, tmp$brand, tmp$variable,'_'))

	adf_untransformed <- lapply(tmp, function(x) {
		res = adf_enders(x$value, maxlag= max.lag, pval = pval, season=NULL)
		data.frame(category=unique(x$category), country=unique(x$country), brand=unique(x$brand), variable=unique(x$variable), rbind(res))
		})

	res$adf_untransformed <- rbindlist(adf_untransformed)

	############################################################################
	# Transform variables to their differences if a unit root has been found   #
	############################################################################
	
	melted_panel[, used_value_transf := used_value] # initialize non-UR variables
	
	# add untransformed y variables (to be used to recompute R2 for equations in first differences)
	ydiff_melted_panel = melted_panel[variable%in%paste0('dv_',names(setup_y))]
	ydiff_melted_panel[, variable:=paste0('untrans_',variable)]
	melted_panel <- rbind(melted_panel, ydiff_melted_panel)		
	
	rm(ydiff_melted_panel)
	
	for (x in 1:nrow(adf_sur)) {
		tmp=adf_sur[x]
		if (tmp$ur==1) melted_panel[which(country==as.character(tmp$country)&
										  category==as.character(tmp$category)&
										  brand==as.character(tmp$brand)&
										  variable==as.character(tmp$variable)),

										  used_value_transf := makediff(used_value), 
										  
										  by=c('country','category','brand','variable')]
		}

	# Assert whether any transformation is going 'nuts'
	if (length(which(abs(melted_panel$used_value_transf)==Inf))>0) stop (paste('Variable normalization error-prone. Potential error variables: ', unique(melted_panel[abs(used_value_transf)==Inf]$variable)))
	
	#################################
	# Activate / deactiviate trends #
	#################################
	
	if (trend=='all') {
		# leave in all trends; no change
		}
	if (trend=='ur') {
		# kick out trends, depending on outcome of trend in UR test
		tmpm <- adf_sur[!brand==benchmark_brand & grepl('dv_', variable)]
		setkey(tmpm, brand)
		setkey(melted_panel, brand)
		melted_panel[, dv_trend:=999]
		melted_panel[tmpm, dv_trend := i.trend]
		melted_panel <- melted_panel[!(variable=='trend' & dv_trend==0)]	
		}
	if (trend=='none') {
		# kick out all trends
		melted_panel <- melted_panel[!(variable=='trend')]	
		}

	################################
	# Apply copula transformations #
	################################
	
	# Add copula series to the data and verify normality
		if (length(setup_xendog>0)) {
			copula_melted_panel = melted_panel[variable%in%setup_xendog]
			copula_melted_panel[, variable:=paste0('cop_',variable)]
			
			# verify normality
			copula_normality <- copula_melted_panel[, list(shapiro_pval = shapiro.test(used_value_transf)$p), by=c('category', 'country', 'brand', 'variable')]
			
			# apply copula transformation
			copula_melted_panel[, used_value_transf := make_copula(used_value_transf),by=c('category', 'country', 'brand', 'variable')]

			# add to panel
			melted_panel = rbind(melted_panel, copula_melted_panel)
			rm(copula_melted_panel)
			
			res$copula_normality <- copula_normality
		}

	#################################################################
	# Transform variables to brand-level equations required for SUR #
	#################################################################

	# function to create y lags
		make_ylags <- function(series, lags) {
			ret=cbind(sapply(seq(length.out=lags), function(l) makelag(series,l)))
			colnames(ret) <- paste0('ylag_', seq(length.out=lags))
			rownames(ret)<-NULL
			return(ret)
		}

	# start with transforming
	melted_panel[, exclude:= FALSE] # initialize that all variables will be included

	# code to use same observations
#	date_sel <- tmp[complete.cases(tmp)]$date
#	tmp <- tmp[date%in%date_sel]
	
	brands=unique(melted_panel$brand)
	
	transf_fkt <- function(lags) {
		lapply(brands[!brands==benchmark_brand], function(brand) {

		# do the hack here: equalize variables etc. / and keep only variables which have not yet been excluded.
		tmp = dcast.data.table(melted_panel[which(exclude==F)], date ~ brand+variable, value.var = 'used_value_transf')
		
		sel_dv = paste0(brand,'_','dv_',setup_y)
		sel_dv_levels = paste0(brand,'_untrans_','dv_',setup_y)
		sel_x = colnames(tmp)[colnames(tmp)%in%c(paste0(brand, '_', names(setup_x)), paste0(brand,'_cop_', setup_xendog))]
		sel_xbench = colnames(tmp)[colnames(tmp)%in%c(paste0(benchmark_brand, '_', names(setup_x)), paste0(benchmark_brand,'_cop_', setup_xendog))]
		sel_xbench <- grep('trend', sel_xbench, value=T,invert=T)
		
		interc = cbind(rep(1, nrow(tmp)))
		colnames(interc) <- paste0(brand, '_intercept')
		
		# add lagged market shares for focal and benchmark
		focal_lags = make_ylags(unlist(tmp[,paste0(brand, '_', names(setup_y)),with=F]), lags)
		colnames(focal_lags) <- paste0(brand, '_', colnames(focal_lags))
	
		bench_lags = make_ylags(unlist(tmp[,paste0(benchmark_brand, '_', names(setup_y)),with=F]), lags)
		colnames(bench_lags) <- paste0(benchmark_brand , '_', colnames(bench_lags))
		
		DV = tmp[,sel_dv,with=F]
		DVlevels = tmp[,sel_dv_levels,with=F]
		X = cbind(interc, tmp[,sel_x,with=F], focal_lags)
		Xbench = cbind(tmp[,sel_xbench,with=F], bench_lags)
		
		complobs <- !apply(X,1,function(x) any(is.na(x))) & !apply(DV,1,function(x) any(is.na(x))) & !apply(Xbench,1,function(x) any(is.na(x))) 
		
		# make sure to account for missing observations in each series; note that I get rid of the
		# observations which are missing PER BRAND; so if one has missings on a given date where others ARE availbale, that is fine.
		X = X[complobs,]
		Xbench = Xbench[complobs,]
		DV = DV[complobs]
		DVlevelslag = makelag(unlist(DVlevels))[complobs]
		DVlevels = unlist(DVlevels)[complobs]
		
		dates = tmp[complobs,]$date

		list(X=X, Xbench=Xbench, DV=DV, DVlevels = DVlevels, DVlevelslag = DVlevelslag, dates=dates, brand = brand)
		})
	}
	
	#################
	# Estimate SURs #
	#################
	
#	source('proc_sur.R')
	
	surs <- NULL
	
	for (lags in seq(from=1, to=1)) { #max.lag currently set to 1 for y lags
		
		for (run in 1:2) { # at max two runs; to kick out insignificant copula's
			
			cat(paste0('performing SUR ', run, '\n...'))
		
			df <- transf_fkt(lags=lags)
			
			X = cbind(bdiag(lapply(df, function(x) as.matrix(x$X))),
					  -1 * do.call('rbind', lapply(df, function(x) as.matrix(x$Xbench))))
			DV = cbind(unlist(lapply(df, function(x) x$DV)))
			
			cnames = c(unlist(lapply(df, function(x) c(colnames(x$X)))), colnames(df[[1]]$Xbench))
			for (brand in c(brands)) cnames<-gsub(paste0(brand, '_'), paste0(brand, '@'), cnames)
			
			colnames(X) <- cnames
			rownames(X) <- NULL
			
			dates_brands <- data.frame(rbindlist(lapply(df, function(x) data.frame(brand=x$brand, date=x$date))))

			m<-itersur(X=as.matrix(X),Y=as.matrix(DV),index=data.frame(date=dates_brands$date, brand=dates_brands$brand))
			
			surs[[lags]] <- list(coefficients=coef(m), bic=m@bic, predicted=m@predicted, resid=m@resid, dates_brands=m@index, X=m@X, y=m@y)
			colnames(surs[[lags]]$coefficients)[1]<-'orig_var'
			
			
			# Change reporting of coefficient estimates (split up variable names in brand and variable names)
			surs[[lags]]$coefficients$brand <- unlist(lapply(strsplit(as.character(surs[[lags]]$coefficients$orig_var), '@'), function(x) x[1]))
			surs[[lags]]$coefficients$variable <- unlist(lapply(strsplit(as.character(surs[[lags]]$coefficients$orig_var), '@'), function(x) x[2]))

			if (!is.null(setup_xendog_signcutoff) & run == 1 & length(setup_xendog)>0) {
				# Check for significance cutoff
				temp.sur = data.table(surs[[lags]]$coefficients)
			
				# perform analysis again, but kick out insign. copula terms
				cop_terms <- temp.sur[grepl('cop_', variable)]
				cop_terms[, tval := coef/se]
				cop_terms[, pval := (1-pnorm(abs(tval)))*2]
				cop_retain <- cop_terms[which(pval<=setup_xendog_signcutoff)] # two tailed test, according to setup_xendog_signcutoff
				cop_delete <- cop_terms[which(pval>setup_xendog_signcutoff)]
				#cop_delete[which(brand=='benchmark'), brand:=benchmark_brand]
				setkey(cop_delete, brand, variable)
				setkey(melted_panel, brand, variable)
				
				melted_panel[cop_delete, exclude:=i.pval>setup_xendog_signcutoff]
				
				setkey(res$copula_normality, brand, variable)
				
				res$copula_normality[cop_delete, used := i.pval <= setup_xendog_signcutoff]
				res$copula_normality[which(is.na(used)), used:=T]
				next
				} else {
				# No check needed
				break
				}
			}
		}


	bics = unlist(lapply(surs, function(x) x$bic))
	res$all_bics = bics
	
	res$sur = surs[[which(bics==min(bics))]]

	# Compute R2s by model
			pred <- data.table(cbind(dates_brands, 
									 DVlevels= unlist(lapply(df, function(x) x$DVlevels)),
									 DVlevelslag=unlist(lapply(df, function(x) x$DVlevelslag)),
									 DV=unlist(lapply(df, function(x) x$DV)),
									 predicted=as.matrix(res$sur$predicted),resid=as.matrix(res$sur$resid)))
			# Has the model been estimated in levels or first differences?
			adftmp = adf_sur[grepl('dv_',variable)]
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

			res$sur$R2 <- R2
			rm(adftmp)

		# Compute VIF values
		vifs = rbindlist(lapply(split(1:nrow(dates_brands), as.character(dates_brands$brand)), function(ind) {
			vifdf=X[ind,]
			# drop zero columns
			vifdf = vifdf[,which(!colSums(vifdf)==0)]
			vify = unlist(DV)[ind]
			
		   .vifvars = colnames(vifdf)
		   df.cop = data.frame(as.matrix(cbind(y=vify, vifdf)))
		   
		   colnames(df.cop) <- c('y', .vifvars)
		   colnames(df.cop) <- gsub('@', '.', colnames(df.cop))
		   .vifvars= colnames(df.cop)[!grepl('cop_',  colnames(df.cop))] # remove copula terms
		   
		   m<-eval(parse(text=paste0('lm(y~1+', paste(.vifvars[!.vifvars=='y' & !grepl('intercept', .vifvars)],collapse='+'),', data=df.cop)')))
		   data.frame(model_brand=as.character(dates_brands[ind[1],]$brand), vars = gsub('.', '@', names(vif(m)), fixed=T), vif=vif(m))
		 }))
		 
		 vifs[, brand := sapply(vars, function(x) strsplit(as.character(x), '@',fixed=T)[[1]][1])]
		 vifs[, variable := sapply(vars, function(x) strsplit(as.character(x), '@',fixed=T)[[1]][2])]
		 vifs[, vars:=NULL]
		 setcolorder(vifs, c('model_brand', 'brand', 'variable', 'vif'))
		 
		res$sur$vif <- data.table(data.frame(category=unique(res$specs$category), country=unique(res$specs$country), vifs))
		rm(vifs)

	res$df = df
	res$melted_panel <- melted_panel
	return(res)
	}

	

# Analysis by country and category
analyze_category <- function(i, setup_y, setup_x, setup_xendog=NULL, setup_xendog_signcutoff=NULL, trend = 'all', pval = .05) { # runs the analysis
	
	cat('data preparation\n...')
	
	panel <<- category_panel[market_id==i]
	
	setup_x <- c(setup_x, trend= 'trend')
	
	# Create empty result sets, to which elements will be added, depending on the analysis steps below
	res = NULL
	
	if (length(unique(panel$date))<min.t) {
		res$error <- c(res$error, 'minimum_n')
		#stop('minimum n')
		return(res)
		}
	
	res$specs = data.frame(category=unique(panel$category),country=unique(panel$country), market_id=as.numeric(unique(panel$market_id[1])),stringsAsFactors=F)
		
	eval(parse(text=paste0('panel[!is.na(',setup_y,'), trend:=1:.N, by=c(\'category\', \'country\')]')))
	
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

	id.vars= c('country','category','market_id', 'date')
		
	suppressWarnings(melted_panel <- melt(panel[,colnames(panel)%in%c(names(setup_y), names(setup_x), id.vars),with=F], id.vars=id.vars))
		
	#########################################
	# Select variables for model estimation #
	#########################################

	# still implement 'forced' time series (e.g., to make sure constant values 
	# are carried along (e.g., pre-adoption variables), or that certain variables 
	# are not tested for unit roots (e.g., trends).
	
	melted_panel[, ts_selected := use_ts(value), by=c('country','category', 'variable')]
	
	# kick out non-selected series
	melted_panel <- melted_panel[ts_selected==T]
	melted_panel[, ts_selected:=NULL]
	
	# store included variables for reporting
	incl_vars = unique(melted_panel$variable)
	res$variables = list(y=names(setup_y)[names(setup_y)%in%incl_vars], x=names(setup_x)[names(setup_x)%in%incl_vars])

	#################################
	# Transform variables into logs #
	#################################
	
	# take logarithm of variables
	melted_panel[, used_value := log(value)]
	
	# Create copula terms later on!
	
	###########################
	# Conduct Unit Root Tests #
	###########################

	# on all variables, but not the trend and not the copula terms)
	
	# Elders' and Kenny's procedure
	cat('performing unit root tests\n...')
	tmp=melted_panel[!grepl('trend', variable)]
	tmp=split(tmp, as.character(tmp$variable))
	j<<-0
	adftest <- rbindlist(lapply(tmp, function(x) {
		j<<-j+1
		res = adf_enders(x$used_value, maxlag= max.lag, pval = pval, season=NULL)
		data.frame(category=unique(x$category), country=unique(x$country),variable=unique(x$variable), rbind(res))
		}))
		
	
	res$adf <- adftest
	
	# Q: Harald: what about integration of order 2?!

	############################################################################
	# Transform variables to their differences if a unit root has been found   #
	############################################################################
	
	melted_panel[, used_value_transf := used_value] # initialize non-UR variables
	# add untransformed y variables (to be used to recompute R2 for equations in first differences)
	
	for (x in 1:nrow(adftest)) {
		tmp=adftest[x]
		if (tmp$ur==1) melted_panel[which(country==as.character(tmp$country)&
										  category==as.character(tmp$category)&
										  variable==as.character(tmp$variable)),

										  used_value_transf := makediff(used_value_transf), 
										  
										  by=c('country','category','variable')]
		}

	#################################
	# Activate / deactiviate trends #
	#################################
	
	if (trend=='all') {
		# leave in all trends; no change
		}
	if (trend=='ur') {
		# kick out trends, depending on outcome of trend in UR test
		if (adftest[variable==names(setup_y)]$trend==0) # no trend, kkick out
			melted_panel <- melted_panel[!(variable=='trend')]
		}
	if (trend=='none') {
		# kick out all trends
		melted_panel <- melted_panel[!(variable=='trend')]
		}

	################################
	# Apply copula transformations #
	################################
		
	# Add copula series to the data and verify normality

	if (length(setup_xendog>0)) {
			copula_melted_panel = melted_panel[variable%in%setup_xendog]
			copula_melted_panel[, variable:=paste0('cop_',variable)]
			
			# verify normality
			copula_normality <- copula_melted_panel[, list(shapiro_pval = shapiro.test(used_value_transf)$p), by=c('category', 'country', 'variable')]
			
			# apply copula transformation
			copula_melted_panel[, used_value_transf := make.copula(used_value_transf),by=c('category', 'country', 'variable')]

			# add to panel
			melted_panel = rbind(melted_panel, copula_melted_panel)
			rm(copula_melted_panel)
			
			res$copula_normality <- copula_normality
		}

	##################
	# Estimate model #
	##################

	dt <- data.table(dcast(melted_panel, country+category+market_id+date~variable, value.var = 'used_value_transf'))
	setorder(dt, date)
	
	# Create lags
	dt[, lag_unitsales := makelag(unitsales,1)]
	
	dty <- data.table(dcast(melted_panel[variable==names(setup_y)], date~variable, value.var = 'used_value'))
	setnames(dty, names(setup_y), 'exclude_ylevels')

	setkey(dt, date)
	setkey(dty, date)

	dt <- merge(dt, dty, by =c('date'))
	dt[, exclude_lagylevels := makelag(exclude_ylevels)]
	
	dt <- dt[complete.cases(dt)]
	
	for (run in 1:2) { # at max two runs; to kick out insignificant copula's
			
			cat(paste0('performing OLS ', run, '\n...'))
			
			.vars <- colnames(dt)[!grepl('exclude[_]', colnames(dt)) & !colnames(dt) %in% c('date', 'country', 'category', 'market_id', names(setup_y))]
				
			m <- eval(parse(text=paste0('lm(', setup_y, ' ~ 1 + ', paste0(.vars, collapse='+'), ', data = dt)')))
			vifs <- NULL
			vifs$complete <- vif(m)
			
			m2 <- eval(parse(text=paste0('lm(', setup_y, ' ~ 1 + ', paste0(.vars[!grepl('cop_', .vars)], collapse='+'), ', data = dt)')))
			vifs$withoutcopula <- vif(m2)
			
			names(m$coefficients)[which(names(m$coefficients)=='(Intercept)')] <- 'intercept'
				
			if (!is.null(setup_xendog_signcutoff) & run == 1 & length(setup_xendog)>0) {
				# Check for significance cutoff
				temp.m = data.table(rownames(summary(m)$coefficients), summary(m)$coefficients[,1:2])
				setnames(temp.m, c('variable', 'coef','se'))
				temp.m[,variable:=tolower(gsub('[(]|[)]', '', variable))]
				
				# perform analysis again, but kick out insign. copula terms
				cop_terms <- temp.m[grepl('cop_', variable)]
				cop_terms[, tval := coef/se]
				cop_terms[, pval := (1-pnorm(abs(tval)))*2]
				cop_retain <- cop_terms[which(pval<=setup_xendog_signcutoff)] # two tailed test, according to setup_xendog_signcutoff
				cop_delete <- cop_terms[which(pval>setup_xendog_signcutoff)]
				setkey(cop_delete, variable)
				setkey(melted_panel, variable)
				
				dt <- dt[,!colnames(dt)%in%cop_delete$variable, with=F]
				
				melted_panel[cop_delete, exclude:=i.pval>setup_xendog_signcutoff]
				
				setkey(res$copula_normality, variable)
				
				res$copula_normality[cop_terms, used := i.pval <= setup_xendog_signcutoff]
				res$copula_normality[which(is.na(used)), used:=T]
				next
				} else {
				# No check needed
				break
				}
			}

	res$model <- m

	pred <- predict(m)
	actual <- m$residuals+pred
	
	estim_ur = adftest[variable==setup_y]$ur
	
	if (estim_ur==1) { # model has been estimated in differences
		R2=data.frame(UR = unique(estim_ur), R2levels = cor(pred+dt$exclude_lagylevels, dt$exclude_ylevels)^2, R2 = cor(pred, dt[, setup_y,with=F])^2)
		} else { # model has been estimated in levels
		R2=data.frame(UR = unique(estim_ur), R2levels = cor(pred, dt[, setup_y,with=F])^2, R2 = cor(pred, dt[, setup_y,with=F])^2)
		}

	
	res$df = dt
	res$vifs = vifs
	res$R2 <- R2

	return(res)
	}
