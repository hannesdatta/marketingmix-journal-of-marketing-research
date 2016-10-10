#     _____    __   _  __                               _                 _       
#    / ____|  / _| | |/ /       /\                     | |               (_)      
#   | |  __  | |_  | ' /       /  \     _ __     __ _  | |  _   _   ___   _   ___ 
#   | | |_ | |  _| |  <       / /\ \   | '_ \   / _` | | | | | | | / __| | | / __|
#   | |__| | | |   | . \     / ____ \  | | | | | (_| | | | | |_| | \__ \ | | \__ \
#    \_____| |_|   |_|\_\   /_/    \_\ |_| |_|  \__,_| |_|  \__, | |___/ |_| |___/
#                                                            __/ |                
#                                                           |___/                 
#
#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|



######################################################
# SIMULATING MARKTE SHARES FROM MCI ATTRACTION MODEL #
######################################################
	
	require(MASS)
	require(lattice)
	
	
	res = results_brands[[1]][[1]]
	

	L=1000 # Number of simulation draws
	nperiods = 40 # periods used in simulation
	
	# Retrieve data set that was used to estimate the model
	res$melted_panel
	
	# Transform system to base-brand representation
	dt <- data.table(dcast(res$melted_panel, brand+month~variable, value.var=c('value')))
	
	# Compute means
	init_means = dt[, lapply(.SD, mean, na.rm=T), .SDcols = setdiff(colnames(dt), c('month', 'brand')), by = c('brand')]
	

	
	simulate <- function(sim_set) {

	# transform to base-brand representation
	m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
	m_form_heterog = as.formula(paste0('~ ', paste0(res$variables$x, collapse = '+'), ' + lagunitsales_sh'))
	m_form_index = as.formula(~ brand + month)

	dtbb_sim <- attraction_data(formula=m_form, 
							data = sim_set, 
							heterogenous = m_form_heterog, 
							index = m_form_index, 
							benchmark=res$benchmark_brand,
							model = 'MCI')

	# Add time trends
	dat_by_brand = split(data.frame(y=dtbb_sim@y, dtbb_sim@X), dtbb_sim@individ)

	eq_by_brand=lapply(seq(along=dat_by_brand), function(z) {
		curr_brand = names(dat_by_brand)[z]
		out_matrix = dat_by_brand[[z]]
		
		if(res$adf_sur[brand==curr_brand&variable=='y']$trend == 1) {
			# add trend
		#	cat(paste0('added trend for ', curr_brand, '...\n'))
			out_matrix <- cbind(out_matrix, trend = seq(from=1, to=nrow(out_matrix)))
			colnames(out_matrix)[ncol(out_matrix)] <- paste0(curr_brand,'_trend')
			}
		
		return(out_matrix)
		})

	stacked_eq = rbindlist(eq_by_brand, fill=TRUE)
		
	# replace NAs for trends/copula to 0
		for (trendvar in grep('[_]trend|[_]cop', colnames(stacked_eq), value=T)) {
			vals = stacked_eq[,trendvar, with=F]
			stacked_eq[,trendvar := ifelse(is.na(get(trendvar)), 0, get(trendvar)), with=F]
			}
		
	# separate DVs from IVs
		dset = stacked_eq[,!colnames(stacked_eq)%in%'y',with=F]
		index=data.frame(date=dtbb_sim@period,brand=dtbb_sim@individ)
		
	# draw from variance-covariance matrix (coefficients)
	res$model@coefficients
	indexmatch = match(colnames(dset), res$model@coefficients$variable) # matches "new"/reduced set of variables to estimated coefficients
	coefs = res$model@coefficients[indexmatch,]$coef
	Sigma = res$model@varcovar[indexmatch, indexmatch]
	Sigma = matrix(double(length(coefs)*length(coefs)), ncol=length(coefs)) # set to zero for now
	
	rho_hat = res$model@rho_hat
	#rho_hat <- rep(0, length(rho_hat))
	
	eq_brands = names(dat_by_brand)
	nbrands = length(eq_brands)
	
	# draw from correlated intercepts
	set.seed(1234)
	nu <- mvrnorm(n=L*nperiods, mu=rep(0, ncol(res$model@sigma)), Sigma=res$model@sigma)
	
	dim(nu) <- c(L, nperiods, ncol(res$model@sigma))
	
	
	#intercepts <- mvrnorm(n=L*nperiods, mu=rep(0, ncol(res$model@sigma)), Sigma=res$model@sigma)
	
	#exp_intercepts = exp(intercepts)
	dimnames(nu)[[1]] <- 1:L
	dimnames(nu)[[2]] <- 1:nperiods
	dimnames(nu)[[3]] <- index[which(index$date==1),]$brand
	
	# compute intercepts
	intercepts = double(L*nperiods*length(eq_brands))
	dim(intercepts) <- dim(nu)
	
	for (p in 1:nperiods) {
		if (p==1) {
			intercepts[,p,] <- matrix(rep(sqrt(1-rho_hat^2),L),nrow=L, byrow=T)*nu[,p,]
			} else {
			intercepts[,p,] <- nu[,p,] - matrix(rep(rho_hat,L),nrow=L, byrow=T) * nu[,p-1,]
			}
		}
	
	# try out to draw only once per simulation, i.e., not per period
	
	#for (p in 1:nperiods) {
	#	if (p==1) {
	#		intercepts[,p,] <- nu[,1,]
	#		} else {
	#		intercepts[,p,] <- nu[,1,]
	#		}
	#	}
		
	exp_intercepts = exp(intercepts)
	
	# create empty matrix to store simulated market shares
	simulated_marketshares <- matrix(double((nbrands+1)*L*(nperiods+1)))
	dim(simulated_marketshares) <- c(nperiods+1, nbrands+1, L)
	simulated_marketshares[1,,] <- matrix(rep(init_means$lagunitsales_sh, L), ncol=L, byrow=F)# initialize 0-period (1st period) marketshares; needs to be lagged market share to correspond to period-2 value for market share)
	colnames(simulated_marketshares) <- init_means$brand

	dset_mat = rep(as.matrix(dset),L)
	dim(dset_mat) <- c(length(unique(index$date)), nbrands, ncol(dset), L)
	# dimensionality: PERIODS x BRANDS x COLUMNS x REPLICATIONS
	dimnames(dset_mat)[[2]] <- index[which(index$date==1),]$brand
	dimnames(dset_mat)[[1]] <- unique(index$date)
	dimnames(dset_mat)[[3]] <- colnames(dset)
	
	draws=mvrnorm(n=L, mu=coefs, Sigma = Sigma)
		
	for (p in seq(from=2, length.out=nperiods)) {
		#cat('periods ', p,'\n')

		# draw from variance-covariance matrix of coefficients
		#
		# for each brand (row in dset_p)
		dmat_curr = dset_mat[p,,,]
		dmat_min1 = dset_mat[p-1,,,]
			
		for (iter in seq(along=dimnames(dset_mat)[[2]])) {
			brandname = dimnames(dset_mat)[[2]][iter]
			
			# insert market shares from result matrix (simulated_marketshares) for every brand
			dmat_curr[iter,paste0(brandname, '_lagunitsales_sh'),] <- log(simulated_marketshares[p-1, match(brandname, colnames(simulated_marketshares)), ])
			dmat_curr[iter,paste0(res$benchmark_brand, '_lagunitsales_sh'),] <- -log(simulated_marketshares[p-1, match(res$benchmark_brand, colnames(simulated_marketshares)), ])
			
			dmat_min1[iter,paste0(brandname, '_lagunitsales_sh'),] <- log(simulated_marketshares[max(1,p-2), match(brandname, colnames(simulated_marketshares)), ])
			dmat_min1[iter,paste0(res$benchmark_brand, '_lagunitsales_sh'),] <- -log(simulated_marketshares[max(1,p-2), match(res$benchmark_brand, colnames(simulated_marketshares)), ])
			
			# adjust system to account for UR in dependent and independent variables
			ur_pres = res$adf_sur[ur == 1 & brand == brandname]$variable
			
			for (.var in ur_pres) {
				#cat(paste0('UR: ', .var, '   ', brandname), '\n')
				if (.var=='y') { # variable where UR is DEPENDENT VARIABLE
					# obtain lagged coefficient
					lagcoef = data.table(res$model@coefficients)[variable==paste0(brandname, '_lagunitsales_sh')]$coef
					lagcoef_bench = data.table(res$model@coefficients)[variable==paste0(res$benchmark_brand, '_lagunitsales_sh')]$coef
					focal_br_share = dmat_curr[iter,paste0(brandname, '_lagunitsales_sh'),]
					bench_br_share = dmat_curr[iter, paste0(res$benchmark_brand, '_lagunitsales_sh'),]
					
					# both terms are added, as data for benchmark has already been multiplied by -1
					dmat_curr[iter, paste0(brandname, '_lagunitsales_sh'), ] <- (1+1/lagcoef) * focal_br_share
					dmat_curr[iter, paste0(res$benchmark_brand, '_lagunitsales_sh'), ] <- (1+1/lagcoef_bench) * bench_br_share
				
				} else {
					# variable where UR is INDEPENDENT VARIABLE
					dmat_curr[iter, .var,] <- dmat_curr[iter, .var, ] - dmat_min1[iter, .var, ]  # <- 0 # use value from previous period
				}
			}

		}


		ms_sim = sapply(seq(1:L), function(l) {
			# compute exp(X * beta) (i.e., attraction) for all brands and draws
			relative_ms = exp(dmat_curr[,,l] %*% draws[l,])
			
			# add intercept correlations
			relative_ms = relative_ms * exp_intercepts[l,p-1,]
			
			# compute market shares
			sumx = sum(c(1,relative_ms))
			ms_sim = c(relative_ms/sumx, 1/sumx)
			return(ms_sim)
			})
		
		rownames(ms_sim) <- c(rownames(dmat_curr), res$benchmark_brand)
		
		# carry simulated values forward 
		if (p<=nperiods+1) {
			simulated_marketshares[p,,] <- ms_sim[match(colnames(simulated_marketshares), rownames(ms_sim)),]
			}

		# cat('\n')
		
		}
	
	means=melt(apply(simulated_marketshares, 1, rowMeans))
	qnt05=melt(apply(simulated_marketshares, 1, rowQuantiles, prob=.05))
	qnt95=melt(apply(simulated_marketshares, 1, rowQuantiles, prob=.95))
	means$type <- 'mean'
	qnt05$type <- 'qnt05'
	qnt95$type <- 'qnt95'
	
	dat <- rbind(means,qnt05,qnt95)
	
	colnames(dat) <- c('brand', 'period', 'marketshare', 'type')
	dat$period = dat$period-1
	
	
	#dat <- dat[which(dat$type=='mean'),]
	#xyplot(marketshare ~ period | type, groups= brand, data = dat, type='l', auto.key=TRUE)
	
	return(dat)
	
	}
	
	require(compiler)
	
	simulate <- cmpfun(simulate)
	
	# Extrapolate for nperiods periods
	sim_set = NULL
	for (p in seq(length.out=nperiods+1)) {
		tmp = init_means
		tmp[, month:=p]
		sim_set <- rbind(sim_set, tmp)
		}
	
	
	# Set up simulation
	sim_vars <- c('price', 'llength', 'novel', 'dist')
	sim_brands = unique(sim_set$brand)
	
	sims <- NULL
	
	tmp <- simulate(sim_set)
	tmp$sim_var = 'baseline'
	tmp$sim_brand = 'baseline'
	
	sims <- rbind(sims, tmp)
	
	for (.var in sim_vars) {
		for (.brand in sim_brands) {
			# baseline + shock
			cat(paste0('simulating for ', .brand, ' and ', .var, '...\n'))
			sim_dat = sim_set
			sim_dat[brand==.brand & month == 2, .var := get(.var)*1.20, with=F]
			tmp = simulate(sim_dat)
			tmp$sim_var = .var
			tmp$sim_brand = .brand
			sims <- rbind(sims, tmp)
			}
		}
	
	# Compute IRFs
	# keep means only
	
	sims = data.table(sims)
	
	par(mfrow=c(2,4))
	
	for (.brand in sim_brands) {
	p1 = sims[brand==.brand&sim_var=='price'&sim_brand=='apple'&type=='mean'&period>=1]
	base = sims[brand==.brand&sim_var=='baseline'&sim_brand=='baseline'&type=='mean'&period>=1]
	
	#with(base, plot(x=period, y=marketshare, type='l'))
	#with(p1, lines(x=period, y=marketshare, type ='l', lty=2))
	
	#IRF
	plot(x=base$period, y=base$marketshare-p1$marketshare, type='l', main = .brand, xlab = 'period', ylab = 'delta market share')
	abline(h=0)
	}
	
	
	# IRFs for each market
	
	# Make plots for each market with imuplse response functions
	
	with(p1, lines(x=period, y=marketshare, type ='l', lty=2))
	
	
	tmp = dcast(sims, brand + period ~ 
	
	res1 <- simulate(sim_set)
	
	sim_set[brand=='apple'&month==2, price := price*1.1]
#	sim_set[brand=='compaq'&month==2, price := price*1.1]
	
	res2 <- simulate(sim_set)
	
	
	res1$sim_type = 'sim1'
	res2$sim_type = 'sim2'
	
	res_all <- rbind(res1, res2)
	
	tmp = data.table(dcast(res_all, brand + period + sim_type ~ type, value.var = 'marketshare'))
	
	xyplot(mean ~ period|brand, groups=sim_type,data = tmp, auto.key=TRUE, type= 'l', subset = period >= 1, scales=list(relation="free"))
	
	
	
	tmp = data.table(dcast(res_all, brand + period ~ type + sim_type, value.var = 'marketshare'))
	
	
	tmp[brand=='apple']
	
	
	res$model@coefficients
	
	# derive 
	
	
	#################################################
	#################### NEXT STEPS #################
	#################################################
	
	# - per model, automatically iterate through all available variables and produce data set with
	#   period, market share, confidence intervals # or, alternatively, for ms changes
	# - understand standard errors of elasticities: need for variance-covariance of parameters, or is sigma (and potential correlations) enough?
	# - for a larger set of models, compare analytical and empircal elasticities
	
	# on the model estimation side...
	# ===============================
	# - finalize coding of R2 and wrapping up analysis in function
	# - then estimate for all markets and see whether that's stable
	# - correct estimation errors with computationally singular matrix (?). weird.
	
	