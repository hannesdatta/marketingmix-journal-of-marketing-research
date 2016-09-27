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
	

	L=500 # Number of simulation draws
	nperiods = 16 # periods used in simulation
	
	# Retrieve data set that was used to estimate the model
	res$melted_panel
	
	# Transform system to base-brand representation
	dt <- data.table(dcast(res$melted_panel, brand+month~variable, value.var=c('value')))
	
	# Compute means
	init_means = dt[, lapply(.SD, mean, na.rm=T), .SDcols = setdiff(colnames(dt), c('month', 'brand')), by = c('brand')]
	
	# Extrapolate for nperiods periods
	sim_set = NULL
	for (p in seq(length.out=nperiods+1)) {
		tmp = init_means
		tmp[, month:=p]
		sim_set <- rbind(sim_set, tmp)
		}
	

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
			cat(paste0('added trend for ', curr_brand, '...\n'))
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
	#Sigma = matrix(double(length(coefs)*length(coefs)), ncol=length(coefs)) # set to zero for now
	
	eq_brands = names(dat_by_brand)
	nbrands = length(eq_brands)
	
	# draw from correlated intercepts
	set.seed(1234)
	intercept_variance = res$model@sigma
	intercepts <- t(mvrnorm(n=L, mu=rep(0, ncol(res$model@sigma)), Sigma=res$model@sigma))
	exp_intercepts = exp(intercepts)
	rownames(intercepts) <- eq_brands
	
	# create empty matrix to store simulated market shares
	simulated_marketshares <- matrix(double((nbrands+1)*L*(nperiods+1)))
	dim(simulated_marketshares) <- c(nperiods+1, nbrands+1, L)
	simulated_marketshares[1,,] <- matrix(rep(init_means$unitsales_sh, L), ncol=L, byrow=F)# initialize 0-period marketshares
	colnames(simulated_marketshares) <- init_means$brand

	for (p in seq(from=2, length.out=nperiods)) {
		
		cat('periods ', p,'\n')
		
		dset_p = dset[which(index$date==p),]
		rownames(dset_p) <- index[which(index$date==p),]$brand
		dset_p_min1 = dset[which(index$date==p-1),]
		
		# draw from variance covariance matrix of coefficients
		draws=mvrnorm(n=L, mu=coefs, Sigma = Sigma)
		
		# for each brand (row in dset_p), adjust system to account for UR in dependent and independent variables
		for (iter in seq(along=rownames(dset_p))) {
			brandname = rownames(dset_p)[iter]
			
			ur_pres = res$adf_sur[ur == 1 & brand == brandname]$variable
			
			for (.var in ur_pres) {
				if (.var=='y') { # variable where UR is DEPENDENT VARIABLE
					# obtain lagged coefficient
					lagcoef = data.table(res$model@coefficients)[variable==paste0(brandname, '_lagunitsales_sh')]$coef
					lagcoef_bench = data.table(res$model@coefficients)[variable==paste0(res$benchmark_brand, '_lagunitsales_sh')]$coef
					
					dset_p[iter, paste0(brandname, '_lagunitsales_sh')] <- (1+1/lagcoef) * dset_p[iter, paste0(brandname, '_lagunitsales_sh'),with=F]
					# both are added, as data for benchmark has already been multiplied by -1
					dset_p[iter, paste0(res$benchmark_brand, '_lagunitsales_sh')] <- (1+1/lagcoef_bench) * dset_p[iter, paste0(res$benchmark_brand, '_lagunitsales_sh'), with=F]
				} else {
					# variable where UR is INDEPENDENT VARIABLE
					dset_p[iter, .var] <- dset_p[iter, .var, with=F] - dset_p_min1[iter, .var, with=F]  # <- 0 # use value from previous period
				}
					
				}

			}
		
		
		# compute exp(X * beta) (i.e., attraction) for all brands and draws
		dset_p_mat = as.matrix(dset_p)
		relative_ms = apply(draws, 1, function(d) {
			exp(dset_p_mat%*%cbind(d))
			})
			
		# add intercept correlations
		relative_ms = relative_ms * exp_intercepts
		
		ms_sim = apply(relative_ms, 2, function(x) {
			sumx = sum(c(1,x))
			c(x/sumx, 1/sumx)
			})
			
		rownames(ms_sim) <- c(rownames(dset_p), res$benchmark_brand)
		
		# carry simulated values forward 
		if (p<nperiods+1) {
			for (br in init_means$brand) {
				multpl = ifelse(br==res$benchmark, -1, 1) # negative values here for benchmark brand
				dset[which(index$date==p+1),paste0(br, '_lagunitsales_sh')] = ifelse(abs(dset[which(index$date==p+1),paste0(br, '_lagunitsales_sh'),with=F])>0, multpl*log(summ_ms[which(names(summ_ms)==br)]), 0)
				}
			}
			
		simulated_marketshares[p,,] <- ms_sim[match(colnames(simulated_marketshares), rownames(ms_sim)),]
		
		 #dset[which(index$date==p),]
		 #dset[which(index$date==p+1),]
		 
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
	
	dat <- dat[which(dat$type=='mean'),]
	xyplot(marketshare ~ period | type, groups= brand, data = dat, type='l', auto.key=TRUE)
	

	# Not taking into account simulated market shares per path L when carrying forward observations
	# Code up a loop that can simulate multiple times for different shocks in explanatory variables
	