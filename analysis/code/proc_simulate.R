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
	
	
	#res = results_brands[[1]]
	
	
	simulate <- function(sim_set, res, L, nperiods) {

	# build in condition for markets in which carry-over coef is zero
	  
	# transform to base-brand representation
	lag_heterog=F
	
	lagms = 'hom_lagunitsales_sh' %in% res$model@coefficients$variable
	
	m_form = as.formula(paste0(res$variables$y, ' ~ ', paste0(res$variables$x, collapse = '+'), ifelse(lagms==T,' + lagunitsales_sh','')))
  m_form_heterog = as.formula(paste0('~ ', paste0(grep('^attr', res$variables$x, value=T, invert=T), collapse = '+'), ifelse(lag_heterog==T, ifelse(lagms==T,' + lagunitsales_sh',''), '')))
	m_form_index = as.formula(~ brand + month)

	# insert market shares from previous periods, as stored in the result matrix (called 'simulated_marketshares'), for every brand
	if (res$attraction_model=='MCI') transf_fkt <- function(x) log(x)
	if (res$attraction_model=='MNL') transf_fkt <- function(x) x
	
	# recall: MCI takes logs, MNL does not
	dtbb_sim <- suppressWarnings(attraction_data(formula=m_form, 
							data = sim_set, 
							heterogenous = m_form_heterog, 
							index = m_form_index, 
							benchmark=res$benchmark_brand,
							model = res$attraction_model))

	# Add time trends, if necessary
	dat_by_brand = split(data.frame(y=dtbb_sim@y, dtbb_sim@X), dtbb_sim@individ)

	eq_by_brand=lapply(seq(along=dat_by_brand), function(z) {
		curr_brand = names(dat_by_brand)[z]
		out_matrix = dat_by_brand[[z]]
		
		if(res$adf_sur[brand==curr_brand&variable=='y']$trend_included == TRUE) {
			# add trend
		  if (res$attraction_model=='MNL') out_matrix <- cbind(out_matrix, trend = seq(from=1, to=nrow(out_matrix)))
		  if (res$attraction_model=='MCI') out_matrix <- cbind(out_matrix, trend = log(seq(from=1, to=nrow(out_matrix))))
		 	colnames(out_matrix)[ncol(out_matrix)] <- paste0(curr_brand,'_trend')
			}
		
		return(out_matrix)
		})

	stacked_eq = rbindlist(eq_by_brand, fill=TRUE)

	# replace NAs for trends/copula/and any other "missing" value 
	# (i.e., values that may have been kicked out due to the use_ts constraint) to 0; 
	# note that "real" missing values are kicked out by complete_obs
	
	for (resetvar in colnames(stacked_eq)) {
	  vals = stacked_eq[,resetvar, with=F]
	  stacked_eq[, (resetvar) := ifelse(is.na(get(resetvar)), 0, get(resetvar))]
	}

	# separate DVs from IVs
		dset = stacked_eq[,!colnames(stacked_eq)%in%'y',with=F]
		index=data.frame(date=dtbb_sim@period,brand=dtbb_sim@individ)
		
	# remove all-zero columns from X (e.g., where a brand variable is removed earlier ("NA"), and contains zeros for all other brands).
		delvars = colnames(dset)[which(colSums(dset)==0)]
		keepv = setdiff(colnames(dset)[colnames(dset)%in%res$model@coefficients$variable], delvars)
		dset = dset[, keepv, with=F]
		
	# draw from variance-covariance matrix (coefficients)
	indexmatch = match(colnames(dset), res$model@coefficients$variable) # matches "new"/reduced set of variables to estimated coefficients
	coefs = res$model@coefficients[indexmatch,]$coef
	
	# reset copulas to zero
	if(length(which(grepl('cop[_]', colnames(dset))))>0) coefs[which(grepl('cop[_]', colnames(dset)))]<-0
	
	Sigma = res$model@varcovar[indexmatch, indexmatch]
	#Sigma = matrix(double(length(coefs)*length(coefs)), ncol=length(coefs)) # set to zero for now (!!!!!!)

	rho_hat = res$model@rho_hat
	
	eq_brands = names(dat_by_brand)
	nbrands = length(eq_brands)
	
	# draw from correlated intercepts
	set.seed(1234)
	nu <- mvrnorm(n=L*nperiods, mu=rep(0, ncol(res$model@sigma)), Sigma=res$model@sigma)
	
	dim(nu) <- c(L, nperiods, ncol(res$model@sigma))
	
	dimnames(nu)[[1]] <- 1:L
	dimnames(nu)[[2]] <- 1:nperiods
	dimnames(nu)[[3]] <- index[which(index$date==1),]$brand
	
	# compute intercepts
	intercepts = double(L*nperiods*length(eq_brands))
	addmat = double(L*nperiods*length(eq_brands)) # matrix to add constants to equations (for purpose of adding lagged logged ratio of market shares for dynamic simulations)
	
	dim(intercepts) <- dim(nu)
	dim(addmat) <- dim(nu)
	
	for (p in 1:nperiods) {
		if (p==1) {
			intercepts[,p,] <- matrix(rep(sqrt(1-rho_hat^2),L),nrow=L, byrow=T)*nu[,p,]
			} else {
			intercepts[,p,] <- nu[,p,] - matrix(rep(rho_hat,L),nrow=L, byrow=T) * nu[,p-1,]
			}
		}

	# create empty matrix to store simulated market shares
	simulated_marketshares <- matrix(double((nbrands+1)*L*(nperiods+1)))
	dim(simulated_marketshares) <- c(nperiods+1, nbrands+1, L)
	simulated_marketshares[1,,] <- matrix(rep(init_means$lagunitsales_sh, L), ncol=L, byrow=F) # initialize 0-period (1st period) marketshares; needs to be lagged market share to correspond to period-2 value for market share)
	colnames(simulated_marketshares) <- init_means$brand

	dset_mat = rep(as.matrix(dset),L)
	dim(dset_mat) <- c(length(unique(index$date)), nbrands, ncol(dset), L)

	# dimensionality: PERIODS x BRANDS x COLUMNS x REPLICATIONS
	dimnames(dset_mat)[[1]] <- unique(index$date)
	dimnames(dset_mat)[[2]] <- index[which(index$date==1),]$brand
	dimnames(dset_mat)[[3]] <- colnames(dset)
	
	draws=mvrnorm(n=L, mu=coefs, Sigma = Sigma)
		
	for (p in seq(from=2, length.out=nperiods)) {
		#cat('periods ', p,'\n')

		# draw from variance-covariance matrix of coefficients
		
	  # loop for each brand (row in dset_p)
		dmat_curr = dset_mat[p,,,,drop=F]
		dmat_min1 = dset_mat[p-1,,,,drop=F]
		
		for (iter in seq(along=dimnames(dset_mat)[[2]])) {
			brandname = dimnames(dset_mat)[[2]][iter]
			
			# for heterogenous lagged market share
			if(0){
			dmat_curr[1,iter,paste0(brandname, '_lagunitsales_sh'),] <- transf_fkt(simulated_marketshares[p-1, match(brandname, colnames(simulated_marketshares)), ])
			dmat_curr[1,iter,paste0(res$benchmark_brand, '_lagunitsales_sh'),] <- -transf_fkt(simulated_marketshares[p-1, match(res$benchmark_brand, colnames(simulated_marketshares)), ])
			
			dmat_min1[1,iter,paste0(brandname, '_lagunitsales_sh'),] <- transf_fkt(simulated_marketshares[max(1,p-2), match(brandname, colnames(simulated_marketshares)), ])
			dmat_min1[1,iter,paste0(res$benchmark_brand, '_lagunitsales_sh'),] <- -transf_fkt(simulated_marketshares[max(1,p-2), match(res$benchmark_brand, colnames(simulated_marketshares)), ])
			}
			
			# for homogenous lagged market share
			if ('hom_lagunitsales_sh'%in%dimnames(dmat_curr)[[3]]) {
			  dmat_curr[1, iter,'hom_lagunitsales_sh',] <- transf_fkt(simulated_marketshares[p-1, match(brandname, colnames(simulated_marketshares)), ]/simulated_marketshares[p-1, match(res$benchmark_brand, colnames(simulated_marketshares)), ])
			  dmat_min1[1, iter,'hom_lagunitsales_sh',] <- transf_fkt(simulated_marketshares[max(1,p-2), match(brandname, colnames(simulated_marketshares)), ]/simulated_marketshares[max(1,p-2), match(res$benchmark_brand, colnames(simulated_marketshares)), ])
			}
			
		}

		if(0){
		ms_sim = sapply(seq(1:L), function(l) {
			# go from log attraction to attraction
		  
		  log_relative_ms = dmat_curr[1,,,l] %*% draws[l,] + intercepts[l,p-1,] + addmat[l,p-1,]
		  relative_ms = exp(log_relative_ms) 
		  
			# compute market shares
			sumx = sum(c(1,relative_ms))
			ms_sim = c(relative_ms/sumx, 1/sumx)
			return(ms_sim)
			})
		}
		
		#if(0){
		relative_ms = t(sapply(seq(length.out=dim(dmat_curr)[2]), function(br_no) {
		  # go from log attraction to attraction
		  
		  log_relative_ms = colSums(dmat_curr[1,br_no,,] * t(draws)) + intercepts[,p-1,br_no] + addmat[,p-1,br_no]
		  relative_ms = exp(log_relative_ms) 
		  return(relative_ms)
		}))
		
		sumx=colSums(relative_ms)+1
		
		ms_sim=rbind(relative_ms/matrix(rep(sumx,each=nrow(relative_ms)),nrow=nrow(relative_ms)), matrix(1/sumx,nrow=1))
		#}
		rownames(ms_sim) <- c(dimnames(dmat_curr)[[2]], res$benchmark_brand)
		
		# carry simulated values forward 
		if (p<=nperiods+1) {
			simulated_marketshares[p,,] <- ms_sim[match(colnames(simulated_marketshares), rownames(ms_sim)),]
			simulated_marketshares[p,,] = ifelse(simulated_marketshares[p,,]<1E-6, 1E-6, simulated_marketshares[p,,])
			}
		summary(c(simulated_marketshares[p,,]))

		}
	
	return(simulated_marketshares)
	
	}
	
	require(compiler)
	
	simulate <- cmpfun(simulate)
	
		
	
	
	
	execute_sim <- function(res, sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), nperiods=60, L = 1000, shock_period=25, shock_perc = 1.01, test = F, shock12=NULL) {
	
		
		# L = number of simulation draws
	  # nperiods = number of periods to simulate
	  # shock_period = period in which to shock
	  # shock_perc = shock percentage (e.g. 1.01 for 1%)
		
		# Retrieve data set that was used to estimate the model
		if ('try-error' %in% class(res)) return(NULL)
		if (!is.null(res$error)) return(NULL)
		#res$melted_panel
		
		# Transform system to base-brand representation
		dt <- data.table(dcast(res$melted_panel, brand+month~variable, value.var=c('value')))
		
		if (!'lagunitsales_sh' %in% colnames(dt)) dt[, lagunitsales_sh:=usalessh]
		
		cat('Number of brands: ', length(unique(dt$brand)),fill=T)
		
		
		# Compute means
		init_means <<- dt[, lapply(.SD, mean, na.rm=T), .SDcols = setdiff(colnames(dt), c('month', 'brand')), by = c('brand')]
	
		# Create simulation data set
		sim_set = NULL
		for (p in seq(length.out=nperiods+1)) {
			tmp = init_means
			tmp[, month:=p]
			sim_set <- rbind(sim_set, tmp)
			}
		
		# Set up simulation
		brand_ofs = unique(sim_set$brand)
		
		if (test==T) brand_ofs = unique(sim_set$brand)[1]
		
		baseline <- simulate(sim_set, res, L=L, nperiods=nperiods)
		
		sims <- NULL
		
		cntr <- 1
		for (.var in sim_vars) {
		  for (.brand in brand_ofs) {
				# baseline + shock
				# simulate only if variable has been estimated for a given brand
				if (!paste0(.brand,'_', .var)%in%res$model@coefficients$variable & !paste0('X', .brand,'_', .var)%in%res$model@coefficients$variable) next
				cat(paste0('simulating for ', .brand, ' and ', .var, '...\n'))
				sim_dat = data.table(sim_set)
				if (!.var%in% shock12) {
				  sim_dat[brand==.brand & month == shock_period+1, (.var) := get(.var)*shock_perc]
				} else {
				  sim_dat[brand==.brand & month >= shock_period+1 & month <= shock_period+1+11, (.var) := get(.var)*shock_perc]
				}
				
				tmp = simulate(sim_dat, res, L=L, nperiods=nperiods)
				#sims <- rbind(sims, tmp)
				sims[[cntr]] <- list(data=tmp, perc_change = (tmp-baseline)/baseline, spec = list(sim_var=.var, brand_of = .brand))
				cntr <- cntr+1
				}
			}
		
		# Compute panel data set w/ perc. changes
		predictions=rbindlist(lapply(sims, function(x) {
			tmp = melt(t(apply(x$data, 1, rowMeans)))
			colnames(tmp) <- c('period','brand', 'ms')
			tmp4 = melt(t(apply(baseline, 1, rowMeans)))
			tmp$base_ms <- tmp4$value
			rm(tmp4)
			tmp$sim_var = as.factor(x$spec$sim_var)
			tmp$brand_of = as.factor(x$spec$brand_of)
			return(tmp)
			}))
		
		predictions[, brand:=as.character(brand)]
		predictions[, brand_of:=as.character(brand_of)]
		predictions[, category:= as.factor(unique(res$specs$category))]
		predictions[, country := as.factor(unique(res$specs$country))]
		predictions[, market_id := as.factor(unique(res$specs$market_id))]
		predictions[, period:=period-1]
		
		setcolorder(predictions, c('market_id','category','country','brand', 'brand_of','sim_var','period'))
		
		elasticities=rbindlist(lapply(sims, function(x) {
		  # market shares
		  trr=(x$data-baseline)/baseline
		  trr_lt=t(apply((x$data-baseline)[(shock_period+1):nrow(x$data),,],2, colSums))/baseline[shock_period+1,,]
		  
		  tmp_st_mean = apply(trr[shock_period+1,,]*100, 1, function(y) mean(y))
		  tmp_st_sd = apply(trr[shock_period+1,,]*100, 1, function(y) sd(y))
		  
		  tmp_lt_mean = apply(trr_lt*100, 1, function(y) mean(y))
		  tmp_lt_sd = apply(trr_lt*100, 1, function(y) sd(y))
		  
		  tmp=data.frame(cbind(simelast=tmp_st_mean, simelast_sd=tmp_st_sd, simelastlt=tmp_lt_mean, simelastlt_sd=tmp_lt_sd))
		  
		  tmp$sim_var = as.factor(x$spec$sim_var)
		  tmp$brand_of = as.factor(x$spec$brand_of)
		  tmp$brand=rownames(tmp)
		  rownames(tmp)<-NULL
		  
		  return(tmp)
		}))
		
		elasticities[, ':=' (brand =as.character(brand),
		                     brand_of=as.character(brand_of),
		                     category = as.factor(unique(res$specs$category)),
		                     country = as.factor(unique(res$specs$country)),
		                     market_id = as.factor(unique(res$specs$market_id)))]
	
		setcolorder(elasticities, c('market_id', 'category','country', 'brand', 'brand_of','sim_var'))
		
		return(list(marketshares=predictions, elasticities=elasticities))
		
		}
	
	
	
	plot_irf <- function(simobj) {
	  
	  # only own-brand elasticities
	  for (.brand in unique(simobj$brand)) {
	    
	    vars = unique(simobj[brand==.brand & brand_of==.brand]$sim_var)
	    par(mfrow=c(2,length(vars)))
	    
	      for (.var in vars) {
	        tmp = simobj[brand==.brand&brand_of==.brand&period>=1&sim_var==.var]
	        
	        # market shares
	        with(tmp, plot(x=period, y=ms, type='l', main = paste('shares ', .brand, '-', .var), xlab = 'period', ylab = 'market share'))
	        with(tmp, lines(x=period, y=base_ms, type='l', lty=2))
	        
	        # IRF
	        with(tmp, plot(x=period, y=ms-base_ms, type='l', main = paste('IRF ', .brand, '-', .var), xlab = 'period', ylab = 'delta market share'))
	        abline(h=0)
	        
	      }
	    
	  }
	}
