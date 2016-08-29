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



### SIMULATION OF SHORT- AND LONG-TERM ELASTICITIES

# LOAD DATA SETS
require(data.table)
require(marketingtools)
load(file='..\\..\\derived\\output\\datasets.RData')
load(file='..\\..\\analysis\\output\\results.RData')

# Rationale:

# Predict market shares at mean marketing mix across complete period.

out <- results_brands[[1]][[1]]

# Determine shocks (this is of all variables in levels)
out$sur$coefficients

melted_panel = out$melted_panel
lagged_ms = melted_panel[variable=='unitsales_sh']
lagged_ms[, value := makelag(value), by=c('brand')]
lagged_ms[, variable:='lagunitsales_sh']
melted_panel <- rbind(melted_panel, lagged_ms)

dt <- data.table(dcast(melted_panel, brand+date~variable, value.var=c('value')))
dt[, month := as.numeric(as.factor(date))]
#lagunitsales_sh

##########################################
# TRANSFORM TO BASE-BRAND REPRESENTATION #
##########################################

	m_form = as.formula(unitsales_sh ~ llength + novel + price + dist + lagunitsales_sh)
	m_form_heterog = as.formula(~  llength + novel + price + dist + lagunitsales_sh)
	m_form_index = as.formula(~ brand + month)
	
	dtbb <- attraction_data(formula=m_form, 
							data = dt, 
							heterogenous = m_form_heterog, 
							index = m_form_index, 
							benchmark=out$benchmark_brand,
							model = 'MCI')

###########################
# CONDUCT UNIT ROOT TESTS #
###########################

	# perform Enders procedure by brand
	te = split(data.frame(y=dtbb@y, dtbb@X), dtbb@individ)

	trans=lapply(te, function(test_matrix) {
		#test_matrix = te[[1]]
		tests=data.frame(t(apply(test_matrix[!colSums(test_matrix)==0 & !grepl('[_]dum|trend', colnames(test_matrix))], 2, adf_enders, maxlag=12,pval=.05,season=NULL)))

		to_be_diffed = rownames(tests)[which(tests$ur==1)]
		
		out_matrix = test_matrix
		for (variable in to_be_diffed) {
			out_matrix[,variable] = makediff(out_matrix[,variable])
			}
		
		res= list(adf=tests, transformed=out_matrix, original = test_matrix, diffed_series = to_be_diffed)
		return(res)
		})
		
	newsystem = rbindlist(lapply(trans, function(x) x$transformed))

	X = newsystem[,colnames(dtbb@X),with=F]
	table(colnames(X)==colnames(dtbb@X))
	Y = data.frame(newsystem[,'y',with=F])
	index=data.frame(date=dtbb@period,brand=dtbb@individ)

	
	# kick out incomplete ('diffed') vars, i.e., their first observation (as it is NA)

	complete = complete.cases(data.frame(X,Y,index))

	X=as.matrix(X[complete,])
	Y=as.matrix(Y[complete,])
	index=data.frame(index[complete,])

##########################################
# ADD COPULAS FOR ENDOGENEITY CORRECTION #
##########################################

	# to be done.
	

##################
# ESTIMATE MODEL #
##################

	# transformed
	m<-itersur(X=X, Y=Y, index=index, method='FGLS')

	# non-differences
	mreg<-itersur(X=dtbb@X, Y=as.matrix(dtbb@y), index=data.frame(date=dtbb@period, brand=dtbb@individ))

	
############
# SIMULATE #
############

	# Number of simulation draws
	L=100
	nperiods = 36 # periods used in simulation
	
	# First do a simulation on the non-differenced ones.
	# First do it for only one brand
	
	# Step 1: Shock variable (in levels?), log?!; for MCI model, this boils down to adding the log of 1.01 times the estimated coefficient.
	
	# Derive simulation data set (e.g., a data at its mean)
	simset <- data.table(date=dtbb@period, brand=dtbb@individ, y=dtbb@y, dtbb@X)
	
	# take the first period
	sim_init <- dt[, lapply(.SD, mean, na.rm=T), by=c('brand'), .SDcols=colnames(dt)[!colnames(dt)%in%c('brand', 'date', 'trend', 'month')]]
	# extrapoluate for nperiods periods
	sim_set = NULL
	for (p in seq(length.out=nperiods)) {
		tmp = sim_init
		tmp[, month:=p]
		sim_set <- rbind(sim_set, tmp)
		}

	# put Xs
	sim1 = sim_set
	sim2 = sim_set
	sim2[month==2&brand=='acer', 'price' := get('price')*1.01,with=F]
	
	sims <- list(sim1, sim2)
	
	simsets <- lapply(sims, function(sim_set) {
		# transform to base-brand representation
		dtbb_sim <- attraction_data(formula=m_form, 
								data = sim_set, 
								heterogenous = m_form_heterog, 
								index = m_form_index, 
								benchmark=out$benchmark_brand,
								model = 'MCI')
		tmp=dtbb_sim@X
		rownames(tmp) <- dtbb_sim@individ
		return(tmp)
		})
	benchmark=out$benchmark_brand
		
	#if (all(coef(mreg)$variable==colnames(dtbb_sim@X))==F) stop('Problem with simulation dataset')
	nbrands = length(unique(sims[[1]]$brand))
	coefs = coef(mreg)$coef
		
	# Next steps: 
	# -> make sure my model can "step" through this period data set, and simulate market shares
	# -> then, change code to incorporate error in market shares (i.e., simulation times number of replications for lagged values)
	# -> make sure I also draw an error at each iteration (i.e., correlation of market shares) (this is NOT incorporated yet)
	# -> make loop through different variables and shock situations
	# -> set copulas to zero.
	
	# -> make sure that my simulations also run in a "differenced" world; and that this is somewhat incorporated in my automatic code
	
		
	# -> it would be cool to package the simulation code (given a specific dataset as prepared by dtbb_sim), and an itersur object, to a function
	#    which can perform the analyses automatically.	
	
	# make code flexible enough to step 
	
	for (s in seq(along=simsets)) {
		# set copulas to zeros!
		
		simulated_marketshares <- matrix(double(nbrands*L*nperiods))
		dim(simulated_marketshares) <- c(nperiods, nbrands, L)

		for (p in seq(length.out=nperiods)) {
		cat('periods ', p,'\n')
		
		# draw from variance covariance matrix
		# compute log diff brand
		require(MASS)
		draws=mvrnorm(n=L, mu=coefs, Sigma = mreg@varcovar)
		
		relative_ms = apply(draws, 1, function(d) {
			exp(simsets[[s]]%*%cbind(d))
			})

		ms_sim = apply(relative_ms, 2, function(x) {
			sumx = sum(c(1,x))
			c(x/sumx, 1/sumx)
			})
			
		# compute market shares for ALL brands
		rownames(ms_sim) <- c(rownames(simsets[[s]]), benchmark)
		ms_sim <- ms_sim[match(sim_set$brand,rownames(ms_sim)),]

		if (p==10) sim_set[brand=='acer']$price <- sim_set[brand=='acer']$price*1.1
		simulated_marketshares[p,,] <- ms_sim
		# carry values forward
		summ_ms <- rowMeans(ms_sim)
		sim_set$lagunitsales_sh<-summ_ms[]
		}
		
		tmp=data.table(apply(simulated_marketshares, 1, rowMeans))
		tmp[, brand:=unique(sim_init$brand)]
		tmp = melt(tmp, id.vars=c('brand'))
		tmp[, period := as.numeric(variable)]
	}
	
		
	require(lattice)
	xyplot(value~period, groups=brand,data=tmp, auto.key=TRUE,type='l')
	

	
	
	
	
	m1=as.matrix(dtbb_sim@X)
	m2=as.matrix(coef(mreg)$coef)
	logys = m1%*%m2
	
	
	
	
	
# Next steps:
# - Discuss inclusion of more lags with Marnik and Harald
# - Simulate response elasticities; possibly wrap in package


