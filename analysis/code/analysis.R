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



### LOAD DATA SETS
require(data.table)
require(bit64)

### Stack data in data.table
	brand_panel=fread('../temp/preclean.csv')
	brand_panel[, ':=' (date = as.Date(date))]

### Load additional packages
	require(parallel)
	
### Setup cluster environment
	ncpu = 4#10
	
### Initialize all required functions (possibly on the cluster, too)

	init <- function() {
		max.lag <<- 12
		min.t <<- 36
	
  	    ### Analysis script
		source('proc_analysis.R')
		require(marketingtools)
		}
	
	init()

# To do
# - MCI vs MNL
# - add switches to proc_analysis

#brand_panel[, ':=' (wpswdst = wpswdst+1, nov3 = nov3 + 1)]


# Specify models
try(detach(m1), silent=T)
m1 <- list(setup_y=c(usalessh = 'usalessh'),
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov3sh', uniq='wpsun'),
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov3sh','wpsun'),
		   #setup_endogenous=c(price = 'iv[_].*mcrwpsprd', dist = 'iv[_].*mcwpswdst', llen = 'iv[_].*mcllen', nov = 'iv[_].*mcnov3'),
		   trend='none', # choose from: all, ur, none.
		   pval = .05,
		   max.lag = 12, 
		   min.t = 36,
		   descr = 'endog_model',
		   fn = 'endog_model')
attach(m1)

#models <- list(m1)

####################
### RUN ANALYSIS ###
####################


	assign_model <- function(m, del=F) {
		if (del==F) {
			for (l in seq(along=m)) {
				var = m[[l]]
				eval(parse(text=paste0(names(m)[l], ' <<- var')))
				}
			}
		if (del==T) {
			for (l in seq(along=m)) eval(parse(text=paste0('rm(', names(m)[l],')')))
			}
		}
		
# define markets for analysis
	markets <- brand_panel[, list(.N), by=c('market_id','country', 'category')]
	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
	
	last.item = length(analysis_markets)
last.item = 10

# deactivate the rest
	# not on cluster
	results_brands <- NULL
	try(detach(m1), silent=T)
	
	for (m in analysis_markets[1:last.item]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = .05, max.lag = 12, 	min.t = 36), silent=T)

	}
	
	# calculate elasticities and significance!!!
	
	######################
	# Cluster			 #
	######################
	
# set up cluster

	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	void<-clusterEvalQ(cl, require(data.table))
	
	void<-clusterEvalQ(cl, init())
		
# run estimation for brand-level attraction models
	results_brands <- NULL
	assign_model(m1)
	
	clusterExport(cl,names(m1))
	
		
	Sys.time()
	results_brands <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
			try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t),silent=T)
			})
	Sys.time()
	

#####################
# summarize results #
#####################

	# model crashes
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	
	# elasticities
	elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))

	zval=1.69
	elast[!is.na(coef), list(median_elast = median(elast), 
												  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
												  N_brands= .N, 
												  perc_positive = length(which(z>=(zval)))/.N, 
												  perc_null = length(which(abs(z)<zval))/.N, 
												  perc_negative = length(which(z<=(-zval)))/.N), by=c('variable')]
		
	# copulas
	copulas <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$model@coefficients))
	copulas <- copulas[grepl('cop_', varname)]
	
	copulas[!is.na(coef), list(median = median(coef), 
											  weighted = sum(coef/se)/sum(1/se), 
											  N_brands= .N, 
											  perc_positive = length(which(z>=(zval)))/.N, 
											  perc_null = length(which(abs(z)<zval))/.N, 
											  perc_negative = length(which(z<=(-zval)))/.N), by=c('varname')]
	# seasonality
	season <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$model@coefficients))
	season <- season[grepl('quarter[0-9]_', variable)]
	
	season[!is.na(coef), list(median = median(coef), 
											  weighted = sum(coef/se)/sum(1/se), 
											  N_brands= .N, 
											  perc_positive = length(which(z>=(zval)))/.N, 
											  perc_null = length(which(abs(z)<zval))/.N, 
											  perc_negative = length(which(z<=(-zval)))/.N), by=c('brand')]
												  
	
	# plot histograms of elasticities
	for (vars in c('llen', 'rwpspr', 'wpsun'))
	
	
	hist(elast[variable=='llen']$elast, breaks=100, col='grey')
	hist(elast[variable=='rwpspr']$elast, breaks=100, col='grey') # -15
	hist(elast[variable=='wpsun']$elast, breaks=100, col='grey')
	hist(elast[variable=='wpswdst']$elast, breaks=100, col='grey')
	hist(elast[variable=='nov3sh']$elast, breaks=100, col='grey')
	
	elast[variable=='llen' & elast > 6]
	# check data / model outputs for top and bottom 10; plus 5+
	elast[variable=='rwpspr' & elast > 6]
	
	# same cases? e.g. no. of observations
	