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
load(file='..\\..\\derived\\output\\datasets.RData')

# Stack data in data.table
	brand_panel=rbindlist(lapply(all_data, function(x) rbindlist(x$data_cleaned)))
	setorder(brand_panel, market_id, category,country,brand,date)
	
	category_panel = rbindlist(lapply(all_data, function(x) rbindlist(x$data_category)))
	setorder(category_panel, market_id, category,country,date)
	
### Load additional packages
	require(parallel)
	require(fUnitRoots)
	require(marketingtools)
	
### SETUP 
	### Setup cluster environment
		ncpu = 16
	
### Initialize all required functions (possibly on the cluster, too)

	init <- function() {
		max.lag <<- 12
		min.t <<- 36

	### Analysis script
		source('proc_analysis.R')
		require(marketingtools)

	}
	
	init()


# Specify models
m1 <- list(brandmodel = list(setup_y=c(unitsales_sh = 'unitsales_sh'),
							 setup_x=c(price="rwpsprice",dist="wpswdist+1",llength="llength",novel="novel+1"),
							 setup_xendog=c('price', 'dist', 'llength', 'novel'),
							 setup_xendog_signcutoff = .1,
							 trend='ur' # choose from: all, ur, none.
							 ),
		   catmodel = list(setup_y=c(unitsales = 'unitsales'),
						   setup_x=c(price="rwpsprice",dist="wpswdist+1",llength="llength",novel="novel+1"),
						   setup_xendog=c('price', 'dist', 'llength', 'novel'),
						   setup_xendog_signcutoff = .1,
						   trend='ur' # choose from: all, ur, none.
						   ), 
		   descr = 'with copula corrections',
		   fn = '21jan_withcopula')

models <- list(m1)

# to do:
# - Horvath en Franses: long-term elasticity computation
# - verify microwave NZ
# - compute elasticities also for benchmark brand


####################
### RUN ANALYSIS ###
####################

# define markets for analysis
	markets <- brand_panel[, list(.N), by=c('market_id','country', 'category')]
	analysis_markets <- unique(markets$market_id)
	
	last.item = length(analysis_markets)
	last.item = 10

# set up cluster
	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'category_panel', 'init'))
	
	void<-clusterEvalQ(cl, init())
	void<-clusterEvalQ(cl, require(data.table))
	
# run estimation for brand-level attraction models
	results_brands <- NULL

	for (m in seq(along=models)) {
		# Estimate brand model
		with(models[[m]]$brandmodel, {
			setup_y <<- setup_y
			setup_x <<- setup_x
			setup_xendog <<- setup_xendog
			setup_xendog_signcutoff <<- setup_xendog_signcutoff
			trend <<- trend
			})
		clusterExport(cl,c('setup_y', 'setup_x', 'setup_xendog', 'setup_xendog_signcutoff', 'trend'))
		
		Sys.time()
		results_brands[[m]]<-parLapply(cl, analysis_markets[1:last.item], function(i) {
				try(analyze_by_market(i, setup_y=setup_y, 
										 setup_x=setup_x, 
									     setup_xendog=setup_xendog, 
										 setup_xendog_signcutoff=setup_xendog_signcutoff, 
										 trend = trend),silent=T)
			})
		Sys.time()
		}

# run estimation for category-level sales response models
	results_category <- NULL

	for (m in seq(along=models)) {
		# Estimate brand model
		with(models[[m]]$catmodel, {
			setup_y <<- setup_y
			setup_x <<- setup_x
			setup_xendog <<- setup_xendog
			setup_xendog_signcutoff <<- setup_xendog_signcutoff
			trend <<- trend
			})
		clusterExport(cl,c('setup_y', 'setup_x', 'setup_xendog', 'setup_xendog_signcutoff', 'trend'))
		
		Sys.time()
		results_category[[m]]<-parLapply(cl, analysis_markets[1:last.item], function(i) {
				try(analyze_category(i, setup_y=setup_y, 
										 setup_x=setup_x, 
									     setup_xendog=setup_xendog, 
										 setup_xendog_signcutoff=setup_xendog_signcutoff, 
										 trend = trend),silent=T)
			})
		Sys.time()
		}

	
# save results
	save(results_brands, results_category, markets, models, file='..\\output\\results.RData')
