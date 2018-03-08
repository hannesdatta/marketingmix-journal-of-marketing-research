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

## How many brands are active in multiple countries
	tmp = brand_panel[, list(N=.N), by = c('brand', 'country')]
	tmp[, Ncountries := .N, by = c('brand')]
	tmp[, N:=NULL]
	tmp[, present := T]
	tmp = dcast.data.table(tmp, brand + Ncountries ~ country, value.var='present')
	
	tmp[, Ncountries:= -Ncountries]
	setorder(tmp, Ncountries, brand)
	tmp[, Ncountries:=-Ncountries]
	write.table(tmp, 'clipboard', row.names=F)
	
	
### Load additional packages
	require(parallel)
	
### Setup cluster environment
	ncpu = 10 #4
	
### Initialize all required functions (possibly on the cluster, too)

	init <- function() {
		max.lag <<- 12
		min.t <<- 36
	
  	    ### Analysis script
		source('proc_analysis.R')
		require(marketingtools)
		}
	
	
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
	
	init()

	
# Specify model
  try(detach(m1), silent=T)
	try(assign_model(m1,del=T), silent=T)
	
	# lag variables
	for (.var in c('rwpspr', 'wpswdst','llen','nov6sh')) {
	  
	  brand_panel[, paste0('lag', .var) := c(NA, get(.var)[-.N]), by = c('market_id')]
	  
	}
	
	

	m1 <- list(setup_y=c(usalessh = 'usalessh'),
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov6sh'),
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov6sh'),
		   trend='none', # choose from: always, ur, none.
		   pval = .1,
		   max.lag = 12, 
		   min.t = 36,
		   descr = 'model',
		   fn = 'model',
		   benchmarkb = NULL,
		   estmethod = "FGLS-Praise-Winsten",
		   attraction_model = "MNL",
		   takediff = 'none', # choose from flexible (UR approach), none, alwaysdiff
		   use_quarters = F,
		   plusx = NULL, #c('nov3sh', 'wpswdst'),
		   lag_heterog = T,
		   squared=F,
		   maxiter = 300)

assign_model(m1)
#assign_model(m1,del=T)


####################
### RUN ANALYSIS ###
####################


# define markets for analysis
	markets <- brand_panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
	
	last.item = length(analysis_markets)
	last.item = 2

	# not on cluster
	results_brands <- NULL

	for (m in analysis_markets[1:last.item]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, 
		                                             setup_endogenous = setup_endogenous, 
		                                             trend = trend, pval = pval, max.lag = max.lag, 
		                                             min.t = min.t, maxiter=maxiter, attraction_model = attraction_model, 
		                                             plusx=plusx, use_quarters=use_quarters, squared=squared, takediff=takediff, lag_heterog=lag_heterog), silent=T)
		
		assign_model(m1, del = TRUE)
		
	}
	assign_model(m1, del = TRUE)
	
	################
	# Cluster			 #
	################
	
# set up cluster

	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	void<-clusterEvalQ(cl, require(data.table))
	
# run estimation for brand-level attraction models
	void<-clusterEvalQ(cl, init())
	init()
	last.item = length(analysis_markets)
	assign_model(m1)
	
	clusterExport(cl,names(m1))
	
	fname = '../temp/results_20180307.RData'
	
	savemodels <- function() {
	  stuff=setdiff(c('m1','analysis_markets', grep('results[_]', ls(envir=.GlobalEnv), value=T)),'results_brands')
	  print(stuff)
	  save(list=stuff , file = fname)
	  
	}
	
	#### M1 MNL- always diffed, no trends and quarter dummies (starting model)
	results_MNL_diffed <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff='alwaysdiff', lag_heterog=lag_heterog),silent=T)
	})
	
	savemodels()
	

	#### M2
	results_MNL_diffed_onlyFGLS <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff='alwaysdiff', lag_heterog=lag_heterog),silent=T)
	})
	
	savemodels()
	
	# MCI diffed
	m1$plusx=c('nov6sh', 'wpswdst')
	m1$attraction_model='MCI'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MCI_diffed_onlyFGLS <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff='alwaysdiff', lag_heterog=lag_heterog),silent=T)
	})
	
	savemodels()
	
	
	# MCI in levels
	results_MCI_lev <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=lag_heterog),silent=T)
	})
	
	savemodels()
	
	
	# MCI in levels, but homogenous carry over
	results_MCI_lev_hom <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=F),silent=T)
	})
	
	savemodels()
	
	# MCI in levels w trend/quarter
	results_MCI_lev_tq <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'always', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=F), silent=T)
	})
	
	savemodels()
	
	
	# M6: add extra lags
	m1$plusx=c('nov6sh', 'wpswdst', 'lagnov6sh', 'lagwpswdst')
	m1$attraction_model='MCI'
	m1$setup_x=c("rwpspr", "wpswdst", "llen", "nov6sh", "lagrwpspr", "lagwpswdst", "lagllen", "lagnov6sh")
	m1$estmethod='FGLS'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MCI_wlag <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'always', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=F), silent=T)
	})
	
	savemodels()
	
	# turn on again praise winsten
	results_MCI_wlag_ac <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS-Praise-Winsten', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'always', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=F), silent=T)
	})
	
	savemodels()
	
	
	# M9: only use trend if MS has trend, no lagged Xs
	m1$plusx=c('nov6sh', 'wpswdst')
	m1$attraction_model='MCI'
	m1$setup_x=c("rwpspr", "wpswdst", "llen", "nov6sh")
	m1$estmethod='FGLS'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MCI_wlag_urtrend <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=F), silent=T)
	})
	
	savemodels()
	
	# m10
	m1$plusx=c('nov6sh', 'wpswdst', 'lagnov6sh', 'lagwpswdst')
	m1$attraction_model='MCI'
	m1$setup_x=c("rwpspr", "wpswdst", "llen", "nov6sh", "lagrwpspr", "lagwpswdst", "lagllen", "lagnov6sh")
	m1$estmethod='FGLS'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_m10 <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  try(analyze_by_market(i, estmethod='FGLS', setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model, squared=squared, takediff=takediff, lag_heterog=F), silent=T)
	})
	
	savemodels()
