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
library(data.table)
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
	library(parallel)
	
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
	

	m1 <- list(setup_y=c(usalessh = 'usalessh'),
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov6sh'),
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov6sh'),
		   plusx = c('nov6sh', 'wpswdst'),
		   trend='always', # choose from: always, ur, none.
		   pval = .1,
		   max.lag = 12, 
		   min.t = 36,
		   descr = 'model',
		   fn = 'model',
		   benchmarkb = NULL,
		   estmethod = "FGLS",
		   attraction_model = "MCI",
		   takediff = 'none', # choose from flexible (UR approach), none, alwaysdiff
		   use_quarters = T,
		   lag_heterog = F,
		   squared=F,
		   maxiter = 400,
		   carryover_zero=F)

assign_model(m1)
#assign_model(m1,del=T)

m2 <- m1
m2$plusx=c('nov6sh', 'wpswdst', 'lagnov6sh', 'lagwpswdst')
m2$setup_x=c("rwpspr", "wpswdst", "llen", "nov6sh", "lagrwpspr", "lagwpswdst", "lagllen", "lagnov6sh")
m2$trend = 'none'

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
		                                             min.t = min.t, maxiter=maxiter, 
		                                             attraction_model = attraction_model, 
		                                             plusx=plusx, use_quarters=use_quarters, 
		                                             squared=squared, takediff=takediff, 
		                                             lag_heterog=lag_heterog, carryover_zero=carryover_zero), silent=T)
		
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

	fname = '../temp/results_20180313.RData'
	
	savemodels <- function() {
	  lscall=ls(envir=.GlobalEnv)
	  stuff=setdiff(c(grep('^m[0-9]+', lscall,value=T),'analysis_markets', grep('results[_]', lscall, value=T)),'results_brands')
	  cat('saving...\n')
	  print(stuff)
	  cat('\ndone.\n')
	  save(list=stuff , file = fname)
	  }
	
	
	# M6: without lagged DVs, but with trend
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MCI_wolag_trend <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                        setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                        max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                        use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                        squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=carryover_zero), silent=T)
	                        })
	
	# inspecting the carry-overs yields a negative one in NZ; set to zero
	if(results_MCI_wolag_trend[[27]]$market_id==31 & data.table(results_MCI_wolag_trend[[27]]$model@coefficients)[grepl('lagunitsales',variable)]$coef<0) {
	  
	  results_MCI_wolag_trend[[27]] <- try(analyze_by_market(31, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                          setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                          max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                          use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                          squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=T), silent=T)
	}

#	savemodels()
	
  # m10
	assign_model(m1, del = TRUE)
	assign_model(m2)
	clusterExport(cl,names(m2))
	
	results_MCI_wlag_wotrend <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  maxit=400
	  
	  # note: TV1 in australia can only be estimated without the lagged vars for novelty and llen
	  try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = if(!i==138) setup_x else setup_x[c(1:6)],
	                        setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                        max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                        use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                        squared=squared, takediff=takediff, lag_heterog=lag_heterog, carryover_zero=carryover_zero), silent=T)
	})
	
	# inspecting the carry-overs yields a negative one in NZ; set to zero
	if(results_MCI_wlag_wotrend[[27]]$market_id==31 & data.table(results_MCI_wlag_wotrend[[27]]$model@coefficients)[grepl('lagunitsales',variable)]$coef<0) {
	 
	results_MCI_wlag_wotrend[[27]] <- try(analyze_by_market(31, estmethod=estmethod, setup_y = setup_y, setup_x = if(!31==138) setup_x else setup_x[c(1:6)],
	                                                         setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                                                         max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                                                         use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                                                         squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=T), silent=T)
	}
	
	
	savemodels()

	