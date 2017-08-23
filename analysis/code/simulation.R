
####################
##### SIMULATION ###
####################

  # Setup
  require(data.table)
  require(marketingtools)

	load(file='..\\temp\\results_20170822.RData')
	
  results_brands <- lapply(results_MNL, function(x) {x$attraction_model='MNL';return(x)})
   
	# one-node simulations
	source('proc_simulate.R')
	
	sims <- NULL
	for (i in 1:1) {
	print(i)
	
	sims[[i]]<-try(execute_sim(res=results_brands[[i]]))
	}

	plot_irf(sims[[1]])
	
	
	simtest = execute_sim(res=results_brands[[i]], sim_vars = c('llen'), L=1000, nperiods=60, shock_period = 30, shock_perc=1.01)
	plot_irf(simtest)
	
	

	# to do: 23/8/2017
	# find out why the elasticities in the simulation are soooo small! they should be much larger, given the model's parameter estimates
	
	
	# - understand standard errors of elasticities: need for variance-covariance of parameters, or is sigma (and potential correlations) enough?
	# - for a larger set of models, compare short-term analytical and empirical elasticities
	
	
	
	
	
	
	
	
	
	
	
	# multi-node simulations
	source('proc_simulate.R')
	void<-clusterEvalQ(cl, source("proc_simulate.R"))
	clusterExport(cl, 'results_brands')
	
	focal_markets = 1:nrow(markets)
	#markets[country=='new zealand']$market_id
	
	sim_res <- parLapplyLB(cl, analysis_markets[focal_markets], function(i) {
	  resobj <<- results_brands[[1]][[i]]
	  try(execute_sim(resobj),silent=T)
	})
	
	save(sim_res, markets, file='..\\output\\simulation.RData')
	
	
	
	