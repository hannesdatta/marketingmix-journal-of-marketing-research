
####################
##### SIMULATION ###
####################

  # Setup
  require(data.table)
  require(marketingtools)

	load(file='..\\output\\results.RData')
	
	# define model
  results_brands <- results_main
  
  brand_panel=fread('../temp/preclean.csv')
  brand_panel[, ':=' (date = as.Date(date))]
  
  
	# load simulation code
	source('proc_simulate.R')
  
  i=40
  simtest = execute_sim(res=results_brands[[i]], sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), L=1000, nperiods=60, shock_period = 30, shock_perc=1.01)
  
  
  plot_irf(simtest)
  
  # comparison of short-term elasticities
    simtest[period==30]
    setnames(simtest, 'sim_var','variable') 
    # compare
    tmp=simtest[period==30&brand==brand_of]
    
    tmp2=results_brands[[i]]$elast[, c('variable','brand','elast')]
    tmp3=merge(tmp,tmp2, by =c('variable','brand'))
    
    tmp3[, list(elast_mean=mean(elast_mean), correlation=cor(elast_mean,elast), rmse=sqrt(mean((elast_mean-elast)^2))),by=c('variable')]
    
    
    with(tmp3, cor(elast_mean, elast))
    
    #
    with(tmp3, sqrt(mean((elast_mean-elast)^2)))
    
    
         
  # comparison of long-term elasticities
    
    # compare
    tmp=simtest[period>=30&brand==brand_of, list(ms_incremental=sum(ms-base_ms), period30_ms=base_ms[period==30]), by = c('brand','variable', 'category','country','market_id')]
    tmp[, elast := 100*(ms_incremental/period30_ms)]
    
    tmp2=results_brands[[i]]$elast[, c('variable','brand','elastlt')]
    tmp3=merge(tmp,tmp2, by =c('variable','brand'))
    
    tmp3[, list(cor(elast,elastlt)),by=c('variable')]
    
    with(tmp3, cor(elast, elastlt))
    
    # absolute size?!
    
    
    
    
    
  
	sims <- NULL
	for (i in 1:10) {
	print(i)
	
	sims[[i]]<-try(execute_sim(res=results_brands[[i]],
	                           sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov6sh'), 
	                           L=1000, nperiods=60, shock_period = 30, shock_perc=1.01))
	}

	
	plot_irf(sims[[1]])
	
	
	sim_dat <- rbindlist(sims)[period==30&brand==brand_of]
	setnames(sim_dat, 'sim_var','variable')
	st_results <- rbindlist(lapply(results_brands, function(x) x$elast))
  comparison=merge(sim_dat,st_results[, c('market_id','variable','brand','elast')], by =c('market_id','variable','brand'),all.x=T)
	comparison<-comparison[!is.na(elast)]
	
	
  comparison[, list(cor(elast_mean,elast)),by=c('variable')]
	
  

  # - for a larger set of models, compare short-term analytical and empirical elasticities
    # simulated elasticities in a comparison of the first ten markets don't correspond
  
  # - understand standard errors of elasticities: need for variance-covariance of parameters, or is sigma (and potential correlations) enough?

  # minor:  
  # only simulate for variable/brand combinations that were actually estimated
  
	
	
	
	
	
	
	
	
	
	
	
	
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
	
	
	
	