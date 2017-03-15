
####################
##### SIMULATION ###
####################
	load(file='..\\output\\results.RData')
	
	# one-node simulations
	source('simulate.R')
	
	sims <- NULL
	for (i in 3:3) {
	print(i)
	
	sims[[i]]<-try(execute_sim(res=results_brands[[1]][[i]]))
	}

		
	# multi-node simulations
	source('simulate.R')
	void<-clusterEvalQ(cl, source("simulate.R"))
	clusterExport(cl, 'results_brands')
	
	focal_markets = 1:nrow(markets)
	#markets[country=='new zealand']$market_id
	
	sim_res <- parLapplyLB(cl, analysis_markets[focal_markets], function(i) {
				resobj <<- results_brands[[1]][[i]]
				try(execute_sim(resobj),silent=T)
				})

	save(sim_res, markets, file='..\\output\\simulation.RData')


####################
##### TESTING ######
####################

	res <- NULL
	for (i in errm) {
	print(i)
	
	out<-try(analyze_by_market(i, setup_y=setup_y, 
										 setup_x=c(price="rwpsprice",dist="wpswdist+1",llength="llength", novel="novel+1"), #,novel="novel+1")
									     setup_xendog=c('price', 'dist', 'llength', 'novel'),
										 setup_xendog_signcutoff=setup_xendog_signcutoff, 
										 trend = trend))
										 res<-c(res, out)
	}

xyplot(novel~date|brand, data=panel, type = 'l')
xyplot(llength~date|brand, data=panel, type = 'l')
xyplot(wpswdist~date|brand, data=panel, type = 'l')
xyplot(rwpsprice~date|brand, data=panel, type = 'l')
							 