
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
  
  i=27
  simtest = execute_sim(res=results_brands[[i]], sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), test=T, L=500, nperiods=60, shock_period = 30, shock_perc=1.01)
  
  
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
    with(tmp3, sqrt(mean((elast_mean-elast)^2)))
    

  # comparison of long-term elasticities
    # compare
    tmp=simtest[period>=30&brand==brand_of, list(ms_incremental=sum(ms-base_ms), period30_ms=base_ms[period==30]), by = c('brand','variable', 'category','country','market_id')]
    tmp[, elast := 100*(ms_incremental/period30_ms)]
    
    tmp2=results_brands[[i]]$elast[, c('variable','brand','elastlt')]
    tmp3=merge(tmp,tmp2, by =c('variable','brand'))
    
    tmp3[, list(cor(elast,elastlt)),by=c('variable')]
    tmp3[, list(elast_mean=mean(elast), elast_analytical=mean(elastlt), correlation=cor(elast,elastlt), rmse=sqrt(mean((elast-elastlt)^2))),by=c('variable')]
    
    with(tmp3, cor(elast, elastlt))
    with(tmp3, sqrt(mean((elast-elastlt)^2)))
    
    
      
  
	sims <- NULL
	
	#cl <- makePSOCKcluster(11)
	#clusterEvalQ(cl, )
	#markets=c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180)
	markets=seq(along=results_brands)
	
	markets=c(27,30,34,36,116)
	for (i in markets) {
	print(i)
	
	sims[[i]]<-try(execute_sim(res=results_brands[[i]],
	                           sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), 
	                           L=100, nperiods=60, shock_period = 25, shock_perc=1.01, test= T))
	}

	
	plot_irf(sims[[1]])
	
	# short-term
  	sim_dat <- rbindlist(sims[unlist(lapply(sims,function(x) 'data.table'%in%class(x)))])[period==25&brand==brand_of]
  	
  	setnames(sim_dat, 'sim_var','variable')
  	
  	st_results <- rbindlist(lapply(results_brands, function(x) x$elast))
  	sim_dat[, market_id:=as.integer(as.character(market_id))]
  	
    comparison=merge(sim_dat,st_results[, c('market_id','variable','brand','elast')], by =c('market_id','variable','brand'),all.x=T)
  	comparison<-comparison[!is.na(elast)]
  	
  	
    comparison[, list(cor(elast_mean,elast)),by=c('variable')]
	
  
    # long-term
    sim_dat <- rbindlist(sims[unlist(lapply(sims,function(x) 'data.table'%in%class(x)))])[period>=25&brand==brand_of, list(ms_incremental=sum(ms-base_ms), period30_ms=base_ms[period==25]), by = c('brand','sim_var', 'category','country','market_id')]
    sim_dat[, elast_mean := 100*(ms_incremental/period30_ms)]
    
    setnames(sim_dat, 'sim_var','variable')
    
    lt_results <- rbindlist(lapply(results_brands, function(x) x$elastlt))
    sim_dat[, market_id:=as.integer(as.character(market_id))]
    
    comparison=merge(sim_dat,st_results[, c('market_id','variable','brand','elastlt')], by =c('market_id','variable','brand'),all.x=T)
    comparison<-comparison[!is.na(elastlt)]
    
    
    comparison[, list(cor(elast_mean,elastlt)),by=c('variable')]
    comparison[, list(Nmarkets=length(unique(market_id)), elast_simulated=mean(elast_mean), elast_analytical=mean(elastlt), correlation=cor(elast_mean,elastlt), mean_abs_dev=mean(abs(elast_mean-elastlt)), rmse=sqrt(mean((elast_mean-elastlt)^2))),by=c('variable')]
    
  # - for a larger set of models, compare short-term analytical and empirical elasticities
    # simulated elasticities in a comparison of the first ten markets don't correspond
  
  # - understand standard errors of elasticities: need for variance-covariance of parameters, or is sigma (and potential correlations) enough?

  # minor:  
  # only simulate for variable/brand combinations that were actually estimated
  
	
	
	
	
	
	
	
	
	
	
	
    
    # multi-node simulations
    source('proc_simulate.R')
    library(parallel)
    cl<-makePSOCKcluster(11)
    void<-clusterEvalQ(cl, source("proc_simulate.R"))
    void<-clusterEvalQ(cl, require(data.table))
    void<-clusterEvalQ(cl, require(marketingtools))
    
    clusterExport(cl, 'results_brands')
    
    focal_markets = seq(along=results_brands)
    
    sim_res <- parLapplyLB(cl, focal_markets, function(i) {
      resobj <<- results_brands[[i]]
      try(execute_sim(resobj, sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), 
                      L=1000, nperiods=60, shock_period = 25, shock_perc=1.01, test=F),silent=T)
    })
    
    save(sim_res, file='..\\output\\simulation.RData')
 
    
	
    # long-term
    sim_dat <- rbindlist(sim_res[unlist(lapply(sim_res,function(x) 'data.table'%in%class(x)))])[period>=25&brand==brand_of, list(ms_incremental=sum(ms-base_ms), base_ms=base_ms[period==25]), by = c('brand','sim_var', 'category','country','market_id')]
    sim_dat[, elast_mean := 100*(ms_incremental/base_ms)]
    
    setnames(sim_dat, 'sim_var','variable')
    
    lt_results <- rbindlist(lapply(results_brands, function(x) x$elast))
    sim_dat[, market_id:=as.integer(as.character(market_id))]
    
    comparison=merge(sim_dat,lt_results[, c('market_id','variable','brand','elastlt')], by =c('market_id','variable','brand'),all.x=T)
    comparison<-comparison[!is.na(elastlt)]
    
    
    comparison[, list(Nmarkets=length(unique(market_id)), elast_simulated=mean(elast_mean), elast_analytical=mean(elastlt), correlation=cor(elast_mean,elastlt), 
                      mean_abs_dev=mean(abs(elast_mean-elastlt)), 
                      mape = mean(abs((elastlt-elast_mean)/elastlt)),
                      rmse=sqrt(mean((elast_mean-elastlt)^2))),by=c('variable')]

    
    # 
    tmp = melt(comparison, id.vars=c('market_id','category','country','brand','variable'))
    
    for (.var in unique(tmp$variable)) {
      print(.var)
      print(summary(lm(value~1+`variable.1`,data=tmp[variable==.var&`variable.1`%in%c('elastlt','elast_mean')])))
    }
    
    
    
    