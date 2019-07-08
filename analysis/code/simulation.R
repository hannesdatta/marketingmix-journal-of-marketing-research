
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
  
  prototype = F
  cluster = T
  
  if (prototype==T) {
    
    i=116
    system.time({
    sim_call = execute_sim(res=results_brands[[i]], sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), 
                          test=T, 
                          L=1000, 
                          nperiods=60, 
                          shock_period = 30, 
                          shock_perc=1.01)
    })
    
    
    plot_irf(sim_call$marketshares)
    
    # comparison of short-term elasticities
    simtest=sim_call$elasticities[brand==brand_of][,brand_of:=NULL]
    setnames(simtest, 'sim_var','variable') 
    
    simtest=merge(simtest,results_brands[[i]]$elast[, c('variable','brand','elast', 'elast_se', 'elastlt', 'elastlt_se')], by =c('variable','brand'))
  
    #tmp3[, w_sim:=(1/elast_sd)/sum(1/elast_sd)]
    #tmp3[, w_emp:=(1/elast_se)/sum(1/elast_se)]
      
    
    simtest[, list(simelast_st=mean(simelast),
                   simelast_lt=mean(simelastlt),
                   correlation_st=cor(simelast,elast),
                   correlation_lt=cor(simelastlt, elastlt),
                   rmse_st=sqrt(mean((simelast-elast)^2)),
                   rmse_lt=sqrt(mean((simelastlt-elastlt)^2))),by=c('variable')]
      
    
    
    sims <- NULL
  	markets=seq(along=results_brands)
  	markets=c(27,30,34,36)
  	for (i in markets) {
  	print(i)
  	
  	sims[[i]]<-try(execute_sim(res=results_brands[[i]],
  	                           sim_vars = c('rwpspr', 'wpswdst', 'llen', 'nov12sh'), 
  	                           L=1000, nperiods=60, shock_period = 25, shock_perc=1.01, test= F))
  	}
  
  	plot_irf(sims[[1]]$marketshares)
  	
  	# short-term
    sim_dat <- rbindlist(lapply(sims[unlist(lapply(sims,function(x) 'data.table'%in%class(x$elasticities)))], function(x) x$elasticities))
    sim_dat=sim_dat[brand==brand_of]
    setnames(sim_dat, 'sim_var','variable')
    elast_results <- rbindlist(lapply(results_brands, function(x) x$elast))
  	
    sim_dat[, market_id:=as.integer(as.character(market_id))]
    	
    comparison=merge(sim_dat,elast_results[, c('market_id','variable','brand','elast', 'elast_se', 'elastlt', 'elastlt_se')], by =c('market_id','variable','brand'),all.x=T)
  	comparison<-comparison[!is.na(elast)]
    	
    	
    comparison[, list(cor_st=cor(elast, simelast), 
                      corse_st = cor(elast_se, simelast_sd),
                      cor_lt=cor(elastlt, simelastlt),
                      corse_lt=cor(elastlt_se, simelastlt_sd)),by=c('variable')]
  }

    
  if (cluster==T) {
    
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
    
  }
  
  if(0) {
    
    
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
    
    
  }
