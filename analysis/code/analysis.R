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

# add squared terms
	brand_panel[, ':=' (llen_sq = llen^2, wpsun_sq = wpsun^2, nov3sh_sq = nov3sh^2, nov6sh_sq = nov6^2)]

## How many brands are active in multiple countries
	tmp = brand_panel[, list(N=.N), by = c('brand', 'country')]
	tmp[, Ncountries := .N, by = c('brand')]
	tmp[, N:=NULL]
	tmp[, present := T]
	tmp = dcast(tmp, brand + Ncountries ~ country, value.var='present')
	
	tmp[, Ncountries:=-Ncountries]
	setorder(tmp, Ncountries, brand)
	tmp[, Ncountries:=-Ncountries]
	write.table(tmp, 'clipboard', row.names=F)
	
	
### Load additional packages
	require(parallel)
	
### Setup cluster environment
	ncpu = 10
	
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
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov6sh', uniq='wpsun',
		             llen_sq = 'llen_sq', nov6sh_sq = 'nov6sh_sq', wpsun_sq = 'wpsun_sq'),
		   #setup_endogenous = NULL,
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov6sh','wpsun'),
		   trend='none', # choose from: all, ur, none.
		   pval = .05,
		   max.lag = 12, 
		   min.t = 36,
		   descr = 'endog_model',
		   fn = 'endog_model',
		   benchmarkb = NULL,
		   estmethod = "FGLS-Praise-Winsten",
		   attraction_model = "MNL",
		   takediff = 'alwaysdiff',
		   use_quarters = F,
		   plusx = NULL, #c('nov3sh', 'wpswdst'),
		   squared=T,
		   maxiter = 300)

#m1$plusx=c('nov3sh', 'wpswdst')
#m1$attraction_model='MCI'

#models <- list(m1)

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

# deactivate the rest
	# not on cluster
	results_brands <- NULL

	for (m in analysis_markets[1:last.item]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, 
		                                             setup_endogenous = setup_endogenous, 
		                                             trend = trend, pval = pval, max.lag = max.lag, 
		                                             min.t = min.t, maxiter=maxiter, attraction_model = attraction_model, 
		                                             plusx=plusx, use_quarters=use_quarters, squared=squared), silent=T)
		
		assign_model(m1, del = TRUE)
		
	}
	assign_model(m1, del = TRUE)
	
	# calculate elasticities and significance!!!
	
	######################
	# Cluster			 #
	######################
	
# set up cluster

	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	void<-clusterEvalQ(cl, require(data.table))
	
# run estimation for brand-level attraction models
	void<-clusterEvalQ(cl, init())
	last.item = length(analysis_markets)
	assign_model(m1)
	
	clusterExport(cl,names(m1))
	
	
	# MNL without quarter (preferred/main)	
	Sys.time()
	
	results_MNL <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	    if(i==27) {maxit=30000} else {maxit=400}
			try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared),silent=T)
			})
	results_MNL[[124]]=analyze_by_market(138, setup_y = setup_y, setup_x = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov6sh', uniq='wpsun', nov6sh_sq='nov6sh_sq', wpsun_sq='wpsun_sq'), setup_endogenous = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), trend = 'none', maxiter=300, use_quarters=F, estmethod='FGLS-Praise-Winsten',squared=squared)
	
	Sys.time()
	
	save(results_MNL, analysis_markets, m1, file = c('../temp/results_20171023.RData'))
	
	
	
	# Estimate different combinations of squared terms
	Sys.time()
	
	# m1: linear: price/distribution, squared terms: line length, novelty, and uniqueness
	results_m1 <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, 
	                        setup_x = c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov6sh', uniq='wpsun',
	                                    llen_sq = 'llen_sq', nov6sh_sq = 'nov6sh_sq', wpsun_sq = 'wpsun_sq'),
	                        setup_endogenous = 	c('rwpspr', 'wpswdst','llen','nov6sh','wpsun'), 
	                        trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, 
	                        use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared),silent=T)
	})
	results_m1[[124]]=analyze_by_market(138, setup_y = setup_y, 
	                                    setup_x = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov6sh', uniq='wpsun', 
	                                                nov6sh_sq='nov6sh_sq', wpsun_sq='wpsun_sq'), 
	                                    setup_endogenous = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov6sh', uniq='wpsun'), 
	                                    trend = 'none', maxiter=300, use_quarters=F, estmethod='FGLS-Praise-Winsten',squared=squared)
	save(results_m1, analysis_markets, m1, file = c('../temp/results_20171025_m1.RData'))
	
	Sys.time()
	
	# m2: linear: price/distribution, squared terms: line length
	results_m2 <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, 
	                        setup_x = c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', 
	                                    llen_sq = 'llen_sq'),
	                        setup_endogenous = 	c('rwpspr', 'wpswdst','llen'), 
	                        trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, 
	                        use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared),silent=T)
	})
	Sys.time()
	
	# m3: linear: price/distribution, squared terms: line length and novelty
	results_m3 <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, 
	                        setup_x = c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov6sh',
	                                    llen_sq = 'llen_sq', nov6sh_sq = 'nov6sh_sq'),
	                        setup_endogenous = 	c('rwpspr', 'wpswdst','llen','nov6sh'), 
	                        trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, 
	                        use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared),silent=T)
	})
	Sys.time()
	
	# m4: linear: price/distribution, squared terms: line length and uniqueness
	results_m4 <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, 
	                        setup_x = c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', uniq='wpsun',
	                                    llen_sq = 'llen_sq', wpsun_sq = 'wpsun_sq'),
	                        setup_endogenous = 	c('rwpspr', 'wpswdst','llen','wpsun'), 
	                        trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, 
	                        use_quarters=F, plusx=plusx, attraction_model=attraction_model, squared=squared),silent=T)
	})
	
	Sys.time()
	
	
	save(results_m1, results_m2, results_m3, results_m4, analysis_markets, m1, file = c('../temp/results_20171024.RData'))
	
	
	
	
	# explore squared terms
	load(file = c('../temp/results_20171023.RData'))
	
	
	# identify model crashes
	results_brands<-results_MNL
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	last.item = length(analysis_markets)
	
	# provide overview of squared terms
	squares <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) data.table(market_id=unique(x$specs$market_id),
	                                                                               category=unique(x$specs$category),
	                                                                               country=unique(x$specs$country),
	                                                                               x$squared_terms)))
	
	pval_sq=.1
	critval = abs(qnorm(pval_sq/2))
	
	tmp=squares[!is.na(lin_coef), list(N_squares_tested=.N, 
	               perc_noeffect = length(which(abs(sq_z)<critval & abs(lin_z)<critval))/.N,
	               perc_pos_lin = length(which(abs(sq_z)<critval & abs(lin_z)>=critval & lin_coef>0))/.N,
	               perc_neg_lin = length(which(abs(sq_z)<critval & abs(lin_z)>=critval & lin_coef<0))/.N,
	               
	               
	               perc_invU = length(which(sq_coef<0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
	               perc_U = length(which(sq_coef>0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
	               perc_pos_decr = length(which(inflection_point>=max&sq_coef<0&abs(sq_z)>=critval))/.N,
	               perc_pos_incr = length(which(inflection_point<=min&sq_coef>0&abs(sq_z)>=critval))/.N,
	               perc_neg_decr = length(which(inflection_point>=max&sq_coef>0&abs(sq_z)>=critval))/.N,
	               perc_neg_incr = length(which(inflection_point<=min&sq_coef<0&abs(sq_z)>=critval))/.N),
	               by = c('var')]

	tmp=squares[!is.na(lin_coef), list(N_squares_tested=.N, 
	                                   perc_sq_insig = length(which(abs(sq_z)<critval))/.N,
	                                   perc_invU = length(which(sq_coef<0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
	                                   perc_U = length(which(sq_coef>0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
	                                   perc_pos_decr = length(which(inflection_point>=max&sq_coef<0&abs(sq_z)>=critval))/.N,
	                                   perc_pos_incr = length(which(inflection_point<=min&sq_coef>0&abs(sq_z)>=critval))/.N,
	                                   perc_neg_decr = length(which(inflection_point>=max&sq_coef>0&abs(sq_z)>=critval))/.N,
	                                   perc_neg_incr = length(which(inflection_point<=min&sq_coef<0&abs(sq_z)>=critval))/.N),
	            by = c('var')]
	
	tmp
	
  squares[, type:=NULL]
  squares[!is.na(lin_coef), type:='']
  
	squares[!is.na(lin_coef) & abs(sq_z)<critval, type := 'sq_insig']
	squares[!is.na(lin_coef) & sq_coef<0&abs(sq_z)>=critval&inflection_inrange==T, type := 'invU']
	squares[!is.na(lin_coef) & sq_coef>0&abs(sq_z)>=critval&inflection_inrange==T, type := 'U']
	squares[!is.na(lin_coef) & inflection_point>=max&sq_coef<0&abs(sq_z)>=critval, type := 'pos_decr']
	squares[!is.na(lin_coef) & inflection_point<=min&sq_coef>0&abs(sq_z)>=critval, type := 'pos_incr']
	squares[!is.na(lin_coef) & inflection_point>=max&sq_coef>0&abs(sq_z)>=critval, type := 'neg_decr']
	squares[!is.na(lin_coef) & inflection_point<=min&sq_coef<0&abs(sq_z)>=critval, type := 'neg_incr']
	
	
	          
	squares[!is.na(lin_coef) & inflection_inrange == F & 
	          inflection_point > max & sq_coef < 0 & abs(sq_z)>=crit_val, type := 'sq-incr_positive']
	
	tmp=squares[!is.na(lin_coef), list(N_squares_tested=.N, 
	                                   perc_sq_insig = length(which(abs(sq_z)<critval))/.N,
	                                   perc_invU = length(which(sq_coef<0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
	                                   perc_U = length(which(sq_coef>0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
	                                   perc_pos_decr = length(which(inflection_point>=max&sq_coef<0&abs(sq_z)>=critval))/.N,
	                                   perc_pos_incr = length(which(inflection_point<=min&sq_coef>0&abs(sq_z)>=critval))/.N,
	                                   perc_neg_decr = length(which(inflection_point>=max&sq_coef>0&abs(sq_z)>=critval))/.N,
	                                   perc_neg_incr = length(which(inflection_point<=min&sq_coef<0&abs(sq_z)>=critval))/.N),
	            by = c('var')]
	
  squares[, brand:=sapply(original_variable, function(x) strsplit(x, '[_]')[[1]][1])]
  
	# summarize coefficients
	coefs <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) data.table(market_id=unique(x$specs$market_id),
	                                                                                         category=unique(x$specs$category),
	                                                                                         country=unique(x$specs$country),
	                                                                                       x$model@coefficients)))
 	coefs <- coefs[!(grepl('cop[_]', varname)|varname=='lagunitsales_sh'|varname=='dum')]
	setnames(coefs, 'variable', 'original_variable')
	
	coefs[, sq_term := ifelse(grepl('[_]sq', original_variable), 'sq', 'lin')]
	coefs[, varname2 := gsub('[_]sq', '', varname)]
	
	tmp = melt(coefs, id.vars=c('market_id', 'category', 'country', 'original_variable', 'brand', 'varname', 'sq_term', 'varname2'))
	
	coefs=dcast.data.table(tmp,market_id+category+country+brand+varname2~sq_term+variable, value.var='value')
	setnames(coefs, 'varname2', 'varname')
	
	# check
	
	
	
	coefs[, type := '']
	crit_val = abs(qnorm(pval_sq/2))
	coefs[is.na(sq_coef)&lin_coef>0&abs(lin_z)>=crit_val, type := 'pos-incr-or-decr']
	coefs[!is.na(sq_coef)&lin_coef>0&abs(lin_z)>=crit_val&sq_z<crit_val, type := 'pos-incr-or-decr']
	
	coefs[is.na(sq_coef)&lin_coef<0&abs(lin_z)>=crit_val, type := 'neg-incr-or-decr']
	coefs[!is.na(sq_coef)&lin_coef<0&abs(lin_z)>=crit_val&sq_z<crit_val, type := 'neg-incr-or-decr']
	
	coefs[!is.na(sq_coef)&sq_coef<0&abs(sq_z)>=crit_val, type := 'inverseU']
	coefs[!is.na(sq_coef)&sq_coef>0&abs(sq_z)>=crit_val, type := 'U']
	coefs[is.na(sq_coef)&abs(lin_z)<crit_val, type := 'insig']
	coefs[abs(lin_z)<crit_val&abs(sq_z)<crit_val, type := 'insig']
	
	tmp = coefs[, list(N=.N), by = c('varname', 'type')]
	
	tmp=data.table(dcast(tmp, varname~type))
	tmp[, Ntotal := rowSums(tmp[,-1, with=F], na.rm=T)]
	
	setcolorder(tmp, c('varname','Ntotal', 'pos-incr-or-decr', 'neg-incr-or-decr', 'U', 'inverseU', 'insig'))
	
	

	
	Sys.time()

	# MNL without quarter (preferred/main), with nov6_sh
	Sys.time()
	
	m1$setup_x["nov"]<-'nov6sh'
	m1$setup_endogenous[which(m1$setup_endogenous=='nov3sh')]<-'nov6sh'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MNL_6sh <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model),silent=T)
	})
	results_MNL_6sh[[124]]=analyze_by_market(138, setup_y = setup_y, setup_x = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov6sh', uniq='wpsun'), setup_endogenous = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov6sh', uniq='wpsun'), trend = 'none', maxiter=300, use_quarters=F, estmethod='FGLS-Praise-Winsten')
	
	
	Sys.time()
	
	# MNL with quarter	

	m1$setup_x["nov"]<-'nov3sh'
	m1$setup_endogenous[which(m1$setup_endogenous=='nov6sh')]<-'nov3sh'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	Sys.time()
	results_MNL_wquarter <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model),silent=T)
	})
	Sys.time()
	
	# MCI without quarter
	m1$plusx=c('nov3sh', 'wpswdst')
	m1$attraction_model='MCI'
	assign_model(m1, del = TRUE)
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MCI <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model),silent=T)
	})
	results_MCI[[124]]=analyze_by_market(138, setup_y = setup_y, setup_x = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), setup_endogenous = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), trend = 'none', maxiter=300, use_quarters=F, estmethod='FGLS-Praise-Winsten')
	
  save(results_MNL, results_MNL_6sh, results_MCI, results_MNL_wquarter, analysis_markets, m1, file = c('../temp/results_20170905.RData'))
  Sys.time()
  
 # run up to this point. 

  ################
  # DESCRIPTIVES #
  ################
  
  # compare elasticities: nov6 vs nov3
  
  load('../temp/results_20170905.RData')
  
  get_elast <- function(results_brands) {
    checks <- unlist(lapply(results_brands, class))
    
    cat('error report:\n\n')
    print(table(checks))
  
    # elasticities
    elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
  
    zval=1.69
    out=elast[!is.na(coef), list(median_elast = median(elast), 
                           w_elast = sum(elast/elast_se)/sum(1/elast_se), 
                           N_brands= .N, 
                           perc_positive = length(which(z>=(zval)))/.N, 
                           perc_null = length(which(abs(z)<zval))/.N, 
                           perc_negative = length(which(z<=(-zval)))/.N), by=c('variable')]
    res=list(checks=checks, elast=out)
    return(res)
  }
  
  m3 = get_elast(results_MNL)
  m6 = get_elast(results_MNL_6sh)
  
  {
  cat('Summary of elasticities with novelty share 3-months\n')
  print(m3$elast)
  cat('\n\nSummary of elasticities with novelty share 6-months\n')
  print(m6$elast)
  }
  
############### LEGACY CODE #########################  
  
  
  
  
#####################################
# COMPARISON QUARTER VS NOT QUARTER #
#####################################
  
  load(file = c('../temp/results_noquarter.RData'))

	# model crashes
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	last.item = length(analysis_markets)

	err_markets = data.frame(market_id=analysis_markets[1:last.item][which(checks=='try-error')],msg=as.character(results_brands[which(checks=='try-error')]))
	merge(err_markets,markets,by=c('market_id'),all.x=T, all.y=F)
	
	# elasticities
	elast_noq <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
	

	fwrite(elast_noq, file = '../output/elasticities.csv', row.names=F)
	
	
	
	load(file = c('../temp/results_withquarter.RData'))
	# model crashes
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	last.item = length(analysis_markets)
	
	# elasticities
	elast_q <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
	
	# compare elasticities
	elast_q[, source:='quarter']
	elast_noq[, source:='noquarter']
	
	melast = rbind(elast_q, elast_noq)
	
	tmp=dcast(melast[variable%in%setup_x&!grepl('cop', var_orig)], brand+market_id+variable~source, value.var='coef')
	tmp=data.table(tmp)
	
	tmp[, (cor(noquarter,quarter,use='pairwise.complete')), by = c('variable')]
	

######################
# ITERATION ANALYSIS #
######################

	

if(0){
	iters<-fread('iter_out.csv')
	
	for (cols in setdiff(colnames(iters), c('iteration','delta'))) {
		fn=paste0('../temp/iter_', cols, '.png')
		png(fn, res=400, units='in', height=8, width=8)
		iters[, varplot := get(cols)]
		print(xyplot(varplot~iteration, main = cols, ylab = cols, xlab = 'iteration', type ='l', data = iters))
		dev.off()
		}
}
	
#####################
# summarize results #
#####################

		load(file = c('../temp/results_noquarter.RData'))
	
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
	
	fn = '../output/elast2.png'
	
	png(fn, res=400, units='in', height=8, width=8)
	
	par(mfrow=c(3,2))
	hist(elast[variable=='llen']$elast, breaks=100, col='grey', main = 'llen', xlab = 'elasticity')
	hist(elast[variable=='rwpspr']$elast, breaks=100, col='grey', main = 'price', xlab = 'elasticity') # -15
	hist(elast[variable=='wpsun']$elast, breaks=100, col='grey', main = 'unique', xlab = 'elasticity')
	hist(elast[variable=='wpswdst']$elast, breaks=100, col='grey', main = 'dist', xlab = 'elasticity')
	hist(elast[variable=='nov3sh']$elast, breaks=100, col='grey', main = 'noveltyshare', xlab = 'elasticity')
	
	dev.off()
	
	elast[variable=='llen' & abs(elast) > 6]
	
	elast[variable=='rwpspr' & abs(elast) > 6]
	

	elast[variable=='llen' & abs(elast) > 6]
	# check data / model outputs for top and bottom 10; plus 5+
	elast[variable=='rwpspr' & elast > 6]

