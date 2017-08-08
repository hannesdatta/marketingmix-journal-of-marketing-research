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
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov3sh', uniq='wpsun'),
		   #setup_endogenous = NULL,
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov3sh','wpsun'),
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
		                                             plusx=plusx, use_quarters=use_quarters), silent=T)
		
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
			try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model),silent=T)
			})
	results_MNL[[124]]=analyze_by_market(138, setup_y = setup_y, setup_x = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), setup_endogenous = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), trend = 'none', maxiter=300, use_quarters=F, estmethod='FGLS-Praise-Winsten')
	
	Sys.time()
	
	# MNL with quarter	
	Sys.time()
	results_MNL_wquarter <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=T, plusx=plusx, attraction_model=attraction_model),silent=T)
	})
	Sys.time()
	
	# MCI without quarter
	m1$plusx=c('nov3sh', 'wpswdst')
	m1$attraction_model='MCI'
	clusterExport(cl,names(m1))
	
	results_MCI <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
	  if(i==27) {maxit=30000} else {maxit=400}
	  try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = trend, pval = pval, max.lag = max.lag, min.t = min.t, maxiter = maxit, use_quarters=F, plusx=plusx, attraction_model=attraction_model),silent=T)
	})
	results_MCI[[124]]=analyze_by_market(138, setup_y = setup_y, setup_x = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), setup_endogenous = c(price = 'rwpspr', dist = 'wpswdst', nov = 'nov3sh', uniq='wpsun'), trend = 'none', maxiter=300, use_quarters=F, estmethod='FGLS-Praise-Winsten')
	
  save(results_MNL, results_MCI, results_MNL_wquarter, analysis_markets, m1, file = c('../temp/results_20170731.RData'))
#

  
  
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

	# To do:
	
	# Check non-converging cases
	# MNL/MCI 
	# Verify extreme-elasticity markets
	# check elasticities for brands that come in when I select 4 years


	
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

