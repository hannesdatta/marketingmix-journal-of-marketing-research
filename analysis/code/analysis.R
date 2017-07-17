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
	
	init()

# To do
# - MCI vs MNL
# - add switches to proc_analysis



# Specify models
try(detach(m1), silent=T)
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
		   use_quarters = T,
		   maxiter = 300)
attach(m1)

#models <- list(m1)

####################
### RUN ANALYSIS ###
####################


	assign_model <- function(m, del=F) {
		if (del==F) {
			for (l in seq(along=m)) {
				var = m[[l]]
				eval(parse(text=paste0(names(m)[l], ' <<- var')))
				}
			}
		if (del==T) {
			for (l in seq(along=m)) eval(parse(text=paste0('rm(', names(m)[l],')')))
			}
		}
		
# define markets for analysis
	markets <- brand_panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	# for now, exclude markets with only two brands
	#markets <- markets[n_brands>2]
	
	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
	
	last.item = length(analysis_markets)
	last.item = 10

# deactivate the rest
	# not on cluster
	results_brands <- NULL
	try(detach(m1), silent=T)
	
	for (m in analysis_markets[1:last.item]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = .05, max.lag = 12, min.t = 36, maxiter=300), silent=T)

	}
	
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
	results_brands <- NULL
	assign_model(m1)
	
	clusterExport(cl,names(m1))
	
		
	Sys.time()
	results_brands <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
			try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t, maxiter = 300, use_quarters=T),silent=T)
			})
	Sys.time()
	

# Save results
save(results_brands, analysis_markets, m1, file = c('../temp/stashed_results.RData'))

	# model crashes
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	
	err_markets = data.frame(market_id=analysis_markets[1:last.item][which(checks=='try-error')],msg=as.character(results_brands[which(checks=='try-error')]))

	merge(err_markets,markets,by=c('market_id'),all.x=T, all.y=F)
	


	#errs <- err_markets$market_id
	
	# plot markets that crash
	
	
	
	try(detach(m1), silent=T)
	
	err_ms<-err_markets$market_id
	errs<-NULL
	for (m in seq(along=err_ms)) {
		assign_model(m1)
		cat('////////////////////////////////////////////\nMODEL ', m, '.....\n////////////////////////////////////////////\n\n\n\n\n\n')
		errs[[m]] <- try(analyze_by_market(err_ms[m], setup_y = setup_y, setup_x = setup_x, setup_endogenous = NULL, trend = 'none', pval = .05, max.lag = 12, min.t = 36, maxiter=300, use_quarters=T,
							  estmethod='FGLS-Praise-Winsten'), silent=T)

	}
	
	checks <- unlist(lapply(errs, class))
	table(checks)
	
	source('d:\\DATTA\\Dropbox\\Tilburg\\Projects\\marketingtools\\R\\itersur.R')

#####################
# summarize results #
#####################

	# To do:
	
	# Check non-converging cases
	# MNL/MCI 
	# Verify extreme-elasticity markets
	# check elasticities for brands that come in when I select 4 years
err_markets <- c(33,  38,  40,  83, 115, 134, 137, 141, 143, 145)

	brand_panel[market_id%in%err_markets, list(dates=length(unique(date)), brands = length(unique(brand))), by=c('country', 'category', 'market_id')]
	
out=analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t)
out=analyze_by_market(i, setup_y = setup_y, 
						 setup_x = setup_x, 
						 setup_endogenous = c("rwpspr", "wpswdst", "llen", "nov3sh", "wpsun"), 
						 trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t)


i=134

brand_panel[market_id==134, usalessh2 := runif(.N), by = c('date')]
brand_panel[market_id==134, usalessh2 := usalessh2/sum(usalessh2,na.rm=T), by = c('date')]


brand_panel <- brand_panel[market_id==134&date<'2008-01-01']

out=analyze_by_market(i, setup_y = 'usalessh', 
						 setup_x = c("wpswdst", "llen", "nov3sh", "wpsun"), 
						 setup_endogenous = NULL, #c("rwpspr","wpswdst", "llen", "nov3sh", "wpsun"), 
						 trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t,
						 estmethod = "FGLS", use_quarters=FALSE)
						 
						 #, benchmarkb = "celestial")
			"llen", 		-Praise-Winsten	 
				
"rwpspr", 
# all the rest runs well if I take out the Copula terms

data.frame(market_id=analysis_markets[1:last.item][which(checks=='try-error')],msg=as.character(results_brands[which(checks=='try-error')]))
brand_panel[market_id%in%err_markets, list(obs=length(unique(date))), by = c('market_id', 'country', 'category')]

						 
brand_panel[market_id==i, list(.N), by = c('brand')]


panel <<- brand_panel[market_id==i]
panel[, cop := makediff(make_copula(wpswdst)), by = c('brand')]
panel[, diff := makediff(rwpspr), by = c('brand')]
panel[, diff2 := makediff(llen), by = c('brand')]

	xyplot(llen~date|brand, data=brand_panel[market_id==i],type='l')
	
xyplot(rwpspr~date|brand, data=panel,type='l')

xyplot(diff~date|brand, data=panel,type='l')

xyplot(wpswdst~date|brand, data=panel,type='l')
xyplot(nov3sh~date|brand, data=panel,type='l')
xyplot(wpsun~date|brand, data=panel,type='l')


xyplot(usalessh~date|brand, data=panel,type='l')

xyplot(cop~date|brand, data=panel,type='l')

tmp=dcast(panel, date~brand, value.var	= c('cop'))
abs(cor(as.matrix(tmp[,-c(1)]), use='pairwise'))


, ''
tmp=dcast(panel, date~brand, value.var	= c('diff'))
tmp=cbind(tmp, dcast(panel, date~brand, value.var	= c('diff2')))

tmp$date<-NULL

abs(cor(as.matrix(tmp), use='pairwise'))


					 
	# model crashes
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	
	err_markets = data.frame(market_id=analysis_markets[1:last.item][which(checks=='try-error')],msg=as.character(results_brands[which(checks=='try-error')]))
	
	err_markets = err_markets$market_id
	
	
	test=brand_panel[market_id%in%err_markets,list(.N), by = c('market_id', 'country', 'category', 'brand')]
	
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
	
	elast[variable=='llen' & abs(elast) > 6]
	# check data / model outputs for top and bottom 10; plus 5+
	elast[variable=='rwpspr' & elast > 6]
	
	# same cases? e.g. no. of observations
	