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


# Model: MCI or MNL
# Everything in differences

#brand_panel[, ':=' (wpswdst = wpswdst+1, nov3 = nov3 + 1)]


# Specify models
try(detach(m1), silent=T)
m1 <- list(setup_y=c(usalessh = 'usalessh'),
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov3div', uniq='wpsun'),
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov3div','wpsun'),
		   #setup_endogenous=c(price = 'iv[_].*mcrwpsprd', dist = 'iv[_].*mcwpswdst', llen = 'iv[_].*mcllen', nov = 'iv[_].*mcnov3'),
		   trend='none', # choose from: all, ur, none.
		   pval = .05,
		   max.lag = 12, 
		   min.t = 36,
		   descr = 'endog_model',
		   fn = 'endog_model')
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
	markets <- brand_panel[, list(.N), by=c('market_id','country', 'category')]
	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
	
	last.item = length(analysis_markets)
#last.item = 10

# deactivate the rest
	# not on cluster
	results_brands <- NULL
	try(detach(m1), silent=T)
	
	for (m in analysis_markets[1:last.item]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = .05, max.lag = 12, 	min.t = 36), silent=T)

	}
	
	# calculate elasticities and significance!!!
	
	######################
	# Cluster			 #
	######################
	
# set up cluster

	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	
	void<-clusterEvalQ(cl, init())
	void<-clusterEvalQ(cl, require(data.table))
	
	#void<-clusterEvalQ(cl, require(RStata))
	
	#clusterEvalQ( cl, options("RStata.StataVersion"=14))
	#clusterEvalQ( cl, options("RStata.StataPath"="\"C:\\Program Files (x86)\\Stata14\\StataSE-64\""))
	
	# cannot parallelize RStata
	
# run estimation for brand-level attraction models
	results_brands <- NULL
	assign_model(m1)
	
	clusterExport(cl,names(m1))
	
		
	Sys.time()
	results_brands <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
			try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t),silent=T)
			})
	Sys.time()
	


# save results
	save(results_brands, markets, file='..\\output\\results_withcopula.RData')
	save(results_brands, markets, file='..\\output\\results_withoutcopula.RData')
	
	save(results_brands, markets, file='..\\output\\results_withoutcopula_levels.RData')
	
	#load(file='..\\output\\results_withoutcopula_levels.RData')
	
	save(results_brands, markets, file='..\\output\\results_withoutcopula_levels_nolagms.RData')
	

save(results_brands, markets, file='..\\output\\results_withcopula_nopraiswinsten.RData')
	
save(results_brands, markets, file='..\\output\\results_28marchharaldmarnikfinal.RData')
	
		
	
	
# summarize elasticities
	checks <- unlist(lapply(results_brands, class))
	table(checks)
	
	elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))

	zval=1.69
	elast[!is.na(coef), list(median_elast = median(elast), 
												  w_elast = sum(elast/elast_se)/sum(1/elast_se), 
												  N_brands= .N, 
												  perc_positive = length(which(z>=(zval)))/.N, 
												  perc_null = length(which(abs(z)<zval))/.N, 
												  perc_negative = length(which(z<=(-zval)))/.N), by=c('variable')]
		
	copulas <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$model@coefficients))
	copulas <- copulas[grepl('cop_', varname)]
	
	copulas[!is.na(coef), list(median = median(coef), 
											  weighted = sum(coef/se)/sum(1/se), 
											  N_brands= .N, 
											  perc_positive = length(which(z>=(zval)))/.N, 
											  perc_null = length(which(abs(z)<zval))/.N, 
											  perc_negative = length(which(z<=(-zval)))/.N), by=c('varname')]

	season <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$model@coefficients))
	season <- season[grepl('quarter[0-9]_', variable)]
	
	season[!is.na(coef), list(median = median(coef), 
											  weighted = sum(coef/se)/sum(1/se), 
											  N_brands= .N, 
											  perc_positive = length(which(z>=(zval)))/.N, 
											  perc_null = length(which(abs(z)<zval))/.N, 
											  perc_negative = length(which(z<=(-zval)))/.N), by=c('brand')]
												  
		# 1 model needs two minutes
		
	
	
	
	
	
	
	res=analyze_by_market(1, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'none', pval = .05, max.lag = 12, 	min.t = 36)
	
	
	
	
	
	#summarize output from ivreg2
	
	ivreg2=rbindlist(lapply(results_brands[unlist(lapply(results_brands,class))=='list'], function(x) data.frame(category = unique(x$specs$category), country = unique(x$specs$country), market_id = unique(x$specs$market_id), x$ivreg2)))
	ivreg2[, value:=as.numeric(value)]
	ivreg2[, variable := as.character(variable)]
	
#	ivreg2[variable=='sargan', list(N=.N, Ninsig = length(which(value>.1)), perc = length(which(value>.1))/.N)]
	
	
#	summary(ivreg2[rownames=='SWFp']$value)
	
	ivreg2 <- ivreg2[!variable=='sargan']
	
	
	ivreg2[, variable_name := sapply(variable, function(x) {strsplit(x, '[_]')[[1]][2]})]
	

	
	ivreg2[rownames=='SWFp', list(N=.N, Nsig = length(which(value<=.1)), perc = length(which(value<=.1))/.N), by = c('variable_name')]
	
	ivreg2[rownames=='APchi2p', list(N=.N, Nsig = length(which(value<=.1)), perc = length(which(value<=.1))/.N)]
	ivreg2[rownames=='APFp', list(N=.N, Nsig = length(which(value<=.1)), perc = length(which(value<=.1))/.N)]
	
	
	
	# diagnose differences in categories and countries
	vars = c('rwpsprd', 'wpswdst', 'llen','nov3')
	tmp=melt(brand_panel[, lapply(.SD, mean, na.rm=T), by = c('category','country','cat_class','country_class'), .SDcols = vars], id.vars=c('category','country','cat_class','country_class'))
	#wpspr - normalize CPI?!
	#var=vars[1]
	
	
	print(dcast(tmp[variable==var], country_class + country ~ cat_class + category, value.var='value'))
	
	for (var in vars) {
		cat(paste0('Variable: ', var, '...\n\n'))
		print(dcast(tmp[variable==var], country_class + country ~ cat_class + category, value.var='value'))
		cat('\n\n\n\n')
		}
	
	# something wrong with the price in india and new zealand?
	
	
	APchi2p
