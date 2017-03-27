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
require(RStata)

### Stack data in data.table
	brand_panel=fread('../temp/preclean.csv')
	brand_panel[, ':=' (date = as.Date(date))]

### Load additional packages
	require(parallel)
	#require(fUnitRoots)
	#require(marketingtools)
	
	# load marketing tools
	sapply(list.files('d:\\DATTA\\Dropbox\\Tilburg\\Projects\\marketingtools\\R\\', full.names=T), source)
	

### Setup cluster environment
	ncpu = 10
	
### Initialize all required functions (possibly on the cluster, too)

	init <- function() {
		max.lag <<- 12
		min.t <<- 36
	
		### Set STATA 14 Path
		options("RStata.StataPath"="\"C:\\Program Files (x86)\\Stata14\\StataSE-64\"")
		options("RStata.StataPath"="\"C:\\Program Files (x86)\\Stata14\\Stata14\\StataSE-64\"")
		options("RStata.StataVersion"=14)

	### Analysis script
		source('proc_analysis.R')
		#require(marketingtools)
		sapply(list.files('d:\\DATTA\\Dropbox\\Tilburg\\Projects\\marketingtools\\R\\', full.names=T), source)
	
	}
	
	#init()

brand_panel[, wpswdst := wpswdst+1]
brand_panel[, nov3 := nov3+1]

try(detach(m1), silent=T)

# Specify models
m1 <- list(setup_y=c(usalessh = 'usalessh'),
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov3'),
		   setup_endogenous=c(price = 'iv[_].*mcrwpsprd', dist = 'iv[_].*mcwpswdst', llen = 'iv[_].*mcllen', nov = 'iv[_].*mcnov3'),
		   trend='ur', # choose from: all, ur, none.
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
	analysis_markets <- unique(markets$market_id)
	
	last.item = length(analysis_markets)
	last.item = 10

# deactivate the rest
	# not on cluster
	results_brands <- NULL
	try(detach(m1), silent=T)
	
	for (m in analysis_markets[1:last.item]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = .05, max.lag = 12, 	min.t = 36), silent=T)

	}

	
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
if(0) {
# set up cluster
	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	
	void<-clusterEvalQ(cl, init())
	void<-clusterEvalQ(cl, require(data.table))
	void<-clusterEvalQ(cl, require(RStata))
	
	clusterEvalQ( cl, options("RStata.StataVersion"=14))
	clusterEvalQ( cl, options("RStata.StataPath"="\"C:\\Program Files (x86)\\Stata14\\StataSE-64\""))
	
	# cannot parallelize RStata
	
# run estimation for brand-level attraction models
	results_brands <- NULL
	assign_model(m1)
	
	clusterExport(cl,names(m1))
	
		
	Sys.time()
	res <- parLapplyLB(cl, analysis_markets[1:last.item], function(i) {
			try(analyze_by_market(i, setup_y = setup_y, setup_x = setup_x, setup_endogenous = setup_endogenous, trend = 'ur', pval = pval, max.lag = max.lag, min.t = min.t),silent=T)
			})
	Sys.time()
	
	}

# save results
	save(results_brands, markets, models, file='..\\output\\results.RData')

	}