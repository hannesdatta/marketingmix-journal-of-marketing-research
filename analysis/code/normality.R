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

# Purpose: Assess required non-normality of marketing mix instruments for the application of Gaussian Copula correction terms


### LOAD DATA SETS
require(data.table)

### Load packages
	require(parallel)
	require(fUnitRoots)
	require(marketingtools)
	
### Setup cluster environment
	ncpu = 10

# Initialize cluster
	init <- function() {
		max.lag <<- 12
		min.t <<- 36

	### Analysis script
		source('proc_analysis.R')
		require(marketingtools)

	}

	init()
	
### Import data
	brand_panel=fread('..\\temp\\preclean.csv')
	brand_panel[, ':=' (date = as.Date(date))]
	
# Start cluster
	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	void<-clusterEvalQ(cl, init())
	void<-clusterEvalQ(cl, require(data.table))
		
# Retrieve variables for testing
	dt=melt(brand_panel[selected_t_brand==T, !colnames(brand_panel)%in%c('selected_t_brand', 'selected_t_cat', 'selected'),with=F], id.vars=c('country', 'brand', 'date', 'category', 'market_id'))
	
	# retain only necessary variables
	dt=dt[variable%in%c('rwpspr', 'wpswdst', 'llen', 'nov3', 'wpsun')]
	
	# apply (log) transformations
	dt[variable=='rwpspr', lvalue:=log(value)]
	dt[variable=='wpswdst', lvalue:=log(value+1)]
	dt[variable=='llen', lvalue:=log(value)]
	dt[variable=='nov3', lvalue:=log(value+1)]
	dt[variable=='wpsun', lvalue:=log(value+1)]
	
# Verify whether each series is to be used; if not, kick out
	dt[, ts_selected := use_ts(value), by=c('country','category', 'brand', 'variable')]
	
	dt <- dt[ts_selected==T]
	dt[,ts_selected:=NULL]

# Check for UR using the cluster
	split_dt = split(dt, dt$market_id)

	cluster_adf <- function(x) {
		out = try({adf_enders(x, maxlag=12, pval=.05,season=NULL)["ur"]}, silent=T)
		if (class(out)=='try-error') return(as.numeric(NA)) else return(out)
		}
	clusterExport(cl, 'cluster_adf')
			
	out = parLapplyLB(cl, split_dt, function(i) {
					i[, list(ur_nonlog = cluster_adf(value), ur_log = cluster_adf(lvalue)), by=c('market_id', 'category', 'country', 'brand', 'variable')]
					})

	urs = rbindlist(out)

	setkeyv(dt, c('market_id', 'category', 'country', 'brand', 'variable'))
	setkeyv(urs, c('market_id', 'category', 'country', 'brand', 'variable'))

	dt[, ':=' (ur_nonlog=NULL, ur_log=NULL)]
	dt[urs, ':=' (ur_nonlog=i.ur_nonlog, ur_log = i.ur_log)]

# Transform to first-differences if UR is found	
	dt[ur_nonlog==1, transfvalue_nonlog := makediff(value), by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt[ur_nonlog==0, transfvalue_nonlog := value, by=c('market_id', 'category', 'country', 'brand', 'variable')]
	
	dt[ur_log==1, transfvalue_log := makediff(lvalue), by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt[ur_log==0, transfvalue_log := lvalue, by=c('market_id', 'category', 'country', 'brand', 'variable')]
	
	dt <- dt[!is.na(transfvalue)]
	
# Conduct Shapiro-Wilk tests on levels and differenced series
	# non-logs
	dt[!is.na(transfvalue_nonlog), shapiro_p_diff_nonlog := shapiro.test(transfvalue_nonlog)$p, by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt[!is.na(value), shapiro_p_lev_nonlog := shapiro.test(value)$p, by=c('market_id', 'category', 'country', 'brand', 'variable')]

	# logs
	dt[!is.na(transfvalue_log), shapiro_p_diff_log := shapiro.test(transfvalue_log)$p, by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt[!is.na(value), shapiro_p_lev_log := shapiro.test(lvalue)$p, by=c('market_id', 'category', 'country', 'brand', 'variable')]

# Summarize results
	summ <- dt[, lapply(.SD, function(x) unique(x[!is.na(x)])), by = c('market_id', 'category', 'country', 'brand', 'variable'), .SDcols=c('ur_nonlog', 'ur_log', grep('shapiro_p', colnames(dt), value=T))]
	setnames(summ, 'variable', 'regressor')
	summ <- melt(summ, id.vars=c('market_id','category','country','brand','regressor','ur_nonlog', 'ur_log'))
	
# Summarize test outcomes
	# non-logs
	summ[grepl('_nonlog', variable), list(length(which(value<.1))/.N), by = c('ur_nonlog', 'variable')]
	
	
# Number of true (= non-normal) test outcomes
	table(summ$shapiro_p_lev<.10)
	table(summ$shapiro_p_diff<.10)
	table(summ2$shapiro_p<.10)

# Share of non-normally distributed variables
	length(which(summ$shapiro_p_lev<.10))/length(summ$shapiro_p_lev)
	length(which(summ$shapiro_p_diff<.10))/length(summ$shapiro_p_diff)

# Test outcomes by variable and unit root outcome
	resdt = summ[, list(length(which(shapiro_p_diff<.10))/.N),  by = c('ur', 'variable')]
	dcast(resdt, variable ~  ur)

	resdt = summ[, list(length(which(shapiro_p_lev<.10))/.N),  by = c('ur', 'variable')]
	dcast(resdt, variable ~  ur)
	
# How many brands
	nrow(dt[, list(.N), by = c('brand', 'market_id' )])

	