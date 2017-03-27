
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

### Import data
	brand_panel=fread('..\\..\\derived\\output\\datasets.csv')
	brand_panel[, ':=' (date = as.Date(date))]

# Start cluster
	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	void<-clusterEvalQ(cl, init())
	void<-clusterEvalQ(cl, require(data.table))
		

# Retrieve variables for testing
	dt=melt(brand_panel[selected_t_brand==T, !colnames(brand_panel)%in%c('selected_t_brand', 'selected_t_cat', 'selected'),with=F], id.vars=c('country', 'brand', 'date', 'category', 'market_id'))
	dt=dt[market_id%in%1:10000 & variable%in%c('rwpspr', 'wpswdst', 'llen', 'nov3')]
	dt[variable=='rwpspr', value:=(value)]
	dt[variable=='llen', value:=(value)]
	dt[variable=='wpswdst', value:=(value)]
	dt[variable=='nov3', value:=(value)]
	#dt[variable=='nov3', value:=log(value+1)]
	

# Check for UR using the cluster
	split_dt = split(dt, dt$market_id)


	try_adf <- function(x) {
		out = try({adf_enders(x, maxlag=12, pval=.05,season=NULL)["ur"]}, silent=T)
		if (class(out)=='try-error') return(as.numeric(NA)) else return(out)
		}
	clusterExport(cl, 'try_adf')
			
	out = parLapplyLB(cl, split_dt, function(i) {
					i[, list(ur = try_adf(value)), by=c('market_id', 'category', 'country', 'brand', 'variable')]
					})

	urs = rbindlist(out)

	setkeyv(dt, c('market_id', 'category', 'country', 'brand', 'variable'))
	setkeyv(urs, c('market_id', 'category', 'country', 'brand', 'variable'))

	dt[, ur:=NULL]
	dt[urs, ur:=i.ur]

# Transform to first-differences if UR is found	
	dt[ur==1, transfvalue := makediff(value), by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt[ur==0, transfvalue := value, by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt <- dt[!is.na(transfvalue)]
	
# Conduct Shapiro-Wilk tests on levels and differenced series
	dt[, shapiro_p_diff := shapiro.test(transfvalue)$p, by=c('market_id', 'category', 'country', 'brand', 'variable')]
	dt[, shapiro_p_lev := shapiro.test(value)$p, by=c('market_id', 'category', 'country', 'brand', 'variable')]

	dt[, shapiro_p_lev_pooled := shapiro.test(value)$p, by=c('market_id', 'category', 'country', 'variable')]

# Summarize results
	summ <- dt[, list(shapiro_p_diff = unique(shapiro_p_diff), shapiro_p_lev = unique(shapiro_p_lev), ur=unique(ur)), by=c('market_id', 'category', 'country', 'brand', 'variable')]
	summ2 <- dt[, list(shapiro_p = unique(shapiro_p_lev_pooled)), by=c('market_id', 'category', 'country', 'variable')]

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

	