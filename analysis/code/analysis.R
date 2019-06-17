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


#if (1==1) quit()

### LOAD DATA SETS
library(data.table)
require(bit64)
library(parallel)

dir.create('../output')

## Load panel data
	brand_panel=fread('../temp/preclean.csv')
	brand_panel[, ':=' (date = as.Date(date))]

	# preprocess brand_panel attr
	for (var in grep('^attr', colnames(brand_panel),value=T)) {
	if (min(brand_panel[, var,with=F],na.rm=T)==0&max(brand_panel[, var,with=F],na.rm=T)<=1) {
	  brand_panel[, (paste0(var)):=get(var)*100+1]
	}  
	}
	
	# preprocess brand_panel attr
	for (var in grep('^attr', colnames(brand_panel),value=T)) {
	  if (min(brand_panel[, var,with=F],na.rm=T)==0&max(brand_panel[, var,with=F],na.rm=T)>1) {
	    brand_panel[, (paste0(var)):=get(var)+.001]
	  }  
	}
	
	
## Obs per market
	tmp=brand_panel[, list(obs=length(unique(date[!is.na(nov12)]))),by=c('market_id','category','country')]
	setorder(tmp, obs)

## How many brands are active in multiple countries
	tmp = brand_panel[, list(N=.N), by = c('brand', 'country')]
	tmp[, Ncountries := .N, by = c('brand')]
	tmp[, N:=NULL]
	tmp[, present := T]
	tmp = dcast.data.table(tmp, brand + Ncountries ~ country, value.var='present')
	
	tmp[, Ncountries:= -Ncountries]
	setorder(tmp, Ncountries, brand)
	tmp[, Ncountries:=-Ncountries]
	write.table(tmp, 'clipboard', row.names=F)
	

# define markets to run analysis on 
	markets <- brand_panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
  length(analysis_markets)
  
### Run specs
	run_manual=TRUE
	run_cluster=TRUE
	ncpu = 11 
	
### Function to initialize all required functions (on a cluster)
	init <- function() {
		source('proc_analysis.R')
		library(marketingtools)
		}
	
	# load model code
	init()

	fname = '../output/results.RData'

######################
### Specify models ###
######################

# delete models, should they exist already in the workspace
	try(detach(m1), silent=T)
	try(assign_model(m1,del=T), silent=T)
	
# define model: with trend, without lagged Xs
	m1 <- list(setup_y=c(usalessh = 'usalessh'),
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov12sh'),
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov12sh'),
		   plusx = c('nov12sh', 'wpswdst', 'llen'),
		   trend='always', # choose from: always, ur, none.
		   pval = .1, max.lag = 12, min.t = 36, descr = 'model',
		   fn = 'model', benchmarkb = NULL, estmethod = "FGLS",
		   attraction_model = "MCI", takediff = 'none', # choose from flexible (UR approach), none, alwaysdiff
		   use_quarters = T, lag_heterog = F,
		   squared=F, maxiter = 400,
		   carryover_zero=F, use_attributes = T)

	m1_adv <- m1
	m1_adv$setup_x <- c(m1_adv$setup_x, adv='adv')
	m1_adv$setup_endogenous <- c(m1_adv$setup_endogenous, 'adv')
	m1_adv$plusx <- c(m1_adv$plusx, 'adv')
	
	# without trend, with lagged Xs
	m2 <- m1
	m2$plusx=c('nov12sh', 'wpswdst', 'lagnov12sh', 'lagwpswdst')
	m2$setup_x=c("rwpspr", "wpswdst", "llen", "nov12sh", "lagrwpspr", "lagwpswdst", "lagllen", "lagnov12sh")
	m2$trend = 'none'

####################
### RUN ANALYSIS ###
####################

if(run_manual==T) {	
	# for prototype, limit the number of items
	last.item = 2

	# not on cluster
	results_brands <- NULL

	for (m in analysis_markets[seq(from=1, to=min(last.item,length(analysis_markets)))]) {
		assign_model(m1)
			
		results_brands[[m]] <- try(analyze_by_market(m, setup_y = setup_y, setup_x = setup_x, 
		                                             setup_endogenous = setup_endogenous, 
		                                             trend = trend, pval = pval, max.lag = max.lag, 
		                                             min.t = min.t, maxiter=maxiter, 
		                                             attraction_model = attraction_model, 
		                                             plusx=plusx, use_quarters=use_quarters, 
		                                             squared=squared, takediff=takediff, 
		                                             lag_heterog=lag_heterog, carryover_zero=carryover_zero,
		                                             use_attributes = use_attributes), silent=T)
		
		assign_model(m1, del = TRUE)
		
	}
	assign_model(m1, del = TRUE)
	
	#assign_model(m1)
	#assign_model(m1,del=T)
	
	savemodels(fname)
}

######################
# Cluster estimation #
######################

if(run_cluster==T) {
	
# set up cluster
	cl<-makePSOCKcluster(ncpu)
	clusterExport(cl,c('brand_panel', 'init'))
	void<-clusterEvalQ(cl, require(data.table))
	
# run estimation for brand-level attraction models
	void<-clusterEvalQ(cl, init())
	init()
	
  ######################################################
	# Main model: with trend, without lagged Xs, nov12sh #
	######################################################
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	#err=which(unlist(lapply(results_nov12sh,class))=='try-error')
	#tmp=unique(brand_panel[market_id%in%analysis_markets[err]],by=c('market_id'))[, c('category','country','market_id'),with=F]
	
	results_main <- parLapplyLB(cl, analysis_markets, function(i) {
	    for (carry in c(F, T)) {
	      tmp=try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                                setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                                max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                                use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                                squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=carry,
	                                use_attributes=use_attributes), silent=T)
	      
	      if (!class(tmp)=='try-error') {
	        coef=data.table(tmp$model@coefficients)[grepl('lagunitsales', variable)]$coef
	        if (length(coef)==0) break
	        if (coef<0) print('carryover prob') else break
	      } else break
	    }
	    return(tmp)
	  })
	
	savemodels(fname)
	assign_model(m1, del=TRUE)
	
	######################################
	# Robustness w/ advertising spending #
	######################################
	
	assign_model(m1_adv)
	clusterExport(cl,names(m1_adv))
	
	china_hk_selection = brand_panel[country%in%c('china','hong kong')&
	                                 any(!is.na(adv))&date<ifelse(country=='china', '2012-09-01', '2014-12-01'), 
	                                 list(obs= length(unique(date))),by=c('category','country', 'market_id')]
	
	china_hk_selection = china_hk_selection[, selected:=obs>ifelse(category=='tablet',4*12,5*12)]
  china_hk = unique(china_hk_selection[selected==T]$market_id)
  
	results_adv <- parLapplyLB(cl, china_hk, function(i) {
	  for (carry in c(F, T)) {
	    tmp=try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                              setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                              max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                              use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                              squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=carry,
	                              use_attributes=use_attributes), silent=T)
	    
	    if (!class(tmp)=='try-error') {
	      coef=data.table(tmp$model@coefficients)[grepl('lagunitsales', variable)]$coef
	      if (length(coef)==0) break
	      if (coef<0) print('carryover prob') else break
	    } else break
	  }
	  return(tmp)
	})
	
	savemodels(fname)
	assign_model(m1_adv, del=TRUE)
	
	#################################################
	# Robustness: with lagged Xs, but without trend #
	#################################################
	
	assign_model(m2)
	clusterExport(cl,names(m2))
	
	results_laggedX <- parLapplyLB(cl, analysis_markets, function(i) {
	  for (carry in c(F, T)) {
	    tmp=try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                              setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                              max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                              use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                              squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=carry,
	                              use_attributes=use_attributes), silent=T)
	    
	    if (!class(tmp)=='try-error') {
	      coef=data.table(tmp$model@coefficients)[grepl('lagunitsales', variable)]$coef
	      if (length(coef)==0) break
	      if (coef<0) print('carryover prob') else break
	    } else break
	  }
	  return(tmp)
	})
	
	savemodels(fname)
	assign_model(m2, del=TRUE)
	
}