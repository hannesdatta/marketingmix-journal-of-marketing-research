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

## Obs per market
	tmp=brand_panel[, list(obs=length(unique(date[!is.na(nov12)]))),by=c('market_id','category','country')]
	setorder(tmp, obs)
	
# For which markets do we only see 1 brand?
  tmp=brand_panel[, list(sumsales=sum(usales), nbrand=length(unique(brand))),by=c('market_id','category','country','date')]
	setorder(tmp, market_id,date)
	
	tmp[, above:=as.numeric(1:.N%in%first(which(nbrand>1))), by=c('market_id')]
	tmp[, below:=as.numeric(1:.N%in%(1+last(which(nbrand>1))))*(-1), by=c('market_id')]
	tmp[, obs:=cumsum(above+below),by=c('market_id')]
	
	dir.create('../audit')
	dir.create('../audit/1-brand-markets')

	for (i in unique(tmp$market_id)) {
	  fn=paste0(unique(tmp[market_id==i]$category), ' - ', unique(tmp[market_id==i]$country), ' (', i, ')', ifelse(any(tmp[market_id==i]$nbrand==1),' - affected',''))
	  
	  png(paste0('../audit/1-brand-markets/', fn,'.png'), res=200, units='in', height=8, width=16)
	  
	  with(tmp[market_id==i], plot(x=date, y=sumsales, type='l', main = fn, xlab='date',ylab='sum of sales'))
	  with(tmp[market_id==i], abline(v=date[which(above==1)],col='red'))
	  with(tmp[market_id==i], abline(v=date[which(below==c(-1))],col='red'))
	  
	  dev.off()
	  
	}
	

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
	ncpu = 11 #4
	analysis_markets#=analysis_markets[1:2]
	
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
		   setup_x=c(price = 'rwpspr', dist = 'wpswdst', llen = 'llen', nov = 'nov6sh'),
		   setup_endogenous = c('rwpspr', 'wpswdst','llen','nov6sh'),
		   plusx = c('nov6sh', 'wpswdst', 'llen'),
		   trend='always', # choose from: always, ur, none.
		   pval = .1, max.lag = 12, min.t = 36, descr = 'model',
		   fn = 'model', benchmarkb = NULL, estmethod = "FGLS",
		   attraction_model = "MCI", takediff = 'none', # choose from flexible (UR approach), none, alwaysdiff
		   use_quarters = T, lag_heterog = F,
		   squared=F, maxiter = 400,
		   carryover_zero=F)

	#m1d <- m1
	#m1d$setup_x['nov'] <- 'nov3sh'
	#m1d$setup_endogenous[4] <- 'nov3sh'
	#m1d$plusx[1] <- 'nov3sh'
	
	m1e <- m1
	m1e$setup_x['nov'] <- 'nov12sh'
	m1e$setup_endogenous[4] <- 'nov12sh'
	m1e$plusx[1] <- 'nov12sh'
	
	
	# without trend, with lagged Xs
	m2 <- m1
	m2$plusx=c('nov6sh', 'wpswdst', 'lagnov6sh', 'lagwpswdst')
	m2$setup_x=c("rwpspr", "wpswdst", "llen", "nov6sh", "lagrwpspr", "lagwpswdst", "lagllen", "lagnov6sh")
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
		                                             lag_heterog=lag_heterog, carryover_zero=carryover_zero), silent=T)
		
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
	
# Estimate without lagged Xs, but with trend
	assign_model(m1)
	clusterExport(cl,names(m1))
	
	results_MCI_wolag_trend <- parLapplyLB(cl, analysis_markets, function(i) {
	  for (carry in c(F, T)) {
	  tmp=try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                        setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                        max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                        use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                        squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=carry), silent=T)
	  
	  if (!class(tmp)=='try-error') {
	    coef=data.table(tmp$model@coefficients)[grepl('lagunitsales', variable)]$coef
	    if (length(coef)==0) break
	    if (coef<0) print('carryover prob') else break
	    } else break
	  }
	  return(tmp)
	})
	  
	savemodels(fname)
	
	# Estimate with lagged Xs, without trend
	assign_model(m1, del = TRUE)

	
	# Nov12sh
	assign_model(m1e)
	clusterExport(cl,names(m1e))
	
	results_nov12sh <- parLapplyLB(cl, analysis_markets, function(i) {
	    for (carry in c(F, T)) {
	      tmp=try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = setup_x, 
	                                setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                                max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                                use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                                squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=carry), silent=T)
	      
	      if (!class(tmp)=='try-error') {
	        coef=data.table(tmp$model@coefficients)[grepl('lagunitsales', variable)]$coef
	        if (length(coef)==0) break
	        if (coef<0) print('carryover prob') else break
	      } else break
	    }
	    return(tmp)
	  })
	
	
	
	savemodels(fname)
	
	assign_model(m1e, del = TRUE)
	
	
	
####
	
	if(0){
	assign_model(m2)
	clusterExport(cl,names(m2))
	
	results_MCI_wlag_wotrend <- parLapplyLB(cl, analysis_markets, function(i) {
	  # note: TV1 in australia can only be estimated without the lagged vars for novelty and llen
	  try(analyze_by_market(i, estmethod=estmethod, setup_y = setup_y, setup_x = if(!i==138) setup_x else setup_x[c(1:6)],
	                        setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                        max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                        use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                        squared=squared, takediff=takediff, lag_heterog=lag_heterog, carryover_zero=carryover_zero), silent=T)
	})
	
	if(0){
	# inspecting the carry-overs yields a negative one in NZ; set to zero
	if(results_MCI_wlag_wotrend[[27]]$market_id==31 & data.table(results_MCI_wlag_wotrend[[27]]$model@coefficients)[grepl('lagunitsales',variable)]$coef<0) {
	 
	results_MCI_wlag_wotrend[[27]] <- try(analyze_by_market(31, estmethod=estmethod, setup_y = setup_y, setup_x = if(!31==138) setup_x else setup_x[c(1:6)],
	                                                         setup_endogenous = setup_endogenous, trend = trend, pval = pval, 
	                                                         max.lag = max.lag, min.t = min.t, maxiter = maxiter, 
	                                                         use_quarters=use_quarters, plusx=plusx, attraction_model=attraction_model, 
	                                                         squared=squared, takediff=takediff, lag_heterog=lag_heterog,carryover_zero=T), silent=T)
		}
	}
	
	savemodels(fname)
  }
	
	
}
