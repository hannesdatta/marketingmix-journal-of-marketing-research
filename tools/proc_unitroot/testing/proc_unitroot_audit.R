##########################
# Audit on proc_unitroot #
##########################

# Load package
	source('proc_unitroot.R')
# Load packages
	require(data.table)
# Load test data (will be compared with eviews output)
	dat <- fread('proc_unitroot_testdata.txt')
	
# Test 1: perform UR for first brand (canon)
	series1 = dat[country=='australia'&brand=='canon']$llength

	adf.equation(series1, lags = 0, maxlag = NULL, trend = F, season = NULL) # --> see table1.pdf. All good.
	adf.test(series1, maxlag = 12, trend = F, season = NULL) # --> see table2.pdf. All good.

	series2 = dat[country=='australia'&brand=='canon']$unitsales
	adf.test(series2, maxlag = 12, trend = F, season = NULL) # --> see table3.pdf. All good.
	adf.test(series2, maxlag = 12, trend = T, season = NULL) # --> see table4.pdf. All good.

	adf.test(log(series2), maxlag = 12, trend = T, season = NULL) # --> see table5.pdf. All good.
	

# Test 2: perform UR for all brands
res<-rbindlist(lapply(split(dat, dat$brand), function(x) {
	teststat = adf.test(x$unitsales, maxlag = 12, trend = T, season = NULL)
	data.frame(brand=unique(x$brand), t= teststat$t, p = teststat$p)
	}))

N_brands = nrow(res)
P = -2 * sum(log(res$p))
pchisq(P, df=2*N_brands, lower.tail=FALSE) # combine p values, see Verbeke 2004, p. 372

# --> looks great, see table6.pdf

# Test 3: perform UR for all brands on different variable
	res<-rbindlist(lapply(split(dat, dat$brand), function(x) {mn
	teststat = adf.test(x$wpsprice, maxlag = 12, trend = T, season = NULL)
	data.frame(brand=unique(x$brand), t= teststat$t, p = teststat$p)
	}))

N_brands = nrow(res)
P = -2 * sum(log(res$p))
pchisq(P, df=2*N_brands, lower.tail=FALSE)

# --> looks great, see table7.pdf

#######################
# Audit on KPSS/Hadri #
#######################

series1 = dat[country=='australia'&brand=='canon']$wpsprice

series1 = dat[country=='australia'&brand=='canon']$unitsales
series1 = dat[country=='australia'&brand=='kodak']$unitsales

series1b=series1[!is.na(series1)]
require(tseries)
kpss.test(series1)

summary(ur.kpss(series1b, type = c("tau"), lags = 'short', use.lag = NULL))

#c("short", "long", "nil"),
 #       use.lag = NULL)




#adf_enders(te, maxlag=12,season=NULL,pval=.05)

#adf.test(te, maxlag=12,season=NULL, trend=T)
#adf.test(te, maxlag=12,season=NULL, trend=F)

#adf_enders_single