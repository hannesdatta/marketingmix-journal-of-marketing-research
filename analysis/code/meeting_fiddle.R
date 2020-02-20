
mid = 1
bid = 1


source('proc_analysis_agg.R')
source('proc_analysis.R')
source('proc_analysis_brand.R')
source('proc_ardl.R')



out=sapply(unique(brand_panel$brand_id)[3], function(bid) try(analyze_brand(bid, quarters=T), silent=T), simplify=F)




# "retrieve" resulting model, reestimate

# simulation
dv='lnusales'
dt=data.table(brand_panel[brand_id==1])
vars = c('lnrwpspr','lnllen','lnwpswdst')
m<-ardl(type='ardl-ec', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1)
summary(m)

m<-ardl(type='ardl-firstdiff', dt = dt, dv = dv, vars = c(vars, quarter_vars), exclude_cointegration = NULL,
        adf_tests= NULL, maxlag = 6, pval = .1)

m$tested_model_specs$formula



mx<-dynardl(lnusales~1+lnrwpspr + lnllen + lnwpswdst + quarter1+quarter2+quarter3,
            data = dt, lags = list(lnusales=0, lnrwpspr=1, lnllen=1, lnwpswdst=1),
            diffs = c('lnrwpspr', 'lnllen', 'lnwpswdst'),
            lagdiffs = list(lnusales=1),
            levels=c('lnrwpspr', 'lnllen', 'lnwpswdst', 'quarter1', 'quarter2', 'quarter3'),
            ec=T)

#shockvariable = vars[1]
shockvalue = log(1.1)

msim = dynardl(m$tested_model_specs$formula, data = dt, 
               lags = m$tested_model_specs$lagstructure[[m$mchoice]]$lags,
               diffs = m$tested_model_specs$diffs, 
               lagdiffs = m$tested_model_specs$lagstructure[[m$mchoice]]$lagdiff,
               levels= m$tested_model_specs$levels, 
               ec = m$tested_model_specs$ec, trend = m$tested_model_specs$trend,
               simulate = T, shockvar='lnrwpspr', range=48, time = 10, shockval=shockvalue, burnin=12, sig=90, fullsims = T)

dynardl.simulation.plot(msim, type='area', response='levels') #levels') #diffs

dynardl.simulation.plot(msim, type='area', response='diffs') #levels') #diffs

dynardl.simulation.plot(msim, type='area', response='shock.effect.decay') #levels') #diffs

# this is the right one!

dynardl.simulation.plot(msim, type='area', response='cumulative.diffs') #levels') #diffs


#ardl-firstdiff
#ardl-levels

