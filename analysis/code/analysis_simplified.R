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
rm(list = ls())

### LOAD DATA SETS

library(parallel)
library(devtools)

init <- function() {
  library(data.table)
  library(bit64)
  library(timeSeries)
  library(marketingtools)
  library(car)
  #source('proc_analysis_marketshare.R')
  #source('proc_analysis_ardl.R')
  #source('proc_analysis_ardlbounds.R')
  #source('proc_analysis_sales.R')
  source('proc_auxilary.R')
  source('c1c5b3af32343d042fcbc8e249ae9ff6/proc_unitroots.R')
  source('proc_simplified.R')
}

init()


#library(dynamac)
#library(tseries)


dir.create('../output')

## Load panel data
	brand_panel=fread('../temp/preclean_main.csv')
	brand_panel[, ':=' (date = as.Date(date))]
  
	
	brand_panel_robust=fread('../temp/preclean_8years.csv')
	brand_panel_robust[, ':=' (date = as.Date(date))]

# define markets to run analysis on 
	markets <- brand_panel[, list(n_brands = length(unique(brand)),
	                              n_obs = .N,
								  n_dates = length(unique(date))), by=c('market_id','country', 'category')]

	setorder(markets,market_id)
	analysis_markets <- unique(markets$market_id)
  length(analysis_markets)
 
  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  
  # total u sales
  alls=sum(brand_panel$usales,na.rm=T)
  cov=sum(brand_panel[selected==T&timewindow==T&obs48]$usales,na.rm=T)
  cov/alls # --> report in paper
  
  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
  brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand')][!grepl('allothers',brand)] # -> uniq brands
  
  
# Define additional variables
  
  brand_panel_raw <- copy(brand_panel)
  
  bp <- lapply(list(brand_panel, brand_panel_robust), function(brand_panel) { 
    setorder(brand_panel, market_id, brand, date)
    brand_panel[selected==T&timewindow==T, trend:=as.double(.GRP),by=c('market_id', 'brand','date')]
    brand_panel[selected==T&timewindow==T, trend:=trend-min(trend,na.rm=T)+1,by=c('market_id', 'brand')]
    brand_panel[selected==T&timewindow==T, lntrend:=log(trend),by=c('market_id', 'brand')]
    
  for (q in 1:3) {  
    brand_panel[, paste0('quarter', q):=0]
    brand_panel[quarter==q, paste0('quarter', q):=1]
  }  
    
  vars=c('rwpspr', 'llen', 'wpswdst', 'usales', 'lagusales', 'radv')#, 'adv')#, grep('comp[_]', colnames(brand_panel),value=T))
  for (var in vars) {
    brand_panel[, anyzero:=as.numeric(any(get(var)==0),na.rm=T),by=c('market_id', 'brand')]
    brand_panel[is.na(anyzero), anyzero:=0]
    brand_panel[, paste0('ln', var):=log(get(var)+anyzero), by = c('market_id', 'brand')]
    brand_panel[, paste0('dln', var):= get(paste0('ln', var))-c(NA, get(paste0('ln', var))[-.N]), by = c('market_id', 'brand')]
  }
  
  # competitive mmix
  for (v in c('lnrwpspr', 'lnllen', 'lnwpswdst', 'lnradv')) {#}, 'lnadv')) {
    brand_panel[, paste0('sum_', v):=sum(get(v),na.rm=T), by = c('market_id', 'date')]
    brand_panel[, paste0('N_', v):=length(which(!is.na(get(v)))), by = c('market_id', 'date')]
    brand_panel[, paste0('comp_', v):=(get(paste0('sum_', v))-get(v))/(get(paste0('N_', v))-1)]
    brand_panel[, paste0('sum_', v):=NULL]
    brand_panel[, paste0('N_', v):=NULL]
    brand_panel[, paste0('dcomp_',v):=get(paste0('comp_', v))-c(NA, get(paste0('comp_', v))[-.N]), by = c('market_id', 'brand')]
    }
  
  # run shapiro-wilk tests to assess non-normality of untransformed inputs to the copula function
  if(0){
    vars=c('lnrwpspr', 'lnllen', 'lnwpswdst')
    source('proc_auxilary.R')
    
    norm_test = function(x, type = 'shapiro') {
     use_ts_eval = use_ts(x)
     if (use_ts_eval==T) {
       x=x[!is.na(x)]
       if (type=='shapiro')        return(shapiro.test(x)$p)
       if (type=='jarquebera')  return(jarque.bera.test(x)$p.value)
     }
       
     return(as.double(NA))
    }
    
    cop=lapply(vars, function(var) {
      out=brand_panel[selected== T, list(shap_pval_levels = norm_test(get(var), type = 'shapiro'),
                             shap_pval_diffs = norm_test(dshift(get(var)), type='shapiro'),
                             jb_pval_levels = norm_test(get(var), type = 'jarquebera'),
                             jb_pval_diffs = norm_test(dshift(get(var)), type='jarquebera')), by = c('category', 'country', 'market_id', 'brand_id','brand')]
      out[, variable:=var]
      return(out)
    })
    
    cop=rbindlist(cop)
    
    cop[!is.na(shap_pval_diffs), list(N=.N, 
               shap_nonnormal_share_lev=length(which(shap_pval_levels<.1))/.N,
               shap_nonnormal_share_diff=length(which(shap_pval_diffs<.1))/.N,
               jb_nonnormal_share_lev=length(which(jb_pval_levels<.1))/.N,
               jb_nonnormal_share_diff=length(which(jb_pval_diffs<.1))/.N), by = c('variable')]
    
    cop[!is.na(shap_pval_diffs), list(N=.N, 
                                      shap_nonnormal_share_lev=length(which(shap_pval_levels<.1))/.N,
                                      shap_nonnormal_share_diff=length(which(shap_pval_diffs<.1))/.N,
                                      jb_nonnormal_share_lev=length(which(jb_pval_levels<.1))/.N,
                                      jb_nonnormal_share_diff=length(which(jb_pval_diffs<.1))/.N)]
    
  }
  
  
  length(unique(brand_panel[selected==T]$brand_id))
  length(unique(brand_panel[selected==T & timewindow==T &obs48==T]$brand_id))
  nrow(brand_panel[selected==T])
  nrow(brand_panel[selected==T & timewindow==T &obs48==T])
  
  brand_panel[, list(.N),by=c('category','country','brand')] # [!grepl('alloth',brand)]
  brand_panel[, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
  brand_panel[selected==T & timewindow == T & obs48 == T, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
  # we lose:
  brand_panel[selected==T & timewindow == T & obs48 == F, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
  brand_panel[selected==T & timewindow == T & obs48 == F, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)][, list(.N, avgN=mean(N)),by=c('category')]
  
  rem_obs=nrow(brand_panel[selected==T & timewindow == F & obs48 == T])
  keep_obs=nrow(brand_panel[selected==T & timewindow == T & obs48 == T])
  
 # (rem_obs)/keep_obs
  
  
  
#  , list(.N),by=c('category','country','brand')][!grepl('alloth',brand)][, list(.N, avgN=mean(N)),by=c('category')]
  
  
  brand_panel <- brand_panel[selected==T & timewindow==T &obs48==T] # at least 48 obs for estimation
  
  length(unique(brand_panel[selected==T]$brand_id))
  
  brand_panel[, list(.N),by=c('brand','category','country')][!grepl('allother',brand)]
  brand_panel[, list(.N),by=c('brand','category','country')][!grepl('allother',brand)][, list(.N),by=c('brand')] # --> uniq brand
  
  
  # Define copula terms
  for (var in c('lnrwpspr', 'lnllen', 'lnwpswdst', 'lnradv')) {
    brand_panel[, paste0('cop_', var):=make_copula(get(paste0(var))), by = c('market_id','brand')]
    brand_panel[, paste0('cop_d.1.', var):=make_copula(dshift(get(paste0(var)))), by = c('market_id','brand')]
  }
  
  #brand_panel[, lngdp := log(gdppercapita)]
  brand_panel[, lnholiday := log(npublicholidays+1)]
  
  brand_panel <- brand_panel[!grepl('alloth',brand, ignore.case=T)]
  return(brand_panel)
  })

  brand_panel<-bp[[1]]
  brand_panel_robust<-bp[[2]]
  
  
  brand_panel[, noadv:=all(adv==0),by=c('category','country','brand')]
  
 # brand_panel[noadv==T, wsadv:=NA]
  brand_panel[noadv==F,list(.N),by=c('category','country')]
   
  list1= brand_panel[, list(.N),by=c('category','country', 'market_id')]
  list2=brand_panel_raw[, list(.N),by=c('category','country', 'market_id')]
  list2[!market_id%in%list1$market_id]
  # we're losing another two tablet categories


##########################
### CLUSTER ESTIMATION ###
##########################

# try whether cluster exists
ncpu=7
try(stopCluster(cl),silent=T)

cl<-makePSOCKcluster(ncpu)
clusterExport(cl,c('brand_panel', 'init'))
void<-clusterEvalQ(cl, init())



bids <- unique(brand_panel$brand_id)#[1:50]
length(bids)

init()


# Test
if(0){
out =  try(simple_ec(1), silent=T)

with(out$predictions_kfold, cor(lnusales_hat, lnusales, use='pairw')^2)
with(out$predictions_kfold, cor(dlnusales_hat, dlnusales, use='pairw')^2)


with(out$predictions, cor(lnusales_hat, lnusales, use='pairw')^2)


summary(rbindlist(lapply(results_ec_main, function(x) x$predictions_kfold[, list(cor(lnusales, lnusales_hat,use='pair')^2)]))$V1)

summary(rbindlist(lapply(results_ec_main, function(x) x$predictions_kfold[, list(cor(dlnusales, dlnusales_hat,use='pair')^2)]))$V1)
}

####### MAIN MODEL ######

results_ec_main = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid), silent=T)
)

if(0) {
  r2=rbindlist(lapply(results_ec_main, function(x) x$predictions_kfold))
  tmp=r2[kfold%in%8:10, list(r2=cor(lnusales, lnlagusales+dlnusales_hat,use='pair')^2,
                rmse=sqrt(mean((lnusales-lnlagusales-dlnusales_hat)^2))), by = c('category','country','brand','kfold')]
  tmp=tmp[, list(r2=mean(r2),
                 rmse=mean(rmse)),by = c('category','country','brand')]
 
  summary(tmp$r2)
  summary(tmp$rmse)
  
  hist(tmp$r2)
  hist(tmp$rmse)
  
  
   r2=rbindlist(lapply(results_ec_main, function(x) x$predictions_kfold))
   tmp=r2[, list(cor(dlnusales, dlnusales_hat,use='pair')^2), by = c('category','country','brand','kfold')]
   tmp=tmp[, list(V1=mean(V1)),by = c('category','country','brand')]
   summary(tmp$V1)
   
  
   out=try(simple_ec(1), silent=T)
  
   newpred = data.table(dhat=predict(out$model))
   nrow(newpred)
   nrow(out$model_matrix)
   
   cor(newpred$dhat, out$model_matrix$dlnusales, use='pair')^2
   
   # within in differences
   summary(unlist(lapply(results_ec_main, function(x) cor(predict(x$model), x$model_matrix$dlnusales, use='pair')^2)))
   summary(unlist(lapply(results_ec_main, function(x) cor(x$model_matrix$lnlagusales+predict(x$model), x$model_matrix$lnlagusales+x$model_matrix$dlnusales, use='pair')^2)))
   
   
   summary(unlist(lapply(results_ec_main, function(x) cor(predict(x$model), x$model_matrix$dlnusales, use='pair')^2)))
   
   tmp=r2[kfold%in%9:10, list(r2d=cor(dlnusales, dlnusales_hat,use='pair')^2,
                 r2l=cor(lnlagusales+dlnusales, dlnusales_hat+lnlagusales)^2), by = c('category','country','brand','kfold')]
   tmp=tmp[, list(r2d=mean(r2d),
                  r2l=mean(r2l)),by = c('category','country','brand')]
   summary(tmp)
   
   # old holdout
   
   
   
 #  summary(unlist(lapply(results_ec_main, function(x) cor(predict(x$model), x$model_matrix$dlnusales, use='pair')^2)))
   
 
 
}


results_ec_main_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, holdout=holdout), silent=T)
)

#
if(0) {
 # Exploration
  
  coefs_main=rbindlist(lapply(results_ec_main, function(x) data.table(x$paneldimension[,-c('date'),with=F][1], 
                                                       variable=names(x$model$coefficients), coef= x$model$coefficients)))[, type:='alldata']
  
  coefs_holdout=rbindlist(lapply(results_ec_main_holdout, function(x) data.table(x$paneldimension[,-c('date'),with=F][1], 
                                                                                 variable=names(x$model$coefficients), coef= x$model$coefficients)))[, type:='testonly']
  
  
  tmp1=dcast(rbind(coefs_main, coefs_holdout), category+country+brand+brand_id~variable+type,value.var='coef')
  tmp2=dcast(rbind(coefs_main, coefs_holdout), category+country+brand+brand_id~type+variable,value.var='coef')
  
  # means for parameters
  data=rbindlist(lapply(results_ec_main, function(x) {

  dt = cbind(x$paneldimension, x$model_matrix)
  dt[, holdout:=ifelse((1:.N)/.N>=.8, 'holdout','training')]
  return(dt)
  }),fill=T)
  
  tmp3 = data[, lapply(.SD, mean, na.rm=T), by = c('market_id','category','country','brand','brand_id', 'holdout')]
  tmp3[, date:=NULL]
  tmp3x = melt(tmp3, id.vars=c('category','country','brand','brand_id','holdout','market_id'))
  tmp3x[is.na(value), value:=NA]
  tmp4=dcast(tmp3x, category+country+brand+brand_id~variable+holdout,value.var='value')
  tmp5=dcast(tmp3x, category+country+brand+brand_id~holdout+variable,value.var='value')
  
  library(xlsx)
  
  tmp1x=data.table(tmp1)[, lapply(.SD, mean,na.rm=T),.SDcols=grep('^brand|category|country', colnames(tmp1), value=T, invert=T)]
  tmp4x=data.table(tmp4)[, lapply(.SD, mean,na.rm=T),.SDcols=grep('^brand|category|country', colnames(tmp4), value=T, invert=T)]
  
  
  melt_coef = melt(coefs_holdout, id.vars=c('market_id','brand','category','country','brand_id','type', 'variable'))
  melt_coef[, type2:='coefficient']
  tmp3x[, type2:=ifelse(holdout=='training','data-training','data-validiation')]
  
  
  tmp5 = rbindlist(list(melt_coef, tmp3x),fill=T)
  
  table(tmp5$variable)
  
  tmp6 <- dcast(tmp5, category+country+brand~variable+type2)
  #colMeans(tmp6)
  
  tmp7 = tmp5[, mean(value, na.rm=T), by = c('variable','type2')]
  tmp8 <- dcast(tmp7, variable~type2, value.var='V1')
  
  barplot(tmp8$`data-training`)
  library(ggplot2)
  
  ggplot(tmp7[grepl('data',type2)], aes(fill=type2, y=V1, x=variable)) + 
    geom_bar(position="dodge", stat="identity")+coord_flip()
  
  
  colsel=grep('brand|category|country', colnames(tmp6), value=T, invert=T)
  tmp7 = data.table(tmp6)[, lapply(.SD, mean,na.rm=T), .SDcols=colsel]
 # tmp8 = cbind(colnames(tmp7), tmp7)
  
  
  
  write.xlsx(tmp1, 'holdout.xlsx', sheetName="parameters_by_variable", 
             col.names=TRUE, row.names=TRUE, append=FALSE)
  write.xlsx(tmp2, 'holdout.xlsx', sheetName="parameters_by_sample", 
             col.names=TRUE, row.names=TRUE, append=TRUE)
  write.xlsx(tmp4, 'holdout.xlsx', sheetName="data_means_by_variable", 
             col.names=TRUE, row.names=TRUE, append=TRUE)
  write.xlsx(tmp5, 'holdout.xlsx', sheetName="data_means_by_sample", 
             col.names=TRUE, row.names=TRUE, append=TRUE)
  
  
}

if(0){

holdout = .10
clusterExport(cl, 'holdout')


results_ec_main_holdout10 = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, holdout=holdout), silent=T)
)

holdout = .25
clusterExport(cl, 'holdout')


results_ec_main_holdout25 = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, holdout=holdout), silent=T)
)

# Compare predictions
#summary(rbindlist(lapply(results_ec_main_holdout10, function(x) x$predictions[estim_set==F][, list(cor(lnusales, lnusales_hat,use='pair')^2)]))$V1)
#summary(rbindlist(lapply(results_ec_main_holdout, function(x) x$predictions[estim_set==F][, list(cor(lnusales, lnusales_hat,use='pair')^2)]))$V1)
#summary(rbindlist(lapply(results_ec_main_holdout25, function(x) x$predictions[estim_set==F][, list(cor(lnusales, lnusales_hat,use='pair')^2)]))$V1)

#
#lapply(list(results_ec_main_holdout, results_ec_main_holdout10, results_ec_main))
}



## LOG LOG MAIN MODEL ##
results_loglog_main = parLapplyLB(cl, bids, function(bid)
  try(simple_loglog(bid), silent=T)
)

results_loglog_main_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_loglog(bid, holdout=holdout), silent=T)
)

## LOG LOG MAIN MODEL - NO LAGGED DV ##
results_loglog_noldv = parLapplyLB(cl, bids, function(bid)
  try(simple_loglog(bid, withlagdv=F), silent=T)
)

results_loglog_noldv_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_loglog(bid, holdout=holdout, withlagdv=F), silent=T)
)


## LOG LOG MAIN MODEL - ONLY LAGGED DV
results_loglog_onlyldv = parLapplyLB(cl, bids, function(bid)
  try(simple_loglog(bid, withlagdv=T, vars=NULL, controls=NULL), silent=T)
)

results_loglog_onlyldv_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_loglog(bid, withlagdv=T, vars=NULL, controls=NULL, holdout=holdout), silent=T)
)


#table(unlist(lapply(results_loglog_noldv_holdout, class)))


####### ADDING MARKETING MIX INSTRUMENTS ITERATIVELY ######

results_ec_nommix = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = NULL, 
                controls_cop = NULL), silent=T)
)

results_ec_nommix_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = NULL, 
                controls_cop = NULL,holdout=holdout), silent=T)
)


results_ec_onlypr = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = c('lnrwpspr'), 
                controls_cop = '^cop[_]ln.*(pr)$'), silent=T)
)

results_ec_onlypr_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = c('lnrwpspr'), 
                controls_cop = '^cop[_]ln.*(pr)$', holdout=holdout), silent=T)
)


results_ec_onlyllen = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = c('lnllen'), 
                controls_cop = '^cop[_]ln.*(llen)$'), silent=T)
)

results_ec_onlyllen_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = c('lnllen'), 
                controls_cop = '^cop[_]ln.*(llen)$', holdout=holdout), silent=T)
)

results_ec_onlydst = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = c('lnwpswdst'), 
                controls_cop = '^cop[_]ln.*(dst)$'), silent=T)
)

results_ec_onlydst_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid,vars = c('lnwpswdst'), 
                controls_cop = '^cop[_]ln.*(dst)$', holdout=holdout), silent=T)
)


####### WITH LAGGED COMPETITION VARIABLES ######

results_ec_unrestrictedcompetition = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_laglevels = '^comp[_].*(pr|llen|dst)$'), silent=T)
)

results_ec_unrestrictedcompetition_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_laglevels = '^comp[_].*(pr|llen|dst)$', holdout=holdout), silent=T)
)

####### WITHOUT ENDOGENEITY CONTROLS ######

results_ec_noendogeneity = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_cop = NULL),silent=T)
)

results_ec_noendogeneity_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_cop = NULL, holdout=holdout),silent=T)
)

####### WITH LN TREND INSTEAD OF TREND ######

results_ec_lntrend = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_curr = 'quarter[1-3]|lnholiday|^lntrend'),silent=T)
)
results_ec_lntrend_holdout = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid, controls_curr = 'quarter[1-3]|lnholiday|^lntrend', holdout=holdout),silent=T)
)


#####################
# SUR ON MAIN MODEL #
#####################


results_model <- results_ec_main


estimated_markets <- rbindlist(lapply(results_model, function(x) x$paneldimension[,-c('date'),with=F][1]))
estimated_markets[, ordered:=1:.N,by=c('market_id')]
estimated_markets[, index:=1:.N]

# ESTIMATE SUR
split_by_market = split(results_model, estimated_markets$market_id)

cat(paste0('Estimating SUR for ', length(split_by_market), ' markets...\n'))

sur_res = parLapplyLB(cl, split_by_market, function(focal_models) {
  mid=focal_models[[1]]$paneldimension$market_id[1]
  cat(mid,fill=T)
  
  res=suppressWarnings(try(model_sur(focal_models), silent=T))
  if(class(res)=='try-error') res=suppressWarnings(try(model_sur(focal_models, maxiter=1), silent=T))
  
  
  list(market_id=mid, results=res)
})


table(unlist(lapply(sur_res,class)))

# WRITE RESULTS OF SUR TO MAIN RESULT SET
for (i in seq(along=sur_res)) {
  mid = unlist(lapply(sur_res, function(x) x$market_id))[i]
  for (j in estimated_markets[market_id==mid]$ordered) {
    ck=try(sur_res[[i]]$results$coefs[[j]],silent=T)
    if (class(ck)!='try-error') {
      results_model[[estimated_markets[market_id==mid]$index[j]]]$sur <- list(coefs=sur_res[[i]]$results$coefs[[j]],
                                                                              varcovar=sur_res[[i]]$results$varcovar[[j]])
    }
  }
}


# calculation of elasticities [hm...]

results_with_sur_models = parLapplyLB(cl, results_model, process_sur)

# remove models
results_ec_main_sur <- lapply(results_with_sur_models, function(x) {
  x$model_matrix <- NULL
  x$paneldimension <- NULL
  x$dt <- NULL
  x$elast_sur$country=x$elast$country
  x$elast_sur$category=x$elast$category
  x$elast_sur$brand=x$elast$brand
  x$elast_sur$brand_id=x$elast$brand_id
  
  
  x$elast <- x$elast_sur
  
  x
})



rm(results_with_sur_models)
rm(results_model)



# Function scans global environment for occurence of regular expression (`regex`), 
# and saves all objects in `filename`.
save_by_regex <- function(regex, filename) {
  lscall = ls(envir=.GlobalEnv)
  stuff_to_save = grep(regex, lscall, value=T)
  if (length(stuff_to_save)>0) {
    cat('saving...\n')
    cat(paste0('(', paste0(stuff_to_save, collapse=', '), ')\n'))
    save(list=stuff_to_save , file = filename)
    cat('...done.\n') } else {
      cat('No objects to save. Verify regular expression.\n')
    }
}

#save_by_regex('^results[_]', filename = '../output/results_simplified.RData')


###########
# Holdout #
###########


#results_ec_holdout10 = parLapplyLB(cl, bids, function(bid)
#  try(simple_ec(bid, holdout=.1),
#      silent = T)
#)


######################################
# Robustness w/ advertising spending #
######################################


china_hk_selection = brand_panel[country%in%c('china','hong kong')&
                                   noadv==F & date<ifelse(country=='china', '2012-09-01', '2014-12-01'), 
                                 list(obs= length(unique(date))),by=c('category','country', 'market_id', 'brand_id')]

china_hk_selection = china_hk_selection[, selected:=obs>ifelse(category=='tablet',4*12,5*12)]
china_hk = unique(china_hk_selection[selected==T]$brand_id)

length(china_hk)

# find markets with adv

table(sapply(china_hk, function(bid) use_ts(brand_panel[brand_id==bid]$adv)))
brand_panel[brand_id%in%china_hk, list(length(unique(brand))),by=c('category','country')]


results_ec_chinahk_withadv = parLapplyLB(cl, china_hk, function(bid)
  try(simple_ec(bid, vars = c('lnrwpspr','lnllen','lnwpswdst', 'lnradv'),
                controls_diffs='^comp[_].*(pr|llen|dst|adv)$', 
                controls_cop = '^cop[_]ln.*(pr|llen|dst|adv)$'),
      silent = T)
)

results_ec_chinahk_withoutadv = parLapplyLB(cl, china_hk, function(bid)
  try(simple_ec(bid),
      silent = T)
)


###################################################
# Robustness w/ first and last x% of observations #
###################################################

save_brandpanel = copy(brand_panel)

brand_sel = brand_panel[, list(obs = sum(usales>0,na.rm=T)), by = c('category','country','brand', 'brand_id')]
brand_sel[, selected:=obs>=8*12]

selected_bids = brand_sel[selected==T]$brand_id
length(selected_bids)


perc_select=.6
brand_panel[!is.na(usales), percentile_obs:=(1:.N)/.N, by = c('category','country','brand')]
brand_panel = brand_panel[percentile_obs<=perc_select & brand_id %in% selected_bids]


clusterExport(cl,c('brand_panel'))
bids = unique(brand_panel$brand_id)
length(bids)

results_ec_first60 = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid), silent = T)
)

# Last X

brand_panel = save_brandpanel

brand_panel[!is.na(usales), percentile_obs:=(1:.N)/.N, by = c('category','country','brand')]
brand_panel = brand_panel[percentile_obs>=(1-perc_select) & brand_id %in% selected_bids]

clusterExport(cl,c('brand_panel'))
bids = unique(brand_panel$brand_id)
length(bids)


results_ec_last60 = parLapplyLB(cl, bids, function(bid)
  try(simple_ec(bid), silent = T)
)

save_by_regex('^results[_]', filename = '../output/results_simplified.RData')
