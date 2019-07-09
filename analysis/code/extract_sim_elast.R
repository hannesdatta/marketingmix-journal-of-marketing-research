library(data.table)

load('../output/simulation.RData')

elast <- fread('../output/elast_results_main.csv')

setkey(elast, market_id, brand, variable)

sink('../output/simulation.txt')

  # long-term
  sim_dat <- rbindlist(lapply(sim_res[unlist(lapply(sim_res,function(x) 'data.table'%in%class(x$elasticities)))], function(x) x$elasticities))
  sim_dat <- sim_dat[brand==brand_of][, brand_of:=NULL]
  
  sim_dat[, market_id:=as.integer(as.character(market_id))]
  
  setkey(sim_dat, market_id, brand, sim_var)
  
  elast2=merge(elast, sim_dat, by.x=c('market_id','category','country','brand','variable'), by.y=c('market_id','category','country','brand','sim_var'), all.x=T, all.y=F)
  
  cat('Correspondence between empirically-derived and simulated elasticities\n\n')
  
  for (.by in c('none','variable')) {
    if (.by=='none') cat('POOLED:\n') else cat('BY VARIABLE:\n')
    elast2[, none:='NA']
    
    comparison = elast2[, list(Nmarkets=length(unique(market_id)), elast_simulated=mean(simelastlt), elast=mean(elastlt), 
                               correlation=cor(simelastlt,elastlt), 
                               mean_abs_dev=mean(abs(simelastlt-elastlt)), 
                               mape = mean(abs((elastlt-simelastlt)/elastlt)),
                               rmse=sqrt(mean((simelastlt-elastlt)^2)),
                               
                               seelast_simulated=mean(simelastlt_sd), elast=mean(elastlt_se), 
                               secorrelation=cor(simelastlt_sd,elastlt_se), 
                               semean_abs_dev=mean(abs(simelastlt_sd-elastlt_se)), 
                               semape = mean(abs((elastlt_se-simelastlt_sd)/elastlt_se)),
                               sermse=sqrt(mean((simelastlt_sd-elastlt_se)^2))
                               
                               
                               
    ),by=.by]
    
    print(comparison)
    
    cat('\n\n\n')
  }
  
  # Statistical check
  tmp = elast2[, c('variable','elastlt', 'elastlt_se', 'simelastlt','simelastlt_sd'),with=F]
  tmp[, id:=1:.N]
  setnames(tmp,'variable','simvar')
  tmp2 = melt(tmp, id.vars = c('id','simvar'))
  tmp2[, se:=ifelse(grepl('_se|_sd',variable), 'se','est')]
  tmp2[, var:=ifelse(grepl('simelast',variable), 'simulated','empirical')]  
  
  elast_comparison=dcast(tmp2, id+simvar+var~se)
  elast_comparison[, weights:=(1/se)/sum(1/se),by=c('simvar')]
  
  
  cor(elast_comparison[var=='empirical']$est, elast_comparison[var=='simulated']$est)
  

  for (.var in unique(elast_comparison$simvar)) {
    cat('======================================\n')
    cat(.var,fill=T)
    cat('======================================\n\n')
    cat('ESTIMATES\n')
    print(summary(lm(est~1+I(var=='simulated'),data=elast_comparison[simvar==.var])))
    cat('SE\n')
    print(summary(lm(se~1+I(var=='simulated'),data=elast_comparison[simvar==.var])))
    
    cat('WLS\n')
    print(summary(lm(est~1+I(var=='simulated'),data=elast_comparison[simvar==.var],weights=weights)))
    
    cat('\n\n')
  }

fwrite(elast2, '../output/elast_results_simulation.csv')

sink()
