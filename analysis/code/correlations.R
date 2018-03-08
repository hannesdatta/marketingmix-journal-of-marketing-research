for (var in setup_x) {
  brand_panel[, (paste0('mc_', var)):=get(var)-mean(get(var),na.rm=T), by = c('market_id', 'brand')]
  }

cor(brand_panel[, c(setup_x, 'lshare') ,with=F],use='pairwise')


cor(brand_panel[, grep('mc[_]', colnames(brand_panel)) ,with=F],use='pairwise')
