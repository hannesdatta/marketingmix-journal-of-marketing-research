require(lattice)

panel = brand_panel[market_id==138]

for (i in unique(panel$brand)) {
  plotdat <- panel[brand==i]
  tmp = melt(plotdat, id.vars=c('market_id','category','country','date','brand'))
  tmp=tmp[variable%in%setup_x]
  tmp[, value:=as.numeric(value)]
  
  png(paste0('../temp/brand_', i, '.png'), res=200, units='in', height=8, width=12)
  print(xyplot(value~date|variable, main = i, data=tmp, type='l'))
  dev.off()
}
