# Setup
  dir.create('../output')

  # Load brand panel
  brand_panel=fread('../../derived/output/datasets.csv')
  brand_panel[, ':=' (date = as.Date(date))]

  # Results
  elast <- fread('../../post-analysis/externals/elast_results_nov12sh.csv')
  

# Plot: Market concentration in HK - Tablets
  
  load(file='../derived/temp/select.RData')
  
  tmp=tmp_brands_select[category=='tablets'&country=='HONG KONG']
  sum(tmp$marketshare)
  setorderv(tmp, 'marketshare', order=-1L)
  
  png('../output/concentration_hktablet.png', res=150, units='in', height=4, width=4)
  plot(1:nrow(tmp), cumsum(tmp$marketshare), xlab = 'Top X firms, ordered by market share',
       ylab='cumulative market share', type='l', xlim = c(1,10), lwd=1,lty=1, font.lab=2,
       main = 'Market concentration for\ntablets in Hong Kong')
  dev.off()

# Plot: Distribution of number of brands per market
  
  tmp=tmp_brands_select[, list(n_brands_selected=length(unique(brand[which(selected_brand==T)]))), by=c('category','country')]
  tmp[,selected_market:=n_brands_selected>1]
  
  png('../output/selectedbrands.png', res=150, units='in', height=4, width=4)
  
  hist(tmp[selected_market==T]$n_brands_selected, breaks=20, main='Number of selected brands\nin a market',
       xlab = 'Number of brands', xlim = c(0,25))
  
  dev.off()

  
# Number of countries active in
  tmp=brand_panel[, list(ncountries=length(unique(country))),by=c('brand')]
  
  png('../output/hist_nocountries.png', res=150, units='in', height=4, width=4)
  
  hist(tmp$ncountries, main='In how many countries\nare brands active?',
       xlab = 'Number of countries', ylab = 'Number of brands', xlim = c(1,14))
  
  dev.off()
  
# In which categories are brands unique?
  brand_panel[, ncountries:=length(unique(country)),by=c('brand')]
  tmp=brand_panel[, mean(ncountries), by=c('category')]
  setorder(tmp, V1)
  
# Plot: Top 5 brands Smart phones Malaysia
  require(latticeExtra)
  tmp=brand_panel[country=='malaysia'&category=='phones_smart'&!brand=='allothers']
  tmp[, ms:=mean(usalessh,na.rm=T),by=c('brand')]
  unique(tmp, by='brand')
  
  require(lattice)
  tmp[, nov12sh:=nov12/llen]
  vars = list(c('usalessh','Market share'), c('rwpspr', 'Price (local currency)'),
              c('llen', 'Line length'), c('wpswdst', 'Distribution'), c('nov12sh', 'New product activity'))
  
  for (var in vars) {
    tmp[, v:=get(var[1])]
    png(paste0('../output/dataplot_',var[1],'.png'), res=150, units='in', height=3, width=3)
    
    print(xyplot(v~date, groups=brand, data=tmp[ms>.08], type='spline',
         par.settings = theEconomist.theme(box = "transparent"),
         lattice.options = theEconomist.opts(), xlab= 'Date', ylab=var[2]))#,auto.key=T))
    dev.off()
  }
  png(paste0('../output/dataplot_legend.png'), res=150, units='in', height=3, width=3)
  
  print(xyplot(v~date, groups=brand, data=tmp[ms>.08], type='spline',
               par.settings = theEconomist.theme(box = "transparent"),
               lattice.options = theEconomist.opts(), xlab= 'Date', ylab=var[2],auto.key=T))
  dev.off()
  
# Plot: Histograms per instrument
  
  for (var in vars[-1]) {
    print(var[1])
    png(paste0('../output/hist-universal_',var[1],'.png'), res=150, units='in', height=3, width=6)
    
    print(histogram(~elastlt,data=elast[variable==var[1]],
              type="density",
              xlab=paste0('Elasticity'),
              main=paste0(var[2], ' elasticity'), breaks = 40,
              par.settings = theEconomist.theme(box = "transparent"),
              lattice.options = theEconomist.opts()))
    dev.off()
  }
  
  # Where does variation come from
  source('../../post-analysis/code/proc_rename.R')
  
  for (var in vars[-1]) {
    print(var[1])
    png(paste0('../output/variation-cat_',var[1],'.png'), res=150, units='in', height=3, width=6)
    elast[, w:=1/elastlt_se]
    
    tmp=elast[variable==var[1], list(elastlt=sum(elastlt*w)/sum(w)), by = 'category']
    setorder(tmp, elastlt)
    tmp[, cat:=rename.fkt(category, dictionary=c('../../post-analysis/code/renaming.txt'))]
    
    tmp[, cat:=factor(cat, levels=tmp$cat)]
    
    print(barchart(elastlt~cat,data=tmp,scales=list(x=list(rot=45)),
                   ylab='Elasticity', main = paste0(var[2], ' elasticity'),
                   par.settings = theEconomist.theme(box = "transparent"),
                   lattice.options = theEconomist.opts()))
          
    dev.off()
  }
  
          
  
  for (var in vars[-1]) {
    print(var[1])
    png(paste0('../output/variation-cntry_',var[1],'.png'), res=150, units='in', height=3, width=6)
    elast[, w:=1/elastlt_se]
    
    tmp=elast[variable==var[1], list(elastlt=sum(elastlt*w)/sum(w)), by = 'country']
    setorder(tmp, elastlt)
    tmp[, cat:=rename.fkt(country, dictionary=c('../../post-analysis/code/renaming.txt'))]
    
    tmp[, cat:=factor(cat, levels=tmp$cat)]
    
    
    print(barchart(elastlt~cat,data=tmp,scales=list(x=list(rot=45)),
                   ylab='Elasticity', main = paste0(var[2], ' elasticity'),
                   par.settings = theEconomist.theme(box = "transparent"),
                   lattice.options = theEconomist.opts()))
    
    dev.off()
  }
  
  
  
  # R2 categories, countries, both
  res=NULL
  for (var in vars[-1]) {
    m1<-lm(elastlt~1+category, data=elast[variable==var[1]])
    m2<-lm(elastlt~1+country, data=elast[variable==var[1]])
    m3<-lm(elastlt~1+category+country, data=elast[variable==var[1]])
    res=rbind(res,cbind(var=var[1], varlabel=var[2], r2_1 = summary(m1)$r.squared,
                                                 r2_2 = summary(m2)$r.squared,
                                                 r2_3 = summary(m3)$r.squared))
  }
  
  res=data.frame(res)
  
  
  # Coefficient of variation
  
  
  
  for (trim in c(T,F)) {
  
    
    elast[, quant:=ecdf(elastlt)(elastlt),by=c('variable')]
    if (trim==T) tmp = elast[quant>=.025&quant<=.975]
    if (trim==F) tmp = elast
    
    
    tmp = tmp[, list(coefvar = abs(sd(elastlt,na.rm=T)/mean(elastlt,na.rm=T))), by = c('country_class', 'variable')]
    tmp[, country_type := ifelse(country_class=='hinc', 'developed', 'emerging')]
    tmp[, varname:=rename.fkt(variable, dictionary=c('../../post-analysis/code/renaming.txt'))]
    tmp[, varname:=factor(varname, levels=c('price','distribution','line length', 'new product activity'))]
    
    png(paste0('../output/coefvar', ifelse(trim==T, '-trim',''),'.png'), res=150, units='in', height=5, width=10)
    
    print(barchart(coefvar~varname, groups= country_type,data=tmp, 
             par.settings = theEconomist.theme(box = "transparent"),
             lattice.options = theEconomist.opts(), main = 'Variation in marketing mix elasticities within emerging and developed countries',
             ylab='Coefficient of variation',
             auto.key=list(space="right", title = 'Country type')))
    
    dev.off()
    
  }
  
  # coef var plot
  
  #for (trim in c(T,F)) {
    
    trim=F
    elast[, quant:=ecdf(elastlt)(elastlt),by=c('variable')]
    if (trim==T) tmp = elast[quant>=.025&quant<=.975]
    if (trim==F) tmp = elast
    
    tmp = tmp[, list(coefvar = abs(sd(elastlt,na.rm=T)/mean(elastlt,na.rm=T)),
                     N=length(unique(brand))), by = c('country_class', 'variable', 'market_id')]
    tmp[, country_type := ifelse(country_class=='hinc', 'developed', 'emerging')]
    tmp[, varname:=rename.fkt(variable, dictionary=c('../../post-analysis/code/renaming.txt'))]
    tmp[, varname:=factor(varname, levels=c('price','distribution','line length', 'new product activity'))]
    
    for (var in vars[-1]) {
      print(var[2])
      print(summary(lm(coefvar~1+country_type, data = tmp[variable==var[1]])))
    }
    
    
    png(paste0('../output/coefvar', ifelse(trim==T, '-trim',''),'.png'), res=150, units='in', height=5, width=10)
    
    print(barchart(coefvar~varname, groups= country_type,data=tmp, 
                   par.settings = theEconomist.theme(box = "transparent"),
                   lattice.options = theEconomist.opts(), main = 'Variation in marketing mix elasticities within emerging and developed countries',
                   ylab='Coefficient of variation',
                   auto.key=list(space="right", title = 'Country type')))
    
    dev.off()
    
  }
  
  # Density plots price/distribution
  
  for (trim in c(T,F)) {
    for (var in vars[-1]) {
      print(var[1])
      tmp=elast[variable==var[1]]
      
      if (trim==T) {
        tmp[, quant:=ecdf(elastlt)(elastlt)]
        tmp = tmp[quant>=.025&quant<=.95]
      }
      
      tmp[, country_type := ifelse(country_class=='hinc', 'developed', 'emerging')]
      
      png(paste0('../output/densities_', ifelse(trim==T, 'quantile',''), '',var[1],'.png'), res=150, 
          units='in', height=5, width=8)
      
      print(histogram(~elastlt,data=tmp,
              type="density", bw=100,kernel="gaussian", groups = country_type,
              panel=function(x, ...) {
                #panel.histogram(x, ...)
                panel.densityplot(x, ...)
              }, xlab= 'Elasticity',main = paste0(var[2], ' elasticity'),
              par.settings = theEconomist.theme(box = "transparent"),
              lattice.options = theEconomist.opts(), ylim = c(0,ifelse(var[1]=='nov12sh', 5, 1)),xlim=c(-5,5),auto.key=list(space="right", title = 'Country type')))
     dev.off()
    }
  }
  
  