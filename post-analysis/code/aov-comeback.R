# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)
library(sjstats)


fns <- c('app/app_workspace.RData')

for (fn in fns) if (file.exists(fn)) {cat(paste0('loading...', fn, '...\n')); load(fn)}

elast <- elasticities$ec_main_sur
set.seed(1234)

vars=c('lnllen','lnrwpspr','lnwpswdst')
dir.create('../audit')
sink('../audit/variance-decomposition.txt')

for (i in vars) {
  cat('========================\n')
  cat(i,fill=T)
  cat('========================\n\n')
  
  m0_1<-aov(elastlt~brand, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_2<-aov(elastlt~country, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_3<-aov(elastlt~category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_4<-aov(elastlt~(brand*country)-brand-country, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_5<-aov(elastlt~(brand*category)-brand-category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_6<-aov(elastlt~(country*category)-country-category, data=elast, subset=variable==i, weights=1/elastlt_se)
  
  m1<-aov(elastlt~brand+country+category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m3<-aov(elastlt~brand*country + brand*category + category*country, data=elast, subset=variable==i, weights=1/elastlt_se)
  
  mlist = list(m0_1,m0_2, m0_3, m0_4, m0_5, m0_6, m1, m3)
  etasq=rbindlist(lapply(seq(along=mlist), function(x) data.table(i=x, anova_stats(mlist[[x]]))))
  etasq[i%in%1:6, model:='m0']
  
  etasq[i%in%7, model:='m1']
  etasq[i%in%8, model:='m2']
  
  
  cat('\n\nPercent explained\n')
  tmp = (dcast(etasq[!term=='Residuals'], model~term, value.var='etasq'))
  
  setnames(tmp, gsub('[:]', ' x ', colnames(tmp)))
  setcolorder(tmp, c('model','brand','category','country','brand x category','brand x country','country x category'))
  
  print(tmp)      
  #print(dcast(etasq[!term=='Residuals'], model~term, value.var='etasq'))
  
}

sink()
  