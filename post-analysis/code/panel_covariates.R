# prepare covariate data for second-stage regressions
library(data.table)

# Load data
brand_panel=fread('../externals/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# find attributes
unlink(list.files('../output/', pattern='^covariates.*', full.names = T))

tmp1=brand_panel[, lapply(.SD, mean,na.rm=T),by=c('category','country','year'),.SDcol=grep('catvolat',colnames(brand_panel),value=T)]
tmp2=tmp1[, lapply(.SD, mean,na.rm=T),by=c('category','country'),.SDcol=grep('catvolat',colnames(brand_panel),value=T)]
setnames(tmp2, c('category','country','catvolatility_range_mean', 'catvolatility_sd_mean'))

brand_panel <- merge(brand_panel, tmp2, by=c('category','country'))

# average mmix
cols = c('rwpspr', 'rwpsprd', 'llen', 'wpswdst')
mix =brand_panel[, lapply(.SD, mean, na.rm=T), by = c('category','country','brand'), .SDcols=cols]
for (.col in cols) mix[, paste0(.col, '_index'):=get(.col)/mean(get(.col)), by = c('category','country')]
for (.col in cols) mix[, paste0(.col, '_std'):=(get(.col)-mean(get(.col)))/sd(get(.col)), by = c('category','country')]

setkey(mix, category, country, brand)
setkey(brand_panel, category, country, brand)
for (.col in cols) eval(parse(text=paste0('brand_panel[mix, ', .col, '_index:=i.', .col, '_index]')))
for (.col in cols) eval(parse(text=paste0('brand_panel[mix, ', .col, '_std:=i.', .col, '_std]')))

# define metrics to vary only at the yearly level for brands
vars <- grep('2010$|avg$', grep('wgi_|gdp|gini|trade', colnames(brand_panel),value=T), value=T, invert=T)

tmp = unique(brand_panel, by = c('category','country','year'))[, lapply(.SD, mean,na.rm=T), by = c('category','country'), .SDcol=vars]
tmp <- tmp[,unlist(lapply(tmp,function(x) all(is.na(x))))==F,with=F]
for (.v in vars) if (.v %in% colnames(tmp)) setnames(tmp, .v, paste0(.v,'yravg'))

year_averages <- copy(tmp)

# log continues variables which are non-negative
   for (.var in setdiff(colnames(year_averages), c('category','country'))) {
      # dummy var
      vrs <- unlist(year_averages[, .var,with=F])
      #print(.var)
      if (class(vrs)=='character') next
      if (all(vrs %in% c(NA,1,0))) next
      if (all(is.na(vrs) | vrs >= 0)) year_averages[, paste0('ln_', .var):=log(get(.var)+1)]
      if (all(is.na(vrs) | vrs > 0)) year_averages[, paste0('ln_', .var):=log(get(.var))]
      
   }

fwrite(year_averages, paste0('../output/covariates-yearaverages_category-country.csv'))

# aggregation
agg_units = list(c('country'), c('country', 'category'),
                 #c('country', 'category', 'year'),
                 c('country','category','brand'))

out=rbindlist(lapply(agg_units, function(agg) {
  #agg = agg_units[[1]]
  
  #tmp = brand_panel[, lapply(.SD, uniqueN), by = agg]
  
  .vars <- setdiff(colnames(brand_panel), agg)
  tmpx=rbindlist(lapply(.vars, function(.v) brand_panel[, list(.N),by=c(agg, .v)][, list(variable=.v, N=.N),by=c(agg)]))
  tmp=dcast(tmpx, as.formula(paste0(paste0(agg, collapse='+'),'~variable')), value.var='N')
  
  tmp2=unlist(lapply(tmp, function(x) all(x==1)))
  res=data.table(variable = names(tmp2)[which(tmp2==T)])
  res[, agg_factor:=paste(agg, collapse='-')]
  for (a in agg) res[, (a):=1]
  res
}),fill=T)

out[, sum:=apply(out[, setdiff(colnames(out), c('variable','agg_factor')),with=F], 1, sum, na.rm=T)]

out[, var_index:=.GRP, by = c('variable')]
setorder(out, var_index, sum)
out[, sel:=1:.N==1, by=c('variable')]
vars <- out[sel==T]

exclude <- c('selected|market_id|^attr|brand_id|noofbrands')

vars <- vars[!grepl(exclude, variable)]

tmp = split(vars, vars$agg_factor)

sink('../output/panel_covariates.txt')

dt <- lapply(tmp, function(x) {
 aggkey = strsplit(unique(x$agg_factor), '[-]')[[1]]
 tmp=unique(brand_panel, by=aggkey)[, c(aggkey, x$variable),with=F]
 
 # log continues variables which are non-negative
 for (.var in x$variable) {
   # dummy var
   vrs <- unlist(tmp[, .var,with=F])
   #print(.var)
   if (class(vrs)=='character') next
   if (all(vrs %in% c(NA,1,0))) next
   if (all(is.na(vrs) | vrs >= 0)) tmp[, paste0('ln_', .var):=log(get(.var)+1)]
   if (all(is.na(vrs) | vrs > 0)) tmp[, paste0('ln_', .var):=log(get(.var))]
   
 }
 
 cat(paste0('Covariates for: ', unique(x$agg_factor), '\n=========================================\n'))
 print(data.frame(variable=colnames(tmp)))
 
 cat('\n\n')
 setkeyv(tmp, aggkey)
 
 fwrite(tmp, paste0('../output/covariates_', unique(x$agg_factor), '.csv'))
 return(tmp)
 
})

sink()

