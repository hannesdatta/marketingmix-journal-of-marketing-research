# prepare covariate data for second-stage regressions
library(data.table)

# Load data
brand_panel=fread('../../analysis/temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# find attributes

tmp1=brand_panel[, lapply(.SD, mean,na.rm=T),by=c('category','country','year'),.SDcol=grep('catvolat',colnames(brand_panel),value=T)]
tmp2=tmp1[, lapply(.SD, mean,na.rm=T),by=c('category','country'),.SDcol=grep('catvolat',colnames(brand_panel),value=T)]
setnames(tmp2, c('category','country','catvolatility_range_mean', 'catvolatility_sd_mean'))

brand_panel <- merge(brand_panel, tmp2, by=c('category','country'))

agg_units = list(c('country'), c('country', 'category'),
                 c('country','category','brand'))

out=rbindlist(lapply(agg_units, function(agg) {
  #agg = agg_units[[1]]
  
  tmp = brand_panel[, lapply(.SD, uniqueN), by = agg]
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

sink('../output/covariates.txt')

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
