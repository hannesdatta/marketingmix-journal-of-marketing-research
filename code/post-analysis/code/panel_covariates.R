# prepare covariate data for second-stage regressions
library(data.table)

# Load data
brand_panel=fread('../externals/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]
brand_panel = brand_panel[selected==T&timewindow==T&obs48==T]


# videocon is from india, not from ger
brand_panel[brand=='videocon']$`brand_from_jp-us-ch-ge-sw`=0
# YOshii is from Japan
brand_panel[grepl('yoshii', brand)]$`brand_from_jp-us-ch-ge-sw`=1
# Simpson is from Australia
brand_panel[grepl('simpson', brand)]$`brand_from_jp-us-ch-ge-sw`=0


# find attributes
unlink(list.files('../output/', pattern='^covariates.*', full.names = T))
dir.create('../output')

tmp1=brand_panel[, lapply(.SD, mean,na.rm=T),by=c('category','country','year'),.SDcol=grep('catvolat',colnames(brand_panel),value=T)]
tmp2=tmp1[, lapply(.SD, mean,na.rm=T),by=c('category','country'),.SDcol=grep('catvolat',colnames(brand_panel),value=T)]
setnames(tmp2, c('category','country','catvolatility_range_mean', 'catvolatility_sd_mean'))

brand_panel <- merge(brand_panel, tmp2, by=c('category','country'))

# growth
tmp1=brand_panel[, list(N=.N, avgsales=mean(usales,na.rm=T)), by = c('category','country','brand','year')]
tmp1[, maxyear:=max(year), by = c('category','country','brand')]
tmp1[, lagavgsales:=c(NA,avgsales[-.N]), by = c('category','country','brand')]

tmp1[, growth:=(avgsales-lagavgsales)/lagavgsales+1]

tmp2=tmp1[, list(avgbrandgrowth=mean(growth[year>=maxyear-2],na.rm=T)), by = c('category','country', 'brand')]

brand_panel <- merge(brand_panel, tmp2, by=c('category','country', 'brand'))


# average mmix
cols = c('rwpspr', 'rwpsprd', 'llen', 'wpswdst', 'nov6sh', 'nov12sh')
mix = brand_panel[, lapply(.SD, mean, na.rm=T), by = c('category','country','brand'), .SDcols=c(cols,'usales')]
for (.col in cols) mix[, paste0(.col, '_windex'):=get(.col)/(sum(usales*get(.col))/sum(usales)), by = c('category','country')]
for (.col in cols) mix[, paste0(.col, '_index'):=get(.col)/mean(get(.col)), by = c('category','country')]
for (.col in cols) mix[, paste0(.col, '_std'):=(get(.col)-mean(get(.col)))/sd(get(.col)), by = c('category','country')]

setkey(mix, category, country, brand)
setkey(brand_panel, category, country, brand)
for (.col in cols) eval(parse(text=paste0('brand_panel[mix, ', .col, '_index:=i.', .col, '_index]')))
for (.col in cols) eval(parse(text=paste0('brand_panel[mix, ', .col, '_windex:=i.', .col, '_windex]')))
for (.col in cols) eval(parse(text=paste0('brand_panel[mix, ', .col, '_std:=i.', .col, '_std]')))

# Average sales
brand_panel[, ':=' (avgsales=mean(usales,na.rm=T),
                             sumsales=sum(usales, na.rm=T)),by=c('category','country','brand')]

brand_panel[, market_growth100:=market_growth*100]
brand_panel[, market_growthneg:=100*(market_growth-1)]


# define country-level metrics to vary only at the yearly level for brands
vars <- grep('2010$|avg$', grep('wgi_|gdp|gini|trade|population|penn[_]', colnames(brand_panel),value=T), value=T, invert=T)
#vars <- vars[!grepl('growthrgdp', vars)]

# retrieve yearly estimates for categories/countries (e.g., some categories are available earlier, some later)
#
tmp = unique(brand_panel, by = c('category','country', 'year'))

tmp_means = tmp[, lapply(.SD, mean,na.rm=T), by = c('category','country'), .SDcol=vars]
tmp_means <- tmp_means[,unlist(lapply(tmp_means,function(x) all(is.na(x))))==F,with=F]
colnames(tmp_means)
for (.v in vars) if (.v %in% colnames(tmp_means)) setnames(tmp_means, .v, paste0(.v,'yravg'))



# calculate growth rates
growthvars=grep('^(penn[_]|penn[_]percapita)(rgdpe|rgdpo)$', colnames(tmp),value=T)
tmp_growth = lapply(growthvars, function(.v) {
  tmp2 = tmp[, list(tmp_last=get(.v)[which(year==max(year))],
              tmp_first=get(.v)[which(year==min(year))],
              nyears = max(year)-min(year)+1), by = c('category','country')]
  tmp2[, paste0(.v, 'ggrowth'):=(tmp_last/tmp_first)^(1/(nyears-1))]
  tmp2[, paste0(.v, 'ggrowth100'):=100 * ((tmp_last/tmp_first)^(1/(nyears-1)))]
  tmp2[, paste0(.v, 'ggrowthneg'):= 100 * ((tmp_last/tmp_first)^(1/(nyears-1))-1)]
  
  tmp2[, ':=' (tmp_last=NULL, tmp_first=NULL, nyears=NULL)]
  setkey(tmp2, category, country)
  tmp2
})
merge.all <- function(x,y, ...) {merge(x,y, all.x=T,all.y=T,...)}
tmp_growth_final= Reduce(merge.all, tmp_growth)

tmp <- merge(tmp_means, tmp_growth_final, by = c('category','country'), all.x=T)



#tmp = unique(brand_panel[selected==T&timewindow==T&obs48==T], by = c('category','country','year'))[, lapply(.SD, mean,na.rm=T), by = c('category','country'), .SDcol=vars]


# calculate geometric growth rates for PENN variables
#

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

