#
#
#
# Extracts elasticities from .RData file
#
#
#
#

# Load packages
library(data.table)
library(marketingtools)

# Load results
load(file = c('../output/results.RData'))

# load panel data
brand_panel=fread('../temp/preclean.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load GDP per cap data
load('..\\..\\derived\\temp\\gdppercap.RData')
gdp = gdppercap[, list(gdppercap=mean(gdppercap,na.rm=T)), by = c('country')]
setkey(gdp, country)

# Load brand-country classifications
brands_countries <- fread('../../../../data/brands_countries/brands_countries.tsv')
setkey(brands_countries, brand)

# Extract model names from .RData file
lscall=ls(envir=.GlobalEnv)
models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')

# Extract elasticities
out = lapply(models, function(model_name) {
  
  results_brands=eval(parse(text=model_name))

  # identify model crashes
  checks <- unlist(lapply(results_brands, class))
  
  markets=data.table(market_id=analysis_markets)[, ':=' (i=1:.N, available=!checks=='try-error')]
  
  # elasticities
  elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))

  # tag global versus local brand
  elast[, ncountries:=length(unique(country)), by = c('brand')]
  elast[, globalbrand:=ncountries>2 & !brand=='unbranded']
  
  elast[, hedon := category %in% c('tablets', 'phones_smart', 'phones_mobile', 'camera_slr', 'camera_compact', 'dvd', 'tv_gen1_crtv', 'tv_gen2_lcd')]
  elast[, highms := mean_ms>=median(mean_ms), by = c('market_id', 'variable')]
  
  # merge GDP per cap
  setkey(elast, country)
  elast[gdp, gdppercap:=i.gdppercap]

  # merge country (high versus low income) and category class (camera, computer, phones, tv/dvd, white goods)
  vars = c('cat_class', 'country_class')
  tmp = brand_panel[, c('market_id', vars), with=F]
  setkey(tmp, market_id)
  tmp = unique(tmp)

  elast = merge(elast, tmp, all.x=T, all.y=F, by = c('market_id'))
  
  # indicator for new categories
  elast[, new:=category%in%c('tablets', 'phones_smart')]
  
  # add world bank classifications
  elast[, wb_lowermid:=country%in%c('india','indonesia', 'vietnam', 'philippines')]
  elast[, wb_uppermid:=country%in%c('china', 'malaysia','thailand')]

  elast[, low := wb_lowermid+wb_uppermid]
  
  # merge country of origins for top brands
  global_brands=elast[globalbrand==T, list(.N), by = c('brand')]
  setkey(global_brands, brand)
  setkey(brands_countries, brand)
  global_brands[brands_countries, brand_country := i.country_cleaned]
  
  setkey(global_brands, brand)
  setkey(elast, brand)
  elast[global_brands, global_country:=i.brand_country]
  
  # country classifications
  elast[, local_to_market:=global_country==country]
  elast[, western_brand:=global_country%in%c('canada','finland','germany','italy','netherlands','sweden','usa')]
  
  return(list(checks=markets, elast=elast, model = model_name))
  })

results <- out

sink('../output/elasticities.txt')
cat('Finished building extract_elast.R at:\n')
print(Sys.time())
cat('\n\n(File used to enable track changes by makefile).\n')
sink()

for (i in seq(along=out)) {
 fwrite(out[[i]]$elast, file=paste0('../output/elast_', out[[i]]$model,'.csv'), row.names=F) 
}
