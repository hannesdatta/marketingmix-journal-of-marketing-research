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
#library(marketingtools)

# Load results
load(file = '../output/results_salesresponsenew.RData')

#unlink('../output/*.csv')

# load panel data
brand_panel=fread('../temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load brand-country classifications
brands_countries <- fread('../../../../data/brands_countries/brands_countries.tsv')
setkey(brands_countries, brand)

# Extract model names from .RData file
lscall=ls(envir=.GlobalEnv)
models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')

# Extract elasticities
out = lapply(models, function(model_name) {
  print(model_name)
  results_brands=eval(parse(text=model_name))

  # identify model crashes
  checks <- unlist(lapply(results_brands, function(x) class(x)[1]))
  
  #markets=data.table(market_id=analysis_markets)[, ':=' (i=1:.N, available=!checks=='try-error')]
  
  # extract elasticities
  elast <- rbindlist(results_brands) #lapply(results_brands[!checks=='character'], function(x) x$elasticities))
  #setnames(elast, 'varname','variable')
  setkey(elast, brand_id)
  setkey(brand_panel, brand_id)
  elast[brand_panel, ':=' (brand=i.brand, country = i.country, category = i.category, market_id=i.market_id)]
  
  
 # tag global versus local brand
  elast[, ncountries:=length(unique(country)), by = c('brand')]
  elast[, globalbrand:=ncountries>2 & !brand=='unbranded']
  
  elast[, ncat_in_country:=length(unique(category)), by = c('brand','country')]
  elast[, ncountry_in_category:=length(unique(country)), by = c('brand','category')]
  
  elast[, hedon := category %in% c('tablets', 'phones_smart', 'phones_mobile', 'camera_slr', 'camera_compact', 'dvd', 'tv_gen1_crtv', 'tv_gen2_lcd')]
  
  elast[, appliance:=0]
  elast[grepl('washing|cooling|microwave', category), appliance:=1]
  
  # merge country (high versus low income) and category class (camera, computer, phones, tv/dvd, white goods)
    # by market ID
    vars = c('cat_class', 'country_class', 'market_growth', 'market_herf', 'market_c3', 'market_c5')
    tmp = brand_panel[, c('market_id', vars), with=F]
    setkey(tmp, market_id)
    tmp = unique(tmp)
  
    elast = merge(elast, tmp, all.x=T, all.y=F, by = c('market_id'))
    
    # by brand/market_id
    vars = c('brand_ms', 'brand_prindex_max', 'brand_prindex_mean')
    tmp = brand_panel[, c('market_id', 'brand', vars), with=F]
    setkey(tmp, market_id,brand)
    tmp = unique(tmp)
    
    elast = merge(elast, tmp, all.x=T, all.y=F, by = c('market_id', 'brand'))
    
  # add world bank classifications
  elast[, wb_lowermid:=country%in%c('india','indonesia', 'vietnam', 'philippines')]
  elast[, wb_uppermid:=country%in%c('china', 'malaysia','thailand')]
  
  # merge country of origins for brands
  brands_countries <- brands_countries[!brand=='']
  brands_countries[country_cleaned=='', country_cleaned:=NA]
  
  setkey(elast, brand)
  setkey(brands_countries, brand)
  
  elast[brands_countries, country_of_origin:=i.country_cleaned]

  # region-of-origin
  #elast[!is.na(country_of_origin), region_of_origin := 'asian']
  #elast[country_of_origin%in%c('finland','france', 'germany','italy','netherlands','sweden'), region_of_origin := 'europe']
  #elast[globalbrand==T & country_of_origin%in%c('canada','usa'), region_of_origin := 'northamerica']
  
  return(list(checks=checks, elast=elast, model = model_name))
  })

results <- out

for (i in seq(along=out)) {
 fwrite(out[[i]]$elast, file=paste0('../output/elast_', out[[i]]$model,'.csv'), row.names=F) 
}

sink('../output/elasticities.txt')
cat('Finished building extract_elast.R at:\n')
print(Sys.time())
for (i in seq(along=out)) {
  cat(out[[i]]$model,fill=T)
print(table(ifelse(out[[i]]$checks=='list','models converged','error in estimation procedure')))
}

cat('\n\n(File used to enable track changes by makefile).\n')
sink()

