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
#load(file = c('../output/results.RData'))
#load(file = c('../temp/results_20180313.RData'))
load(file = c('../output/results23may2018.RData'))

# load panel data
brand_panel=fread('../temp/preclean.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load brand-country classifications
brands_countries <- fread('../../../../data/brands_countries/brands_countries.tsv')
setkey(brands_countries, brand)

# Extract model names from .RData file
lscall=ls(envir=.GlobalEnv)
models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')

# load code to calculate SBBE
source('sbbe.R')

# Extract elasticities
out = lapply(models, function(model_name) {
  
  results_brands=eval(parse(text=model_name))

  # identify model crashes
  checks <- unlist(lapply(results_brands, class))
  
  markets=data.table(market_id=analysis_markets)[, ':=' (i=1:.N, available=!checks=='try-error')]
  
  # extract elasticities
  elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
  
  # extract and merge SBBE
    sbbe <- rbindlist(lapply(results_brands[!checks=='try-error'], calc_sbbe))
    # standardize SBBE by category
    sbbe[,':=' (sbbe_std = (sbbe-mean(sbbe))/sd(sbbe)), by = c('market_id')]
    # merge to elasticities
    elast=merge(elast, sbbe, by = c('market_id', 'brand'),all.x=T)
  
  # extract lagged market share coefficient
  lambdas = rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$model@coefficients[unique(x$elast$index_lagms),]))
  setkey(lambdas, market_id)
  setkey(elast, market_id)
  elast[lambdas, lagged_ms_coef := i.coef]
  elast[is.na(lagged_ms_coef), lagged_ms_coef:=0]
  
  elast[, ':=' (sbbelt = sbbe/(1-lagged_ms_coef))]
  
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
    vars = c('cat_class', 'country_class', 'market_growth', 'herf', 'c3', 'c5')
    tmp = brand_panel[, c('market_id', vars), with=F]
    setkey(tmp, market_id)
    tmp = unique(tmp)
  
    elast = merge(elast, tmp, all.x=T, all.y=F, by = c('market_id'))
    
    # by brand/market_id
    vars = c('overall_ms', 'overall_prindex')
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
  
  return(list(checks=markets, elast=elast, model = model_name))
  })

results <- out

for (i in seq(along=out)) {
 fwrite(out[[i]]$elast, file=paste0('../output/elast_', out[[i]]$model,'.csv'), row.names=F) 
}

sink('../output/elasticities.txt')
cat('Finished building extract_elast.R at:\n')
print(Sys.time())
cat('\n\n(File used to enable track changes by makefile).\n')
sink()

