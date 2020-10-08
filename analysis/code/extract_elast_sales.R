#
#
#
# Extracts elasticities from .RData file
#
#
#
#
rm(list = ls())

# Load packages
library(data.table)

# Load results
load(file = '../output/results_sales.RData')

#unlink('../output/*.csv')

# load panel data
brand_panel=fread('../temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

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
  # extract model types and adf tests
  
  
  adftests=rbindlist(lapply(results_brands[checks=='list'], function(x) data.frame(market_id=as.numeric(x$adf_tests$market_id), variable=as.character(x$adf_tests$variable), brand_id=as.numeric(x$adf_tests$brand_id), ur=as.numeric(x$adf_tests$ur))))
  
  
  modeltypes=rbindlist(lapply(results_brands[checks=='list'], function(x) {
    res=data.table(x$paneldimension[1], model_type=x$m_final_type)
    return(res[, !colnames(res)%in%'date',with=F])
  }))
  
  # extract elasticities
  elast <- rbindlist(lapply(results_brands[checks=='list'], function(x) x$elasticities))
  #setnames(elast, 'varname','variable')
  setkey(elast, brand_id)
  setkey(brand_panel, brand_id)
  elast[brand_panel, ':=' (brand=i.brand, country = i.country, category = i.category, market_id=i.market_id)]
  
  # merge model types and UR tests
    # by variable
    setkey(adftests, market_id, brand_id, variable)
    setkey(elast, market_id, brand_id, varname)
    elast[adftests, ur_focalmmix:=i.ur]
    # by DV
    setkey(adftests, market_id, brand_id)
    setkey(elast, market_id, brand_id)
    elast[adftests[grepl('lnusales',variable)], ur_dv:=i.ur]
    
    # model types
    setkey(modeltypes, market_id, brand_id)
    setkey(elast, market_id, brand_id)
    elast[modeltypes, modeltype:=as.factor(i.model_type)]
    
  summary(elast$ur_focalmmix)
  summary(elast$ur_dv)
  summary(elast$modeltype)
  
  elast[, dekimpe_classification:='']
  elast[ur_dv==0&ur_focalmmix == 0, dekimpe_classification:='businessasusual(dv0,mmix0)']
  elast[ur_dv==1&ur_focalmmix == 0, dekimpe_classification:='hysteresis(dv1,mmix0)']
  elast[ur_dv==0&ur_focalmmix == 1, dekimpe_classification:='escalation(dv0,mmix1)']
  elast[ur_dv==1&ur_focalmmix == 1, dekimpe_classification:='evolving(dv1,mmix1)']
  
  # region-of-origin
  #elast[!is.na(country_of_origin), region_of_origin := 'asian']
  #elast[country_of_origin%in%c('finland','france', 'germany','italy','netherlands','sweden'), region_of_origin := 'europe']
  #elast[globalbrand==T & country_of_origin%in%c('canada','usa'), region_of_origin := 'northamerica']
  elast[, varname:=gsub('^ln', '', varname)]
    
  return(list(checks=checks, elast=elast, model = model_name))
  })

results <- out

for (i in seq(along=out)) {
 fwrite(out[[i]]$elast, file=paste0('../output/elast_', out[[i]]$model,'.csv'), row.names=F) 
}

sink('../output/elasticities_sales.txt')
cat('Finished building extract_elast.R at:\n')
print(Sys.time())
for (i in seq(along=out)) {
  cat(out[[i]]$model,fill=T)
print(table(ifelse(out[[i]]$checks=='list','models converged','error in estimation procedure')))
}

cat('\n\n(File used to enable track changes by makefile).\n')
sink()

