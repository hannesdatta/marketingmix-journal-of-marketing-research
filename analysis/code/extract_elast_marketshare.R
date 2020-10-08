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
load(file = c('../output/results_marketshare.RData'))

unlink('../output/*.csv')

# load panel data
brand_panel=fread('../temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Extract model names from .RData file
lscall=ls(envir=.GlobalEnv)
models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')

# load code to calculate SBBE
source('sbbe.R')

# Extract elasticities
out = lapply(models, function(model_name) {
  print(model_name)
  results_brands=eval(parse(text=model_name))

  # identify model crashes
  checks <- unlist(lapply(results_brands, class))
  
  #markets=data.table(market_id=analysis_markets)[, ':=' (i=1:.N, available=!checks=='try-error')]
  
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

