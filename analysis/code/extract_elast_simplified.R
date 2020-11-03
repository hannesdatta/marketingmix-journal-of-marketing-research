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
load(file = c('../output/results_simplified.RData'))

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
  checks <- unlist(lapply(results_brands, class))
  
  #markets=data.table(market_id=analysis_markets)[, ':=' (i=1:.N, available=!checks=='try-error')]
  
  # extract elasticities
  elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
  
  return(list(checks=checks, elast=elast, model = model_name))
  })

results <- out

for (i in seq(along=out)) {
 fwrite(out[[i]]$elast, file=paste0('../output/elast_', out[[i]]$model,'.csv'), row.names=F) 
}

sink('../output/elasticities_simplified.txt')
cat('Finished building extract_elast.R at:\n')
print(Sys.time())
for (i in seq(along=out)) {
  cat(out[[i]]$model,fill=T)
print(table(ifelse(out[[i]]$checks=='list','models converged','error in estimation procedure')))
}

cat('\n\n(File used to enable track changes by makefile).\n')
sink()

