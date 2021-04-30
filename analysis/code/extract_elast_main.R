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
load(file = c('../output/results_main.RData'))

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
  
  if (nrow(rbindlist(lapply(results_brands, function(x) x$elast)))==0) {
    elast=NULL} else {
      # extract elasticities
      elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
      
      setkey(brand_panel, category, country, brand)
      setkey(elast, category, country, brand)
      elast[brand_panel, ':=' (selection_obs48=i.obs48, selection_brands=!grepl('allothers|^super|^amazon',brand,ignore.case=T))]
      
    }
  
  # get predictions
  preds <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$predictions))
  
  # get predictions
  predsk <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$predictions_kfold))
  
  #markets=data.table(market_id=analysis_markets)[, ':=' (i=1:.N, available=!checks=='try-error')]
  
  
  return(list(model = model_name, checks=checks, elast=elast, predictions=preds,
predictions_kfold = predsk))
  })

results <- out

for (i in seq(along=out)) {
  if (!is.null(out[[i]]$elast)) fwrite(out[[i]]$elast, file=paste0('../output/elast_', out[[i]]$model,'.csv'), row.names=F) 
  fwrite(out[[i]]$predictions, file=paste0('../output/predictions-within_', out[[i]]$model,'.csv'), row.names=F) 
  fwrite(out[[i]]$predictions_kfold, file=paste0('../output/predictions-kfold_', out[[i]]$model,'.csv'), row.names=F) 
  
}

sink('../output/elasticities_main.txt')
cat('Finished building extract_elast.R at:\n')
print(Sys.time())
for (i in seq(along=out)) {
  cat(out[[i]]$model,fill=T)
print(table(ifelse(out[[i]]$checks=='list','models converged','error in estimation procedure')))
}

cat('\n\n(File used to enable track changes by makefile).\n')
sink()

