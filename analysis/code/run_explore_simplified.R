rm(list = ls())
library(knitr)
library(data.table)

load('../output/results_simplified.RData')

unlink('../audit/*.html')

models <- grep('results[_]', ls(), value = T)

for (mname in models) {
  print(mname) 
  mname <<- mname
  
  vifs_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) x$vif))')))
  elast_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) x$elast))')))
  if (nrow(elast_model)>0) {
    elast_model[, z:=elastlt/elastlt_se]
    results_model <<- eval(parse(text=paste0(mname)))
    markdown_title <<- mname
    zval_sig=qnorm(.95)
    rmarkdown::render('explore_simple.rmd', output_file=paste0('../audit/model_', mname, '.html'))
    }
}

sink('../temp/explore_simplified.txt')
cat('done.')
sink()
