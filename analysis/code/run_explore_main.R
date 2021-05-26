rm(list = ls())
library(knitr)
library(data.table)

unlink('../audit/*.html')

# Load results
files <- list.files('../output/', pattern = 'results.*[.]RData$', full.names=T)
files <- grep('marketshare',files, invert=T, value=T)

for (fn in files) {
  cat(paste0('Loading ', fn, '...\n'))
  load(file=fn)
}


models <- grep('results[_]', ls(), value = T)

for (mname in models) {
  print(mname) 
  mname <<- mname
  
  vifs_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) x$vif))')))
  elast_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) x$elast))')))
  coefs_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) cbind(unique(x$elast[, c(\'category\',\'country\',\'brand\'),with=F],by=c(\'brand\')), variable=names(x$model$coefficients), coef=x$model$coefficients)))')))
  if (nrow(elast_model)>0) {
    elast_model[, z:=elastlt/elastlt_se]
    results_model <<- eval(parse(text=paste0(mname)))
    markdown_title <<- mname
    zval_sig=qnorm(.95)
    if (!grepl('nommix|noendogeneity', mname)) rmarkdown::render('explore_main.rmd', output_file=paste0('../audit/model_', mname, '.html'))
    }
}

sink('../temp/explore_main.txt')
cat('done.')
sink()
