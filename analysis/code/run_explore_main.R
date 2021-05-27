rm(list = ls())
library(knitr)
library(data.table)

unlink('../audit/*.html')

# Load results
files <- list.files('../output/', pattern = 'results.*[.]RData$', full.names=T)
files <- grep('marketshare',files, invert=T, value=T)

# Extract model names from .RData file
for (fn in files) {
  
  cat(paste0('Loading ', fn, '...\n'))
  print(ls())
  load(file=fn)
  print(ls())
  
  lscall=ls() #envir=.GlobalEnv)
  models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')
  mname <<- models[1]
  
  print(mname) 
  
  if (grepl('nommix|noendogeneity', mname)) next
    
  vifs_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) x$vif))')))
  elast_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) x$elast))')))
  coefs_model <<- eval(parse(text=paste0('rbindlist(lapply(', mname, ', function(x) cbind(unique(x$elast[, c(\'category\',\'country\',\'brand\'),with=F],by=c(\'brand\')), variable=names(x$model$coefficients), coef=x$model$coefficients)))')))
  
  if (nrow(elast_model)>0) {
    elast_model[, z:=elastlt/elastlt_se]
    results_model <<- eval(parse(text=paste0(mname)))
    markdown_title <<- mname
    zval_sig=qnorm(.95)
    rmarkdown::render('explore_main.rmd', output_file=paste0('../audit/model_', mname, '.html'))
  }
  rm(results_model)
  eval(parse(text=paste0('rm(', mname,')')))
}

sink('../temp/explore_main.txt')
cat('done.')
sink()
