rm(list = ls())
library(knitr)
library(data.table)

dir.create('../audit/')
unlink('../audit/*.html')

# Load results
files <- list.files('../output/', pattern = 'results.*[.]RData$', full.names=T)
files <- grep('marketshare',files, invert=T, value=T)

# Extract model names from .RData file
out <- lapply(files, function(fn) {
  cat(paste0('Loading ', fn, '...\n'))
  load(file=fn)
  
  lscall=ls() #envir=.GlobalEnv)
  models <- setdiff(c(grep('results[_]', lscall, value=T)),'results_brands')
  mname <<- models[1]
  
  print(mname) 
  
  
  check = data.frame(eval(parse(text=paste0('table(unlist(lapply(', mname, ', class)))'))))
  check$model <- mname
  rm(results_model)
  eval(parse(text=paste0('rm(', mname,')')))
  return(check)
})

out2 = rbindlist(out)

stopifnot(!any(out2$Var1=='try-error'))

sink('../output/audit_main.txt')
cat("Estimated models:\n================================\n\n")

print(out2)

cat('\n\n')
sink()
