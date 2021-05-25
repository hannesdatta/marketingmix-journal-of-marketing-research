# check %-significance for npublic holidays variable

load('../../analysis/output/results_ec_main_sur.RData')

out=lapply(results_ec_main_sur, function(x) data.table(variable=names(x$model$coefficients),
                                                       summary(x$model)$coefficients,
           x$elast[, c('category','country','brand'),with=F][1]))
                                                   
coefs =rbindlist(out)
setnames(coefs, c('variable','est','se','t','p','category','country','brand'))
coefs[grepl('holiday', variable)][, list(pos_sig=length(which(p<=.10&est>0)),
                                         neg_sig=length(which(p<=.10&est<0)),
                                         ns = length(which(p>.10)))]

