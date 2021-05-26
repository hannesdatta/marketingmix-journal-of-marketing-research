# Likelihood ratio tests
library(data.table)
models <- c('ec_main', 'ec_rempr', 'ec_remllen', 'ec_remdst', 'ec_remocpr', 'ec_remocllen', 'ec_remocdst')

for (fn in models) load(paste0('../output/', 'results_', fn, '.RData'))



out = rbindlist(lapply(models, function(mod) { 
  
  res=rbindlist(lapply(eval(parse(text=paste0('results_', mod))), function(x) data.table(x$dt[, c('brand','category','country'),with=F][1],
                                               SSR=sum(x$model$residuals^2),
                                               df = x$model$df.residual,
                                               N = length(x$model$residuals),
                                               sigma = summary(x$model)$sigma)))
  res[, model := mod]
  res
}))

out[, list(.N), by = c('model')]


# overall sum of squares

test_stats = out[, list(N_models = .N, SSR=sum(SSR), df=sum(df), N=sum(N)),by=c('model')]


{
print(test_stats)

# pooled sigma^2: https://en.wikipedia.org/wiki/Pooled_variance

for (.mod in models[-1]) {
  cat(paste0('\nExtra-sum-of-squares test (F-test) for ', .mod, ':\n\n\n'))
  
  SSR_diff = test_stats[model==.mod]$SSR-test_stats[model=='ec_main']$SSR
  df_diff = test_stats[model==.mod]$df-test_stats[model=='ec_main']$df
  
  denominator = test_stats[model=='ec_main']$SSR/test_stats[model=='ec_main']$df
  test_statistic = (SSR_diff/df_diff)/denominator
  #  test_stats[model=='ec_main']$pooled_sigma_square

  df1 = df_diff
  df2 = test_stats[model=='ec_main']$df
  p=pf(test_statistic,df1,df2)

  if(p > .5) p = 1-p
  
  cat(paste0('  Test statistic (F): ', formatC(test_statistic, digits=3)),fill=T)
  cat(paste0('  F(', df1, ', ', df2, ') = ', formatC(p, digits=3), ' (p-value)'),fill=T)
  
}
}

# https://influentialpoints.com/notes/n8rttst.htm
