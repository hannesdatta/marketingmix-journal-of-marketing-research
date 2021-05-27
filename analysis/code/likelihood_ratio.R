##########################
# Likelihood ratio tests #
##########################

# Load models
library(data.table)
models <- c('ec_main_nc', 'ec_noocmmixnc', 
            'ec_main', 'ec_remocprnc', 'ec_remocllennc', 'ec_remocdstnc', 
            'ec_remprnc', 'ec_remllennc', 'ec_remdstnc', 
            'ec_onlyocllennc', 'ec_onlyocdstnc',
            'ec_onlyocprnc')
for (fn in models) load(paste0('../output/', 'results_', fn, '.RData'))

# Retrieve necessary information from estimated models
out = rbindlist(lapply(models, function(mod) { 
  
  res=rbindlist(lapply(eval(parse(text=paste0('results_', mod))), function(x) data.table(x$dt[, c('brand','category','country'),with=F][1],
                                               SSR=sum(x$model$residuals^2),
                                               df = x$model$df.residual,
                                               N = length(x$model$residuals),
                                               sigma = summary(x$model)$sigma)))
  res[, model := mod]
  res
}))

# Aggregate over model types
test_stats = out[, list(N_models = .N, SSR=sum(SSR), df=sum(df), N=sum(N)),by=c('model')]

# Check number of estimated models
stopifnot(all(test_stats$N_models==1619))

print(test_stats)

# Test significance


# https://influentialpoints.com/notes/n8rttst.htm
f_test <- function(full, restricted) {
  
    cat(paste0('\nExtra-sum-of-squares test (F-test) for: \n'))
    cat(paste0('  Restricted model: ', restricted),fill=T)
    cat(paste0('  Full model: ', full, '\n\n\n'))
    
    SSR_diff = test_stats[model==restricted]$SSR-test_stats[model==full]$SSR
    df_diff = test_stats[model==restricted]$df-test_stats[model==full]$df
    
    denominator = test_stats[model=='ec_main']$SSR/test_stats[model==full]$df
    test_statistic = (SSR_diff/df_diff)/denominator
    
    df1 = df_diff
    df2 = test_stats[model==full]$df
    p=pf(test_statistic,df1,df2)
  
    if(p > .5) p = 1-p
    
    cat(paste0('  Test statistic (F): ', formatC(test_statistic, digits=3)),fill=T)
    cat(paste0('  F(', df1, ', ', df2, ') = ', formatC(p, digits=3), ' (p-value)'),fill=T)
    
  }

test_stats


sink('../output/likelihood-tests.txt')

print(test_stats)
cat('\n\n\n')
cat('1) Significance of *removing* focal and competitive marketing-mix [all models without Copula terms]', fill=T)
f_test(full = 'ec_main_nc', restricted = c('ec_remocllennc'))
f_test(full = 'ec_main_nc', restricted = c('ec_remocprnc'))
f_test(full = 'ec_main_nc', restricted = c('ec_remocdstnc'))

cat('\n\n\n')
cat('2) Significance of *adding* focal and competitive marketing-mix to a model WITHOUT marketing mix instruments [all without Copula terms]', fill=T)
f_test(full = 'ec_onlyocllennc', restricted = c('ec_noocmmixnc'))
f_test(full = 'ec_onlyocprnc', restricted = c('ec_noocmmixnc'))
f_test(full = 'ec_onlyocdstnc', restricted = c('ec_noocmmixnc'))

cat('\n\n\n')

cat('3) Significance of *removing* focal marketing-mix [all models without Copula terms]', fill=T)
f_test(full = 'ec_main_nc', restricted = c('ec_remllennc'))
f_test(full = 'ec_main_nc', restricted = c('ec_remprnc'))
f_test(full = 'ec_main_nc', restricted = c('ec_remdstnc'))

sink()
