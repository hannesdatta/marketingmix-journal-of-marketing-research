#     _____    __   _  __                               _                 _       
#    / ____|  / _| | |/ /       /\                     | |               (_)      
#   | |  __  | |_  | ' /       /  \     _ __     __ _  | |  _   _   ___   _   ___ 
#   | | |_ | |  _| |  <       / /\ \   | '_ \   / _` | | | | | | | / __| | | / __|
#   | |__| | | |   | . \     / ____ \  | | | | | (_| | | | | |_| | \__ \ | | \__ \
#    \_____| |_|   |_|\_\   /_/    \_\ |_| |_|  \__,_| |_|  \__, | |___/ |_| |___/
#                                                            __/ |                
#                                                           |___/                 
#
#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|



### LOAD DATA SETS
require(data.table)

# load data
brand_panel=fread('../temp/preclean.csv')
brand_panel[, ':=' (date = as.Date(date))]

# load results
load('../temp/results_20171113.RData')


get_elast <- function(results_brands) {
  checks <- unlist(lapply(results_brands, class))
  
  cat('error report:\n\n')
  print(table(checks))
  
  # elasticities
  elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))
  
  zval=1.69
  out=elast[!is.na(coef), list(median_elast = median(elast), 
                               w_elast = sum(elast/elast_se)/sum(1/elast_se), 
                               N_brands= .N, 
                               perc_positive = length(which(z>=(zval)))/.N, 
                               perc_null = length(which(abs(z)<zval))/.N, 
                               perc_negative = length(which(z<=(-zval)))/.N), by=c('variable')]
  res=list(checks=checks, elast=out, raw=elast)
  return(res)
}

m1 = get_elast(results_MNL)

fwrite(m1$raw, file='../output/elasticities.csv')

require(foreign)
write.foreign(m1$raw, '../output/elasticities_data.txt', '../output/elasticities_code.sps', package='SPSS')

library(sjstats)

for (i in unique(m1$raw$variable)) {
  cat(paste0('\n//////////////////////////////\n', i, '\n//////////////////////////////\n\n'))
  
  m<-aov(elast~brand+country+category, data=m1$raw, subset=variable==i)
  print(anova_stats(m))
}



