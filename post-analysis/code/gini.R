# load gini income inequality
library(data.table)
require(xlsx)

tmp <- read.xlsx('../../../../Data/other/gini/gini_final.xlsx',1, stringsAsFactors=F)
gini <- tmp[,c('country', 'value')]
setnames(gini, c('country', 'ginicoef'))
fwrite(gini,'../temp/gini.csv')

