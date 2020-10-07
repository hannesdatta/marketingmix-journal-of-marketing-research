# load gini income inequality
library(data.table)
library(xlsx)

tmp <- read.xlsx('../../../../Data/other/gini/gini_final.xlsx',1, stringsAsFactors=F)
gini <- tmp[,c('country', 'value')]
setnames(gini, c('country', 'ginicoef'))
fwrite(gini,'../output/gini.csv')

