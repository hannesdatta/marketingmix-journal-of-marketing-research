# load gini income inequality
library(data.table)
library(xlsx)

tmp <- read.xlsx('../../../Data/gini/gini.xlsx',1, stringsAsFactors=F)
gini <- tmp[,c('country', 'value')]
setnames(gini, c('country', 'ginicoef'))
fwrite(gini,'../output/gini.csv')

