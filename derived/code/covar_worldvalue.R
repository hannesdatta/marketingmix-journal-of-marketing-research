library(data.table)
library(xlsx)
# worldvalue
worldvalues <- read.xlsx('../../../../data/worldvalue/worldvalue.xlsx', sheetIndex=1)

setnames(worldvalues, c('nation-wave','country','tradrat','survself'))

worldvalues$`nation-wave` <- NULL

fwrite(worldvalues, '../output/worldvalue.csv')
