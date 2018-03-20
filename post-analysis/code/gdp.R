library(data.table)

load('../../derived/temp/gdppercap.RData') # keep 2010
gdppercap=gdppercap[year==2010]

setnames(gdppercap, 'gdppercap', 'gdppercap2010')
fwrite(gdppercap, '../temp/gdp.csv')
