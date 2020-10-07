library(data.table)

# rule of law
ruleoflaw <- fread('../../../../data/worldbank/4742d3fa-a9ea-41fb-b562-c0378a6bec56_Data.csv')
ruleoflaw[, country:=gsub('[,].*', '', tolower(`Country Name`))]
ruleoflaw[, country:=gsub('[ ]sar$', '', country)]
ruleoflaw[country=='korea', country:='south korea']
ruleoflaw[, ruleoflaw:=`2010 [YR2010]`]
ruleoflaw <- ruleoflaw[!is.na(ruleoflaw)]

ruleoflaw <- ruleoflaw[, c('country','ruleoflaw'),with=F]

fwrite(ruleoflaw, '../output/ruleoflaw.csv')
