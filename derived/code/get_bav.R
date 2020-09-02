library(xlsx)
library(data.table)
library(stringi)

bav <- read.xlsx('../../../../data/bav/delivery_20200417/Brand_Equity_BAV_Group.xlsx', 1)
bav <- data.table(bav)


bav[, year:=as.numeric(gsub("\\D", "", StudyGroupName))]

bav[grepl('Hong Kong', StudyGroupName), StudyGroupName:=gsub('PRC - ', 'PRC ', StudyGroupName)]

bav[, country := tolower(gsub('[ ][-][ ].*', '', StudyGroupName))]
bav[country=='prc', country:='china']
bav[country=='prc hong kong', country:='hong kong']

#table(bav$country)

# earliest year by country

bav[, list(min_year = min(year), max_year = max(year), n_brands = uniqueN(BrandName)), by = c('country')]

bav[, brand:=tolower(stri_trim(BrandName))]
bav[brand =='dick smith', brand:='dicksmith']
bav[brand == 'fisher & paykel', brand:='fisherpaykel']
bav[brand=='sony ericsson', brand:='sonyericsson']

bav[, N:=.N,by=c('country','brand','year')]
# check they have the unique values
bav[, Nunique:=uniqueN(Brand_Strength_R),by=c('country','brand','year')]

stopifnot(nrow(bav[Nunique>1])==0)

setcolorder(bav, c('country','year','brand'))

bav <- bav[!brand=='amazon'] # the wrong amazon

bav <- bav[, lapply(.SD, mean), by =c ('country','year','brand'), .SDcols=grep('[_]R', colnames(bav), value=T)]


fwrite(bav[, grep('brand$|year|country|[_]R', colnames(bav), value=T),with=F], '../output/bav.csv')
