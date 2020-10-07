library(data.table)
library(xlsx)

bav <- fread('../output/bav.csv')

# try to match w/ our list of brands

our_brands <- fread('../output/datasets_main.csv', select = c('brand', 'country','category', 'market_id', 'selected', 'usales'))
our_brands = our_brands[selected==T, list(sum_sales=sum(usales, na.rm=T)),by=c('brand','country','category')]
our_brands[, sales_share:=sum_sales/sum(sum_sales), by = c('category','country')]

# try to match
setkey(bav, brand)
setkey(our_brands, brand)


bav_list = bav[, list(.N),by=c('brand')]
bav_list[our_brands, match:=i.brand]

tmp=unique(our_brands, by='brand')[unique(bav, by=c('brand'))]


# assert all brands have been matched
stopifnot(nrow(bav_list[is.na(match)])==0)

# do overview
setkey(our_brands, brand, country)
setkey(bav, brand, country)

our_brands[bav, has_bav := !is.na(i.brand)]

unique(bav$brand)[!unique(bav$brand)%in%our_brands[has_bav==T]$brand]


setorderv(our_brands, c('category','country','sum_sales'), order=c(1,1,-1))
our_brands[, sales_rank:=1:.N, by = c('category','country')]
our_brands[is.na(has_bav), has_bav:=F]

our_brands[, markets_with_data:=any(has_bav==T), by=c('category','country')]
# overall share in terms of usales
with(our_brands, sum(sum_sales[has_bav==T])/sum(sum_sales))
# --> 70.6% market share.
with(our_brands[markets_with_data==T], sum(sum_sales[has_bav==T])/sum(sum_sales))

# N unique
length(unique(our_brands[has_bav==T]$brand))

# namrkets
nrow(our_brands[markets_with_data==T, list(.N), by = c('category','country')])





# category
# no bav brands versus total

tmp=our_brands[, list(total_brands=.N, total_bav_brands=sum(has_bav,na.rm=T), total_bav_brands_top5=sum(has_bav[sales_rank<=5],na.rm=T)),by=c('category','country')]
tmp[, ':=' (share_total=total_bav_brands/total_brands, share_top5 = total_bav_brands_top5 / min(5, total_brands)),by=c('category','country')]

setorder(tmp, share_total)

write.xlsx(tmp, '../audit/bav.xlsx', sheetName = 'market_overview')

# OVERVIEW

tmp2 = dcast(bav, country+brand~year, value.var=c('Brand_Strength_R'))

write.xlsx(tmp2, '../audit/bav.xlsx', sheetName = 'brand_strength', append=T)

# match brands with our data?