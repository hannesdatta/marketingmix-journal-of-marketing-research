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

# determine set of brands active in more than one country
tmp = brand_panel[, list(N=.N), by = c('brand', 'country')]

tmp[, Ncountries := .N, by = c('brand')]
tmp[, N:=NULL]
tmp[, present := T]

tmp = dcast(tmp, brand + Ncountries ~ country, value.var='present')

mbrands <- tmp[Ncountries>1][, c('brand'), with=F]

dt = brand_panel[, list(N=.N, monthly_ms = mean(usalessh), sum_sales = sum(usales)), by = c('brand', 'country', 'category')]
dt[, ms_bar := sum_sales/sum(sum_sales), by = c('country', 'category')]


for (i in unique(dt$country)) {
  for (j in unique(dt$category)) {
    dt=rbind(dt, cbind(brand='EMPTY', country = i, category=j), fill=T)
  }
}
dt[, present:=1]

#	https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
set_zeros <- function(DT) {
  # either of the following for loops
  
  # by name :
  for (j in names(DT))
    set(DT,which(is.na(DT[[j]])),j,0)
  return(DT)
}


library(xlsx)

presence = lapply(split(dt, dt$category), function (x) {
  step1 = dcast(x, brand ~ country, value.var='present', fill = 0)
  step1[, Ncountries:=rowSums(step1[,-1, with=F])]
  setcolorder(step1, c('brand', 'Ncountries', unique(x$country)[order(unique(x$country))]))
  step2 = merge(mbrands, step1, by = c('brand'), all.x=T)
  return(clean_zeros(step2))
})


for (i in 1:length(out)) {
  write.xlsx(presence[[i]][!brand=='EMPTY'], file = '../output/descr_presence.xlsx', showNA=FALSE, sheetName = names(presence)[i], row.names=FALSE, append = ifelse(i==1, F, T))
  
}


share = lapply(split(dt, dt$category), function (x) {
  step1 = dcast(x, brand ~ country, value.var='ms_bar', fill = '')
  step1[, Ncountries:=rowSums(step1[,-1, with=F])]
  setcolorder(step1, c('brand', 'Ncountries', unique(x$country)[order(unique(x$country))]))
  step2 = merge(mbrands, step1, by = c('brand'), all.x=T)
  return((step2))
})


for (i in 1:length(out)) {
  write.xlsx(share[[i]][!brand=='EMPTY'], file = '../output/descr_marketshare.xlsx', showNA=FALSE, sheetName = names(share)[i], row.names=FALSE, append = ifelse(i==1, F, T))
  
}

# table by categories in columns, in how many countries present

# determine set of brands active in more than one country
tmp = brand_panel[, list(N=length(unique(country))), by = c('brand', 'category')]
tmp = tmp[brand%in% mbrands$brand]

tmp = dcast(tmp, brand ~ category, value.var='N')

write.xlsx(tmp, file = '../output/descr_countries.xlsx', showNA=FALSE, sheetName = 'Ncountries_by_category', row.names=FALSE, append = F)

