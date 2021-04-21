# Investigate SKU entry and exit

load(file='..\\temp\\uniqueness_and_lagsales.RData')
library(data.table)
skus_by_date_list[[1]]

tmp=rbindlist(lapply(skus_by_date_list, function(x) x[, list(first_date=min(first_date),
                                               last_date=max(last_date)),
                                        by=c('category','country','brand','model')]))

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

tmp[, N_months:=elapsed_months(last_date, first_date)]

tmp=tmp[N_months!=0]

hist(tmp$N_months)
