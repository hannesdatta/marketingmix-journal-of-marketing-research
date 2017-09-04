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

# load "unselected" brands
load('../../derived/temp/select.RData')

brand_selection[, country:=as.character(tolower(country))]
brand_selection[, brand:=as.character(tolower(brand))]
brand_selection[, category:=as.character(tolower(category))]

nonselected = brand_selection[selected_brand==F & brand%in%unique(dt$brand)][, c('category', 'country', 'brand'),with=F][, present:=.5]

dt = rbind(dt, nonselected, fill = T)

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
  step1[, Ncountries:=rowSums(step1[,-1, with=F]==1)]
  setcolorder(step1, c('brand', 'Ncountries', unique(x$country)[order(unique(x$country))]))
  step2 = merge(mbrands, step1, by = c('brand'), all.x=T)
  return(set_zeros(step2))
})


for (i in 1:length(presence)) {
  write.xlsx(presence[[i]][!brand=='EMPTY'], file = '../output/descr_presence.xlsx', showNA=FALSE, sheetName = names(presence)[i], row.names=FALSE, append = ifelse(i==1, F, T))
  
}


share = lapply(split(dt, dt$category), function (x) {
  step1 = dcast(x, brand ~ country, value.var='ms_bar', fill = '')
  step1[, Ncountries:=rowSums(step1[,-1, with=F])]
  setcolorder(step1, c('brand', 'Ncountries', unique(x$country)[order(unique(x$country))]))
  step2 = merge(mbrands, step1, by = c('brand'), all.x=T)
  return((step2))
})


for (i in 1:length(share)) {
  write.xlsx(share[[i]][!brand=='EMPTY'], file = '../output/descr_marketshare.xlsx', showNA=FALSE, sheetName = names(share)[i], row.names=FALSE, append = ifelse(i==1, F, T))
  
}

# table by categories in columns, in how many countries present

# determine set of brands active in more than one country
tmp = brand_panel[, list(N=length(unique(country))), by = c('brand', 'category')]
tmp = tmp[brand%in% mbrands$brand]

tmp = dcast(tmp, brand ~ category, value.var='N')

write.xlsx(tmp, file = '../output/descr_countries.xlsx', showNA=FALSE, sheetName = 'Ncountries_by_category', row.names=FALSE, append = F)


######## get the results

# Load results
#load(file = c('../temp/results_20170822.RData')) # still gotta fix the mistakes here!!! e.g market 55.
load(file = c('../temp/results_20170731.RData'))


# identify model crashes
results_brands <- results_MNL

checks <- unlist(lapply(results_brands, class))
table(checks)
last.item = length(analysis_markets)

# load data
brand_panel=fread('../temp/preclean.csv')
brand_panel[, ':=' (date = as.Date(date))]

# elasticities
elast <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$elast))

# sbbe + lag ms
sbbe <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) cbind(x$model@coefficients, market_id =  unique(x$specs$market_id))))
sbbe <- sbbe[grepl('[_]dum|lag', variable)]
sbbe = sbbe[, c('market_id', 'brand', 'coef', 'z','variable'),with=F]
sbbe[, variable:=ifelse(grepl('[_]dum', variable), '_intercept', '_lagms')]

setnames(sbbe, 'coef', 'value')

elast[, z:=elast/elast_se]

elast[, sig := ifelse(abs(z)>1.69,1,0)]
elast[!variable=='rwpspr' & elast>0, facevalid := 1]
elast[!variable=='rwpspr' & !elast>0, facevalid := 0]
elast[variable=='rwpspr' & elast>0, facevalid := 0]
elast[variable=='rwpspr' & !elast>0, facevalid := 1]

elast[!is.na(elast), list(face_valid_sig = sum(facevalid * sig)/.N, 
                                       not_facevalid_sig = sum((1-facevalid) * (sig))/.N,
                                       insig = sum(1-sig)/.N), by = c('category')]



elast_tmp = elast[, c('brand', 'market_id', 'variable', 'mean_var', 'elast', 'z')]
setnames(elast_tmp, 'variable', 'var')
elast_melted=melt(elast_tmp, id.vars= c('brand', 'market_id', 'var'))
elast_melted = merge(elast_melted[!variable=='z'], elast_melted[variable=='z', c('brand', 'market_id', 'var', 'value'),with=F], by = c('brand', 'market_id', 'var'))
setnames(elast_melted, 'value.x', 'value')
setnames(elast_melted, 'value.y', 'z')
elast_melted[variable=='mean_var', z:=NA]


elast_melted[variable=='mean_var', variable := 'level']
elast_melted[, variable:=paste0(var, '_', variable)]

elast_melted[, var:=NULL]
elast_melted=elast_melted[, colnames(sbbe), with=F]

dt = rbind(sbbe, elast_melted)
markets = brand_panel[, list(N=.N), by = c('market_id', 'category', 'country')][, N:=NULL]
dt = merge(markets, dt, all=T, by = c('market_id'))



for (i in unique(dt$country)) {
  for (j in unique(dt$category)) {
    dt=rbind(dt, cbind(brand='EMPTY', country = i, category=j), fill=T)
  }
}

varoutput = lapply(split(dt, dt$category), function (x) {
  step1 = dcast(data.table(x), brand + variable ~ country, value.var='value')
  setnames(step1, 'variable', 'metric')
  step1 = step1[!brand=='EMPTY']
  
  x[, sig:=ifelse(abs(z)>1.69,1,0)]
  
  sig = dcast(data.table(x), brand + variable ~ country, value.var='sig')
  setnames(sig, 'variable', 'metric')
  sig = sig[!brand=='EMPTY']
  
  return(list(coefs = step1, sig = sig))
})


wb <- createWorkbook()


for (i in 1:length(varoutput)) {
  print(i)
#  wb <- createWorkbook()
  sheet  <- createSheet(wb, sheetName = names(varoutput)[i])
  #addDataFrame(varoutput[[i]]$coefs, sheet, row.names=FALSE, showNA=FALSE)
  
 # saveWorkbook(wb, file = '../output/descr_coefelast.xlsx')
  
  cs <- CellStyle(wb) + DataFormat("#,##0.00")
  
  cb <- CellBlock(sheet, 1, 1, 2500, 20)
  CB.setRowData(cb, colnames(varoutput[[i]]$coefs),1,showNA=FALSE)
  
  CB.setMatrixData(cb, as.matrix(varoutput[[i]]$coefs[,1:2]),2,1,showNA=FALSE)
  CB.setMatrixData(cb, as.matrix((varoutput[[i]]$coefs[,-c(1:2),with=F])),2,3,showNA=TRUE)
  
  
  tmp=data.frame(varoutput[[i]]$sig)
  tmp[tmp==0]<-NA
  
  ind  <- which(tmp == 1, arr.ind=TRUE)
  fill <- Fill(backgroundColor="red")
  CB.setFont(cb, Font(wb, isBold=TRUE), ind[,1]+1,ind[,2])
  
   # write.xlsx(varoutput[[i]][!brand=='EMPTY'], , showNA=FALSE, row.names=FALSE, append = ifelse(i==1, F, T))
  #saveWorkbook(wb, file = paste0('../output/descr_coefelast_',  names(varoutput)[i], '.xlsx'))
  #rm(wb)
  rm(cb)
  rm(sheet)
  Sys.sleep(1)
}
saveWorkbook(wb, file = paste0('../output/descr_coefelast.xlsx'))
