library(data.table)
library(xlsx)

read <- function(i) {
  fn='../../../../data/wgi/wgidataset.xlsx'
  
  meta <- read.xlsx(fn,i, stringsAsFactors=F,startRow=1, endRow=17, header=F)
  
  yr_row <- sapply(11:20, function(r) grepl('^[0-9]{4}$',meta[r, 3]))
  yr_row = c(11:20)[which(yr_row==T)[1]]
  
  dat <-read.xlsx(fn,i, stringsAsFactors=F,startRow=yr_row+2, header=F)
  measure_name=meta[1,1]
  cols <- sapply(3:ncol(meta), function(r) tolower(paste0(meta[seq(from=yr_row, length.out=2),r], collapse='_')))
  colnames(dat) <- c('country','country_code', cols)
  dat$measure <- measure_name
  
  tmp <- melt(data.table(dat), id.vars=c('country','country_code','measure'))
  tmp[value=='#N/A', value:=NA]
  tmp[, value:=as.numeric(value)]
  tmp[, year:= as.numeric(gsub('[_].*', '', variable))]
  tmp[, metric:= gsub('[0-9]{4}[_]', '', variable)]
  return(tmp)
}

library(parallel)
wgi <- lapply(2:7, function(i) {
  print(i)
  cl <- makePSOCKcluster(1)
  clusterExport(cl, c('read'))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(xlsx))
  dat <- eval(parse(text=paste0('clusterEvalQ(cl, read(', i, '))')))
  stopCluster(cl)
  return(dat[[1]])
})

wgi <- rbindlist(wgi)



tmp <-wgi[grepl('austral|india|indonesia|philipp|thail|taiwan|^china|hong|zealand|japa|korea, rep|malaysia|singa|vietna', wgi$country, ignore.case=T)]
tmp <- tmp[year%in%2004:2014]

tmp[grepl('voice', measure, ignore.case=T), measure2:='wgi_accountability']
tmp[grepl('corrupt', measure, ignore.case=T), measure2:='wgi_corruptionctrl']
tmp[grepl('effectiv', measure, ignore.case=T), measure2:='wgi_governmenteffectiveness']
tmp[grepl('stability', measure, ignore.case=T), measure2:='wgi_stability']
tmp[grepl('regulat', measure, ignore.case=T), measure2:='wgi_regulatoryqual']
tmp[grepl('law', measure, ignore.case=T), measure2:='wgi_ruleoflaw']

tmp <- dcast(tmp[metric=='estimate'], country+country_code+year~measure2)

tmp[, country:=tolower(country)]
tmp[grepl('hong', country), country:='hong kong']
tmp[grepl('korea', country), country:='south korea']
tmp[grepl('taiwa', country), country:='taiwan']
#tmp[grepl('hong', country), country:='hong kong']
#tmp[grepl('hong', country), country:='hong kong']

stopifnot(length(unique(tmp$country))==14)

fwrite(tmp, '../output/wgi.csv')
