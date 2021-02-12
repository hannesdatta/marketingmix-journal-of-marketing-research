rm(list=ls())


# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)
library(shiny)


brand_panel=fread('../externals/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load auxilary functions
source('proc_auxilary.R')
source('proc_rename.R')

files = list.files('../externals/', pattern = '^elast[_].*[.]csv$', full.names=T)
names(files) = gsub('[.]csv$', '', gsub('.*elast[_]results[_]', '', files))

elast_list <- lapply(files, function(fn) {
  tmp = fread(fn)
  tmp[, w_elastlt := (1/elastlt_se)/sum(1/elastlt_se), by = c('variable')]
  setkey(tmp, category, country, brand)
  return(tmp)
})

sbbe = copy(elast_list$marketshare)

elast_list = lapply(elast_list, function(elast) {
  #if (!any(grepl('sbbe[_]', colnames(elast)))) {
    elast[sbbe, sbbe_round1:=i.sbbe_std]
    elast[!is.na(elastlt), sbbe_round1_mc := sbbe_round1-mean(sbbe_round1,na.rm=T),by=c('variable')]
  #}
  return(elast)
})

# Load covariates from panel data set

files = list.files('../externals/', pattern = '^predictions-within[_].*[.]csv$', full.names=T)
names(files) = gsub('[.]csv$', '', gsub('.*predictions-within[_]results[_]', '', files))

predictions <- rbindlist(lapply(seq(along=files), function(f) {
  pr <- fread(files[f])
  pr[, type:=names(files)[f]]
  setcolorder(pr, 'type')
  return(pr)
  
}), fill = T)

files = list.files('../externals/', pattern = '^predictions-kfold[_].*[.]csv$', full.names=T)
names(files) = gsub('[.]csv$', '', gsub('.*predictions-kfold[_]results[_]', '', files))

predictions_kfold <- rbindlist(lapply(seq(along=files), function(f) {
  pr <- fread(files[f])
  pr[, type:=names(files)[f]]
  setcolorder(pr, 'type')
  return(pr)
  
}), fill = T)

merge_covar <- function(dt){
  fns <- list.files('../output/',pattern='covariates.*csv', full.names = T)
  
  for (fn in fns) {
    tmp <- fread(fn)
    aggkey = unlist(strsplit(gsub('[.]csv', '', rev(strsplit(fn,'_')[[1]])[1]), '[-]'))
    
    setkeyv(tmp, aggkey)
    setkeyv(dt, aggkey)
    dt <- merge(dt, tmp, all.x=T, all.y=F)
    
    added_vars <- setdiff(colnames(tmp), aggkey)
    for (.v in added_vars) {
      if (!class(unlist(dt[,.v,with=F]))=='character') dt[!is.na(elastlt), paste0(.v,'_mc'):=(get(.v)-mean(get(.v),na.rm=T)),by=c('variable')]
    }
    
  }
  return(dt)
    
}


elasticities=lapply(elast_list, merge_covar)



setkey(elasticities$ec_main_sur, category,country,brand)
setkey(elasticities$marketshare, category,country,brand)
elasticities$marketshare[elasticities$ec_main_sur, brand_id:=i.brand_id]


save(elasticities, predictions, predictions_kfold, file= 'app/app_workspace.RData')
