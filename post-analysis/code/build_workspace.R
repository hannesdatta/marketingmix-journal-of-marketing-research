rm(list=ls())


# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)

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

fwrite(predictions, '../output/predictions_within.csv')
rm(predictions)

files = list.files('../externals/', pattern = '^predictions-kfold[_].*[.]csv$', full.names=T)
names(files) = gsub('[.]csv$', '', gsub('.*predictions-kfold[_]results[_]', '', files))

predictions_kfold <- rbindlist(lapply(seq(along=files), function(f) {
  pr <- fread(files[f])
  pr[, type:=names(files)[f]]
  setcolorder(pr, 'type')
  #to_factor = c('date|category|country|brand|type')
  #for (.v in grep(to_factor, colnames(pr),value=T)) pr[, (.v):=as.factor(get(.v))]
  return(pr)
  
}), fill = T)

fwrite(predictions_kfold, '../output/predictions_kfold.csv')
rm(predictions_kfold)

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


elasticities <- elast_list

# merge covariates
elast_w_covariates = c('ec_main', 'ec_main_sur','ec_main_w_novelty', 'ec_main_w_novelty_sur')

elasticities2=lapply(elast_list[elast_w_covariates], merge_covar)

elasticities[elast_w_covariates] <- elasticities2[elast_w_covariates]

# carry over brand IDs to market share data set
setkey(elasticities$ec_main_sur, category,country,brand)
setkey(elasticities$marketshare, category,country,brand)
elasticities$marketshare[elasticities$ec_main_sur, brand_id:=i.brand_id]

# select elasticities for reporting
  
  
  # Select final result set  
  elast <- elasticities$ec_main_sur 
  
  # correct brand classification
  
  # videocon is from india, not from ger
  elast[brand=='videocon']$`brand_from_jp-us-ch-ge-sw`=0
  # YOshii is from Japan
  elast[grepl('yoshii', brand)]$`brand_from_jp-us-ch-ge-sw`=1
  # Simpson is from Australia
  elast[grepl('simpson', brand)]$`brand_from_jp-us-ch-ge-sw`=0
  
  # videocon is from india, not from ger
  elast[brand=='videocon']$western_brand = 0
  # YOshii is from Japan
  elast[grepl('yoshii', brand)]$western_brand=0
  # Simpson is from Australia
  elast[grepl('simpson', brand)]$western_brand=1
  
  # set order of variables to appear in figures and tables
  ordered_vars =  c('llen', 'rwpspr', 'wpswdst')
  ordered_vars = ordered_vars[which(ordered_vars%in%elast$variable)]
  
  names(ordered_vars) <- paste0(unlist(sanitize_table(data.frame(gsub('^ln', '', ordered_vars)))), ' elasticity')
  
  # Notes for tables
  notes_sig = 'Significance levels: \\* *p*<.1, \\*\\* *p*<.05, \\*\\*\\* *p*<.01 (two-sided).'
  estimnote = paste0('Elasticities are weighted by inverse standard errors.')
  
  # Significance levels
  sigvalue = .1
  zval = qnorm(1-sigvalue/2)
  
  # Compute weights and z-values for estimated short- and long-term elasticities
  for (.var in c('elast', 'elastlt')) {
    elast[!is.na(get(.var)), paste0('w_', .var) := 1/get(paste0(.var, '_se'))]
    # rescale
    elast[!is.na(get(.var)), paste0('w_', .var) := get(paste0('w_', .var))/max(get(paste0('w_', .var)))]
    elast[!is.na(get(.var)), paste0('z_', .var) := get(.var)/get(paste0(.var, '_se'))]
  }

save(elasticities, elast, file= '../output/workspace.RData')
