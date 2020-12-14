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
  #if ('sbbe' %in% colnames(tmp)) {
  #  tmp[, sbbe_round1:=sbbe]
  #  tmp[!is.na(elastlt), sbbe_round1_mc := sbbe_round1-mean(sbbe_round1,na.rm=T),by=c('variable')]
  #  }
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

### Auxilary functions

estim_models <- function(models) {
  lapply(seq(along=models), function(i) {
    print(i)
    if (grepl('[|]', as.character(models[[i]])[3])) {
      m1 <- lmer(update.formula(elastlt~1, models[[i]]),
                 data=elast[grep('pr',variable)], weights=w_elastlt,
                 control = lmerctrl, REML=F)
      m2 <- lmer(update.formula(elastlt~1, models[[i]]),
                 data=elast[grep('llen',variable)], weights=w_elastlt,
                 control = lmerctrl, REML=F)
      m3 <- lmer(update.formula(elastlt~1, models[[i]]),
                 data=elast[grep('dst',variable)], weights=w_elastlt,
                 control = lmerctrl, REML=F)
    } else {
      m1 <- lm(update.formula(elastlt~1, models[[i]]),
               data=elast[grep('pr',variable)], weights=w_elastlt)
      m2 <- lm(update.formula(elastlt~1, models[[i]]),
               data=elast[grep('llen',variable)], weights=w_elastlt)
      m3 <- lm(update.formula(elastlt~1, models[[i]]),
               data=elast[grep('dst',variable)], weights=w_elastlt)
    }
    return(list(m1,m2,m3))
  })}

rsq <- function(m) {
  resid=resid(m)
  pred=predict(m)
  y=pred+resid
  tss=sum((y-mean(y))^2)
  rss=sum(resid^2)
  rsq=1-(rss/tss)
  return(rsq)
}

rsq <- function(m) {
  resid=resid(m)
  pred=predict(m)
  y=pred+resid
  return(cor(y,pred)^2)
}

newmod <- function(model, fn) {
  mods = estim_models(model)
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  
  if (!is.null(fn)) stargazer(do.call('c', mods),type='html', 
                              column.labels = rep(c('price','line length','distribution'), length(model)), 
                              out = fn, add.lines = list(r2s,obs))
  return(mods)
}



get_formulas <- function(sel) {
  forms= lapply(c('pr','dst', 'llen'), function(x) {
    effects=sel[variable==x]
    if (nrow(effects)>0) return(update.formula(maineffects, formula(paste0('.~.+', paste0(effects$varname,collapse='+')))))
    return(maineffects)
    
  })
  names(forms) <- c('pr','dst', 'llen')
  forms
}


all_mods <- function(models) {
  lapply(models, function(forms) {
    list(m1 = lmer(update.formula(elastlt~1, forms$pr),
                   data=elast[grep('pr',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         m2 = lmer(update.formula(elastlt~1, forms$dst),
                   data=elast[grep('dst',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
    m3 = lmer(update.formula(elastlt~1, forms$llen),
              data=elast[grep('llen',variable)], weights=w_elastlt,
              control = lmerctrl, REML=F))
    
  })
}
# estimate models
newmodV2 <- function(model, fn, ..., return_models = T) {
  
  mods = all_mods(model)
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  
  stargazer(do.call('c', mods),type='html', 
            column.labels = rep(c('price','distribution','line length'), length(model)), 
            out = fn, add.lines = list(r2s,obs), ...)
  return(mods)
}

lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")





if(0) {
#ln_gdppercapita2010_mc
elast <- copy(elast_sales)

mods=newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)), '../temp/round1-results-sales.html')

elast <- copy(elast_marketshare)

mods=newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)), '../temp/round1-results-marketshare.html')

me<-newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)), NULL)


out <- paste0(capture.output({me<-newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)), NULL, return_models=F)}), collapse='')

}



# Ingelhardt
if(0){
elast <- copy(elast_sales)
mainef= . ~ 1 + (1|country) + (1|category) + (1|brand) + 
  emerging + sbbe_round1_mc*emerging + 
  ln_market_herf_mc + ln_market_growth_mc + 
  appliance + ln_ginicoef_mc + local_to_market_mc + tradrat_mc + survself_mc


mods=newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)), '../temp/round1-results-sales_culture.html')
}

#### SAVE FOR SHINY DEPLOYMENT

save.image(file= 'app/app_workspace.RData')
