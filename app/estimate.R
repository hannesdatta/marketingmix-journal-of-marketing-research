rm(list=ls())
# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)
library(shiny)
library(sandwich)
library(lmtest)
library(car)
library(knitr)
library(digest)
library(xlsx)
library(stringr)
library(foreign)

library(ggplot2)
library(ggthemes)

fns <- c('app_workspace.RData')

load(fns)

model = 'ec_main_sur'
dv='elastlt'

# get elasticities

tmp = copy(elasticities[[model]][selection_obs48==T&selection_brands==T])

# define DV
tmp[, elastlt:=get(dv)]
tmp[, elastlt_se:=get(paste0(dv,'_se'))]
tmp[, w_elastlt:=1/elastlt_se, by = c('variable')]

tmp <- tmp[!is.na(elastlt), percentile:=ecdf(elastlt)(elastlt), by = c('variable')]

perc_extract = as.numeric(gsub('.*[_]','', input$trimming))/100

# winsorizing
perc_extract = .01
tmp[, perc_low := quantile(elastlt, probs = perc_extract), by = c('variable')]
tmp[, perc_high := quantile(elastlt, probs = 1-perc_extract), by = c('variable')]
  
tmp[percentile<perc_extract, elastlt:=perc_low]
tmp[percentile>(1-perc_extract), elastlt:=perc_high]

elast <- copy(tmp)

######## FUNCTIONS #######

rsq2 <- function(m) {
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

## Estimate model



formulas = list(. ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw_mc`+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_nov6sh_index_mc+ln_market_herf_mc+ln_market_growth_mc+appliance+ln_penn_growthrgdpeyravg_mc+ln_ginicoef_mc+ln_penn_percapitargdpeyravg_mc+ln_penn_popyravg_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc)

clust = ~brand + category + country

lapply(formulas, function(form) {
  pr = lm(update.formula(elastlt~1, form),
                data=elast[grep('pr',variable)], weights=w_elastlt)
  
  dst = lm(update.formula(elastlt~1, form),
                data=elast[grep('dst',variable)], weights=w_elastlt)
  llen = lm(update.formula(elastlt~1, form),
          data=elast[grep('llen',variable)], weights=w_elastlt)
  
  avail_pr=setdiff(1:nrow(elast[grep('pr',variable)]), pr$na.action)
  avail_dst=setdiff(1:nrow(elast[grep('dst',variable)]), dst$na.action)
  avail_llen=setdiff(1:nrow(elast[grep('llen',variable)]), llen$na.action)
  
  
  rsqs=unlist(lapply(list(pr,dst,llen),rsq))
  rsqs2=unlist(lapply(list(pr,dst,llen),rsq2))
  obs=unlist(lapply(list(pr,dst,llen),function(x) length(residuals(x))))
  aics = unlist(lapply(list(pr,dst,llen), function(x) AIC(x)))
  bics = unlist(lapply(list(pr,dst,llen), function(x) BIC(x)))
  logliks = unlist(lapply(list(pr,dst,llen), function(x) logLik(x)))
  k=unlist(lapply(list(pr, dst, llen), function(x) length(grep('elastlt|weights',colnames(x$model),value=T, invert=T))+2))
  ret = rbind(obs, k, aics,bics,  logliks) #, rsqs) #, rsqs2)
  colnames(ret) <- c('pr','dst','llen')
  return(list(fit=ret, models = list(llen, pr, dst)))
})

