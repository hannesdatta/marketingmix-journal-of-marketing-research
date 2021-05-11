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

#### INVESTIGATE ELASTICITIES

hist(elast[variable=='llen']$elastlt)
hist(elast[variable=='rwpspr']$elastlt)
hist(elast[variable=='wpswdst']$elastlt)

elast[, sig := elastlt/elastlt_se]

elast[, list(pos = length(which(abs(sig)>1.645&elastlt>0))/.N,
             neg = length(which(abs(sig)>1.645&elastlt<0))/.N,
             ns = length(which(abs(sig)<=1.645))/.N), by = c('variable')]

summary(elast[variable=='llen']$elastlt)
summary(elast[variable=='rwpspr']$elastlt)
summary(elast[variable=='wpswdst']$elastlt)

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

formulas = list(. ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw_mc`+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_nov6sh_index_mc+ln_market_herf_mc+ln_market_growth_mc+appliance,
. ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw_mc`+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_nov6sh_index_mc+ln_market_herf_mc+ln_market_growth_mc+appliance+ln_gdpgrowthyravg_mc+ln_ginicoef_mc+ln_gdppercapitacurrentyravg_mc+ln_gci_p10_marketsize_s_mc,
. ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw_mc`+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_nov6sh_index_mc+ln_market_herf_mc+ln_market_growth_mc+appliance+ln_uai_mc+ln_pdi_mc+ln_mas_mc,
. ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw_mc`+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_nov6sh_index_mc+ln_market_herf_mc+ln_market_growth_mc+appliance+ln_gdpgrowthyravg_mc+ln_ginicoef_mc+ln_gdppercapitacurrentyravg_mc+ln_gci_p10_marketsize_s_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc)

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
  return(ret)
})




#### PLOTS

dt <- copy(elast[, c('category','country','brand', 'variable','elastlt','elastlt_se'),with=F])

# first aggregate
dt[, w_elastlt:=1/elastlt_se]

tmp = dt[, list(w_elast=sum(elastlt*w_elastlt)/sum(w_elastlt)),by=c('country', 'variable')]

tmp[, abs_val:=w_elast]
tmp[grepl('pr$', variable), abs_val:=-w_elast]

tmp[, rel_val:=abs_val/sum(abs_val), by =c('country')]

tmp[, sum:=sum(abs_val),by=c('country')]

tmp[, printvar:=formatC(w_elast, digits=3,
                          flag="", format="f")]

tmp[, country:=str_to_title(country)]

tmp[, variable_label:=as.character('')]
tmp[grepl('pr', variable), variable_label:=as.character('Price')]
tmp[grepl('llen', variable), variable_label:=as.character('Line length')]
tmp[grepl('dst', variable), variable_label:=as.character('Distribution')]

tmp[, variable_label:=factor(variable_label, levels=rev(c('Line length','Price','Distribution')))]

order_country = unique(tmp[, c('country','sum'),with=F],by=c('country'))

setorder(order_country, country)

tmp[, country_plot:=factor(as.character(country), levels=rev(order_country$country))]


setorder(order_country, sum)
tmp[, country_plot2:=factor(as.character(country), levels=order_country$country)]

ggplot(tmp, aes(fill=variable_label, y=abs_val, 
                                     x=country_plot,
                                     label=printvar)) + geom_bar(position="stack", stat="identity")  +
  scale_fill_grey(start = .6, end = .9) + coord_flip() +
  theme_bw() + ggtitle('Relative Contribution of Marketing Mix Effects Across Countries') + 
  xlab('Country') + ylab('Magnitude of Marketing Elasticities') + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(fill='Elasticities', caption = '')



ggplot(tmp, aes(fill=variable_label, y=abs_val, 
                x=country_plot2,
                label=printvar)) + geom_bar(position="stack", stat="identity")  +
  scale_fill_grey(start = .6, end = .9) + coord_flip() +
  theme_bw() + ggtitle('Relative Contribution of Marketing Mix Effects Across Countries') + 
  xlab('Country') + ylab('Magnitude of Marketing Elasticities') + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(fill='Elasticities', caption = '')




paste0('Plot generated on the basis of'))
  
  
  ', 
                                             ifelse(input$plot_stack_val=='rel_val', 'relative elasticities', 'absolute elasticities'), ' for brand ', 
                                             str_to_title(input$plot_brands), ' (', tolower(replace_categories(input$plot_categories)), 
                                             ').\nCountries sorted in decreasing order of ', ifelse(input$plot_stack_val=='rel_val', 'line-length elasticities', 
                                                                                                    'combined marketing effectiveness'),'.'))

})
