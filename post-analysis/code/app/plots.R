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

#### CORRELATION MEAN / MEDIAN

elast[, list(cor=cor(elastlt, elastmedianlt, use='pairwise')),by=c('variable')]

countries <- unique(elast, by = c('country'))
nrow(countries)


# logged
vars=c('ln_penn_growthrgdpeyravg_mc','ln_ginicoef_mc', 'ln_penn_percapitargdpeyravg_mc',
  'ln_penn_popyravg_mc', 'ln_uai_mc', 'ln_pdi_mc', 'ln_mas_mc')



cor(unique(elast, by = c('category','country','brand'))[, c('emerging',vars),with=F], use='pairwise')

#unlogged
vars=c('penn_growthrgdpeyravg_mc','ginicoef_mc', 'penn_percapitargdpeyravg_mc',
       'penn_popyravg_mc', 'uai_mc', 'pdi_mc', 'mas_mc')



cor(unique(elast, by = c('category','country','brand'))[, c('emerging',vars),with=F], use='pairwise')


# N=14
vars=c('penn_growthrgdpe2010','ginicoef_mc', 'penn_percapitargdpe2010',
       'penn_pop2010', 'uai_mc', 'pdi_mc', 'mas_mc')

cor(unique(countries, by = c('category','country','brand'))[, c('emerging',vars),with=F], use='pairwise')

# How much Y&R coverage?
sum(elast[, list(any(!is.na(bav_brandstrength))), by = c('brand')]$V1)





#### INVESTIGATE ELASTICITIES

hist(elast[variable=='llen']$elastlt)
hist(elast[variable=='rwpspr']$elastlt)
hist(elast[variable=='wpswdst']$elastlt,breaks=100)

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


################################################
#### PLOT POOLED ACROSS ALL COUNTRIES      #####
################################################

dt <- copy(elast[, c('category','country','brand', 'variable','elastlt','elastlt_se'),with=F])

# first aggregate
dt[, w_elastlt:=1/elastlt_se]

tmp = dt[, list(w_elast=sum(elastlt*w_elastlt)/sum(w_elastlt)),by=c('country', 'variable')]

tmp[, abs_val:=w_elast]
tmp[grepl('pr$', variable), abs_val:=-w_elast]


tmp[, sum:=sum(abs_val),by=c('country')]

tmp[, rel_val:=abs_val/sum]

tmp[, printvar:=formatC(w_elast, digits=3,
                          flag="", format="f")]

#tmp[, printvar_rel:=round(rel_val*100,0)]

tmp[, printvar_rel:=paste0(formatC(round(signif(rel_val,3)*100,0), digits=0,
                        flag="", format="f"),'%')]


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

png('../../output/relative_elasticities.png', res=400, units='in', height=5, width=8)

ggplot(tmp, aes(fill=variable_label, y=abs_val, 
                                     x=country_plot,
                                     label=printvar)) + geom_bar(position="stack", stat="identity")  +
  scale_fill_grey(start = .6, end = .9) + coord_flip() +
  theme_bw() + #+ ggtitle('Relative Contribution of Marketing Mix Effects Across Countries') + 
  xlab('Country') + ylab('Magnitude of Marketing Elasticities') + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(fill='Elasticities', caption = '')+ theme(legend.position = 'bottom')+ guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()


# V2

png('../../output/relative_elasticities2.png', res=400, units='in', height=5, width=8)

tmp[, printvar_sum:=paste0('(', formatC(sum, digits=3,
                                   flag="", format="f"),')')]
tmp[!variable=='wpswdst', printvar_sum:='']

ggplot(tmp, aes(fill=variable_label, y=abs_val, 
                x=country_plot2,
                label=printvar_rel)) + geom_bar(position="stack", stat="identity")  +
  scale_fill_grey(start = .6, end = .9) + coord_flip() +
  theme_bw() + #+ ggtitle('Relative Contribution of Marketing Mix Effects Across Countries') + 
  xlab('Country') + ylab('Magnitude of Marketing Elasticities') + 
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + ylim(c(0,2))+
  #geom_text(size = 3, position = position_stack(vjust = 1), aes(y=abs_val, fill=variable_label, label=printvar_sum, hjust=-.3), angle=0)+
  labs(fill='Elasticities', caption = '')+ theme(legend.position = 'bottom')+ guides(fill = guide_legend(reverse = TRUE))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()



#############################
#### PLOT HOLDOUT SAMPLE ####
#############################

predictions <- fread('../../output/predictions_within.csv')[type=='ec_main']

agg_sales = predictions[, list(avg_sales=mean(usales,na.rm=T)),by=c('category','country','brand')]
agg_sales[, rank:=rank(-avg_sales), by = c('category','country')]
setkey(agg_sales, category, country, brand)

setkey(predictions, category, country, brand)
predictions[agg_sales, rank:=i.rank]

predictions[, cc := .GRP, by = c('country', 'category')]

predictions[, rnum := runif(1), by = c('cc','brand')]
predictions[rank>=6, rnum:=999]


predictions[, selected_brand:=rnum==min(rnum), by= c('cc')]

predictions[, selected:=FALSE]
predictions[country=='australia'&grepl('smart', category), selected:=T]
predictions[country=='china'&grepl('camera_compact', category), selected:=T]
predictions[country=='hong kong'&grepl('desktop', category), selected:=T]
predictions[country=='india'&grepl('gen2', category), selected:=T]
predictions[country=='indonesia'&grepl('laptop', category), selected:=T]
predictions[country=='japan'&grepl('mobile', category), selected:=T]
predictions[country=='malaysia'&grepl('micro', category), selected:=T]
predictions[country=='new zealand'&grepl('gen3', category), selected:=T]
predictions[country=='philippines'&grepl('cooling', category), selected:=T]
predictions[country=='singapore'&grepl('slr', category), selected:=T]
predictions[country=='south korea'&grepl('gen1', category), selected:=T]
predictions[country=='taiwan'&grepl('tablet', category), selected:=T]
predictions[country=='thailand'&grepl('dvd', category), selected:=T]
predictions[country=='vietnam'&grepl('washing', category), selected:=T]


  
retained <- predictions[selected_brand==T&selected==T]
retained[, date:=as.Date(date)]

library(ggplot2)


library(gridExtra)
library(grid)



replace_categories <- function(x) {
  ret = rep('', length(x))
  ret[grepl('washing',x)] <- 'Washing machines'
  ret[grepl('tv_gen1',x)] <- 'CRT TVs'
  ret[grepl('tv_gen2_ptv',x)] <- 'Plasma TVs'
  ret[grepl('tv_gen3_lcd_only',x)] <- 'LCD TVs'
  ret[grepl('tablets',x)] <- 'Tablets'
  ret[grepl('phones_smart',x)] <- 'Smartphones'
  ret[grepl('phones_mobile',x)] <- 'Mobile phones'
  ret[grepl('microwave',x)] <- 'Microwaves'
  ret[grepl('laptop',x)] <- 'Laptop computers'
  ret[grepl('dvd',x)] <- 'DVD players and recorders'
  ret[grepl('desktoppc',x)] <- 'Desktop computers'
  ret[grepl('cooling',x)] <- 'Refrigerators'
  ret[grepl('camera_slr',x)] <- 'SLR cameras'
  ret[grepl('camera_compact',x)] <- 'Compact cameras'
  
  return(ret)
}

setorderv(retained, c('country', 'category', 'brand', 'date'), order=1L)

plots<-lapply(unique(retained$cc)[1:14], function(csel) ggplot() + geom_line(data=retained[cc==csel], aes(date, usales)) +
  geom_line(data=retained[cc==csel], linetype = 2, aes(date, usales_hat)) +
  scale_x_date(date_labels = "%Y") + ylab('Unit sales') + xlab('Date') + 
    ggtitle(paste0(str_to_title(unique(retained[cc==csel]$country))),
  subtitle=replace_categories(unique(retained[cc==csel]$category)))+
    theme(plot.title = element_text(size = 12),
          plot.subtitle = element_text(size=9)))

png('../../output/fit_plot.png', res=400, units='in', height=7, width=12)

grid.arrange(grobs=plots,
             ncol = 5, nrow = 3)
dev.off()





######### LELVES OF MMIX



# BOX PLOTS

# load data
brand_panel <- fread('../externals/preclean_main.csv')

#agg levels
agg=brand_panel[selected==T&obs48==T&timewindow==T, lapply(.SD, mean), 
                by = c('category','country','brand'), .SDcols=c('llen','rwpspr','rwpsprd','wpswdst')]

# box plot

agg_melt = melt(agg, by = c('category','country','brand'))
agg_melt[, cc:=.GRP,by=c('category', 'variable')]
agg_melt[, country_label := str_to_title(country)]

replace_variables <- function(x, lower=F) {
  ret = rep('', length(x))
  ret[grepl('llen',x)] <- 'Line length'
  ret[grepl('rwpspr',x)] <- 'Price'
  ret[grepl('rwpsprd',x)] <- 'Price (in USD)'
  ret[grepl('wpswdst',x)] <- 'Distribution'
  if (lower==T) paste0(tolower(substr(ret,1,1)), substr(ret,2,100))
  return(ret)
}

# grouped boxplot
ccsel=1
plot_box = function(ccsel) {
ggplot(agg_melt[cc==ccsel], aes(x=country_label, y=value, fill=country_label)) + 
  geom_boxplot() +  theme(plot.title = element_text(size = 12),
                          plot.subtitle = element_text(size=9)) + theme_bw() + 
  scale_fill_grey(start = .6, end = .9)+ theme(legend.position="none")+
  ggtitle(paste0(str_to_title(replace_variables(unique(agg_melt[cc==ccsel]$variable)))),
          subtitle=replace_categories(unique(agg_melt[cc==ccsel]$category))) + 
  ylab(paste0('Average ', tolower(replace_variables(unique(agg_melt[cc==ccsel]$variable)))))+
  xlab('Country')
}

agg_melt[, category_label:=replace_categories(category)]
setorder(agg_melt, country_label, category_label, variable, brand)
orders=unique(agg_melt$country_label)

agg_melt[, country_label:=factor(country_label, labels=rev(orders))]


replace_categories <- function(x) {
  ret = rep('', length(x))
  ret[grepl('washing',x)] <- 'Washing machines'
  ret[grepl('tv_gen1',x)] <- 'CRT TVs'
  ret[grepl('tv_gen2_ptv',x)] <- 'Plasma TVs'
  ret[grepl('tv_gen3_lcd_only',x)] <- 'LCD TVs'
  ret[grepl('tablets',x)] <- 'Tablets'
  ret[grepl('phones_smart',x)] <- 'Smartphones'
  ret[grepl('phones_mobile',x)] <- 'Mobile phones'
  ret[grepl('microwave',x)] <- 'Microwaves'
  ret[grepl('laptop',x)] <- 'Laptop computers'
  ret[grepl('dvd',x)] <- 'DVD players and rec.'
  ret[grepl('desktoppc',x)] <- 'Desktop computers'
  ret[grepl('cooling',x)] <- 'Refrigerators'
  ret[grepl('camera_slr',x)] <- 'SLR cameras'
  ret[grepl('camera_compact',x)] <- 'Compact cameras'
  
  return(ret)
}

plot_box = function(ccsel) {
  ggplot(agg_melt[cc==ccsel], aes(x=country_label, y=value, fill=country_label)) + 
    geom_boxplot() +  theme(plot.title = element_text(size = 9),
                            plot.subtitle = element_text(size=9)) + theme_bw() + 
    scale_fill_grey(start = .6, end = .9)+ theme(legend.position="none")+
    ggtitle(replace_categories(unique(agg_melt[cc==ccsel]$category)))+
    ylab(paste0('Average ', replace_variables(unique(agg_melt[cc==ccsel]$variable), lower=T)))+
    xlab('Country')+ coord_flip()+theme(text = element_text(size=9))
}

for (.v in c('llen','rwpsprd','wpswdst')) {
plots<-lapply(unique(agg_melt[variable==.v]$cc), plot_box)
png(paste0('../output/boxplot_', .v, '.png'), res=400, units='in', height=7, width=12)
grid.arrange(grobs=plots,
             ncol = 5, nrow = 3)
dev.off()
}


