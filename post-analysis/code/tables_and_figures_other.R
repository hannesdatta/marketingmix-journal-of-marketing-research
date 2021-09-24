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

fns <- c('app/app_workspace.RData')

load(fns)

model = 'ec_main_sur'
dv='elastlt'



### UNIT SALES COVERAGE


# compute total unit sales coverage
alls=sum(brand_panel$usales,na.rm=T)
cov=sum(brand_panel[selected==T&timewindow==T&obs48]$usales,na.rm=T)
cov/alls # --> report in paper


brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]

brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]
brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand')][!grepl('allothers',brand)] # -> uniq brands






## other


length(unique(brand_panel[selected==T]$brand_id))
length(unique(brand_panel[selected==T & timewindow==T &obs48==T]$brand_id))
nrow(brand_panel[selected==T])
nrow(brand_panel[selected==T & timewindow==T &obs48==T])

nrow(brand_panel[selected==T & timewindow==T &obs48==T])/nrow(brand_panel[selected==T])



brand_panel[, list(.N),by=c('category','country','brand')] # [!grepl('alloth',brand)]
brand_panel[, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
brand_panel[selected==T & timewindow == T & obs48 == T, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
# we lose:
brand_panel[selected==T & timewindow == T & obs48 == F, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)]
brand_panel[selected==T & timewindow == T & obs48 == F, list(.N),by=c('category','country','brand')][!grepl('alloth',brand)][, list(.N, avgN=mean(N)),by=c('category')]

rem_obs=nrow(brand_panel[selected==T & timewindow == F & obs48 == T])
keep_obs=nrow(brand_panel[selected==T & timewindow == T & obs48 == T])





# share of observations/unit sales in data (reported for paper)
sum(brand_panel[selected==T & timewindow==T &obs48==T]$usales, na.rm=T)/sum(brand_panel$usales,na.rm=T)

brand_panel <- brand_panel[selected==T & timewindow==T &obs48==T] # at least 48 obs for estimation

length(unique(brand_panel[selected==T]$brand_id))

brand_panel[, list(.N),by=c('brand','category','country')][!grepl('allother',brand)]
brand_panel[, list(.N),by=c('brand','category','country')][!grepl('allother',brand)][, list(.N),by=c('brand')] # --> uniq brand


# get elasticities

tmp = copy(elasticities[[model]][selection_obs48==T&selection_brands==T])

# define DV
tmp[, elastlt:=get(dv)]
tmp[, elastlt_se:=get(paste0(dv,'_se'))]
tmp[, w_elastlt:=1/elastlt_se, by = c('variable')]

tmp <- tmp[!is.na(elastlt), percentile:=ecdf(elastlt)(elastlt), by = c('variable')]

# winsorizing
perc_extract = .01
tmp[, perc_low := quantile(elastlt, probs = perc_extract), by = c('variable')]
tmp[, perc_high := quantile(elastlt, probs = 1-perc_extract), by = c('variable')]
  
tmp[percentile<perc_extract, elastlt:=perc_low]
tmp[percentile>(1-perc_extract), elastlt:=perc_high]

elast <- copy(tmp)

# videocon is from india, not from ger
elast[brand=='videocon']$`brand_from_jp-us-ch-ge-sw`=0
# YOshii is from Japan
elast[grepl('yoshii', brand)]$`brand_from_jp-us-ch-ge-sw`=1
# Simpson is from Australia
elast[grepl('simpson', brand)]$`brand_from_jp-us-ch-ge-sw`=0

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


################
# correlations #
################



# adapted from: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package

library(Hmisc)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex"), ndec = 2){
  
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "   ")))
  
  ## trunctuate the correlation matrix to three decimals
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), ndec))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  Rnew <- as.matrix(Rnew)
  Rnew <- as.data.frame(Rnew)
  
  ## remove last column and return the correlation matrix
  #Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
  
} 

#################


###########
# TABLE 7 #
###########

# Correlations for equity (standardized within category)

rowvars = c('sbbe_round1_mc','brand_from_jp-us-ch-ge-sw',
            'ln_llen_windex_mc', 'ln_rwpspr_windex_mc' ,
            'ln_wpswdst_windex_mc',
             'ln_nov6sh_windex_mc',
            'ln_market_herf_mc', 'ln_market_meangrowth_mc',
              'appliance',
            'ln_penn_popyravg_mc', 'ln_penn_percapitargdpeyravg_mc',
            'ln_penn_growthrgdpeyravg_mc', 'ln_ginicoef_mc',
             'ln_pdi_mc' ,'ln_uai_mc','ln_mas_mc')


tmp = unique(elast, by = c('category','country','brand'))[, c(rowvars),with=F]
tmp = tmp[complete.cases(tmp)]

tmpcor=cor(as.matrix(tmp))

options(width=600)
sink('../../output/correlations_secondstage.txt')
print(corstars(as.matrix(tmp), method="pearson", removeTriangle='none', ndec=3))
cat(paste0('\nNumber of observations: ', nrow(as.matrix(tmp)), '\n'))
sink()

write.table(tmpcor, '../../output/correlations_secondstage.csv', row.names=T)

