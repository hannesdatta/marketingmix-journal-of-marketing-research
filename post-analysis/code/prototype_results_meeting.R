# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)

brand_panel=fread('../../analysis/temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load auxilary functions
source('proc_auxilary.R')
source('proc_rename.R')

#_maxiter

# 1
  elast <- fread('../externals/elast_results_salesresponse_max3_p10_cop_sur.csv') 
  #source('preclean.R')
  elast = elast[!elast6_sd==0]
  elast[, elastlt:=elast6]
  elast[, elastlt_se:=elast6_sd]
  elast[, w_elastlt := (1/elast6_sd)/sum(1/elast6_sd), by = c('varname')]
  grepfilter = 'market[_]id|^brand$|varname|^elast|^w[_]elast|^category$|^country$'
  elast <- elast[, grep(grepfilter,colnames(elast),value=T),with=F]
  setnames(elast, 'varname', 'variable')
  
  elast_sales <- copy(elast)
  
# 2
  if(0){
  elast <- fread('../externals/elast_results_main.csv') 
  for (.var in c('elast', 'elastlt')) {
      eval(parse(text=paste0("elast[!is.na(get(.var)), paste0('w_', .var) := 1/get(paste0(.var, '_se'))]")))
      # rescale
      eval(parse(text=paste0("elast[!is.na(get(.var)), paste0('w_', .var) := get(paste0('w_', .var))/max(get(paste0('w_', .var)))]")))
      eval(parse(text=paste0("elast[!is.na(get(.var)), paste0('z_', .var) := get(.var)/get(paste0(.var, '_se'))]")))
    }
  
  grepfilter = 'market[_]id|^brand$|varname|variable|^elast|^w[_]elast|^category$|^country$'
  elast <- elast[, grep(grepfilter,colnames(elast),value=T),with=F]
  
  elast_combin = merge(elast_sales, elast, by = c('category','country','brand', 'variable'),all.x=T)
  
  with(elast_combin, cor(elast6, elastlt.y,use='pairwise'))
  with(elast_combin, cor(elast6*(1/elast6_sd), `elastlt.y`/(1/`elastlt_se.y`),use='pairwise'))
  
  }
  
  
# load SBBE
  sbbe <- fread('../externals/elast_results_marketshare.csv')
  setkey(sbbe, category,country,brand)
  elast[, lower_brand:=tolower(brand)]
  setkey(elast, category,country,lower_brand)
  elast[sbbe, sbbe_round1:=i.sbbe_std]
  elast[!is.na(elastlt), sbbe_round1_mc := sbbe_round1-mean(sbbe_round1,na.rm=T),by=c('variable')]
  
# Load covariates
fns <- list.files('../output/',pattern='covariates.*csv', full.names = T)

for (fn in fns) {
  tmp <- fread(fn)
  aggkey = unlist(strsplit(gsub('[.]csv', '', rev(strsplit(fn,'_')[[1]])[1]), '[-]'))
  setkeyv(tmp, aggkey)
  setkeyv(elast, aggkey)
  elast <- merge(elast, tmp, all.x=T, all.y=F)
  
  added_vars <- setdiff(colnames(tmp), aggkey)
  for (.v in added_vars) {
    if (!class(unlist(elast[,.v,with=F]))=='character') elast[!is.na(elastlt), paste0(.v,'_mc'):=(get(.v)-mean(get(.v),na.rm=T)),by=c('variable')]
  }
}




lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")



if(0) {
  
###### AOV #####
library(sjstats)
vars=unique(elast$variable)

for (i in vars) {
  cat('========================\n')
  cat(i,fill=T)
  cat('========================\n\n')
  
  m0_1<-aov(elastlt~brand, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_2<-aov(elastlt~country, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_3<-aov(elastlt~category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_4<-aov(elastlt~(brand*country)-brand-country, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_5<-aov(elastlt~(brand*category)-brand-category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m0_6<-aov(elastlt~(country*category)-country-category, data=elast, subset=variable==i, weights=1/elastlt_se)
  #m<-aov(elastlt~brand+country+category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m1<-aov(elastlt~brand+country+category, data=elast, subset=variable==i, weights=1/elastlt_se)
  m3<-aov(elastlt~brand*country + brand*category + category*country, data=elast, subset=variable==i, weights=1/elastlt_se)
  
  mlist = list(m0_1,m0_2, m0_3, m0_4, m0_5, m0_6, m1, m3)
  etasq=rbindlist(lapply(seq(along=mlist), function(x) data.table(i=x, anova_stats(mlist[[x]]))))
  etasq[i%in%1:6, model:='m0']
  
  etasq[i%in%7, model:='m1']
  etasq[i%in%8, model:='m2']
  
  
  cat('\n\nPercent explained\n')
  tmp = (dcast(etasq[!term=='Residuals'], model~term, value.var='etasq'))
  
  setnames(tmp, gsub('[:]', ' x ', colnames(tmp)))
  setcolorder(tmp, c('model','brand','category','country','brand x category','brand x country','country x category'))
  
  print(tmp)      
  #print(dcast(etasq[!term=='Residuals'], model~term, value.var='etasq'))
  
}

################################
# TESTING ALL OF OUR VARIABLES #
################################

## Country factors

country_factors = list(country_cult1 = . ~ tradrat_mc + survself_mc,
                       country_cult2 = . ~ ln_pdi_mc + ln_idv_mc + ln_mas_mc + ln_uai_mc + ln_ltowvs_mc + ln_ivr_mc,
                       
                       country_economic =  . ~ ln_gdppercapita2010_mc + 
                         ln_ginicoef_mc + 
                         ln_population2010_mc+ 
                         ln_gci_p06_goods_s_mc+
                         ln_gci_p02_infrastructure_s_mc,
                       
                       country_regulative = . ~ln_gci_p01_institutions_s + 
                         ruleoflaw,
                       brand = . ~  brand_prindex_mean_mc + sbbe_round1_mc + ln_brandnovelty_mc + local_to_market_mc,
                       category = . ~ ln_market_herf_mc + ln_market_growth_mc + catvolatility_sd_mean_mc + ln_catnovelty_mc)
# add random effects
country_factors2 = lapply(country_factors, update.formula, .~  (1|country) + (1|category) + (1|brand) + .)

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

newmod <- function(model, fn) {
  mods = estim_models(model)
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  
  stargazer(do.call('c', mods),type='html', 
            column.labels = rep(c('price','line length','distribution'), length(model)), 
            out = fn, add.lines = list(r2s,obs))
  return(mods)
  }


#################################
# MAIN MODEL FROM MEETING W/ JB #
#################################

main_mod = list(m1 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                  ln_gdppercapita2010_mc + ln_ginicoef_mc + sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc +
  ln_market_herf_mc + ln_market_growth_mc,
                m2 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
  sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
  ln_gdppercapita2010_mc + ln_ginicoef_mc + 
  tradrat_mc + survself_mc +
  ln_gci_p01_institutions_s_mc + ruleoflaw_mc +
  
  ln_market_herf_mc + ln_market_growth_mc + ln_catnovelty_mc,
  m3 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
    sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
    ln_gdppercapita2010_mc + ln_ginicoef_mc + 
    tradrat_mc + survself_mc+
    
    ln_market_herf_mc + ln_market_growth_mc)


mods=newmod(main_mod, fn = '../temp/explore-main.html')

library(car)
vif(mods[[2]][[1]])
vif(mods[[3]][[1]])

covars=c('sbbe_round1_mc', 'local_to_market_mc', 'ln_brandnovelty_mc', 
           'ln_gdppercapita2010_mc', 'ln_ginicoef_mc',
           'tradrat_mc', 'survself_mc',
           'ln_gci_p01_institutions_s_mc', 'ruleoflaw_mc',
           'ln_market_herf_mc', 'ln_market_growth_mc', 'ln_catnovelty_mc')
df =unique(elast,by=c('country','category','brand'))[, covars,with=F]
cor(df, use='pairwise')

cor(df$ln_gdppercapita2010_mc, df$ruleoflaw_mc, use='pairwise')
cor(df$ln_gdppercapita2010_mc, df$ln_gci_p01_institutions_s_mc, use='pairwise')
cor(df$ln_brandnovelty_mc, df$ln_catnovelty_mc, use='pairwise')




#########################################
# EXPLORE INTERACTIONS W/ BRAND FACTORS #
#########################################

country_factors_int1 = list(country_cult1 = . ~ tradrat_mc + survself_mc,
                       country_cult1a = . ~ tradrat_mc*brand_prindex_mean_mc + survself_mc*brand_prindex_mean_mc,
                       country_cult1b = . ~ tradrat_mc*sbbe_round1_mc + survself_mc*sbbe_round1_mc,
                       country_cult1c = . ~ tradrat_mc*ln_brandnovelty_mc + survself_mc*ln_brandnovelty_mc,
                       country_cult1d = . ~ tradrat_mc*local_to_market_mc + survself_mc*local_to_market_mc,
                       
                       country_cult2 = . ~ ln_pdi_mc + ln_idv_mc + ln_mas_mc + ln_uai_mc + ln_ltowvs_mc + ln_ivr_mc,
                       country_cult2a = . ~ ln_pdi_mc*brand_prindex_mean_mc + ln_idv_mc*brand_prindex_mean_mc + ln_mas_mc*brand_prindex_mean_mc + 
                         ln_uai_mc*brand_prindex_mean_mc + ln_ltowvs_mc*brand_prindex_mean_mc + ln_ivr_mc*brand_prindex_mean_mc,
                       country_cult2b = . ~ ln_pdi_mc*sbbe_round1_mc + ln_idv_mc*sbbe_round1_mc + ln_mas_mc*sbbe_round1_mc +
                         ln_uai_mc*sbbe_round1_mc + ln_ltowvs_mc*sbbe_round1_mc + ln_ivr_mc*sbbe_round1_mc,
                       country_cult2c = . ~ ln_pdi_mc*ln_brandnovelty_mc + ln_idv_mc*ln_brandnovelty_mc + ln_mas_mc*ln_brandnovelty_mc + ln_uai_mc*ln_brandnovelty_mc + ln_ltowvs_mc*ln_brandnovelty_mc + ln_ivr_mc*ln_brandnovelty_mc,
                       country_cult2d = . ~ ln_pdi_mc*local_to_market_mc + ln_idv_mc*local_to_market_mc + ln_mas_mc*local_to_market_mc + ln_uai_mc*local_to_market_mc + ln_ltowvs_mc*local_to_market_mc + ln_ivr_mc*local_to_market_mc)
                       
country_factors_int2 = list(country_economic =  . ~ ln_gdppercapita2010_mc + 
                              ln_ginicoef_mc + 
                              ln_population2010_mc+ 
                              ln_gci_p06_goods_s_mc+
                              ln_gci_p02_infrastructure_s_mc,
                            country_economica =  . ~ ln_gdppercapita2010_mc*brand_prindex_mean_mc + 
                              ln_ginicoef_mc*brand_prindex_mean_mc + 
                              ln_population2010_mc*brand_prindex_mean_mc+ 
                              ln_gci_p06_goods_s_mc*brand_prindex_mean_mc+
                              ln_gci_p02_infrastructure_s_mc*brand_prindex_mean_mc,
                            country_economicb =  . ~ ln_gdppercapita2010_mc*sbbe_round1_mc + 
                              ln_ginicoef_mc*sbbe_round1_mc + 
                              ln_population2010_mc*sbbe_round1_mc+ 
                              ln_gci_p06_goods_s_mc*sbbe_round1_mc+
                              ln_gci_p02_infrastructure_s_mc*sbbe_round1_mc,
                            country_economicc =  . ~ ln_gdppercapita2010_mc*ln_brandnovelty_mc + 
                              ln_ginicoef_mc*ln_brandnovelty_mc + 
                              ln_population2010_mc*ln_brandnovelty_mc+ 
                              ln_gci_p06_goods_s_mc*ln_brandnovelty_mc+
                              ln_gci_p02_infrastructure_s_mc*ln_brandnovelty_mc,
                            country_economicd =  . ~ ln_gdppercapita2010_mc*local_to_market_mc + 
                              ln_ginicoef_mc*local_to_market_mc + 
                              ln_population2010_mc*local_to_market_mc+ 
                              ln_gci_p06_goods_s_mc*local_to_market_mc+
                              ln_gci_p02_infrastructure_s_mc*local_to_market_mc)
                            
country_factors_int3 <- list(country_regulative = . ~ln_gci_p01_institutions_s_mc + 
                               ruleoflaw_mc,
                             country_regulativea = . ~ln_gci_p01_institutions_s_mc*brand_prindex_mean_mc + 
                               ruleoflaw_mc*brand_prindex_mean_mc,
                             country_regulativeb = . ~ln_gci_p01_institutions_s_mc*sbbe_round1_mc + 
                               ruleoflaw_mc*sbbe_round1_mc,
                             country_regulativec = . ~ln_gci_p01_institutions_s_mc*ln_brandnovelty_mc + 
                               ruleoflaw_mc*ln_brandnovelty_mc,
                             country_regulatived = . ~ln_gci_p01_institutions_s_mc*local_to_market_mc + 
                               ruleoflaw_mc*local_to_market_mc)

# Interactions
int4 <- list(category = . ~ ln_market_herf_mc + ln_market_growth_mc + catvolatility_sd_mean_mc + ln_catnovelty_mc,
             categorya = . ~ ln_market_herf_mc*brand_prindex_mean_mc + ln_market_growth_mc*brand_prindex_mean_mc + catvolatility_sd_mean_mc*brand_prindex_mean_mc + 
               ln_catnovelty_mc*brand_prindex_mean_mc,
             
             categoryb = . ~ ln_market_herf_mc*sbbe_round1_mc + ln_market_growth_mc*sbbe_round1_mc + catvolatility_sd_mean_mc*sbbe_round1_mc + 
               ln_catnovelty_mc*sbbe_round1_mc,
             categoryc = . ~ ln_market_herf_mc*ln_brandnovelty_mc + ln_market_growth_mc*ln_brandnovelty_mc + catvolatility_sd_mean_mc*ln_brandnovelty_mc + 
               ln_catnovelty_mc*ln_brandnovelty_mc,
             categoryd = . ~ ln_market_herf_mc*local_to_market_mc + ln_market_growth_mc*local_to_market_mc + catvolatility_sd_mean_mc*local_to_market_mc + ln_catnovelty_mc*local_to_market_mc)


newmod(lapply(country_factors_int1, update.formula, .~  (1|country) + (1|category) + (1|brand) + .),
       fn = '../temp/explore-interactions_country_culture.html')
newmod(lapply(country_factors_int2, update.formula, .~  (1|country) + (1|category) + (1|brand) + .),
       fn = '../temp/explore-interactions_country_economic.html')

newmod(lapply(country_factors_int3, update.formula, .~  (1|country) + (1|category) + (1|brand) + .),
       fn = '../temp/explore-interactions_country_regulative.html')
           
newmod(lapply(int4, update.formula, .~  (1|country) + (1|category) + (1|brand) + .),
       fn = '../temp/explore-interactions_categories.html')



#########################################################
# Narrowing down after meeting with Marnik, 12 Oct 2020 #
#########################################################

secondstage <- list(m1 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                      sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
                      ln_gdppercapita2010_mc + ln_ginicoef_mc + 
                      tradrat_mc + survself_mc +
                      ln_market_herf_mc + ln_market_growth_mc,
                    m2 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                      sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
                      ln_gdppercapita2010_mc + ln_ginicoef_mc + 
                      tradrat_mc + survself_mc +
                      ln_market_herf_mc*sbbe_round1_mc + ln_market_herf_mc*local_to_market_mc + ln_market_herf_mc*ln_brandnovelty_mc+
                      ln_market_growth_mc*sbbe_round1_mc + ln_market_growth_mc*local_to_market_mc + ln_market_growth_mc*ln_brandnovelty_mc,
                    m3 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                      sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
                      ln_gdppercapita2010_mc + ln_ginicoef_mc + 
                      tradrat_mc + survself_mc +
                      ln_market_herf_mc + ln_market_growth_mc +
                      ln_gdppercapita2010_mc*sbbe_round1_mc + ln_gdppercapita2010_mc*local_to_market_mc + ln_gdppercapita2010_mc*ln_brandnovelty_mc+
                      ln_ginicoef_mc*sbbe_round1_mc + ln_ginicoef_mc*local_to_market_mc + ln_ginicoef_mc*ln_brandnovelty_mc,
                    m4 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                      sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
                      ln_gdppercapita2010_mc + ln_ginicoef_mc + 
                      tradrat_mc + survself_mc +
                      ln_market_herf_mc + ln_market_growth_mc +
                      
                      tradrat_mc*sbbe_round1_mc + tradrat_mc*local_to_market_mc + tradrat_mc*ln_brandnovelty_mc+
                      survself_mc*sbbe_round1_mc + survself_mc*local_to_market_mc + survself_mc*ln_brandnovelty_mc,
                    m6 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                      sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
                      ln_gdppercapita2010_mc + ln_ginicoef_mc + 
                      tradrat_mc + survself_mc +
                      ln_market_herf_mc + ln_market_growth_mc +
                      
                      ln_gdppercapita2010_mc*sbbe_round1_mc + ln_gdppercapita2010_mc*local_to_market_mc + ln_gdppercapita2010_mc*ln_brandnovelty_mc+
                      ln_ginicoef_mc*sbbe_round1_mc + ln_ginicoef_mc*local_to_market_mc + ln_ginicoef_mc*ln_brandnovelty_mc+
                      tradrat_mc*sbbe_round1_mc + tradrat_mc*local_to_market_mc + tradrat_mc*ln_brandnovelty_mc+
                      survself_mc*sbbe_round1_mc + survself_mc*local_to_market_mc + survself_mc*ln_brandnovelty_mc
)

models<-newmod(secondstage, fn = '../temp/explore-blockinteractions-updated.html')

names(models) <- names(secondstage)

# collect interaction terms

interacts=rbindlist(lapply(c('m2','m3','m4','m5','m6'), function(x) rbindlist(lapply(1:3, function(i) data.table(modeltype=x, var_index=i, varname=rownames(summary(models[[x]][[i]])$coefficients), summary(models[[x]][[i]])$coefficients)))))
interacts[, variable:=c('pr','llen','dst')[var_index]]

interacts <- interacts[grepl('[:]',varname)]


maineffects = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
  sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
  ln_gdppercapita2010_mc + ln_ginicoef_mc + 
  tradrat_mc + survself_mc +
  ln_gci_p01_institutions_s_mc + ruleoflaw_mc +
  ln_market_herf_mc + ln_market_growth_mc + ln_catnovelty_mc


get_formulas <- function(sel) {
 forms= lapply(c('pr','llen','dst'), function(x) {
    effects=sel[variable==x]
    if (nrow(effects)>0) return(update.formula(maineffects, formula(paste0('.~.+', paste0(effects$varname,collapse='+')))))
    return(maineffects)
  
 })
 names(forms) <- c('pr','llen','dst')
 forms
 }


all_mods <- function(models) {
  lapply(models, function(forms) {
    list(m1 = lmer(update.formula(elastlt~1, forms$pr),
                      data=elast[grep('pr',variable)], weights=w_elastlt,
                      control = lmerctrl, REML=F),
            m2 = lmer(update.formula(elastlt~1, forms$llen),
                      data=elast[grep('llen',variable)], weights=w_elastlt,
                      control = lmerctrl, REML=F),
            m3 = lmer(update.formula(elastlt~1, forms$dst),
                      data=elast[grep('dst',variable)], weights=w_elastlt,
                      control = lmerctrl, REML=F))
  })
}
# estimate models
newmodV2 <- function(model, fn) {
  
  mods = all_mods(model)
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  
  stargazer(do.call('c', mods),type='html', 
            column.labels = rep(c('price','line length','distribution'), length(model)), 
            out = fn, add.lines = list(r2s,obs))
  return(mods)
}

forms = get_formulas(interacts[modeltype%in%c('m2', 'm3','m4','m5')&abs(`t value`>=1)])
forms2 = get_formulas(interacts[modeltype%in%c('m2', 'm6')&abs(`t value`>=1)])

forms3 = get_formulas(interacts[modeltype%in%c('m2', 'm3','m4','m5')&abs(`t value`>=1.644854)])
forms4 = get_formulas(interacts[modeltype%in%c('m2', 'm6')&abs(`t value`>=1.644854)])

mods=newmodV2(list(forms,forms2, forms3, forms4), '../temp/explore-keep-significant-effects.html')


#stargazer(mods, type = 'text', column.labels=rep(c('price','line length','distribution'), length(mods)),
 #         object.names=T, out = '../temp/test.html')

##### NEW


mainef= . ~ 1 + (1|country) + (1|category) + (1|brand) + 
  sbbe_round1_mc + local_to_market_mc + ln_brandnovelty_mc+
  ln_gdppercapita2010_mc + ln_ginicoef_mc + 
  tradrat_mc + survself_mc +
  ln_market_herf_mc + ln_market_growth_mc

brand <- c('sbbe_round1_mc','local_to_market_mc','ln_brandnovelty_mc')
others <- c('ln_gdppercapita2010_mc', 'ln_ginicoef_mc','tradrat_mc','survself_mc','ln_market_herf_mc','ln_market_growth_mc')

brandinteracts <- unlist(lapply(others, function(i) {
  unlist(lapply(brand, function(b) update.formula(mainef,formula(paste0('. ~ . + ', paste0(b,':',i))))))
}                    ))

models<-newmod(brandinteracts, fn = '../temp/explore-full-brandinteractions.html')

names(models) <- 1:length(models)

# collect interaction terms

interacts=rbindlist(lapply(1:length(models), function(x) rbindlist(lapply(1:3, function(i) data.table(modeltype=x, var_index=i, varname=rownames(summary(models[[x]][[i]])$coefficients), summary(models[[x]][[i]])$coefficients)))))
interacts[, variable:=c('pr','llen','dst')[var_index]]

interacts <- interacts[grepl('[:]',varname)]


maineffects = mainef

get_formulas <- function(sel) {
  forms= lapply(c('pr','llen','dst'), function(x) {
    effects=sel[variable==x]
    if (nrow(effects)>0) return(update.formula(maineffects, formula(paste0('.~.+', paste0(effects$varname,collapse='+')))))
    return(maineffects)
    
  })
  names(forms) <- c('pr','llen','dst')
  forms
}

forms = get_formulas(interacts[modeltype%in%1:18&abs(`t value`>=1)])
forms2 = get_formulas(interacts[modeltype%in%1:18&abs(`t value`>=1.644854)])

mods=newmodV2(list(forms,forms2), '../temp/explore-harald-interact.html')

### NEW 2


##### NEW
comb <-c(brand, others)

combinations=data.table(expand.grid(c(1:length(comb)), c(1:length(comb))))
setnames(combinations, c('index1','index2'))
combinations <- data.frame(combinations[index1<index2])

 
brandinteracts <- unlist(lapply(1:nrow(combinations), function(ind) {
  update.formula(mainef,formula(paste0('. ~ . + ', paste0(comb[combinations[ind,1]], ':', comb[combinations[ind,2]]))))
}))

models<-newmod(brandinteracts, fn = '../temp/explore-full-brandinteractions.html')

names(models) <- 1:length(models)

# collect interaction terms
interacts=rbindlist(lapply(1:length(models), function(x) rbindlist(lapply(1:3, function(i) data.table(modeltype=x, var_index=i, varname=rownames(summary(models[[x]][[i]])$coefficients), summary(models[[x]][[i]])$coefficients)))))
interacts[, variable:=c('pr','llen','dst')[var_index]]
interacts <- interacts[grepl('[:]',varname)]

maineffects = mainef

forms = get_formulas(interacts[modeltype%in%1:36&abs(`t value`>=1)])
forms2 = get_formulas(interacts[modeltype%in%1:36&abs(`t value`>=1.644854)])

mods=newmodV2(list(forms,forms2), '../temp/explore-harald-interact-full.html')
