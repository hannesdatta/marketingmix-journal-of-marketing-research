# Load data

brand_panel=fread('../../analysis/temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load auxilary functions
source('proc_auxilary.R')
source('proc_rename.R')


elast <- fread('../externals/elast_results_salesresponse_max3_p10_cop.csv') 
elast[, elastlt:=elast6]
elast[, elastlt_se:=elast6_sd]
elast[, w_elastlt := (1/elast6_sd)/sum(1/elast6_sd)]

# load SBBE
sbbe <- fread('../externals/elast_results_main.csv')
setkey(sbbe, category,country,brand)
elast[, lower_brand:=tolower(brand)]
setkey(elast, category,country,lower_brand)
elast[sbbe, sbbe_round1:=i.sbbe_std]

# load BAV
bav <- fread('../../derived/output/bav.csv')
setkey(bav, country,brand)

setkey(elast, country,lower_brand)
bav[, brandstrength_mean:=mean(Brand_Strength_R), by =c('country','brand')]
elast[bav, brandstrength:=brandstrength_mean]


setnames(elast, 'varname', 'variable')
source('preclean.R')

lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")

library(lme4)

# brand novelty
novel = brand_panel[, list(novelty=mean(nov6sh)),by=c('category','country', 'brand')]
setkey(novel, category,country,brand)

setkey(elast, category, country,lower_brand)

elast[novel, ':=' (ln_brnovelty=log(i.novelty+1), brnovelty=i.novelty)]

# category novelty
novel = brand_panel[, list(novelty=mean(nov6sh),
                           Nbrand=length(unique(brand)),
                           sumnovelty=sum(nov6sh)
                           ),by=c('category','country')]

setkey(novel, category,country)

setkey(elast, category, country)

elast[novel, ':=' (ln_catnovelty=log((i.sumnovelty-brnovelty)/(i.Nbrand-1)))]

#,
#catnoveltysum = i.sumnov6sh,
#catnoveltyN = i.Nbrand)


elast[, sbbe_round1_mc:=sbbe_round1-mean(sbbe_round1,na.rm=T),by=c('variable')]


# descriptives of elastitcities
# main model like this
# tested other models + suggestions marnik/jb


#brand_prindex_mean_mc ++ + ln_catnovelty
#sbbe_round1 branz
#+ (1|brand) +  sbbe_round1 ln_brand_prindex_mean_mcgci_p06_goods_s
formula_basic = list(m4 = . ~ 1 + ln_gdppercap2010_mc + ln_gini_mc + sbbe_round1_mc + ln_brnovelty + local_to_market + ln_market_herf_mc + ln_market_growth_mc ) #log(catvolatility_range))

formula = list(m4 = . ~ 1 + (1|country) + (1|category) + (1|brand) + ln_gdppercap2010_mc + ln_gini_mc + sbbe_round1_mc + ln_brnovelty + local_to_market+
                 ln_market_herf_mc + ln_market_growth_mc) #log(catvolatility_range))

#formula = list(m4 = . ~ 1 + emerging + sbbe_round1 + ln_market_herf_mc + ln_market_growth_mc + appliance + ln_gini_mc + local_to_market)

vars= unique(c('rwpspr','wpswdst','llen'))
regs <- lapply(vars, function(varname) {
  fit=NULL
  lt = lapply(formula, function(form) lmer(update(form, elastlt ~ .),  control = lmerctrl, 
                                           REML = F, data = data.table(elast[variable==varname&!is.na(elastlt)]), weights=w_elastlt))
 reg = lapply(formula_basic, function(form) lm(update(form, elastlt ~ .),  data = data.table(elast[variable==varname&!is.na(elastlt)]), weights=w_elastlt))
  
  return(lt)
})

m<-lm(update(formula_basic[[1]], elastlt ~ .), data = elast[variable==vars[1]&!is.na(elastlt)], weights=w_elastlt)
vif(m)




library(stargazer)

stargazer(regs,type='text', column.labels=vars)



# Calculate new models
# Calculate VIF

