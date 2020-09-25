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

#,
#catnoveltysum = i.sumnov6sh,
#catnoveltyN = i.Nbrand)


#elast[, sbbe_round1_mc:=sbbe_round1-mean(sbbe_round1,na.rm=T),by=c('varname')]

setnames(elast, 'varname', 'variable')

source('preclean.R')

# get reputation data
rep <- fread('reputation.csv')
rep[, score2013:=as.numeric(gsub('[,]','.', rep2013))]
rep[, score2015:=as.numeric(gsub('[,]','.', rep2015))]
rep[!is.na(score2013)&!is.na(score2015), total_score:=(score2013+score2015)/2]
rep[is.na(score2013)&!is.na(score2015), total_score:=(score2015)]

setkey(rep, country)
setkey(elast, country_of_origin)
elast[rep, repscore:=i.total_score]


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

elast[, sbbe_round1_mc:=sbbe_round1-mean(sbbe_round1,na.rm=T),by=c('variable')]
lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")

library(lme4)


# descriptives of elastitcities
# main model like this
# tested other models + suggestions marnik/jb


#brand_prindex_mean_mc ++ + ln_catnovelty
#sbbe_round1 branz
#+ (1|brand) +  sbbe_round1 ln_brand_prindex_mean_mcgci_p06_goods_s
#formula_basic = list(m4 = . ~ 1 + ln_gdppercap2010_mc + ln_gini_mc + sbbe_round1_mc + ln_brnovelty + local_to_market + ln_market_herf_mc + ln_market_growth_mc ) #log(catvolatility_range))

####### TESTING COUNTRY FACTORS

formula = list(m1 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                 ln_gdppercap2010_mc + ln_gini_mc + sbbe_round1_mc + ln_brnovelty  + local_to_market +
                 ln_market_herf_mc + ln_market_growth_mc + appliance)
formula$m2 <- update(formula$m1, .~.+log(gci_p01_institutions_s))
formula$m3 <- update(formula$m1, .~.+log(gci_p02_infrastructure_s))
formula$m3b <- update(formula$m1, .~.+log(gci_p06_goods_s))
formula$m4 <- update(formula$m1, .~.+log(gci_p09_tech_s))
formula$m6 <- update(formula$m1, .~.+log(ruleoflaw))
formula$m7 <- update(formula$m1, .~.+log(secularity))
formula$m8 <- update(formula$m1, .~.+log(emancipation))
formula$m9 <- update(formula$m1, .~.+log(repscore))

####### TESTING BRAND AND CATEGORY FACTORS

formula_brand = list(m1=formula$m1)

formula_brand$m1b <- update(formula$m1, .~.+log(brandstrength)-sbbe_round1_mc)
formula_brand$m1c <- update(formula$m1, .~.+brandz-sbbe_round1_mc)

formula_brand$m2 <- update(formula$m1, .~.+ln_brand_prindex_mean_mc)
formula_brand$m2b <- update(formula$m1, .~.+`brand_from_jp-us-ch-ge-sw`)

formula_brand$m3 <- update(formula$m1, .~.+log(catvolatility_sd))
formula_brand$m3b <- update(formula$m1, .~.+ln_catnovelty)

###### TESTING DIFFERENT DEVELOPMENT INDICATORS


formula2 <- list(m1=formula$m1)
formula2$m1b <- update(formula$m1, .~.+ln_hdi2010_mc-ln_gdppercap2010_mc)
formula2$m1c <- update(formula$m1, .~.+log(gci_overall_s)-ln_gdppercap2010_mc)
formula2$m1d <- update(formula$m1, .~.+emerging-ln_gdppercap2010_mc)


formula3 = list(m1 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                  ln_gdppercap2010_mc + ln_gini_mc + sbbe_round1_mc + ln_brnovelty  + local_to_market+
                  ln_market_herf_mc + ln_market_growth_mc + appliance,
                m2 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                  ln_gdppercap2010_mc + ln_gini_mc + sbbe_round1_mc*ln_gdppercap2010_mc + ln_brnovelty*ln_gdppercap2010_mc  + local_to_market*ln_gdppercap2010_mc+
                  ln_market_herf_mc*ln_gdppercap2010_mc + ln_market_growth_mc*ln_gdppercap2010_mc + appliance*ln_gdppercap2010_mc+
                  sbbe_round1_mc*ln_gini_mc + ln_brnovelty*ln_gini_mc  + local_to_market*ln_gini_mc+
                  ln_market_herf_mc*ln_gini_mc + ln_market_growth_mc*ln_gini_mc + appliance*ln_gini_mc,
                m3 = . ~ 1 + (1|country) + (1|category) + (1|brand) + 
                  ln_gdppercap2010_mc*sbbe_round1_mc + ln_gini_mc*sbbe_round1_mc + local_to_market*sbbe_round1_mc+
                  ln_market_herf_mc*sbbe_round1_mc + ln_market_growth_mc*sbbe_round1_mc + appliance*sbbe_round1_mc+
                  ln_gdppercap2010_mc*ln_brnovelty + ln_gini_mc*ln_brnovelty + sbbe_round1_mc*ln_brnovelty+ local_to_market*ln_brnovelty+
                  ln_market_herf_mc*ln_brnovelty + ln_market_growth_mc*ln_brnovelty + appliance*ln_brnovelty)

#formula = list(m4 = . ~ 1 + emerging + sbbe_round1 + ln_market_herf_mc + ln_market_growth_mc + appliance + ln_gini_mc + local_to_market)
vars= unique(c('rwpspr','wpswdst','llen'))

process_regs <- function(formula) {
  regs <- lapply(vars, function(varname) {
    fit=NULL
    lt = lapply(formula, function(form) lmer(update(form, elastlt ~ .),  control = lmerctrl, 
                                             REML = F, data = data.table(elast[variable==varname&!is.na(elastlt)]), weights=w_elastlt))
    return(lt)
  })}

#m<-lm(update(formula_basic[[1]], elastlt ~ .), data = elast[variable==vars[1]&!is.na(elastlt)], weights=w_elastlt)
#vif(m)

#out1=regmodel(formula=formula, dat=elast, model='lmer')


#cat("<P style='page-break-before: always'>")

#printout(out1, 'lt', title = tab(paste0('Regression with long-term elasticities'), prefix=''), vars=ordered_vars,  notes=notes_base, covariate_choices = covars)



library(stargazer)

# by variable: all
regs_unlisted = do.call('c', process_regs(formula))
lbls=rep(vars, each=length(regs_unlisted)/length(vars))

stargazer(regs_unlisted,type='html', column.labels=lbls, out = 'explore-countryfactors.html')


# by variable: all
regs_unlisted = do.call('c', process_regs(formula_brand))
lbls=rep(vars, each=length(regs_unlisted)/length(vars))

stargazer(regs_unlisted,type='html', column.labels=lbls, out = 'explore-categorybrandfactors.html')

# development indicators
regs_unlisted = do.call('c', process_regs(formula2))
lbls=rep(vars, each=length(regs_unlisted)/length(vars))

stargazer(regs_unlisted,type='html', column.labels=lbls, out = 'explore-devindicators.html')

# development indicators
regs_unlisted = do.call('c', process_regs(list(formula$m1)))
lbls=rep(vars, each=length(regs_unlisted)/length(vars))

stargazer(regs_unlisted,type='html', column.labels=lbls, out = 'explore-singleresults.html')



#regs_unlisted = do.call('c', process_regs(formula3))
#lbls=rep(vars, each=length(regs_unlisted)/length(vars))#
#
#stargazer(regs_unlisted,type='html', column.labels=lbls, out = 'output-interactions.html')


