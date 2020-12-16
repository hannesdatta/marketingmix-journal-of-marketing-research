
rm(list=ls())
# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)
library(shiny)


library(car)
library(knitr)

fns <- c('app_workspace.RData')


for (fn in fns) if (file.exists(fn)) {cat(paste0('loading...', fn, '...\n')); load(fn)}


brand_panel=fread('../../../analysis/temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]

# Load auxilary functions
source('../proc_auxilary.R')
source('../proc_rename.R')



#_maxiter
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
    list(pr = lmer(update.formula(elastlt~1, forms$pr),
                   data=elast[grep('pr',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         llen = lmer(update.formula(elastlt~1, forms$llen),
                   data=elast[grep('llen',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         dst = lmer(update.formula(elastlt~1, forms$dst),
                   data=elast[grep('dst',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F))
  })
}
# estimate models
newmodV2 <- function(model, fn, ...) {
  
  mods = all_mods(model)
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  
  #sapply(names(mods), function(mo) paste0(mo, '_', c('price', 'llen length','distr'))
  
  #paste('')
  stargazer(do.call('c', mods),type='html', 
            column.labels = rep(c('price','line length','distribution'), length(model)), 
            out = fn, add.lines = list(r2s,obs), ...)
  return(mods)
}

lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")





#########################################################
##### NEW:  #
#########################################################

elast = elasticities$ec_restricted_sigcop


# Ingelhardt
mainef= . ~ 1 + (1|country) + (1|category) + (1|brand) + 
  sbbe_round1_mc + local_to_market_mc + ln_brandnovelty3_mc+
  ln_gdppercapita2010_mc + ln_ginicoef_mc + 
  
  ln_market_herf_mc + ln_market_growth_mc


brand = c('sbbe_round1_mc','local_to_market_mc','ln_rwpspr_index_mc', 'ln_wpswdst_index_mc', 'ln_llen_index_mc')
category = c("ln_market_herf_mc","ln_market_growth_mc", "appliance")
country_econ = c("ln_gdpgrowthavg_mc","ln_ginicoef_mc","ln_tradeopenessavg_mc")
country_culture = c('tradrat_mc', 'survself_mc')
country_context = c('wgi_regulatoryqualavg_mc')


tval = qnorm(.95)

mainef= . ~ 1 + (1|brand) + (1|category) + (1|country) + 
  sbbe_round1_mc + local_to_market_mc + ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+
  ln_market_herf_mc + ln_market_growth_mc+appliance


my_form = list(m0=mainef,
               m1a=update.formula(mainef, formula(paste0('.~.+', paste(country_econ,collapse='+')))),
               m1b=update.formula(mainef, formula(paste0('.~.+', paste(country_culture,collapse='+')))),
               m1c=update.formula(mainef, formula(paste0('.~.+', paste(country_context,collapse='+'))))
               )

my_form <- lapply(my_form, function(x) list(pr=x, dst=x, llen=x))

my_models <- all_mods(my_form)


# extract coefficients
coefs <- rbindlist(lapply(names(my_models), function(m) {
  
  rbindlist(lapply(names(my_models[[m]]), function(dv) {
    res=data.table(summary(my_models[[m]][[dv]])$coefficients, m=m, dv=dv)
    setnames(res, c('est','se','t','model','dv'))
    v=(rownames(summary(my_models[[m]][[dv]])$coefficients))
    res[, variable:=v]
    res[, p:=(1-pnorm(abs(t)))*2]
    setcolorder(res, c('model','dv','variable','est','se','t','p'))
    return(res)
  }))
    
  }))

#coefs[, keep_2a:=T]
#coefs[grepl("^m1", model) & p>.1, keep_2a:=F]

# reassmeble


# M2a
forms <- coefs[grepl('^m1', model)&!grepl('Intercept',variable), list(keep=any(p<=.1)|variable%in%c(brand, category)), by = c('dv','variable')]

forms = forms[keep==T, paste0(variable, collapse=' + '), by = c('dv')]

m2a = list(pr=formula(paste0('. ~ 1 + (1|country) + (1|category) + (1|brand) + ', forms[dv=='pr']$V1, collapse='+')),
           llen=formula(paste0('. ~ 1 + (1|country) + (1|category) + (1|brand) + ', forms[dv=='llen']$V1, collapse='+')),
           dst=formula(paste0('. ~ 1 + (1|country) + (1|category) + (1|brand) + ', forms[dv=='dst']$V1, collapse='+')))



# M2b
forms <- coefs[grepl('^m1', model)&!grepl('Intercept',variable), list(sig=any(p<=.1)), by = c('dv','variable')]

forms = forms[sig==T, paste0(variable, collapse=' + '), by = c('dv')]

m2b = list(pr=formula(paste0('. ~ 1 + (1|country) + (1|category) + (1|brand) + ', forms[dv=='pr']$V1, collapse='+')),
           llen=formula(paste0('. ~ 1 + (1|country) + (1|category) + (1|brand) + ', forms[dv=='llen']$V1, collapse='+')),
           dst=formula(paste0('. ~ 1 + (1|country) + (1|category) + (1|brand) + ', forms[dv=='dst']$V1, collapse='+')))




my_form2 <- my_form
my_form2$m2a <- m2a
my_form2$m2b <- m2b

input <- list(trimming='trim_1')

perc_extract = as.numeric(gsub('.*[_]','', input$trimming))/100


elast <- elast[!is.na(elastlt), percentile:=ecdf(elastlt)(elastlt), by = c('variable')]


elast[, perc_low := quantile(elastlt, probs = perc_extract), by = c('variable')]
elast[, perc_high := quantile(elastlt, probs = 1-perc_extract), by = c('variable')]
  
elast[percentile<perc_extract, elastlt:=perc_low]
elast[percentile>(1-perc_extract), elastlt:=perc_high]


newmodV2(my_form2, 'testresults.html')





my_models <- all_mods(my_form)
  
updt_forms <- 



}
summary(my_models[[1]]$pr)$coefficients


$coefficients




brandinteracts <- unlist(lapply(c(category,country), function(i) {
  unlist(lapply(brand, function(b) update.formula(mainef,formula(paste0('. ~ . + ', paste0(b,':',i))))))
}                    ))

models<-newmod(brandinteracts, fn = NULL)

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

#qnorm(.95)

forms = get_formulas(interacts[abs(`t value`)>=tval])
#forms2 = get_formulas(interacts[modeltype%in%1:18&abs(`t value`>=1.644854)])

#mods=newmodV2(list(forms), '../temp/explore-new-interactions18.html')


###################################################################################
##### NEW: 26 interactions: brand x category, brand x country, country x category #
###################################################################################


combinations=data.table(rbind(expand.grid(brand, category),
                              expand.grid(brand, country),
                              expand.grid(category,country)))

setnames(combinations, c('index1','index2'))
combinations = data.frame(combinations)

brandinteracts <- unlist(lapply(1:nrow(combinations), function(ind) {
  update.formula(mainef,formula(paste0('. ~ . + ', paste0(combinations[ind,1], ':', combinations[ind,2]))))
}))

models<-newmod(brandinteracts, fn = NULL)

names(models) <- 1:length(models)

# collect interaction terms
interacts=rbindlist(lapply(1:length(models), function(x) rbindlist(lapply(1:3, function(i) data.table(modeltype=x, var_index=i, varname=rownames(summary(models[[x]][[i]])$coefficients), summary(models[[x]][[i]])$coefficients)))))
interacts[, variable:=c('pr','llen','dst')[var_index]]
interacts <- interacts[grepl('[:]',varname)]

maineffects = mainef

forms26 = get_formulas(interacts[abs(`t value`)>=tval])

##### 36

comb <-c(brand, category,country)

combinations=data.table(expand.grid(c(1:length(comb)), c(1:length(comb))))
setnames(combinations, c('index1','index2'))
combinations <- data.frame(combinations[index1<index2])


brandinteracts <- unlist(lapply(1:nrow(combinations), function(ind) {
  update.formula(mainef,formula(paste0('. ~ . + ', paste0(comb[combinations[ind,1]], ':', comb[combinations[ind,2]]))))
}))

models<-newmod(brandinteracts, fn = NULL)#'../temp/explore-new-full-brandinteractions.html')

names(models) <- 1:length(models)

# collect interaction terms
interacts=rbindlist(lapply(1:length(models), function(x) rbindlist(lapply(1:3, function(i) data.table(modeltype=x, var_index=i, varname=rownames(summary(models[[x]][[i]])$coefficients), summary(models[[x]][[i]])$coefficients)))))
interacts[, variable:=c('pr','llen','dst')[var_index]]
interacts <- interacts[grepl('[:]',varname)]

maineffects = mainef


forms36 = get_formulas(interacts[abs(`t value`)>=tval])


combinations=data.frame(rbind(expand.grid(brand, category),
                              expand.grid(brand, country),
                              expand.grid(category,country)))


ord <- c(brand,category,country,unlist(lapply(1:nrow(combinations), function(ind) {
  paste0(as.character(combinations[ind,1]), ':', as.character(combinations[ind,2]))
})))

ord = paste0('^',ord,'$')

mods=newmodV2(list(list(pr=mainef, llen=mainef, dst=mainef), forms,forms26, forms36), '../temp/explore-new-interactions-18-26-36-hofstede-bombaji-dekimpe.html', order=ord)


