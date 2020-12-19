#rm(list=ls())
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

fns <- c('app_workspace.RData')

for (fn in fns) if (file.exists(fn)) {cat(paste0('loading...', fn, '...\n')); load(fn)}




# Bstrap LM
load('inputs.RData')
input<-saved_input
#input$culture <- paste0(input$culture, '_bootstrap')
input$bootstrap_reps=10

# build formula


# Erst LM


elast = get_sample(input)
mspec = get_model(input)


if (input$bootstrap_used) {
  bstrap_select = bstrap(elasticities[[input$model]], input)
  ses <- calculate_ses(input, elast, bstrap_select)
}

#formula_wo_bootstrap = all.vars(mspec$formula)


mods = all_mods(list(list(pr=mspec$formula_without_bootstrap, dst=mspec$formula_without_bootstrap, llen=mspec$formula_without_bootstrap)),
                mtype=mspec$modeltype,
                clust=mspec$cluster)

mods2 = lapply(seq(along=ses), function(i) {
  
  outt=summary(mods[[1]][[i]])$coefficients
  print(outt)
  if (input$bootstrap_used==T) outt[,2] <- outt[,2] + ses[[i]]
  outt[,3]= outt[,1]/outt[,2]
  
  outt= cbind(outt, 2*(1-pnorm(abs(outt[,3]))))
  colnames(outt) <-c("Estimate","Std. Error","t value","Pr(>|t|)")
  class(outt) <- 'coeftest'
  print(outt)
  outt})





# update SEs

outp=paste0(paste0(capture.output({stargazer(mods2, type='html')}), collapse=''),
            '<br><br>', mspec$formula, "<br><br>", paste0(as.character(mspec$cluster), collapse=''), '<br><br>', mspec$modeltype)





m2 <- lm(elastlt~1+sbbe_round1_mc+survself_mc, data = elast)
m2s=summary(m2)$coefficients
class(m2s) <- 'coeftest'
#attr(m2s, 'df')<- 12

  
stargazer(m2s, type='text')


m2b <- coeftest(m2, vcov = vcovCL, cluster = ~brand)
m2c <- m2b
m2c[,2] <- m2c[,2]+standarderrors

stargazer(m2, m2b, m2c, type='text')

# Done.

# ok. done.

# Idea: produce coefficients always


# update variance covariance matrix

# combine standard error


# Questions:
# - Can I add the OLS bootstrapped SEs to the RE bootstrapped ones? 
# Or do the bootstrapping of SEs also has to occur in an RE model (speed considerations)

