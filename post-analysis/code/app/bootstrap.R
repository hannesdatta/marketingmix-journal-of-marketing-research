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



setkey(elasticities$with_sur, category,country,brand)
setkey(elasticities$marketshare, category,country,brand)
elasticities$marketshare[elasticities$with_sur, brand_id:=i.brand_id]

bstrap <- function(elast,
                   var_select = c('ln_gdpgrowthyravg_mc', 'ln_gdppercapitacurrentyravg_mc', 'ln_ginicoef_mc'),
                   focal_vars = c('survself_mc', 'tradrat_mc'),
                   rep=1000) {
  
  
 
  
  dat <- unique(elast, by = c('brand_id'))[, c('brand_id', focal_vars, var_select),with=F]
  #rep=100
  
  bstrap_X <- rep(double(length(var_select)*nrow(dat)*c(rep)))
  dim(bstrap_X) <- c(nrow(dat), length(var_select), rep)
  
  bstrap_y <- rep(double(length(focal_vars)*nrow(dat)*c(rep)))
  dim(bstrap_y) <- c(nrow(dat), length(focal_vars), rep)
  
  set.seed(1234)
  base_X = as.matrix(dat[, var_select,with=F])
  base_y = as.matrix(dat[, focal_vars,with=F])
  
  for (i in seq(length.out=rep)) {
    smpl=sample(1:nrow(base_X), size=nrow(base_X), replace=T)
    bstrap_X[,,i]<-base_X[smpl,]
    bstrap_y[,,i]<-base_y[smpl,]
  }
  
  colnames(bstrap_X) <- colnames(base_X)
  colnames(bstrap_y) <- colnames(base_y)
  
  
  bstrap_values = lapply(focal_vars, function(colY) {
  
    out<-  lapply(seq(length.out=dim(bstrap_X)[3]), function(i) {
        X=cbind(1, bstrap_X[,,i])
        y=bstrap_y[,colY,i]
        beta = (solve(t(X)%*%X))%*%(t(X)%*%cbind(y))
        pred <- cbind(1,base_X) %*% beta
        resid <- pred-base_y[, colY]
        return(resid)
    })
    
    tmp=as.matrix(do.call('cbind', out))
    #tmp[, brand_id:=dat$brand_id]
    #setnames(tmp, c(paste0(colY,'_', 1:rep), 'brand_id'))
    #setcolorder(tmp, 'brand_id')
    return(tmp)
  })
  
  # reorder
  bstrap_return <- list()
  for (i in 1:rep) {
    ret <- do.call('cbind', lapply(bstrap_values, function(val) val[,i]))
    colnames(ret) <- c(paste0(focal_vars, '_bootstrap'))
    bstrap_return[[i]] <- data.table(brand_id=dat$brand_id, ret, key = 'brand_id')
  }
  
  return(bstrap_return)
}
    

# this can be done before (e.g., list of rep 10, 50, 100, 500, 1000)


# bootstrapping
bstrap_values <- lapply(elasticities, bstrap, rep = 10)


# Bstrap LM
load('inputs.RData')
input<-saved_input
input$culture <- paste0(input$culture, '_bootstrap')


# build formula


bstrap_select = bstrap_values[[input$model]]

  
calculate_ses <- function(elast, bstrap_select, modeltype='lm') {
  rep = length(bstrap_select)
  index <- elast$brand_id
  
  elastfocal <- list()
  
  mspec = get_model(input)
  
  elast = get_sample(input)
  
  covars = gsub('[_]bootstrap$', '', all.vars(update.formula(elastlt~ ., mspec$formula)))
  
  bootstrap <- grep('[_]bootstrap$', all.vars(update.formula(elastlt~ ., mspec$formula)), value=T)
  
  vars <- c('brand_id', 'variable', 'elastlt', 'w_elastlt', covars)
  
  
  for (i in 1:rep){
    elastfocal[[i]] <- cbind(elast[, vars,with=F], bstrap_select[[i]][index,])
  }
  
  #add_covars <- covars
  #add_covars[covars%in%bootstrap] <- paste0(bootstrap,'_bootstrap')
  
  ses=lapply(c('pr','llen','dst'), function(.v) {
    bs_coefs = do.call('cbind', lapply(elastfocal, function(df) {
      if (mspec$modeltype=='lm') lm(update.formula(elastlt~.,mspec$formula), data=df[grepl(.v,variable)], weights=w_elastlt)$coefficients
      if (mspec$modeltype=='lmer') attr(lmer(update.formula(elastlt~.,mspec$formula), data=df[grepl(.v,variable)], weights=w_elastlt,
                                  control = lmerctrl, REML=F), 'beta')
      
    }))
    
    standarderrors = apply(bs_coefs, 1, sd)
  
    return(standarderrors)
  })
  names(ses) <- c('pr','llen','dst')
return(ses)
}


# Erst LM
elast = elasticities[[input$model]]
bstrap_select = bstrap_values[[input$model]]

ses <- calculate_ses(elast, bstrap_select)


# actual model

# do form


vars=paste0(c(unlist(input$brandequity),
              unlist(input$brandlocation),
              unlist(input$brandmmix),
              unlist(input$brandother),
              
              unlist(input$categoryfactors), 
              unlist(input$econ),
              unlist(input$culture),
              unlist(input$institutions)), collapse='+')


modeltype=input$estim
if (!is.null(input$estim)) modeltype=input$estim

randomef = sapply(input$randomeffects, function(x) paste0('(1|', x, ')'))
if (length(randomef)>0 & modeltype=='lmer') myform = as.character(paste0('. ~ 1 + ', paste(randomef, collapse='+')))
if (length(randomef)==0|modeltype%in%c('lm')) myform = as.character(paste0('. ~ 1'))

clust = sapply(input$randomeffects, function(x) paste0(x))
if (length(clust)>0 & modeltype=='lm') clust = formula(paste0('~', paste(input$randomeffects,collapse='+')))
if (length(clust)==0| !modeltype=='lm') clust = NULL
cat(file=stderr(), as.character(clust), " clusters", "\n")

if (nchar(vars)>0) myform = paste0(myform, ' + ', vars)
if (nchar(unlist(input$interact))>0) myform = paste0(myform, ' + ', unlist(input$interact))

mainef = as.formula(myform)




mods = all_mods(list(list(pr=mainef, dst=mainef, llen=mainef)), mtype=modeltype, clust=clust)



m2 <- lm(elastlt~1+sbbe_round1_mc+survself_mc, data = elast[grepl(.v, variable)])
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

