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

for (fn in fns) if (file.exists(fn)) {cat(paste0('loading...', fn, '...\n')); load(fn)}


brands <- elasticities$with_sur[, list(Ncountries = length(unique(country)), Ncategory=length(unique(category))),by=c('brand')]
setorderv(brands, c('Ncountries', 'Ncategory'), order=-1L)
brands <- brands[!grepl('alloth', brand)]

categories = unique(elasticities$with_sur$category)
countries = unique(elasticities$with_sur$country)



######## FUNCTIONS #######

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


all_mods <- function(models, mtype = 'lmer', clust = NULL, clust_type = 'HC1') {
  lapply(models, function(forms) {
    if (mtype=='lm')  {
      
      pr = lm(update.formula(elastlt~1, forms$pr),
              data=elast[grep('pr',variable)], weights=w_elastlt)
      dst = lm(update.formula(elastlt~1, forms$dst),
              data=elast[grep('dst',variable)], weights=w_elastlt)
      llen = lm(update.formula(elastlt~1, forms$llen),
              data=elast[grep('llen',variable)], weights=w_elastlt)
      avail_pr=setdiff(1:nrow(elast[grep('pr',variable)]), pr$na.action)
      avail_dst=setdiff(1:nrow(elast[grep('dst',variable)]), dst$na.action)
      avail_llen=setdiff(1:nrow(elast[grep('llen',variable)]), llen$na.action)
      
      pred_pr=data.table(elast[grepl('pr',variable), c('brand','brand_id','category','country', 'elastlt')][avail_pr], variable='pr', elast_lt_pred=predict(pr))
      pred_llen=data.table(elast[grepl('llen',variable), c('brand','brand_id','category','country', 'elastlt')][avail_llen], variable='llen',elast_lt_pred=predict(llen))
      pred_dst=data.table(elast[grepl('dst',variable), c('brand','brand_id','category','country', 'elastlt')][avail_dst], variable='dst', elast_lt_pred=predict(dst))
      preds= rbind(pred_pr, pred_llen, pred_dst)
      
      rsqs=unlist(lapply(list(pr,dst,llen),rsq))
      obs=unlist(lapply(list(pr,dst,llen),function(x) length(residuals(x))))
      aics = unlist(lapply(list(pr,dst,llen), function(x) AIC(x)))
      bics = unlist(lapply(list(pr,dst,llen), function(x) BIC(x)))
      
      
      if (!is.null(clust)) {
        pr <- coeftest(pr, vcov = vcovCL, cluster = clust, fix = T, type = clust_type)
        dst <- coeftest(dst, vcov = vcovCL, cluster = clust, fix = T, type = clust_type)
        llen <- coeftest(llen, vcov = vcovCL, cluster = clust, fix = T, type = clust_type)
        return(list(pr=pr,dst=dst, llen=llen, rsqs=rsqs, obs = obs, aic=aics, bic=bics,
                    predictions=preds))
        
      }
      
      return(list(pr=pr,dst=dst, llen=llen, rsqs=rsqs, obs = obs, aic=aics, bic=bics,
                  predictions=preds))
      
    }
    
    if (mtype=='lmer') {
     pr = lmer(update.formula(elastlt~1, forms$pr),
                data=elast[grep('pr',variable)], weights=w_elastlt,
                control = lmerctrl, REML=F)
     dst = lmer(update.formula(elastlt~1, forms$dst),
                data=elast[grep('dst',variable)], weights=w_elastlt,
                control = lmerctrl, REML=F)
     llen = lmer(update.formula(elastlt~1, forms$llen),
                        data=elast[grep('llen',variable)], weights=w_elastlt,
                        control = lmerctrl, REML=F)
     
     avail_pr=rownames(elast[grep('pr',variable)])%in%rownames(pr@frame)
     avail_dst=rownames(elast[grep('dst',variable)])%in%rownames(dst@frame)
     avail_llen=rownames(elast[grep('llen',variable)])%in%rownames(llen@frame)
     
     
     pred_pr=data.table(elast[grepl('pr',variable), c('brand','brand_id','category','country', 'elastlt')][avail_pr], variable='pr', elast_lt_pred=predict(pr))
     pred_llen=data.table(elast[grepl('llen',variable), c('brand','brand_id','category','country', 'elastlt')][avail_llen], variable='llen',elast_lt_pred=predict(llen))
     pred_dst=data.table(elast[grepl('dst',variable), c('brand','brand_id','category','country', 'elastlt')][avail_dst], variable='dst' ,elast_lt_pred=predict(dst))
     preds= rbind(pred_pr, pred_llen, pred_dst)
     
    return(list(pr=pr, dst=dst, llen=llen,predictions=preds))
    }
    
  })
}


lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")


orthogonalization <- function(elast, input, bootstrap = T) {
  
  var_select = input$orth_ivs 
  focal_vars = input$orth_dvs 
  rep = as.numeric(input$bootstrap_reps)
  
  dat <- unique(elast, by = c('brand_id'))[, c('brand_id', focal_vars, var_select),with=F]
  
  if (bootstrap==F) rep = 0
  
  bstrap_X <- rep(double(length(var_select)*nrow(dat)*c(rep+1)))
  dim(bstrap_X) <- c(nrow(dat), length(var_select), rep+1)
  
  bstrap_y <- rep(double(length(focal_vars)*nrow(dat)*c(rep+1)))
  dim(bstrap_y) <- c(nrow(dat), length(focal_vars), rep+1)
  
  set.seed(3515)
  base_X = as.matrix(dat[, var_select,with=F])
  base_y = as.matrix(dat[, focal_vars,with=F])
  
  for (i in seq(length.out=rep+1)) {
    if (i==1) smpl = 1:nrow(base_X)
    if (i>1) smpl=sample(1:nrow(base_X), size=nrow(base_X), replace=T)
    
    bstrap_X[,,i]<-base_X[smpl,]
    bstrap_y[,,i]<-base_y[smpl,]
  }
  
  colnames(bstrap_X) <- colnames(base_X)
  colnames(bstrap_y) <- colnames(base_y)
  
  bstrap_values = lapply(focal_vars, function(colY) {
    
    out<-  lapply(seq(length.out=dim(bstrap_X)[3]), function(i) {
      # estimate parameters on bootstrap sample
      X=cbind(1, bstrap_X[,,i])
      y=bstrap_y[,colY,i]
      compl = complete.cases(X)
      X=X[compl,]
      y=y[compl]
      
      beta = (solve(t(X)%*%X))%*%(t(X)%*%cbind(y))
      
      # generate residuals for observed data
      pred <- cbind(1,base_X) %*% beta
      resid <- base_y[, colY]-pred
      return(resid)
    })
    
    tmp=as.matrix(do.call('cbind', out))
    return(tmp)
  })
  
  # add brand IDs
  bstrap_return <- list()
  
  for (i in 1:c(rep+1)) {
    ret <- do.call('cbind', lapply(bstrap_values, function(val) val[,i]))
    colnames(ret) <- c(paste0(focal_vars, '_orth'))
    bstrap_return[[i]] <- data.table(brand_id=dat$brand_id, ret, key = 'brand_id')
  }
  
  return(list(bootstraps=bstrap_return[-1], means =bstrap_return[[1]] ))
}



#########################################################
##### Add variables to choice set #
#########################################################

names(elasticities)

model_names = list('M1) Error correction (sales, with SUR)' = 'with_sur',
                   'M2) Error correction (sales, without SUR)' = 'ec_restricted_sigcop',
                   #'M3) Error correction (sales; but with copula of d_mmix)' = 'ec_restricted_sigdcop',
                   'M3) Attraction model (market share)' = 'marketshare')
            
potential_vars_raw = list(brandequity=list('!SBBE' = 'sbbe_round1_mc',
                                       'BrandZ indicator' = 'brandz',
                                       'Brand strength (BAV)' = 'ln_bav_brandstrength_mc',
                                       'Brand stature (BAV)' = 'ln_bav_brandstature_mc',
                                       'Brand energized diff. (BAV)' = 'ln_bav_energizeddifferentiation_mc',
                                       'Marketshare' = 'ln_brand_ms_mc'),
                      brandlocation = list('!Domestic market indicator' = 'local_to_market_mc',
                                           '!JP, US, Swiss, GER, Sweden indicator' = "`brand_from_jp-us-ch-ge-sw_mc`",
                                           'Western brand indicator' = 'western_brand_mc'),
                      brandmmix = list('!Price (log index)' = 'ln_rwpspr_index_mc',
                                       '!Distr. (log index)' = 'ln_wpswdst_index_mc',
                                       '!Line length (log index)' = 'ln_llen_index_mc',
                                       'Price (index)' = 'rwpspr_index_mc',
                                       'Distr. (index)' = 'wpswdst_index_mc',
                                       'Line length (index)' = 'llen_index_mc',
                                       'Price (std.)' = 'rwpspr_std_mc',
                                       'Distr. (std.)' = 'wpswdst_std_mc',
                                       'Line length (std.)' = 'llen_std_mc'),
                      
                      brandother = list('!Brand novelty' = 'ln_brandnovelty3_mc'),#,
                                        #'Price positioning' = 'brand_prindex_mean_mc'),
                      
                      category = list('!Market concentration' = "ln_market_herf_mc",
                                      '!Market growth' = "ln_market_growth_mc",
                                      '!Appliances (vs. electronics)' = 'appliance',
                                      'Category innovativeness' = 'ln_catnovelty3_mc'
                                      ),
                      
                      country_econ = list('!GDP growth (obs. avg)' = 'ln_gdpgrowthyravg_mc',
                                          'GDP growth (avg)' = 'ln_gdpgrowthavg_mc',
                                          'GDP growth (2010)' = 'ln_gdpgrowth2010_mc',
                                          "!Income Inequality (nearest to 2010)" = "ln_ginicoef_mc",
                                          
                                          '!GDP per capita (obs. avg)' = "ln_gdppercapitacurrentyravg_mc",
                                          'GDP per capita (avg)' = "ln_gdppercapitacurrentavg_mc",
                                          'GDP per capita (2010)' = "ln_gdppercapitacurrent2010_mc",
                                          
                                          "Trade openess (obs. avg)" = 'ln_tradeopenessyravg_mc',
                                          "Trade openess (avg)" = 'ln_tradeopenessavg_mc',
                                          "Trade openess (in 2010)" = 'ln_tradeopeness2010_mc',
                                          
                                          'HDI (2010)' = 'ln_hdi2010_mc',
                                          
                                          "Emerging market indicator" = "emerging",
                                          "Goods Market Efficiency (GCI 2010)" = "ln_gci_p06_goods_s_mc",
                                          "Infrastructure (GCI 2010)" = 'ln_gci_p02_infrastructure_s_mc',
                                          "Market size (GCI 2010)" = 'ln_gci_p10_marketsize_s_mc'
                                          
                                          ),
                      
                      country_culture = list("!WVS: Traditional vs. rational" = "tradrat_mc",
                                             "!WVS: Survival vs. Self-expression" = "survself_mc",
                                             "Hofstede: Longterm orientation" = "ln_ltowvs_mc",
                                             "Hofstede: Individualism" = "ln_idv_mc",
                                             "Hofstede: Uncertainty Avoidance" = "ln_uai_mc",
                                             "Hofstede: Power distance" = "ln_pdi_mc",
                                             "Hofstede: Masculinity" = "ln_mas_mc"),
                      country_institutions = list(#"Rule of law" = "ln_wjp_rule_of_law_mc",
                        "WGI Voice and Accountability (obs. avg)" = 'wgi_accountabilityyravg_mc',
                        #"WGI Voice and Accountability (avg)" = 'wgi_accountabilityavg',
                        "WGI Control of Corruption (obs. avg)" = 'wgi_corruptionctrlyravg_mc',
                        #"WGI Control of Corruption (avg)" = 'wgi_corruptionctrlavg',
                        "WGI Government Effectiveness (obs. avg)" = 'wgi_governmenteffectivenessyravg_mc',
                        #"WGI Government Effectiveness (avg)" = 'wgi_governmenteffectivenessavg',
                        "WGI Regulatory Quality (obs. avg)" = 'wgi_regulatoryqualyravg_mc',
                        #"WGI Regulatory Quality (avg)" = 'wgi_regulatoryqualavg',
                        "WGI Rule of Law (obs. avg)" = 'wgi_ruleoflawyravg_mc',
                        #"WGI Rule of Law (avg)" = 'wgi_ruleoflawavg',
                        "WGI Political Stability (obs. avg)" = 'wgi_stabilityyravg_mc',
                        #"WGI Political Stability (avg)" = 'wgi_stabilityavg',
                        #"WGI Voice and Accountability (2010)" = 'wgi_accountability2010',
                         #                         "WGI Control of Corruption (2010)" = 'wgi_corruptionctrl2010',
                          #                        "WGI Government Effectiveness (2010)" = 'wgi_governmenteffectiveness2010',
                           #                       "WGI Regulatory Quality (2010)" = 'wgi_regulatoryqual2010',
                            #                      "WGI Rule of Law (2010)" = 'wgi_ruleoflaw2010',
                             #                     "WGI Political Stability (2010)" = 'wgi_stability2010',
                                                  "Institutions (GCI)" = 'ln_gci_p01_institutions_s_mc'
                            #                     "Infrastructure (GCI)" = 'ln_gci_p02_infrastructure_s_mc'
                                                  )
)

potential_choices <- lapply(potential_vars_raw, function(x) {
  ret = (unlist(x)[grepl('^[!]',names(x))])
  names(ret) <- gsub('^[!]','', names(ret))
  return(ret)
})

potential_vars <- lapply(potential_vars_raw, function(x) {
    names(x) <- gsub('^[!]','', names(x))
    x
})
  
  

potential_vars_unlisted <- unlist(potential_vars)
names(potential_vars_unlisted) <-unlist(lapply(potential_vars,names))



# variable check
all_cols = unique(unlist(lapply(elasticities, colnames)))
unlist(lapply(potential_vars, function(x) x[!x%in%all_cols]))

#grep('from', all_cols,value=T)

trimming <- list('None' = '',
                'Trimmed 1% two-sided' = 'trim_1',
                 'Trimmed 2.5% two-sided' = 'trim_2.5',
                'Winsorized 1% two-sided' = 'wins_1',
                'Winsorized 2.5% two-sided' = 'wins_2.5')


ui_new <- fluidPage(
  
  titlePanel("GfK Singapore"),
  
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 500,
                  min = 1,
                  max = 1000)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
  
)

ui <- fluidPage(
  
  titlePanel("GfK Singapore"),
  
  fluidRow(
    
    column(4,
           tabsetPanel(type = "tabs",
                       tabPanel("Variables", 
             selectInput("brandequity", label = h5("Brand factors: Equity"),
                         choices = (potential_vars$brandequity), selected = potential_choices$brandequity, multiple=TRUE),
             selectInput("brandlocation", label = h5("Brand factors: Location"),
                         choices = (potential_vars$brandlocation), selected = potential_choices$brandlocation, multiple=TRUE),
             selectInput("brandmmix", label = h5("Brand factors: Marketing mix"),
                         choices = (potential_vars$brandmmix), selected = potential_choices$brandmmix, multiple=TRUE),
             selectInput("brandother", label = h5("Brand factors: Others"),
                         choices = (potential_vars$brandother), selected = potential_choices$brandother, multiple=TRUE),
             
             selectInput("categoryfactors", label = h5("Category factors"),
                         choices = (potential_vars$category), selected = potential_choices$category, multiple=TRUE),
             selectInput("econ", label = h5("Country factors: Economic development"),
                         choices = (potential_vars$country_econ), selected = potential_choices$country_econ, multiple=TRUE),
             selectInput("culture", label = h5("Country factors: Culture"),
                         choices = (potential_vars$country_culture), selected = potential_choices$country_culture, multiple=TRUE),
             selectInput("institutions", label = h5("Country factors: Institutions"),
                         choices = (potential_vars$country_institutions), selected = potential_choices$country_institutions, multiple=TRUE),
             textInput("interact", label = h5("Interactions"), value = "")#,
             #downloadButton("downloadData", "Download model specification")
           ),
           tabPanel("Model specification", 
             selectInput("model", label = h5("Model estimation (first stage)"),
                       choices = model_names,
                       selected=model_names[1]),
             
             br(),
             selectInput("estim", label = h5("Model estimation (second stage)"),
                       choices = list('Mixed-Effects Model (REs)'= 'lmer',
                                      'OLS' = 'lm'), selected= 'lm', multiple=F),
             selectInput("randomeffects", label = h5("List of random effects (for RE model), or clustering (in case of OLS)"),
                       choices = list('Brand'= 'brand', 'Category'='category',
                                      'Country' = 'country'), selected = c('brand', 'category','country'), multiple=TRUE),
             #selectInput("clustering_type", label = h5("Clustering type (for OLS)"),
             #            choices = list('HC0'= 'HC0', 'HC1'='HC1',
             #                           'HC2'= 'HC2', 'HC3'='HC3'), selected = c('HC1'), multiple=FALSE),
             
             selectInput("trimming", label = h5("Trimming/Winsorizations"),
                       choices = (trimming), selected = trimming[4], multiple=FALSE)),
           
           tabPanel("Orthogonalization and bootstrapping", 
                    radioButtons("orth_used", label = h5("Use orthogonalization (e.g., Batra et al. 2000; ter Braak et al. 2013)"),
                                 c("Yes" = T,
                                   "No" = F),
                                 selected = F),
                    selectInput("orth_dvs", label = h5("Orthogonalization of..."),
                                choices = potential_vars_unlisted, selected =grep('survself|tradrat|wgi[_]rugulator|wgi[_]ruleof', potential_vars_unlisted,ignore.case=T,value=T), multiple=TRUE),
                    selectInput("orth_ivs", label = h5("...using ind. variables"),
                                choices = potential_vars_unlisted, selected = c('ln_gdpgrowthyravg_mc', 'ln_gdppercapitacurrentyravg_mc', 'ln_ginicoef_mc'), multiple=TRUE),
                    radioButtons("bootstrap_used", label = h5("Use bootstrapping (only when orthogonalization used)"),
                                 c("Yes" = T,
                                   "No" = F),
                                 selected = F),
                    selectInput("bootstrap_reps", label = h5("Bootstrap samples"),
                                choices = c(10,20,50,100), selected = 10, multiple=FALSE)
                    
                    
                    
                    
           ),
           tabPanel("Plotting", 
                    selectInput("plot_vars", label = h5("Variables"),
                                choices = list('Price' = 'pr',
                                               'Line length' = 'llen',
                                               'Distribution' = 'dst'), 
                                selected = c('pr','llen','dst'), multiple=TRUE),
                   selectInput("plot_brands", label = h5("Brands"),
                               choices = sapply(str_to_title(brands$brand), function(x) tolower(x), simplify=F), 
                               selected = c('samsung', 'apple'), multiple=TRUE),
                   selectInput("plot_categories", label = h5("Categories"),
                               choices = sapply(str_to_title(categories), function(x) tolower(x), simplify=F), 
                               selected =c('phones_smart'), multiple=FALSE),
                   selectInput("plot_countries", label = h5("Countries"),
                               choices = sapply(str_to_title(countries), function(x) tolower(x), simplify=F), 
                               selected = countries, multiple=TRUE),
                   radioButtons("plot_predicted", label = h5("Use estimated or predicted elasticities?"),
                                c("Estimated" = 'elastlt',
                                  "Predicted" = 'elastlt_pred'),
                                selected = 'elastlt_pred'))
                   #,
                  # selectInput("plot_splits", label = h5("Split plots by"),
                  #             choices = list('Brand'= 'brand', 'Category'='category',
                  #                            'Country' = 'country'), selected = c('brand', 'category','country'), multiple=TRUE))
           #selectInput("orth_ivs", label = h5("...using ind. variables"),
#                                choices = potential_vars_unlisted, selected = c('ln_gdpgrowthyravg_mc', 'ln_gdppercapitacurrentyravg_mc', 'ln_ginicoef_mc'), multiple=TRUE),
 #                   radioButtons("bootstrap_used", label = h5("Use bootstrapping (only when orthogonalization used)"),
                                 #c("Yes" = T,
                                  # "No" = F),
                                 #selected = F),
                    #selectInput("bootstrap_reps", label = h5("Bootstrap samples"),
                                #choices = c(10,20,50,100), selected = 10, multiple=FALSE)
                    
                    
                    
                    
           )
           
           
           
    ),
    column(8,
           tabsetPanel(type = "tabs",
                       
                       # tabPanel("Model Summary", verbatimTextOutput("summary")),
                       tabPanel("Model results", htmlOutput("stargazer")),#, # Regression output
                       tabPanel("VIFs", htmlOutput("vif")),
                       tabPanel("Plots (brand-specific bar plots)", plotOutput('plot')),
                       tabPanel("Plots (scatter plots)", plotOutput('surface')),
                       
                       #tabPanel("VIFs2", htmlOutput("vif2")),
                       #tabPanel("Elasticities", DT::dataTableOutput("elasticities")),
                       
                       #}, options = list(searching = FALSE)
                       tabPanel("Tables for the paper", includeHTML("tables.html")),
                       tabPanel("Downloads", br(),
                                downloadButton("downloadData", "Download model specification (RData file)"),
                                br(),
                                downloadButton("downloadElast", "Download estimated and predicted elasticities (CSV file)"),
                                br(),
                                downloadButton("downloadElastXLS", "Download estimated and predicted elasticities (Excel file)"))
                              #br(),
                                #downloadButton("downloadregdata", "Download data for second-stage regressions (csv file)"))#,
                     #  tabPanel("Correlations", htmlOutput("correl"))#, # Regression output
                       #, # Regression output
                       # tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
        #  htmlOutput("stargazer")
    ))
  
)
  
)


get_model <- function(input) {
  vars_orig = c(unlist(input$brandequity),
                unlist(input$brandlocation),
                unlist(input$brandmmix),
                unlist(input$brandother),
                
                unlist(input$categoryfactors), 
                unlist(input$econ),
                unlist(input$culture),
                unlist(input$institutions))
  
  
  vars <- vars_orig
  if (as.logical(input$orth_used)) vars[vars%in%input$orth_dvs] <-paste0(vars[vars%in%input$orth_dvs], '_orth')
  vars_array=vars
  
  vars=paste0(vars, collapse='+')
  vars_orig=paste0(vars_orig, collapse='+')
  
  modeltype=input$estim
  if (!is.null(input$estim)) modeltype=input$estim
  
  
  randomef = sapply(input$randomeffects, function(x) paste0('(1|', x, ')'))
  if (length(randomef)>0 & modeltype=='lmer') myform = as.character(paste0('. ~ 1 + ', paste(randomef, collapse='+')))
  if (length(randomef)==0|modeltype%in%c('lm')) myform = as.character(paste0('. ~ 1'))
  
  clust = sapply(input$randomeffects, function(x) paste0(x))
  if (length(clust)>0 & modeltype=='lm') clust = formula(paste0('~', paste(input$randomeffects,collapse='+')))
  if (length(clust)==0| !modeltype=='lm') clust = NULL
  #cat(file=stderr(), as.character(clust), " clusters", "\n")
  
  if (nchar(vars)>0) myform = paste0(myform, ' + ', vars)
  
  if (nchar(unlist(input$interact))>0) myform = paste0(myform, ' + ', unlist(input$interact))
  
  
  list(formula = myform, variables = vars_array, modeltype=modeltype, randomeffects = randomef, cluster = clust,
       clustering_type = 'HC1')#input$clustering_type)
}

get_sample <- function(input) {
  tmp <- copy(elasticities[[input$model]][selection_obs48==T&selection_brands==T])
  
  tmp <- tmp[!is.na(elastlt), percentile:=ecdf(elastlt)(elastlt), by = c('variable')]
  
  perc_extract = as.numeric(gsub('.*[_]','', input$trimming))/100
  
  if (grepl('^trim', input$trimming, ignore.case=T)) {
    tmp <- tmp[percentile>=perc_extract & percentile<=(1-perc_extract)]
  }
  
  if (grepl('win', input$trimming, ignore.case=T)) {
    tmp[, perc_low := quantile(elastlt, probs = perc_extract), by = c('variable')]
    tmp[, perc_high := quantile(elastlt, probs = 1-perc_extract), by = c('variable')]
    
    tmp[percentile<perc_extract, elastlt:=perc_low]
    tmp[percentile>(1-perc_extract), elastlt:=perc_high]
    
  }
  return(tmp)
}



calculate_ses <- function(input, elast, bstrap_select) {
  
  rep = length(bstrap_select)
  index <- elast$brand_id
  
  mspec = get_model(input)
  
  
  covars = gsub('[_]orth$', '', all.vars(update.formula(elastlt~ ., mspec$formula)))
  
  bootstrap <- grep('[_]orth$', all.vars(update.formula(elastlt~ ., mspec$formula)), value=T)
  
  vars <- c('brand_id', 'variable', 'elastlt', 'w_elastlt', covars)
  
  elastfocal <- list()
  
  for (i in 1:rep){
    elastfocal[[i]] <- cbind(copy(elast[, vars,with=F]), bstrap_select[[i]][match(index, brand_id),])
  }
  
  ses=lapply(c('pr','dst','llen'), function(.v) {
    cntr=0
    bs_coefs = do.call('cbind', lapply(elastfocal, function(df) {
      cntr=cntr+1
      
      if (mspec$modeltype=='lm') o=lm(update.formula(elastlt~.,mspec$formula), data=df[grepl(.v,variable)], weights=w_elastlt)$coefficients
      if (mspec$modeltype=='lmer') o=attr(lmer(update.formula(elastlt~.,mspec$formula), data=df[grepl(.v,variable)], weights=w_elastlt,
                                             control = lmerctrl, REML=F), 'beta')
      return(o)
    }))
    
    standarderrors = apply(bs_coefs, 1, sd)
    
    return(standarderrors)
  })
  names(ses) <- c('pr','dst','llen')
  return(ses)
}


result_storage <<- NULL
bootstrap_storage <<- NULL
data_storage <<- NULL


produce_model <- function(input) {
  
  my_hash <- function(input, ...) digest(paste0(paste0(names(input), collapse='_'), '|', paste0(sapply(unlist(input), function(x) if(x=='') return('[ ]') else return(x)), collapse='_')),...)
  
  hash=try(my_hash(reactiveValuesToList(input), algo=c("md5")),silent=T)
  if(class(hash)=='try-error') hash=try(my_hash(input, algo=c("md5")),silent=T)
  
  
  
  outp = result_storage[[hash]]$results
  dstorage = data_storage[[hash]]
  
  if (is.null(outp)) {
    
    mspec = get_model(input)
    
    elast <<- get_sample(input)
    
    if (!class(input)=='list') bstrap_input = reactiveValuesToList(input) else bstrap_input=input
    bstrap_input = bstrap_input[grepl('bootstrap|trimming|^model$|^orth',names(bstrap_input))]
    bstrap_hash=my_hash(bstrap_input, algo=c("md5"))
    
    if (as.logical(input$orth_used==T)) {
      # insert orthogonal values in reg.
      #orth_from_storage = bootstrap_storage[[bstrap_hash]]
      
      #if (is.null(bstrap_from_storage)) {
      #print('calculating bootstrapping values')
      #bstrap_from_storage = bstrap(elast, input)
      #bootstrap_storage[[bstrap_hash]] <<- bstrap_from_storage
      
      orthog = orthogonalization(elast, input, bootstrap = as.logical(input$bootstrap_used))
      setkey(orthog$means, brand_id)
      setkey(elast, brand_id)
      for (.v in input$orth_dvs) elast[orthog$means, paste0(.v,'_orth'):=get(paste0('i.', .v, '_orth'))]
      elast <<- elast
    }
    
    
    if (as.logical(input$bootstrap_used) & as.logical(input$orth_used)) {
      showModal(modalDialog("Computing standard errors by bootstrapping procedure. ", footer=NULL))
      if (length(orthog$bootstraps)>20) showModal(modalDialog("Computing standard errors by bootstrapping procedure. This can take a couple of minutes.", footer=NULL))
      #print(paste0('starting to calculate SEs: ', ))
      ses <- calculate_ses(input, elast, orthog$bootstraps)
      #print('done calculating SEs')
      removeModal()
      
    }
    
    
    mods = all_mods(list(list(pr=mspec$formula, dst=mspec$formula, llen=mspec$formula)),
                    mtype=mspec$modeltype,
                    clust=mspec$cluster, clust_type = mspec$clustering_type)
    
    mods2 = lapply(seq(along=c('pr','dst','llen')), function(i) {
      
      outt = try(summary(mods[[1]][[i]])$coefficients, silent=T)
      if (class(outt)=='try-error') outt = mods[[1]][[i]]
      
      if (as.logical(input$bootstrap_used) & as.logical(input$orth_used)) outt[,2] <- sqrt(outt[,2]^2 + ses[[i]]^2)
      outt[,3]= outt[,1]/outt[,2]
      
      outt= cbind(outt[,1:3], 2*(1-pnorm(abs(outt[,3]))))
      colnames(outt) <-c("Estimate","Std. Error","t value","Pr(>|t|)")
      class(outt) <- 'coeftest'
      outt})
    
    
    lbllist = NULL
    
    if (mspec$modeltype=='lmer') {
      rsqs=unlist(lapply(mods, function(x) lapply(x[c('pr','dst','llen')], rsq)))
      obss = unlist(lapply(mods, function(x) lapply(x[c('pr','dst','llen')], function(i) length(which(!is.na(residuals(i)))))))
      fits = lapply(mods[[1]][c('pr','dst','llen')], function(i) summary(i)$AICtab)
      aics = unlist(lapply(fits, function(x) x['AIC']))
      bics = unlist(lapply(fits, function(x) x['BIC']))
      logliks = unlist(lapply(mods[[1]][c('pr','dst','llen')], function(x) as.numeric(logLik(x))))
      
      r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
      aic = c('AIC',sub('^(-)?0[.]', '\\1.', formatC(aics, digits=2, format='f', flag='#')))
      bic = c('BIC',sub('^(-)?0[.]', '\\1.', formatC(bics, digits=2, format='f', flag='#')))
      loglik = c('LL',sub('^(-)?0[.]', '\\1.', formatC(logliks, digits=2, format='f', flag='#')))
      obs = c('Observations',obss)
      
      lbllist = list(r2s,aic, bic,loglik, obs)
    } 
    
    if (mspec$modeltype=='lm') {
      rsqs=mods[[1]]$rsqs
      
      obss = mods[[1]]$obs
      
      aics = mods[[1]]$aic
      bics =mods[[1]]$bic
      
      r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
      aic = c('AIC',sub('^(-)?0[.]', '\\1.', formatC(aics, digits=2, format='f', flag='#')))
      bic = c('BIC',sub('^(-)?0[.]', '\\1.', formatC(bics, digits=2, format='f', flag='#')))
      obs = c('Observations',obss)
      
      lbllist = list(r2s,aic, bic,obs)
    } 
    
    m_ord = c(3,1,2)
    outp=paste0(paste0(capture.output({stargazer(mods2[m_ord], type='html', add.lines=lbllist,
                                                 column.labels = rep(c('price','distribution','line length')[m_ord], length(mods2)))}), collapse=''),
                '<br><br>', mspec$formula, "<br><br>", paste0(as.character(mspec$cluster), collapse=''), '<br><br>', mspec$modeltype)
    
    if (mspec$modeltype=='lmer') outp = paste0(outp, '<br><br>', paste0({lapply(mods[[1]][m_ord], function(x) {
      test= capture.output({summary(x)})
      # select
      start=grep('Random effects', test, ignore.case=T)
      end=grep('Fixed effect', test, ignore.case=T)
      return(paste0(test[start:c(end-1)], collapse='<br>'))
      
    })}, collapse='<br>'))
    
    setcolorder(mods[[1]]$predictions, c('category','country','brand','brand_id','variable','elastlt','elast_lt_pred'))
    setnames(mods[[1]]$predictions, 'elast_lt_pred','elastlt_pred')
    
    dstorage<<- mods[[1]]$predictions
    result_storage[[hash]]$results <<- paste0(outp, '<br>from memory<br>')
    
    data_storage[[hash]]$predictions <<- mods[[1]]$predictions
    
    #rm(me)
  }
  print(str(result_storage))
  print(str(bootstrap_storage))
  
  return(list(printed_model = outp, predictions = data_storage[[hash]]$predictions))# = data_)
}


# SERVER
server <- function(input, output) {
  
  
  output$stargazer = renderText({
  # assemble form 
   
    #saved_input = reactiveValuesToList(input)
    o=produce_model(input)
    
    return(o$printed_model)
  })

  output$plot <- renderPlot({
    o=produce_model(input)
    dt = o$predictions
    
    if (!is.null(input$plot_brands)) dt <- dt[brand%in%input$plot_brands]
    if (!is.null(input$plot_categories)) dt <- dt[category%in%input$plot_categories]
    if (!is.null(input$plot_countries)) dt <- dt[country%in%input$plot_countries]
    if (!is.null(input$plot_vars)) dt <- dt[variable%in%input$plot_vars]
    
    ggplot(dt, aes(fill=brand, y = eval(parse(text=input$plot_predicted)), x = variable)) + geom_bar(position='dodge2',stat='identity') + 
      facet_wrap( ~ country) + theme_tufte() + ylab(input$plot_predicted) +
      labs(#color='price elasticity',
             caption = 'Plot generated for SELECTED categories, countries, brands, and marketing mix elasticities.')
    
    
    
  })
  
  output$surface <- renderPlot({
    o=produce_model(input)
    dt = o$predictions
    
    #if (!is.null(input$plot_brands)) dt <- dt[brand%in%input$plot_brands]
    #if (!is.null(input$plot_categories)) dt <- dt[category%in%input$plot_categories]
    if (!is.null(input$plot_countries)) dt <- dt[country%in%input$plot_countries]
    #if (!is.null(input$plot_vars)) dt <- dt[variable%in%input$plot_vars]
    
    
    tmp = dcast(dt, category+country+brand~variable, value.var=input$plot_predicted)
    
   # library(plotly)
    
   # kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
    
    
    #fig <- plot_ly() %>% add_trace(data = tmp[!is.na(llen)],  x=tmp[!is.na(llen)]$llen, y=tmp[!is.na(llen)]$pr, z=tmp[!is.na(llen)]$dst, type="mesh3d" ) 
    
   
    
    if(0){
    plot_ly(x=~tmp[!is.na(llen)]$llen,y=~tmp[!is.na(llen)]$dst, color=~tmp[!is.na(llen)]$pr) %>% layout(xaxis=list(title= 'line length elasticity'),
                                                                                                        yaxis=list(title = 'distribution elasticity'),
                                                                                                        legend=list(title=list(text='<b> Trend </b>')))
    }
    
    
    
   # fig <- fig %>%  add_trace(data = tmp,  x=x, y=data$y, z=data$z, type="mesh3d" ) 
    
    
    #add_surface()
    
   # fig
    
    
   # ggplot(tmp, aes(x=llen, y=pr, z=dst)) + geom_point()
    
   # fill=brand, y = eval(parse(text=input$plot_predicted)), x = variable)) + geom_bar(position='dodge2',stat='identity') + 
   #               facet_wrap( ~ country) + theme_tufte() + ylab(input$plot_predicted)
                
    ggplot(tmp[!is.na(llen)], aes(x=llen, y=dst)) + geom_point(aes(color=pr)) + scale_colour_gradient2_tableau() +
      xlab('line length elasticity') + ylab('distribution elasticity') + labs(color='price elasticity',
                                                                              caption = 'Plot generated for ALL categories and brands, in selected countries.')
    
  })
    
  
  output$elasticities = DT::renderDataTable({
    #o=produce_model(input)
    #o$predictions
    return(data.frame(1:100))})
  
 # output$vif2 = renderText({
 #   return('test')})
  
  output$vif = renderText({
    # assemble form 
    
    mspec = get_model(input)
    
    elast <<- get_sample(input)
    
    if (as.logical(input$orth_used)==T) {
      orthog = orthogonalization(elast, input, bootstrap = F)
      setkey(orthog$means, brand_id)
      setkey(elast, brand_id)
      for (.v in input$orth_dvs) elast[orthog$means, paste0(.v,'_orth'):=get(paste0('i.', .v, '_orth'))]
    }
    
    vifform = as.formula(paste0('randomnr ~ 1 + ', paste0(mspec$variables, collapse='+')))
    
    tmp <- unique(elast, by = c('category','country','brand'))
    tmp[, randomnr:=runif(.N)]
    
    vifm <- lm(vifform, data=tmp)
    vifs = data.table(variable = names(vifm$coefficients)[-1], VIF=vif(vifm))
    
    
    #if (input$model=='ec') elast <<- copy(elast_sales)
    #if (input$model=='attraction') elast <<- copy(elast_marketshare)
    #input
    kable(vifs, caption = 'VIF', format = 'html')
    
  })
  output$correl = renderText({
    # assemble form 
    
    
    vars=c(unlist(input$brandequity),
                  unlist(input$brandlocation),
                  unlist(input$brandmmix),
                  unlist(input$brandother),
                  
                  unlist(input$categoryfactors), 
                  unlist(input$econ),
                  unlist(input$culture),
                  unlist(input$institutions))
    
    
    tmp = unique(copy(elasticities[[input$model]][selection_obs48==T&selection_brands==T]), by = c('brand_id'))
                 
    correl <- cor(tmp[, vars, with=F], use = 'pairwise')
    kable(correl, caption = 'Correlations', format = 'html')
    
  })
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
    lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(swiss[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(swiss[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('download.RData')
    },
    content = function(file) {
      saved_input = reactiveValuesToList(input)
      if (!grepl('[.]RData$',file, ignore.case=T)) file=paste(file, '.RData')
      save(saved_input, file=file)
    }
  )
 
  output$downloadElast <- downloadHandler(
    filename = function() {
      paste('elasticities.csv')
    },
    content = function(file) {
      if (!grepl('[.]csv$',file, ignore.case=T)) file=paste(file, '.csv')
      o=produce_model(input)
      fwrite(o$predictions, file=file)
    }
  )
 
  output$downloadElastXLS <- downloadHandler(
    filename = function() {
      paste('elasticities.xlsx')
    },
    content = function(file) {
      if (!grepl('[.]xlsx$',file, ignore.case=T)) file=paste(file, '.xlsx')
      o=produce_model(input)
      
      write.xlsx(o$predictions, file=file, sheetName = 'elasticities', row.names=FALSE, showNA=FALSE)
    }
  ) 
  output$downloadregdata <- downloadHandler(
    filename = function() {
      paste('download.csv')
    },
    content = function(file) {
      mspec = get_model(input)
      elast = get_sample(input)
      tmp = elast[, c('category','country','brand','brand_id',mspec$variables),with=F]
      if (!grepl('[.]csv$',file, ignore.case=T)) file=paste(file, '.csv')
      fwrite(tmp, file=file, row.names=F)
    }
  )
  

  
  
  
}


shinyApp(ui = ui, server = server)



#install.packages('rsconnect')
