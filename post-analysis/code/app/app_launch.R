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


brands <- elasticities$ec_main_sur[, list(Ncountries = length(unique(country)), Ncategory=length(unique(category))),by=c('brand')]
setorderv(brands, c('Ncountries', 'Ncategory'), order=-1L)
brands <- brands[!grepl('alloth', brand)]

categories = unique(elasticities$ec_main_sur$category)
countries = unique(elasticities$ec_main_sur$country)


replace_categories <- function(x) {
  ret = rep('', length(x))
  ret[grepl('washing',x)] <- 'Washing machines'
  ret[grepl('tv_gen2_lcd',x)] <- 'LCD TVs'
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


#########################################################
##### Add variables to choice set #
#########################################################

names(elasticities)

model_names = list('M1a) Linear Error Correction (weights t-3...t-1, SUR)' = 'ec_main_sur',
                   'M1b) Lin. EC (weights t-3...t-1, no SUR)' = 'ec_main')#,
                 #  'M2a) Lin. EC (weights t-2,...t, SUR)' = 'ec_main_currweights',
                #   'M2b) Lin. EC (weights t-2,...t, SUR)' = 'ec_main_currweights_sur')
                   
                   #'M2a) Lin. EC (weights t-2...t, SUR)' = 'ec_main_currweights_sur',
                   #'M2b) Lin. EC (weights t-2...t, SUR)' = 'ec_main_currweights',
                   #'M3a) Lin. EC (no weights, SUR)' = 'ec_main_noweights_sur',
                   #'M3b) Lin. EC (no weights, no SUR)' = 'ec_main_noweights')#,

                   
                  # 'M2) Error correction (sales, without SUR)' = 'ec_main',
                  # 'M3) with lag competition' = 'ec_unrestrictedcompetition',
                   #'M3) Error correction (sales; but with copula of d_mmix)' = 'ec_restricted_sigdcop',
                 #  'M3) Attraction model (market share)' = 'marketshare')
            
potential_vars_raw = list(brandequity=list('!SBBE' = 'sbbe_round1_mc',
                                       'BrandZ indicator' = 'brandz',
                                       'Log Brand strength (BAV)' = 'ln_bav_brandstrength_mc',
                                       'Log Brand stature (BAV)' = 'ln_bav_brandstature_mc',
                                       'Log Brand energized diff. (BAV)' = 'ln_bav_energizeddifferentiation_mc',
                                       'Log Marketshare' = 'ln_brand_ms_mc',
                                       'Marketshare' = 'brand_ms_mc'),
                          
                      brandlocation = list('Domestic market indicator' = 'local_to_market_mc',
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
                                       'Line length (std.)' = 'llen_std_mc',
                                       'Innovativeness 12 (log index)' ='ln_nov12sh_index_mc',
                                       'Innovativeness 12 (index)' = 'nov12sh_index_mc',
                                       'Innovativeness 12 (std)' = 'nov12sh_std_mc',
                                       '!Innovativeness 6 (log index)' ='ln_nov6sh_index_mc',
                                       'Innovativeness 6 (index)' = 'nov126_index_mc',
                                       'Innovativeness 6 (std)' = 'nov6sh_std_mc'),
                      
                      
                      brandother = list('Log Brand novelty' = 'ln_brandnovelty6_mc',
                                        'Brand novelty' = 'brandnovelty6_mc'),#,
                                        #'Price positioning' = 'brand_prindex_mean_mc'),
                      
                      category = list('!Log Market concentration' = "ln_market_herf_mc",
                                      '!Log Market growth' = "ln_market_growth_mc",
                                      '!Appliances (vs. electronics)' = 'appliance',
                                      'Log Category innovativeness' = 'ln_catnovelty3_mc',
                                      'Market concentration' = "market_herf_mc",
                                      'Market growth' = "market_growth_mc",
                                      'Category innovativeness' = 'catnovelty3_mc'
                                        ),
                      
                      country_econ = list('!Log GDP growth (obs. avg)' = 'ln_gdpgrowthyravg_mc',
                                          'Log GDP growth (avg)' = 'ln_gdpgrowthavg_mc',
                                          'Log GDP growth (2010)' = 'ln_gdpgrowth2010_mc',
                                          "!Log Income Inequality (nearest to 2010)" = "ln_ginicoef_mc",
                                          
                                          '!Log GDP per capita (obs. avg)' = "ln_gdppercapitacurrentyravg_mc",
                                          'Log GDP per capita (avg)' = "ln_gdppercapitacurrentavg_mc",
                                          'Log GDP per capita (2010)' = "ln_gdppercapitacurrent2010_mc",
                                          
                                          "Log Trade openess (obs. avg)" = 'ln_tradeopenessyravg_mc',
                                          "Log Trade openess (avg)" = 'ln_tradeopenessavg_mc',
                                          "Log Trade openess (in 2010)" = 'ln_tradeopeness2010_mc',
                                          
                                          'Log HDI (2010)' = 'ln_hdi2010_mc',
                                          
                                          "Emerging market indicator" = "emerging",
                                          "Log Goods Market Efficiency (GCI 2010)" = "ln_gci_p06_goods_s_mc",
                                          "Log Infrastructure (GCI 2010)" = 'ln_gci_p02_infrastructure_s_mc',
                                          "!Log Market size (GCI 2010)" = 'ln_gci_p10_marketsize_s_mc',
                                          
                                          
                                          'GDP growth (obs. avg)' = 'gdpgrowthyravg_mc',
                                          'GDP growth (avg)' = 'gdpgrowthavg_mc',
                                          'GDP growth (2010)' = 'gdpgrowth2010_mc',
                                          "Income Inequality (nearest to 2010)" = "ginicoef_mc",
                                          
                                          'GDP per capita (obs. avg)' = "gdppercapitacurrentyravg_mc",
                                          'GDP per capita (avg)' = "gdppercapitacurrentavg_mc",
                                          'GDP per capita (2010)' = "gdppercapitacurrent2010_mc",
                                          
                                          "Trade openess (obs. avg)" = 'tradeopenessyravg_mc',
                                          "Trade openess (avg)" = 'tradeopenessavg_mc',
                                          "Trade openess (in 2010)" = 'tradeopeness2010_mc',
                                          
                                          'HDI (2010)' = 'hdi2010_mc',
                                          
                                          "Goods Market Efficiency (GCI 2010)" = "gci_p06_goods_s_mc",
                                          "Infrastructure (GCI 2010)" = 'gci_p02_infrastructure_s_mc',
                                          "Market size (GCI 2010)" = 'gci_p10_marketsize_s_mc'
                                          
                                          ),
                      
                      country_culture = list("WVS: Traditional vs. rational" = "tradrat_mc",
                                             "WVS: Survival vs. Self-expression" = "survself_mc",
                                             "Hofstede: Log Longterm orientation" = "ln_ltowvs_mc",
                                             "Hofstede: Log Individualism" = "ln_idv_mc",
                                             "!Hofstede: Log Uncertainty Avoidance" = "ln_uai_mc",
                                             "!Hofstede: Log Power distance" = "ln_pdi_mc",
                                             "!Hofstede: Log Masculinity" = "ln_mas_mc",
                                             "Hofstede: Longterm orientation" = "ltowvs_mc",
                                             "Hofstede: Individualism" = "idv_mc",
                                             "Hofstede: Uncertainty Avoidance" = "uai_mc",
                                             "Hofstede: Power distance" = "pdi_mc",
                                             "Hofstede: Masculinity" = "mas_mc"),
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
             selectInput("dv", label = h5("Dependent variable (second stage)"),
                         choices = list('LT elasticity at the mean'= 'elastlt',
                                        'LT elasticity at the median' = 'elastmedianlt'), selected= 'elastlt', multiple=F),
             
             selectInput("randomeffects", label = h5("List of random effects (for RE model), or clustering (in case of OLS)"),
                       choices = list('Brand'= 'brand', 'Category'='category',
                                      'Country' = 'country'), selected = c('brand', 'category','country'), multiple=TRUE),
             #selectInput("clustering_type", label = h5("Clustering type (for OLS)"),
             #            choices = list('HC0'= 'HC0', 'HC1'='HC1',
             #                           'HC2'= 'HC2', 'HC3'='HC3'), selected = c('HC1'), multiple=FALSE),
             
             selectInput("trimming", label = h5("Trimming/Winsorizations"),
                       choices = (trimming), selected = trimming[4], multiple=FALSE)),
           
           
           tabPanel("Plotting", 
                    #selectInput("plot_vars", label = h5("Variables"),
                    #            choices = list('Price' = 'pr',
                    #                           'Line length' = 'llen',
                    #                           'Distribution' = 'dst'), 
                    #            selected = c('pr','llen','dst'), multiple=TRUE),
                   selectInput("plot_brands", label = h5("Brands"),
                               choices = sapply(str_to_title(brands$brand), function(x) tolower(x), simplify=F), 
                               selected = c('samsung'), multiple=FALSE),
                   selectInput("plot_categories", label = h5("Categories"),
                               choices = sapply(str_to_title(categories), function(x) tolower(x), simplify=F), 
                               selected =c('phones_smart'), multiple=FALSE),
                   #selectInput("plot_countries", label = h5("Countries"),
                  #             choices = sapply(str_to_title(countries), function(x) tolower(x), simplify=F), 
                  #             selected = countries, multiple=TRUE),
                   radioButtons("plot_predicted", label = h5("Use estimated or predicted elasticities?"),
                                c("Estimated" = 'elastlt',
                                  "Predicted" = 'elastlt_pred'),
                                selected = 'elastlt_pred'),
                   #selectInput("plot_stack_by", label = h5("Stacked bar chart by..."),
                  #             choices = list('Countries'='country',
                  #                            'Categories'='category'),
                  #             selected = 'country', multiple=FALSE),
                   selectInput("plot_stack_val", label = h5("Stacked bar chart with..."),
                               choices = list('Relative elasticities (in %)'='rel_val',
                                              'Absolute elasticities'='abs_val'),
                               selected = 'abs_val', multiple=FALSE)
                   
                   
                   
                   )
                   
           )
           
           
           
    ),
    column(8,
           tabsetPanel(type = "tabs",
                       
                       # tabPanel("Model Summary", verbatimTextOutput("summary")),
                       tabPanel("Model results", htmlOutput("stargazer")),#, # Regression output
                       tabPanel("VIFs", htmlOutput("vif")),
                       tabPanel("Plots", plotOutput('stacked')),
                       #tabPanel("Plots (brand-specific bar plots)", plotOutput('plot')),
                       #tabPanel("Plots (scatter plots)", plotOutput('surface')),
                       
                       #tabPanel("VIFs2", htmlOutput("vif2")),
                       #tabPanel("Elasticities", DT::dataTableOutput("elasticities")),
                       
                       #}, options = list(searching = FALSE)
                       #tabPanel("Tables for the paper", includeHTML("tables.html")),
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

get_sample <- function(input, dv = 'elastlt') {
  tmp <- copy(elasticities[[input$model]][selection_obs48==T&selection_brands==T])
  
  # define DV
  tmp[, elastlt:=get(dv)]
  tmp[, elastlt_se:=get(paste0(dv,'_se'))]
  tmp[, w_elastlt:=1/elastlt_se, by = c('variable')]
  
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



result_storage <<- NULL
data_storage <<- NULL


produce_model <- function(input) {
  
  my_hash <- function(input, ...) digest(paste0(paste0(names(input), collapse='_'), '|', paste0(sapply(unlist(input), function(x) if(x=='') return('[ ]') else return(x)), collapse='_')),...)
  
  hash=try(my_hash(reactiveValuesToList(input), algo=c("md5")),silent=T)
  if(class(hash)=='try-error') hash=try(my_hash(input, algo=c("md5")),silent=T)
  
  
  
  outp = result_storage[[hash]]$results
  dstorage = data_storage[[hash]]
  
  if (is.null(outp)) {
    
    mspec = get_model(input)
    
    elast <<- get_sample(input, dv = input$dv)
    
   
    
    mods = all_mods(list(list(pr=mspec$formula, dst=mspec$formula, llen=mspec$formula)),
                    mtype=mspec$modeltype,
                    clust=mspec$cluster, clust_type = mspec$clustering_type)
    
    mods2 = lapply(seq(along=c('pr','dst','llen')), function(i) {
      
      outt = try(summary(mods[[1]][[i]])$coefficients, silent=T)
      if (class(outt)=='try-error') outt = mods[[1]][[i]]
      
      outt[,3]= outt[,1]/outt[,2]
      
      outt= cbind(outt[,1:3], 2*(1-pnorm(abs(outt[,3]))))
      colnames(outt) <-c("Estimate","Std. Error","t value","Pr(>|t|)")
      class(outt) <- 'coeftest'
      outt})
    
    
    lbllist = NULL
    
    m_ord = c(3,1,2)
    
    if (mspec$modeltype=='lmer') {
      rsqs=unlist(lapply(mods, function(x) lapply(x[c('pr','dst','llen')], rsq)))
      obss = unlist(lapply(mods, function(x) lapply(x[c('pr','dst','llen')], function(i) length(which(!is.na(residuals(i)))))))
      fits = lapply(mods[[1]][c('pr','dst','llen')], function(i) summary(i)$AICtab)
      aics = unlist(lapply(fits, function(x) x['AIC']))
      bics = unlist(lapply(fits, function(x) x['BIC']))
      logliks = unlist(lapply(mods[[1]][c('pr','dst','llen')], function(x) as.numeric(logLik(x))))
      
      r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs[m_ord], digits=3, format='f', flag='#')))
      aic = c('AIC',sub('^(-)?0[.]', '\\1.', formatC(aics[m_ord], digits=2, format='f', flag='#')))
      bic = c('BIC',sub('^(-)?0[.]', '\\1.', formatC(bics[m_ord], digits=2, format='f', flag='#')))
      loglik = c('LL',sub('^(-)?0[.]', '\\1.', formatC(logliks[m_ord], digits=2, format='f', flag='#')))
      obs = c('Observations',obss[m_ord])
      
      lbllist = list(r2s,aic, bic,loglik, obs)
    } 
    
    if (mspec$modeltype=='lm') {
      rsqs=mods[[1]]$rsqs[m_ord]
      
      obss = mods[[1]]$obs[m_ord]
      
      aics = mods[[1]]$aic[m_ord]
      bics =mods[[1]]$bic[m_ord]
      
      r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
      aic = c('AIC',sub('^(-)?0[.]', '\\1.', formatC(aics, digits=2, format='f', flag='#')))
      bic = c('BIC',sub('^(-)?0[.]', '\\1.', formatC(bics, digits=2, format='f', flag='#')))
      obs = c('Observations',obss)
      
      lbllist = list(r2s,aic, bic,obs)
    } 
    
    
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
    
    dt[, var:=get(input$plot_predicted)]
    
    
    
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
    
    
    tmp = dcast.data.table(dt, category+country+brand~variable, value.var=input$plot_predicted)
    
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
    
  output$stacked <- renderPlot({
    o=produce_model(input)
    dt = o$predictions
    
    if (!is.null(input$plot_brands)) dt <- dt[brand%in%input$plot_brands]
    if (!is.null(input$plot_categories)) dt <- dt[category%in%input$plot_categories]
   # if (!is.null(input$plot_countries)) dt <- dt[country%in%input$plot_countries]
    #if (!is.null(input$plot_vars)) dt <- dt[variable%in%input$plot_vars]
    
    dt[, abs_val:=(get(input$plot_predicted))]
    dt[variable=='pr', abs_val:=-(get(input$plot_predicted))]
    
    dt[, rel_val:=abs_val/sum(abs_val), by =c('category','country','brand')]
    
    # agglevel
    tmp = dcast.data.table(dt, category+country+brand~variable, value.var=input$plot_stack_val)#'rel_share')
    
    agglevel = 'country'# input$plot_stack_by #c('country')
    
    tmp2 = tmp[, lapply(.SD, mean,na.rm=T),by=c(agglevel), .SDcol=c('llen','pr','dst')]
    tmp2[, sum:=llen+pr+dst]
    
    tmp3 = melt(tmp2, id.vars=c(agglevel))
    levels(tmp3$variable)
    tmp3[, variable:=factor(as.character(variable), levels=c('llen','pr','dst', 'sum'))]
    #levels(tmp3$variable) <- c('price','distribution','line length')
    levels(tmp3$variable)
    
    setkey(tmp3, country, variable)
    setkey(dt, country, variable)
    
    tmp3[dt, printvar:=get(paste0('i.', input$plot_predicted))]
    tmp3[, printvar2:=formatC(printvar, digits=3,
                              flag="", format="f")]
    
    
    tmp3$country<-str_to_title(tmp3$country) 
    
    if (input$plot_stack_val=='rel_val') levs=unlist(tmp3[variable=='llen',1])[order(tmp3[variable=='llen']$value)]
    if (input$plot_stack_val=='abs_val') levs=unlist(tmp3[variable=='sum',1])[order(tmp3[variable=='sum']$value)]
    
    tmp3[, paste0(agglevel):=factor(as.character(get(agglevel)), levels = levs)]
    
    #
    tmp[, variable_label:=as.character('')]
    
    tmp3[grepl('llen', variable), variable_label:='Line length']
    tmp3[grepl('pr', variable), variable_label:='Price']
    tmp3[grepl('dst', variable), variable_label:='Distribution']
    
    tmp3[, variable_label:=factor(variable_label, levels=rev(c('Line length','Price','Distribution')))]
    #sorted_country =as.character(unique(tmp3$country)[order(tolower(unique(tmp3$country)),decreasing=T)])
    #tmp3[, country:=factor(country,levels=(sorted_country))]
    
    ggplot(tmp3[!variable%in%'sum'], aes(fill=variable_label, y=value, 
                                         x=eval(parse(text=agglevel)),
           label=printvar2)) + geom_bar(position="stack", stat="identity")  + scale_fill_grey(start = .6, end = .9) + coord_flip() +
      theme_bw()+
      xlab(str_to_title(agglevel)) + ylab('Magnitude of Marketing Elasticities') + 
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      labs(fill='Elasticities', caption = paste0('Plot generated on the basis of ', 
                                                 ifelse(input$plot_stack_val=='rel_val', 'relative elasticities', 'absolute elasticities'), ' for brand ', 
                                                 str_to_title(input$plot_brands), ' (', tolower(replace_categories(input$plot_categories)), 
                                                 ').\nCountries sorted in decreasing order of ', ifelse(input$plot_stack_val=='rel_val', 'line-length elasticities', 
                                                                                                       'combined marketing effectiveness'),'.'))
      
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
    
    elast <<- get_sample(input, dv=input$dv)
    
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
      elast = get_sample(input, dv=input$dv)
      tmp = elast[, c('category','country','brand','brand_id',mspec$variables),with=F]
      if (!grepl('[.]csv$',file, ignore.case=T)) file=paste(file, '.csv')
      fwrite(tmp, file=file, row.names=F)
    }
  )
  

  
  
  
}


shinyApp(ui = ui, server = server)



#install.packages('rsconnect')
