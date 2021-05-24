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
library(digest)
library(xlsx)
library(stringr)
library(foreign)
library(kableExtra)
library(ggplot2)
library(ggthemes)

fn <- c('app_workspace.RData')
if (!exists('elasticities')) load(fn)


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

all_mods <- function(formula, mtype = 'lmer', clust = NULL, clust_type = 'HC1') {
  loop_vars = unique(elast$variable)
  
  results <- lapply(loop_vars, function(var) {
    estim_data = elast[variable==var]
    
    if (mtype=='lm')  {
      m = lm(update.formula(elastlt~1, formula), data=estim_data, weights=w_elastlt)
      avail=setdiff(1:nrow(estim_data), m$na.action)
      pred=data.table(estim_data[, c('brand','brand_id','category','country', 'elastlt')][avail], variable=var, elast_lt_pred=predict(m))
      
      r2= rsq(m)
      loglik = logLik(m)
      aic=AIC(m)
      bic=BIC(m)
      if (!is.null(clust)) m <- coeftest(m, vcov = vcovCL, cluster = clust, fix = T, type = clust_type)
      
      return(list(model=m, r2=r2, aic=aic, bic=bic, loglik=loglik, predictions=pred))
    }
       
    
    if (mtype=='lmer') {
     m = lmer(update.formula(elastlt~1, formula),
                data=estim_data, weights=w_elastlt,
                control = lmerctrl, REML=F)
     
     avail=rownames(estim_data)%in%rownames(m@frame)
     
     pred=data.table(estim_data[, c('brand','brand_id','category','country', 'elastlt')][avail], variable=var, elast_lt_pred=predict(m))
     
     loglik = logLik(m)
     aic = AIC(m)
     bic = BIC(m)
     r2 = rsq(m)
     
     return(list(model=m, r2=r2, aic=aic, bic=bic, loglik=loglik, predictions=pred))
    
    }
    
  })
  
  names(results) <- loop_vars
  return(results)
}


lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")


#########################################################
##### Add variables to choice set #
#########################################################

names(elasticities)

model_names = list('M1a) Linear Error Correction (weights t-3...t-1, SUR)' = 'ec_main_sur',
                   #'M1b) Lin. EC (weights t-3...t-1, no SUR)' = 'ec_main',
                   'M2a) Lin. EC w/ Novelty (weights t-3, ...t-1, SUR)' = 'ec_main_w_novelty_sur')
                   #'M2b) Lin. EC w/ Novelty (weights t-3, ...t-1, no SUR)' = 'ec_main_w_novelty')



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
                                       'Log Marketshare' = 'ln_brand_ms_mc'),
                                       #'Marketshare' = 'brand_ms_mc'),
                          
                      brandlocation = list('Domestic market indicator' = 'local_to_market_mc',
                                           '!JP, US, Swiss, GER, Sweden indicator' = "`brand_from_jp-us-ch-ge-sw_mc`",
                                           'Western brand indicator' = 'western_brand_mc',
                                           'International brand active in more than 1 country' = 'internat_brand'),
                      
                      brandmmix = list('Price (log index)' = 'ln_rwpspr_index_mc',
                                       'Distr. (log index)' = 'ln_wpswdst_index_mc',
                                       'Line length (log index)' = 'ln_llen_index_mc',
                                       'Innovativeness 6 (log index)' ='ln_nov6sh_index_mc',
                                        'Innovativeness 12 (log index)' ='ln_nov12sh_index_mc',
                                       '!Price (log weighted index)' = 'ln_rwpspr_windex_mc',
                                       '!Distr. (log weighted index)' = 'ln_wpswdst_windex_mc',
                                       '!Line length (log weighted index)' = 'ln_llen_windex_mc',
                                       '!Innovativeness 6 (log weighted index)' ='ln_nov6sh_windex_mc',
                                       'Innovativeness 12 (log weighted index)' ='ln_nov12sh_windex_mc'),
                                      
                                        
                      
                      brandother = list('Log Brand novelty' = 'ln_brandnovelty6_mc',
                                        'Brand novelty' = 'brandnovelty6_mc'),#,
                                        #'Price positioning' = 'brand_prindex_mean_mc'),
                      
                      category = list('!Log Market concentration' = "ln_market_herf_mc",
                                      'Log Market growth' = "ln_market_growth_mc",
                                      '!Log market growth new' = 'ln_market_meangrowth_mc',
                                      
                                      'Log Market growth 100' = "ln_market_growth100_mc",
                                      'Log Market growth neg' = "market_growthneg_mc",
                                      
                                      '!Appliances (vs. electronics)' = 'appliance',
                                      'Log Category innovativeness' = 'ln_catnovelty3_mc'
                                      
                                        ),
                      
                      country_econ = list(
                                          'Log GDP growth WB (obs. avg)' = 'ln_gdpgrowthyravg_mc',
                                          #'Log GDP growth WB (avg)' = 'ln_gdpgrowthavg_mc',
                                          'Log GDP growth WB (2010)' = 'ln_gdpgrowth2010_mc',
                                          '!Log GDP growth PENN (obs. avg)' = 'ln_penn_growthrgdpeyravg_mc',
                                          #'Log GDP growth PENN (2010)' = 'ln_penn_growthrgdpe2010_mc',
                                          
                                          'Log GDP growth NEW PENN (obs avg.)' = 'ln_penn_rgdpeggrowth_mc',
                                          'Log GDP per cap. growth NEW PENN (obs avg.)' = 'ln_penn_percapitargdpeggrowth_mc',
                                          'Log GDP growth NEW PENN (obs avg. x100)' = 'ln_penn_rgdpeggrowth100_mc',
                                          'Log GDP per cap. growth NEW PENN (obs avg. x 100)' = 'ln_penn_percapitargdpeggrowth100_mc',
                                          'Log GDP growth NEW PENN (obs avg. neg)' = 'ln_penn_rgdpeggrowthneg_mc',
                                          'Log GDP per cap. growth NEW PENN (obs avg. neg)' = 'penn_percapitargdpeggrowthneg_mc',
                                          
                                          
                                          
                                          "!Log Income Inequality (nearest to 2010)" = "ln_ginicoef_mc",
                                          
                                          'Log GDP per capita WB (obs. avg)' = "ln_gdppercapitacurrentyravg_mc",
                                          #'Log GDP per capita (avg)' = "ln_gdppercapitacurrentavg_mc",
                                          'Log GDP per capita WB (2010)' = "ln_gdppercapitacurrent2010_mc",
                                          
                                          '!Log GDP per capita PENN (obs. avg)' = 'ln_penn_percapitargdpeyravg_mc',
                                          'Log GDP per capita PENN (2010)' = 'ln_penn_percapitargdpe2010_mc',
                                            
                                          "Log Trade openess (obs. avg)" = 'ln_tradeopenessyravg_mc',
                                          #"Log Trade openess (avg)" = 'ln_tradeopenessavg_mc',
                                          "Log Trade openess (in 2010)" = 'ln_tradeopeness2010_mc',
                                          
                                          'Log HDI (2010)' = 'ln_hdi2010_mc',
                                          
                                          "Log population WB (obs. avg)" = 'ln_populationyravg_mc',
                                          #"Log population WB (avg)" = 'ln_populationavg_mc',
                                          "Log population WB (in 2010)" = 'ln_population2010_mc',
                                          
                                          "!Log population PENN (obs. avg)" = 'ln_penn_popyravg_mc',
                                         # "Log population PENN (avg)" = 'ln_populationavg_mc',
                                          "Log population PENN (in 2010)" = 'ln_penn_pop2010_mc',
                                          
                                          
                                          "Emerging market indicator" = "emerging",
                                          "Log Goods Market Efficiency (GCI 2010)" = "ln_gci_p06_goods_s_mc",
                                          "Log Infrastructure (GCI 2010)" = 'ln_gci_p02_infrastructure_s_mc',
                                          "Log Market size (GCI 2010)" = 'ln_gci_p10_marketsize_s_mc'
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
           
           
           tabPanel("Plots/Tables", 
                   selectInput("plot_brands", label = h5("Brands"),
                               choices = sapply(str_to_title(brands$brand), function(x) tolower(x), simplify=F), 
                               selected = c('apple'), multiple=FALSE),
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
                   selectInput("plot_stack_val", label = h5("Stacked bar chart with (only for elasticity plot)..."),
                               choices = list('Relative elasticities (in %)'='rel_val',
                                              'Absolute elasticities'='abs_val'),
                               selected = 'abs_val', multiple=FALSE),
                  selectInput("plot_vars", label = h5("Marketing mix instrument (only for allocation table/plot)"),
                              choices = list('Price' = 'pr',
                                             'Line length' = 'llen',
                                             'Distribution' = 'dst'), 
                              selected = c('llen','dst'), multiple=FALSE),
                  
                   
                   
                   
                   )
                   
           )
           
           
           
    ),
    column(8,
           tabsetPanel(type = "tabs",
                       
                       # tabPanel("Model Summary", verbatimTextOutput("summary")),
                       tabPanel("Model results", htmlOutput("stargazer")),#, # Regression output
                       tabPanel("VIFs", htmlOutput("vif")),
                       tabPanel("Plot (brand elasticities)", plotOutput('stacked')),
                       tabPanel("Table (allocation)", htmlOutput('allocation')),
                       tabPanel("Plot (allocation)", plotOutput('allocationplot')),
                       
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
                                downloadButton("downloadElastXLS", "Download estimated and predicted elasticities (Excel file)"),
                                br(),
                                downloadButton("downloadPlots", "Download (stacked) brand plot (png file)") )
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
  
  tmp[, internat_brand:=ifelse(ncountries>1,1,0)]
  return(tmp)
}



result_storage <<- NULL
data_storage <<- NULL


model_order = list('Line length' = 'llen',
                   'Price' = 'rwpspr',
                   'Distribution' = 'wpswdst',
                   'Innovativeness' = 'nov6sh',
                   'Innovativeness'  ='nov12sh',
                   'Innovativeness' = 'nov3sh')


produce_model <- function(input) {
  
  my_hash <- function(input, ...) digest(paste0(paste0(names(input), collapse='_'), '|', paste0(sapply(unlist(input), function(x) if(x=='') return('[ ]') else return(x)), collapse='_')),...)
  
  hash=try(my_hash(reactiveValuesToList(input), algo=c("md5")),silent=T)
  if(class(hash)=='try-error') hash=try(my_hash(input, algo=c("md5")),silent=T)
  
  
  
  outp = result_storage[[hash]]$results
  dstorage = data_storage[[hash]]
  
  if (is.null(outp)) {
    
    mspec = get_model(input)
    
    elast <<- get_sample(input, dv = input$dv)
    
    mods = all_mods(formula = mspec$formula,
                    mtype=mspec$modeltype,
                    clust=mspec$cluster, clust_type = mspec$clustering_type)
    
    mods2 = lapply(mods, function(m) {
      
      outt = try(summary(m$model)$coefficients, silent=T)
      if (class(outt)=='try-error') outt = m$model
      
      outt[,3]= outt[,1]/outt[,2]
      
      outt= cbind(outt[,1:3], 2*(1-pnorm(abs(outt[,3]))))
      colnames(outt) <-c("Estimate","Std. Error","t value","Pr(>|t|)")
      class(outt) <- 'coeftest'
      outt})
    
    
    lbllist = NULL
    
    m_ord = match(model_order, names(mods2))
    m_ord = m_ord[!is.na(m_ord)]
    m_names = names(model_order)[match(names(mods)[m_ord], model_order)]
    
    r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(unlist(lapply(mods[m_ord], function(x) x$r2)), digits=3, format='f', flag='#')))
    aic = c('AIC', sub('^(-)?0[.]', '\\1.', formatC(unlist(lapply(mods[m_ord], function(x) x$aic)), digits=2, format='f', flag='#')))
    bic = c('BIC', sub('^(-)?0[.]', '\\1.', formatC(unlist(lapply(mods[m_ord], function(x) x$bic)), digits=2, format='f', flag='#')))
    obs = c('Observations', formatC(unlist(lapply(mods[m_ord], function(x) nrow(x$predictions)))))
    loglik = c('LL', sub('^(-)?0[.]', '\\1.', formatC(unlist(lapply(mods[m_ord], function(x) x$loglik)), digits=2, format='f', flag='#')))
    lbllist = list(r2s,aic, bic, loglik, obs)
    
    
    outp=paste0(paste0(capture.output({stargazer(mods2[m_ord], type='html', add.lines=lbllist,
                                                 column.labels = m_names)}), collapse=''),
                '<br><br>', mspec$formula, "<br><br>", paste0(as.character(mspec$cluster), collapse=''), '<br><br>', mspec$modeltype)
    
    if (mspec$modeltype=='lmer') outp = paste0(outp, '<br><br>', paste0({lapply(mods[m_ord], function(x) {
      test= capture.output({summary(x$model)})
      # select
      start=grep('Random effects', test, ignore.case=T)
      end=grep('Fixed effect', test, ignore.case=T)
      return(paste0(test[start:c(end-1)], collapse='<br>'))
      
    })}, collapse='<br>'))
    
    preds = rbindlist(lapply(mods, function(x) x$predictions))
    setcolorder(preds, c('category','country','brand','brand_id','variable','elastlt','elast_lt_pred'))
    setnames(preds, 'elast_lt_pred','elastlt_pred')
    
    dstorage<<- preds
    result_storage[[hash]]$results <<- paste0(outp, '<br>from memory<br>')
    
    data_storage[[hash]]$predictions <<- preds
    
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

  output$allocation = renderText({
    # assemble form 
    
    
    
    o=produce_model(input)
    dt = o$predictions
    
    if (!is.null(input$plot_brands)) dt <- dt[brand%in%input$plot_brands]
    if (!is.null(input$plot_categories)) dt <- dt[category%in%input$plot_categories]
    #if (!is.null(input$plot_countries)) dt <- dt[country%in%input$plot_countries]
    if (!is.null(input$plot_vars)) dt <- dt[grepl(input$plot_vars, variable)]
    
    # Load other stuff
    # mean unit sales
    setkey(dt, category, country, brand)
    
    
    tmp = unique(elast[,c('avgsales','avgbrandgrowth', 'brand','category','country'),with=F], by = c('category','country','brand'))
    
    dt <- merge(dt, tmp, by = c('category','country','brand'),all.x=T)
    
    # constant
    set.seed(1234)
    const = runif(1,1,4)
    dt[, avgsales:=avgsales*const]
    
    dt[, relelast:=100*abs(get(input$plot_predicted))/sum(abs(get(input$plot_predicted)))]
    dt[, relsales:=100*avgsales/sum(avgsales)]
    dt[, elast_x_size:=abs(get(input$plot_predicted))*avgsales*avgbrandgrowth]
    dt[, optimal:=100*elast_x_size/sum(elast_x_size)]
    
    
    tmp = dt[, c('country','avgsales', input$plot_predicted,  'avgbrandgrowth', 'relsales','relelast', 'optimal'),with=F]
    tmp[, country:=str_to_title(country)]
    setorder(tmp, country)
    
    setnames(tmp, c('Country', 'Unit sales', 'Marketing elasticity', 'Expected growth', 'Unit sales (%)','Marketing elasticity (%)',  'Optimal (%)'))
    varname = ''
    if (input$plot_vars=='dst') varname = 'distribution'
    if (input$plot_vars=='pr') varname = 'price'
    if (input$plot_vars=='llen') varname = 'line length'
    
    ret=kable(tmp, digits=c(0,0,3,3,1,1,1), 
          caption = paste0('Alternative Allocations of Marketing Mix Budget for ', str_to_title(varname), ' Elasticities'),
          format='html',
          initial.zero = FALSE) %>% 
      add_header_above(list(' '=4, 'Allocation proportional to' =3)) %>% kable_styling() %>% footnote(number = paste0('Countries shown in alphabetical order. ', ifelse(grepl('pred', input$plot_predicted), 'Predicted', 'Estimated'), ' elasticities shown for ', str_to_title(input$plot_brands), ' (', str_to_title(replace_categories(input$plot_categories)), '). Unit sales are a brand\'s average unit sales in the data. The optimal allocation is based on marketing mix elasticities x unit sales.'))
    
    
    
    return(ret)
  })
  
  output$allocationplot = renderPlot({
    # assemble form 
    
    
    
    o=produce_model(input)
    dt = o$predictions
    
    if (!is.null(input$plot_brands)) dt <- dt[brand%in%input$plot_brands]
    if (!is.null(input$plot_categories)) dt <- dt[category%in%input$plot_categories]
    #if (!is.null(input$plot_countries)) dt <- dt[country%in%input$plot_countries]
    if (!is.null(input$plot_vars)) dt <- dt[grepl(input$plot_vars, variable)]
    
    # Load other stuff
    # mean unit sales
    setkey(dt, category, country, brand)
    
    tmp = unique(elast[,c('avgsales','brand','category','country'),with=F], by = c('category','country','brand'))
    
    dt <- merge(dt, tmp, by = c('category','country','brand'),all.x=T)
    dt[, avgsales:=avgsales]
    dt[, relelast:=100*abs(get(input$plot_predicted))/sum(abs(get(input$plot_predicted)))]
    dt[, relsales:=100*avgsales/sum(avgsales)]
    dt[, elast_x_size:=abs(get(input$plot_predicted))*avgsales]
    dt[, optimal:=100*elast_x_size/sum(elast_x_size)]
    
    
    tmp = dt[, c('country',input$plot_predicted, 'avgsales', 'relelast','relsales','optimal'),with=F]
    tmp[, country:=str_to_title(country)]
    setorder(tmp, country)
    
    setnames(tmp, c('Country','Marketing elasticity', 'Unit sales', 'Marketing elasticity (%)', 'Unit sales (%)', 'Optimal (%)'))
    varname = ''
    if (input$plot_vars=='dst') varname = 'distribution'
    if (input$plot_vars=='pr') varname = 'price'
    if (input$plot_vars=='llen') varname = 'line length'
    
    tmp2 = melt(tmp, id.vars=c('Country'))
    
    ggplot(tmp2[grepl('[%]',variable)], aes(x=Country, y = value, fill=variable)) + geom_bar(stat='identity', position=position_dodge()) + ylab('%-allocation') +
      labs(fill='Allocation by') +scale_fill_grey(start = .6, end = .9) +
      theme_bw()+ theme(legend.position="bottom") + 
      labs(caption=paste0('Countries shown in alphabetical order. ', ifelse(grepl('pred', input$plot_predicted), 'Predicted', 'Estimated'), ' elasticities shown for ', str_to_title(input$plot_brands), ' (', str_to_title(replace_categories(input$plot_categories)), ').\nUnit sales are a brand\'s average unit sales in the data.\nThe optimal allocation is based on marketing mix elasticities x unit sales.')) +
    ggtitle(paste0('Alternative Allocations of Marketing Mix Budget for ', str_to_title(varname), ' Elasticities')) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
   # return(ret)
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
    
  make_stacked <- function() {
    
    o=produce_model(input)
    dt = o$predictions
    
    if (!is.null(input$plot_brands)) dt <- dt[brand%in%input$plot_brands]
    if (!is.null(input$plot_categories)) dt <- dt[category%in%input$plot_categories]
    # if (!is.null(input$plot_countries)) dt <- dt[country%in%input$plot_countries]
    #if (!is.null(input$plot_vars)) dt <- dt[variable%in%input$plot_vars]
    
    dt[, abs_val:=(get(input$plot_predicted))]
    dt[grepl('pr$', variable), abs_val:=-(get(input$plot_predicted))]
    
    dt[, rel_val:=abs_val/sum(abs_val), by =c('category','country','brand')]
    
    # agglevel
    tmp = dcast.data.table(dt, category+country+brand~variable, value.var=input$plot_stack_val)#'rel_share')
    
    agglevel = 'country'# input$plot_stack_by #c('country')
    
    
    chosen_vars = unlist(model_order[model_order%in%colnames(tmp)])
    
    tmp2 = tmp[, lapply(.SD, mean,na.rm=T),by=c(agglevel), .SDcol=chosen_vars]
    
    eval(parse(text=paste0('tmp2[, sum:=', paste0(chosen_vars,collapse='+'),']')))
    
    tmp3 = melt(tmp2, id.vars=c(agglevel))
    #levels(tmp3$variable)
    # tmp3[, variable:=factor(as.character(variable), levels=c('llen','pr','dst', 'sum'))]
    #levels(tmp3$variable) <- c('price','distribution','line length')
    levels(tmp3$variable)
    
    setkey(tmp3, country, variable)
    setkey(dt, country, variable)
    
    tmp3[dt, printvar:=get(paste0('i.', input$plot_predicted))]
    
    format_number <- function(x) {
      sub('^(-)?0[.]', '\\1.', formatC(x, digits=3,
              flag="", format="f"))
    }
    
    
    tmp3[, printvar2:=format_number(printvar)]
    
    
    tmp3$country<-str_to_title(tmp3$country) 
    
    if (input$plot_stack_val=='rel_val') levs=unlist(tmp3[variable=='llen',1])[order(tmp3[variable=='llen']$value)]
    if (input$plot_stack_val=='abs_val') levs=unlist(tmp3[variable=='sum',1])[order(tmp3[variable=='sum']$value)]
    
    tmp3[, paste0(agglevel):=factor(as.character(get(agglevel)), levels = levs)]
    
    #
    tmp[, variable_label:=as.character('')]
    
    tmp3[, variable_label:=names(model_order)[match(variable, unlist(model_order))]]
    
    tmp3[, variable_label:=factor(variable_label, levels=rev(unique(names(model_order))))]
    
    
    
    
    #sorted_country =as.character(unique(tmp3$country)[order(tolower(unique(tmp3$country)),decreasing=T)])
    tmp3[, sortval:=value[variable=='sum'], by=c('country')]
    
    
    sorted_country =as.character(unique(tmp3[order(tmp3$sortval)]$country))
    
    tmp3[, country:=factor(country,levels=(sorted_country))]
    
    
    return(ggplot(tmp3[!variable%in%'sum'], aes(fill=variable_label, y=value, 
                                         x=eval(parse(text=agglevel)),
                                         label=printvar2)) + geom_bar(position="stack", stat="identity")  + scale_fill_grey(start = .6, end = .9) + coord_flip() +
      theme_bw()+
      xlab(str_to_title(agglevel)) + ylab('Magnitude of Marketing Elasticities') + 
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      theme(legend.position = 'bottom')+ guides(fill = guide_legend(reverse = TRUE)) +
      labs(fill='Elasticities', caption = paste0('Plot generated on the basis of ', 
                                                 ifelse(input$plot_stack_val=='rel_val', 'relative elasticities', 'absolute elasticities'), ' for brand ', 
                                                 str_to_title(input$plot_brands), ' (', tolower(replace_categories(input$plot_categories)), 
                                                 ').\nCountries shown in alphabetical order.'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())) 
    
  }
  
  output$stacked <- renderPlot({
    
    make_stacked()
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
  
  output$downloadPlots <- downloadHandler(
    filename = "plot.png",
    
    content = function(file) {
      png(file, res=400, units='in', height=5, width=8)
      print(make_stacked())
      dev.off()
    }
  )
  

  
  
  
}


shinyApp(ui = ui, server = server)



#install.packages('rsconnect')
