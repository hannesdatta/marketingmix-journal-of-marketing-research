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

fns <- c('app_workspace.RData')

for (fn in fns) if (file.exists(fn)) {cat(paste0('loading...', fn, '...\n')); load(fn)}



if(0) {
#
vifuniq=unique(elasticities$with_sur, by = c('category','country', 'brand'))
vifuniq[, random:=runif(.N)]

vifeq <- lm(random~1+sbbe_round1_mc+local_to_market_mc+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_market_growth_mc+appliance+ln_market_herf_mc+ln_gdpgrowthavg_mc+ln_ginicoef_mc+ln_idv_mc+ln_ltowvs_mc+wgi_regulatoryqualavg, data=vifuniq)
vif(vifeq)
}

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
  forms= lapply(c('pr','dst', 'llen'), function(x) {
    effects=sel[variable==x]
    if (nrow(effects)>0) return(update.formula(maineffects, formula(paste0('.~.+', paste0(effects$varname,collapse='+')))))
    return(maineffects)
    
  })
  names(forms) <- c('pr','dst', 'llen')
  forms
}


all_mods <- function(models, mtype = 'lmer', clust = NULL) {
  lapply(models, function(forms) {
    if (mtype=='lm')  {
      
      m1 = lm(update.formula(elastlt~1, forms$pr),
              data=elast[grep('pr',variable)], weights=w_elastlt)
      m2 = lm(update.formula(elastlt~1, forms$dst),
              data=elast[grep('dst',variable)], weights=w_elastlt)
      m3 = lm(update.formula(elastlt~1, forms$llen),
              data=elast[grep('llen',variable)], weights=w_elastlt)
      
      if (!is.null(clust)) {
        m1 <- coeftest(m1, vcov = vcovCL, cluster = clust)
        m2 <- coeftest(m2, vcov = vcovCL, cluster = clust)
        m3 <- coeftest(m3, vcov = vcovCL, cluster = clust)
        return(list(m1=m1,m2=m2,m3=m3)) 
        
      }
      
      return(list(m1=m1,m2=m2,m3=m3))
      
    }
    
    if (mtype=='lmer') {
      
    return(list(m1 = lmer(update.formula(elastlt~1, forms$pr),
                   data=elast[grep('pr',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         m2 = lmer(update.formula(elastlt~1, forms$dst),
                   data=elast[grep('dst',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         m3 = lmer(update.formula(elastlt~1, forms$llen),
                   data=elast[grep('llen',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F)))
    }
    
  })
}


# estimate models

newmodV2 <- function(model, fn, ..., return_models = T, mtype='lmer', clust = NULL) {
  
  mods = all_mods(model, mtype=mtype, clust=clust)
  
  #if(0){
  if (mtype=='lmer'){
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = NULL
  
  if (mtype=='lmer') r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  lbllist = list(r2s,obs)
 } #if (mtype=='lm') 
    
  if (mtype=='lm') lbllist = NULL
  #lbllist = NULL
      
  stargazer(do.call('c', mods),type='html', 
            column.labels = rep(c('price','distribution','line length'), length(model)), 
            out = fn, add.lines = lbllist, ...)
  return(mods)
}

lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")



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
                                           'JP, US, Swiss, GER, Sweden indicator' = "`brand_from_jp-us-ch-ge-sw_mc`",
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
                      
                      brandother = list('Brand novelty' = 'ln_brandnovelty3_mc'),#,
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
             textInput("interact", label = h5("Interactions"), value = "")
           ),
           tabPanel("Model specification", 
             selectInput("model", label = h5("Model specification"),
                       choices = model_names,
                       selected=model_names[1]),
             selectInput("estim", label = h5("Model estimation"),
                       choices = list('Mixed-Effects Model (REs)'= 'lmer',
                                      'OLS' = 'lm'), selected= 'lmer', multiple=F),
             selectInput("randomeffects", label = h5("List of random effects (for RE model), or clustering (in case of OLS)"),
                       choices = list('Brand'= 'brand', 'Category'='category',
                                      'Country' = 'country'), selected = c('brand'), multiple=TRUE),
           
             selectInput("trimming", label = h5("Trimming/Winsorizations"),
                       choices = (trimming), selected = trimming[4], multiple=FALSE)),
           
           tabPanel("Variables2", 
                    selectInput("brandequity2", label = h5("Brand factors: Equity"),
                                choices = (potential_vars$brandequity), selected = potential_choices$brandequity, multiple=TRUE))
           
           )
    ),
    column(8,
           tabsetPanel(type = "tabs",
                       
                       # tabPanel("Model Summary", verbatimTextOutput("summary")),
                       tabPanel("Model results", htmlOutput("stargazer")),#, # Regression output
                       tabPanel("VIFs", htmlOutput("vif"))#,
                     #  tabPanel("Correlations", htmlOutput("correl"))#, # Regression output
                       #, # Regression output
                       # tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
        #  htmlOutput("stargazer")
    ))
  
)
  
  )




# test
if(0){
mainef = . ~ 1 + sbbe_round1_mc+local_to_market_mc+ln_rwpspr_index_mc+ln_wpswdst_index_mc+ln_llen_index_mc+ln_market_herf_mc+ln_market_growth_mc+appliance+ln_gdpgrowthyravg_mc+ln_ginicoef_mc+tradrat_mc+survself_mc+wgi_regulatoryqualyravg_mc
clust = ~ brand+category+country
modeltype='lm'
me<-newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)),
                                                fn= 'NULL.html', return_models=T, mtype=modeltype,
                                                clust=NULL)

test=coeftest(me[[1]]$m1, vcov = vcovCL, cluster = clust)


#summary(me[[1]]$m1)

#me[[1]]

}

#
get_model <- function(input) {
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
  #cat(file=stderr(), as.character(clust), " clusters", "\n")
  
  if (nchar(vars)>0) myform = paste0(myform, ' + ', vars)
  if (nchar(unlist(input$interact))>0) myform = paste0(myform, ' + ', unlist(input$interact))
  
  list(formula = myform, modeltype=modeltype, randomeffects = randomef, cluster = clust)
}

get_sample <- function(input) {
  tmp <- copy(elasticities[[input$model]][selection_obs48==T&selection_brands==T])
  
  tmp <- tmp[!is.na(elastlt), percentile:=ecdf(elastlt)(elastlt), by = c('variable')]
  
  # input=list(trimming='trim_1')
  
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

# look up
#hash(input)


storage <<- NULL

# SERVER
server <- function(input, output) {
  
  
  output$stargazer = renderText({
  # assemble form 
    saved_input = reactiveValuesToList(input)

    #save(saved_input, file='inputs.RData')
    my_hash <- function(input, ...) digest(paste0(paste0(names(input), collapse='_'), '|', paste0(sapply(unlist(input), function(x) if(x=='') return('[ ]') else return(x)), collapse='_')),...)
    
    
    hash=my_hash(reactiveValuesToList(input), algo=c("md5"))
    outp = storage[[hash]]$results
    if (is.null(outp)) {
      
      mspec = get_model(input)
     
      elast <<- get_sample(input)
      
      outp=paste0(paste0(capture.output({me<-newmodV2(list(list(pr=mspec$formula, dst=mspec$formula, llen=mspec$formula)),
                                                      fn= NULL, return_models=F, mtype=mspec$modeltype,
                                                      clust=mspec$cluster)}), collapse=''),
             '<br><br>', mspec$formula, "<br><br>", paste0(as.character(mspec$cluster), collapse=''), '<br><br>', mspec$modeltype)
      
      storage[[hash]]$results <<- paste0(outp, '<br>from memory<br>')
      rm(me)
    }
    print(str(storage))
    return(outp)
  })

  output$vif = renderText({
    # assemble form 
    
    
    vars=paste0(c(unlist(input$brandequity),
                  unlist(input$brandlocation),
                  unlist(input$brandmmix),
                  unlist(input$brandother),
                  
                  unlist(input$categoryfactors), 
                  unlist(input$econ),
                  unlist(input$culture),
                  unlist(input$institutions)), collapse='+')
    myform = as.character('. ~ 1 ')
    
    if (nchar(vars)>0) myform = paste0(myform, ' + ', vars)
    if (nchar(unlist(input$interact))>0) myform = paste0(myform, ' + ', unlist(input$interact))
    
    mainef = as.formula(myform)
    
    tmp <- unique(copy(elasticities[[input$model]][selection_obs48==T&selection_brands==T]), by = c('category','country','brand'))
    tmp[, randomnr:=runif(.N)]
    
    vifm <- lm(update.formula(randomnr~., mainef), data=tmp)
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
}


shinyApp(ui = ui, server = server)



#install.packages('rsconnect')
