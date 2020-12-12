rm(list=ls())
# Load data
library(lme4)
library(bit64)
library(data.table)
library(stargazer)
library(shiny)

fns <- c('../temp/app_workspace.RData', 'app_workspace.RData')

for (fn in fns) if (file.exists(fn)) {cat(paste0('loading...', fn, '...\n')); load(fn)}

# copy of functions


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


all_mods <- function(models) {
  lapply(models, function(forms) {
    list(m1 = lmer(update.formula(elastlt~1, forms$pr),
                   data=elast[grep('pr',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         m2 = lmer(update.formula(elastlt~1, forms$dst),
                   data=elast[grep('dst',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F),
         m3 = lmer(update.formula(elastlt~1, forms$llen),
                   data=elast[grep('llen',variable)], weights=w_elastlt,
                   control = lmerctrl, REML=F))
    
  })
}
# estimate models
newmodV2 <- function(model, fn, ..., return_models = T) {
  
  mods = all_mods(model)
  rsqs=unlist(lapply(mods, function(x) lapply(x, rsq)))
  obss = unlist(lapply(mods, function(x) lapply(x, function(i) length(which(!is.na(residuals(i)))))))
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(rsqs, digits=3, format='f', flag='#')))
  obs = c('Observations',obss)
  
  stargazer(do.call('c', mods),type='html', 
            column.labels = rep(c('price','distribution','line length'), length(model)), 
            out = fn, add.lines = list(r2s,obs), ...)
  return(mods)
}

lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")



#########################################################
##### Add variables to choice set #
#########################################################

names(elasticities)

model_names = list('M1) Error correction (sales)' = 'ec_restricted_sigcop',
                   'M2) Error correction (sales; but with copula of d_mmix)' = 'ec_restricted_sigdcop',
                   'M3) Attraction model (market share)' = 'marketshare')
                   


potential_vars = list(brandequity=list('SBBE' = 'sbbe_round1_mc',
                                       'BrandZ indicator' = 'brandz',
                                       'Brand strength (BAV)' = 'ln_bav_brandstrength_mc',
                                       'Brand stature (BAV)' = 'ln_bav_brandstature_mc',
                                       'Brand energized diff. (BAV)' = 'ln_bav_energizeddifferentiation_mc',
                                       'Marketshare' = 'ln_brand_ms_mc'),
                      brandlocation = list('Domestic market indicator' = 'local_to_market_mc',
                                           'JP, US, Swiss, GER, Sweden indicator' = "`brand_from_jp-us-ch-ge-sw_mc`",
                                           'Western brand indicator' = 'western_brand_mc'),
                      brandmmix = list('Price (log index)' = 'ln_rwpspr_index_mc',
                                       'Price (index)' = 'rwpspr_index_mc',
                                       'Price (std.)' = 'rwpspr_std_mc',
                                       'Distr. (log index)' = 'ln_wpswdst_index_mc',
                                       'Distr. (index)' = 'wpswdst_index_mc',
                                       'Distr. (std.)' = 'wpswdst_std_mc',
                                       'Line length (log index)' = 'ln_llen_index_mc',
                                       'Line length (index)' = 'llen_index_mc',
                                       'Line length (std.)' = 'llen_std_mc'),
                      
                      brandother = list('Brand novelty' = 'ln_brandnovelty3_mc',
                                        'Price positioning' = 'brand_prindex_mean_mc'),
                      
                      category = list('Market concentration' = "ln_market_herf_mc",
                                      'Market growth' = "ln_market_growth_mc",
                                      'Category innovativeness' = 'ln_catnovelty3_mc',
                                      'Appliances (vs. electronics)' = 'appliance'),
                      
                      country_econ = list('GDP per capita (in 2010)' = "ln_gdppercapitacurrent2010_mc",
                                          'GDP per capita (avg. 2004-2014)' = "ln_gdppercapitacurrentavg_mc",
                                          'GDP growth (in 2010)' = 'ln_gdpgrowth2010_mc',
                                          'GDP growth (avg. 2004-2014)' = 'ln_gdpgrowthavg_mc',
                                          "Income Inequality (nearest to 2010)" = "ln_ginicoef_mc",
                                          'HDI (in 2010)' = 'ln_hdi2010_mc',
                                          "Emerging market indicator" = "emerging",
                                          "Goods Market Efficiency (GCI 2010)" = "ln_gci_p06_goods_s_mc",
                                          "Infrastructure (GCI 2010)" = 'ln_gci_p02_infrastructure_s_mc'),
                      
                      country_culture = list("WVS: Traditional vs. rational" = "tradrat_mc",
                                             "WVS: Survival vs. Self-expression" = "survself_mc",
                                             "Hofstede: Longterm orientation" = "ln_ltowvs_mc",
                                             "Hofstede: Individualism" = "ln_idv_mc",
                                             "Hofstede: Uncertainty Avoidance" = "ln_uai_mc",
                                             "Hofstede: Power distance" = "ln_pdi_mc",
                                             "Hofstede: Masculinity" = "ln_mas_mc"),
                      country_institutions = list("Rule of law" = "ln_wjp_rule_of_law_mc",
                                                  "Institutions (GCI)" = 'ln_gci_p01_institutions_s_mc')
)

# variable check
all_cols = unique(unlist(lapply(elasticities, colnames)))
unlist(lapply(potential_vars, function(x) x[!x%in%all_cols]))

#grep('gdpper', all_cols,value=T)


ui <- fluidPage(
  titlePanel("Model exploration: GfK Singapore"),
  sidebarLayout(
    sidebarPanel(
      selectInput("brandequity", label = h5("Brand factors: Equity"),
                  choices = (potential_vars$brandequity), selected = 1, multiple=TRUE),
      selectInput("brandlocation", label = h5("Brand factors: Location"),
                  choices = (potential_vars$brandlocation), selected = 1, multiple=TRUE),
      selectInput("brandmmix", label = h5("Brand factors: Marketing mix"),
                choices = (potential_vars$brandmmix), selected = 1, multiple=TRUE),
      selectInput("brandother", label = h5("Brand factors: Others"),
              choices = (potential_vars$brandother), selected = 1, multiple=TRUE),

      selectInput("categoryfactors", label = h5("Category factors"),
                  choices = (potential_vars$category), selected = 1, multiple=TRUE),
      selectInput("econ", label = h5("Country factors: Economic development"),
                  choices = (potential_vars$country_econ), selected = 1, multiple=TRUE),
      selectInput("culture", label = h5("Country factors: Culture"),
                  choices = (potential_vars$country_culture), selected = 1, multiple=TRUE),
      selectInput("institutions", label = h5("Country factors: Institutions"),
                  choices = (potential_vars$country_institutions), selected = 1, multiple=TRUE),
      textInput("interact", label = h5("Interactions"), value = ""),
      selectInput("model", label = h5("Model"),
                  choices = model_names,
                  selected=1)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                 # tabPanel("Model Summary", verbatimTextOutput("summary")),
                 tabPanel("Model results", htmlOutput("stargazer"))#, # Regression output
                 # tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))



# SERVER
server <- function(input, output) {
  
  output$stargazer = renderText({
  # assemble form 
    

    vars=paste0(c(unlist(input$brandequity),
                  unlist(input$brandlocation),
                  unlist(input$brandmmix),
                  unlist(input$brandother),
                  
                  unlist(input$categoryfactors), 
                  unlist(input$econ),
                  unlist(input$culture),
                  unlist(input$institutions)), collapse='+')
    myform = as.character('. ~ 1 + (1|country) + (1|category) + (1|brand)')
    
    if (nchar(vars)>0) myform = paste0(myform, ' + ', vars)
    if (nchar(unlist(input$interact))>0) myform = paste0(myform, ' + ', unlist(input$interact))
    
    mainef = as.formula(myform)
                      
    
    #  emerging + sbbe_round1_mc*emerging + 
    #  ln_market_herf_mc + ln_market_growth_mc + 
    #  appliance + ln_ginicoef_mc + local_to_market_mc + tradrat_mc + survself_mc
    elast <<- elasticities[[input$model]][selection_obs48==T&selection_brands==T]
    
    #if (input$model=='ec') elast <<- copy(elast_sales)
    #if (input$model=='attraction') elast <<- copy(elast_marketshare)
    #input
    paste0(paste0(capture.output({me<-newmodV2(list(list(pr=mainef, dst=mainef, llen=mainef)), NULL, return_models=F)}), collapse=''),
           '<br><br>', myform)
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
