rm(list=ls())

# Load data
library(lme4)
library(bit64)
library(data.table)
library(car)
library(stargazer)
library(sandwich)

load('../output/workspace.RData')
brand_panel <- fread('../externals/preclean_main.csv')
source('proc_auxilary.R')

# Correlations between Hofstede Dimensions
# ========================================

hofstede <- unique(brand_panel[, c('country', 'pdi', 'idv')], by = c('country'))
cor(hofstede$pdi, hofstede$idv)

# Number of markets on which we estimate models
# ================================================

nrow(brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('category','country')])

# For information on excluded categories, see
# \derived\output\brand_selection_main.txt

# Zooming in on the tablet category in Hong Kong
# ====================================================

load('../../derived/temp/categorized.RData')

length(unique(datlist_final$tablets[country=='HONG KONG']$brand))
# --> more than 100

brands_by_marketshare = datlist_final$tablets[country=='HONG KONG'][, list(sales=sum(sales_units)),by = c('brand')]
setorderv(brands_by_marketshare, 'sales', order = -1L)
brands_by_marketshare[, marketshare := sales/sum(sales)]
sum(brands_by_marketshare$marketshare[1:2])
# > 80% for the top x brands

# Number of included brands & %-unit sales coverage
# ====================================================

# unit sales coverage
included_sales = sum(brand_panel[selected==T&timewindow==T&obs48, list(sales=sum(usales)),by=c('brand','category','country')]$sales)
total_sales = sum(brand_panel$usales)

included_sales/total_sales

# number of brands
nrow(brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)])

# cross-check: number of brands for estimation
nrow(elast[, list(.N),by= c('category','country','brand')])

# number of unique brands
uniq_brands = unique(brand_panel[selected==T&timewindow==T&obs48, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]$brand)
uniq_brands = uniq_brands[order(uniq_brands)]
length(uniq_brands)

# Number of SKUs in raw data
# ==========================

skus=rbindlist(lapply(datlist_final, function(x) x[, list(1),by=c('catname','country','brand','model')][, list(number_of_skus=.N),by=c('catname','country','brand')]))
skus[, list(number_of_skus_in_category = sum(number_of_skus)), by = c('catname','country')]
# --> >1000

# Assessing multi-collinearity
# ===================================================

tmp = elast[variable == 'rwpspr' & !country_of_origin=='']
tmp[, random_regressor := runif(.N)]

model_formula <- random_regressor ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw`+ln_rwpspr_windex_mc+ln_wpswdst_windex_mc+ln_llen_windex_mc+ln_nov6sh_windex_mc+ln_market_herf_mc+ln_market_meangrowth_mc+appliance+ln_penn_growthrgdpeyravg_mc+ln_ginicoef_mc+ln_penn_percapitargdpeyravg_mc+ln_penn_popyravg_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc

m <- lm(update(random_regressor~., model_formula), data=tmp)

vifs=data.frame(vif(m))
colnames(vifs) <- c('VIF')

max(vifs$VIF)

# --> 5.36

# Shapiro-Wilk Tests for Non-Normality of Marketing-Mix Regressors
# ================================================================

norm_tests <- rbindlist(lapply(c('rwpspr','llen','wpswdst'), function(.v) brand_panel[selected==T&timewindow==T&obs48==T&!is.na(get(.v))&!grepl('allother',brand), list(shapiro_p_val=shapiro.test(get(.v))$p), by = c('category','country','brand')][,variable:=.v]))
norm_tests[, list(non_normal_share = length(which(shapiro_p_val<=.1))/.N),by=c('variable')]
norm_tests[, list(non_normal_share = length(which(shapiro_p_val<=.1))/.N)]

# Share of retained copula terms
# ==============================

load('../../analysis/output/results_ec_main.RData')

variables <- unlist(lapply(results_ec_main, function(x) grep('cop[_]|d(rwpspr|llen|wpswdst)', names(x$model$coefficients), value=T)))

copula_terms = grep('cop[_]', variables, value = T)
non_copula_terms = grep('^d', variables, value = T)

length(copula_terms)/length(non_copula_terms)

# Nested model F-tests
# ==============================

# Load models
  models <- c('ec_main_nc', 'ec_noocmmixnc',
              'ec_main', 'ec_remocprnc', 'ec_remocllennc', 'ec_remocdstnc',
              'ec_remprnc', 'ec_remllennc', 'ec_remdstnc',
              'ec_onlyocllennc', 'ec_onlyocdstnc',
              'ec_onlyocprnc')


  for (fn in models) load(paste0('../../analysis/output/', 'results_', fn, '.RData'))

# Retrieve necessary information from estimated models
  out = rbindlist(lapply(models, function(mod) {

    res=rbindlist(lapply(eval(parse(text=paste0('results_', mod))), function(x) data.table(x$dt[, c('brand','category','country'),with=F][1],
                                                                                           SSR=sum(x$model$residuals^2),
                                                                                           df = x$model$df.residual,
                                                                                           N = length(x$model$residuals),
                                                                                           sigma = summary(x$model)$sigma)))
    res[, model := mod]
    res
  }))

# Aggregate over model types
  test_stats = out[, list(N_models = .N, SSR=sum(SSR), df=sum(df), N=sum(N)),by=c('model')]

# Check number of estimated models
  stopifnot(all(test_stats$N_models==1619))

# Test significance

  # https://influentialpoints.com/notes/n8rttst.htm
  f_test <- function(full, restricted) {

    cat(paste0('\nExtra-sum-of-squares test (F-test) for: \n'))
    cat(paste0('  Restricted model: ', restricted),fill=T)
    cat(paste0('  Full model: ', full, '\n\n\n'))

    SSR_diff = test_stats[model==restricted]$SSR-test_stats[model==full]$SSR
    df_diff = test_stats[model==restricted]$df-test_stats[model==full]$df

    denominator = test_stats[model=='ec_main']$SSR/test_stats[model==full]$df
    test_statistic = (SSR_diff/df_diff)/denominator

    df1 = df_diff
    df2 = test_stats[model==full]$df
    p=pf(test_statistic,df1,df2)

    if(p > .5) p = 1-p

    cat(paste0('  Test statistic (F): ', formatC(test_statistic, digits=3)),fill=T)
    cat(paste0('  F(', df1, ', ', df2, ') = ', formatC(p, digits=3), ' (p-value)'),fill=T)

  }


  print(test_stats)
  cat('\n\n\n')
  cat('1) Significance of *removing* focal and competitive marketing-mix [all models without Copula terms]', fill=T)
  f_test(full = 'ec_main_nc', restricted = c('ec_remocllennc'))
  f_test(full = 'ec_main_nc', restricted = c('ec_remocprnc'))
  f_test(full = 'ec_main_nc', restricted = c('ec_remocdstnc'))

  cat('\n\n\n')
  cat('2) Significance of *adding* focal and competitive marketing-mix to a model WITHOUT marketing mix instruments [all without Copula terms]', fill=T)
  f_test(full = 'ec_onlyocllennc', restricted = c('ec_noocmmixnc'))
  f_test(full = 'ec_onlyocprnc', restricted = c('ec_noocmmixnc'))
  f_test(full = 'ec_onlyocdstnc', restricted = c('ec_noocmmixnc'))

  cat('\n\n\n')



# Comparison of elasticities at the mean vs. median
# =================================================

iterator = list(mean = c('elastlt','elastlt_se'),
                median = c('elastmedianlt', 'elastmedianlt_se'))

lapply(iterator, function(iter) {
  tmp = elast[!is.na(get(iter[1]))&!grepl('super|amazon', brand)]

  tmp[, tmp_w := 1/get(iter[2]), by = c('variable')]

  tmp[, list(Neffects=.N,
                   weighted_elast = round(sum(get(iter[1])*tmp_w)/sum(tmp_w),3)),
            by=c('variable')]
  })


# Coefficient of variation
# =================================================

elast[!is.na(elastlt) & !grepl('super|amazon', brand), list(N=.N,CV = round(sd(elastlt)/mean(elastlt),3)), by = c('variable')]

# Re-estimation of main model and alternative configurations
# ==========================================================

# Model specification
  model_formulas <- list(main_model  = . ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw`+ln_rwpspr_windex_mc+ln_wpswdst_windex_mc+ln_llen_windex_mc+ln_nov6sh_windex_mc+ln_market_herf_mc+ln_market_meangrowth_mc+appliance+ln_penn_growthrgdpeyravg_mc+ln_ginicoef_mc+ln_penn_percapitargdpeyravg_mc+ln_penn_popyravg_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc,
                        emerging_market  = . ~ 1 + sbbe_round1_mc+`brand_from_jp-us-ch-ge-sw`+ln_rwpspr_windex_mc+ln_wpswdst_windex_mc+ln_llen_windex_mc+ln_nov6sh_windex_mc+ln_market_herf_mc+ln_market_meangrowth_mc+appliance+emerging,
                        western_brand  = . ~ 1 + sbbe_round1_mc+western_brand+ln_rwpspr_windex_mc+ln_wpswdst_windex_mc+ln_llen_windex_mc+ln_nov6sh_windex_mc+ln_market_herf_mc+ln_market_meangrowth_mc+appliance+ln_penn_growthrgdpeyravg_mc+ln_ginicoef_mc+ln_penn_percapitargdpeyravg_mc+ln_penn_popyravg_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc,
                        omit_brandequity  = . ~ 1 + `brand_from_jp-us-ch-ge-sw`+ln_rwpspr_windex_mc+ln_wpswdst_windex_mc+ln_llen_windex_mc+ln_nov6sh_windex_mc+ln_market_herf_mc+ln_market_meangrowth_mc+appliance+ln_penn_growthrgdpeyravg_mc+ln_ginicoef_mc+ln_penn_percapitargdpeyravg_mc+ln_penn_popyravg_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc,
                        omit_brandequity2  = . ~ 1 + western_brand+ln_rwpspr_windex_mc+ln_wpswdst_windex_mc+ln_llen_windex_mc+ln_nov6sh_windex_mc+ln_market_herf_mc+ln_market_meangrowth_mc+appliance+ln_penn_growthrgdpeyravg_mc+ln_ginicoef_mc+ln_penn_percapitargdpeyravg_mc+ln_penn_popyravg_mc+ln_uai_mc+ln_pdi_mc+ln_mas_mc)


  for (model_formula in model_formulas) {
  # Prepare data
    tmp = data.table(elast[!grepl('super|amazon', brand)])
    tmp[, internat_brand:=ifelse(ncountries>1,1,0)]

    dv_name='elastlt'
    tmp[, ':=' (dv = get(dv_name), dv_se = get(paste0(dv_name,'_se')), w_dv = 1/get(paste0(dv_name,'_se')))]

  # Winsorization
    tmp[!is.na(dv), percentile:=ecdf(dv)(dv), by = c('variable')]
    perc_extract = 0.01

    tmp[, perc_low := quantile(dv, probs = perc_extract/2), by = c('variable')]
    tmp[, perc_high := quantile(dv, probs = 1-perc_extract/2), by = c('variable')]

    tmp[percentile<perc_extract/2, dv:=perc_low]
    tmp[percentile>(1-perc_extract/2), dv:=perc_high]

    # check w/ Marnik: one or two tailed?

    ordered_vars <- c('llen', 'rwpspr', 'wpswdst')

    model_results <- lapply(ordered_vars, function(var) {
      estim_data = tmp[grepl(var, variable)]
      m<-lm(update.formula(dv~1, model_formula), data = estim_data, weights = w_dv)

      m <- coeftest(m, vcov = vcovCL, cluster = ~ brand + category + country, fix = T, type = 'HC1')

      return(m)
    })

    stargazer(model_results, type = 'text')
  }

# Correlation emerging market dummy & power distance
# ==========================================================

with(unique(elast, by = c('country')), cor(pdi, emerging))



# Additional requests/stats for the paper
# ==========================================================

# Compute correlations between country factors and levels/coefficient of variation for price and line length

# filter on data that we use in the second stage
tmp = brand_panel[brand_id%in%elast$brand_id, list(llen = mean(llen),
                                                   llen_sd = sd(llen)/mean(llen),
                                                   price = mean(rwpsprd),
                                                   price_sd = sd(rwpsprd),
                                                   penn_pop=mean(penn_pop),
                                                   penn_growthrgdpe = mean(penn_growthrgdpe),
                                                   penn_rgdpe=mean(penn_rgdpe)), by = c('country','category','brand')]

tmp[,llen_coefvar := llen_sd/llen]
tmp[,price_coefvar := price_sd/price]
tmp[,penn_gdppercap := penn_rgdpe/penn_pop]

# collapse to country level first
outc=tmp[, lapply(.SD, mean, na.rm=T), by = c('country'), .SDcols = c('llen','llen_coefvar',
                                                                      'price','price_coefvar',
                                                                      'penn_pop','penn_gdppercap','penn_growthrgdpe')]

print(corstars(data.frame(outc[,-1,with=F])), type= 'text')

sink('tables_and_figures_other.log')
cat('done')
sink()
