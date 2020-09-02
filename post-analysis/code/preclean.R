# preclean for second-stage analysis

# requires: elast (dataset with elasticities)
elast[country_of_origin=='', country_of_origin:=NA]

# load data for extra variables
hdi <- fread('../temp/hdi.csv')

# change taiwan (reported value from China is not reliable)
hdi[, hdi2010:=ifelse(country=='taiwan', .879, hdi2010)]

# Load GDP
gdp <- fread('../temp/gdp.csv')

# Load GINI
gini <- fread('../temp/gini.csv')

# Merge
setkey(hdi, country)
setkey(elast, country)
elast[hdi, hdi2010:=i.hdi2010]
setkey(gdp, country)
elast[gdp, gdppercap2010:=i.gdppercap2010]

setkey(elast, country)
setkey(gini, country)
elast[gini, gini:=i.ginicoef]

# elast asian devel vs. not
elast[, region_of_origin:=country_of_origin]
elast[region_of_origin=='asian'& country_class=='hinc', region_of_origin := 'asian-high']
elast[region_of_origin=='asian'& country_class=='linc', region_of_origin := 'asian-low']

elast[, developed:=0]
elast[country%in%c('australia', 'singapore', 'japan', 'new zealand', 'hong kong', 'south korea', 'taiwan'), developed := 1]


# Load GCI infrastructure data
gci <- fread('../temp/gci.csv')

elast=merge(elast, gci, by = c('country'),all.x=T)

# take natural log of variables
vars=c('market_herf','market_c3','market_c5', 'gini', 'market_growth','hdi2010','gdppercap2010', 'ncat_in_country', 'ncountry_in_category',
       'brand_ms', 'brand_prindex_max','brand_prindex_mean')
 #'gci_00.03_gdppercap_s',  #, 'gci_sub_basicrequire_s','gci_sub_efficiencyenhance_s','gci_sub_innovation_s', 'gci_overall_s'
for (var in vars) {
  elast[, (paste0('ln_', var)):=log(get(var))]
  
}

# grand-mean centering by variable (llen, etc.) for all explanatory (continuous) variables
for (var in unique(drop(unlist(lapply(c(vars, 'sbbe_std', 'sbbe', 'sbbelt', 'meanglobalx'), grep, colnames(elast), value=T))))) {
  print(var)
  elast[, (paste0(var, '_mc')):=get(var)-mean(get(var)), by = c('variable')]
}

# focal dummies: local_to_market, western (versus asian)
elast[, local_to_market:=as.numeric(country_of_origin==country & ncountries==1)]
elast[, local_multip_market:=as.numeric(country_of_origin==country&ncountries>1)]


elast[, asian_brand:=0]
elast[country_of_origin%in%c('south korea', 'japan','taiwan', 'thailand', 'indonesia', 'philippines',
                              'india', 'singapore', 'malaysia', 'vietnam', 'cambodja', 'pakistan', 
                              'hong kong'), asian_brand:=1]


# country classifications
western=c('australia', 'canada','finland','france', 'germany','great britain', 
          'italy','luxembourg', 'netherlands','new zealand', 'spain', 'sweden',
          'switzerland', 'turkey', 'usa')

elast[, western_brand:=as.numeric(country_of_origin%in%western)]

elast[, worldbank := '']
elast[country%in%c('india','indonesia', 'vietnam', 'philippines'), worldbank:='lowermid']
elast[country%in%c('china', 'malaysia','thailand'), worldbank:='uppermid']
elast[worldbank=='', worldbank:='high']

options(knitr.kable.NA = '')

ordered_vars = c('rwpspr', 'wpswdst','llen','nov12sh')
elast[, other_brand:=1-asian_brand-western_brand]

elast[, emerging:=1-developed]

elast[, brandz:=0]
brandz_brands<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone','lenovo', 'haier', 'midea', 'hisense')
elast[brand%in%brandz_brands, brandz:=1]

elast[, brandz_globalonly:=0]
brandz_brands_globalonly<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone')
elast[brand%in%brandz_brands_globalonly, brandz_globalonly:=1]

elast[, brandz_chinaonly:=0]
elast[brand%in%c('lenovo', 'haier', 'midea', 'hisense'), brandz_chinaonly:=1]

elast[, brandz_global_alltime:=0]
brandz_brands_all<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone', 'canon', 'motorola')
elast[brand%in%brandz_brands_all, brandz_global_alltime:=1]

elast[, brandz_financial500:=0]
brandz_brands_fin<-c('samsung', 'sony', 'apple', 'hp', 'nokia', 'dell','blackberry', 'ge', 'siemens', 'ibm','vodafone', 'canon', 'motorola',
                     'fujifilm', 'huawei', 'lenovo', 'lg', 'panasonic', 'philips', 'toshiba', 'zte')
elast[brand%in%brandz_brands_fin, brandz_financial500:=1]

elast[, brand:=my_capitalize(brand)]
