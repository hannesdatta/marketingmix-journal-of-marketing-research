# preclean for second-stage analysis

# requires: elast (dataset with elasticities)

# load data for extra variables
hdi <- fread('../temp/hdi.csv')

# change taiwan (reported value from China is not reliable)
hdi[, hdi2010:=ifelse(country=='taiwan', mean(hdi2010[country%in%c('thailand', 'china')]), hdi2010)]

# Load GDP
gdp <- fread('../temp/gdp.csv')

# Merge
setkey(hdi, country)
setkey(elast, country)
elast[hdi, hdi2010:=i.hdi2010]
setkey(gdp, country)
elast[gdp, gdppercap2010:=i.gdppercap2010]

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
vars=c('herf','c3','c5','market_growth','hdi2010','gdppercap2010', 'ncat_in_country', 'ncountry_in_category',
       'overall_ms', 'gci_sub_basicrequire_s','gci_sub_efficiencyenhance_s','gci_sub_innovation_s', 'gci_overall_s')
 
for (var in vars) {
  elast[, (paste0('ln_', var)):=log(get(var))]
  
}

# grand-mean centering by variable (llen, etc.) for all explanatory (continuous) variables
for (var in unique(drop(unlist(lapply(c(vars, 'sbbe_std', 'sbbe'), grep, colnames(elast), value=T))))) {
  elast[, (paste0(var, '_mc')):=get(var)-mean(get(var)), by = c('variable')]
}

# focal dummies: local_to_market, western (versus asian)
elast[, asian_country:=0]
elast[country_of_origin%in%c('south korea', 'japan','taiwan', 'thailand', 'indonesia', 'philippines',
                              'india', 'singapore', 'malaysia', 'vietnam', 'cambodja', 'pakistan', 
                              'hong kong'), asian_country:=1]


elast[, worldbank := '']
elast[country%in%c('india','indonesia', 'vietnam', 'philippines'), worldbank:='lowermid']
elast[country%in%c('china', 'malaysia','thailand'), worldbank:='uppermid']
elast[worldbank=='', worldbank:='high']

options(knitr.kable.NA = '')

### brand selection for analysis
#elast=elast[globalbrand==T] # global brands (active in >=3 countries) only

#elast=elast[ncountries>=2]

ordered_vars = c('rwpspr', 'wpswdst','llen','nov6sh')

elast[!is.na(elast), weightsst := (1/elast_se)/sum(1/elast_se), by = c('variable')]
elast[!is.na(elastlt), weightslt := (1/elastlt_se)/sum(1/elastlt_se), by = c('variable')]
elast[, othercountry:=0]
elast[country_of_origin%in%c('','egypt', 'united arab emirates'), othercountry:=1]
