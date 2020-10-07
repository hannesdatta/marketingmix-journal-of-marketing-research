library(data.table)
library(stringi)

unlink('../output/dataset*')

fns <- list.files('../temp/', pattern = 'dataset.*csv', full.names = T)

my_capitalize <- function(x) {
  sapply(stri_trans_totitle(x), function(y) {
    if (nchar(y)<=3) return(toupper(y)) else return(y)
    
  })
}

for (fn in fns) {

  brand_panel <- fread(fn)
  brand_panel[, date:=as.Date(date, format='%Y-%m-%d')]
  
  brand_panel[country_of_origin=='', country_of_origin:=NA]
  
  ################
  # HOLIDAY DATA #
  ################
  
  holidays <- fread('../../../../data/holidays/holidays.csv')
  
  holidays[country=='AU', country:='australia']
  holidays[country=='CN', country:='china']
  holidays[country=='HK', country:='hong kong']
  holidays[country=='IN', country:='india']
  holidays[country=='ID', country:='indonesia']
  holidays[country=='JP', country:='japan']
  holidays[country=='MY', country:='malaysia']
  holidays[country=='NZ', country:='new zealand']
  holidays[country=='PH', country:='philippines']
  holidays[country=='SG', country:='singapore']
  holidays[country=='KO', country:='south korea']
  holidays[country=='TW', country:='taiwan']
  holidays[country=='TH', country:='thailand']
  holidays[country=='VN', country:='vietnam']
  
  holidays[, month:=as.Date(paste0(substr(date, 1,7),'-01'))]
  
  holiday_period= holidays[public==T, list(N=.N),by=c('country', 'month')]

  # Merge holiday data
  setkey(brand_panel, country, date)
  setkey(holiday_period, country, month)
  brand_panel[holiday_period, npublicholidays:=i.N]
  brand_panel[is.na(npublicholidays), npublicholidays:=0]
  
  
  ##########################
  # DEVELOPMENT INDICATORS #
  ##########################
  
  dev_indicators <- fread('../output/dev_indicators.csv')
  
  brand_panel <- merge(brand_panel, dev_indicators, by = c('year', 'country'), all.x=T)
  
  # set 2010 values
  tmp=copy(dev_indicators[year==2010])
  setnames(tmp, paste0(colnames(tmp), '2010'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('country2010'), all.x=T)
  brand_panel[, year2010:=NULL]
  
  
  ######################
  # WORLD VALUE SURVEY #
  ######################
  
  worldvalues <- fread('../output/worldvalue.csv')
  brand_panel <- merge(brand_panel, worldvalues,by=c('country'), all.x=T)
  
  ###############
  # RULE OF LAW #
  ###############
  
  tmp <- fread('../output/ruleoflaw.csv')
  brand_panel <- merge(brand_panel, tmp,by=c('country'), all.x=T)
  

  ###############
  # BAV METRICS #
  ###############
  
  tmp <- fread('../output/bav.csv')
  brand_panel <- merge(brand_panel, tmp,by=c('country', 'year', 'brand'), all.x=T)
  
  ###############
  # GCI METRICS #
  ###############
  
  # Load GCI infrastructure data
  gci <- fread('../output/gci.csv')
  brand_panel=merge(brand_panel, gci[, c('country', grep('(p[0-9]|overall|sub).*[_]s$', colnames(gci), value=T)),with=F], by = c('country'),all.x=T)
  
  # Load GDP
  #gdp <- fread('../temp/gdp.csv')
  #
  #setkey(gdp, country)
  #elast[gdp, gdppercap2010:=i.gdppercap2010]
  
  ############
  # HOFSTEDE #
  ############
  
  tmp <- fread('../../../../data/hofstede/hofstede_rule_of_law.csv')
  tmp[, country:=tolower(country)]
  brand_panel=merge(brand_panel, tmp, by = c('country'),all.x=T)
  
  brand_panel[, brand_capitalized:=my_capitalize(brand)]
  setorder(brand_panel, market_id, brand, date)
  fwrite(brand_panel, file = gsub('temp', 'output', fn), row.names=F)
  
}

sink('../output/datasets.txt')
cat('done prepping datasets: ', paste0(fns, collapse=', '))
sink()
