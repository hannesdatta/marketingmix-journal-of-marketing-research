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
  tmp=copy(dev_indicators[year==2010])[, year:=NULL]
  setnames(tmp, paste0(colnames(tmp), '2010'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('country2010'), all.x=T)
  
  # set mean values
  tmp=copy(dev_indicators)[, lapply(.SD, mean, na.rm=T), by = c('country'), .SDcols=setdiff(colnames(dev_indicators), c('year','country'))]
  setnames(tmp, paste0(colnames(tmp), 'avg'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('countryavg'), all.x=T)
  
  #######
  # WGI #
  #######
  
  wgi <- fread('../output/wgi.csv')
  
  brand_panel <- merge(brand_panel, wgi, by = c('year', 'country'), all.x=T)
  
  # set 2010 values
  tmp=copy(wgi[year==2010])[, year:=NULL]
  setnames(tmp, paste0(colnames(tmp), '2010'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('country2010'), all.x=T)
  
  # set mean values
  tmp=copy(wgi)[, lapply(.SD, mean, na.rm=T), by = c('country'), .SDcols=setdiff(colnames(wgi), c('year','country', 'country_code'))]
  setnames(tmp, paste0(colnames(tmp), 'avg'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('countryavg'), all.x=T)
  
  
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
  .vars=grep('[_]R$',colnames(tmp),value=T)
  tmp_means <- tmp[, lapply(.SD,mean,na.rm=T),by=c('country','brand'),
                   .SDcols=.vars]
  for (.var in .vars) setnames(tmp_means, .var, paste0('bav_',gsub('[_]R$|[_]','',tolower(.var),ignore.case=T)))
  
  brand_panel <- merge(brand_panel, tmp,by=c('country', 'year', 'brand'), all.x=T)
  brand_panel <- merge(brand_panel, tmp_means,by=c('country', 'brand'), all.x=T)
  
  ###############
  # GCI METRICS #
  ###############
  
  # Load GCI infrastructure data
  gci <- fread('../output/gci.csv')
  brand_panel=merge(brand_panel, gci[, c('country', grep('(p[0-9]|overall|sub|population).*[_]s$', colnames(gci), value=T)),with=F], by = c('country'),all.x=T)

  ################
  # NEAREST GINI #
  ################
  
  # Load GCI infrastructure data
  gini <- fread('../output/gini.csv')
  brand_panel=merge(brand_panel, gini, by = c('country'),all.x=T)
  
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
  
  
  ######################
  # COUNTRY REPUTATION #
  ######################
  
  
  # get reputation data
  rep <- fread('../../../../data/reptrak/reputation.csv')
  rep[, score2013:=as.numeric(gsub('[,]','.', rep2013))]
  rep[, score2015:=as.numeric(gsub('[,]','.', rep2015))]
  rep[!is.na(score2013)&!is.na(score2015), total_score:=(score2013+score2015)/2]
  rep[is.na(score2013)&!is.na(score2015), total_score:=(score2015)]
  
  setkey(rep, country)
  setkey(brand_panel, country_of_origin)
  brand_panel[rep, repscore:=i.total_score]
  
  ####################
  # INTERNAL METRICS #
  ####################
  
  # brand novelty
  for (novelvar in c('nov3sh','nov6sh','nov12sh')) {
      
    indexn = gsub('[^0-9]','', novelvar)
    
    brandnovel = brand_panel[, list(novelty=mean(get(novelvar))),by=c('category','country', 'brand')]
    
    # category novelty
    catnovel = brand_panel[, list(novelty=mean(get(novelvar)),
                               Nbrand=length(unique(brand)),
                               sumnovelty=sum(get(novelvar))
    ),by=c('category','country')]
    
    setkey(brandnovel, category,country,brand)
    setkey(brand_panel, category,country,brand)
    brand_panel[brandnovel, paste0('brandnovelty', indexn):=i.novelty]
    
    setkey(catnovel, category,country)
    setkey(brand_panel, category,country)
    brand_panel[catnovel, paste0('catnoveltybrand', indexn):=(i.sumnovelty-get(paste0('brandnovelty',indexn)))/(i.Nbrand-1)]
    brand_panel[catnovel, paste0('catnovelty', indexn):=(i.novelty)]
  }
  ##### FINALIZE
  
  brand_panel[, brand_capitalized:=my_capitalize(brand)]
  setorder(brand_panel, market_id, brand, date)
  fwrite(brand_panel, file = gsub('temp', 'output', fn), row.names=F)
  
}

sink('../output/datasets.txt')
cat('done prepping datasets: ', paste0(fns, collapse=', '))
sink()
