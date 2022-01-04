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
  
  holidays <- fread('../../../data/holidays/holidays.csv')
  
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
  
  #########################
  # PENN STATE INDICATORS #
  #########################
  
  penn_indicators <- fread('../output/penn_indicators.csv')
  setnames(penn_indicators, paste0('penn_',colnames(penn_indicators)))
  
  brand_panel <- merge(brand_panel, penn_indicators, by.x = c('year', 'country'), by.y = c('penn_year','penn_country'), all.x=T)
  
  # set 2010 values
  tmp=copy(penn_indicators[penn_year==2010])[, penn_year:=NULL]
  setnames(tmp, paste0(colnames(tmp), '2010'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('penn_country2010'), all.x=T)
  
  # set mean values
  tmp=copy(penn_indicators)[, lapply(.SD, mean, na.rm=T), by = c('penn_country'), .SDcols=setdiff(colnames(penn_indicators), c('year','penn_country'))]
  setnames(tmp, paste0(colnames(tmp), 'avg'))
  brand_panel <- merge(brand_panel, tmp, by.x = c('country'), by.y=c('penn_countryavg'), all.x=T)
  
  ########
  # GINI #
  ########
  
  gini <- fread('../output/gini.csv')
  brand_panel=merge(brand_panel, gini, by = c('country'),all.x=T)
  
  ############
  # HOFSTEDE #
  ############
  
  tmp <- fread('../../../data/hofstede/hofstede_rule_of_law.csv')
  tmp[, country:=tolower(country)]
  brand_panel=merge(brand_panel, tmp, by = c('country'),all.x=T)
  
  ####################
  # INTERNAL METRICS #
  ####################
  
  # brand novelty
  for (novelvar in c('nov3sh','nov6sh')) {
      
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
  
  doc_filename = gsub('temp', 'output', fn)
  doc_filename = gsub('.csv', '.txt', doc_filename)
  
  sink(doc_filename)
  cat('Dataset summary:\n')
  print(colnames(brand_panel))
  cat('\n\n')
  print(summary(brand_panel))
  cat('\n')
  sink()
  
}

sink('../output/datasets.txt')
cat('done prepping datasets: ', paste0(fns, collapse=', '))
sink()
