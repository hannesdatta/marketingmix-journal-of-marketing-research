# load global competitiveness report data
library(data.table)
library(stringr)

# Load and clean individual items of GCI data
items <- fread('../../../../Data/gci/gci_items.tsv',header=T)
items[, row:=1:.N]
items=melt(items, id.vars='row')

suppressWarnings({items[, rank:=as.numeric(unlist(lapply(strsplit(value, ' ',fixed=T), function(x) x[1])))]})

items[, score:=gsub('[0-9][.][.]','', value)]
items[, score:=str_extract(substr(gsub('[,]', '', score),4,100), "\\d+\\.*\\d*")]
items[, score := as.numeric(score)]

items[, country:=sapply(value, function(x) strsplit(x, '[.]')[[1]][1])]
items[, country:=gsub('n/a', '' , country,fixed=T)]
items[, country:=str_trim(gsub('[0-9]', '', country))]
items <- items[!grepl('RANK COUNTRY', country)]

items = items[, c('country', 'variable', 'rank', 'score'),with=F]
setnames(items, 'variable', 'measure')

# Load and append combined pillars ("overall"), and subdimensions ("subdimensions") of GCI data
dimensions=rbindlist(lapply(c(overall='gci_overall.tsv',subdimensions='gci_subdimensions.tsv'), function(fn) { 
  tmp <- fread(paste0('../../../../Data/gci/', fn), header=T)
  suppressWarnings({tmp=melt(tmp, id.var=c('country'))})
  tmp[, type_var:=ifelse(grepl('[_]r', variable),'rank', 'score')]
  tmp[, measure:=gsub('[_]s|[_]r','', variable)]
  tmp[, country:=gsub('[.]', '', str_trim(country))]
  return(dcast(tmp, country+measure~type_var, value.var='value'))
  }))

gci = rbindlist(list(items, dimensions))

# keep focal countries
countries <- c('Australia', 'Singapore', 'Japan', 'New Zealand', 'Hong Kong SAR', 'Korea, Rep',
               'Malaysia', 'Thailand', 
               'Taiwan, China',
               'China',
               'Indonesia',
               'Philippines', 'India', 'Vietnam')

table(gci[country%in%countries]$country)
length(table(gci[country%in%countries]$country))

gci <- gci[country%in%countries]

# label contructs
gci[, measure_label:=paste0('gci_', measure)]

# relabel countries
gci[, country :=tolower(country)]
gci[country=='hong kong sar', country :='hong kong']
gci[country=='korea, rep', country :='south korea']
gci[country=='taiwan, china', country :='taiwan']

# save
tmp=melt(gci, id.vars=c('country', 'measure', 'measure_label'))
tmp[, type:=ifelse(variable=='rank', 'r', 's')]

gci_save=dcast(tmp, country~measure_label+type, value.var='value')

fwrite(gci_save,'../temp/gci.csv')

