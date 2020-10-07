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

# relabel countries
gci[, country :=tolower(country)]

gci[country=='hong kong sar', country :='hong kong']
gci[country=='korea, rep', country :='south korea']
gci[country=='taiwan, china', country :='taiwan']

# label contructs
gci[, measure_label:=paste0('gci_', measure)]

## derive 1: list of countries X metrics
# metrics:
#
#00.03
#02.01-02, 04-05, 07-09
#03.03
#04.09-10
#05.01, 02, 03
#06.01, 02, 09, 14, 15
#07.07
#08.01, 02
#10.01
#11.01, 02, 04, 05, 08
#12.01

gci[, selected:=grepl(paste0('00.03|02.01|02.02|02.04|02.05|02.07|02.08|02.09|',
                                                '03.03|04.09|05.01|05.02|05.03|06.01|06.02|06.09|06.14|06.15|',
                                                '07.07|08.01|08.02|10.01|11.01|11.02|11.04|11.05|11.08|12.01'), measure)]


gci_pca=dcast(gci[selected==T], country~measure_label, value.var='score')

fwrite(gci_pca,'../output/gci_pca.csv',row.names=F)

# derive 2: data set only with countries that are actually being used in analysis

# keep focal countries
countries <- tolower(c('Australia', 'Singapore', 'Japan', 'New Zealand', 'Hong Kong', 'South Korea',
               'Malaysia', 'Thailand', 
               'Taiwan',
               'China',
               'Indonesia',
               'Philippines', 'India', 'Vietnam'))

table(gci[country%in%countries]$country)
length(table(gci[country%in%countries]$country))

# save
tmp=melt(gci[country%in%countries], id.vars=c('country', 'measure', 'measure_label', 'selected'))
tmp[, type:=ifelse(variable=='rank', 'r', 's')]

gci_save=dcast(tmp, country~measure_label+type, value.var='value')

fwrite(gci_save,'../output/gci.csv')

