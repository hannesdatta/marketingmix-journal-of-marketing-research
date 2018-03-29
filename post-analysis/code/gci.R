# load global competitiveness report data
library(data.table)
library(stringr)

gci <- fread('../../../../Data/gci/gci_data.tsv',header=T)
gci[, row:=1:.N]
gci=melt(gci, id.vars='row')

gci[, rank:=as.numeric(unlist(lapply(strsplit(value, ' ',fixed=T), function(x) x[1])))]

gci[, score:=gsub('[0-9][.][.]','', value)]
gci[, score:=str_extract(substr(gsub('[,]', '', score),4,100), "\\d+\\.*\\d*")]

# clean countries
gci[, country:=sapply(value, function(x) strsplit(x, '[.]')[[1]][1])]
gci[, country:=gsub('n/a', '' , country,fixed=T)]
gci[, country:=str_trim(gsub('[0-9]', '', country))]
gci <- gci[!grepl('RANK COUNTRY', country)]


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
gci[, construct:=paste0('gci', variable)]

# relabel countries
gci[, country :=tolower(country)]
gci[country=='hong kong sar', country :='hong kong']
gci[country=='korea, rep', country :='south korea']
gci[country=='taiwan, china', country :='taiwan']

# drop unnecessary variables
gci[, ':=' (row=NULL, value=NULL, variable=NULL)]
gci[, scorenum := as.numeric(score)]

gci_save=dcast(gci, country~construct, value.var='scorenum')

fwrite(gci_save,'../temp/gci.csv')

