# load global competitiveness report data
library(data.table)
library(stringr)

gci <- fread('../../../../Data/gci/infrastructure.tsv',header=T)
gci[, rank:=1:.N]
gci=melt(gci, id.vars='rank')

gci[, actual_rank:=as.numeric(unlist(lapply(strsplit(value, ' ',fixed=T), function(x) x[1])))]
gci[, score:=str_extract(substr(value,4,100), "\\d+\\.*\\d*")]
gci[, country:=str_trim(gsub('[0-9]|[.]', '', value))]

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
gci[, construct:='']
gci[variable=='2.01', construct:='overall_infra']
gci[variable=='2.02', construct:='roadqual']
gci[variable=='2.03', construct:='railroadqual']
gci[variable=='2.04', construct:='portqual']
gci[variable=='2.05', construct:='airqual']
gci[variable=='2.06', construct:='airseatkm']
gci[variable=='2.07', construct:='electricityqual']
gci[variable=='2.08', construct:='fixedphone']
gci[variable=='2.09', construct:='mobilephone']

# relabel countries
gci[, country :=tolower(country)]
gci[country=='hong kong sar', country :='hong kong']
gci[country=='korea, rep', country :='south korea']
gci[country=='taiwan, china', country :='taiwan']

# drop unnecessary variables
gci[, ':=' (rank=NULL, value=NULL)]

gci_save=dcast(gci, country~construct, value.var='score')

fwrite(gci_save,'../temp/gci.csv')

