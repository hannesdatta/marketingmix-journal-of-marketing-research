#setwd('c:\\Users\\hanne\\Dropbox\\Tilburg\\Projects\\GfK Singapore\\SVN_GfkSingapore\\derived\\code\\')
#setwd('d:\\Datta\\Dropbox\\Tilburg\\Projects\\GfK Singapore\\SVN_GfkSingapore\\derived\\code\\')
load('../temp/unzipped.RData')

require(data.table)

# Merge datasets (of countries) for each product category

	# Get each main category (i.e., every country)
	dat_types = data.table(do.call('rbind', lapply(1:(length(datlist)), function(x) cbind(index=x, category=sub("[.][^.]*","", unique(datlist[[x]]$REPORTINGPRODUCTGROUP)),country=datlist[[x]]$COUNTRY[1],
							subcategory=unique(datlist[[x]]$CATEGORY)))), key=c('index'))[order(category,country)]

	print(t(with(dat_types, table(country, category))))

							
	for (i in 1:length(datlist)) {
		datlist[[i]][, REPORTINGPRODUCTGROUP_CLEANED := sub("[.][^.]*","", REPORTINGPRODUCTGROUP)]
		}

	catsplit <- lapply(split(dat_types, dat_types$category), function(x) unique(as.numeric(x$index)))
	datlist_by_cat <- lapply(catsplit, function(x) rbindlist(datlist[x],fill=T))
	# -> datlist_by_cat contains all data for each main category


# Do an overview: which categories are availble for which time horizon
paneldata=lapply(datlist_by_cat, function(x) unique(x$PERIOD))



	
# Inspect China and desktop PCs

tmp = rbindlist(list(datlist_by_cat$'DESK COMPUT',
											 datlist_by_cat$'DESK COMPUT NZ'),fill=T)
tmp[, COUNTRY:=as.factor(COUNTRY)]
tmp2=tmp[COUNTRY=='CHINA', list(sales=sum(SALES_UNITS,na.rm=T)),by=c('PERIOD')]

xyplot(sales~PERIOD, data=tmp2,type='l')


	

	
require(lattice)
require(latticeExtra)

for (i in seq(along=datlist_by_cat)) {


	}
	
tmp = paneldata[, list(sales=sum(unitsales,na.rm=T)),by=c('country','category','date')][order(country,category,date)]

# split by categorization
cats <- list(c('camera_adv', 'camera_std', 'desktoppc', 'laptop'),
			 c('phones_mobile', 'phones_smart', 'tablets'),
			 c('washing', 'tv', 'microwave', 'dvd', 'refrigerators'))
tmp <- list(cat1=tmp[category%in%cats[[1]]],cat2=tmp[category%in%cats[[2]]],cat3=tmp[category%in%cats[[3]]])


for (i in seq(along=tmp)) {
	print(i)
	tmp[[i]][, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
	levels(tmp[[i]]$country)<-rename.fkt(levels(tmp[[i]]$country))
	levels(tmp[[i]]$category)<-rename.fkt(levels(tmp[[i]]$category))

	png(paste0('../output/salesplots_', i, '.png'), res=200, units='in', height=8, width=12)
	print(xyplot(sales~date|country, groups=category,data=tmp[[i]], auto.key=list(space="bottom", columns=2, 
						   title="Categories", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free")),
				par.settings = theEconomist.theme(box = "transparent"),
				lattice.options = theEconomist.opts()))
	dev.off()

