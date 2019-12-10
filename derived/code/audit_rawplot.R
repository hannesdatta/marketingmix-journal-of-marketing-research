# Perform audit on all files and produce plots

# Preamble
	library(data.table)
	library(lattice)
	library(latticeExtra)
	load(file='..\\temp\\categorized.RData')


##############################
# Figures of category sales  #
##############################

	# with all data (Gfk 2012, Gfk 2015)
	
		# per countries and categories
		tmp=rbindlist(lapply(datlist_final, function(x) x[, list(sales=sum(sales_units)), by=c('catname', 'country', 'source', 'date')]))[order(catname, country,source,date)]
		
		# by country
			fpath = '../audit/categorysales_all_bycountry/'
			dir.create(fpath)
			unlink(paste(fpath,'*.png'))
			
			
			for (i in unique(tmp$country)) {
				png(paste0(fpath, gsub('[/]|[+]','_', i),'.png'), res=150, units='in', height=8, width=12)
				print(xyplot(sales~date|catname, groups = source, data=tmp[country==i], type='l',
				   scales = list(y = list(relation = "free")), main=i, lty=c(2,3), ylab='sum of SALES_UNITS',
				   auto.key=list(space="bottom", columns=2, title="data source", cex.title=1, lines=T, points=F),
				   par.settings = theEconomist.theme(box = "transparent"),lattice.options = theEconomist.opts()))
				dev.off()
				}

		# by category
			fpath = '../audit/categorysales_all_bycategory/'
			dir.create(fpath)
			unlink(paste(fpath,'*.png'))
			
			for (i in unique(tmp$catname)) {
				png(paste0(fpath, gsub('[/]|[+]','_', i),'.png'), res=150, units='in', height=8, width=12)
				print(xyplot(sales~date|country, groups = source, data=tmp[catname==i], type='l',
				   scales = list(y = list(relation = "free")), main=i, lty=c(1,1), ylab='sum of SALES_UNITS',
				   auto.key=list(space="bottom", columns=2, title="data source", cex.title=1, lines=T, points=F),
				   par.settings = theEconomist.theme(box = "transparent"),lattice.options = theEconomist.opts()))
				dev.off()
				}
	

################################
# Occurence of unbranded brand #
################################

res<-lapply(1:length(datlist_final), function(i) {
	x=datlist_final[[i]]
	x[, category_new:=names(datlist_final)[i]]
	x[, list(total_sales=sum(sales_units)),by=c('category', 'brand', 'country')]
	})
res=rbindlist(res)

res[, marketshare := total_sales/sum(total_sales), by=c('category', 'country')]

# top brands
res[grepl('other|unbrand', brand, ignore.case=T)]

sink('../temp/audit_rawplot.txt')
cat(paste0('done at: ', Sys.time()))
sink()
