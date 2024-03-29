# GfK Singapore Project
#
#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|
#
#

# Load data sets
	library(data.table)
	library(lattice)
	library(latticeExtra)
	library(RColorBrewer)

# Lattice Theme
	mytheme=custom.theme(symbol = brewer.pal(n = 7, name = "Dark2"),
				 fill = brewer.pal(n = 7, name = "Dark2"),
				 region = brewer.pal(n = 7, name = "Dark2"),
				 reference = "#e8e8e8", lwd=2,
				 bg = "transparent", fg = "black")


#######################################################
# FIGURE: Plotting variables for all markets by brand #
#######################################################

# Stack data in data.table
	paneldata_brands=fread('../output/datasets_main.csv')

	paneldata_brands[which(selected==T), trend:=1:.N, by=c('category', 'country', 'brand')]
	paneldata_brands[, ':=' (date = as.Date(date))]
	
	
# Define output directories
	fpaths <- c('../audit/markets_brands_by_cat/', # Plots sorted by category	
			   '../audit/markets_brands_by_country/', # Plots sorted by country
			   '../audit/markets_categorysales/') # Category-level plots
	
	for (pn in fpaths) {
		dir.create(pn)
		unlink(paste0(pn,'*'))
		}

# Plotting function
	plotfkt <- function(fn, tmp, make_png = T) {
		if (make_png==T) png(fn, res=200, units='in', height=8, width=16)
		
		print(xyplot(value~date|variable,groups=brand,data=tmp[variable%in%.vars],type='l', scales = list(y = list(relation = "free")),
					par.settings = mytheme,#theEconomist.theme(box = "transparent"),
					lattice.options = theEconomist.opts(),
					auto.key=list(space="bottom", columns=4, 
								  title="Brands", cex.title=1, lines=T, points=F),
		main = paste0(unique(tmp$category),': ',unique(tmp$country))
				))

		if (make_png==T) dev.off()
		}

###############################
# Execute plotting per MARKET #
###############################
	
	paneldata_brands[, nov12sh:=(nov12/llen)*100]
	paneldata_brands[, nov6sh:=(nov6/llen)*100]
	
	paneldata_brands[, usalessh := usales/sum(usales,na.rm=T), by=c('category', 'country', 'date')]
	paneldata_brands[, vsalessh := vsales/sum(vsales,na.rm=T), by=c('category', 'country', 'date')]

.vars=c('llen', 'nov12sh', 'nov6sh', 'wpswdst', 'wpsun',
		'usalessh', 'usales', 'rwpspr')

for (i in unique(paneldata_brands$market_id))	{
	cat(paste0('Plotting for market ', i,'\n'))
	
	marketname=paste0(unique(paneldata_brands[market_id==i]$category),'_',unique(paneldata_brands[market_id==i]$country),'_id_', i)
	marketname_rev=paste0(unique(paneldata_brands[market_id==i]$country),'_',unique(paneldata_brands[market_id==i]$category),'_id_', i)
	
	# Make brand-level plots
		df=suppressWarnings(melt(paneldata_brands[selected==T&market_id==i],id.vars=c('country','brand','date','category','market_id')))
		df$variable <- factor(df$variable, levels = .vars)
		if (nrow(df)>0) { 
			plotfkt(paste0(fpaths[1], marketname, '.png'),df)
			file.copy(paste0(fpaths[1], marketname, '.png'),paste0(fpaths[2], marketname_rev, '.png'))
			} else {
			for (fp in fpaths[1:2]) {
				sink(paste0(fp, marketname_rev, '.txt'))
				cat('Data not available for this market\n')
				sink()
				}
			}
}

sink('../temp/audit_markets.txt')
cat(paste0('done at: ', Sys.time()))
sink()
