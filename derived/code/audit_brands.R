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
	require(data.table)
	require(lattice)
	require(latticeExtra)

# Load externals
	source('../external/proc_rename.R')

# Lattice Theme
	mytheme=custom.theme(symbol = brewer.pal(n = 7, name = "Dark2"),
				 fill = brewer.pal(n = 7, name = "Dark2"),
				 region = brewer.pal(n = 7, name = "Dark2"),
				 reference = "#e8e8e8", lwd=2,
				 bg = "transparent", fg = "black")


##########################################################
# FIGURE: Plotting brands that have periods with 0 sales #
##########################################################
	
# Stack data in data.table
	paneldata_brands=fread('../output/datasets.csv')
	paneldata_brands=paneldata_brands[selected==T]
  paneldata_brands[, trend:=1:.N, by=c('category', 'country', 'brand')]
	paneldata_brands[, ':=' (date = as.Date(date))]
	
	
	
# Define output directories
	fpaths <- c('../audit/brands_with_zero_sales/')
	
	for (pn in fpaths) {
		dir.create(pn)
		unlink(paste0(pn,'*'))
		}

	paneldata_brands[, has_zero_sales:=any(usales==0),by=c('market_id','brand')]
	
	paneldata_brands[, brand_id:=.GRP,by=c('market_id','brand')]
	
	length(unique(paneldata_brands[has_zero_sales==T]$brand_id))
	
	
	
# Plotting function
	plotfkt <- function(fn, tmp, make_png = T) {
		if (make_png==T) png(fn, res=200, units='in', height=8, width=16)
		
		print(xyplot(value~date|variable, data=tmp,type='l', scales = list(y = list(relation = "free")),
					par.settings = mytheme,
					lattice.options = theEconomist.opts(),
					auto.key=list(space="bottom", columns=4, 
								  title="Brands", cex.title=1, lines=T, points=F),
		main = paste0(unique(tmp$category),' - ',unique(tmp$country), ' - ', unique(tmp$brand))
				))

		if (make_png==T) dev.off()
		}

###############################
# Execute plotting per MARKET #
###############################
	
	paneldata_brands[, usalessh := usales/sum(usales,na.rm=T), by=c('category', 'country', 'date')]
	paneldata_brands[, vsalessh := vsales/sum(vsales,na.rm=T), by=c('category', 'country', 'date')]

.vars=c('llen', 'nov12sh', 'wpswdst', 
		'usalessh', 'usales', 'rwpspr')

df=melt(paneldata_brands[selected==T],id.vars=c('market_id','category','country','brand','brand_id','date'),
        measure.vars=.vars)




for (i in unique(df$brand_id))	{
	cat(paste0('Plotting for brand ', i,'\n'))
	
	# Make brand-level plots
		#df$variable <- factor(df$variable, levels = .vars)
    tmpdat=df[brand_id==i]
    
    if(nrow(tmpdat)>0) {
      plotfkt(paste0(fpaths[1], i, '.png'), tmpdat, make_png = T) 
    } else {
        sink(paste0(fpaths[1], i, '.txt'))
				cat('Data not available for this brand\n')
				sink()
		}
}

sink('../temp/audit_brands.txt')
cat(paste0('done at: ', Sys.time()))
sink()
