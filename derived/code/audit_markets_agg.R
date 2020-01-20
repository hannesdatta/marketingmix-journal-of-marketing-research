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

# Load externals
	source('../external/proc_rename.R')

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
	paneldata_agg=fread('../output/datasets_main_agg.csv')

	paneldata_agg[which(selected==T), trend:=1:.N, by=c('category', 'country')]
	paneldata_agg[, ':=' (date = as.Date(date))]
	
	
# Define output directories
	fpaths <- c('../audit/markets_aggregated/')
	
	for (pn in fpaths) {
		dir.create(pn)
		unlink(paste0(pn,'*'))
		}

# Plotting function
	plotfkt <- function(fn, tmp, make_png = T) {
		if (make_png==T) png(fn, res=200, units='in', height=8, width=16)
		
		print(xyplot(value~date|variable,data=tmp,type='l', scales = list(y = list(relation = "free")),
					par.settings = mytheme,
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
	
.vars=c('usales', 'rwpspr', 'wpswdst', 'wpsllen')

for (i in unique(paneldata_agg$market_id))	{
	cat(paste0('Plotting for market ', i,'\n'))
	
	marketname=paste0(unique(paneldata_agg[market_id==i]$category),'_',unique(paneldata_agg[market_id==i]$country),'_id_', i)
	marketname_rev=paste0(unique(paneldata_agg[market_id==i]$country),'_',unique(paneldata_agg[market_id==i]$category),'_id_', i)
	
	# Make brand-level plots
		df=suppressWarnings(melt.data.table(paneldata_agg[selected==T&market_id==i],id.vars=c('country','category','market_id', 'date'), measure.vars=.vars))
		df$variable <- factor(df$variable, levels = .vars)
		if (nrow(df)>0) { 
			plotfkt(paste0(fpaths[1], marketname, '.png'),df)
			} else {
			for (fp in fpaths[1]) {
				sink(paste0(fp, marketname, '.txt'))
				cat('Data not available for this market\n')
				sink()
				}
			}
}


sink('../temp/audit_markets_agg.txt')
cat(paste0('done at: ', Sys.time()))
sink()
