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

# 
	
	adv <- fread('../temp/advertising.csv')

	
	
	
# Define output directories
	fpaths <- c('../audit/advertising/')
	
	for (pn in fpaths) {
		dir.create(pn)
		unlink(paste0(pn,'*'))
		}

adv[, brand_id:=.GRP,by=c('category','country','brand')]
adv[, market_uid := .GRP, by = c('category','country')]
adv[, date:=as.Date(date)]
setorder(adv, market_uid,brand,date)
# Plotting function
	plotfkt <- function(fn, tmp, make_png = T) {
		if (make_png==T) png(fn, res=150, units='in', height=6, width=12)
		
		print(xyplot(adspent~date|brand, data=tmp, type=c('b'), scales = list(y = list(relation = "free")),
					par.settings = mytheme,
					lattice.options = theEconomist.opts(), main = paste0(unique(tmp$category),' - ',unique(tmp$country))))
		
		if (make_png==T) dev.off()
		}

for (i in unique(adv$market_uid)) {
  cat(paste0('Plotting for market ', i,'\n'))
  plotfkt(paste0(fpaths, unique(adv[market_uid==i]$country),'_', unique(adv[market_uid==i]$category), '.png'),
          tmp=adv[market_uid==i])
}
	
sink('../temp/audit_advertising.txt')
cat(paste0('done at: ', Sys.time()))
sink()
