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
source('../../derived/external/proc_rename.R')

# Lattice Theme
mytheme=custom.theme(symbol = brewer.pal(n = 7, name = "Dark2"),
                     fill = brewer.pal(n = 7, name = "Dark2"),
                     region = brewer.pal(n = 7, name = "Dark2"),
                     reference = "#e8e8e8", lwd=2,
                     bg = "transparent", fg = "black")


# Stack data in data.table
paneldata_brands=fread('../../derived/output/datasets.csv')
paneldata_brands=paneldata_brands[selected==T]
paneldata_brands[, trend:=1:.N, by=c('category', 'country', 'brand')]
paneldata_brands[, ':=' (date = as.Date(date))]

##########################################################
# FIGURE: Plotting brands that have periods with 0 sales #
##########################################################

# Load results

elast <- fread('../externals/elast_results_nov12sh.csv') #[globalbrand==T]

# Identify extreme markets

.vars=unique(elast$variable)

setorder(elast, variable, elastlt)
elast[!is.na(elastlt), rankelast:=1:.N,by=c('variable')]
elast[!is.na(elastlt), rankelastrev:=.N:1,by=c('variable')]

filter=elast[rankelast%in%1:3|rankelastrev%in%1:3]


# Define output directories
fpaths <- c('../audit/extrem_elast_plots/')

for (pn in fpaths) {
  dir.create(pn)
  unlink(paste0(pn,'*'))
}



# Plotting function
plotfkt <- function(fn, tmp, make_png = T, main = NULL) {
  if (make_png==T) png(fn, res=150, units='in', height=6, width=12)
  
  if (is.null(main)) main = paste0(unique(tmp$category),' - ',unique(tmp$country), ' - ', unique(tmp$brand))
  print(xyplot(value~date|variable, data=tmp,type='l', scales = list(y = list(relation = "free")),
               par.settings = mytheme,
               lattice.options = theEconomist.opts(),
               auto.key=list(space="bottom", columns=4, 
                             title="Brands", cex.title=1, lines=T, points=F),
               main = main
  ))
  
  if (make_png==T) dev.off()
}

paneldata_brands[, usalessh := usales/sum(usales,na.rm=T), by=c('category', 'country', 'date')]
paneldata_brands[, vsalessh := vsales/sum(vsales,na.rm=T), by=c('category', 'country', 'date')]

.vars=c('llen', 'nov12sh', 'wpswdst', 
        'usalessh', 'usales', 'rwpspr')

df=melt(paneldata_brands[selected==T],id.vars=c('market_id','category','country','brand','date'),
        measure.vars=.vars)


for (i in 1:nrow(filter)) {
  tmpdat=df[market_id==filter[i]$market_id&brand==filter[i]$brand]

  # Make brand-level plots
  #df$variable <- factor(df$variable, levels = .vars)
  
  if(nrow(tmpdat)>0) {
    lbl = paste0(unique(tmpdat$category),' - ',unique(tmpdat$country), ' - ', unique(tmpdat$brand),'--', 'variable ', filter[i]$variable, ', elast is ', round(filter[i]$elastlt,3))
    plotfkt(paste0(fpaths[1], lbl, '_', i, '.png'), tmpdat, make_png = T, main = gsub('[--]', '\n', lbl))
  } else {
    sink(paste0(fpaths[1], lbl, '_', i, '.txt'))
    cat('Data not available for this brand\n')
    sink()
  }
}

  
  
}

for (var in .vars) {
  
  
  }



###############################
# Execute plotting per MARKET #
###############################




for (i in unique(df$brand_id))	{
  cat(paste0('Plotting for brand ', i,'\n'))
  

sink('../temp/audit_brands.txt')
cat(paste0('done at: ', Sys.time()))
sink()
