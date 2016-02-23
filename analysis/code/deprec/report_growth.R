#/* 
#    _ __    ___   _ __     ___    _ __  | |_          
#   | '__|  / _ \ | '_ \   / _ \  | '__| | __|         
#   | |    |  __/ | |_) | | (_) | | |    | |_          
#   |_|     \___| | .__/   \___/  |_|     \__|         
#                 | |                                  
#                 |_|                                  
# 

#*/



#+ setup, include=FALSE
library(knitr)
require(data.table)
require(reshape2)
require(lucid)
require(xtable)


#require(lattice)
#source(paste(Sys.getenv('uvt_setup_spotify'), 'setup.R', sep=''))
#opts_chunk$set(fig.path='figure/silk-', fig.width=12, fig.height=6, dpi = 200)

prettify <- function(x, ...) {
	formatC(x, big.mark = ",", drop0trailing = T, ...)
	}
	
cacheoption = FALSE

#growthstat = paneldata[selected==T, list(sales=sum(unitsales,na.rm=T)),by=c('category', 'country', 'date')][order(category,country,date)]


growth = paneldata[selected==T,list(sales_first = sum(unitsales[1:12]), sales_last = sum(rev(unitsales)[1:12])),by=c('category','country', 'brand')]
growth[, ':=' (perc_change = (sales_last-sales_first)/sales_first)]
growth_sel = growth[perc_change<quantile(perc_change,na.rm=T,.99)]

#hist(growth_sel$perc_change,breaks=100)


#' # Descriptive stats
summary(growth$perc_change)

#' # Growth by category
#+echo=FALSE
histogram(~perc_change|category,data=growth_sel, xlab = paste0('Perc. change in unit sales (first year vs. last year); 1 = 100%.\nN = ', nrow(growth_sel), ' brands, excluding brands > 99% percentile.'), main = 'Growth by category',breaks=20)
print(growth_sel[, list(mean_growth = mean(perc_change,na.rm=T), N=.N),by=c('category')][order(mean_growth,decreasing=T)])

#' # Growth by country
#+echo=FALSE
histogram(~perc_change|country,data=growth_sel, xlab = paste0('Perc. change in unit sales (first year vs. last year); 1 = 100%.\nN = ', nrow(growth_sel), ' brands, excluding brands > 99% percentile.'), main = 'Growth by category',breaks=20)
print(growth_sel[, list(mean_growth = mean(perc_change,na.rm=T), N=.N),by=c('country')][order(mean_growth,decreasing=T)])
