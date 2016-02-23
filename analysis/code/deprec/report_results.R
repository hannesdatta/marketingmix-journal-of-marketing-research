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


#/*
########################################################################################################################################################################
#*/


#' GfK Singapore: Analysis results
#' ==================================
#' Date of report generation: `r I(format(Sys.time(), "%d %B %Y, %H:%M (NL local time)"))`
#'

#' # Dataset overview
#' ## Number of brands per category/country
#' Selection rule: Top 7 brands per category/country, with market shares higher than 2.5%. 
#+echo=FALSE,results='asis'
tabl = rbindlist(lapply(all_results, function(x) if (!is.null(x)) return(data.table(x$specs)) else return(data.table(rbind(NA,NA,NA,NA)))))
tablX = with(tabl, table(category, country))
print(xtable(tablX, caption = 'Number of brands per category/country'), type = "html")
cat('In total ', nrow(tabl), 'brand-category-country datasets.\n')
#+include=FALSE
missings = merge(index, models[, list(.N),by=c('category','country','brand')],all.x=T,by=c('category','country','brand'))
missings = missings[is.na(N)]

#+echo=FALSE,results='asis',include=FALSE
tabl = with(missings, table(category, country))
print(xtable(tabl, caption = 'Number of missing brands per category/country'), type = "html")


#' # Unit Root tests
#' Unit roots on logged series, tested using ADF tests with deterministic trend (according to Elders), and quarterly dummies. Optional lag length selected using SBC. 
#'
#+echo=FALSE,results='asis'

for (i in n_models) {
	cat(paste0('<br><b>Model ', i, '</b>\n'))
	tabl=with(ur[model==i], table(realname,order))
	colnames(tabl) <- paste('order ', colnames(tabl))
	print(xtable(tabl,caption = paste0('Order of integration of time series; model ', i)),type='html')

	#tabl=out$models[[i]]$ur[,c('varname', 'trend' ,'ur','order','n','t','p','lags','type'),with=F]
	#print(xtable(tabl, caption = paste0('Unit root tests for model ', i, '.')), type = "html", include.rownames = T)
	}

	
	

#' # Unit Roots vs. Cointegration
#' Johansen cointegration tests with mean-centered quarterly dummies are conducted on endogenous variables that are integrated of order 1 or higher.
#' In the few cases that variables are integrated higher than 1 (e.g., 2, or 3), variables are differenced once.
#' Cointegration rank determined based on maximum Trace statistics. Optimal lag length in cointegration test is selected on the basis of SBC in a VAR model with the same variables.

#+results='asis',echo=FALSE
for (i in n_models) {
	cat(paste0('<br><b>Model ', i, '</b>\n'))
	tabl=table(models[model==i]$n_unitroot, models[model==i]$coint_rank)
	rownames(tabl) <- paste(rownames(tabl), ' unit root(s)',sep='')
	colnames(tabl) <- paste('rank ',colnames(tabl), sep='')

	print(xtable(tabl, caption = paste0('Number of unit roots vs. cointegration rank for ', nrow(models), ' brand-category-country datasets; model ', i,'.')), type = "html", include.rownames = T)
	}

	
	
	
#' Note: Analysis carried out at a significance level of p < .05.

#' # Unit root and cointegration results for all models
#+echo=FALSE,results='asis'
print(xtable(models[order(category,country,brand)]), type='html')


