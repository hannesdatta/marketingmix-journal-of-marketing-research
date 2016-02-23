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
#' Results based on model `r I(max(n_models))`.
#' 
#' # Classification according to Dekimpe and Hanssens 1999
#' ## overall
#' Tests conducted with y = log(unit sales), and x equal to one of the remaining four time series.
#' 
#' * sales = no UR; x = no UR --> Business as usual
#' * sales = no UR; x = UR --> Escalation
#' * sales = UR; x = no UR --> Hysteresis
#' * sales = UR; x = UR --> Evolving
#'
#+echo=FALSE,results='asis'

varnames = c('llength', 'rwprice', 'wdist', 'novel')
classific <- function(y, x, integr) {
	ret=''
	if (y==0 & x == 0) ret='business as usual'
	if (y==0 & x == 1) ret='escalation'
	if (y==1 & x == 0) ret='hysteresis'
	if (y==1 & x == 1) ret='evolving'
	return(ret)
	}
	
out=lapply(varnames, function(x) {
	tmp = ur[model==max(model) &!grepl('c[_]', varname) & grepl(paste0('unitsa|',x),varname)]
	tmp = split(tmp, tmp$dat_id)
	tmp = rbindlist(tmp[which(unlist(lapply(tmp, nrow))==2)])
	
	tmp=tmp[, list(classification=classific(ur[grepl('unitsales', varname)], ur[!grepl('unitsales', varname)])), by=c('country', 'category','brand')]
	tmp[, x_variable:=x]
	return(tmp)
	})
out=rbindlist(out)

tabl=with(out,table(x_variable, classification))

tabl2=t(apply(tabl, 1, function(x) {res = paste0(formatC(100*x/sum(x),digits=0, format='f'), '% (',x,')',sep='')
									res[which(x==max(x))] <- paste0('<strong>', res[which(x==max(x))], "",sep='')
									return(res)

									}))
									
colnames(tabl2)=colnames(tabl)
print(xtable(tabl2,align=c('l', rep('r',ncol(tabl2))), caption = paste0('Classification of time series (no. of cases in parantheses, maximum in bold).' )), type = "html", sanitize.text.function = force)

#' ## by countries (all marketing mix instruments)
#+echo=FALSE,results='asis'

tabl=with(out,table(country, classification))
tabl2=t(apply(tabl, 1, function(x) {res = paste0(formatC(100*x/sum(x),digits=0, format='f'), '% (',x,')',sep='')
									res[which(x==max(x))] <- paste0('<strong>', res[which(x==max(x))], "",sep='')
									return(res)

									}))
colnames(tabl2)=colnames(tabl)

print(xtable(tabl2,align=c('l', rep('r',ncol(tabl2))), caption = paste0('Classification of time series by country with all marketing mix instruments (no. of cases in parantheses, maximum in bold).' )), type = "html", sanitize.text.function = force)


#' ## by countries and variables
#+echo=FALSE,results='asis'

for (i in unique(out$x_variable)) {
tabl=with(out[which(x_variable==i)],table(country, classification))
tabl2=t(apply(tabl, 1, function(x) {res = paste0(formatC(100*x/sum(x),digits=0, format='f'), '% (',x,')',sep='')
									res[which(x==max(x))] <- paste0('<strong>', res[which(x==max(x))], "",sep='')
									return(res)

									}))
colnames(tabl2)=colnames(tabl)
cat(paste0('<b>Variable: ', i, '</b>\n'))
print(xtable(tabl2, align=c('l', rep('r',ncol(tabl2))), caption = paste0('Classification of time series by country with x variable = ',i,' (no. of cases in parantheses, maximum in bold).' )), type = "html", sanitize.text.function = force)
}

#/*print(xtable(test,align=rep('r',ncol(test)+1),digits=0),type='html')*/

#' # Unit roots vs. countries
#+echo=FALSE
histogram(~as.factor(n_unitroot)|country, xlab='Number of unit roots in all endogenous and exogenous time series per brand/country/category',data=models,main='Number of unit roots in all data sets, per country')


