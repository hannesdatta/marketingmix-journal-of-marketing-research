#/* 
#                   _                             _                             _                                  _ 
#                  | |                           | |                           | |                                | |
#     ___    __ _  | |_   ______    ___   _ __   | |_   _ __   _   _   ______  | |__    _ __    __ _   _ __     __| |
#    / __|  / _` | | __| |______|  / __| | '_ \  | __| | '__| | | | | |______| | '_ \  | '__|  / _` | | '_ \   / _` |
#   | (__  | (_| | | |_           | (__  | | | | | |_  | |    | |_| |          | |_) | | |    | (_| | | | | | | (_| |
#    \___|  \__,_|  \__|           \___| |_| |_|  \__| |_|     \__, |          |_.__/  |_|     \__,_| |_| |_|  \__,_|
#                                _   _                          __/ |                                                
#                               | | | |                        |___/                                                 
#    _ __    ___   ___   _   _  | | | |_   ___                                                                       
#   | '__|  / _ \ / __| | | | | | | | __| / __|                                                                      
#   | |    |  __/ \__ \ | |_| | | | | |_  \__ \                                                                      
#   |_|     \___| |___/  \__,_| |_|  \__| |___/                                                                      
#                                                                                                                    
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


options(width=1000)


compplot <- function(date,focal, comp_focal,selection=NULL, main=NULL, label=NULL,ylim=NULL,printlegend=F,sub=NULL) {
		if (is.null(ylim)) .ylim=c(min(focal,comp_focal,na.rm=T),max(focal,comp_focal,na.rm=T)) else .ylim=ylim
		plot(date, focal, main = main,type='l',font.lab=2,lwd=2,ylim=.ylim,ylab=label,sub=sub)
		lines(date, comp_focal,lty=2)
		
		if (!is.null(selection)) {
			abline(v=min(date[selection==T]))
			abline(v=max(date[selection==T]))
			}
		if (printlegend==T) legend('topleft', c('focal brand', 'competitors'),lty=c(1,2), lwd=c(2,1),bty='n', cex=.75)
		}
		
target_model=length(out$models)
#/*
########################################################################################################################################################################
#*/


#' Results: `r I(paste(out$specs[1:3],collapse= ' - '))` 
#' ==================================
#' Date of report generation: `r I(format(Sys.time(), "%d %B %Y, %H:%M (NL local time)"))`
#'

#' Data set
#' * ID number: `r I(unique(out$specs["dat_id"]))`
#' * Sample size (periods): `r I(length(which(panel$selected==T)))`
#'

#' Model specification:
#' * Endogenous (`r I(length(out$models[[target_model]]$variables$endog))`): `r I(paste(out$models[[target_model]]$variables$endog,collapse=', '))`
#' * Exogenous (`r I(length(out$models[[target_model]]$variables$exog))`): `r I(paste(out$models[[target_model]]$variables$exog,collapse=', '))`
#' * Quarterly dummies (centred): `r I(ifelse(out$models[[target_model]]$seasonality==T, 'yes','no'))`
#' * Deterministic trend: `r I(ifelse(out$models[[target_model]]$trend==T, 'yes','no'))`
#' * Model type: `r I(ifelse(out$models[[target_model]]$coint_rank>0, 'VEC','VARX'))`
#'

#' ## Plots of the data
#' Plotted for all (own and competitor) variables included in the estimates models. The vertical bars indicate the estimation sample across the complete set of variables.

#+ echo = FALSE, fig.width=7, fig.height=12,dpi=100
par(mar=c(5, 4, 3, 2))#, xaxs='i', yaxs='i')
plotvars = unique(unlist(lapply(out$models, function(x) if(class(x)=='try-error') return(NULL) else return(x$variables$endog))))
plotcompvars = unique(unlist(lapply(out$models, function(x) if(class(x)=='try-error') return(NULL) else return(x$variables$exog))))

par(mfrow=c(length(plotvars),1))
	
for (.var in plotvars) {
	.var=gsub('I|(makediff)|(as[.]numeric)|[(]|[)]|log|([+]1)|([*]100)','', .var)
	
	tmp = eval(parse(text=paste0('with(panel, ', .var,')')))
	# cleanname
	.cleanname=gsub('[*]100','', gsub('[)]', '', gsub('[+]1', '', gsub('log[(]', '',.var))))
	.found = grepl(paste0('c_',.cleanname), plotcompvars)
	if (length(which(.found))>0) {
	.cleanname=gsub('I|(makediff)|(as[.]numeric)|[(]|[)]|log|([+]1)|([*]100)','', plotcompvars[.found])
		tmp_comp = eval(parse(text=paste0('with(panel, ', .cleanname,')')))
		} else {
		tmp_comp = NULL
		}
	compplot(panel$date,tmp,tmp_comp,panel$selected, label=.var,printlegend=T)
	}

#+include=FALSE
tmp = models[category==out$specs$category & country==out$specs$country & !brand==out$specs$brand][,c('category','country','brand'),with=F]
setkey(tmp, 'category', 'country', 'brand')
tmp<-unique(tmp)

tmp[, link:= paste0('<a href=\"', paste0('res_', paste(c(category,country,brand),collapse='_'),'.html','\">Link</a><br>\n')), by=c('category','country','brand')]
#' ## Links to competitors
#+results='asis',echo=FALSE
print(xtable(tmp, caption = ''), type = "html", sanitize.text.function = force)

	
#' ## Unit Root tests
#+results='asis',echo=FALSE
for (i in seq(along=out$models)) {
	cat(paste0('<br><b>Model ', i, '</b>\n'))
	tabl=out$models[[i]]$ur[,c('varname', 'trend' ,'ur','order','n','t','p','lags','type'),with=F]
	print(xtable(tabl, caption = paste0('Unit root tests for model ', i, '.')), type = "html", include.rownames = T)
	}

#' ## Cointegration
#' Johansen cointegration test conducted on UR(1) endogenous variables (using Osterwald-Lenum critical values).
#'
#+results='asis',echo=FALSE
for (i in seq(along=out$models)) {
	cat(paste0('<br><b>Model ', i, '</b>\n'))
	tabl=out$models[[i]]$coint_result
	print(xtable(tabl, caption = paste0('Johansen Cointegration Test for model ', i, '.')), type = "html", include.rownames = T)
	cat(paste0('Result: Cointegration rank = ', out$models[[i]]$coint_rank,'.<br>\n',sep=''))
	cat(paste0('Endogenous UR(1) variables: ', paste(out$models[[i]]$coint_variables,collapse=', '),'<br>\n'))
	}
	

#' ## Sales response model(s)
#+results='asis',echo=FALSE,out.width=1000
for (i in seq(along=out$models)) {
	cat(paste0('<br><b>Model ', i, '</b>\n'))
	tabl=out$models[[i]]$salesresponse
	tabl<-tabl[,!colnames(tabl)%in%c('depvar'),with=F]
	print(xtable(tabl, caption = paste0('Results from a one-equation sales response model (in log-log space), with dependent variable ', unique(out[i]$salesresponse$depvar),'.')), type = "html", include.rownames = T)
	}

	
#' ## Data set
#+results='asis',echo=FALSE,out.width=1000,include=TRUE
tabl=panel
#.category = out$
#tmp_skutable=all_data[[i]]$sku_info[country==unique(tmp_dat$country)]
suppressWarnings({print(xtable(tabl, caption = paste0('Complete dataset used for model estimation')), type = "html", include.rownames = T)})
			
			
#for (i in unique(out$salesresponse$model)) {
#	tabl=out$salesresponse[model==i]
	#tabl<-tabl[,!colnames(tabl)%in%c('model'),with=F]
#	print(xtable(tabl, caption = paste0('Results from a one-equation sales response model (in log-log space)')), type = "html", include.rownames = T)
#	}


#/*
########################################################################################################################################################################

#' ## Derivation of response elasticities
#' Derived using generalized impulse response functions.
#' If unitsales are specified in levels, accumulated responses are shown.

#+results='asis',echo=FALSE
tabl=data.table(out$irf)
periods=c(1,3,6,12,24,36)
tabl=t(tabl[periods])
colnames(tabl) <- paste('period ', periods,sep='')
rownames(tabl) <- gsub('I|(makediff)|(as[.]numeric)|[(]|[)]|log|([+]1)|([*]100)','', rownames(tabl))
tabl=tabl[-1,]

print(xtable(tabl, caption = paste0('Response elasticities on unitsales')), type = "html", include.rownames = T)


#' ## VAR/VEC model
#+,echo=FALSE,out.width=1000
print(summary(out$varvec))
#*/