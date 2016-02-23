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


#coef = rbindlist(lapply(all_results[!(rep_errors)], function(x) cbind(x$salesresponse, rbind(x$specs))))

coef = rbindlist(lapply(all_results[!(rep_errors)], function(x) rbindlist(lapply(seq(along=x$models), function(y) cbind(model=y, rbind(x$specs), x$models[[y]]$salesresponse)))))


	testcoef <- c('rwprice','llength','wdist','novel', 'radv')
	flag1 = coef[, list(insign=length(which(pval[realname%in%testcoef]>.10))/length(which(!is.na(pval[realname%in%testcoef])))),by=c('dat_id','model','category','country','brand')]
	# merge with number of observations
	obs = paneldata[, list(N=length(which(selected==T))),by=c('country','category','brand')]

	flag1 <- merge(flag1, obs, by=c('category','country','brand'))
	tmp<-flag1[insign>.5][order(insign,decreasing=T)]
	#tmp[, link:='<a href=http://google.com>Link</a>']
	tmp[, fn := paste('res_', apply(cbind(category,country,brand),1, paste, collapse='_'), '.html', sep='')]
	tmp[, link:=paste0('<a href=\"', fn,'\">', 'Link','</a>')]
	tmp[, fn:=NULL]
	tabl1 = tmp



# VIFs higher than 10
	flag2 = coef[, list(VIF=any(vif>10),meanvif=mean(vif,na.rm=T)),by=c('model', 'dat_id','category','country','brand')]
	flag2 <- merge(flag2, obs, by=c('category','country','brand'))
	tmp<-flag2[VIF==T][order(meanvif,decreasing=T)]
	tmp[, fn := paste('res_', apply(cbind(category,country,brand),1, paste, collapse='_'), '.html', sep='')]
	tmp[, link:=paste0('<a href=\"', fn,'\">', 'Link','</a>')]
	tmp[, fn:=NULL]
	tabl2=tmp
	
#	tmp=merge(coef,tmp,by=c('id','category','country','brand'))
	
#coef[vif>10, list(N=.N),by=c('realname')]
	
		
	
# Extreme coefficients
	flag3 = coef[, list(extreme=any(abs(beta[!var_model%in%c('(Intercept)')])>4),meanbeta=mean(abs(beta),na.rm=T)),by=c('dat_id','model','category','country','brand')]
	flag3 <- merge(flag3, obs, by=c('category','country','brand'))
	tmp<-flag3[extreme==T][order(meanbeta,decreasing=T)]
	nrow(tmp) # --> unproblematic
	tmp[, fn := paste('res_', apply(cbind(category,country,brand),1, paste, collapse='_'), '.html', sep='')]
	tmp[, link:=paste0('<a href=\"', fn,'\">', 'Link','</a>')]
	tmp[, fn:=NULL]
	tabl3=tmp


#/*
########################################################################################################################################################################
#*/


#' GfK Singapore: Problematic cases (sales response models; preliminary step)
#' ==================================
#' Date of report generation: `r I(format(Sys.time(), "%d %B %Y, %H:%M (NL local time)"))`
#'


	
	
#' # Problematic cases
#' 
#' Summary of problematic cases for `r I(length(unique(tabl1$model)))` model cases (see estimates
#' for sales response models for how these models were estimated.
#'

#' ## Extreme VIFs
#' Selection rule: All models with at least one coefficient that has a VIF exceeding 10.
#' I am reporting mean VIFs across all coefficients as an additional check.
#+echo=FALSE,results='asis'
printtabl=tabl2

for (iter in unique(printtabl$model)[order(unique(printtabl$model))]) {
cat(paste0('<b>Model ', iter, ' (', nrow(printtabl[model==iter]), ' cases)</b>\n'))
print(xtable(printtabl[model==iter], caption = ''), type = "html", sanitize.text.function = force)
}


#' ## Number of insignificant coefficients
#' Selection rule: All models with at least 50% insignificant coefficients in the main explanatory variables (p<.05, one-tailed).
#' Insign. is the share of insignificant coefficients (1=100%), for a brand's own marketing mix: rwprice, llength, wdist, novel, radv.
#+echo=FALSE,results='asis'
printtabl=tabl1

for (iter in unique(printtabl$model)[order(unique(printtabl$model))]) {
cat(paste0('<b>Model ', iter, ' (', nrow(printtabl[model==iter]), ' cases)</b>\n'))
print(xtable(printtabl[model==iter], caption = ''), type = "html", sanitize.text.function = force)
}

#' ## "Weird" coefficients
#' Selection rule: All models with any coefficient exceeding an absolute value of 4.
#+echo=FALSE,results='asis'
printtabl=tabl3

for (iter in unique(printtabl$model)[order(unique(printtabl$model))]) {
cat(paste0('<b>Model ', iter, ' (', nrow(printtabl[model==iter]), ' cases)</b>\n'))
print(xtable(printtabl[model==iter], caption = ''), type = "html", sanitize.text.function = force)
}
