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

coef = rbindlist(lapply(all_results[!(rep_errors)], function(x) rbindlist(lapply(seq(along=x$models), function(y) cbind(model=y, rbind(x$specs), x$models[[y]]$salesresponse)))))
ur_tmp = rbindlist(lapply(all_results[!(rep_errors)], function(x) rbindlist(lapply(seq(along=x$models), function(y) cbind(model=y, rbind(x$specs), x$models[[y]]$ur)))))
ur_tmp = ur_tmp[, list(sales_ur = ur[grepl('sales', realname)]==1),by=c('category','country','brand','model')]

coef <- merge(coef, ur_tmp, by=c('category','country','brand','model'),all.x=T)

# Coefficients with and without expectations
	excp.signs = data.table(rbind(#c('Intercept', NA),
								  c('rwprice', -1),
								  c('llength', +1),
								  c('wdist', +1),
								  c('novel', +1),
								  c('radv', +1),
								  c('wunique', NA),
								  #c('laggedunitsales', +1),
								  c('c_rwprice', NA),
								  c('c_llength', NA),
								  c('c_wdist', NA),
								  c('c_novel3', NA),
								  c('c_radv', NA)
								  
								  ))
	setnames(excp.signs, c('realname', 'expected_sign'))
	excp.signs[, rank:=1:.N]
	excp.signs[, expected_sign:=as.integer(expected_sign)]

	tabl <- merge(excp.signs, coef,by=c('realname'), all.x=F,all.y=T)[order(rank)]

# "Secondary" coefficients (e.g., intercepts, trends, etc.)

pval_twotailed=.1
tabl[, sign_val := substr(formatC(expected_sign,flag='+-'),1,1)]
tabl[, ':=' (n_correct = pval<pval_twotailed*2&sign(beta)==sign(expected_sign), n_pos = ifelse((sign_val)=='N', 1, NA)&pval<pval_twotailed&sign(beta)==(+1), n_neg = ifelse((sign_val)=='N', 1, NA)&pval<pval_twotailed&sign(beta)==(-1))]

rep_tabl = tabl[, list(expected_sign = unique(sign_val),mean_coef = mean(beta), n_correct = sum(n_correct), n_pos = sum(n_pos), n_neg = sum(n_neg), vif_min=min(vif), vif_mean=mean(vif),vif_max=max(vif),N_models=.N),by=c('realname,model')]
rep_tabl[, mainvars := realname%in%excp.signs$realname]
rep_tabl[, rank:=NULL]
rep_tabl2 = tabl[, list(expected_sign = unique(sign_val),mean_coef = mean(beta), n_correct = sum(n_correct), n_pos = sum(n_pos), n_neg = sum(n_neg), vif_min=min(vif), vif_mean=mean(vif),vif_max=max(vif),N_models=.N),by=c('realname,model,sales_ur')]
rep_tabl2[, mainvars := realname%in%excp.signs$realname]
rep_tabl2[, rank:=NULL]

#' ## Coefficient summary of sales response models
#+echo=FALSE,results='asis'
for (iter in unique(rep_tabl$model)[order(unique(rep_tabl$model))]) {
cat(paste0('<b>Model ', iter, ' (', nrow(rep_tabl[model==iter]), ' variables)</b><br><br>'))
cat(paste0('<u>Main estimates</u>\n'))
print(xtable(rep_tabl[mainvars==T&model==iter,!colnames(rep_tabl)%in%c('model','mainvars'),with=F], caption = ''), type = "html", sanitize.text.function = force)
cat(paste0('<u>Estimates of remaining variables for models where DV has a unit root</u>\n'))
print(xtable(rep_tabl2[mainvars==F&sales_ur==T&model==iter,!colnames(rep_tabl2)%in%c('model','mainvars','sales_ur'),with=F], caption = ''), type = "html", sanitize.text.function = force)
cat(paste0('<u>Estimates of remaining variables for models where DV does not have a unit root</u>\n'))
print(xtable(rep_tabl2[mainvars==F&sales_ur==F&model==iter,!colnames(rep_tabl2)%in%c('model','mainvars','sales_ur'),with=F], caption = ''), type = "html", sanitize.text.function = force)

cat('\nEstimated sales response models in log-log space, with unitsales as dependent variable\n')
cat('(differenced according to UR outcomes, and\n')
cat('all other variables listed in this table as independent variables. \n')
cat('Competitor variables are indicated with the prefix c_.\n')
cat('All monetary variables are in local currencies, and corrected with each country\'s CPI.\n')
cat('The column N_models counts the estimated models.\n')
cat('For directional hypotheses, the number of significant and correct signs are reported (one-tailed, p<.1).\n')
cat('For undirectional hypotheses, and for coefficients without any expectations, the number of positive \n')
cat('and negative signs are reported (two-tailed, p<.1).\n')
cat('\n\n')

}

#+echo=FALSE


coef_sum=tabl[, list(mean_beta = mean(beta), n_correct = sum(n_correct), n_pos = sum(n_pos), n_neg=sum(n_neg), N=.N), by=c('category', 'realname', 'model')]

void<-coef_sum[, res_column := paste0('<span style=\"font-weight:bold\">', formatC(mean_beta,digits=3,format='f'), "</span><br>", ifelse(is.na(n_correct), paste0(n_pos, '&#47;', n_neg), n_correct),'<br>(', N,')',sep='')]

#' ## Estimates by category
#' significance tested at .1 (one-tailed) for directional hypotheses, and .1 (two-tailed) for undirectional hypotheses.
#'
#+echo=FALSE,results='asis'

for (iter in unique(coef_sum$model)[order(unique(coef_sum$model))]) {
	suppressWarnings({tmpX=melt(coef_sum[model==iter], id.vars=c('category','realname'))})
	tmpX=dcast(tmpX[variable=='res_column'&realname%in% c('rwprice','llength', 'wdist', 'novel', 'wunique', 'c_rwprice', 'c_llength', 'c_wdist', 'c_novel')], category~realname)
	cat(paste0('<b>Model ', iter,'</b>\n'))
	print(xtable(tmpX, align=c('l', 'l', rep('r',ncol(tmpX)-1)),
		  caption = 'Mean effect sizes by category. For directional hypotheses, number of significant coefficients (with correct sign) is given (one-tailed, p<.1). For undirectional hypotheses, number of positive and negative significant coefficients indicated (positive/negative, two-tailed). Number of estimated models in parentheses.'), type = "html", sanitize.text.function = force)
	}


#+echo=FALSE
coef_sum=tabl[, list(mean_beta = mean(beta), n_correct = sum(n_correct), n_pos = sum(n_pos), n_neg=sum(n_neg), N=.N), by=c('category', 'country', 'realname', 'model')]
void<-coef_sum[, res_column := paste0('<span style=\"font-weight:bold\">', formatC(mean_beta,digits=3,format='f'), "</span><br>", ifelse(is.na(n_correct), paste0(n_pos, '&#47;', n_neg), n_correct),'<br>(', N,')',sep='')]

#' ## Estimates by category and country (per variable)
#' significance tested at .1 (one-tailed) for directional hypotheses, and .1 (two-tailed) for undirectional hypotheses.
#'
#+echo=FALSE,results='asis'

for (iter in unique(coef_sum$model)[order(unique(coef_sum$model))]) {
	cat(paste0('<b>Model ', iter,'</b><br>\n'))
	for (.var in unique(coef_sum[model==iter]$realname)[unique(coef_sum[model==iter]$realname) %in% c('rwprice','llength', 'wdist', 'novel', 'wunique')]) {
	cat(paste0('<u>Variable: ', .var,'</u>\n'))
	suppressWarnings({tmpX=melt(coef_sum[model==iter & realname==.var], id.vars=c('category','country', 'realname', 'model'))})
	tmpX=dcast(tmpX[variable=='res_column'], category~country)
	
	print(xtable(tmpX,caption = paste0('Mean effect sizes by category and country for ', .var, '. Number of positive / negative coefficients (p<.1, one-tailed) in parantheses.')), type = "html", sanitize.text.function = force)
	}
	}
	