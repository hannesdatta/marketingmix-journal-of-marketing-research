#     _____    __   _  __                               _                 _       
#    / ____|  / _| | |/ /       /\                     | |               (_)      
#   | |  __  | |_  | ' /       /  \     _ __     __ _  | |  _   _   ___   _   ___ 
#   | | |_ | |  _| |  <       / /\ \   | '_ \   / _` | | | | | | | / __| | | / __|
#   | |__| | | |   | . \     / ____ \  | | | | | (_| | | | | |_| | \__ \ | | \__ \
#    \_____| |_|   |_|\_\   /_/    \_\ |_| |_|  \__,_| |_|  \__, | |___/ |_| |___/
#                                                            __/ |                
#                                                           |___/                 
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


# Load data sets and results
source('load.R')
require(knitr)


###############################
#                             #
#                             #
#  M O D E L   R E S U L T S  #
#                             #
#                             #
###############################

signstars <- function(zscore) { # converts a z-score into a signifance asteriks
	  if (length(zscore)==0) return("   ")
	  if (is.nan(zscore) | !is.numeric(zscore) | is.na(zscore)) return("   ")
	  ret <- "ns."
	  #if (abs(zscore)>qnorm(1-(0.1))) ret <- c(paste("  ", rawToChar(as.raw(134)), sep=''))
	  if (abs(zscore)>qnorm(1-(0.1/2))) ret <- c("  .")
	  if (abs(zscore)>qnorm(1-(0.05/2))) ret <- c("  *")
	  if (abs(zscore)>qnorm(1-(0.01/2))) ret <- c(" **")
	  if (abs(zscore)>qnorm(1-(0.001/2))) ret <- c("***")
	  return(ret)
	  }
	  
if(0){
# BUILD LATEX REPORTS FOR EACH SUBELEMENT OF RESULTS
for (r in 1) {# seq(along=results)) {
	brand_results <<- results_brands[[r]]
	model <<- models[[r]]
	brands_check <<- checks_brands[[r]]
	
	model$descr <- gsub('[%]', 'perc. ', model$descr)
	
	# Run report
	savewd = getwd()
	setwd('..//temp')
	
	knit("..//code//template.Rnw", output = 'results.tex')
	
	
	shell(paste0('pdflatex results.tex -job-name=', paste0('results_', r, ifelse(!is.null(models[[r]]$fn), paste0('_', models[[r]]$fn), '')), ' -output-directory=..//output'))
	
	setwd(savewd)
	}

	}


##################################################
##################################################
#                                                #
#   R E P O R T I N G   T H E    R E S U L T S   #
#                                                #
##################################################
##################################################
	
	
	
###### CODE TO PRODUCE OUTPUT TABLES ######

#### DATA COVERAGE
	tmp = brand_panel[selected==T, list(nbrands = length(unique(brand)),
							 obs = length(unique(date)))
							 , by = c('country', 'category')]
							 
							  rename.fkt(colnames(df))
    #tmp[, renamed_category := rename.fkt(category)]
	#tmp[, renamed_country := rename.fkt(country)]
	
	#setorder(tmp, renamed_country, renamed_category)
	setorder(tmp, country, category)
	
	
	convert_txt <- function(df) {
		df=data.frame(df)
		df<-apply(df, 2, function(x)
			if (class(x)%in%c('character', 'factor')) rename.fkt(x) else x)
		colnames(df) <- rename.fkt(colnames(df))
		df
		}
	
	#tmp_pr = tmp[, list(categories = paste(renamed_category, collapse= ', '), ncategories = length(unique(renamed_category)), nbrands = sum(nbrands), nobs = mean(obs)), by = c('renamed_country')]
	
	

	tmp_pr <- data.frame(convert_txt(tmp))
	totals = cbind("Total", "", sum(tmp$nbrands), mean(tmp$obs))
	colnames(totals) <- colnames(tmp_pr)
	tmp_pr <- rbind(tmp_pr, totals)
	
	
	#write.table(tmp_pr, '..//output//data_coverage.csv', row.names=F, sep = ';')
	sink('../output/data_coverage.txt')
	print(tmp_pr)
	sink()
	
### Elasticity computation
	

	# missing markets
	elast <- rbindlist(sim_res[!checks_simulation=='error'])
	elast[,market_id := as.numeric(as.character(market_id))]
	elast=elast[brand == sim_brand]
	
	# Merge empirical elasticities (short-term estimates)
	ms_means <- rbindlist(lapply(results_brands[[1]][checks_brands[[1]]=='ok'], function(x) {
		ms=x$melted_panel[variable=='unitsales_sh', list(unitsales_sh=mean(value)), by=c('country', 'category', 'market_id', 'brand')]
		
		coefs=data.table(x$model@coefficients)[varname%in%c('price', 'dist', 'llength', 'novel'), c('coef', 'se', 'z', 'brand', 'varname'),with=F]
		fit = data.table(x$R2)
		out=merge(coefs, ms, by=c('brand'))
		merge(out, fit[, c('brand', 'R2levels'),with=F], all.x=T, by=c('brand'))
		
		
		}))
		
	ms_means[, empirical_st := coef * (1-unitsales_sh)]	
	
	setkey(ms_means, market_id, brand, varname)
		

	# calculate long-term elasticities
	# merge coefficients
	elast[, cum_elast12 := sum(elast_mean[period>=2&period<=12+1], na.rm=T), by = c('sim_var', 'market_id', 'brand')]
	elast[, cum_elast24 := sum(elast_mean[period>=2&period<=24+1], na.rm=T), by = c('sim_var', 'market_id', 'brand')]
	elast[, cum_elast36 := sum(elast_mean[period>=2&period<=36+1], na.rm=T), by = c('sim_var', 'market_id', 'brand')]
	
	export <- elast[, list(st=mean(100*elast_mean[period==2]), cum12 = mean(cum_elast12*100), cum24 = mean(cum_elast24*100), cum36 = mean(cum_elast36*100), 
								   perm12=mean(100*elast_mean[period==12+1]), perm24=mean(100*elast_mean[period==24+1]), perm36=mean(100*elast_mean[period==36+1])), by=c('market_id', 'category', 'country', 'brand', 'sim_var')]
	
	setkey(export, market_id, brand, sim_var)
	export[ms_means, ':=' (st_empirical = i.empirical_st, st_z = i.z)]
	
	write.table(export, file = '../output/elasticities.csv', row.names=F, sep=';')
	
	
	# Provide some summaries here
	export[, correct_sign := 1]
	export[sim_var == 'price', correct_sign := -1]
	
	# parameter significance
	export[, list(expected_sign = ifelse(unique(correct_sign)==1, "+", "-"),n=.N, n_correct = length(which((st_empirical*correct_sign>0 & abs(st_z) >= 1.69)))/.N,
				  n_ns = length(which(abs(st_z) < 1.69))/.N,
				  n_wrong = length(which((st_empirical*correct_sign<=0 & abs(st_z) >= 1.69)))/.N), by = c('sim_var')]
				  
				  
	export[, lapply(.SD, median), by = c('sim_var'), .SDcols = c('st', 'cum12', 'cum24', 'cum36', 'perm12', 'perm24', 'perm36')]
	
	tmp = export[, lapply(.SD, median), by = c('sim_var', 'country'), .SDcols = c('st', 'cum12', 'cum24', 'cum36', 'perm12', 'perm24', 'perm36')]
	setorder(tmp, sim_var, country)
	
	tmp = export[, lapply(.SD, median), by = c('sim_var', 'category'), .SDcols = c('st', 'cum12', 'cum24', 'cum36', 'perm12', 'perm24', 'perm36')]
	setorder(tmp, sim_var, category)
	
	
	
	tmp[sim_var=='price']
	
	by(tmp[,-c(1:2), with=F], tmp$sim_var, function(x) apply(x, 2, median))
	
	
	dcast(tmp[sim_var=='price'], country ~ sim_var)
	
	

	# Plot elasticities by market
	for (m in unique(elast$market_id)) {
		for (.var in unique(elast[market_id==m]$sim_var)) {
			fn=paste0('../output/irfs/IRF_', m,'_', unique(elast[market_id==m]$country), '_',
											  unique(elast[market_id==m]$category), '_',
											  .var, '.png')
										   	  
			png(fn, res=200, units='in', height=8, width=16)
			print(xyplot(elast_mean ~ period|as.character(brand), type = 'l', main = fn, data = elast[market_id==m & sim_var==.var], auto.key= TRUE, scales=list(relation=list(y='free')),
				panel =function(x,y,...){ 
									   panel.xyplot(x,y,...);
									   panel.lines(x=0:36, y=rep(0,37), col="black", lty=2);
									   }))
			dev.off()
			}
		}

	# Plot data by market
	for (m in which(checks_brands[[1]]=='ok')) {
		obj = results_brands[[1]][[m]]
		fn=paste0('../output/data/plot_', unique(obj$specs$market_id),'_', unique(obj$specs$country), '_',
											  unique(obj$specs$category), '.png')

		png(fn, res=200, units='in', height=8, width=16)
		print(xyplot(value ~ date|variable, groups=brand, data = obj$melted_panel[variable%in%c('price', 'dist', 'llength', 'novel','unitsales_sh')],
				scales=list(relation=list(y='free')), main = fn, type='l', auto.key=T))
		
		dev.off()
		}
		
		
	# Print model results
	sink('../output/model_results.txt')
	for (m in which(checks_brands[[1]]=='ok')) {
		obj=results_brands[[1]][[m]]
		cat(paste0('\n\nModel for market ID ', unique(obj$specs$market_id), ' - ', unique(obj$specs$category), ' - ',unique(obj$specs$country), '\n'))
		cat('====================================================================\n\n')
		print(obj$model)
		cat('\n\nFit\n\n')
		print(obj$R2)
		cat('\n\nUnit root tests\n\n')
		print(obj$adf_sur)
		
		}
	sink()
	
	# Fit stats
	sink('../output/summary.txt')
	cat('Model fit for log(ms_b/ms_B), R2s:\n==================================================\n\n')
	print(summary(ms_means[, list(R2=mean(R2levels)), by =c('market_id', 'brand')]$R2))
	cat('\nNote: NAs are models for benchmark brands - these are not directly estimated\n')
	
	# Summarized elasticities
	cat('\n\n\nEstimated elasticities\n==================================================\n\n')
	by(export[, -c(1:5), with=F], export$sim_var, summary)
	
	cat('\n\n\nCorrelations between simulated and estimated short term elasticities\n==================================================\n\n')
	by(export, export$sim_var, function(x) cor(x$st, x$st_empirical))
	
	sink()
	
	
	
	