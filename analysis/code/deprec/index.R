
#########################
### CREATE INDEX.HTML ###
#########################
	
	tmp=do.call('rbind',lapply(all_results[!rep_errors], function(x) {res = try(x$specs,silent=T); if(class(res)=='try-error') return(data.frame(NA,NA,NA,NA)) else return(x$specs)}))
	tmp<-data.table(tmp)
	tmp[,dat_id:=as.numeric(dat_id)]
	
	
	tmp<-tmp[order(category,country)]
	tmp=tmp[!is.na(category)]
	unlink(paste0(dirs$svn,'html_report/index.html'))
	# Create global INDEX file (index.html)
	sink(paste0(dirs$svn,'html_report/index.html'))
	cat('<html>\n')
	
	cat(paste0('<h1>GfK Singapore Project</h1>\n'))
	cat('Date of report generation: ', format(Sys.time(), "%d %B %Y, %H:%M (NL local time)<br><br>"))
	#cat(paste0('<b>Overview of the data</b><br>'))
	
	cat(paste0('<h2>Summaries</h2>'))
	cat(paste0('<a href=\"', paste0('sum_results.html'),'\">', '<li>Data set overview and outcome of UR/cointegration tests','</a><br>\n'))
	cat(paste0('<a href=\"', paste0('sum_classification.html'),'\">', '<li>Classification according to Dekimpe and Hanssens (1999)','</a><br>\n'))
	cat(paste0('<a href=\"', paste0('sum_coefsummary.html'),'\">', '<li>Summary of elasticities (from sales response model)','</a><br>\n'))
	cat(paste0('<a href=\"', paste0('sum_estimproblems.html'),'\">', '<li>Problematic cases (VIF>10, lack of significance, weird coefficients)','</a><br>\n'))
	cat(paste0('<a href=\"', paste0('sum_growth.html'),'\">', '<li>Growth in brand sales by categories and countries','</a><br>\n'))
	
	cat(paste0('<h2>Estimated sales response and VAR/VEC models</h2>'))
	
	#cat(paste0('<b>Estimated models</b><br>'))
	for (k in 1:nrow(tmp)) {
		cat(paste0('<a href=\"', paste0('res_',paste(tmp[k,1:3,with=F],collapse='_'),'.html'),'\">', paste(tmp[k,1:3,with=F],collapse=' - '),'</a><br>\n'))
		
		}
	
	cat(paste0('<h2>Category-country overview and product attributes</h2>'))
	
	for (i in seq(along=all_data)) {
		for (j in seq(along=all_data[[i]]$data_cleaned)) {
			tmp_dat=all_data[[i]]$data_cleaned[[j]]
			tmp_skutable=all_data[[i]]$sku_info[country==unique(tmp_dat$country)]
			specs =c(as.character(tmp_dat$country[1]),as.character(tmp_dat$category[1]))
			
			cat(paste0('<a href=\"', paste0('cat_', paste(specs,collapse='_'),'.html','\">', paste(rev(specs),collapse=' - '),'</a><br>\n')))
		
			}
		}
	
	cat('</body>\n')
	cat('</html>\n')
	sink()
