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


ncpu = 16

require(parallel)
cl<-makePSOCKcluster(ncpu)

### LOAD DATA SETS AND RESULTS
	source(paste0(Sys.getenv('uvt_setup_gfk'), 'setup.R'))
	load(file=paste0(dirs$svn,'derived\\output\\datasets.RData'))
	load(file=paste0(dirs$svn,'analysis\\output\\results.RData'))

	paneldata=rbindlist(lapply(all_data, function(x) rbindlist(x$data_cleaned)))
	paneldata<-paneldata[order(category,country,brand,date)]

	# Assemble results (if they are multiple results, take the LAST result set)
	
	# re-structure result objects by model
	all_results <- NULL
	
# changes dimensions of list
	for (j in seq(along=multiple_results[[1]])) {
		all_results[[j]]<- list(models = NULL, specs=NULL)
		if (class(multiple_results[[1]][[j]])=='try-error') next
		all_results[[j]]$specs <- multiple_results[[1]][[j]]$specs
		
		for (i in seq(along=multiple_results)) {
				all_results[[j]]$models[[i]] <- multiple_results[[i]][[j]]
				}
			}

max.prod = length(all_results)

### FUNCTIONS
library(lattice)
lattice.options(default.theme = standard.theme(color = FALSE))

{

MHmakeRandomString <- function(n=1, lenght=12)
	{
		randomString <- c(1:n)                  # initialize vector
		for (i in 1:n)
		{
			randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
									 lenght, replace=TRUE),
									 collapse="")
		}
		return(randomString)
	}

	
new_spin <- function(spinfn, outfn) {
			cur_wd = getwd()
			setwd(dirs$temp)
			random_fname=MHmakeRandomString(1,12)
			random_fpath=MHmakeRandomString(1,6)
			dir.create(random_fpath)
			setwd(paste0(dirs$temp,'\\',random_fpath))
			file.copy(spinfn, paste0(random_fname,'.R'),overwrite=T)
			
			capture.output(spin(paste0(random_fname,'.R')))
			
			file.copy(paste0(random_fname,'.html'),outfn, overwrite=T)
			unlink(paste0(random_fname,'.*'))
			setwd(cur_wd)
			unlink(paste0(dirs$temp,'\\',random_fpath),recursive=T)
			cat(paste0('File saved to: ', outfn, '\n'))
		}
		
brand_report <- function(out) {
		out<<-out
		if (!is.null(out)) {
		panel<<-paneldata[dat_id==as.numeric(out$specs["dat_id"])]
		#sku_overview <- all_data[[which(names(all_data)==panel$country
		new_spin(paste0(dirs$svn, 'analysis\\code\\report_brand.R'), paste0(dirs$svn,'html_report\\', 'res_',paste0(out$specs[1:3],collapse='_'),'.html'))
			}
		rm(out)
		}

get_names <- function(filename) {
	tmp2=strsplit(filename,'_')
	tmp<-NULL
	tmp$category = tolower(tmp2[[1]][2])
	tmp$country = tolower(tmp2[[1]][3])
	tmp$brand = tolower(strsplit(tmp2[[1]][4], '.csv')[[1]][1])
	return(tmp)
	}

}
	
######################
# INVESTIGATE ERRORS #
######################

# Estimation procedure crashes!
	# check all models
	if(0){
	analysis_errors <- lapply(c('tryerror', 'skipping'), function(type) lapply(multiple_results, function(x) unlist(lapply(x, function(y) {if (type=='tryerror') return(class(y)=='try-error')
																																		   if (type=='skipping') return(is.null(y))
	
																																			}))))
	analysis_errors <- lapply(analysis_errors, function(x) do.call('cbind', x))
	analysis_errors <- do.call('cbind', analysis_errors)
	
	n_models = seq(along=all_results[[1]]$models)
	
	colnames(analysis_errors) <- paste0(rep(c('tryerror', 'skipping'),each=length(n_models)),rep(n_models, 2))
	analysis_errors <- data.table(analysis_errors)
	analysis_errors[, id:=1:.N]
	
	analysis_errors[tryerror1==T]
	rep_errors = apply(analysis_errors[, 1:(ncol(analysis_errors)-1),with=F],1, function(x) any(x))
	}
# try to find some errors
rep_errors=unlist(lapply(all_results, function(x) length(unlist(lapply(x$models, function(y) y$error)))>0))
	

	n_models = seq(along=all_results[[1]]$models)
	
ur=rbindlist(lapply(all_results[!(rep_errors)], function(x) do.call('rbind', lapply(1:length(x$models), function(target_model) cbind(model=target_model, x$models[[target_model]]$ur,x$specs)))))
coint=data.table(do.call('rbind', lapply(all_results[!(rep_errors)], function(x) do.call('rbind', lapply(1:length(x$models), function(target_model) cbind(model=target_model,coint_rank=x$models[[target_model]]$coint_rank,x$specs))))))
coint[is.na(coint_rank), coint_rank:=0]
void<-coint[, model:=as.integer(model)]
rm(void)

models1 = ur[, list(nvars=.N,n_unitroot=sum(order>0), n_endog_ur = sum(order[type=='endog']>0,na.rm=T)),by=c('category', 'country', 'brand','model')]
models <- merge(models1, coint, by=c('category','country','brand', 'model'),all.x=T, all.y=T)





			
####################################
#### BRAND-SPECIFIC REPORTING  #####
####################################

#brand_report(all_results[[692]])

# Cluster production of reports
	void<-clusterEvalQ(cl, library(knitr))
	void<-clusterEvalQ(cl, library(data.table))
	void<-clusterEvalQ(cl, source(paste0(Sys.getenv('uvt_setup_gfk'), 'setup.R')))
	rm(void)
	
	clusterExport(cl, c('paneldata', 'index','MHmakeRandomString', 'brand_report', 'new_spin', 'models'))
	
	# execute on cluster
		unlink(paste0(dirs$svn,'html_report/res*'))
		void<-clusterApply(cl, all_results[!rep_errors], brand_report)
		rm(void)
		
	# Non-clusterized version
		if (0){ 
		for (i in 1:nrow(index)) {
			print(i)
			brand_report(all_results[[i]])
		#	try(brand_report(all_results[[i]]),silent=T)
			}
		}
	
#######################################
#### CATEGORY-SPECIFIC REPORTING  #####
#######################################

if(0) {
unlink(paste0(dirs$svn,'html_report/cat_*'))

	for (i in seq(along=all_data)) {
		for (j in seq(along=all_data[[i]]$data_cleaned)) {
			print(i)
			cur_wd = getwd()
			setwd(dirs$temp)
			random_fname=MHmakeRandomString(1,12)
			
			file.copy(paste0(dirs$svn, 'analysis\\code\\report_category.R'), paste0(random_fname,'.R'),overwrite=T)
			tmp_dat=all_data[[i]]$data_cleaned[[j]]
			tmp_skutable=all_data[[i]]$sku_info[tolower(country)==unique(tmp_dat$country)]
			specs =c(as.character(tmp_dat$country[1]),as.character(tmp_dat$category[1]))
			specs <- sapply(specs, function(x) gsub('[-]|[/]','', x))
			# 'correct' the category name beforehand!
			
			spin(paste0(random_fname,'.R'))
			file.copy(paste0(random_fname,'.html'), paste0(dirs$svn,'html_report\\', 'cat_', paste(specs,collapse='_'),'.html'))
			unlink(paste0(random_fname,'.*'))
			setwd(cur_wd)
			}
		}
}


#out=analyze(1)
#makedat(1)
#gospin <- function() spin(paste0(dirs$svn, 'meta_analysis\\code\\report_cat.R'))


	
################################################
# RESULTS OF COINTEGRATION AND UNIT ROOT TESTS #
################################################

new_spin(paste0(dirs$svn, 'analysis\\code\\report_results.R'),  paste0(dirs$svn,'html_report\\', 'sum_results.html'))
new_spin(paste0(dirs$svn, 'analysis\\code\\report_classification.R'),  paste0(dirs$svn,'html_report\\', 'sum_classification.html'))

new_spin(paste0(dirs$svn, 'analysis\\code\\report_growth.R'),  paste0(dirs$svn,'html_report\\', 'sum_growth.html'))

###################################################################
#																  #	
# SALES RESPONSE MODELS 										  #
#																  #	
# Estimation problems (VIFs, extreme coefs, insign. coefficients) #
#																  #	
###################################################################

new_spin(paste0(dirs$svn, 'analysis\\code\\report_estimproblems.R'), paste0(dirs$svn,'html_report\\', 'sum_estimproblems.html'))
new_spin(paste0(dirs$svn, 'analysis\\code\\report_coefsummary.R'), paste0(dirs$svn,'html_report\\', 'sum_coefsummary.html'))

#####################
# CREATE INDEX.HTML #
#####################

source(paste0(dirs$svn, 'analysis\\code\\index.R'))

##########################################
# COPY FILES TO PUBLIC FOLDER IN DROPBOX #
##########################################

all_files <- list.files(path=paste0(dirs$svn, 'html_report\\'),'.html')
unlink('D:/DATTA/Dropbox/Public/uvt_singapore/*')
for (fn in all_files) {
	file.copy(paste0(dirs$svn, 'html_report\\',fn), paste0('D:/DATTA/Dropbox/Public/uvt_singapore/',fn),overwrite=T)
	}

	
#####################################
########### MUELLHALDE ##############
#####################################

#> cor(paneldata[,5:(ncol(paneldata)-2),with=F], use='pairwise.complete.obs')
 
	
### REPORT CORRELATIONS BETWEEN LLENGTH AND NOVEL

#test=paneldata[, list(cor=cor(llength,novel,use='complete.obs')),by=c('category','country','brand')]
#test[abs(cor)>.9]
# Check WHERE price is positive, by category
coef = rbindlist(lapply(all_results[!(rep_errors)], function(x) cbind(x$salesresponse, rbind(x$specs))))

coef[grep('price',var_model), list(N=length(which(beta>0&pval<.2)), N.tot=.N, share = length(which(beta>0&pval<.2))/.N), by=c('category')]
coef[grep('price',var_model), list(N=length(which(beta>0&pval<.2)), N.tot=.N, share = length(which(beta>0&pval<.2))/.N), by=c('country')]


coef[grep('wunique',var_model), list(meanbeta=mean(beta),N=length(which(beta>0&pval<.2)), N.tot=.N, share = length(which(beta>0&pval<.2))/.N), by=c('category')]



coef[grep('price',var_model), list(N=length(which(beta>0&pval<.2)), N.tot=.N, share = length(which(beta>0&pval<.2))/.N), by=c('country')]

test=coef[grep('price',var_model), list(N=length(which(beta>0&pval<.2)), N.tot=.N, share = length(which(beta>0&pval<.2))/.N), by=c('category', 'country')][order(share,decreasing=T)]


# Correlations by category
#out=lapply(split(paneldata,paneldata$category), function(x) cor(x[,5:(ncol(paneldata)-2),with=F], use='pairwise.complete.obs'))
#cor(paneldata[,5:(ncol(paneldata)-2),with=F], use='pairwise.complete.obs')


#length(out)


