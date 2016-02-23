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


#################################
## PRODUCE TABLES WITH RESULTS  #
#################################

# Load data sets and results
	load(file = '..\\..\\derived\\output\\datasets.RData')
	load(file = '..\\..\\derived\\temp\\attributes.RData')
	load(file = '..\\..\\analysis\\output\\results.RData')

	#all_results_cop <- all_results
	#load(file = '..\\..\\analysis\\output\\resultsNOCOP.RData')

# Load externals	
	source('..\\external\\proc_rename.R')

# Load packages
	require(data.table)
	require(xtable)

# Prepare panel data
	brand_panel=rbindlist(lapply(all_data, function(x) rbindlist(x$data_cleaned)))
	setorder(brand_panel, market_id, category,country,brand,date)
	
	category_panel = rbindlist(lapply(all_data, function(x) rbindlist(x$data_category)))
	setorder(category_panel, market_id, category,country,date)


# Check models for completeness / crashes
checks_brands <- NULL
for (i in seq(along=results_brands)) {
	check = unlist(lapply(results_brands[[i]], function(x) {
	if(class(x)=='try-error') return('error')
	if(class(x)=='list' & length(x$error)>0) return('small_N')
	return('ok')
	}))
	checks_brands[[i]]<-check
	}
	

	checks_category <- NULL
for (i in seq(along=results_category)) {
	check = unlist(lapply(results_category[[i]], function(x) {
	if(class(x)=='try-error') return('error')
	if(class(x)=='list' & length(x$error)>0) return('small_N')
	return('ok')
	}))
	checks_category[[i]]<-check
	}
	
