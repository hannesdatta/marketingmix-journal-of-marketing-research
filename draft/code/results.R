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
	  
# BUILD LATEX REPORTS FOR EACH SUBELEMENT OF RESULTS
for (r in 1) {# seq(along=results)) {
	brand_results <<- results_brands[[r]]
	category_results <<- results_category[[r]]
	model <<- models[[r]]
	brands_check <<- checks_brands[[r]]
	categories_check <<- checks_category[[r]]

	model$descr <- gsub('[%]', 'perc. ', model$descr)
	
	# Run report
	savewd = getwd()
	setwd('..//temp')
	
	knit("..//code//template.Rnw", output = 'results.tex')
	shell(paste0('pdflatex results.tex -job-name=', paste0('results_', r, ifelse(!is.null(models[[r]]$fn), paste0('_', models[[r]]$fn), '')), ' -output-directory=..//output'))
	
	setwd(savewd)
	}

