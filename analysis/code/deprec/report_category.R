#/* 
#    _____            _                            _   
#   |  __ \          | |                          | |  
#   | |  | |   __ _  | |_    __ _     ___    ___  | |_ 
#   | |  | |  / _` | | __|  / _` |   / __|  / _ \ | __|
#   | |__| | | (_| | | |_  | (_| |   \__ \ |  __/ | |_ 
#   |_____/   \__,_|  \__|  \__,_|   |___/_ \___|  \__|
#                                        | |           
#    _ __    ___   _ __     ___    _ __  | |_          
#   | '__|  / _ \ | '_ \   / _ \  | '__| | __|         
#   | |    |  __/ | |_) | | (_) | | |    | |_          
#   |_|     \___| | .__/   \___/  |_|     \__|         
#                 | |                                  
#                 |_|                                  
# 

#gospin <- function() spin('report_dataset_overview.R')

#*/




#+ setup, include=FALSE
library(knitr)
require(data.table)
require(reshape2)
require(lucid)
require(xtable)
require(CADFtest)


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


#' GfK Singapore Project: `r I(as.character(tmp_dat$country[1]))` - `r I(as.character(tmp_dat$category[1]))`
#' ==================================
#' Date of report generation: `r I(format(Sys.time(), "%d %B %Y, %H:%M (NL local time)"))`
#'


	
#/* 
#     _____                               _          _                  _           _                            _   
#    / ____|                             | |        | |                | |         | |                          | |  
#   | |        ___    _ __ ___    _ __   | |   ___  | |_    ___      __| |   __ _  | |_    __ _     ___    ___  | |_ 
#   | |       / _ \  | '_ ` _ \  | '_ \  | |  / _ \ | __|  / _ \    / _` |  / _` | | __|  / _` |   / __|  / _ \ | __|
#   | |____  | (_) | | | | | | | | |_) | | | |  __/ | |_  |  __/   | (_| | | (_| | | |_  | (_| |   \__ \ |  __/ | |_ 
#    \_____|  \___/  |_| |_| |_| | .__/  |_|  \___|  \__|  \___|    \__,_|  \__,_|  \__|  \__,_|   |___/  \___|  \__|
#                                | |                                                                                 
#                                |_|                                                                                 
#*/


#' ## Overview


#+ print_data_overview
#'* starting date: `r I(min(tmp_dat$date[!is.na(tmp_dat$date)]))`
#'* end date: `r I(max(tmp_dat$date[!is.na(tmp_dat$date)]))`
#'* total number of brands: `r I(prettify(length(unique(tmp_skutable$brand))))`
#'* total number of SKUs: `r I(prettify(length(unique(tmp_skutable$sku_id))))`
#'* total number of attributes: `r I(prettify(length(grep('attr_',colnames(tmp_skutable),value=T))))`
#'* total number of included brands: `r I(prettify(length(unique(tmp_dat$brand))))`
#'* total number of included SKUs: `r I(prettify(length(unique(tmp_skutable[tolower(brand)%in%tolower(tmp_dat$brand)]$sku_id))))`
#'
#' (selection rule for including brands: Top 7 brands in each country-category, unless the brand has a marketshare lower than 2.5%.)
#'

#' ### Brands, total sales, and market shares
#+ market share,echo=FALSE
out=tmp_dat[, list(totalsales = sum(unitsales,na.rm=T),mean_adspent=mean(radv,na.rm=T)), by=c('country', 'brand')] #number_NAs=length(which(any_na==T)
void<-out[, marketshare:=totalsales/sum(totalsales,na.rm=T),by=c('country')]
rm(void)
out=out[order(marketshare,decreasing=T)]
void<-out[, rank:=1:.N]
void<-out[, cum_share := cumsum(marketshare)]
print(kable(data.frame(out)))

#/*
#' ### Individual analyses and response elasticities
#for (brandX in out$brand)
#cat(paste0('<a href=\"', paste0('res_',paste(specs,collapse='_'),'.html'),'\">', paste(specs,collapse=' - '),'</a><br>\n'))
#*/
	
	
#' ### Attributes of included brands
#' 

#+ echo = FALSE, fig.width=7, fig.height=12,dpi=100
for (brandX in out$brand) {
    cat('##################################################\n')
	cat("### ", brandX, "\n")
	cat('##################################################\n')
	par(mar=c(5, 4, 3, 2))#, xaxs='i', yaxs='i')
	#print(brandX)
	print(kable(out[tolower(brand)==tolower(brandX)]))
	cat('\n')
    cat('###########################\n')
	cat("### Product attributes  ###\n")
	cat('###########################\n')
	attrib = grep('attr_',colnames(tmp_skutable),value=T)
	
	cat(paste0('Total number of SKU\'s: ', length(unique(tmp_skutable[tolower(brand)==tolower(brandX)]$sku_id))),fill=T)
	for (attrib2 in attrib) {
		cat('\n')
		cat(attrib2,fill=T)
		print(tmp_skutable[tolower(brand)==tolower(brandX), list(freq=length(unique(sku_id))),by=attrib2])
		}
	
	#specs =c(as.character(tmp_dat$country[1]),as.character(tmp_dat$category[1]), tolower(brandX))

	
	cat('\n')

	cat('\n')
	}

	
#' 
#'
#' ### Descriptive statistics for all brands
#+ summarysection, echo=FALSE, width=300,results='asis'
options(width=300)
#descrdat=tmp_dat[,,with=F]
#br=tmp_dat$brand
#print(by(descrdat,br,summary))
suppressWarnings({
varsofinterest=c('llength','unitsales','r_wprice','wdist','wunique', 'r_adv')
#varsofinterest=c('line_length','monetarysales')
melted <- melt(tmp_dat,id.vars=c('brand','date'))
melted <- melted[!variable%in%c('country','category')&variable%in%varsofinterest]
void<-melted[, value:=as.numeric(value)]
stuff=melted[, list(mean=mean(value,na.rm=T), min=min(value,na.rm=T), max=max(value,na.rm=T), sd=sd(value,na.rm=T), nas = length(which(is.na(value)))), by=c('brand', 'variable')]
setnames(stuff,'variable', 'varname')
melted2 <- melt(stuff, id.vars=c('brand', 'varname'))
test=dcast(melted2, brand~varname+variable)
})
print(xtable(test,align=rep('r',ncol(test)+1),digits=0),type='html')


#tmp_dat[,lapply(.SD, function(x) c(mean=mean(x), max=max(x))), by='brand', .SDcols=c('line_length','monetarysales')]
#tmp_dat[,lapply(.SD, function(x) c(mean(x),max(x))), by='brand', .SDcols=c('line_length','monetarysales')]
