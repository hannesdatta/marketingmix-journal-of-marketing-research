#    _____            _                                                                   _     _                 
#   |  __ \          | |                                                                 | |   (_)                
#   | |  | |   __ _  | |_    __ _     _ __    _ __    ___   _ __     ___   _ __    __ _  | |_   _    ___    _ __  
#   | |  | |  / _` | | __|  / _` |   | '_ \  | '__|  / _ \ | '_ \   / _ \ | '__|  / _` | | __| | |  / _ \  | '_ \ 
#   | |__| | | (_| | | |_  | (_| |   | |_) | | |    |  __/ | |_) | |  __/ | |    | (_| | | |_  | | | (_) | | | | |
#   |_____/   \__,_|  \__|  \__,_|   | .__/  |_|     \___| | .__/   \___| |_|     \__,_|  \__| |_|  \___/  |_| |_|
#                                    | |                   | |                                                    
#                                    |_|                   |_|                                                    
#
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

########################################################################################################

# START UP 

########################################################################################################

library(data.table)
library(bit64)

####################################
# SALES AND MARKETING INSTRUMENTS  #
# provided by GfK                  #
####################################

# Function to read a zip file and save it
	
read_file <- function(fn, source_comment) {
		csvname=gsub('.ZIP', '.CSV', rev(strsplit(fn,'/')[[1]])[1], ignore.case=T)
		
		cat('Getting information for file: ', fn,fill=T)
		
		# Unzip and read file
			unzip(paste0(file_path,fn),exdir='../temp')
			dat<-fread(paste0('../temp/',csvname),stringsAsFactors=T, na.strings=c('NA', 'N.A.', 'UNKNOWN'))
			Sys.sleep(.8)
		# Remove temporary file
			unlink(paste0('../temp/',csvname))
		
		# convert local currency column to a long integer to make sure we can
		# consistently capture the high amount of digits.
			if ('PRICE <LC>' %in% colnames(dat)) {
				dat[, "PRICE <LC>":=as.numeric(get("PRICE <LC>"))]
				}
			
		# remove leading and trailing whitespace in column names
			trim <- function (x) gsub("^\\s+|\\s+$", "", x)
			setnames(dat, trim(colnames(dat)))
			
		# replace remaining special characters by underscores or blanks
			setnames(dat, gsub(' ', '_', colnames(dat)))
			setnames(dat, gsub('[-]', '_', colnames(dat)))
			setnames(dat, gsub('[(]', '', colnames(dat)))
			setnames(dat, gsub('[)]', '', colnames(dat)))
			setnames(dat, gsub('[.]', '', colnames(dat)))
			setnames(dat, gsub('[>]', '', colnames(dat)))
			setnames(dat, gsub('[<]', '', colnames(dat)))
		
		# rename 2012/2015 product categories
			if (any(unique(dat$REPORTINGPRODUCTGROUP)%in%c('DVD-PLAYER/REC.', 'WEBBOOKS'))) {
				dat[REPORTINGPRODUCTGROUP=='WEBBOOKS', REPORTINGPRODUCTGROUP:='MEDIATABLETS']
				dat[REPORTINGPRODUCTGROUP=='DVD-PLAYER/REC.', REPORTINGPRODUCTGROUP:='VIDEO PLAYER/REC.']
				}
				
			dat[, REPORTINGPRODUCTGROUP := as.factor(sub("[.][^.]*","", REPORTINGPRODUCTGROUP))]
			
		# add a count to columns with duplicate column names
			replace_multiple <- function(x) {
				dupl = seq(along=x)-sapply(x, function(l) match(l, x))>0
				if (length(which(dupl>0))) {
					x[dupl] <- paste(x[dupl], seq(from=2, length.out=length(which(dupl))),sep='')
					}
				return(x)
				}
			setnames(dat, replace_multiple(colnames(dat)))
		
		# add source comment to the data file (e.g., to indicate from where the data has been loaded, typically Gfk2012 or Gfk2015).
			dat[, source := as.factor(source_comment)]

		# convert PERIODS to R's date format
			Sys.setlocale("LC_TIME", "C")
			
			dates <- data.table(PERIOD=unique(dat$PERIOD))
			dates[!grepl('[-]',PERIOD), date:=base::as.Date(paste0('1 ',PERIOD), '%d %B %Y')]
			dates[grepl('[-]',PERIOD), date:=base::as.Date(paste0('1 ',PERIOD), '%d %b-%y')]
	
			dat <- merge(dat, dates,by='PERIOD', all.x=T)
				
		# clear memory, return data set
			gc()
			return(dat)
		}


# Read 2015 data
		# Get content of all zip files
		file_path = '..\\..\\..\\..\\Data\\gfk2015\\'
		file_list <- list.files(file_path, recursive=T,include.dirs=T,pattern='.ZIP',ignore.case = T)

		# Take out item-based smartphone files (i.e., take product-based files instead); based on email conversation
		# with GfK Singapore (they also use the product-based files in generating reports for their clients).

		file_list <- file_list[!grepl('ITEM', file_list)]

		# order by size for performance issues
		file_sizes=sapply(file_list, function(x) file.info(paste0(file_path,x))$size)
		file_list = file_list[order(file_sizes,decreasing=T)]

		datlist <- NULL
		
		for (i in (1:length(file_list))) {
			datlist[[i]]<-read_file(file_list[i], 'gfk2015')
		}
		

# Read 2012 data
		# Get content of all zip files
		file_path = '..\\..\\..\\..\\Data\\gfk2012\\'
		file_list <- list.files(file_path, recursive=T,include.dirs=T,pattern='.ZIP',ignore.case = T)

		# order by size for performance issues
		file_sizes=sapply(file_list, function(x) file.info(paste0(file_path,x))$size)
		file_list = file_list[order(file_sizes,decreasing=T)]

		old_length = length(datlist)
		
		for (i in (1:length(file_list))) {
			datlist[[i+old_length]]<-read_file(file_list[i], 'gfk2012')
		}
				
save(datlist, file = '../temp/unzipped.RData')
