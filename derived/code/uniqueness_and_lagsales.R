#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|

# Goal: Define uniqueness measures per SKU and date

require(data.table)
require(compiler)

#################################################################
### LOAD RAW DATA AND PREPARE DATA FILES INTO datlist_final
#################################################################

# Load merged and categorized data sets
load('..\\temp\\categorized.RData')

# Check number of categories in file
rbindlist(lapply(datlist_final, function(x) x[, list(N=.N),by=c('catname','country')]))

# Remove 'non-used' parts of the dataset (e.g., GfK 2012, if GfK 2015 data is being used)
datlist_final<-lapply(datlist_final, function(x) {
	tmp=x[which(used==T)][, category:=catname]
	if (!nrow(tmp)==0) return(tmp)})
# delete empty categories

datlist_final <- datlist_final[!unlist(lapply(datlist_final, is.null))]


# Check number of categories in file
rbindlist(lapply(datlist_final, function(x) x[, list(N=.N),by=c('category','country')]))

# Extract first availability dates from the data
  # get string vector of first activity dates
  months=data.table(char_month=unique(unlist(lapply(datlist_final, function(x) unique(x$firstactivity)))))
  months[, char_month:=gsub('[-]', ' 20', char_month)]
  months[, year:=sapply(char_month, function(x) strsplit(x, ' ')[[1]][2])]
  months[, month:=sapply(char_month, function(x) tolower(substr(strsplit(x, ' ')[[1]][1],1,3)))]
  
  raw_months<-c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')
  months[, month_no := as.character(formatC(match(month, raw_months), width=2,flag=0))]
  
  months[, firstactivity_date:=as.Date(paste0(year,'-', month_no, '-01'))]
  
  months[char_month=='BEFORE 2001', firstactivity_date:=as.Date('2000-12-01')]
  months<-months[!is.na(char_month)]

  
#################################################################
### EXECUTING CREATION OF VARIABLES
#################################################################

source('proc_functions.R')

# Put all data in a list (per category, and country)
# Creates all attribute-based measures
skus_by_date_list <- NULL
novelty_list <- NULL


for (i in 1:length(datlist_final)) {
	cat('=============================================================\n')
	cat(names(datlist_final)[i],fill=T)
	cat('=============================================================\n')
	assign("last.warning", NULL, envir = baseenv())
	

	setkey(datlist_final[[i]], category, country, brand, model, date)

	datlist_final[[i]][, sales_units_nonzero:=sales_units]
	datlist_final[[i]][sales_units_nonzero<0, sales_units_nonzero:=0]
	
	skus_by_date = datlist_final[[i]][, list(t_sales_units=sum(sales_units_nonzero), 
											 t_value_sales = sum(sales_units_nonzero*as.numeric(price_lc)),
											 t_value_sales_usd = sum(sales_units_nonzero*as.numeric(price_usd)),
											 t_price = mean(price_lc),
											 t_price_usd = mean(price_usd),
											 t_numdist = ifelse(all(sales_units_nonzero==0), mean(numeric_distribution), sum(sales_units_nonzero*numeric_distribution)/sum(sales_units_nonzero)),
											 t_wdist = ifelse(all(sales_units_nonzero==0), mean(weighted_distribution), sum(sales_units_nonzero*weighted_distribution)/sum(sales_units_nonzero))), 
											 by=c('category', 'country', 'market_id', 'brand', 'model', 'date')]
	
	###################################
	# Calculation of novelty measure  #
	###################################

	# Definition: Number of SKUs introduced within the last X months, expressed as percentage of unique SKUs sold in the current period
		setkey(months, char_month)
		setkey(datlist_final[[i]], firstactivity)
		datlist_final[[i]][months, firstactivity_date:=i.firstactivity_date]
		
		tmp_novelty = datlist_final[[i]][, list(first_datecoded = min(firstactivity_date), first_datedata = min(date), last_date = max(date)),by = c('category','country','brand','model')]
    tmp_novelty[is.na(first_datecoded), first_date:=first_datedata]
    tmp_novelty[first_datecoded>first_datedata, first_date:=first_datedata]
    tmp_novelty[first_datecoded<=first_datedata, first_date:=first_datecoded]
    
    if(0){
    # Novelty
    tmp = tmp_novelty[, c('category','country','brand','model','first_date')]
    
    alldates=seq(from=min(tmp$first_date,na.rm=T), to=as.Date('2015-12-01'), by = '1 month')
    
    tmp=rbind(tmp, data.table(category='null',country='null',brand='null',model='null', first_date=alldates))
    
    tmp[, id := .GRP,by=c('category','country','brand','model')]
    tmp[, value:=1]
    novelty=dcast(tmp, id+first_date~., drop=F, fill=0)
    setnames(novelty, '.', 'novel')
    
    setkey(novelty)
    setkey(tmp, id)
    novelty=novelty[tmp, ':=' (category=i.category, country=i.country, brand=i.brand, model=i.model)]
    novelty=novelty[!id==max(id)]
    
    
    #dim(test)
    
    }
    
    novelty_list[[i]]<-tmp_novelty
    
    
		skus_by_date <- merge(skus_by_date, tmp_novelty ,by=c('category', 'country','brand','model'),all.x=T)

		uniq_date <- skus_by_date[, list(N=.N),by='date']
		uniq_date[, N:=NULL]
		
		for (lags in c(1,3,6,12)) {
			uniq_date[, paste0('date_lag', lags) := as.Date(sapply(date, function(x) {m<-as.POSIXlt(x); m$mon=m$mon-lags; return(as.character(as.Date(m)))}))]
			}
		
		skus_by_date=merge(skus_by_date, uniq_date, by = c('date'))
		rm(uniq_date)
	
	skus_by_date_list[[i]] <- skus_by_date
	
	print(warnings())
	}
	
names(skus_by_date_list) <- names(datlist_final)
names(novelty_list) <- names(datlist_final)

print(warnings())

  # verify first_date field has been properly coded
  all=rbindlist(novelty_list)
  all[,diff:=as.numeric(difftime(first_datedata,first_datecoded, units='weeks'))]
  
  # hard-coded first activity dates BEFORE first sales observation
  val1=nrow(all[first_datecoded<first_datedata])/nrow(all)
  # hard-coded first activity dates EQUAL to first sales observation
  val2=nrow(all[first_datecoded==first_datedata])/nrow(all)
  # hard-coded first activity dates AFTER first sales observation
  val3=nrow(all[first_datecoded>first_datedata])/nrow(all)
  # hard-coded first activity MISSING
  val4=nrow(all[is.na(first_datecoded)])/nrow(all)
  #asserts
  stopifnot(val1+val2+val3+val4==1)
  stopifnot(nrow(all[is.na(first_date)])==0)
  
# save
save(skus_by_date_list, novelty_list, file='..\\temp\\uniqueness_and_lagsales.RData')
