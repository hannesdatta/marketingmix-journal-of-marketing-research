#  _________________________
# |  _______________  |     |
# | |               | |1 2 3|
# | |               | |4 5 6|
# | |               | |7 8 9|
# | |               | |# # #|
# | |               | | ___ |
# | |_______________| ||___|| 
# |___________________|_____|

# Goals: 
# - Create variables at the brand level
# - Determine beginning and end of observation periods per brand
# - Aggregate small brands to a common "allothers" brand

library(data.table)

dir.create('../audit')
sink('../audit/log.txt')
sink()

# -> brand_selection: selection of brands
  load('..//temp//select.RData') 

all_datasets <<- NULL


for (selrule in names(selection)) {
  print(selrule)
  
  attach(selection[[selrule]])
  
  # -> skus_by_date_list: sales data on a SKU-level by date
    load('..//temp//uniqueness_and_lagsales.RData') 
  
  # product attributes
    load('..//temp//attributes.RData') 
    
  
  # -> indicators (economic indicators)
  	load('..//temp//exch_cpi.RData')
  	Rcpp::sourceCpp('runsum.cpp')
  
  # -> advertising
  	adv <- fread('../temp/advertising.csv')
  	adv[, date:=as.Date(date)]
  	setkey(adv, category, country, brand, date)
  	
  # initialize object to store final data
  	all_data <- NULL
  	
  	source('proc_functions.R')
  	
  	
  	
  # run the preparation
  for (i in 1:length(skus_by_date_list)) {
  	cat('=============================================================\n')
  	cat(names(skus_by_date_list)[i],fill=T)
  	cat('=============================================================\n')
  	
  	skus_by_date <- skus_by_date_list[[i]]
  	
  	xattr = xattribs[[match(names(skus_by_date_list)[i], names(xattribs))]]
  	#####################################
  	#### Aggregation to brand-level #####
  	#####################################
  
  	cat('Aggregation to brand-level\n')
  	
  	# Which brands have been selected for modeling? Add the info here, plus brand names to be used for aggregation (e.g., for collapsing some brands)
  	skus_by_date[, category:=names(skus_by_date_list)[i]]
  	setkey(skus_by_date, category, country, brand)
  	setkey(brand_selection, category, country, brand)
  	
  	skus_by_date[brand_selection, ':=' (selected_brand=i.selected_brand, n_brands_selected=i.n_brands_selected, brand_rename=i.brand_rename, composite_included=i.composite_included)]
  	
  	# Rename brands to new aggregation level (e.g., composite brand)
  	setnames(skus_by_date, 'brand', 'brand_orig')
  	setnames(skus_by_date, 'brand_rename', 'brand')
  	
  	setkey(skus_by_date, category, country, brand, brand_orig, date)
  	
  	# keep only markets for which we have at least 2 brands selected
  	skus_by_date = skus_by_date[n_brands_selected>1]
  	
  	if(nrow(skus_by_date)==0) {
  	  all_data[[i]]<-NULL
  	  next
  	}
  	all_data[[i]] <- list()
  	
  	# Compute novelty of product assortment
    	idvars=c('market_id','category','country','brand','brand_orig', 'selected_brand', 'model')
    	
    	tmp = skus_by_date[, list(first_date=min(first_date,na.rm=T)),by=idvars]
    	alldates=seq(from=min(tmp$first_date,na.rm=T), to=as.Date('2015-12-01'), by = '1 month')
    	
    	emptydf=data.table(first_date=alldates)
    	for (coln in colnames(tmp)[!colnames(tmp)%in%c('first_date')]) {
    	  emptydf[, (coln):=NA]
    	}
    	
    	tmp=rbind(tmp, emptydf)
    	
    	tmp[, id := .GRP,by=idvars]
    	tmp[, value:=1]
    	novelty=dcast(tmp, id+first_date~., drop=F, fill=0)
    	setnames(novelty, '.', 'novel')
    	
    	setkey(novelty)
    	setkey(tmp, id)
    	
    	cmds=paste(sapply(idvars, function(cols) paste0(cols, '=i.',cols)),collapse=',')
    	eval(parse(text=paste0("novelty[tmp, ':='(", cmds, ")]")))
    	
    	novelty=novelty[!id==max(id)]
    	setorder(novelty, id, first_date)
    	novelty[, novel_sum:=cumsum(cumsum(novel)),by=idvars]
  	
  	# Compute average prices, distribution, etc. using current and lagged sales
    	skus_by_date[, id:=.GRP,by=idvars]
    	
      tmp=dcast(skus_by_date, id+date~., value.var=c('t_sales_units','t_value_sales', 't_value_sales_usd', 't_wdist', 't_price','t_price_usd'),drop=F)
    	
      setkey(tmp, id)
    	setkey(skus_by_date, id)
    	cmds=paste(sapply(idvars, function(cols) paste0(cols, '=i.',cols)),collapse=',')
    	eval(parse(text=paste0("tmp[skus_by_date, ':='(", cmds, ")]")))
    
    	tmp[is.na(t_sales_units), t_sales_units:=0]
    	tmp[t_sales_units<0, t_sales_units:=0]
    	
    	for (.var in c('t_price', 't_price_usd', 't_wdist')) {
    	  tmp[, paste0(.var, '_filled'):=na_forwarding(get(.var)), by=idvars]
    	  }
    
    	# add rolling sum of unit sales in `n` periods, including the current one
    	#tmp[, t_sales_units_rolled := run_sum(t_sales_units, n=3),by=idvars]
    	tmp[, ':=' (tmp_sales_units_rolled = run_sum(t_sales_units, n=3),
    	            first_sale=min(date[t_sales_units>0]), 
    	            last_sale=max(date[t_sales_units>0])),by = idvars]
    	
    	# weights INCLUDING current period
    	tmp[, t_sales_units_rolled_incl := tmp_sales_units_rolled,by=idvars]
    	tmp[is.na(t_sales_units_rolled_incl), t_sales_units_rolled_incl := 0]
    	tmp[(date>last_sale)|!last_sale>0, t_sales_units_rolled_incl:=0]
    	
    	# weights EXCLUDING current period
    	tmp[, t_sales_units_rolled := c(NA, tmp_sales_units_rolled[-.N]),by=idvars] # lag, to exclude the current period
    	tmp[is.na(t_sales_units_rolled), t_sales_units_rolled := 0]# set NAs to 0
    	tmp[(date>last_sale)|!last_sale>0, t_sales_units_rolled:=0]
    	    
    	tmp[, t_noweights:=1]
    	tmp[(date<first_sale)|(date>last_sale)|!last_sale>0, t_noweights:=0]
    	
    	
    	aggkey=c(setdiff(idvars,c('model','brand_orig')),'date')
    	aggkey_agg=c(setdiff(idvars,c('brand', 'model','brand_orig')),'date')
    	
    	setkeyv(tmp, c(idvars, 'date'))
    	setkeyv(novelty, c(idvars, 'first_date'))
    	
    	tmp[novelty, novelty_sum:=i.novel_sum]
  	
    	setkey(tmp, category, country, brand_orig, model)
    	setkey(xattr, category, country, brand, model)
    	
    	for (var in grep('^attr', colnames(xattr), value=T)) {
    	  tmp[xattr, paste0(var):=get(var)]
    	}
         
    # Aggregate data
    	
  	setkey(tmp, category, country, brand, date)
  	tmp[adv, adv:=i.adspent]  
  	tmp[is.na(adv), adv:=0]
  	
    w_type = 'arithmetic' #geometric' #arithmetic' # or geometric
    
     
    for (aggkey_iter in list(aggkey, aggkey_agg)) {
      print(aggkey_iter)
      #tmp[, brand_model:=paste0(brand_orig, model)]
      #tmp[, model_weights:=c(sum(t_sales_units_rolled), rep(0, .N-1)),by=c('market_id', 'brand_model', 'date')]
      #tmp[, N:=.N,by=c('market_id','brand_model', 'date')]
      #tmp[, sold:=0]
      #tmp[t_sales_units>0, sold:=1]
      
      merged_attr_sales = tmp[, list( usales=sum(t_sales_units,na.rm=T),
                                      upsales=sum(t_sales_units_rolled,na.rm=T),
                                      ucpsales=sum(t_sales_units_rolled_incl,na.rm=T),
                                      
                                      usales_sum_log = sum(ifelse(is.na(t_sales_units), 0, log(t_sales_units+1))),
                                      vsales = sum(t_value_sales,na.rm=T), 
  	                                   vsalesd = sum(t_value_sales_usd,na.rm=T),
  	                                         
  	                                         llen = length(unique(paste(brand_orig, model)[t_sales_units>0])),
  	                                         
                                            wspr=weigh_by_w(t_price_filled, t_sales_units, na.rm=T, type = w_type),
  	                                         wpspr=weigh_by_w(t_price_filled, t_sales_units_rolled, na.rm=T, type = w_type),
                                            wcpspr=weigh_by_w(t_price_filled, t_sales_units_rolled_incl, na.rm=T, type = w_type),
                                            nwpr= weigh_by_w(t_price_filled, t_noweights,na.rm=T, type = w_type),
        	                                         
  	                                         wsprd=weigh_by_w(t_price_usd_filled, t_sales_units, na.rm=T, type = w_type),
  	                                         wpsprd=weigh_by_w(t_price_usd_filled, t_sales_units_rolled, na.rm=T, type = w_type),
                                            wcpsprd=weigh_by_w(t_price_usd_filled, t_sales_units_rolled_incl, na.rm=T, type = w_type),
                                            nwprd = weigh_by_w(t_price_usd_filled, t_noweights,na.rm=T, type = w_type),
        	                                         
  	                                         wswdst = weigh_by_w(t_wdist_filled,t_sales_units,na.rm=T, type = w_type),
  	                                         wpswdst = weigh_by_w(t_wdist_filled,t_sales_units_rolled,na.rm=T, type = w_type),
                                            wcpswdst = weigh_by_w(t_wdist_filled,t_sales_units_rolled_incl,na.rm=T, type = w_type),
                                            nwwdst = weigh_by_w(t_wdist_filled, t_noweights, na.rm=T, type = w_type),
        	                                         
                                            adv = mean(adv, na.rm=T),
                                            wsadv = weigh_by_w(adv, t_sales_units, na.rm=T, type = w_type),
                                            wpsadv = weigh_by_w(adv, t_sales_units_rolled, na.rm=T, type = w_type),
                                            wcpsadv = weigh_by_w(adv, t_sales_units_rolled_incl, na.rm=T, type = w_type),
                                      
  	                                         nov1 = length(unique(model[t_sales_units>0&novelty_sum%in%1])),
  	                                         nov3 = length(unique(model[t_sales_units>0&novelty_sum%in%1:3])),
  	                                         nov6 = length(unique(model[t_sales_units>0&novelty_sum%in%1:6])),
    	                                      nov12 = length(unique(model[t_sales_units>0&novelty_sum%in%1:12])),
    	                                      noofbrands_orig=length(unique(brand_orig))
  	                                
  	                                 ),
  	by=aggkey_iter]
  
  	# Add attributes
    attrdata=lapply(grep('^attr', colnames(tmp),value=T), function(var) {
      #w_type_overwrite = w_type
      #if (all(unlist(tmp[,var,with=F])%in%0:1)) w_type_overwrite='arithmetic'
      w_type_overwrite = 'arithmetic'
      rtmp=tmp[, list(outcomevar=weigh_by_w(get(var), t_sales_units_rolled, type = w_type_overwrite),
                      outcomevar2=weigh_by_w(get(var), t_sales_units_rolled_incl, type = w_type_overwrite),
                      outcomevar3=weigh_by_w(get(var), t_noweights, type = w_type_overwrite)), by = aggkey_iter]
      setnames(rtmp, 'outcomevar', var)
      setnames(rtmp, 'outcomevar2', gsub('^attr', 'cpsattr', var))
      setnames(rtmp, 'outcomevar3', gsub('^attr', 'nwattr', var))
      
      rtmp
    })
    
    merge.all <- function(x,y, ...) {merge(x,y, all.x=T,all.y=T, by=aggkey_iter, ...)}
    attrdata_merged=Reduce(merge.all, attrdata)
    
    merged_attr_sales = merge(merged_attr_sales, attrdata_merged, by=aggkey_iter, all.x=T)
    
    if (!'brand'%in%aggkey_iter) {
      llen_by_brand = tmp[, list(llen=length(unique(paste(brand_orig, model)[t_sales_units>0])),
                                 t_sales_units = sum(t_sales_units,na.rm=T),
                                 t_sales_units_rolled=sum(t_sales_units_rolled,na.rm=T)), by = aggkey]
      llen_agg = llen_by_brand[, list(wsllen=weigh_by_w(llen, t_sales_units, na.rm=T, type=w_type),
                                      wpsllen=weigh_by_w(llen, t_sales_units_rolled, na.rm=T, type=w_type),
                                      noofbrands=length(unique(brand[which(t_sales_units>0)]))), by = aggkey_agg]
      merged_attr_sales = merge(merged_attr_sales, llen_agg, by=aggkey_iter, all.x=T)
      
    }
  	# transform novelty variables to shares; if llen = 0, set novelty share to 0.
  	novvars <- grep('nov[0-9].*', colnames(merged_attr_sales),value=T)
  	for (.var in novvars) merged_attr_sales[, (paste0(.var,'sh')) := ifelse(llen==0, 0, (get(.var)/llen)*100)]
  	
  	# The novelty data is censored at the beginning of a category's observation period, except in 
  	# the tablets category (this is monitored at the start of the category)
  	tmp_catdates <- merged_attr_sales[,list(sumsales=sum(usales)),by=c('market_id','date')][sumsales>0, list(date=unique(date)), by='market_id']
  	setorder(tmp_catdates, market_id,date)
  	tmp_catdates[, Ncens:=1:.N, by = c('market_id')]
  	
  	setkey(tmp_catdates, market_id, date)
  	setkey(merged_attr_sales, market_id, date)
  	merged_attr_sales[tmp_catdates, Ncens:=i.Ncens]
  	
  	catname=names(skus_by_date_list)[i]
  	
  	for (lags in c(1,3,6,12)) merged_attr_sales[Ncens%in%1:lags&!catname=='tablets', (paste0('nov', lags)):=-999]
  
  	# Add indicators for which category-date observations should be included
    	setkey(merged_attr_sales, category, country, date)
    	merged_attr_sales[time_selection, selected_t_cat := i.selected_t_cat]
  	
  	# Merge CPIs
  		cpi=indicators[type=='cpi']
  		setkey(cpi, date, country)
  	
  		stopifnot(!any(is.na(merged_attr_sales$selected_brand)))
  		
  		setkey(merged_attr_sales, date, country)
  		merged_attr_sales[cpi, cpi:=i.value]
  		
  		
  	# Convert some columns to factors
  		if ('brand'%in%colnames(merged_attr_sales)) {
  		  merged_attr_sales[, ':=' (brand=as.factor(brand), country=as.factor(country), category=as.factor(category))]
  		
    	# Sort the data
  		  setorder(merged_attr_sales,country,brand,date)
  		} else {
  		  merged_attr_sales[, ':=' (country=as.factor(country), category=as.factor(category))]
  		  
  		  # Sort the data
  		  setorder(merged_attr_sales,country,date)
  		}
  		
  
  	##############################################################
  	# Interpolation of missings in the case of brand-level data  #
  	##############################################################
  		
  	if ('brand' %in% colnames(merged_attr_sales)) {
  		
  		# define columns to interpolate if missings occur inbetween subsequent observations
  		all_cols=colnames(merged_attr_sales)
  		interp_cols = all_cols[!all_cols%in%c('category', 'country', 'market_id', 'brand', 'date', 'selected_t_cat', 'selected_brand')]
  		
  		# set some columns to NA before interpolating (e.g., negative prices - they do not end up occuring in the data, btw...)
  		for (.var in grep('spr', all_cols,value=T)) {
  		  lneg = nrow(merged_attr_sales[which(get(.var)<=0)])
  		  if (lneg>0) {
  		    sink('../audit/log.txt',append=T)
  		    report_tmp=merged_attr_sales[which(get(.var)<=0), list(.N, min = min(get(.var)), max=max(get(.var))),by=c('category','country','brand')]
  		    for (r in seq(from=1, length.out=nrow(report_tmp))) {
  		      cat(paste0(.var, ': negative or zero in market ', i, ', brand = ', report_tmp[r]$brand, ', country = ', report_tmp[r]$country,', category = ', report_tmp[r]$category,' (min = ',report_tmp[r]$min,', max=',report_tmp[r]$max,')!\n'))
  		    }
  		    
  		    sink()
  		  
  		  }
  		  
  		  eval(parse(text=paste0('merged_attr_sales[which(', .var,'<=0), ', .var, ':=NA]')))
  		}
  		
  		# set sales columns to NA if they are below 0.
  		for (.var in grep('sales$|salesd$|dst$', all_cols, value=T)) {
  		  
  		  lneg = nrow(merged_attr_sales[which(get(.var)<0)])
  		  if (lneg>0) {
  		    sink('../audit/log.txt',append=T)
  		    report_tmp=merged_attr_sales[which(get(.var)<0), list(.N, min = min(get(.var)), max=max(get(.var))),by=c('category','country','brand')]
  		    for (r in seq(from=1, length.out=nrow(report_tmp))) {
  		      cat(paste0(.var, ': negative in market ', i, ', brand = ', report_tmp[r]$brand, ', country = ', report_tmp[r]$country,', category = ', report_tmp[r]$category,' (min = ',report_tmp[r]$min,', max=',report_tmp[r]$max,')!\n'))
  		    }
  		    
  		    sink()
  		    
  		  }
  		  
  		  eval(parse(text=paste0('merged_attr_sales[, ', .var, ' := ifelse(', .var, '<0, NA, ', .var,')]')))
  		}
  		
  		setorder(merged_attr_sales,market_id,category,country,brand,date)
  		
  		# interpolate variables
  		dframe_interp<-cbind(merged_attr_sales[, c('date', 'country', 'category','selected_t_cat', 'selected_brand'),with=F], 
  		                     merged_attr_sales[, lapply(.SD, nafill),by=c('category','country','market_id', 'brand'),.SDcols=interp_cols])
  		
  		dframe_noninterp <- merged_attr_sales[,colnames(dframe_interp),with=F]
  		
  		# compare whether values were interpolated, or not
  		unequal = rowSums(!is.na(dframe_interp))-rowSums(!is.na(dframe_noninterp))
  		# add an indicator which rows contain at least one interpolated value
  		dframe_interp[, interpolated:= !unequal==0]
  		
  		lneg = sum(dframe_interp$interpolated,na.rm=T)
  		if (lneg>0) {
  		  sink('../audit/log.txt',append=T)
  		  report_tmp=dframe_interp[which(interpolated==T), list(interp=sum(interpolated)),by=c('category','country','brand')]
  		  for (r in seq(from=1, length.out=nrow(report_tmp))) {
  		    cat(paste0('interpolated in market ', i, ', brand = ', report_tmp[r]$brand, ', country = ', report_tmp[r]$country,', category = ', report_tmp[r]$category,' (interpolated N = ', report_tmp[r]$interp,')!\n'))
  		  }
  		  
  		  sink()
  		  
  		}
  		
  		# add market share metrics
  		dframe_interp[,':=' (usalessh = as.numeric(usales/sum(usales,na.rm=T)),
  		                     vsalessh = as.numeric(vsales/sum(vsales,na.rm=T))), by=c('country', 'date')]
  		
  		all_data[[i]]$data=split(dframe_interp, dframe_interp$country)
  		all_data[[i]]$noninterp=dframe_noninterp
  		all_data[[i]]$interp=dframe_interp
  		
  	} else {
  	  all_data[[i]]$data_agg=split(merged_attr_sales, merged_attr_sales$country)
  	  all_data[[i]]$noninterp_agg=merged_attr_sales
  	  
  	}
  	
  		
  	
    }
    
  # remove objects
  		rm(merged_attr_sales,skus_by_date)
  		
  		# clear memory
  		gc()
  		cat('\n')
  }
  	
  	# rename
  	names(all_data)<-names(skus_by_date_list)
  	
  	
  	# generate summary statistics about interpolated variables (i.e., how many missings remain after linear interpolation)
  	for (i in seq(along=all_data)) {
  	  print(i)
  	  
  	if (is.null(all_data[[i]])) next
  	  
  	varnames = intersect(colnames(all_data[[i]]$interp), colnames(all_data[[i]]$noninterp))
  	tmp=lapply(varnames, function(var) {
  	  unlist(all_data[[i]]$interp[, var,with=F])==unlist(all_data[[i]]$noninterp[, var,with=F])
  	})
  	tmp=data.table(do.call('cbind',tmp))
  	setnames(tmp,varnames)
  	print(table(rowSums(tmp,na.rm=T)))
  	
  	#tmp[rowSums(tmp,na.rm=T)<ncol(tmp),]
  	
  	
  	}
  
  	
  	
  	# Initialize count variables to assign unique IDs to brands and markets
  	cnt_brand=0
  	cnt_market=0
  	
  	# prepare final (cleaned) data sets / longest consecutive stretch selection
  	for (i in 1:length(all_data)) {
  	  if (is.null(all_data[[i]])) next
  	  print(i)
  	  
  	  cat('Flagging missing observations per brand\n')
  	  all_data[[i]]$data_cleaned <- NULL
  	  
  	  for(j in seq(along=all_data[[i]]$data)) {
  	    # Get data
  	  
  	    for (it in c('data', 'data_agg')) {

  	      if (it=='data') {
  	        paneldata = all_data[[i]]$data[[j]]
  	        } else {
  	        paneldata=all_data[[i]]$data_agg[[j]]

  	        paneldata[selected_brand==T, brand:='aggregated']
  	        paneldata[selected_brand==F, brand:='aggregated-notselected']
  	        
  	        }
  	      
    	    # Correct monetary variables with a country's CPI
  	       .loop_vars = c('vsales',
  	         'wspr','wpspr','wcpspr','nwpr',
  	         'wsprd','wpsprd','wcpsprd','nwprd',
  	         'adv')
  	       for (.l in .loop_vars) panel[, paste0('r',.l):=get(.l)/cpi]
  	       
    	    # Investigate which part of the data set is complete and can be used for model estimation
    	    tmp <- split(paneldata, as.character(paneldata$brand))
    	    
    	    tmp <- lapply(tmp, function(dframe) {
    	      keyvars = c('market_id', 'category', 'country','brand','date')
    	      
    	      # include 0-sales periods inbetween non-zero sales				
    	      if(0) {
    	        dframe[, usales0:=usales]
    	        dframe[1:.N<first(which(usales>0)), usales0:=NA]
    	        dframe[1:.N>last(which(usales>0)), usales0:=NA]
    	      }
    	      
    	      #if(0){
    	      # exclude 0-sales periods inbetween non-zero sales				
    	      dframe[, usales0:=usales]
    	      dframe[usales==0, usales0:=NA]
    	      #}
    	      
    	      all_cols=colnames(dframe)
    	      
    	      .availabilitycheck1 = setdiff(all_cols,c(keyvars, 'cpi', 'interpolated', 'selected_t_cat', 'selected_brand'))
    	      .availabilitycheck2 = NULL 
    	      
    	      # determine max consecutive observations; disregard (potentially incomplete) advertising data here
    	      .zoo = zoo(dframe[, -which(colnames(dframe)%in%'adv'),with=F])
    	      
    	      .excl <- NULL
    	      
    	      .out = try(na.contiguous(.zoo),silent=T)
    	      
    	      if (class(.out)=='try-error') {
    	        if (grepl('all times contain an NA', .out[1])) { 
    	          
    	          # check out which time series has only NAs, rerun
    	          .excl = names(which(colSums(is.na(.zoo))==length(unique(dframe$date))))
    	          .zoo = zoo(dframe[, setdiff(c(keyvars, .availabilitycheck1, .availabilitycheck2), .excl),with=F])
    	          .out = try(na.contiguous(.zoo),silent=T)
    	        }
    	      }
    	      
    	      .zoo = dframe
    	      
    	      # tag longest consecutive stretch
    	      suppressWarnings(.zoo[, selected_t_brand:=!1:.N %in% as.numeric(attr(.out, 'na.action'))])
    	      .zoo <- .zoo[, c(keyvars, .availabilitycheck1, .availabilitycheck2, 'selected_t_brand', 'selected_t_cat', 'selected_brand'),with=F]
    	      .zoo[, usales0:=NULL]
    	      return(.zoo)
    	    })
    	    
    	    .zoo <- rbindlist(tmp)
    	    
    	    # clean category/country/brand names
    	    if (nrow(.zoo)>0){
    	      .zoo[, category:=as.character(gsub('[-]|[/]','', category))]
    	      .zoo[, brand:=as.character(gsub(' |[-]|[/]|[(]|[)]|[.]|[&]','', brand))]
    	      .zoo[, ':=' (category=tolower(category),country=tolower(country),brand=tolower(brand))]
    	      
    	      #.zoo[which(!is.na(usales) & selected_t_brand==T & selected_brand == T & !selected_t_cat %in% c(NA, F)), selected:=T, by=c('category', 'country', 'brand')]
    			#.zoo[is.na(selected), selected:=F, by=c('category', 'country', 'brand')]
    			
    		}
    		
    		novvars= grep('nov[0-9]+', colnames(.zoo),value=T)
    		for (.var in novvars) .zoo[get(.var)==-999, (.var):=NA]
    	
    	if (it=='data_agg') all_data[[i]]$data_cleaned_agg[[j]] <- .zoo else all_data[[i]]$data_cleaned[[j]] <- .zoo
    		
  	    }
  	  }
  	  
  	}	
  	detach(selection[[selrule]])
  	
  	all_datasets[[selrule]]<-all_data
  
 
}

names(all_datasets) <- names(selection)

save(all_datasets, file='../temp/brand_metrics.RData')	

