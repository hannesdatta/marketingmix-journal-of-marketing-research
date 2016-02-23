stop('deprecated')
		
			
		
		#################### DEPRECATED ##############################
		
		
		
		
		
		
		
		
		
		
# Plots by market


			
			
			
	
	# Phone call with my student Panos:
		# there are duplicates because for some products, manufacturers have special agreements with stores (e.g., to stock specific items in a particular month).
		# in such circumstances, there will be duplicate rows.
		
		# The question is: are there duplicates, at all?!
		# Panos: the thing that i see here is that they have different introduction date to the market so either is one way promotion ( i see that the weighted distribution is higher in the low price product) 
		# or it is different introduction phase.


# Objectives: 
# - get the product attributes right (i.e., unique attributes by SKU (most likely the best selling SKU)
	
			# filter on observations-to-be-used (e.g., Gfk 2012 vs. Gfk 2015 selection)
			datlist_final[[i]] <- datlist_final[[i]][used==T]
			datlist_final[[i]] <- datlist_final[[i]][,used:=NULL]
	
if (0) {
# Asserts:
# TV
models1 = datlist_final$tv_gen1_crtv[, list(N_crtv=.N),by=c('country', 'brand','model')]
models2 = datlist_final$tv_gen2_lcd[, list(N_lcdnoled=.N),by=c('country', 'brand','model')]
models3 = datlist_final$tv_gen3_lcdwithled[, list(N_lcdled=.N),by=c('country', 'brand','model')]
models4 = datlist_final$tv_gen3_plasma[, list(N_plasma=.N),by=c('country', 'brand','model')]

all_tv_models <- merge(models1, models2, all.x=T, all.y=T, by=c('country','brand','model'))
all_tv_models <- merge(all_tv_models, models3, all.x=T, all.y=T, by=c('country','brand','model'))
all_tv_models <- merge(all_tv_models, models4, all.x=T, all.y=T, by=c('country','brand','model'))
# now, only one product overlaps

#with(all_tv_models, table(!is.na(N_crtv),


datlist_final$tv_gen1_crtv = rbindlist(list(datlist_by_cat$'CRT-TV'[REPORTINGPRODUCTGROUP=='CRT-TV']),fill=T)
	datlist_final$tv_gen1_crtv[, used := source == 'gfk2015']
	datlist_final$tv_gen2_lcd = rbindlist(list(datlist_by_cat$'LCD-TV W/O LED'[REPORTINGPRODUCTGROUP=='LCD-TV W/O LED']),fill=T)
	datlist_final$tv_gen2_lcd[, used := source == 'gfk2015']
	datlist_final$tv_gen3_lcdwithled = rbindlist(list(datlist_by_cat$'LCD-TV WITH LED'[REPORTINGPRODUCTGROUP=='LCD-TV WITH LED']),fill=T)
	datlist_final$tv_gen3_lcdwithled[, used := source == 'gfk2015']
	datlist_final$tv_gen3_plasma = rbindlist(list(datlist_by_cat$'PTV/FLAT'[REPORTINGPRODUCTGROUP=='PTV/FLAT']),fill=T)
	datlist_final$tv_gen3_plasma[, used := source == 'gfk2015']
}

if (0) {
# Check for unmatched products
	match_columns <- c('REPORTINGPRODUCTGROUP','Brand', 'Model')
	raw_unmatched=rbindlist(lapply(datlist_by_cat, function(x) x[,list(N=.N),by=match_columns]))
	raw_unmatched=raw_unmatched[, list(N=sum(N)),by=match_columns]

	matched=rbindlist(lapply(datlist_final, function(x) x[,list(N=.N),by=c(match_columns, 'catname')]))
	setnames(matched, 'N', 'match_present')

	complmatch <- merge(raw_unmatched,matched,by=match_columns,all.x=T,all.y=T, allow.cartesian=T)


# check out... where we have duplicates
	# unmatched?
	complmatch[is.na(match_present)]

	complmatch[, duplicates:=.N>1,by=c('REPORTINGPRODUCTGROUP', 'Brand', 'Model')]

# Check duplicate models and brands between tablets and laptops; delete duplicates froem Laptops
	verify=complmatch[duplicates==T]
	head(verify,50)

	datlist_final$laptop<-datlist_final$laptop[!Model %in% unique(verify$Model)]

# If there are duplicates between smartphones and mobile phones, kick phones out of mobile phone table
	datlist_final$phones_mobile<-datlist_final$phones_mobile[!Model %in% unique(verify$Model)]

# Everything matched! (though verify this here... again...)
	verify=complmatch[!N==match_present][order(Brand,  Model, catname)]
	head(verify,50)
	}
	
