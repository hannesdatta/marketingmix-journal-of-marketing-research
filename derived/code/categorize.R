
#    _____                       _                  _                      _                                   _              
#   |  __ \                     | |                | |                    | |                                 (_)             
#   | |__) |  _ __    ___     __| |  _   _    ___  | |_      ___    __ _  | |_    ___    __ _    ___    _ __   _    ___   ___ 
#   |  ___/  | '__|  / _ \   / _` | | | | |  / __| | __|    / __|  / _` | | __|  / _ \  / _` |  / _ \  | '__| | |  / _ \ / __|
#   | |      | |    | (_) | | (_| | | |_| | | (__  | |_    | (__  | (_| | | |_  |  __/ | (_| | | (_) | | |    | | |  __/ \__ \
#   |_|      |_|     \___/   \__,_|  \__,_|  \___|  \__|    \___|  \__,_|  \__|  \___|  \__, |  \___/  |_|    |_|  \___| |___/
#                                                                                        __/ |                                
#                                                                                       |___/                                 

require(data.table)
load('../temp/unzipped.RData')

### STEP 1: Compute overlap per file and category; decide where to get the data from (2012, 2015, or both data dumps)

###################################
# Identify problematic categories #
###################################	

	# Data ranges per file
	cnt=0
	out=rbindlist(lapply(datlist, function(x) {
		cnt<<-cnt+1
		return(data.frame(fileid=cnt,category=unique(x$REPORTINGPRODUCTGROUP), country=unique(x$COUNTRY),source=unique(x$source), period_min=min(x$date), period_max = max(x$date)))
		}
		))

	out <- out[order(category,country,source)]
	out[, same_start := length(unique(period_min))==1, by=c('category','country')]
	out[, periods := difftime(period_max,period_min,units='weeks')]

	out[order(periods, decreasing=F)]
	
	print(out[same_start==F])

#######################################
# Sort data sets by product category  #
#######################################

# Merge datasets (of countries) for each product category

	# Get each main category (i.e., every country)
	dat_types = data.table(do.call('rbind', lapply(1:(length(datlist)), function(x) 
							cbind(index=x, category=unique(as.character(datlist[[x]]$REPORTINGPRODUCTGROUP)),
								  country=as.character(datlist[[x]]$COUNTRY)[1],
								  subcategory=as.character(unique(datlist[[x]]$CATEGORY))))
								  ), 
								  key=c('index'))[order(category,country)]
	setkey(dat_types, index)
	
	print(dat_types[duplicated(dat_types)])
	
	print(t(with(dat_types, table(country, category))))

	catsplit <- lapply(split(dat_types, dat_types$category), function(x) unique(as.numeric(x$index)))
	catsplit$'DESK COMPUT' <- c(catsplit$'DESK COMPUT', catsplit$'DESK COMPUT NZ')
	catsplit$'DESK COMPUT NZ'<-NULL
	
	datlist_by_cat <- lapply(1:length(catsplit), function(x) {
		res=rbindlist(datlist[catsplit[[x]]],fill=T)
		res=res[REPORTINGPRODUCTGROUP %in% names(catsplit)[x]]
		return(res)
		})
	names(datlist_by_cat) <- names(catsplit)
	
#	rm(catsplit)
	gc()
	# -> datlist_by_cat contains all data for each main category

	
#####################################################################
# Investigate how data sets from raw category names can be combined #
#####################################################################
	
	#########################
	# Notebooks and tablets #
	#########################
	
	# The data for notebooks is spread across two raw categories, originally called
	# 'tablets + notebook', and 'mobile computing'.
	
	# Here, I check for an overlap between these categories to assess how they can be combined.
	
	datlist_by_cat$'MOBILE COMPUTING'[, ':=' (cat='mobilecomp', subcat = DESIGN)]
	datlist_by_cat$'TABLET+NOTEBOOK'[, ':=' (cat='tablets', subcat = ProductGroups)]

	tmp = rbindlist(list(datlist_by_cat$'MOBILE COMPUTING',datlist_by_cat$'TABLET+NOTEBOOK'),fill=T)

	# aggregate unit sales
	tmp2 = tmp[, list(sales=sum(SALES_UNITS)),by=c('cat', 'subcat', 'COUNTRY','source')]
	dcast(COUNTRY~cat+subcat, value.vars='sales',data=tmp2[source=='gfk2015'])
	
	# table suggests that mobilecomp_TABLET and tablets_COMPUTERS TABLET are the same.
	# we can verify this by comparing models contained in both data sets.
	
	models1=data.table(tmp[source=='gfk2015'&cat=='mobilecomp'&subcat=='TABLET'][,list(N=.N),by=c('Brand','Model')])
	models2=data.table(tmp[source=='gfk2015'&cat=='tablets'&subcat=='COMPUTERS TABLET'][,list(N2=.N),by=c('Brand','Model')])
		
	cbn=merge(models1,models2,by=c('Brand','Model'),all.x=T,all.y=T)
	cbn[is.na(N)]
	cbn[is.na(N2)]
	
	# -> overlap is the same; both tablets contain the same models
	
	# --> mobile computing with design = 'TABLETS', and tablet+notebook with product group 'COMPUTERS TABLET' 
	# are exactly the same.
	
	# what kind of products are these?
	setorder(cbn, N)
	cbn[Brand=='SAMSUNG']
	# --> convertible laptops / hybrids (e.g., Acer Iconia W510, Microsoft Surface Pro)
		
	
	# We have sorted out two sub-categories; we're left with mobilecomp_NOTEBOOK and tablet_OTHER.
  # compute overlap by model
	
	models1=data.table(tmp[source=='gfk2015'&cat=='mobilecomp'&subcat=='NOTEBOOK'][,list(N=.N, sales1=sum(SALES_UNITS)),by=c('Brand','Model')])
	models2=data.table(tmp[source=='gfk2015'&cat=='tablets'&subcat=='OTHERS'][,list(N2=.N, sales2=sum(SALES_UNITS)),by=c('Brand','Model')])
	
	cbn=merge(models1,models2,by=c('Brand','Model'),all.x=T,all.y=T)
	nrow(cbn[!is.na(N)&!is.na(N2)])/nrow(cbn) # complete, these are real laptops
	nrow(cbn[is.na(N)&!is.na(N2)])/nrow(cbn) # mobilecomp missing (originally from the tablets-others category, these are Chromebooks)
	nrow(cbn[!is.na(N)&is.na(N2)])/nrow(cbn) # tablets missing --> these are real laptops, e.g., Acer Aspire One 533 or Netbooks, Verzio Truebook N50
	nrow(cbn[is.na(N)&is.na(N2)])/nrow(cbn) # both missing

	# --> conclusion: essentially, the remaining two subcategories from the raw categories also contain
	# laptops/notebooks.
	
	# I suggest to combine them for simplicity of write-up. However, the overlap is not
	# perfect.
	
	# Are sales the same when they DO overlap?
	with(cbn[!is.na(N)&!is.na(N2)], table(sales1==sales2))
	
	models1=data.table(tmp[source=='gfk2015'&cat=='mobilecomp'&subcat=='NOTEBOOK'][,list(N=.N, sales1=sum(SALES_UNITS)),by=c('Brand','Model', 'COUNTRY','date')])
	models2=data.table(tmp[source=='gfk2015'&cat=='tablets'&subcat=='OTHERS'][,list(N2=.N, sales2=sum(SALES_UNITS)),by=c('Brand','Model','COUNTRY', 'date')])
	
	cbn=merge(models1,models2,by=c('Brand','Model', 'COUNTRY', 'date'),all.x=T,all.y=T)
	
	cbn[!is.na(N)&!is.na(N2)][!sales1==sales2]
	
	# doesn't hold up for only a very low number of products
	cbn[!is.na(N)&!is.na(N2)][sales1>sales2]
	# in both cases, sales1 is higher.
	
	# check for duplicates: 2012-10-01 Singapore ASPIRE ONE 725
	tmp[source=='gfk2015'&cat=='mobilecomp'&subcat=='NOTEBOOK'][Model=='ASPIRE ONE 725'&COUNTRY=='SINGAPORE'&date=='2012-10-01']
	
	# Are sales the same when they DO overlap?
	nrow(cbn[!is.na(N)&!is.na(N2)][sales1>sales2])/nrow(cbn)
	nrow(cbn[!is.na(N)&!is.na(N2)][!sales1==sales2])/nrow(cbn) # --> take source 1: mobilecomp subcat notebook (has the higher sales data)
	
	# combine:
	# if present in both: take mobilecomp/notebook (higher sales - prob more SKUs and/or stores in here)
	# add non-overlapping ones from both data sets
	

#####################################################
# Create new category classifications and define    #
# from which data sets to get the sales data        #
#####################################################

# plot overlap
require(lattice)

plotfkt<-function(dt) {
  tmp=dt[, list(sales=sum(SALES_UNITS)),by=c('COUNTRY','date','source')]
  setorder(tmp,COUNTRY,date,source)
  pl<-xyplot(sales~date|COUNTRY,groups=source,type='l',data=tmp, scales=list(y='free'))
  print(pl)
}


	names(datlist_by_cat)
	
	datlist_final = NULL

# (1) Desktop PC
	datlist_final$desktoppc = rbindlist(list(datlist_by_cat$'DESK COMPUT'),fill=T)
	datlist_final$desktoppc[, used := source == 'gfk2015' & !COUNTRY %in% c('INDIA','CHINA')]
	
	plotfkt(datlist_final$desktoppc)
	
	
# (2) Laptops
	datlist_final$laptop_regular = rbindlist(list(
	  overlap_from_both=datlist_by_cat$'MOBILE COMPUTING'[DESIGN=='NOTEBOOK'][Model%in%unique(datlist_by_cat$'TABLET+NOTEBOOK'[ProductGroups=='OTHERS']$Model)],
	  uniques_from_mobilecomp=datlist_by_cat$'MOBILE COMPUTING'[DESIGN=='NOTEBOOK'][!Model%in%unique(datlist_by_cat$'TABLET+NOTEBOOK'[ProductGroups=='OTHERS']$Model)],
	  uniques_from_tabletnotebook=datlist_by_cat$'TABLET+NOTEBOOK'[ProductGroups=='OTHERS'][!Model%in%unique(datlist_by_cat$'MOBILE COMPUTING'[DESIGN=='NOTEBOOK']$Model)]
	),fill=T)
	
  datlist_final$laptop_hybrids = rbindlist(list(datlist_by_cat$'MOBILE COMPUTING'[DESIGN=='TABLET']),fill=T)
		# (which is exactly same as the category Notebook+Tablets, ProductGroup 'TABLET'.

	# merge laptops and hybrid laptops
	datlist_final$laptop <- rbindlist(list(datlist_final$laptop_regular, datlist_final$laptop_hybrid), fill=T)
	# -> sales series of india and china don't overlap well; exclude.
	
	datlist_final$laptop[, used := source == 'gfk2015' & !COUNTRY %in% c('CHINA','INDIA')]
	
	plotfkt(datlist_final$laptop)
	
	datlist_final$laptop_regular <- NULL
	datlist_final$laptop_hybrids <- NULL

# (3) Tablets
	datlist_final$tablets_only = rbindlist(list(#datlist_by_cat$'MOBILE COMPUTING'[DESIGN=='TABLET'], 
										   #datlist_by_cat$'TABLET+NOTEBOOK'[ProductGroups=='COMPUTERS TABLET'],
										   #datlist_by_cat$'WEBBOOKS',
										   datlist_by_cat$'MEDIATABLETS'),fill=T)
	
	datlist_final$tablets_only[, used := source == 'gfk2015']
	
	plotfkt(datlist_final$tablets_only)
	
	datlist_final$tablets_phablets = rbindlist(list(datlist_by_cat$'SMART+MOBILEPHONES'[MOB_SMP=='PHABLET']),fill=T)
	datlist_final$tablets_phablets[, used := source == 'gfk2015']

	plotfkt(datlist_final$tablets_phablets)
	
	datlist_final$tablets <- rbindlist(list(datlist_final$tablets_phablets, datlist_final$tablets_only),fill=T)
	plotfkt(datlist_final$tablets)
	
	datlist_final$tablets_phablets <- NULL
	datlist_final$tablets_only <- NULL
	
# (4) Smartphones	
	
	datlist_final$phones_smart = rbindlist(list(datlist_by_cat$'SMART+MOBILEPHONES'[MOB_SMP=='SMARTPHONE']),fill=T)	
	datlist_final$phones_smart[, used := source == 'gfk2015']
	plotfkt(datlist_final$phones_smart)
	
# (5) Mobile phones
	datlist_final$phones_mobile = rbindlist(list(datlist_by_cat$'SMART+MOBILEPHONES'[MOB_SMP=='MOBILEPHONE']),fill=T)
	#datlist_final$phones_mobile[, used := source == 'gfk2015' & !COUNTRY %in% c('INDIA')]

	datlist_final$phones_mobile[, used := source == 'gfk2015']
	
	plotfkt(datlist_final$phones_mobile)
	
# (6) SLR Cameras	
	datlist_final$camera_slr = rbindlist(list(datlist_by_cat$'DIGITAL CAMERAS'[CAMERA_TYPE%in%c('SLR')]),fill=T)
	datlist_final$camera_slr[, used := source == 'gfk2015']
	plotfkt(datlist_final$camera_slr)
	
# (7) Compact cameras
	datlist_final$camera_compact = rbindlist(list(datlist_by_cat$'DIGITAL CAMERAS'[CAMERA_TYPE%in%c('OTHERS','COMPACT')]),fill=T)
	datlist_final$camera_compact[, used := source == 'gfk2015']
	plotfkt(datlist_final$camera_compact)
	
# (8) DVD
	datlist_final$dvd = rbindlist(list(datlist_by_cat$'VIDEO PLAYER/REC'),fill=T)
	datlist_final$dvd[, used := source == 'gfk2015']
	plotfkt(datlist_final$dvd)
	
# (9) Microwave
	datlist_final$microwave = rbindlist(list(datlist_by_cat$'MICROWAVE OVENS'),fill=T)
	datlist_final$microwave[, used := source == 'gfk2015']
	plotfkt(datlist_final$microwave)
	
# (10) Refrigerators
	datlist_final$cooling = rbindlist(list(datlist_by_cat$COOLING),fill=T)
	datlist_final$cooling[, used := source == 'gfk2015']
	plotfkt(datlist_final$cooling)
	
# (11) TV (first generation)
	datlist_final$tv_gen1_crtv = rbindlist(list(datlist_by_cat$'CRT-TV'),fill=T)
	datlist_final$tv_gen1_crtv[, used := source == 'gfk2015']
	plotfkt(datlist_final$tv_gen1_crtv)
	
# (12) TV (second generation)
	datlist_final$tv_gen2_lcd = rbindlist(list(datlist_by_cat$'LCD-TV W/O LED',datlist_by_cat$'LCD-TV WITH LED',datlist_by_cat$'PTV/FLAT'),fill=T)
	datlist_final$tv_gen2_lcd[, used := source == 'gfk2015']
	
	plotfkt(datlist_final$tv_gen2_lcd)
	
	# investigate plasma vs other TVs
	if(0){
	LCD=rbindlist(list(datlist_by_cat$'LCD-TV W/O LED',datlist_by_cat$'LCD-TV WITH LED'), fill=T)
	PLASMA=rbindlist(list(datlist_by_cat$'PTV/FLAT'))
	
	LCD=LCD[source=='gfk2015']
	PLASMA=PLASMA[source=='gfk2015']
	
	tmp=LCD[, list(sales=sum(SALES_UNITS)),by=c('COUNTRY','date')]
	tmp2=PLASMA[, list(sales=sum(SALES_UNITS)),by=c('COUNTRY','date')]
	setorder(tmp,COUNTRY,date)
	setorder(tmp2,COUNTRY,date)
	
	require(lattice)
	xyplot(sales~date|COUNTRY,tmp,type='l',scales=list(y='free'),main='LCD')
	xyplot(sales~date|COUNTRY,tmp2,type='l',scales=list(y='free'), main='PLASMA')
	
	# overlap / duplicates?
	PLASMA[COUNTRY=='CHINA'&Brand=='PIONEER'] #?
	# reportinggroup PTV/FLAT also contains LCD TVs
	table(PLASMA[COUNTRY=='CHINA'&Brand=='PIONEER']$Model%in%LCD[COUNTRY=='CHINA'&Brand=='PIONEER'])
	}
	
# (13) Washing machines
	datlist_final$washing = rbindlist(list(datlist_by_cat$'WASHINGMACHINES'),fill=T)
	datlist_final$washing[, used := source == 'gfk2015']
	plotfkt(datlist_final$washing)
	

# (14) Miniovens
	datlist_final$minioven = rbindlist(list(datlist_by_cat$'MINI OVENS'),fill=T)
	datlist_final$minioven[, used := F] # too few countries
	plotfkt(datlist_final$minioven)
	
# (15) Tumbledryers
	datlist_final$tumbledryers = rbindlist(list(datlist_by_cat$'TUMBLEDRYERS'),fill=T)
	datlist_final$tumbledryers[, used := F] # too few countries
	plotfkt(datlist_final$tumbledryers)
	
rm(datlist_by_cat, datlist)
gc()

# Category overview:
	print(names(datlist_final))

# Compute earliest dates by data source: CATEGORY START DATES
	dir.create('../output/')
	sink('../output/category_start.txt')
	tmp=rbindlist(lapply(seq(along=datlist_final), function(x) datlist_final[[x]][, list(cat=names(datlist_final)[x], min=min(date), max=max(date)),by=c('source')]))
	print(data.frame(tmp))
	sink()

# verify obs in category/country we got
	sink('../output/category_selection.txt')
	tmp=rbindlist(lapply(seq(along=datlist_final), function(x) datlist_final[[x]][, list(cat=names(datlist_final)[x], used = any(used[source=='gfk2015']==T)),by=c('COUNTRY')]))
	tmp=tmp[!cat%in%c('minioven', 'tumbledryers')]
	
	cat('Categories excluded due to data errors:\n')
	print(data.frame(tmp[used==F]))
	
	tmp[, present:=1]
	tmp=melt(dcast(tmp, cat~COUNTRY,value.var='present'), id.vars=c('cat'))
	
	cat('Categories with no data from GfK:\n')
	print(data.frame(tmp[is.na(value)][, value:=NULL]))
	
	cat('Number of possible markets (Ncategories x Ncountries): ', nrow(tmp))
	
	sink()
	

# Process
for (i in 1:length(datlist_final)) {

	# assign category name
		category_name = names(datlist_final)[i]
		datlist_final[[i]][, catname := category_name]
		cat(paste0('Category ', i, ': ', category_name,'\n'))
	
	# make column names lower-space without special characters
		setnames(datlist_final[[i]], gsub('[/]', '', tolower(colnames(datlist_final[[i]]))))
	
	# remove duplicates rows (i.e., rows which are *exactly* the same as other rows)
		setkeyv(datlist_final[[i]], colnames(datlist_final[[i]]))
		datlist_final[[i]] <- unique(datlist_final[[i]])
	
	if (!category_name=='camera_compact' & !category_name=='camera_slr') {
		# remove spaces from model names, to make sure to count products such as PAVILIONG 3728CX NJ092AA and PAVILIONG3728CX NJ092AA as the same product.
		datlist_final[[i]][, model:=gsub(' ', '', model)]
		}

	# Correct model descriptions for some categories
		if (category_name=='camera_compact'|category_name=='camera_slr') {
			# For cameras, clean out the Objektiv-Statistik (e.g., 18-135); because
			# attribute composition changes from one year to another.
			
			trim <- function (x) gsub("^\\s+|\\s+$", "", x)
			
				# Aggregate remaining measures to the brand level
			weigh_by_sales <- function(x, w) {
				if (sum(w)==0) w = rep(1, length(x))
				sum(x*w)/sum(w)
				}
			datlist_final[[i]][, prev_model:=model]
			datlist_final[[i]][, model:=trim(gsub(' [0-9]+[-][0-9].*','',model))]
			datlist_final[[i]][, model:=trim(gsub(' [0-9]+[ ][/][ ][0-9].*','',model))]
			datlist_final[[i]][, model:=gsub(' ', '', model)]
		
			#datlist_final[[i]][grepl('^D',model)]
			#datlist_final[[i]][grepl('^D',model)&period=='JANUARY 2013']
			
			#datlist_final[[i]][, model:=gsub(' ', '', model)]
		
			# combine variables
			datlist_final[[i]] <- datlist_final[[i]][, list(
									  digital_zoom = digital_zoom[1], # take attribute of earliest model, because it's missing for the later one
									  price_lc = weigh_by_sales(as.numeric(price_lc), sales_units),
									  price_eur = weigh_by_sales(price_eur, sales_units),
									  price_usd = weigh_by_sales(price_usd, sales_units),
									  sales_units = sum(sales_units),
									  #snumeric_distribution = sum(numeric_distribution),
									  #sweighted_distribution = sum(weighted_distribution),
									  numeric_distribution = weigh_by_sales(numeric_distribution, sales_units),
									  weighted_distribution = weigh_by_sales(weighted_distribution, sales_units),
									  firstactivity=firstactivity[1]),
									  by=setdiff(colnames(datlist_final[[i]]), c('prev_model','digital_zoom', 'price_lc', 'price_eur', 'price_usd', 'sales_units', 'numeric_distribution', 'weighted_distribution', 'firstactivity'))]
			
			#summary(tmp$weighted_distribution)
			#summary(tmp$sweighted_distribution)
			#tmp[sweighted_distribution>200]
			
			## rule for distribution metrics: if the same prev model name: 
			
			#datlist_final[[i]]
			
			# kick out observations older than 2012-12-01 because of inability to compute distribution metrics starting Jan-13.
			#datlist_final[[i]] <- datlist_final[[i]][date<='2012-12-01']

			}
		if (category_name=='cooling') {
			# investigation: large peak of new SKUs for Fisher & Paykel in Australia
			#te=datlist_final[[i]][used==T&country=='AUSTRALIA'&brand=='FISHER&PAYKEL']
			#te=te[, list(llength=length(unique(model))),by=c('date', 'brand')][order(date)]
			#te2=te[date>='2012-07-01' & date<='2012-08-01', list(N=.N),by=c('date','model')][order(model,date)]
			#te2[, att:=any(date=='2012-07-01'),by=c('model')]
			# -> conclusion: this is real; there are really a large amount of new SKUs (on the micro level).
			
			# investigation: drop in wunique for india: ITs because just ONE brand during this period carries a freezer. Set to zero.
			#te=datlist_final[[i]][used==T&country=='INDIA'&brand=='WHIRLPOOL'][order(date,brand)]
			
			
			#te=te[, list(sales=sum(sales_units)),by=c('brand','date','model')]
			
			#te=te[, list(llength=length(unique(model))),by=c('date', 'brand')][order(date)]
			
			#te2=te[date>='2012-07-01' & date<='2012-08-01', list(N=.N),by=c('date','model')][order(model,date)]
			#te2[, att:=any(date=='2012-07-01'),by=c('model')]

			# investigation: oeak in novel for haier in china
			#te=datlist_final[[i]][used==T&country=='CHINA'&brand=='HAIER'&date>='2005-01-01'&date<='2008-01-01'][order(date,brand)]
			#te=te[, list(N=length(unique(model))),by=c('date', 'brand')]
			#te=te[, list(N=.N),by=c('firstactivity')]
			#te=te[, list(N=.N),by=c('date', 'brand','model')]
			#te=te[, list(N=.N, firstobs=date[1]),by=c('brand','model')][order(firstobs,model)]
			#te=te[, list(N=.N, firstobs=date[1]),by=c('brand','model')][order(model)]
			#te[, flag:=firstobs>='2006-01-01'&firstobs<='2006-06-01']
			
			# investigation: oeak in novel for polytron in indonesia
			if(0){
			te=datlist_final[[i]][used==T&country=='INDONESIA'&brand=='POLYTRON'&date>='2012-01-01'][order(date,brand)]
			te=te[, list(N=.N, firstobs=date[1]),by=c('brand','model')][order(model)]
			te[, flag:=firstobs>='2013-01-01']
			}
			
			
			
			#te=te[, list(sales=sum(sales_units)),by=c('brand','date','model')]
			
			#te=te[, list(llength=length(unique(model))),by=c('date', 'brand')][order(date)]
			
			#te2=te[date>='2012-07-01' & date<='2012-08-01', list(N=.N),by=c('date','model')][order(model,date)]
			#te2[, att:=any(date=='2012-07-01'),by=c('model')]

		}
		
		if (category_name=='desktoppc') {
			# investigation: amount of novel products in china
			if(0){
			te=datlist_final[[i]][used==T&country=='CHINA'][, list(N=.N),by=c('brand', 'model', 'date')][,list(nmodel=.N),by=c('brand', 'date')][order(brand,date)]
			plot(te[brand=='HP']$nmodel,type='l')
			
			# investigate names in detail
			te=datlist_final[[i]][used==T&country=='CHINA'][, list(N=.N),by=c('brand', 'model', 'date')][order(brand,date,model)]
			te[brand=='HP']
			# the amount of spaces between model name and model number (e.g., PAVILIONG 3728CX NJ092AA --> PAVILIONG3728CX NJ092AA) changes.
			# The solution is to remove spaces from model names.
			
			# I will do this for ALL product categories, just to be sure I get everything right...!
		
			# investigation: peak in novel for ACER
			#te=datlist_final[[i]][used==T&country=='JAPAN'&brand=='ACER'][order(date,brand)]
			#te=te[, list(N=.N, firstobs=date[1]),by=c('brand','model')][order(model)]
			#te[, flag:=firstobs>='2009-01-01']
			}
			
		# convert to numerics
		datlist_final[[i]][, ':=' (price_lc = as.double(price_lc), price_usd = as.double(price_usd), sales_units = as.double(sales_units))]

		
		}
				
	}

# Add market IDs	
cntr = 0 
for (i in 1:length(datlist_final)) {
	for (c in unique(datlist_final[[i]]$country)) {
		cntr = cntr+1
		datlist_final[[i]][country==c, market_id := cntr]
		}
	}
dir.create('../temp/')
save(datlist_final, file='..\\temp\\categorized.RData')
