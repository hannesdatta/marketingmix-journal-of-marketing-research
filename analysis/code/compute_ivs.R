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



### LOAD DATA SETS
require(data.table)

### Stack data in data.table
	brand_panel=fread('../external/datasets.csv')
	brand_panel[, ':=' (date = as.Date(date))]

	brand_panel <- brand_panel[selected==T]
	brand_panel[, usalessh := usales/sum(usales,na.rm=T), by=c('category', 'country', 'date')]
	brand_panel[, month_no := as.numeric(as.factor(date))]

	brand_panel[brand=='allothers', brand := paste0('allothers', .GRP), by = c('category', 'country')]
	
# Compute instrumental variables
	setup_x_iv=c("rwpsprd", "wpswdst", "llen", "nov3", "wpsun")
	
	# (1) same marketing instrument, but from other PRODUCT CLASSES in the same country (but not from the same brand)
	
	# Define product classes:
	# - Cameras (camera_slr, camera_compact)
	# - Computers (desktoppc, laptop)
	# - Phones and tablets (tablets, phones_smart, phones_mobile)
	# - TVs and DVDs (tv_gen1_crtv, tv_gen2_lcd, dvd)
	# - White goods (microwave, cooling, washing)
	
	brand_panel[grepl('camera', category), cat_class := 'cam']
	brand_panel[grepl('phones|tablets', category), cat_class := 'ph']
	brand_panel[grepl('desktoppc|laptop', category), cat_class := 'cp']
	brand_panel[grepl('tv[_]|dvd', category), cat_class := 'tvdvd']
	brand_panel[grepl('washing|cooling|microwave', category), cat_class := 'wte']
	
	# (2) same marketing instrument and category, but from different COUNTRY CLASSES
	
	# - Developed (Australia, Hong Kong, Japan, New Zealand, Singapore, South Korea, Taiwan)
	# - Developing (China, India, Indonesia, Malaysia, Philippines, Thailand, Vietnam)
	
	brand_panel[country %in% c('australia', 'hong kong', 'japan', 'new zealand', 'singapore', 'south korea', 'taiwan'), country_class := 'hinc']
	brand_panel[is.na(country_class), country_class := 'linc']
	
	# Summary:
	# Per marketing instrument, we have 4 instruments from non-focal product classes, and 1 instrument from non-focal country classes (= 5 instruments).
	# So for four mmix, we have a total of 20 instruments. The model is thus overidentified. Plus we have enough observations to estimate this.
	
	# create instruments
	calc_ivs <- function(x, same_key, different_key, exclude_brands = F) {		
		cat(paste0('Calculating instruments for ', x,', with same ', same_key, ' and different ', different_key, ', exclude = ', as.character(exclude_brands), '...\n'))
		# step-wise approach: calculate sum across everything by classification variable
		
		same_list = unique(unlist(c(brand_panel[,same_key,with=F])))
		different_list = unique(unlist(c(brand_panel[,different_key,with=F])))
		
		out=lapply(same_list, function(same_as) rbindlist(lapply(different_list, function(different_from) {
			#cat('new\n')
			#print(same_as)
			#print(different_from)
			# filter on same (e.g., same country)
			a=brand_panel[get(same_key) == same_as]
			
			# tag all brands that are present in the focal different key
			if (exclude_brands == T) a[, tag := any(get(different_key)==different_from), by = c('brand')]
			if (exclude_brands == F) a[, tag:=0]
			
			# kick out focal different key
			a=a[!get(different_key)==different_from]
			
			# kick out brands that were tagged
			a=a[tag==0]
			
			if(nrow(a)==0) return(NULL)
			
			# calculate mean
			tmp = a[, list(all_n = length(which(!is.na(get(x)))), tot_sum = sum(get(x), na.rm=T)), by=c('date', same_key, different_key)]
			tmp[, xmean := (tot_sum)/(all_n)]
		
			tmp2 = eval(parse(text=paste0('dcast(tmp, date ~ ', different_key, ', value.var=\'xmean\')')))
			tmp2[, (same_key) := same_as]
			tmp2[, (different_key) := different_from]
			tmp2[, variable_name := x]
			
			tmp2}), fill=T))
			
		out = rbindlist(out, fill=T)
		
		setcolorder(out, c('date', same_key, different_key, 'variable_name', different_list))
		
		out2 = melt(out, id.vars=c('date',same_key,different_key, 'variable_name'))
		out2[, iv_label := paste0('iv_', variable, '_', variable_name)]
		#out2[, variable_name:=NULL]
		#out2[, variable := NULL]
		
		return(out2)
		}

	# (1) same mmix, same country, but DIFFERENT category type
	ivs1 <- rbindlist(lapply(setup_x_iv, calc_ivs, same_key = 'country', different_key = 'cat_class', exclude_brands = T))
	ivs1b = dcast.data.table(ivs1, date + country + cat_class ~ iv_label, value.var = c('value'))[order(country, cat_class,date)]
		
	# (2) same mmix, same category, but DIFFERENT country class
	ivs2 <- rbindlist(lapply(setup_x_iv, calc_ivs, same_key = 'category', different_key = 'country_class', exclude_brands = T))
	ivs2b = dcast.data.table(ivs2, date + category + country_class ~ iv_label, value.var = c('value'))[order(category, country_class,date)]

	# merge to panel
	brand_panel <- merge(brand_panel, ivs1b, by = c('date', 'country', 'cat_class'), all.x=T)
	brand_panel <- merge(brand_panel, ivs2b, by = c('date', 'category', 'country_class'), all.x=T)
	
	setorder(brand_panel, category, country, brand, date)
	
	# DIAGNOSTICS
	tmp=dcast(ivs1[, list(obs=length(unique(date[!is.na(value)]))), by = c('country', 'cat_class', 'variable')], country+cat_class~variable,value.var='obs')
	obs=brand_panel[, list(all_obs=length(unique(date[!is.na(usales)]))), by = c('country', 'cat_class')]
	setkey(tmp, country, cat_class)
	setkey(obs, country, cat_class)
	iv1_check=obs[tmp]
	setorder(iv1_check, cat_class, country)
	
	tmp=dcast(ivs2[, list(obs=length(unique(date[!is.na(value)]))), by = c('country_class', 'category', 'variable')], category+country_class~variable,value.var='obs')
	obs=brand_panel[, list(all_obs=length(unique(date[!is.na(usales)]))), by = c('country_class', 'category')]
	setkey(tmp, country_class, category)
	setkey(obs, country_class, category)
	iv2_check=obs[tmp]

	sink('../audit/iv_computation.txt')
	cat('CHECK: NUMBER OF OBSERVATIONS PER CATEGORY/COUNTRY (CLASSES) FOR EACH INSTRUMENT\n\n')
	print(iv1_check)
	cat('\n\n\n')
	print(iv2_check)
	sink()

	# Save file
	fwrite(brand_panel, '../temp/preclean.csv')
	