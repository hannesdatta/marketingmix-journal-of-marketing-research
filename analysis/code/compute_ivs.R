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

# Compute instrumental variables
	setup_x_iv=c("wpsprd", "wpswdst", "llen", "nov3", "wpsun")
	
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
	calc_ivs <- function(x, same_key, different_key) {		
		cat(paste0('Calculating instruments for ', x,', with same ', same_key, ' and different ', different_key, '...\n'))
		# step-wise approach: calculate sum across everything by classification variable
		a=brand_panel[, list(all_n = length(which(!is.na(get(x)))), tot_sum = sum(get(x), na.rm=T)), by=c('date', same_key, different_key)]
		setkeyv(a, c('date', same_key, different_key))
		
		# subtract value from own brand, if present in a specific classification
		b=brand_panel[, list(own_n = length(which(!is.na(get(x)))), own_sum = sum(get(x),na.rm=T)), by = c('date', same_key, different_key, 'brand')]
		setkeyv(b, c('date', same_key, different_key, 'brand'))
		# -> b contains, for each brand and product class it belongs to, its own value of marketing mix
		
		# calculate brand-specific means
		tmp=b[a]
		tmp[, xmean := (tot_sum - own_sum)/(all_n-own_n)]
		# -> tmp contains for each brand in each product class the value of cat mean minus their own mean
		
		
		tmp2 = eval(parse(text=paste0('melt.data.table(dcast.data.table(tmp, date + ', same_key , ' + brand ~ ', different_key, ', value.var = c(\'xmean\')), id.vars=c(\'date\', \'', same_key, '\', \'brand\'))')))
		
		# -> this also means that all columns with NAs should be filled from (a), the mean without subtracting anything
		setkeyv(tmp2, c('date', same_key, 'variable'))
		setkeyv(a, c('date', same_key, different_key))
		
		tmp2[a, value_mean := i.tot_sum/i.all_n]
		tmp2[is.na(value), value := value_mean]
		tmp2[, variable_name := paste0('iv_', variable, '_', x)]
		res = tmp2[, c('date', same_key, 'brand', 'variable_name', 'value'),with=F]
		#dcast.data.table(tmp2, date + country + brand ~ variable_name, value.var = c('value'))
		rm(a,b,tmp, tmp2)
		return(res)
		}

	# (1) same mmix, same country, but DIFFERENT category type
	ivs1 <- lapply(setup_x_iv, calc_ivs, same_key = 'country', different_key = 'cat_class')
	# (2) same mmix, same category, but DIFFERENT country class
	ivs2 <- lapply(setup_x_iv, calc_ivs, same_key = 'category', different_key = 'country_class')
	

	ivs1b = dcast.data.table(rbindlist(ivs1), date + country + brand ~ variable_name, value.var = c('value'))
	ivs2b = dcast.data.table(rbindlist(ivs2), date + category + brand ~ variable_name, value.var = c('value'))
	
	# merge to panel
	brand_panel <- merge(brand_panel, ivs1b, by = c('date', 'country', 'brand'), all.x=T)
	brand_panel <- merge(brand_panel, ivs2b, by = c('date', 'category', 'brand'), all.x=T)
	
	setorder(brand_panel, category, country, brand, date)
	
	# set own values of IVs in a category / country to NAs
	for (var in grep('iv[_]', colnames(brand_panel), value=T)) {
		cat(paste(var, '...\n'))
		brand_panel[, to_be_replaced :=  grepl(paste0('[_]', unique(cat_class), '[_]|[_]', unique(country_class), '[_]'), var), by = c('category', 'country', 'brand')]
		brand_panel[which(to_be_replaced), (var) := NA]
		brand_panel[, to_be_replaced :=  NULL]
		}
	
	
	
	# Prediction procedure: do not apply UR tests in the first stage? On what system to run ivreg2?
	
	# - how to deal with local currencies? --> predict price using us-equivalent currencies

	
	# For the IV regression, I just include variables from other cat_classes.
	# Issues to deal with: 
		
	# - how to deal with unit roots in first-stage IV decisions
	#   o option 1: - do not take care of URs, predict level variable, and then diff the predictions in second stage if necessary
	
	#	o option 2: - check URs in predictor variables and outcome variable
	#			    - predict diff or level.
	# 				- when using it in second stage, apply appropriate transformations (e.g., logs)
	
	# - how to deal with standard errors if we do this in two stages?
	
	# Q's & to do's
	# ======
	
	# - I also need to use all other "exogenous" variables that are part of the model, so... a trend, but nothing else
	# - to what extent do I need to estimate the system at once, i.e., first stage AND second stage?
	
	
	panel <- brand_panel[selected==T]
	panel[, usalessh := usales/sum(usales,na.rm=T), by=c('category', 'country', 'date')]
	panel[, month_no := as.numeric(as.factor(date))]
	
	#panel=panel[market_id==1]
	# calculate %-differences in IV variables
	#tmp=panel[, lapply(.SD, sd, na.rm=T), by = c('category','country','date'), .SDcols= grep('iv[_]', colnames(panel), value=T)]
	
	
	fwrite(panel, '../temp/preclean.csv')
	