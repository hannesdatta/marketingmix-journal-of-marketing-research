require(data.table)
load('../temp/extract_attributes.RData')

# summary stats function for exploration of data attributes
go <- function() {tmp<<-attribs[[i]];print(nrow(attribs[[i]])); print(summary(attribs[[i]]))}
pa<-function(x) {
  res=unique(x)
  res=res[order(res)]
  plot(res)
}

# Fill missings
for (i in 1:length(attribs)) { 
  # the column names of chosen attributes have to start with 'attr_'
  category_name = names(attribs)[i]
  print(category_name)
  
  if (category_name=='desktoppc') {
    attribs[[i]][, attr_raminmb := as.numeric(ifelse(!is.na(ram), as.character(ram), as.character(ram_in_mb)))]
    attribs[[i]][, attr_raminmb := ifelse(is.na(attr_raminmb)|attr_raminmb<=10|attr_raminmb>=quantile(attr_raminmb,.99,na.rm=T), mean(attr_raminmb,na.rm=T), attr_raminmb)]
    attribs[[i]][, attr_lograminmb := log(attr_raminmb)]
    attribs[[i]][, attr_raminmb := NULL]
    
    attribs[[i]][, attr_cpumhz := as.numeric(as.character(processor_speed))]
    attribs[[i]][, attr_cpumhz := ifelse(is.na(attr_cpumhz)|attr_cpumhz<=1|attr_cpumhz<=quantile(attr_cpumhz,.001,na.rm=T), mean(attr_cpumhz,na.rm=T), attr_cpumhz)]
    
    attribs[[i]][, attr_hdd := as.numeric(as.character(size_of_hdd))]
    attribs[[i]][, attr_hdd := ifelse(is.na(attr_hdd)|attr_hdd<=1, mean(attr_hdd,na.rm=T), attr_hdd)]
    attribs[[i]][, attr_loghdd := log(attr_hdd)]
    attribs[[i]][, attr_hdd := NULL]
  }
  
  
  if (category_name=='laptop') {
    attribs[[i]][, attr_raminmb := as.numeric(ifelse(!is.na(ram), as.character(ram), as.character(ram_in_mb)))]
    attribs[[i]][, attr_raminmb := as.numeric(ifelse(!is.na(attr_raminmb), attr_raminmb, as.character(memory_size)))]
    attribs[[i]][, attr_raminmb := ifelse(is.na(attr_raminmb)|attr_raminmb<=1|attr_raminmb>=quantile(attr_raminmb,.99,na.rm=T), mean(attr_raminmb,na.rm=T), attr_raminmb)]
    attribs[[i]][, attr_lograminmb := log(attr_raminmb)]
    attribs[[i]][, attr_raminmb := NULL]
    
    attribs[[i]][, attr_weightkg := as.numeric(ifelse(!is.na(weight_in_kg), as.character(weight_in_kg), as.numeric(as.character(weight_gramm))/1000))]
    attribs[[i]][, attr_weightkg := ifelse(attr_weightkg>10, attr_weightkg/10, attr_weightkg)]
    attribs[[i]][, attr_weightkg := ifelse(attr_weightkg>10, attr_weightkg/10, attr_weightkg)]
    attribs[[i]][, attr_weightkg := ifelse(attr_weightkg>10, attr_weightkg/10, attr_weightkg)]
    attribs[[i]][, attr_weightkg := ifelse(is.na(attr_weightkg)|attr_weightkg<.2, mean(attr_weightkg,na.rm=T), attr_weightkg)]
    
    attribs[[i]][, attr_cpumhz := as.numeric(as.character(processor_speed))]
    attribs[[i]][, attr_cpumhz := ifelse(is.na(attr_cpumhz)|attr_cpumhz<=1, mean(attr_cpumhz,na.rm=T), attr_cpumhz)]
    
    attribs[[i]][, attr_touchscreen := 'NO']
    attribs[[i]][design=='TABLET', attr_touchscreen := 'YES']
    attribs[[i]][, attr_touchscreen := as.factor(attr_touchscreen)]
    
    attribs[[i]][, attr_camera := as.factor(camera)]	
    
    attribs[[i]][, attr_wifi := as.character(ifelse(!is.na(wireless_lan), as.character(wireless_lan), as.character(wifi)))]
    attribs[[i]][, attr_wifi := as.factor(ifelse(is.na(attr_wifi)&attr_touchscreen=='NO', 'NO', as.character(attr_wifi)))]
    attribs[[i]][, attr_wifi := as.factor(ifelse(is.na(attr_wifi)&attr_touchscreen=='YES', 'YES', as.character(attr_wifi)))]
    
  }
  
  if (category_name=='tablets') {
    attribs[[i]][, attr_raminmb := as.numeric(ifelse(!is.na(ram), as.character(ram), as.character(internal_memory)))]
    attribs[[i]][attr_raminmb<=64, attr_raminmb := attr_raminmb*1000]
    attribs[[i]][, attr_raminmb := ifelse(is.na(attr_raminmb)|attr_raminmb<=1, mean(attr_raminmb,na.rm=T), attr_raminmb)]
    attribs[[i]][, attr_lograminmb := log(attr_raminmb)]
    attribs[[i]][, attr_raminmb := NULL]
    
    attribs[[i]][, attr_weightkg := as.numeric(as.character(weight_gramm))/1000]
    attribs[[i]][, attr_weightkg := ifelse(is.na(attr_weightkg)|attr_weightkg<.01, mean(attr_weightkg,na.rm=T), attr_weightkg)]
    
    attribs[[i]][, attr_screensize := as.numeric(as.character(screen_size))]
    attribs[[i]][, attr_screensize := ifelse(is.na(attr_screensize)|attr_screensize<=0, mean(attr_screensize,na.rm=T), attr_screensize)]
    
    attribs[[i]][, attr_camera := as.factor(camera)]
    attribs[[i]][is.na(attr_camera), attr_camera:='YES']
  }
  
  
  if (category_name=='phones_smart') {
    attribs[[i]][, attr_screensize := as.numeric(as.character(screen_size))]
    attribs[[i]][, attr_screensize := ifelse(is.na(attr_screensize), mean(attr_screensize,na.rm=T), attr_screensize)]
    
    attribs[[i]][, attr_touchscreen := as.factor(touchscreen)]
    
    attribs[[i]][, attr_bluetooth := as.factor(ifelse(bluetooth=='YES', 'YES', 'NO'))]
    
    attribs[[i]][, attr_wifi := as.factor(wifi)]
    
    attribs[[i]][, attr_memoryingb := as.numeric(as.character(internal_memory))]
    attribs[[i]][attr_memoryingb>64, attr_memoryingb := attr_memoryingb/1000]
    attribs[[i]][attr_memoryingb>64, attr_memoryingb := attr_memoryingb/1000]
    attribs[[i]][, attr_memoryingb := ifelse(is.na(attr_memoryingb)|attr_memoryingb<=0, mean(attr_memoryingb,na.rm=T), attr_memoryingb)]
    attribs[[i]][, attr_logmemoryingb := log(attr_memoryingb)]
    attribs[[i]][, attr_memoryingb := NULL]
    
	# include?
	# attribs[[i]][, attr_internet := as.factor(as.character(digital_stand))]
    
	# include?
    # attribs[[i]][, attr_os := as.character(operating_system)]
    # attribs[[i]][, attr_os := as.factor(ifelse(is.na(attr_os), 'OTHERS', as.character(attr_os)))]
  }
  
  if (category_name=='phones_mobile') {
    attribs[[i]][, attr_touchscreen := as.factor(touchscreen)]
    attribs[[i]][, attr_bluetoothX := as.factor(bluetooth)]
    
    attribs[[i]][, attr_bluetooth := as.factor(ifelse(attr_bluetoothX=='YES', 'YES', 'NO'))]
    attribs[[i]][, attr_bluetoothX := NULL]
    
    attribs[[i]][, attr_wifi := as.factor(wifi)]
    
	if(0){
    attribs[[i]][, attr_internet := as.character(digital_stand)]
    attribs[[i]][, attr_internet := as.factor(ifelse(is.na(attr_internet), 'OTHERS', as.character(attr_internet)))]
    
    attribs[[i]][, attr_os := as.character(operating_system)]
    attribs[[i]][, attr_os := as.factor(ifelse(is.na(attr_os), 'OTHERS', as.character(attr_os)))]
	}
  }
  
  if (category_name=='camera_slr') {
    attribs[[i]][, attr_megapixels := as.numeric(as.character(mega_pixels))]
    attribs[[i]][, attr_megapixels := ifelse(is.na(attr_megapixels)|attr_megapixels<=0|attr_megapixels>=quantile(attr_megapixels,.99,na.rm=T), mean(attr_megapixels,na.rm=T), attr_megapixels)]
    
    attribs[[i]][, attr_digitalzoom := as.numeric(as.character(digital_zoom))]
    attribs[[i]][, attr_digitalzoom := ifelse(is.na(attr_digitalzoom)|attr_digitalzoom<=0|attr_digitalzoom>=quantile(attr_digitalzoom,.99,na.rm=T), 0, attr_digitalzoom)]
  }
  
  if (category_name=='camera_compact') {
    #	attribs[[i]][, attr_type := as.factor(as.character(camera_type))]
    
    attribs[[i]][, attr_megapixels := as.numeric(as.character(mega_pixels))]
    attribs[[i]][, attr_megapixels := ifelse(is.na(attr_megapixels)|attr_megapixels<=0|attr_megapixels>=quantile(attr_megapixels,.99,na.rm=T), mean(attr_megapixels,na.rm=T), attr_megapixels)]
    
    attribs[[i]][, attr_digitalzoom := as.numeric(as.character(digital_zoom))]
    attribs[[i]][, attr_digitalzoom := ifelse(is.na(attr_digitalzoom)|attr_digitalzoom<=0|attr_digitalzoom>=quantile(attr_digitalzoom,.99,na.rm=T), 0, attr_digitalzoom)]
  }
  
  if (category_name=='dvd') {
    attribs[[i]][, attr_blueray:=as.factor(ifelse(grepl('NO', blu_ray_play), 'NO', 'YES'))]
    attribs[[i]][, attr_function:=playerrecorder]
  }
  
  if (category_name=='microwave') {
    attribs[[i]][, attr_capacity := as.numeric(net_ltrs_total)]
    attribs[[i]][, attr_capacity := ifelse(is.na(attr_capacity), mean(attr_capacity,na.rm=T), attr_capacity)]
    
    attribs[[i]][, attr_type := as.factor(detailed_main_types)]
    
    attribs[[i]][, attr_power := as.numeric(power_watts)]
    attribs[[i]][, attr_power := ifelse(is.na(attr_power)|attr_power==0, mean(attr_power,na.rm=T), attr_power)]
    
    attribs[[i]][, attr_timecontrol := as.factor(ifelse(grepl('DIGITAL', time_control), 'DIGTIAL', 'OTHERS'))]
  }
  
  if (category_name=='cooling') {
    attribs[[i]][, attr_freezer:=as.factor(freez_position)]
    attribs[[i]][, attr_numdoors:=as.numeric(as.character(no_of_doors))]
    attribs[[i]][, attr_numdoors:=ifelse(is.na(attr_numdoors)|attr_numdoors==0, quantile(attr_numdoors,.5,na.rm=T), attr_numdoors)]
    
    #with(attribs[[i]], table(country, attr_freezer))
    
    # put attribs for india to all YES.
    #attribs[[i]][country=='INDIA', attr_freezer:='YES']
    
    
    #attribs[[i]][,cap1 := as.numeric(levels(capacity_in__liters)[as.numeric(capacity_in__liters)])]
    #attribs[[i]][,cap2 := as.numeric(levels(capacity_in_liters)[as.numeric(capacity_in_liters)])]
    # guess capacity from three-digit model numbers OR model numbbers with digits + L
    
    #attribs[[i]][, cap_from_model := as.numeric(sapply(model, function(x) {
    #	m<-regexpr('[0-9]{3}',x)
    ##	return(regmatches(x, m))
    #	}))]
    
    # validate on non-missing columns
    #with(attribs[[i]][!is.na(attr_capacity)&!is.na(cap_from_model)], plot(attr_capacity, cap_from_model,type='p'))
    #m<-lm(attr_capacity~1+cap_from_model*I(as.factor(as.character(brand)))-I(as.factor(as.character(brand))), data= attribs[[i]][!is.na(attr_capacity)&!is.na(cap_from_model)])
    # R2:= 60%.
    #plot(fitted(m), m$model$attr_capacity)
    # predict new values
    #test=predict(m, newdata = attribs[[i]][brand%in% attribs[[i]][!is.na(attr_capacity)&!is.na(cap_from_model)]$brand & is.na(attr_capacity)&!is.na(cap_from_model)])
    
    
    #attribs[[i]][!is.na(attr_capacity)&!is.na(cap_from_model)]
    #attribs[[i]][!is.na(attr_capacity)&!is.na(cap_from_model)][attr_capacity<100&cap_from_model>600]
    #attribs[[i]][, attr_capacity := ifelse(!is.na(cap1),cap1,cap2)] # --> about 82% missing.
    #attribs[[i]][is.na(attr_capacity)]
    
    
    
  }
  
  if (category_name=='tv_gen1_crtv') {
    attribs[[i]][, attr_frequency := as.factor(as.character(frequency))]
    attribs[[i]][, attr_screensize := as.factor(as.character(screen_size))]
	
	attribs[[i]][, attr_screensize_recode := as.factor('OTHERS')]
	attribs[[i]][attr_screensize%in%c('14 INCH'), attr_screensize_recode := '<20 INCH']
	attribs[[i]][attr_screensize%in%c('20 INCH', '21 INCH', '24 INCH'), attr_screensize_recode := '20-24 INCH']
	attribs[[i]][attr_screensize%in%c('25 INCH', '28 INCH', '29 INCH'), attr_screensize_recode := '25-29 INCH']
	attribs[[i]][attr_screensize%in%c('32-33 INCH', '+36 INCH'), attr_screensize_recode := '30+ INCH']
	
	attribs[[i]][, attr_screensize:=NULL]
   # attribs[[i]][, attr_builtindvd:=as.factor(ifelse(built_in_dvd=='DVD', 'YES','NO'))]
  }
  
  if (category_name=='tv_gen2_ptv') {
    
    #attribs[[i]][, attr_type:=as.factor(as.character(reportingproductgroup))]
    attribs[[i]][, attr_frequency := as.character(frequency)]
    attribs[[i]][, attr_frequency := as.factor(ifelse(attr_frequency=='PLASMA', 'OTHERS', attr_frequency))]
    
    
    attribs[[i]][, attr_screensize := as.character(screen_size)]
    attribs[[i]][, attr_screensize := as.factor(ifelse(attr_screensize=='', 'OTHERS', attr_screensize))]

    attribs[[i]][, attr_screensize_recode := as.factor(NA)]
    attribs[[i]][attr_screensize%in%c('<= 24 INCH', '25 - 29 INCH'), attr_screensize_recode := '<30 INCH']
    attribs[[i]][attr_screensize%in%c('30 - 34 INCH', '35 - 39 INCH'), attr_screensize_recode := '30-39 INCH']
    attribs[[i]][attr_screensize%in%c('40 - 44 INCH', '45 - 49 INCH', '43 INCH', '46 INCH'), attr_screensize_recode := '40-49 INCH']
    attribs[[i]][attr_screensize%in%c('50 - 54 INCH', '55 - 59 INCH'), attr_screensize_recode := '50-59 INCH']
    attribs[[i]][attr_screensize%in%c('- 42 INCH', '+ 46 INCH'), attr_screensize_recode := 'OTHERS']
    attribs[[i]][is.na(attr_screensize_recode), attr_screensize_recode := attr_screensize]
    
    attribs[[i]][, attr_screensize := NULL]

        
    attribs[[i]][, attr_3d:=as.factor(ifelse(threedimensional=='3-D', 'YES','NO'))]
    attribs[[i]][, attr_builtindvdorblueray:=as.factor(ifelse(built_in_dvd=='DVD'|blu_ray_play=='BLU-RAY PLAY.', 'YES','NO'))]
    
  }
  
  
  if (category_name=='tv_gen3_lcd_only') {
    
    attribs[[i]][, attr_type:=as.factor(as.character(reportingproductgroup))]
    attribs[[i]][, attr_frequency := as.character(frequency)]
    attribs[[i]][, attr_frequency := as.factor(ifelse(attr_frequency=='PLASMA', 'OTHERS', attr_frequency))]
    
    
    attribs[[i]][, attr_screensize := as.character(screen_size)]
    attribs[[i]][, attr_screensize := as.factor(ifelse(attr_screensize=='', 'OTHERS', attr_screensize))]
    
  	attribs[[i]][, attr_screensize_recode := as.factor(NA)]
  	attribs[[i]][attr_screensize%in%c('- 13 INCH', '- 15 INCH', '- 17 INCH'), attr_screensize_recode := '<20 INCH']
  	attribs[[i]][attr_screensize%in%c('<= 24 INCH', '20 - 21 INCH', '22 - 30 INCH', '25 - 29 INCH'), attr_screensize_recode := '20-29 INCH']
  	attribs[[i]][attr_screensize%in%c('30 - 34 INCH', '35 - 39 INCH', '+ 30 INCH'), attr_screensize_recode := '30-39 INCH']
  	attribs[[i]][attr_screensize%in%c('40 - 44 INCH', '45 - 49 INCH'), attr_screensize_recode := '40-49 INCH']
  	attribs[[i]][attr_screensize%in%c('50 - 54 INCH', '55 - 59 INCH'), attr_screensize_recode := '50-59 INCH']
  	attribs[[i]][is.na(attr_screensize_recode), attr_screensize_recode := attr_screensize]
  	
  	attribs[[i]][, attr_screensize := NULL]
	
    attribs[[i]][, attr_3d:=as.factor(ifelse(threedimensional=='3-D', 'YES','NO'))]
    attribs[[i]][, attr_builtindvd:=as.factor(ifelse(built_in_dvd=='DVD', 'YES','NO'))]
    attribs[[i]][, attr_builtinbluray:=as.factor(ifelse(blu_ray_play=='BLU-RAY PLAY.', 'YES','NO'))]
  }
  
  if (category_name=='washing') {
    attribs[[i]][, attr_capacity := as.numeric(capacity)]
    attribs[[i]][, attr_capacity := ifelse(is.na(attr_capacity), mean(attr_capacity,na.rm=T), attr_capacity)]
    attribs[[i]][, attr_function := as.factor(functionality)]
    attribs[[i]][, attr_type := as.factor(type_of_washm)]
  }
  
  if (category_name=='minioven') {
    attribs[[i]][, attr_capacity := as.numeric(capacity_in_liters)]
    attribs[[i]][, attr_capacity := ifelse(is.na(attr_capacity), mean(attr_capacity,na.rm=T), attr_capacity)]
  }		
  
  if (category_name=='tumbledryers') {
    attribs[[i]][, attr_capacity := as.numeric(loading_kg)]
    attribs[[i]][, attr_capacity := ifelse(is.na(attr_capacity), mean(attr_capacity,na.rm=T), attr_capacity)]
  }
  
}

# Create dummy variables from factor variables

xattribs <- NULL

for (i in 1:length(attribs)) {
  vars=grep('^attr',colnames(attribs[[i]]),value=T)
  types=unlist(lapply(attribs[[i]],class))
  res=NULL
  for (var in vars) {
    tmp = eval(parse(text=paste0('model.matrix(~-1+', var, ',data=attribs[[i]])')))
    if (types[var]=='factor') tmp=tmp[,-ncol(tmp)]
    res=cbind(res,tmp)
  }
  xattribs[[i]]<-cbind(attribs[[i]][, c('country','brand', 'model')], category=names(attribs)[i], res)
  
}

names(xattribs) <- names(attribs)

#############################################
# Table of product attributes per category  #
# and summary stats  						#
#############################################
dir.create('../output')
sink('../output/attributes.txt')	

for (i in 1:length(attribs)) {
  cat('////////////////////////////////////////////////////\n')
  cat(paste0('Category: ', names(attribs)[i],'\n'))
  cat('////////////////////////////////////////////////////\n')
  
  attr_cols = colnames(attribs[[i]])[grepl('attr_', colnames(attribs[[i]]))]
  
  for (coln in attr_cols) {
    cat(paste0('\nAttribute: ', coln,'\n'))
    if (eval(parse(text=paste0('class(attribs[[i]]$', coln,')')))=='factor') {
      print(table(attribs[[i]][,coln,with=F]))
	  cat(paste0('\n     (Number of NAs in this variable: ', length(which(is.na(attribs[[i]][,coln,with=F]))),')\n\n'))
	  
    } else {
      print(summary(attribs[[i]][,coln,with=F]))
    }
    
  }
  cat('\n\n\n\n\n\n\n\n')
}

sink()


save(attribs, xattribs, file='..\\temp\\attributes.RData')

