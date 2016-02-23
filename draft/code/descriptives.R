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


source('load.R')
	
################################################
#                                              #
#                                              #
# D E S C R I P T I V E   S T A T I S T I C S  #
#                                              #
#                                              #
################################################


# Stats for the paper
	paneldata[, nobs:=length(unique(date[selected==T]))>36,by=c('market_id')]
	paneldata[, selected:=selected&nobs]
	length(unique(paneldata[selected==T&nobs==T]$category))
	length(unique(paneldata[selected==T&nobs==T]$market_id))
	length(unique(paneldata[selected==T&nobs==T]$brand_id))
	length(unique(paneldata[selected==T&nobs==T]$brand))


##################################################
# TABLE 1: Descriptive stats by category-country #
##################################################

	descr_summary = paneldata[, list(nobs = length(which(selected==T))), by=c('category','country','brand')]
	#descr_summary[nobs<36][order(category,country)]

	tmp <- with(descr_summary[nobs>=36], table(category,country))
	rownames(tmp) <- rename.fkt.break(rownames(tmp))

	tmp <- cbind(tmp, rowtotal = rowSums(tmp))
	tmp <- rbind(tmp, coltotal = colSums(tmp))
	tmp=t(tmp)
	tmp[tmp==0]<-NA
	tmp=xtable(tmp,caption=NULL,digits=0)
	align(tmp)[1]<-'p{2.2cm}'#'l'
	align(tmp)[2:(ncol(tmp)+1)]<-'p{1.0cm}'

	# Create LATEX tables
		print(tmp, file='..\\output\\table_descr_by_cat.tex',
			  latex.environments=c('center'), scalebox = 0.7, NA.string='-',
			  caption.placement='top', floating=F, sanitize.text.function=rename.fkt.break)

#################################
# FIGURE: Plotting unit sales #
#################################

	require(lattice)
	require(latticeExtra)

	tmp = paneldata[selected==T, list(sales=sum(unitsales,na.rm=T)),by=c('country','category','date')][order(country,category,date)]

	# split by categorization
	cats <-  list(c('camera_compact', 'camera_slr', 'desktoppc', 'laptop'),
				 c('phones_mobile', 'phones_smart', 'tablets','dvd'),
				 c('washing',  'refrigerators', 'tumbledryers', 'minioven'),
				 c('tv_gen1_crtv', 'tv_gen2_lcd', 'microwave'))
	tmp <- list(cat1=tmp[category%in%cats[[1]]],cat2=tmp[category%in%cats[[2]]],cat3=tmp[category%in%cats[[3]]],cat4=tmp[category%in%cats[[4]]])


	for (i in seq(along=tmp)) {
		print(i)
		tmp[[i]][, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
		levels(tmp[[i]]$country)<-rename.fkt(levels(tmp[[i]]$country))
		levels(tmp[[i]]$category)<-rename.fkt(levels(tmp[[i]]$category))

		png(paste0('../output/salesplots_', i, '.png'), res=200, units='in', height=8, width=12)
		print(xyplot(sales~date|country, groups=category,data=tmp[[i]], auto.key=list(space="bottom", columns=2, 
							   title="Categories", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free")),
					par.settings = theEconomist.theme(box = "transparent"),
					lattice.options = theEconomist.opts()))
		dev.off()


	}


###################################
# TABLE: Attributes per category  #
###################################


	tableout <- NULL

	for (i in 1:length(attribs)) {
		
		attr_cols = colnames(attribs[[i]])[grepl('attr_', colnames(attribs[[i]]))]
		sep_char = '; '
		char_cutoff = 150 # number of chars at which to cut the summary stats
		
		out=lapply(attr_cols, function(coln) {
			if (eval(parse(text=paste0('class(attribs[[i]]$', coln,')')))=='factor') {
				# factor variable
				# numeric variable

				minmeanmax = table(attribs[[i]][,coln,with=F])
				minmeanmax = minmeanmax[order(minmeanmax, decreasing=T)]
				minmeanmax = paste(paste0(names(minmeanmax),': ', as.numeric(minmeanmax)), collapse = sep_char)
				if (nchar(minmeanmax)>char_cutoff) minmeanmax = paste0(substr(minmeanmax,1,char_cutoff), ' [...]')
				res = c('non-metric', minmeanmax)
				} else {
				# numeric variable
				minmeanmax = summary(attribs[[i]][,coln,with=F])
				minmeanmax = gsub(' ', '', minmeanmax)
				minmeanmax = paste(minmeanmax, collapse = sep_char)
				
				res = c('metric', minmeanmax)
				}
			return(data.frame(attribute = gsub('attr_', '', coln), type=res[1], levels=res[2]))
		
			})
		out=rbindlist(out)
		
		res <- data.frame(category=names(attribs)[i], out)
		
		tableout <- rbind(tableout, res)
	}

	tmp=data.table(tableout)
	tmp[, category:=rename.fkt(category)]
	tmp=xtable(tmp,caption=NULL,digits=0)
	align(tmp)[5]<-'p{14cm}'#'l'
	#align(tmp)[2:(ncol(tmp)+1)]<-'p{1.0cm}'

	# Create LATEX tables
	print(tmp,
			 file='..\\output\\table_attributes.tex',
			  latex.environments=c('center'), scalebox = 0.7, NA.string='-',
			  caption.placement='top', floating=F)


######################################
# TABLE: Brands in multiple markets  #
######################################

	# Generate overview
		tmp = paneldata[, list(countries=length(unique(country)), categories=length(unique(category)),markets=length(unique(market_id)), valuesales_usd=sum(valuesales_usd,na.rm=T)),by=c('brand')][order(valuesales_usd,decreasing=T)]
		tmp[, rank:=1:.N]
		tmp[, value_bn_usd := paste0(formatC(valuesales_usd/1E9, digits=3), 'bn. US$')]
		.cols=c('brand', 'value_bn_usd', 'countries', 'categories', 'markets')
		tmp=tmp[, .cols,with=F]
		

	# Create LATEX tables
	print(xtable(tmp[1:50]),
			 file='..\\output\\table_brandranking.tex',
			  latex.environments=c('center'), scalebox = 0.7, NA.string='-',
			  caption.placement='top', floating=F)

