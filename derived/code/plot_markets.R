# GfK Singapore Project
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
#
#

# Load data sets
	require(data.table)
	require(xtable)
	
	load(file='..\\output\\datasets.RData')

# Stack data in data.table
	paneldata=rbindlist(lapply(all_data, function(x) rbindlist(x$data_cleaned)))
	paneldata<-paneldata[order(category,country,brand,date)]
	paneldata[, market_id := .GRP, by=c('category', 'country')]
	paneldata[!is.na(unitsales)&selected==T, trend:=1:.N, by=c('category', 'country', 'brand')]
	
# Load externals
	source('../external/proc_rename.R')
	



##################################################
# TABLE 1: Descriptive stats by category-country #
##################################################

descr_summary = paneldata[, list(nobs = length(which(selected==T))), by=c('category','country','brand')]
#descr_summary[nobs<36][order(category,country)]

tmp <- with(descr_summary[nobs>=36], table(category,country))
tmp <- cbind(tmp, rowtotal = rowSums(tmp))
tmp <- rbind(tmp, coltotal = colSums(tmp))
tmp=t(tmp)
tmp[tmp==0]<-NA
tmp=xtable(tmp,caption=NULL,digits=0)
align(tmp)[1]<-'p{4cm}'#'l'
#align(tmp)[2]<-'p{4cm}'
align(tmp)[2:(ncol(tmp)+1)]<-'p{1.0cm}'

# Create LATEX tables
print(tmp,
		 file='..\\output\\table_descr_by_cat.tex',
		  latex.environments=c('center'), scalebox = 0.7, NA.string='-',
		  caption.placement='top', floating=F, sanitize.text.function=rename.fkt.break)

#################################
# FIGURE: Plotting unit sales #
#################################


tmp = paneldata[, list(sales=sum(unitsales,na.rm=T)),by=c('country','category','date')][order(country,category,date)]

# split by categorization
cats <- list(c('camera_slr', 'camera_compact', 'desktoppc', 'laptop', "laptop_hybrids"),
			 c('phones_mobile', 'phones_smart', 'tablets', 'tablets_phablets'),
			 c('washing', 'microwave', 'dvd', 'refrigerators', 'minioven','tumbledryers'),
			 c('tv_gen1_crtv', 'tv_gen2_lcd', 'tv_gen3_lcdwithled'))
			 
tmp <- list(cat1=tmp[category%in%cats[[1]]],cat2=tmp[category%in%cats[[2]]],cat3=tmp[category%in%cats[[3]]],cat4=tmp[category%in%cats[[4]]])

require(lattice)
require(latticeExtra)

for (i in seq(along=tmp)) {
	print(i)
	tmp[[i]][, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
	levels(tmp[[i]]$country)<-rename.fkt(levels(tmp[[i]]$country))
	levels(tmp[[i]]$category)<-rename.fkt(levels(tmp[[i]]$category))

	png(paste0('../output/salesplots_', i, '.png'), res=200, units='in', height=8, width=12)
	print(xyplot(sales~date|country, groups=category,data=tmp[[i]], auto.key=list(space="bottom", columns=2, 
						   title="Categories", cex.title=1, lines=T, points=F),type='l', scales = list(y = list(relation = "free")),
				par.settings = theEconomist.theme(box = "transparent"),
				main=paste('Categories: ', paste(cats[[i]],collapse=', '),sep=''),
				lattice.options = theEconomist.opts()))
	dev.off()


}


##############################################
# FIGURE: Plotting variables for all markets #
##############################################
i=1
.vars=c('unitsales_sh', 'rwprice', 'llength', 'novel', 'wdist','wunique')

tmp = melt(paneldata[selected==T&market_id==1],id.vars=c('country','brand','date','category','brand_id','market_id'))

plotfkt <- function(fn, tmp) {

	png(fn, res=200, units='in', height=8, width=12)
	print(xyplot(value~date|variable,groups=brand,data=tmp[variable%in%.vars],type='l', scales = list(y = list(relation = "free")),
				par.settings = theEconomist.theme(box = "transparent"),
				lattice.options = theEconomist.opts(),
				auto.key=list(space="bottom", columns=3, 
				   		      title="Brands", cex.title=1, lines=T, points=F),
	main = paste0(unique(tmp$category),': ',unique(tmp$country))
			))
	dev.off()
	}

plotfkt('../output/allvars_market1.png',melt(paneldata[selected==T&market_id==1],id.vars=c('country','brand','date','category','brand_id','market_id')))
plotfkt('../output/allvars_market2.png',melt(paneldata[selected==T&market_id==100],id.vars=c('country','brand','date','category','brand_id','market_id')))



#####################
# TABLE: Model fit  #
#####################

.vars=c('rwprice', 'llength', 'novel+1', 'wdist+1','wunique')

# model fit
fit <- rbindlist(lapply(all_results[which(check=='ok')], function(x) cbind(category=unique(x$specs$category), country=unique(x$specs$country), ur_sales = x$ur_tests$ur[1], x$sur$r2)))

################################
# TABLE: Elasticities and VIFs #
################################

# Elasticities
	.vars=c('rwprice', 'llength', 'novel+1', 'wdist+1','wunique')

	# collect coefficients from market share model
	coef = rbindlist(lapply(all_results[which(check=='ok')], function(x) x$sur$coefficients))
	coef[,grpid:=.GRP,by=c('category','country')]
	# summarize coefs
	tmp = coef[variable%in%.vars, list(N_markets = length(unique(grpid)), N_equations = .N, mean=mean(coef), sd=sd(coef),min=min(coef),max=max(coef)), by=c('variable')]
	
# VIFs
	
	# collect coefficients
	vifs = rbindlist(lapply(all_results[which(check=='ok')], function(x) data.frame(category=unique(x$specs$category), country=unique(x$specs$country), x$vif)))
	vifs[,grpid:=.GRP,by=c('category','country')]
	
	# summarize vifs
	tmp <- merge(tmp, vifs[, list(vif_mean=mean(vif), N_markets_extreme = length(unique(grpid[vif>10])),  N_eq_extreme = length(which(vif>10)),min_vif=min(vif),max_vif=max(vif)), by=c('variable')], by=c('variable'))
	
	.n = tmp$variable
	tmp=t(data.frame(tmp)[,-1])
	colnames(tmp)<-.n
	
	
print(xtable(tmp), file = '..\\output\\table_ms_elast.tex',floating=F, latex.environments=c('center'))

tmp=fit[,list(N=.N, mean_R2=mean(r2)),by=c('ur_sales')] # -> some problem here
print(xtable(tmp), file = '..\\output\\table_ms_fit.tex',floating=F, latex.environments=c('center'), sanitize.text.function=rename.fkt)

#########################################
# FIGURE: # Histograms for elasticities #
#########################################


tmp = coef
tmp[, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
levels(tmp$country)<-rename.fkt(levels(tmp$country))
levels(tmp$category)<-rename.fkt(levels(tmp$category))
tmp[, ':=' (quant_min=quantile(coef,.05), quant_max=quantile(coef,.95)),by=c('variable')]
tmp=tmp[coef>=quant_min & coef <=quant_max]

png(paste0('../output/elast_hist.png'), res=200, units='in', height=8, width=12)
	
print(histogram(~coef|variable, data= tmp[variable%in%.vars], xlim=c(min(tmp$coef),max(tmp$coef)),par.settings = theEconomist.theme(box = "transparent"),
				lattice.options = theEconomist.opts()))
dev.off()

#########################################
# FIGURE: # Boxplot for elasticities #
#########################################


tmp = coef
tmp[, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
levels(tmp$country)<-rename.fkt(levels(tmp$country))
levels(tmp$category)<-rename.fkt(levels(tmp$category))
#tmp[, ':=' (quant_min=quantile(coef,.05), quant_max=quantile(coef,.95)),by=c('variable')]
#tmp=tmp[coef>=quant_min & coef <=quant_max]

png(paste0('../output/elast_boxpl.png'), res=200, units='in', height=8, width=12)
	
print(bwplot(~coef|variable, data= tmp[variable%in%.vars],par.settings = theEconomist.theme(box = "transparent"),
				lattice.options = theEconomist.opts(), scales = list(x = list(relation = "free"))))
dev.off()


########################
# TABLE: Meta analysis #
########################

# ' run meta analysis on elasticities
tmp = coef
tmp[, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
levels(tmp$country)<-rename.fkt(levels(tmp$country))
levels(tmp$category)<-rename.fkt(levels(tmp$category))

meta <- NULL
for (.var in .vars) {
	meta[[.var]] = lm(coef ~ 1+ country+category, data= tmp, subset = variable==.var)#, weights = 1/se)
	}
	
require(memisc)
models = paste0(paste0('\"',.vars,'\"'),'=meta[[',seq(along=.vars),']]')

#mtable(m1=meta[[1]], m2=meta[[2]],factor.style="($f): ($l)")



mtab <- eval(parse(text=paste('mtable(', paste(models,collapse=','),')')))
write.mtable(mtab,forLaTeX=TRUE,file="../output/meta_elast.tex", useDcolumn=T)


########################
# TABLE: Category sales #
########################
	
# ' run meta analysis on elasticities
tmp = paneldata

nanfkt <- function(x, w) {
	res=sum(w*x)
	if (all(is.na(res))) return (0)
	return(res)
	}

tmp = tmp[selected==T, list(market_id = unique(market_id),
				#trend=1:.N,
				 unitsales=sum(as.numeric(unitsales),na.rm=T), 
				 llength=nanfkt(llength,unitsales_sh),
				 rwprice=nanfkt(rwprice,unitsales_sh),
				 wunique=nanfkt(wunique,unitsales_sh),
				 wdist=nanfkt(wdist,unitsales_sh),
				 novel=nanfkt(novel,unitsales_sh)), by=c('country', 'category', 'date')]

tmp[, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
levels(tmp$country)<-rename.fkt(levels(tmp$country))
levels(tmp$category)<-rename.fkt(levels(tmp$category))

tmp[, ':=' (dunitsales = makediff(log(unitsales+1)),dllength=makediff(log(llength+1)),drwprice=makediff(log(rwprice+1)),dwunique=makediff(log(wunique+1)),dwdist=makediff(log(wdist+1)),dnovel=makediff(log(novel+1))),by=c('market_id')]
tmp<-tmp[complete.cases(tmp),]
tmp[, trend:=1:.N,by=c('market_id')]


catsales <- NULL
for (.model in unique(tmp$market_id)) {
	#print(.model)
	# any series NA?
	
	#transform everything to first differences
	
	
	tmp[market_id==.model, lagunitsales := makelag(dunitsales)]
	# transform to first differences
	m <- lm(dunitsales ~ 1+ dllength + drwprice + dwunique + dwdist + dnovel + trend + lagunitsales, data= tmp, subset = market_id==.model,  na.action=na.exclude)
	vif = try(vif(m),silent=T)
	catsales[[.model]] = list(market_id=.model, m=m, vif = vif)#, weights = 1/se)
	}

	
# collect coefficients
tmp2 = rbindlist(lapply(catsales, function(x) data.frame(market_id=x$market_id, rownames(summary(x$m)$coefficients), summary(x$m)$coefficients)))
setnames(tmp2, c('market_id', 'var','est', 'se', 't', 'p'))
tmp4 = rbindlist(lapply(catsales, function(x) if (!class(x$vif)=='try-error') return(data.frame(market_id=x$market_id,vars= names(x$vif), vif = x$vif)) else return(NULL)))
setnames(tmp4, c('market_id', 'var','vif'))


tmp2 <- merge(tmp2, paneldata[, list(nobs  = .N, category=unique(category), country=unique(country)), by=c('market_id')], by=c('market_id'))
tmp2 <- merge(tmp2, tmp4, by=c('market_id','var'),all.x=T)

tmp2[, ':=' (country=as.factor(as.character(country)), category=as.factor(as.character(category)))]
levels(tmp2$country)<-rename.fkt(levels(tmp2$country))
levels(tmp2$category)<-rename.fkt(levels(tmp2$category))


.vars2=c('dllength','dnovel','drwprice','dwdist','dwunique','lagunitsales','trend')
# summarize coefs
tmp3 = tmp2[var%in%.vars2, list(mean=mean(est), sd=sd(est),max=max(est), min=min(est), vifmean=mean(vif,na.rm=T), maxvif = max(vif,na.rm=T)), by=c('var')]

print(xtable(tmp3), file = '..\\output\\table_cat_elast.tex',floating=F, latex.environments=c('center'), sanitize.text.function=rename.fkt)

# meta analysis!

.vars2=c('dllength','dnovel','drwprice','dwdist','dwunique')
#.vars2=c('log(llength + 1)','log(novel + 1)','log(rwprice + 1)','log(wdist + 1)','log(wunique + 1)')

catmeta <- NULL
for (.var in .vars2) {
	catmeta[[.var]] = lm(est ~ 1+ country+category, data= tmp2, subset = var==.var, weights = 1/se)
	}

	
models = paste0(paste0('\"',.vars2,'\"'),'=catmeta[[',seq(along=.vars2),']]')
mtab <- eval(parse(text=paste('mtable(', paste(models,collapse=','),')')))
write.mtable(mtab,forLaTeX=TRUE,file="../output/catmeta_elast.tex", useDcolumn=T)

	
	## collect coefficients
	#vifs = rbindlist(lapply(all_results[which(check=='ok')], function(x) data.frame(x$vif))
	
	# summarize vifs
	#vifs[, list(mean=mean(vif), N_tot = .N, N_extreme = length(which(vif>10)),max=max(vif), min=min(vif)), by=c('variable')]
	
	# summarize vifs
	#vifs[, list(mean=mean(vif), N_tot = .N, N_extreme = length(which(vif>10)),max=max(vif), min=min(vif)), by=c('variable')]

  