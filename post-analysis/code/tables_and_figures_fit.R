library(ggplot2)
library(gridExtra)
library(grid)


#############################
#### PLOT HOLDOUT SAMPLE ####
#############################

predictions <- fread('../output/predictions_within.csv')[type=='ec_main']

agg_sales = predictions[, list(avg_sales=mean(usales,na.rm=T)),by=c('category','country','brand')]
agg_sales[, rank:=rank(-avg_sales), by = c('category','country')]
setkey(agg_sales, category, country, brand)

setkey(predictions, category, country, brand)
predictions[agg_sales, rank:=i.rank]

predictions[, cc := .GRP, by = c('country', 'category')]

predictions[, rnum := runif(1), by = c('cc','brand')]
predictions[rank>=6, rnum:=999]


predictions[, selected_brand:=rnum==min(rnum), by= c('cc')]

predictions[, selected:=FALSE]
predictions[country=='australia'&grepl('smart', category), selected:=T]
predictions[country=='china'&grepl('camera_compact', category), selected:=T]
predictions[country=='hong kong'&grepl('desktop', category), selected:=T]
predictions[country=='india'&grepl('gen2', category), selected:=T]
predictions[country=='indonesia'&grepl('laptop', category), selected:=T]
predictions[country=='japan'&grepl('mobile', category), selected:=T]
predictions[country=='malaysia'&grepl('micro', category), selected:=T]
predictions[country=='new zealand'&grepl('gen3', category), selected:=T]
predictions[country=='philippines'&grepl('cooling', category), selected:=T]
predictions[country=='singapore'&grepl('slr', category), selected:=T]
predictions[country=='south korea'&grepl('gen1', category), selected:=T]
predictions[country=='taiwan'&grepl('tablet', category), selected:=T]
predictions[country=='thailand'&grepl('dvd', category), selected:=T]
predictions[country=='vietnam'&grepl('washing', category), selected:=T]



retained <- predictions[selected_brand==T&selected==T]
retained[, date:=as.Date(date)]



replace_categories <- function(x) {
  ret = rep('', length(x))
  ret[grepl('washing',x)] <- 'Washing machines'
  ret[grepl('tv_gen1',x)] <- 'CRT TVs'
  ret[grepl('tv_gen2_ptv',x)] <- 'Plasma TVs'
  ret[grepl('tv_gen3_lcd_only',x)] <- 'LCD TVs'
  ret[grepl('tablets',x)] <- 'Tablets'
  ret[grepl('phones_smart',x)] <- 'Smartphones'
  ret[grepl('phones_mobile',x)] <- 'Mobile phones'
  ret[grepl('microwave',x)] <- 'Microwaves'
  ret[grepl('laptop',x)] <- 'Laptop computers'
  ret[grepl('dvd',x)] <- 'DVD players and recorders'
  ret[grepl('desktoppc',x)] <- 'Desktop computers'
  ret[grepl('cooling',x)] <- 'Refrigerators'
  ret[grepl('camera_slr',x)] <- 'SLR cameras'
  ret[grepl('camera_compact',x)] <- 'Compact cameras'
  
  return(ret)
}

setorderv(retained, c('country', 'category', 'brand', 'date'), order=1L)

plots<-lapply(unique(retained$cc)[1:14], function(csel) ggplot() + geom_line(data=retained[cc==csel], aes(date, usales),
                                                                             size = .6) +
                geom_line(data=retained[cc==csel], linetype = 2, size = .6, aes(date, usales_hat)) +
                scale_x_date(date_labels = "%Y") + ylab('Unit sales') + xlab('Date') + 
                ggtitle(paste0(str_to_title(unique(retained[cc==csel]$country))),
                        subtitle=replace_categories(unique(retained[cc==csel]$category)))+
                theme(plot.title = element_text(size = 12),
                      plot.subtitle = element_text(size=9)))


png('../output/JMR.19.0501.R2_figure2.png', res=300, units='in', height=7, width=12)

grid.arrange(grobs=plots,
             ncol = 5, nrow = 3)

dev.off()

pdf('../output/JMR.19.0501.R2_figure2.pdf', height=7, width=12)

grid.arrange(grobs=plots,
             ncol = 5, nrow = 3)

dev.off()



######### LEVELS OF MMIX



# BOX PLOTS

# load data
brand_panel <- fread('../externals/preclean_main.csv')

#agg levels
agg=brand_panel[selected==T&obs48==T&timewindow==T, lapply(.SD, mean), 
                by = c('category','country','brand'), .SDcols=c('llen','rwpspr','rwpsprd','wpswdst')]

# box plot

agg_melt = melt(agg, by = c('category','country','brand'))
agg_melt[, cc:=.GRP,by=c('category', 'variable')]
agg_melt[, country_label := str_to_title(country)]

replace_variables <- function(x, lower=F) {
  ret = rep('', length(x))
  ret[grepl('llen',x)] <- 'Line length'
  ret[grepl('rwpspr',x)] <- 'Price'
  ret[grepl('rwpsprd',x)] <- 'Price (in USD)'
  ret[grepl('wpswdst',x)] <- 'Distribution'
  if (lower==T) paste0(tolower(substr(ret,1,1)), substr(ret,2,100))
  return(ret)
}

# grouped boxplot
ccsel=1
plot_box = function(ccsel) {
  ggplot(agg_melt[cc==ccsel], aes(x=country_label, y=value, fill=country_label)) + 
    geom_boxplot() +  theme(plot.title = element_text(size = 12),
                            plot.subtitle = element_text(size=9)) + theme_bw() + 
    scale_fill_grey(start = .6, end = .9)+ theme(legend.position="none")+
    ggtitle(paste0(str_to_title(replace_variables(unique(agg_melt[cc==ccsel]$variable)))),
            subtitle=replace_categories(unique(agg_melt[cc==ccsel]$category))) + 
    ylab(paste0('Average ', tolower(replace_variables(unique(agg_melt[cc==ccsel]$variable)))))+
    xlab('Country')
}

agg_melt[, category_label:=replace_categories(category)]
setorder(agg_melt, country_label, category_label, variable, brand)
orders=unique(agg_melt$country_label)

agg_melt[, country_label:=factor(country_label, labels=rev(orders))]


replace_categories <- function(x) {
  ret = rep('', length(x))
  ret[grepl('washing',x)] <- 'Washing machines'
  ret[grepl('tv_gen1',x)] <- 'CRT TVs'
  ret[grepl('tv_gen2_ptv',x)] <- 'Plasma TVs'
  ret[grepl('tv_gen3_lcd_only',x)] <- 'LCD TVs'
  ret[grepl('tablets',x)] <- 'Tablets'
  ret[grepl('phones_smart',x)] <- 'Smartphones'
  ret[grepl('phones_mobile',x)] <- 'Mobile phones'
  ret[grepl('microwave',x)] <- 'Microwaves'
  ret[grepl('laptop',x)] <- 'Laptop computers'
  ret[grepl('dvd',x)] <- 'DVD players and rec.'
  ret[grepl('desktoppc',x)] <- 'Desktop computers'
  ret[grepl('cooling',x)] <- 'Refrigerators'
  ret[grepl('camera_slr',x)] <- 'SLR cameras'
  ret[grepl('camera_compact',x)] <- 'Compact cameras'
  
  return(ret)
}

plot_box = function(ccsel) {
  ggplot(agg_melt[cc==ccsel], aes(x=country_label, y=value, fill=country_label)) + 
    geom_boxplot() +  theme(plot.title = element_text(size = 9),
                            plot.subtitle = element_text(size=9)) + theme_bw() + 
    scale_fill_grey(start = .6, end = .9)+ theme(legend.position="none")+
    ggtitle(replace_categories(unique(agg_melt[cc==ccsel]$category)))+
    ylab(paste0('Average ', replace_variables(unique(agg_melt[cc==ccsel]$variable), lower=T)))+
    xlab('Country')+ coord_flip()+theme(text = element_text(size=9))
}

for (.v in c('llen','rwpsprd','wpswdst')) {
  plots<-lapply(unique(agg_melt[variable==.v]$cc), plot_box)
  
  png(paste0('../output/boxplot_', .v, '.png'), res=400, units='in', height=7, width=12)
  grid.arrange(grobs=plots,
               ncol = 5, nrow = 3)
  dev.off()
  
  
}

