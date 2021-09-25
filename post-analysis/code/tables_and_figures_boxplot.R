library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(data.table)

#################################################
#### LEVELS OF THE MARKETING MIX (BOX PLOTS) ####
#################################################

# load data
brand_panel <- fread('../externals/preclean_main.csv')
source('proc_auxilary.R')

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

sink('../output/tables_and_figures_boxplot.txt')
cat('done.')
sink()


