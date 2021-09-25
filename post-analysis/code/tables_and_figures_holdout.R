library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(data.table)

source('proc_auxilary.R')

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

sink('../output/tables_and_figures_holdout.txt')
cat('done.')
sink()

