rm(list = ls())

### LOAD DATA SETS
library(data.table)

dir.create('../output')

## Load panel data
brand_panel=fread('../temp/preclean_main.csv')
brand_panel[, ':=' (date = as.Date(date))]
brand_panel <- brand_panel[!brand=='mobistar']

# per brand, aggregate to quartlery level
tmp = brand_panel[, list(usales=sum(usales,na.rm=T)),by=c('brand_id','market_id','category','country','brand','year','quarter', 'month_no')]
tmp[, firstdiff:=dshift(usales),by=c('brand_id')]
tmp[, seconddiff:=dshift(firstdiff),by=c('brand_id')]
tmp[, ratio:=seconddiff/usales]
tmp[, ismax:=ratio==max(ratio,na.rm=T),by=c('brand_id')]
tmp[, max_sales:=max(usales, na.rm=T),by=c('brand_id')]
tmp[, first_crossing:=1:.N%in%which(usales>.05*max_sales)[1],by=c('brand_id')]
tmp[, last_crossing:=1:.N%in%rev(which(usales>.05*max_sales))[1],by=c('brand_id')]

fpaths = c('../audit/start-end/')

# Plotting function
plotfkt <- function(tmp, fn = NULL) {
  if (!is.null(fn)) png(fn, res=150, units='in', height=6, width=12)
  
  lbl = paste0(unique(tmp$category),' - ',unique(tmp$country), ' - ', unique(tmp$brand), ' - ', unique(tmp$brand_id))
  
  with(tmp, {
    plot(usales,type='l', main = lbl, cex=.8)
    abline(v=which(first_crossing==1),col='blue')
    text(x=which(first_crossing==1),y=max(usales),labels='start',cex=.8)
    #text(x=which(ismax==1),y=max(usales),labels='(1) Stremersch',cex=.8)
    abline(v=which(last_crossing==1),col='red')
    text(x=which(last_crossing==1),y=max(usales),labels='end',cex=.8)
    
  })
  
  if (!is.null(fn)) dev.off()
}

#plotfkt(tmp[brand_id==1000])


dir.create('../audit/start-end')
unlink('../audit/start-end/*')

for (bid in unique(brand_panel$brand_id))	{
  cat(paste0('Plotting for brand ', bid,'\n'))
  tmpdat = tmp[brand_id==bid]
  
  lbl = paste0(unique(tmpdat$category),' - ',unique(tmpdat$country), ' - ', unique(tmpdat$brand), ' - ', unique(tmpdat$brand_id))
  
  if(nrow(tmpdat)>0) {
    
    plotfkt(tmpdat, paste0(fpaths[1], lbl, ' (', bid, ').png'))
  } else {
    sink(paste0(fpaths[1], lbl, ' (', bid, ').txt'))
    cat('Data not available for this brand\n')
    sink()
  }
}
