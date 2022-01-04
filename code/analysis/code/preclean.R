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


# Load required library
  library(data.table)

# Load data
  fn = 'datasets_main.csv'
  
  brand_panel=fread(paste0('../../derived/output/', fn))
  brand_panel[, ':=' (date = as.Date(date))]
  setorder(brand_panel, market_id, brand, date)
	
# Keep selected brands and composite rest brand
  brand_panel <- brand_panel[selected==T]

  brand_panel[selected_brand==T, list(.N),by=c('brand','category','country')][!grepl('allothers',brand)]

# Compute market shares
  brand_panel[, usales_incr:=ifelse(usales==0, usales+.01, usales)]

  brand_panel[, usalessh := usales_incr/sum(usales_incr,na.rm=T), by=c('category', 'country', 'date')]
  
# Rename composite rest brand
  brand_panel[brand=='allothers', brand := paste0('allothers', .GRP), by = c('category', 'country')]

# Lag variables
  setorder(brand_panel, market_id, date)
  for (.var in 'usales') {
    brand_panel[, paste0('lag', .var) := c(NA, get(.var)[-.N]), by = c('market_id', 'brand')]
  }
	
# Determine start and end of modeling period for brands
  thres=.05 # threshold of max/min sales
  tmp = brand_panel[, list(usales=sum(usales,na.rm=T)),by=c('brand_id','date')]
  tmp[, max_sales:=max(usales, na.rm=T),by=c('brand_id')]
  tmp = tmp[, list(first_crossing=date[which(usales>=thres*max_sales)[1]],
                   last_crossing=date[rev(which(usales>thres*max_sales))[1]]),by=c('brand_id')]
  
  setkey(tmp, 'brand_id')
  setkey(brand_panel, 'brand_id')
  brand_panel[tmp, timewindow:=date>=i.first_crossing & date<=i.last_crossing]
  brand_panel[, obs48 := sum(timewindow)>=48, by = c('brand_id')]
  
  setorder(brand_panel, market_id, brand, date)

# Save cleaned data
  dir.create('../temp')
  fwrite(brand_panel, paste0('../temp/', gsub('datasets', 'preclean', fn)))

# Generate file to inform other parts of the project that data has been cleaned.
  sink('../temp/preclean.txt')
  cat('done\n')
  sink()