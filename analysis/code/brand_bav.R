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

# Generate overview for BAV for which brand data is required

### LOAD DATA SETS
library(data.table)
source('../../post-analysis/code/proc_rename.R')
library(xlsx)

## Load panel data
	brand_panel=fread('../../derived/output/datasets_main.csv') #temp/preclean_main.csv')
	brand_panel[, ':=' (date = as.Date(date))]

	rename_file = '../../post-analysis/code/renaming.txt'
## 
  brand_panel[, ':=' (first_date=min(date[!is.na(usales)]), last_date=max(date[!is.na(usales)])),by=c('category','country')]
  
  setorder(brand_panel, brand, country,category,date)
  tmp=	brand_panel[, list(countries = paste0(rename.fkt(unique(country), dictionary = rename_file), collapse = ', '), ncountries=uniqueN(country), categories = paste0(rename.fkt(unique(category), dictionary= rename_file), collapse = ', ')), by = c('brand')]
  tmp = tmp[!grepl('allother|unbranded',brand)]
  setorderv(tmp, 'ncountries', order = -1L)
  
  write.xlsx(tmp, '../output/brands_bav.xlsx')
  
  