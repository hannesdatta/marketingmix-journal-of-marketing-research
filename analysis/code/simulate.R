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



### SIMULATION OF SHORT- AND LONG-TERM ELASTICITIES

# LOAD DATA SETS
require(data.table)
load(file='..\\..\\derived\\output\\datasets.RData')
load(file='..\\..\\derived\\output\\results.RData')

# Rationale:

# Predict market shares at mean marketing mix across complete period.

out <- results_brands[[1]][[1]]

# Determine shocks (this is of all variables in levels)
out$sur$coefficients

dt <- dcast(out$melted_panel, brand+date~variable, value.var=c('value'))


dtbb <- attraction_data(formula=unitsales_sh ~ -1 | llength + novel + price + dist | brand + date, data=dt, benchmark=out$benchmark_brand)

m<-itersur(X=dtbb@X, Y=as.matrix(dtbb@y), index=data.frame(date=dtbb@period, brand=dtbb@individ))
