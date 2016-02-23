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



# Load data sets and results
source('load.R')
require(lattice)
require(latticeExtra)

unlink('../audit/*')

for (r in seq(along=results)) {
	all_results <<- results[[r]]
	model <<- models[[r]]
	check <<- checks[[r]]

	for (i in which(check=='ok')) {
	# plot fit
	out=all_results[[i]]
	fit = data.table(with(out$sur, cbind(dates_brands, actual=Y, resid=as.matrix(resid), predicted=as.matrix(predicted))))
	setkey(fit, brand)
	tmp=out$sur$R2
	setkey(tmp, brand)

	fit[tmp, ':=' (sales_ur = i.UR, R2 = i.R2, R2levels= i.R2levels)]
	
	
	#fit[, brand_label := paste0(brand, ' (', ifelse(sales_ur==1, 'diff.', 'lev.'), ', R2-levels = ', round(R2,2), ')')]
	fit[, brand_label := paste0(brand, '/', ifelse(sales_ur==1, paste0('diff, R2 lev(diff)=', round(unique(R2levels),2),'(', round(unique(R2),2),')'), paste0('lev, R2=', round(unique(R2),2), ''))), by='brand']

	png(paste0('../audit/fit_', model$fn, '_', unique(out$specs$market_id), '.png'), res=150, units='in', height=8, width=12)
	
	descr = paste0('fit plot: ', unique(out$specs$country), ' - ', unique(out$specs$category), ' (market_id = ', unique(out$specs$market_id),', benchmark brand = ', out$benchmark_brand, ')')
	
	print(xyplot(actual+predicted ~ date|brand_label, main = descr, data= fit, scales = list(y = list(relation = "free")), type = 'l',
	   par.settings = theEconomist.theme(box = "transparent"),lattice.options = theEconomist.opts(),
	   auto.key=list(space="bottom", columns=2, title="data source", cex.title=1, lines=T, points=F)))

	dev.off()
	}
}
