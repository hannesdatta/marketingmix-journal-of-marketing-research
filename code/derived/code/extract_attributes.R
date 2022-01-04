# Create attributes

library(data.table)
load('../temp/categorized.RData')

# Do some final data cleaning/clarification, and perform a set of asserts
attribs <- NULL

for (i in 1:length(datlist_final)) {
		#######################
		# Set attribute names #
		#######################
		
		# rename problematic columns
		try(setnames(datlist_final[[i]], "3d", "threedimensional"),silent=T)
		try(setnames(datlist_final[[i]], 'function', 'functionality'),silent=T)
			
		
		# define attributes per unique model (i.e., aggregate information)
		attr_cols <- colnames(datlist_final[[i]])[!colnames(datlist_final[[i]])%in%c('period','country', 'category','market_id', 'brand', 'model', 'firstactivity', 'price_lc', 'price_eur', 'price_usd', 'sales_units', 'numeric_distribution', 'weighted_distribution', 'source', 'date', 'used', 'catname')]
		
		
		attrs <- lapply(attr_cols, function(.col) {
			print(.col)
			# simple method to identify the right product attributes
			
			# (a) if 1 element only: return this one element
			# (b) if all elements are missing, return missing
			# (c) if more than 1 element, pick elements which are NOT missing, and chose element which has the highest amount of sales for a given product attribute
			# (c) at a tie, return the first element
			
			attrs <- datlist_final[[i]][, list(sum_sales=sum(sales_units,na.rm=T)), by=c('country','brand','model', .col)]
			attrs[, rank_sales := rank(sum_sales), by=c('country','brand')]
			
			eval(parse(text=paste0('attrs[is.na(', .col, '), rank_sales:=NA]')))
			attrs[!is.na(rank_sales), max_rank:=max(rank_sales),by=c('country', 'brand', 'model')]
			if (all(is.na(attrs$rank_sales))) attrs[, max_rank:=NA]
			
			attrs[, is_max := rank_sales==max_rank]
			attrs[, is_missing:=all(is.na(rank_sales)),by=c('country','brand','model')]
			
			ret = attrs[is_max|is_missing] # take first one in case of ties.
			ret[, ind := 1:.N, by=c('country', 'brand', 'model')]
			ret = ret[ind==1, c('country', 'brand', 'model', .col), with=F][order(country,brand,model)]
			setkey(ret, 'country', 'brand', 'model')
			return(ret)
			
		})
	
		merge.all <- function(x,y, ...) {merge(x,y, all.x=T,all.y=T,...)}
		attrs=Reduce(merge.all, attrs)
			
		attrs[, sku_id := .GRP, by=c('country','brand', 'model')]	
		attribs[[i]]<-attrs
		
	}

names(attribs)<-names(datlist_final)

save(attribs, file='../temp/extract_attributes.RData')

