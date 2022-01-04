
rename.fkt <- function(x, textbreak=T, dictionary = 'renaming.txt') {
	renaming <- read.table(dictionary, sep='\t',header=T)
	as.character(sapply(x, function(x) {
		x=x
		for (i in 1:nrow(renaming)) {
			if (renaming$type[i]=="grepl") {
				if (grepl(renaming$source[i], x)[1]==T) x = renaming$target[i]
				}
			if (renaming$type[i]=="gsub") {
				x=gsub(renaming$source[i], renaming$target[i], x,fixed=F)
				}
				
			}
		if (textbreak==F) {
			x=gsub('[-]', '',x)
			}
		return(as.character(x))
		}))
	}
rename.fkt.break = function(x) rename.fkt(x, textbreak=T)



sanitize_table <- function(x, dictionary='renaming.txt', drop_leading_zero=T) {
  res=x
  # check factor or character columns
  for (col in seq(along=res)) {
    colclass=unlist(lapply(res,class))[col]
    coln=colnames(res)[col]
    
    if (colclass%in%c('character', 'factor')) {
      
      eval(parse(text=paste0('res$', coln, '=rename.fkt(res$', coln,', dictionary=dictionary)')))
    }
    #if (colclass%in%c('numeric')) {
    #  drop_zero <- function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))
    #  eval(parse(text=paste0('res$', coln, '=drop_zero(res$', coln,')')))
    #  
    #}
  }
  if (!is.null(colnames(res))) colnames(res)<-rename.fkt(colnames(res))
  if (!is.null(rownames(res))) rownames(res)<-rename.fkt(rownames(res))
  
  res
  }
