
rename.fkt <- function(x, textbreak=F) {
	renaming <- read.table('renaming.txt', sep='\t',header=T)
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



sanitize_table <- function(x) {
  res=x
  if (!is.null(colnames(res))) colnames(res)<-rename.fkt(colnames(res))
  if (!is.null(rownames(res))) rownames(res)<-rename.fkt(rownames(res))
  
  # check factor or character columns
  lapply(res, function(x) x)
  
  for (col in seq(along=res)) {
    print(as.numeric(col))
    print(unlist(lapply(res[, as.numeric(col)],class)))
    if (unlist(lapply(res[, as.numeric(col)],class))%in%c('character', 'factor')) {
      print('coln')
      coln=colnames(res)[col]
      print(coln)
      # eval(parse(text=paste0('res$', coln, ':=rename_fkt(res$', coln,')')))
    }
  }
  
  
}
