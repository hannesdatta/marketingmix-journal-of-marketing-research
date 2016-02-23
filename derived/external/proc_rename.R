
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
