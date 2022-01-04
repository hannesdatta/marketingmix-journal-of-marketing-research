# filling of NAs
library(zoo)

nafill <- function(x, maxgap=2) {
	# maxgap indicates the maximum number of consecutive observations to fill
	
	returncall <- function() {
		if (class(x)=='numeric') return(as.numeric(res))
		if (class(x)=='integer') return(as.integer(res))
		}
		
	if (all(is.na(x))) {
		return(x)
		
		}
		
		.first=which(!is.na(x))[1]
		.last=rev(which(!is.na(x)))[1]
		res=rep(NA,length(x))
		.out = try(na.approx(x[.first:.last],maxgap=maxgap),silent=T)
		if (class(.out)=='try-error') returncall()
		#if (is.null(res)) return(as.numeric(x))
		res[.first:.last] <- .out

	#res=try(na.fill(x,fill=c(NA,'extend',NA),maxgap=2),silent=T)
	returncall()
	
	}

weigh_by_w <- function(x, w, na.rm=FALSE, type = 'arithmetic') {
		if (sum(w)==0) w = rep(1, length(x))
		
		if (type=='arithmetic') {
			if (na.rm==FALSE) return(sum(x*w)/sum(w))
			if (na.rm==TRUE) return(sum(x[!is.na(x)]*w[!is.na(x)])/sum(w[!is.na(x)]))
		}
		
		if (type=='geometric') {
			# remove non-zero entries
			x[x<0]<-0
			if (0%in%x) x=x+1
			
			if (na.rm==FALSE) {
				return(exp((sum(w*log(x))/sum(w))))
				}
			if (na.rm==TRUE) {
				return(exp((sum(w[!is.na(x)]*log(x[!is.na(x)]))/sum(w[!is.na(x)]))))
				}
		}
}
