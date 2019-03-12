if(0) {
  require(compiler)
  
  # Distance metrics as used in the GfK project
  
  # FUNCTIONS 
  
  relative_dist <- function(y) {
  		if (length(unique(y))==1) return(rep(1, length(y)))
  				
  		1-((sapply(y, function(x) sum(abs(x-y))))/(length(y)*(max(y)-min(y))))
  		}
  		
  relative_dist <- cmpfun(relative_dist)
  
  metric_dispersion <- function(x) {
  	N=length(x)
  	meanx=mean(x)
  	res=(1/log(N))*sum( (x/(N*meanx)) * log(  (N*meanx) / x   )     )
  	
  	#res = (1/N)*sum( (x/mean(x)) * log(x/meanx))
  	# normalize:
  	#res=res/log(N)
  	if (is.na(res)) res=0
  	#return(1-res)
  	res
  	}
  
  metric_dispersion<-cmpfun(metric_dispersion)
  
  metric_dispersion(c(1,2,2,2,1,2,4,4,4,4))
  
  entropy_fkt <- function(p) {
  	p=p/sum(p)
  	res=-sum(p*log(p))/(-log(1/length(p)))
  	if (is.na(res)) res=0
  	return(res)#-log(1/.N)
  	}
  
  # testing
  if(0) {
  	entropy_fkt(c(.1))
  	entropy_fkt(c(.1,.1,.1,.1))
  	metric_dispersion(c(.1,.1,.1,.1))
  
  	entropy_fkt(c(.1,.4,.4,.4))
  	metric_dispersion(c(.1,.4,.4,.4))
  
  	entropy_fkt(c(.1))
  	metric_dispersion(c(.1))
  }
}

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

weigh_by_w <- function(x, w,na.rm=FALSE) {
		if (sum(w)==0) w = rep(1, length(x))
		if (na.rm==FALSE) return(sum(x*w)/sum(w))
		if (na.rm==TRUE) return(sum(x[!is.na(x)]*w[!is.na(x)])/sum(w[!is.na(x)]))
}
