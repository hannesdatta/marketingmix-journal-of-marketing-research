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

# filling of NAs
require(zoo)
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


# rolling three-month sum
	rolling_threemonth <- function(X) {

	sapply(1:length(X), function(x) {
		subscr=(x-3):(x-1)
		subscr=ifelse(subscr<1,NA,subscr)
		sum(X[subscr])
		})
	}
	
# rolling three-month mean
	rolling_threemonth <- function(X) {
		sapply(1:length(X), function(x) {
			subscr=(x-2):(x-0)
			subscr=ifelse(subscr<1,NA,subscr)
			mean(X[subscr])
			})
		}

		comp_sum <- function(x) {
			res=sum(x,na.rm=T)-ifelse(is.na(x),0,x)
			# If all others are NA, set to NA.
			na=sapply(seq(along=x), function(n) {
				all(is.na(x[-n]))
				})
			if (length(na)>0) res[na]<-NA
			return(res)
			}
			
		comp_wmean <- function(x, w=NULL) {
			if (is.null(w)) w=rep(1,length(x))
			res=(sum(x * w,na.rm=T)-ifelse(is.na(x*w),0,x*w))/(sum(w,na.rm=T)-w)
			# If all others are NA, set to NA.
			na=sapply(seq(along=x), function(n) {
				all(is.na(x[-n]))
				})
			if (length(na)>0) res[na]<-NA
			return(res)
			}
			
			
weigh_by_w <- function(x, w) {
		if (sum(w)==0) w = rep(1, length(x))
		sum(x*w)/sum(w)
		}