################
# EXTRACT SBBE #
################

library(car)

calc_sbbe <- function(x) {
#x = needs to be an estimated result object resulting from marketingtools package
  
  coef_sum = x$model@coefficients
  
  ind <- which(grepl('[_]dum', coef_sum$variable))
  res=NULL
  if (length(ind)>0) { 
    # Extract coefficients
    sbbe_raw <- data.table(coef_sum[ind,])
    sbbe_raw[, index:= 1:.N]
    
    sbbe_sigma = x$model@varcovar[ind,ind]
    coefs=sbbe_raw$coef
    names(coefs)<-paste0('x', sbbe_raw$index)
    
    # SBBE Computation: NORMALIZED INTERCEPTS
    # SEs computed using the Delta Method (requiring package 'car' to be loaded)
    computeSBBE <- function(index) {
      # for non-benchmark brands
      
      forms <- sapply(index, function(x) { # build formulas
        if(length(index)>1) return(paste0('(1/',length(index)+1, ') * (', length(index), ' * x', x, '-', paste0('x', setdiff(index,x), collapse='-'), ')'))
        if(length(index)==1) return(paste0('(1/',length(index)+1, ') * (', length(index), ' * x', x,')'))
        
      })
      eq1 = sapply(forms, function(f) deltaMethod(coefs, f, sbbe_sigma), simplify=F) # compute
      if(!class(eq1)=='list') eq1=list(eq1)
      
      eq1_ret=lapply(eq1, function(x) c(x$Estimate, x$SE))
      
      # for benchmark brand
      eqbbf = paste0('(1/',length(index)+1, ') * (-', paste0('x', index, collapse='-'), ')') # formula
      eqbb = deltaMethod(coefs, eqbbf, sbbe_sigma) # compute
      eqbb_ret=lapply(list(eqbb), function(x) c(x$Estimate, x$SE))
      
      # assign names
      ret = t(matrix(c(as.numeric(unlist(eq1_ret)),as.numeric(unlist(eqbb_ret))),nrow=2))
      rownames(ret) = c(names(index), "benchmark")
      colnames(ret) = c('sbbe', 'sbbe_se')
      ret
    }
    
  tmp=sbbe_raw
  index=tmp$index
  names(index) <- tmp$brand
  res=data.frame(computeSBBE(index))
  rownames(res)[nrow(res)]<-x$benchmark_brand
  res$brand=rownames(res)
  rownames(res)=NULL
  res=res[, c('brand', 'sbbe', 'sbbe_se')]
  res$market_id <- x$market_id
  }
  
return(res)
}
