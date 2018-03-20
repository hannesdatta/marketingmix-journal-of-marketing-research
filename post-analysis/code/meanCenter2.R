meancenter2 <- function(model) {

mt <- terms(model)
mdata <- model.frame(model)
ys  <- drop(mdata[, 1]) # do not mean center y
##dm = design matrix, columns of predictors as numerically coded
dm <- model.matrix(model)#[ , -1, drop=FALSE] #no intercept

dmnames <- paste(colnames(dm),"s", sep = "")

dmnamesticked <- paste("`",dmnames,"`", sep = "")
dmnamesticked <- gsub("``","`", dmnamesticked)

dvname <- paste(colnames(mdata)[1],"", sep = "")
dvnameticked <-  paste("`", dvname,"`", sep = "")
dvnameticked <- gsub("``","`", dvnameticked)

# determine which variables to center (all continuous variables; i.e., not factor vars, and not dummy vars
std <- function(x) {
  if(is.numeric(x)) {
    if (all(x%in%c(0,1))) x else scale(x, scale=FALSE)
  } else x
}

stddat <- apply(dm, 2, std)  ##mean-center numeric vars

colnames(stddat) <- paste(colnames(stddat), "s", sep = "")

stddat <- cbind(ys, stddat)

stddat <- as.data.frame(stddat)

colnames(stddat) <- c(dvname, dmnames)

colnames(stddat) <- gsub("`","", colnames(stddat))



mc <- model$call

existing_data = eval(parse(text=model$call$data))

stddat = cbind(existing_data, stddat)

mc$data <- quote(stddat)

fmla <- paste(dvnameticked, " ~ ", "-1 + ", paste(dmnamesticked, collapse= " + "))

mc$formula <- formula(fmla)

res <- eval(mc)

class(res) <- c("stdreg", class(model))

res

}



meanCenterHannes <-
  
  function(model, centerOnlyInteractors = TRUE, centerDV = FALSE, 
           standardize = FALSE, terms = NULL) {
    
    # function to take mean (called "standardize" here)
    
    std <- function(x) {
      
      if(!is.numeric(x)) stop("can't center a factor variable. No Can Do!")

      xmean <- mean(x, na.rm = TRUE)
      
      if (standardize) {
        
        xsd <- sd(x, na.rm = TRUE)
        
      } else {
        
        xsd <- 1
        
      }

      if (all(x%in%c(0,1))) {
        x<-x
      } else {
      x <- (x-xmean)/xsd
      }
      
      list(x = x, xmean = xmean, xsd = xsd)
      
    }
    
    std <- function(x) {
      if(is.numeric(x)) {
        if (all(x%in%c(0,1))) res=x else res=x-mean(x,na.rm=T)
      } else res=x
      
    list(x=res, xmean=ifelse(is.numeric(x), mean(x,na.rm=T), NA),
         xsd=ifelse(is.numeric(x), sd(x,na.rm=T), NA))
         
    }
    
    
    ## rdf <- get_all_vars(formula(model), model$model) #raw data frame
    
    rdf <- model.data(model)
    
    t <- terms(model)
    
    ## TODO 20140417: look at using na.action attribute of model to be more delicate here.
    
    tl <- attr(t, "term.labels")
    
    tmdc <- attr(t, "dataClasses") ##term model data classes
    
    
    
    isNumeric <- names(tmdc)[ which(tmdc %in% c("numeric"))]
    
    isFac <-  names(tmdc)[ which(tmdc %in% c("factor"))]
    
    
    
    if (centerDV & tmdc[1] != "numeric")
      
      stop("Sorry, the DV is not a numeric column, it does not make sense to center it.")
    
    
    
    ##Build "nc", a vector of variable names that "need centering"
    
    ##
    
    if (!centerDV) {
      
      if (!is.null(terms)){
        
        nc <- as.vector(terms)
        
        nc <- unique(nc)
        
      } else if (centerOnlyInteractors == FALSE){
        
        nc <- isNumeric[-1] #-1 excludes response
        
        nc <- unique(nc)
        
      } else {
        
        interactTerms <- tl[grep(":", tl)]
        
        nc <- unique(unlist(strsplit( interactTerms, ":")))
        
        nc <-  nc[which(nc %in% isNumeric)]
        
      }
      
    } else {
      
      if (!is.null(terms)){
        
        nc <- as.vector(terms)
        
        nc <- c(names(tmdc)[1] , nc)
        
      } else if (centerOnlyInteractors == FALSE){
        
        nc <- isNumeric
        
      } else {
        
        interactTerms <- tl[grep(":", tl)]
        
        nc <- unique(unlist(strsplit( interactTerms, ":")))
        
        nc <- nc[which(nc %in% isNumeric)]
        
        nc <- c(names(tmdc)[1] , nc)
        
      }
      
    }
    
    
    
    mc <- model$call
    
    ## run same model call, replacing non centered data with centered data.
    
    ##
    existing_data = eval(parse(text=model$call$data))
    
    stddat = cbind(existing_data, rdf)
    
    #stddat <- rdf
    
    centeredVars <- matrix(NA, nrow=2, ncol=length(nc))
    
    colnames(centeredVars) <- nc
    
    rownames(centeredVars) <- c("mean","scale")
    
    
    
    formulaReplace <- function(fmla, xname, newname){
      
      do.call("substitute", list(fmla, setNames(list(as.name(newname)), xname)))
      
    }
    
    
    
    newFmla <- mc$formula
    
    for (i in seq_along(nc)){
      
      icenter <- std(stddat[, nc[i]])
      
      centeredVars[1, nc[i]] <- icenter$xmean
      
      centeredVars[2, nc[i]] <- icenter$xsd
      
      newname <- paste(as.character(nc[i]), "c", sep = "")
      
      if (isTRUE(standardize)) newname <- paste(newname, "s", sep = "")
      
      stddat[ ,newname] <- icenter$x
      
      newFmla <- formulaReplace(newFmla,  as.character(nc[i]), newname)
      
      nc[i] <- newname
      
    }
    
    colnames(centeredVars) <- nc
    
    mc$formula <- newFmla
    
    mc$data <- quote(stddat)
    
    res <- eval(mc)
    
    #class(res) <- c("mcreg", class(model))
    
    #attr(res, "centeredVars") <- centeredVars
    
    #attr(res, "centerCall") <-  match.call()
    
    res
    
  }