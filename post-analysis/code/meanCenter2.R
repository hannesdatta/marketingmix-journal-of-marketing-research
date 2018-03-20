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