# Function to run regression model (#mmix, with spec formula)
regmodel <- function(formula=list(~1+I(country_class=='linc') + as.factor(category) + as.factor(brand)), 
                     dat) {
  
  if (class(formula)=='formula') formula=list(formula)
  
  regs <- lapply(unique(dat$variable), function(varname) {
    st = lapply(formula, function(form) lm(update(form, elast ~ .), data = data.table(dat[variable==varname&!is.na(elast)]), weights=1/elast_se))
    lt = lapply(formula, function(form) lm(update(form, elastlt ~ .), data = data.table(dat[variable==varname&!is.na(elastlt)]), weights=1/elastlt_se))

    return(list(variable=varname, st=st, lt=lt))
      
  })
  
names(regs) <- unique(dat$variable)
return(regs)
}

# Function to plot model results (next to each other)
printout = function(x, type='st', vars=NULL, omit='category|brand', title='',printtype='html') {
  if (is.null(vars)) vars=seq(along=x)
  
  if (type=='st') res = do.call('c', lapply(x[vars], function(m) m$st))
  if (type=='lt') res = do.call('c', lapply(x[vars], function(m) m$lt))
  
  keep_alpha <- function(x) gsub("[^[:alnum:][:space:]]","",x)
  
  stargazer(res, type = printtype, omit=omit, title = title, column.labels=names(res), dep.var.caption=NULL, initial.zero=FALSE,
            notes.align='l',
            notes=c(paste0('model includes fixed effects for: ', keep_alpha(gsub('[|]', ', ', omit)))))

}

# Function to produce model results for short- and long-term elasticities
printres <- function(x, omit, title='', vars= NULL) {
  
  printout(x, 'st', omit=omit, title = tab(ifelse(title=='', 'short-term', paste0(title, ', short-term'))), vars=vars)
  printout(x, 'lt', omit=omit, title = tab(ifelse(title=='', 'long-term', paste0(title, ', long-term'))), vars=vars)

}

# Numbering of figures and tables
fig<-function(caption) {figureno<<-figureno+1;paste0('Figure ', figureno, ': ', caption)}
tab<-function(caption) {tableno<<-tableno+1;paste0('Table ', tableno, ': ', caption)}

# initialize numbering
tableno<<-0
figureno<<-0

