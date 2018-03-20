# Function to run regression model (#mmix, with spec formula)
regmodel <- function(formula=~1+I(country_class=='linc') + as.factor(category) + as.factor(brand), 
                     dat) {

  # extract variable names from formula / model frame
  
  mc = ~ln_gdppercap
  library(rockchalk)
  
  
  source('meancenter2.R')
  
  regs <- lapply(unique(dat$variable), function(varname) {
      st<-lm(update(formula, elast ~ .), data = data.table(dat[variable==varname&!is.na(elast)]), weights=1/elast_se)
      st2=meancenter2(st)
      
      lt<-lm(update(formula, elastlt ~ .), data = data.table(dat[variable==varname&!is.na(elastlt)]), weights=1/elastlt_se)
      
      return(list(variable=varname, st=st, lt=lt))
      
  })
  
names(regs) <- unique(dat$variable)
return(regs)
}

# Function to plot model results (next to each other)
printout = function(x, type='st', omit='category|brand', title='',printtype='html') {
  if (type=='st') res = lapply(out, function(m) m$st)
  if (type=='lt') res = lapply(out, function(m) m$lt)
  
  stargazer(res, type = printtype, omit=omit, title = title, column.labels=names(out),
            notes=c(paste0('ommited fixed effects for: ', gsub('[|]', ', ', omit))))

}

# Function to produce model results for short- and long-term elasticities
printres <- function(x, omit) {
  
  printout(x, 'st', omit=omit, title = tab('short-term'))
  printout(x, 'lt', omit=omit, title = tab('long-term'))

}

# Numbering of figures and tables
fig<-function(caption) {figureno<<-figureno+1;paste0('Figure ', figureno, ': ', caption)}
tab<-function(caption) {tableno<<-tableno+1;paste0('Table ', tableno, ': ', caption)}

# initialize numbering
tableno<<-0
figureno<<-0

