psignstars <- function(x) {
  sapply(x, function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "    "))))
}


# Function to run regression model (#mmix, with spec formula)
regmodel <- function(formula=list(~1+I(country_class=='linc') + as.factor(category) + as.factor(brand)), 
                     dat, model = 'lm') {
 
  lmerctrl = lmerControl(optimizer ="Nelder_Mead", check.conv.singular="ignore")
  
  if (class(formula)=='formula') formula=list(formula)
  
  regs <- lapply(unique(dat$variable), function(varname) {
    fit=NULL
    if (model=='lm') {
      st = lapply(formula, function(form) lm(update(form, elast ~ .), data = data.table(dat[variable==varname&!is.na(elast)]), weights=w_elast))
      lt = lapply(formula, function(form) lm(update(form, elastlt ~ .), data = data.table(dat[variable==varname&!is.na(elastlt)]), weights=w_elastlt))
      fit=NULL
      }
    if (model=='lmer') {
      st = lapply(formula, function(form) lmer(update(form, elast ~ .),  control = lmerctrl, REML=F, data = data.table(dat[variable==varname&!is.na(elast)]), weights=w_elast))
      lt = lapply(formula, function(form) lmer(update(form, elastlt ~ .),  control = lmerctrl, REML = F, data = data.table(dat[variable==varname&!is.na(elastlt)]), weights=w_elastlt))
      fit= NULL #sem.model.fits(list(st,lt))
      }
    return(list(variable=varname, st=st, lt=lt, fit = fit))
      
  })
  
names(regs) <- unique(dat$variable)
return(regs)
}

rsq <- function(m) {
  resid=resid(m)
  pred=predict(m)
  y=pred+resid
  tss=sum((y-mean(y))^2)
  rss=sum(resid^2)
  rsq=1-(rss/tss)
  return(rsq)
}

# test
#~1+I(country_class=='linc')
#m<-lmer(elast ~ 1 + ln_gdppercap2010_mc + (1 | category) + (1 | country) + (1|brand) + necessity, data = data.table(dat[variable=='llen'&!is.na(elast)]), weights=1/elast_se)

#library(blme)
#m<-blmer(elast ~ 1 + ln_gdppercap2010_mc + (1 | category) + necessity, data = data.table(dat[variable=='llen'&!is.na(elast)]), weights=1/elast_se)
#stargazer(m, type='text')


# Function to plot model results (next to each other)
printout = function(x, type='st', vars=NULL, omit=NULL, title='', printtype='html', notes=NULL, dep.var.labels.include=FALSE, table.layout=c('-c#m-t-a-n'), covariate_choices=NULL,colnames_overwrite=NULL, colsep_overwrite = NULL, table.layout.overwrite=NULL, ...) {
  
  if (is.null(vars)) vars=seq(along=x)
  
  dep.var.labels=NULL
  
  
  table.layout=c('-#m-t-a-n')
  if (length(vars)>1) table.layout=c('-c#m-t-a-n')
  
  if (type=='st') {
    res = do.call('c', lapply(x[unique(vars)], function(m) m$st))
    llres = do.call('c', lapply(x[unique(vars)], function(m) m$llratio$st))
    colsep = unlist(lapply(x[unique(vars)], function(x) length(x$st)))
    
  }
  if (type=='lt') {
    res = do.call('c', lapply(x[unique(vars)], function(m) m$lt))
    llres = do.call('c', lapply(x[unique(vars)], function(m) m$llratio$lt))
    colsep = unlist(lapply(x[unique(vars)], function(x) length(x$lt)))
  }
  if (type=='stlt') {
    res = do.call('c', lapply(x[unique(vars)], function(m) c(m$st, m$lt)))
    llres = do.call('c', lapply(x[unique(vars)], function(m) c(m$llratio$st, m$llratio$lt)))
    colsep = unlist(lapply(x[unique(vars)], function(x) length(x$st)+length(x$lt)))
    dep.var.labels.include=T
    dep.var.labels=rep(c('short-term', 'long-term'),length(unique(vars)))
    
    table.layout=gsub('[#]m', '#dm', table.layout)
  }
  if (!is.null(table.layout.overwrite)) table.layout=table.layout.overwrite ##table.layout=c('-c#m-t-a-n')
  # get all variable names
  covlabels= lapply(res, function(x) {
    if(class(x)=='lmerMod') return(colnames(x@pp$X))
    if(class(x)=='lm') return(names(x$coefficients))
  })
  
  # take order of variables of model with most variables
  llabels=unlist(lapply(covlabels, length))
  chose_labels = unlist(covlabels[which(llabels==max(llabels))][1])
  # add all other variables, by order (should they have been excluded before)
  
  for (i in seq(along=covlabels)) chose_labels=union(chose_labels, covlabels[[i]])
  #chose_labels=rev(chose_labels)
  
  # add custom labels if necessary
  if (!is.null(covariate_choices)) chose_labels=covariate_choices[covariate_choices%in%chose_labels]
    
  chose_labels=chose_labels[!chose_labels%in%c('(Intercept)')]
  covlabels_print = unlist(sanitize_table(data.frame(chose_labels)))
  covlabels_print=c(covlabels_print, 'Constant')
  
  chose_labels=gsub('[(]', '[(]', chose_labels)
  chose_labels=gsub('[)]', '[)]', chose_labels)
  chose_labels=gsub('[*]', '[*]', chose_labels)
  
  covlabels_print_order=paste0("^", chose_labels , "$")
  

  keep_alpha <- function(x) gsub("[^[:alnum:][:space:][,]]","",x)
  
  if (!is.null(omit)) note_text = c(paste0('model includes fixed effects for: ', keep_alpha(gsub('[|]', ', ', omit))))
  if (is.null(omit)) note_text = 'model does not contain fixed effects'
  
  if (!is.null(notes)) note_text=notes
  
  if (!is.null(vars)) collabels=names(vars) else collabels=names(res)
  
  if (!is.null(colnames_overwrite)) {
    collabels=colnames_overwrite
    colsep = colsep_overwrite
  }
  
  r2s = c('R-squared', sub('^(-)?0[.]', '\\1.', formatC(sapply(res, rsq), digits=3, format='f', flag='#')))
  obs = c('Observations', sapply(res, function(x) length(residuals(x))))
  if (length(llres)>0) {
    llnote = c('Log-likelihood ratio tests', llres) 
    added_notes=list(r2s,obs,llnote)
    
    } else { 
      added_notes=list(r2s,obs)
      
    }
  
  
  
  to_stargz <- function(x) {
    gsub('\\\\[*]', '*', x)
  }
  
  #print(table.layout)
  stargazer(res, type = printtype, omit=omit, title = paste0(title, '<sup>1</sup>'), 
            column.labels=collabels, 
            digits=3, digit.separate = 3, digit.separator = ',',
            dep.var.caption=NULL, 
            initial.zero=FALSE,
            notes.align='l',
            notes.append=FALSE,
            dep.var.labels.include = dep.var.labels.include, 
            order=covlabels_print_order,
            dep.var.labels=dep.var.labels,
            covariate.labels=covlabels_print,
            notes=to_stargz(paste0('<sup>1</sup> ', notes_sig, ' ', note_text)), 
            omit.stat=c('aic','bic'), 
            single.row=F, notes.label='',
            table.layout=table.layout, 
            add.lines=added_notes, 
            column.separate=colsep,
            ...)

}


# Function to produce model results for short- and long-term elasticities
printres <- function(x, omit, title='', vars= NULL, pagebreak=F) {
  if(pagebreak==T) cat("<P style='page-break-before: always'>")
  printout(x, 'st', omit=omit, title = tab(ifelse(title=='', 'short-term', paste0(title, ', short-term'))), vars=vars)
  if(pagebreak==T) cat("<P style='page-break-before: always'>")
  printout(x, 'lt', omit=omit, title = tab(ifelse(title=='', 'long-term', paste0(title, ', long-term'))), vars=vars)

}

# Numbering of figures and tables
fig<-function(caption,prefix='') {figureno<<-figureno+1;paste0('Figure ', prefix, figureno, ': ', caption)}
tab<-function(caption,prefix='') {tableno<<-tableno+1;paste0('Table ', prefix, tableno, ': ', caption)}

# initialize numbering
tableno<<-0
figureno<<-0

library(Hmisc)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex"), ...){
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ### remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
    
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew, ...), type="html", caption.placement='top')
    else print(xtable(Rnew), type="latex", ...) 
  }
} 


# convert z-score into a signifance asteriks
signstars <- function(zscore) { 
  if (length(zscore)==0) return("   ")
  if (zscore==Inf) return("   ")
  if (is.nan(zscore) | !is.numeric(zscore) | is.na(zscore)) return("   ")
  ret <- "ns."
  if (abs(zscore)>qnorm(1-(0.1/2))) ret <- c("*")
  if (abs(zscore)>qnorm(1-(0.05/2))) ret <- c("**")
  if (abs(zscore)>qnorm(1-(0.01/2))) ret <- c("***")
  return(ret)
}

# sort x alphabetically
sort_alpha <- function(x) return(x[order(x)])

winsor_onesided <- function(x, fraction, side='both') {
    side=unique(side)
    
    if(length(fraction) != 1 || fraction < 0 ||
       fraction > 0.5) {
      stop("bad value for 'fraction'")
    }
  
    lim <- quantile(x, probs=c(fraction, 1-fraction))
    if (side=='both'|side=='left') x[ x < lim[1] ] <- lim[1]
    if (side=='both'|side=='right') x[ x > lim[2] ] <- lim[2]
  
    x
}


my_capitalize <- function(x) {
  sapply(stri_trans_totitle(x), function(y) {
    if (nchar(y)<=3) return(toupper(y)) else return(y)
    
  })
}
