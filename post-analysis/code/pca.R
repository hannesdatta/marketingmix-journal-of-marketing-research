################################
# Principal Component Analysis #
################################

library(psych)
library(data.table)

source('corstars.R')

ds <- fread('../temp/gci_pca.csv')

vars = setdiff(colnames(ds), 'country')

sink('../temp/pca.txt')

for (nfactors in 1:5) {
    
    cat('\n\n===============================================================================\n')
    cat(paste0('Principal Component Analysis on the GCI data with ', nfactors, ' components to be extracted\n'))
    cat('===============================================================================\n\n\n')
    
    keys = c('country')
    
    mydata=unique(ds, key=keys)
    
    # remove.NA
    mydata = mydata[complete.cases((mydata)),]
    
    cat(paste0('Removing NA observations; retaining data on ', nrow(mydata), ' countries.\n\n'))
    
    cat('\nCorrelation matrix:\n')
    
    #print(corstars(as.matrix(mydata[, vars,with=F], method="pearson", removeTriangle='none', ndec=2)))
    #cat(paste0('\nNumber of observations: ', nrow(as.matrix(mydata[, vars,with=F])), '\n'))
    
    fit <- principal(mydata[, vars,with=F], nfactors=nfactors, rotate="varimax")
    
    summary(fit)
    print(fit)
    
    # Test: standardize scores
    #mydata[, score1:=fit$scores[,1]]
    #mydata[, score2:=fit$scores[,2]]
    
    # standardize by cat
    #mydata[, score1_STD := (score1-mean(score1))/sd(score1), by = c('cat_name')]
    #mydata[, score2_STD := (score2-mean(score2))/sd(score2), by = c('cat_name')]
    
    # convert to factor scores
    
    # first input matrix is standardized
    #scale(as.matrix(mydata[, vars, with=F]))%*%fit$weights
    
    cat('\n\nInitial Eigenvalues:\n')
    eig <- data.table(eigen(cor(mydata[, vars, with=F]))$values)
    setnames(eig, 'eigenvalue')
    print(eig)
    
    cat('\n\nMatrix to compute component scores from standardized input matrix (and vice versa):\n')
    print(fit$weights)
    
    #tmp=as.matrix(mydata[, vars,with=F])
    #scale(tmp)%*%fit$weights # --> calculates component scores; hence fit$weights is matrix A* from Rust, Lemon, Zeithaml 2004, p. 124.
    
    fit_scores <- cbind(mydata[, keys,with=F], fit$scores)
    setkeyv(fit_scores, keys)
    
    #for (nf in 1:nfactors) eval(parse(text=paste0(ds, '[fit_scores, F', nfactors,'_PC', nf, ':=i.PC', nf,']')))
    
    #if (ds == 'equity') equity[, var_name:='none']
    
  }

sink()
