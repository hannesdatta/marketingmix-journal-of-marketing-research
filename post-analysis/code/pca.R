################################
# Principal Component Analysis #
################################

library(psych)
library(data.table)

source('corstars.R')

ds <- fread('../temp/gci_pca.csv')

vars = setdiff(colnames(ds), 'country')

sink('../output/pca.txt')

for (nfactors in 1:1) {
    
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
    
    fwrite(data.frame(unclass(fit$loadings)), paste0('../output/pca_', nfactors, 'comp_loadings.csv'),row.names=T)
    
    # Test: standardize scores
    mydata[, score1:=fit$scores[,1]]
    
    # yields the same as: scale(as.matrix(mydata[, vars, with=F]))%*%fit$weights
    
    cat('\n\nInitial Eigenvalues:\n')
    eig <- data.table(eigen(cor(mydata[, vars, with=F]))$values)
    setnames(eig, 'eigenvalue')
    print(eig)
    
    fwrite(eig, paste0('../output/pca_', nfactors, 'comp_eigen.csv'))
    
    cat('\n\nMatrix to compute component scores from standardized input matrix (and vice versa):\n')
    print(fit$weights)
    
    #tmp=as.matrix(mydata[, vars,with=F])
    #scale(tmp)%*%fit$weights # --> calculates component scores; hence fit$weights is matrix A* from Rust, Lemon, Zeithaml 2004, p. 124.
    
    fit_scores <- cbind(mydata[, keys,with=F], fit$scores)
    setkeyv(fit_scores, keys)
    setkeyv(ds, keys)
    
    cat('\n\nStandardized component scores:\n')
    print(data.frame(fit_scores), row.names=F)
    
    cat('\n\nStandardized component scores for country selection:\n')
    countries<-tolower(c('Australia', 'Singapore', 'Japan', 'New Zealand', 'Hong Kong', 'South Korea',
                                      'Malaysia', 'Thailand',
                                      'Taiwan',
                                      'China',
                                      'Indonesia',
                                      'Philippines', 'India', 'Vietnam'))
    
    print(data.frame(fit_scores[country%in%countries]), row.names=F)
    
    for (nf in 1:nfactors) eval(parse(text=paste0('ds[fit_scores, F', nfactors,'_PC', nf, ':=i.PC', nf,']')))
    
    
  }

sink()
fwrite(ds, file = '../temp/gci_pca_out.csv')