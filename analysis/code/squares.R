# EVALUATES SQUARED TERMS

load(file = c('../temp/results_20171024.RData'))

# m1: linear: price/distribution, squared terms: line length, novelty, and uniqueness
# m2: linear: price/distribution, squared terms: line length
# m3: linear: price/distribution, squared terms: line length and novelty
# m4: linear: price/distribution, squared terms: line length and uniqueness


# identify model crashes
results_brands<-results_m1

out_m1 = eval_models(results_m1)
out_m2 = eval_models(results_m2)
out_m3 = eval_models(results_m3)
out_m4 = eval_models(results_m4)


eval_models <- function(results_brands) {
  checks <- unlist(lapply(results_brands, class))
  cat('Errors in model estimation (try-error = error, list = no error)\n')
  print(table(checks))
  
  last.item = length(analysis_markets)
  pval_sq=.1
  critval = abs(qnorm(pval_sq/2))
  
  
  # provide overview of squared terms
  squares <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) data.table(market_id=unique(x$specs$market_id),
                                                                                           category=unique(x$specs$category),
                                                                                           country=unique(x$specs$country),
                                                                                           x$squared_terms)))
  
  tmp=squares[!is.na(lin_coef), list(N_squares_tested=.N, 
                                     perc_noeffect = length(which(abs(sq_z)<critval & abs(lin_z)<critval))/.N,
                                     perc_pos_lin = length(which(abs(sq_z)<critval & abs(lin_z)>=critval & lin_coef>0))/.N,
                                     perc_neg_lin = length(which(abs(sq_z)<critval & abs(lin_z)>=critval & lin_coef<0))/.N,
                                     
                                     
                                     perc_invU = length(which(sq_coef<0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
                                     perc_U = length(which(sq_coef>0&abs(sq_z)>=critval&inflection_inrange==T))/.N,
                                     perc_pos_decr = length(which(inflection_point>=max&sq_coef<0&abs(sq_z)>=critval))/.N,
                                     perc_pos_incr = length(which(inflection_point<=min&sq_coef>0&abs(sq_z)>=critval))/.N,
                                     perc_neg_decr = length(which(inflection_point>=max&sq_coef>0&abs(sq_z)>=critval))/.N,
                                     perc_neg_incr = length(which(inflection_point<=min&sq_coef<0&abs(sq_z)>=critval))/.N),
              by = c('var')]
  
  res = NULL
  res$errors=checks
  res$square_test = tmp
  
  # summarize coefficients
  coefs <- rbindlist(lapply(results_brands[!checks=='try-error'], function(x) data.table(market_id=unique(x$specs$market_id),
                                                                                         category=unique(x$specs$category),
                                                                                         country=unique(x$specs$country),
                                                                                         x$model@coefficients)))
  coefs <- coefs[!(grepl('cop[_]', varname)|varname=='lagunitsales_sh'|varname=='dum')]
  setnames(coefs, 'variable', 'original_variable')
  
  coefs[, sq_term := ifelse(grepl('[_]sq', original_variable), 'sq', 'lin')]
  coefs[, varname2 := gsub('[_]sq', '', varname)]
  coefs[, original_variable := gsub('[_]sq', '', original_variable)]
  
  tmp = melt(coefs, id.vars=c('market_id', 'category', 'country', 'original_variable', 'brand', 'varname', 'sq_term', 'varname2'))
  
  coefs=dcast.data.table(tmp,market_id+category+country+brand+varname2+original_variable~sq_term+variable, value.var='value')
  setnames(coefs, 'varname2', 'var')
  
  # clean out non-estimated linear coefficients
  coefs <- coefs[!is.na(lin_coef)]
  
  # calculate (potential) inflection points
  coefs[!is.na(sq_coef), inflection_point := -lin_coef/(2*sq_coef)]
  
  # get ranges of X
  ranges=rbindlist(lapply(results_brands[!checks=='try-error'], function(x) x$melted_panel[, list(min=min(value), max=max(value)), by = c('market_id', 'brand', 'variable')]))
  
  setkey(ranges, market_id, brand, variable)
  setkey(coefs, market_id, brand, var)
  coefs[ranges, ':=' (min=i.min, max=i.max)]
  coefs[is.na(sq_coef), ':=' (min=NA, max=NA)]
  
  # classify types
  coefs[, type := '']
  
  
  coefs[is.na(sq_coef)&lin_coef>0&abs(lin_z)>=critval, type := 'pos_lin']
  coefs[!is.na(sq_coef)&sq_z<critval&lin_coef>0&abs(lin_z)>=critval, type := 'pos_lin']
  
  coefs[is.na(sq_coef)&lin_coef<0&abs(lin_z)>=critval, type := 'neg_lin']
  coefs[!is.na(sq_coef)&sq_z<critval&lin_coef<0&abs(lin_z)>=critval, type := 'neg_lin']
  
  coefs[!is.na(sq_coef)&sq_coef<0&abs(sq_z)>=critval&inflection_point>min&inflection_point<max, type := 'invU']
  coefs[!is.na(sq_coef)&sq_coef>0&abs(sq_z)>=critval&inflection_point>min&inflection_point<max, type := 'U']

  coefs[!is.na(sq_coef)&sq_coef<0&abs(sq_z)>=critval&inflection_point>=max, type := 'pos_decr']
  coefs[!is.na(sq_coef)&sq_coef>0&abs(sq_z)>=critval&inflection_point<=min, type := 'pos_incr']
  coefs[!is.na(sq_coef)&sq_coef>0&abs(sq_z)>=critval&inflection_point>=max, type := 'neg_decr']
  coefs[!is.na(sq_coef)&sq_coef<0&abs(sq_z)>=critval&inflection_point<=min, type := 'neg_incr']
  
  coefs[is.na(sq_coef)&abs(lin_z)<critval, type := 'noeffect']
  coefs[abs(lin_z)<critval&abs(sq_z)<critval, type := 'noeffect']
  
  coefs[, N_all := .N, by = c('var')]
  
  tmp = coefs[, list(N=.N, perc=.N/unique(N_all)), by = c('var', 'type')]
  tmp[, perc:=NULL]
  
  tmp=data.table(dcast(tmp, var~type))
  tmp[, Ntotal := rowSums(tmp[,-1, with=F], na.rm=T)]
  
  setcolorder(tmp, intersect(c('var','Ntotal', 'noeffect', 'pos_lin', 'neg_lin', 'invU','U', 'pos_decr','pos_incr','neg_decr','neg_incr'), colnames(tmp)))
  
  # turn into percent
  res$coefs=cbind(tmp[,c(1:2)], tmp[,-c(1:2)]/matrix(rep(unlist(tmp[,2]),ncol(tmp)-2), ncol=ncol(tmp)-2))
  
  return(res)
}

  
  