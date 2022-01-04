#include <Rcpp.h>
using namespace Rcpp;

// taken from http://gallery.rcpp.org/articles/run_sum-benchmark/

// [[Rcpp::export]]
NumericVector run_sum(NumericVector x, int n) {
  
  int sz = x.size();
  
  NumericVector res(sz);
  
  // sum the values from the beginning of the vector to n 
  for(int i = 0; i<n; i++) {
    //res[i] = std::accumulate(x.begin(), x.end()-sz+n-n+i, 0.0);
    res[i] = std::accumulate(x.begin(), x.begin()+i+1, 0.0);
  }

  // loop through the rest of the vector
  for(int i = n; i < sz; i++) {
    res[i] = res[i-1] + x[i] - x[i-n];
  }
  
  // pad the first n-1 elements with NA
  //std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
  
  return res;
}


// [[Rcpp::export]]
NumericVector na_forwarding(NumericVector x) {
  
  int sz = x.size();
  
  NumericVector res(sz);
  
  // sum the values from the beginning of the vector to n 
  //res[n-1] = std::accumulate(x.begin(), x.end()-sz+n, 0.0);
  
  // pad the first n-1 elements with NA
  //std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
  res[0] = x[0];
  
  // loop through the rest of the vector
  for(int i = 1; i < sz; i++) {
    if(R_IsNA(x[i])) {
      res[i] = res[i-1];
    } else {
      res[i] = x[i];
    }
  }
  return res;
}