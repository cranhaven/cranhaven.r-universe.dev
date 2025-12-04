#include <Rcpp.h>
using namespace Rcpp;

double MeanNA_cpp(NumericVector x) {
  int n = x.size() ;
  double s = 0 ;
  int notNA = 0 ;
  
  for( int i=0; i<n; i++){
    if(!NumericVector::is_na(x[i])){
      notNA = notNA + 1 ;
      s = s + x[i] ;
    }
  }
  return s / notNA ; 
}

// [[Rcpp::export]]
NumericVector Score_cpp(NumericMatrix mat) {
  
  int nr = mat.nrow() ;
  int nc = mat.ncol() ;
  NumericVector tmp(nc) ;
  NumericVector tmp2(nr) ;
  NumericVector score(nc) ;

  for( int iscore=0; iscore < nc; iscore++){
    for( int i=0; i<nc; i++){
      if(i == iscore )
	tmp[i] = NA_REAL ;
      else {
	for( int j=0; j<nr; j++){
	  if(NumericVector::is_na(mat(j,iscore)) ||  NumericVector::is_na(mat(j,i))){
	    tmp2[j] = NA_REAL ;
	  }  
	  else {
	    if((mat(j,iscore) - mat(j,i)) > 0)
	      tmp2[j] = 1.0;
	    else
	      tmp2[j] = 0.0;
	  }
	}
	tmp[i] = MeanNA_cpp(tmp2) ;
      }
    }
    score[iscore] = MeanNA_cpp(tmp) ;
  }
  return score ; 
}

