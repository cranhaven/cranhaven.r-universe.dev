#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix predictOob_cpp(NumericMatrix origNodes, NumericMatrix inbag, int nobs, int ntree){
  NumericMatrix result(nobs, nobs);
  LogicalVector oobPerTree(nobs);
  //LogicalVector nonOobPerTree(nobs);
  LogicalVector isEqual(nobs);
  NumericVector nbOob(nobs);
  NumericVector Boob(nobs);
  NumericVector colK(nobs);
  double LbOob=NA_REAL;
  for(int k=0; k<ntree; k++){
    oobPerTree = (inbag(_,k) == 0);
    colK = inbag(_,k);
    //nonOobPerTree = (inbag(_,k) != 0);
    for(int i=0; i<nobs; i++){
      if( inbag(i,k)==0 ){
        isEqual = origNodes(_,k) == origNodes(i,k);
        nbOob = colK * as<NumericVector>(isEqual);
        if(is_true(any(nbOob!=0))){
          LbOob = sum(nbOob);
          nbOob = nbOob/LbOob;
          result(_,i) = result(_,i) + nbOob;
          Boob[i]++;
        }
      }
    }
  }
  for(int i=0; i<nobs; i++){
    result(_,i) = result(_,i)/Boob[i];
  }
  return(result);
}
