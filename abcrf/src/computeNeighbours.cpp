#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix computeNeighboursWeights(NumericMatrix predTrainingID, NumericMatrix predTestingID, int ntrain, int ntest, int ntree){
  NumericMatrix result(ntrain, ntest);
  IntegerVector counti(ntrain);
  for(int k=0; k<ntree; k++){
    for(int i=0; i<ntest; i++){
      for(int j=0; j<ntrain; j++){
        if( predTrainingID(j,k) == predTestingID(i,k) ){
          counti[j] = 1;
        } else {
          counti[j] = 0;
        }
      }
      for(int j=0; j<ntrain; j++){
        result(j,i) = result(j,i) + counti[j];
      }
    } 
  }
  return(result);
}
