#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix findweights(NumericMatrix trainingNodeID, NumericMatrix testingNodeID, IntegerMatrix inbag, int ntrain, int nnew, int ntree){
  NumericMatrix result(ntrain,nnew);
  IntegerVector counti(ntrain);
  double meancount;
  for(int k=0; k<ntree; k++){
   for(int i=0; i<nnew; i++){
     meancount = 0;
     for(int j=0; j<ntrain; j++){
       if( ( trainingNodeID(j,k) == testingNodeID(i,k) ) && (inbag(j,k) != 0) ){
         counti[j] = inbag(j,k);
         meancount = meancount + inbag(j,k);
       } else{
         counti[j] = 0;
       }
     }
     if( meancount >=1 ){
       for(int j=0; j<ntrain; j++){
         result(j,i) = result(j,i) + counti[j]/ meancount;
       }
     }
   } 
  }
  return(result);
}
