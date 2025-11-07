#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix findweights_train(NumericMatrix trainingNodeID, NumericMatrix inbag, int ntrain, int trainIdx, int ntree){
  NumericMatrix result(ntrain, 1);     //to store the result, a column vector with the oob weights for 1 training data
  LogicalVector oobPerTree(ntrain);       //to store the number of out-of-bag per tree k
  //LogicalVector nonOobPerTree(ntrain);
  LogicalVector isEqual(ntrain);          //to identify the positions 
  NumericVector nbOob(ntrain);
  double Boob = 0;                               //the denominator for the weight computation
  NumericVector colK(ntrain);             
  double LbOob=NA_REAL;
  for(int k=0; k<ntree; k++){
    //oobPerTree = (inbag(_,k) == 0);       //identify what training data is out-of-bag in tree k
    colK = inbag(_,k);                    // store the inbag identifier
    //nonOobPerTree = (inbag(_,k) != 0);
      if( inbag(trainIdx,k)==0 ){                // if the i-th training data is out-of-bag for tree k
        isEqual = trainingNodeID(_,k) == trainingNodeID(trainIdx,k);   //identify what training data falls in the same leaf than the i-th training
        nbOob = colK * as<NumericVector>(isEqual);              //recover a vector with the number of times an in-bag data fell with the i-th training
        if(is_true(any(nbOob!=0))){                             //if there is at least one training inbag that falls that falls with the i-th training
          LbOob = sum(nbOob);                                   //count their number (L_b.oob)
          nbOob = nbOob/LbOob;                                  
          result(_,0) = result(_,0) + nbOob;                    //update weight vector
          Boob++;                                               //update the number of trees where oob
        }
      }
  }
  
  if(Boob > 0){
    result(_,0) = result(_,0)/Boob;
  }
  
  return(result);
  
}
