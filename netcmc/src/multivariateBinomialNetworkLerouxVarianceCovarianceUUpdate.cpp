#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericMatrix multivariateBinomialNetworkLerouxVarianceCovarianceUUpdate(NumericVector randomEffects,
                                                                          const int numberOfResponses,
                                                                          double xi,
                                                                          NumericMatrix omega)
{
  
  Rcpp::Environment base_env2("package:MCMCpack");
  Rcpp::Function riwish = base_env2["riwish"];
  
  int numberOfJointRandomEffects = randomEffects.size() / numberOfResponses;
  NumericMatrix matrixHolder(numberOfResponses, numberOfResponses); 
  
  for(int j = 0; j < numberOfJointRandomEffects; j ++) {
    NumericVector kthRandomEffects(numberOfResponses);
    for(int z = 0; z < numberOfResponses; z++) {
      kthRandomEffects[z] = randomEffects[j + (z * numberOfJointRandomEffects)];
    }
    NumericMatrix vectorByVector = vectorVectorTransposeMultiplicationRcpp(kthRandomEffects, kthRandomEffects);
    NumericMatrix matrixHolderLoop = matrixHolder;
    matrixHolder = matrixMatrixAdditionRcpp(matrixHolderLoop, vectorByVector);
  }
  
  for(int j = 0; j < numberOfResponses; j ++) {
    for(int z = 0; z < numberOfResponses; z++) {
      matrixHolder(j, z) += omega(j, z);
    }
  }
  
  NumericMatrix varianceCovariance = as<NumericMatrix>(riwish(xi + numberOfJointRandomEffects, matrixHolder));
  
  return varianceCovariance;
  
}
