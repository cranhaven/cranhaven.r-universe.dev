#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

List univariatePoissonNetworkLerouxBetaUpdate(NumericMatrix standardizedX,
                                                NumericVector y,
                                                const int numberOfRowsInX,
                                                const int numberOfColumnsInX,
                                                NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                NumericMatrix WInTripletForm,
                                                const int numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                const int numberOfRowsInWTripletForm,
                                                NumericVector beta,
                                                NumericVector spatialRandomEffects,
                                                NumericVector uRandomEffects,
                                                double covarianceBetaPrior,
                                                const int numberOfBetaBlocks,
                                                const int maxBetaBlockSize,
                                                NumericVector betaTuningParameter,
                                                NumericVector betaAcceptanceRate,
                                                NumericVector numberOfAcceptedBetaDraws,
                                                NumericVector numberOfAllAcceptedBetaDraws,
                                                int currentNumberOfIterations)
{
  
  int updateIndex = 100;
  
  NumericVector resultantVByspatialRandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInSpatialAssignmentMatrixTripletForm; z++) {
    resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] = resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] + spatialAssignmentMatrixInTripletForm(z, 2) * spatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 1)];
  }
  
  NumericVector resultantWByURandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
    resultantWByURandomEffects[WInTripletForm(z, 0)] = resultantWByURandomEffects[WInTripletForm(z, 0)] + WInTripletForm(z, 2) * uRandomEffects[WInTripletForm(z, 1)];
  }
  
  NumericVector logBetaStarLikelihood(numberOfBetaBlocks);
  NumericVector logBetaCurrentLikelihood(numberOfBetaBlocks);
  
  for(int i = 0; i < numberOfBetaBlocks; i++) {
    
    int startOfBetaBlockIndex = i * maxBetaBlockSize;
    int endOfBetaBlockIndex = (i + 1) * maxBetaBlockSize - 1;
    
    int betaBlockSize;
    
    if(endOfBetaBlockIndex > (numberOfColumnsInX - 1)) {
      endOfBetaBlockIndex = numberOfColumnsInX - 1;
      betaBlockSize = (endOfBetaBlockIndex - startOfBetaBlockIndex);
    }
    
    betaBlockSize = (endOfBetaBlockIndex - startOfBetaBlockIndex) + 1;
    
    NumericVector betaStar(numberOfColumnsInX);
    for(int g = 0; g < numberOfColumnsInX; g++) {
      betaStar[g] = beta[g];
    }
    
    for(int g = 0; g < betaBlockSize; g++) {
      betaStar[startOfBetaBlockIndex + g] = rnorm(1, beta[startOfBetaBlockIndex + g], betaTuningParameter[startOfBetaBlockIndex + g])[0];
    }
    
    double U = runif(1)[0];
    
    NumericVector XBetaStar = matrixVectorMultiplicationRcpp(standardizedX, betaStar);
    NumericVector XBeta = matrixVectorMultiplicationRcpp(standardizedX, beta);
    
    logBetaStarLikelihood[i] = - 0.5 * (1 / covarianceBetaPrior) * vectorTransposeVectorMultiplicationRcpp(betaStar, betaStar)
      + vectorTransposeVectorMultiplicationRcpp(y, XBetaStar)
      - getSumExpNetworkLeroux(XBetaStar, resultantVByspatialRandomEffects, resultantWByURandomEffects);
      
    logBetaCurrentLikelihood[i] = - 0.5 * (1 / covarianceBetaPrior) * vectorTransposeVectorMultiplicationRcpp(beta, beta)
      + vectorTransposeVectorMultiplicationRcpp(y, XBeta)
      - getSumExpNetworkLeroux(XBeta, resultantVByspatialRandomEffects, resultantWByURandomEffects);
      
    double logA = logBetaStarLikelihood[i] - logBetaCurrentLikelihood[i];
    
    if(log(U) < logA) {
      for(int g = 0; g < numberOfColumnsInX; g++) {
        beta[g] = betaStar[g];
      }
      for(int g = 0; g < betaBlockSize; g++) {
        numberOfAcceptedBetaDraws[startOfBetaBlockIndex + g] += 1;
        numberOfAllAcceptedBetaDraws[startOfBetaBlockIndex + g] += 1;
      }
    }
    
    for(int g = 0; g < betaBlockSize; g++) {
      betaAcceptanceRate[startOfBetaBlockIndex + g] = numberOfAllAcceptedBetaDraws[startOfBetaBlockIndex + g] / currentNumberOfIterations;
    }
    
    if(currentNumberOfIterations % updateIndex == 0) {
      for(int g = 0; g < betaBlockSize; g++) {
        double kthBetaAcceptanceRate = numberOfAcceptedBetaDraws[startOfBetaBlockIndex + g] / updateIndex;
        if(kthBetaAcceptanceRate > 0.5) {
          betaTuningParameter[startOfBetaBlockIndex + g] = 1.1 * betaTuningParameter[startOfBetaBlockIndex + g];
        }
        if(kthBetaAcceptanceRate < 0.3) {
          betaTuningParameter[startOfBetaBlockIndex + g] = 0.9 * betaTuningParameter[startOfBetaBlockIndex + g];
        }
        numberOfAcceptedBetaDraws[startOfBetaBlockIndex + g] = 0;
      }
    }
  }
  
  List output(5);
  output[0] = beta;
  output[1] = betaTuningParameter;
  output[2] = betaAcceptanceRate;
  output[3] = numberOfAcceptedBetaDraws;
  output[4] = numberOfAllAcceptedBetaDraws;
  return output;
  
}
