#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariatePoissonNetworkRandVRandomEffectsUpdate(NumericMatrix standardizedX,
                                                            NumericVector y,
                                                            NumericMatrix V,
                                                            NumericMatrix W,
                                                            const int numberOfResponses,
                                                            NumericMatrix WInTripletForm,
                                                            const int numberOfRowsInWTripletForm,
                                                            NumericVector beta,
                                                            NumericVector vRandomEffects,
                                                            NumericVector uRandomEffects,
                                                            NumericMatrix varianceCovarianceV,
                                                            NumericVector vRandomEffectsTuningParameters,
                                                            NumericVector vRandomEffectsAcceptanceRate,
                                                            NumericVector numberOfAcceptedVREDraws,
                                                            NumericVector numberOfAllAcceptedVREDraws,
                                                            int currentNumberOfIterations,
                                                            bool centerVRandomEffects)
{

  int updateIndex = 100;
  
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInV = V.cols();
  int numberOfColumnsInW = W.cols();

  NumericVector XBeta(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector betaRthResponse = getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1);
    NumericVector XBetaRthResponse = matrixVectorMultiplicationRcpp(standardizedX, betaRthResponse);
    for(int z = 0; z < numberOfRowsInX; z++) {
      XBeta[z + (j * numberOfRowsInX)] = XBetaRthResponse[z];
    }
  }

  NumericVector resultantWByURandomEffects(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector uRandomEffectsRthResponse = getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1);
    for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
      resultantWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] = resultantWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] + WInTripletForm(z, 2) * uRandomEffectsRthResponse[WInTripletForm(z, 1)];
    }
  }
  
  for(int i = 0; i < numberOfColumnsInV; i++) {
    
    NumericVector indecies = getNonZeroEntries(V(_, i));
    
    for(int j = 0; j < numberOfResponses; j++) {
      
      NumericVector kthVRandomEffects(numberOfResponses);
      for(int k = 0; k < numberOfResponses; k++) {
        kthVRandomEffects[k] = vRandomEffects[i + (k * numberOfColumnsInV)];
      }
      
      NumericVector kthVRandomEffectsStar(numberOfResponses);
      for(int k = 0; k < numberOfResponses; k++) {
        kthVRandomEffectsStar[k] = kthVRandomEffects[k];
      }
      double vRandomEffectStar = rnorm(1, kthVRandomEffects[j], vRandomEffectsTuningParameters[i + (j * numberOfColumnsInV)])[0];
      kthVRandomEffectsStar[j] = vRandomEffectStar;
      
      double U = runif(1)[0];
      
      NumericVector vRandomEffectStarRepeatedVector = doubleVectorMultiplicationRcpp(kthVRandomEffectsStar[j], V(_, i));
      NumericVector vRandomEffectsRepeatedVector = doubleVectorMultiplicationRcpp(kthVRandomEffects[j], V(_, i));
      
      int begin = j * numberOfRowsInX;
      int end = ((j + 1) * numberOfRowsInX) - 1;
      
      NumericVector rthXBeta = getSubvector(XBeta, begin, end);
      NumericVector rthResultantWByURandomEffects = getSubvector(resultantWByURandomEffects, begin, end);
      
      double logVStarLikelihood = - 0.5 * vectorTransposeVectorMultiplicationRcpp(kthVRandomEffectsStar, matrixVectorMultiplicationRcpp(matrixInverseRcppConversion(varianceCovarianceV), kthVRandomEffectsStar))
        + kthVRandomEffectsStar[j] * vectorTransposeVectorMultiplicationRcpp(getSubvector(y, begin, end), V(_, i))
        - getSumExpNetworkLerouxIndecies(rthXBeta, rthResultantWByURandomEffects, vRandomEffectStarRepeatedVector, indecies);
        
      double logVCurrentLikelihood = - 0.5 * vectorTransposeVectorMultiplicationRcpp(kthVRandomEffects, matrixVectorMultiplicationRcpp(matrixInverseRcppConversion(varianceCovarianceV), kthVRandomEffects))
        + kthVRandomEffects[j] * vectorTransposeVectorMultiplicationRcpp(getSubvector(y, begin, end), V(_, i))
        - getSumExpNetworkLerouxIndecies(rthXBeta, rthResultantWByURandomEffects, vRandomEffectsRepeatedVector, indecies);
        
      double logA = logVStarLikelihood - logVCurrentLikelihood;
      
      if(log(U) < logA){
        vRandomEffects[i + (j * numberOfColumnsInV)] = kthVRandomEffectsStar[j];
        numberOfAcceptedVREDraws[i + (j * numberOfColumnsInV)]++;
        numberOfAllAcceptedVREDraws[i + (j * numberOfColumnsInV)] ++;
        
      }
    
      vRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInV)] = numberOfAllAcceptedVREDraws[i + (j * numberOfColumnsInV)] / currentNumberOfIterations;
      
      if(currentNumberOfIterations % updateIndex == 0){
        if(vRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInV)] > 0.5){
          vRandomEffectsTuningParameters[i + (j * numberOfColumnsInV)] = 1.1 * vRandomEffectsTuningParameters[i + (j * numberOfColumnsInV)];
        }
        if(vRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInV)] < 0.3){
          vRandomEffectsTuningParameters[i + (j * numberOfColumnsInV)] = 0.9 * vRandomEffectsTuningParameters[i + (j * numberOfColumnsInV)];
        }
        numberOfAcceptedVREDraws[i + (j * numberOfColumnsInV)] = 0;
      }
          
    }
    
  }

  NumericVector allCenteredRandomEffects(vRandomEffects.size());
  for(int j = 0; j < numberOfResponses; j++){
    NumericVector centeredRandomEffects = getMeanCenteredRandomEffects(getSubvector(vRandomEffects, j * numberOfColumnsInV, ((j + 1) * numberOfColumnsInV) - 1));
    for(int z = 0; z < numberOfColumnsInW; z++){
      allCenteredRandomEffects[(j * numberOfColumnsInV) + z] = centeredRandomEffects[z];
    }
  }

  List output(5);
  if(centerVRandomEffects){
    output[0] = allCenteredRandomEffects;
  } else {
    output[0] = vRandomEffects;
  }
  output[1] = vRandomEffectsTuningParameters;
  output[2] = vRandomEffectsAcceptanceRate;
  output[3] = numberOfAcceptedVREDraws;
  output[4] = numberOfAllAcceptedVREDraws;
  
  return output;

}
