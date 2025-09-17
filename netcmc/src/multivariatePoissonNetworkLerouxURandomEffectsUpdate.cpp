#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariatePoissonNetworkLerouxURandomEffectsUpdate(NumericMatrix standardizedX,
                                                          NumericVector y,
                                                          NumericMatrix W,
                                                          const int numberOfResponses,
                                                          NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                          NumericMatrix WInTripletForm,
                                                          const int numberOfRowsInSpatialAssignmentMatrixInTripletForm,
                                                          const int numberOfRowsInWTripletForm,
                                                          NumericVector beta,
                                                          NumericVector spatialRandomEffects,
                                                          NumericVector uRandomEffects,
                                                          NumericMatrix varianceCovarianceU,
                                                          NumericVector uRandomEffectsTuningParameters,
                                                          NumericVector uRandomEffectsAcceptanceRate,
                                                          NumericVector numberOfAcceptedUREDraws,
                                                          NumericVector numberOfAllAcceptedUREDraws,
                                                          int currentNumberOfIterations,
                                                          bool centerURandomEffects)
{

  int updateIndex = 100;
  
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInV = spatialRandomEffects.size() / numberOfResponses;
  int numberOfColumnsInW = W.cols();

  NumericVector XBeta(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector betaRthResponse = getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1);
    NumericVector XBetaRthResponse = matrixVectorMultiplicationRcpp(standardizedX, betaRthResponse);
    for(int z = 0; z < numberOfRowsInX; z++) {
      XBeta[z + (j * numberOfRowsInX)] = XBetaRthResponse[z];
    }
  }

  NumericVector resultantVByVRandomEffects(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector vRandomEffectsRthResponse = getSubvector(spatialRandomEffects, j * numberOfColumnsInV, ((j + 1) * numberOfColumnsInV) - 1);
    for(int z = 0; z < numberOfRowsInSpatialAssignmentMatrixInTripletForm; z++) {
      resultantVByVRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0) + (j * numberOfRowsInX)] = resultantVByVRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0) + (j * numberOfRowsInX)] + spatialAssignmentMatrixInTripletForm(z, 2) * vRandomEffectsRthResponse[spatialAssignmentMatrixInTripletForm(z, 1)];
    }
  }

  for(int i = 0; i < numberOfColumnsInW; i++) {

    NumericVector resultantShedWByURandomEffects(numberOfRowsInX * numberOfResponses);
    for(int j = 0; j < numberOfResponses; j++) {
      NumericVector uRandomEffectsRthResponse = getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1);
      for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
        if(i != WInTripletForm(z, 1)){
          resultantShedWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] = resultantShedWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] + WInTripletForm(z, 2) * uRandomEffectsRthResponse[WInTripletForm(z, 1)];
        }
      }
    }

    NumericVector resultantVByVAndWByURandomEffects = resultantShedWByURandomEffects + resultantVByVRandomEffects;

    NumericVector indecies = getNonZeroEntries(W(_, i));

    for(int j = 0; j < numberOfResponses; j++) {

      NumericVector kthURandomEffects(numberOfResponses);
      for(int k = 0; k < numberOfResponses; k++) {
        kthURandomEffects[k] = uRandomEffects[i + (k * numberOfColumnsInW)];
      }

      NumericVector kthURandomEffectsStar(numberOfResponses);
      for(int k = 0; k < numberOfResponses; k++) {
        kthURandomEffectsStar[k] = kthURandomEffects[k];
      }
      double uRandomEffectStar = rnorm(1, kthURandomEffects[j], uRandomEffectsTuningParameters[i + (j * numberOfColumnsInW)])[0];
      kthURandomEffectsStar[j] = uRandomEffectStar;

      double U = runif(1)[0];

      NumericVector uRandomEffectStarRepeatedVector = doubleVectorMultiplicationRcpp(kthURandomEffectsStar[j], W(_, i));
      NumericVector uRandomEffectsRepeatedVector = doubleVectorMultiplicationRcpp(kthURandomEffects[j], W(_, i));

      int begin = j * numberOfRowsInX;
      int end = ((j + 1) * numberOfRowsInX) - 1;

      NumericVector rthXBeta = getSubvector(XBeta, begin, end);
      NumericVector rthResultantVByVAndWByURandomEffects = getSubvector(resultantVByVAndWByURandomEffects, begin, end);

      double logUStarLikelihood;
      double logUCurrentLikelihood;

      logUStarLikelihood = - 0.5 * vectorTransposeVectorMultiplicationRcpp(kthURandomEffectsStar, matrixVectorMultiplicationRcpp(matrixInverseRcppConversion(varianceCovarianceU), kthURandomEffectsStar))
        + kthURandomEffectsStar[j] * vectorTransposeVectorMultiplicationRcpp(getSubvector(y, begin, end), W(_, i))
        - getSumExpNetworkLerouxIndecies(rthXBeta, rthResultantVByVAndWByURandomEffects, uRandomEffectStarRepeatedVector, indecies);

      logUCurrentLikelihood = - 0.5 * vectorTransposeVectorMultiplicationRcpp(kthURandomEffects, matrixVectorMultiplicationRcpp(matrixInverseRcppConversion(varianceCovarianceU), kthURandomEffects))
        + kthURandomEffects[j] * vectorTransposeVectorMultiplicationRcpp(getSubvector(y, begin, end), W(_, i))
        - getSumExpNetworkLerouxIndecies(rthXBeta, rthResultantVByVAndWByURandomEffects, uRandomEffectsRepeatedVector, indecies);

      double logA = logUStarLikelihood - logUCurrentLikelihood;

      if(log(U) < logA){
        uRandomEffects[i + (j * numberOfColumnsInW)] = kthURandomEffectsStar[j];
        numberOfAcceptedUREDraws[i + (j * numberOfColumnsInW)] ++;
        numberOfAllAcceptedUREDraws[i + (j * numberOfColumnsInW)] ++;
      }

      uRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInW)] = numberOfAllAcceptedUREDraws[i + (j * numberOfColumnsInW)] / currentNumberOfIterations;

      if(currentNumberOfIterations % updateIndex == 0){
        if(uRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInW)] > 0.5){
          uRandomEffectsTuningParameters[i + (j * numberOfColumnsInW)] = 1.1 * uRandomEffectsTuningParameters[i + (j * numberOfColumnsInW)];
        }
        if(uRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInW)] < 0.3){
          uRandomEffectsTuningParameters[i + (j * numberOfColumnsInW)] = 0.9 * uRandomEffectsTuningParameters[i + (j * numberOfColumnsInW)];
        }
        numberOfAcceptedUREDraws[i + (j * numberOfColumnsInW)] = 0;
      }
    }
  }

  NumericVector allCenteredRandomEffects(uRandomEffects.size());
  for(int j = 0; j < numberOfResponses; j++){
    NumericVector centeredRandomEffects = getMeanCenteredRandomEffects(getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1));
    for(int z = 0; z < numberOfColumnsInW; z++){
      allCenteredRandomEffects[(j * numberOfColumnsInW) + z] = centeredRandomEffects[z];
    }
  }

  List output(5);
  if(centerURandomEffects){
    output[0] = allCenteredRandomEffects;
  } else {
    output[0] = uRandomEffects;
  }
  output[1] = uRandomEffectsTuningParameters;
  output[2] = uRandomEffectsAcceptanceRate;
  output[3] = numberOfAcceptedUREDraws;
  output[4] = numberOfAllAcceptedUREDraws;
  
  return output;

}
