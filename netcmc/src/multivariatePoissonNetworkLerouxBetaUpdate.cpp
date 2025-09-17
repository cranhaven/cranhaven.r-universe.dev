#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

List multivariatePoissonNetworkLerouxBetaUpdate(NumericMatrix standardizedX,
                                                  NumericVector y,
                                                  const int numberOfResponses,
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

  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInSpatialAssignmentMatrix = spatialRandomEffects.size() / numberOfResponses;
  int numberOfColumnsInW = uRandomEffects.size() / numberOfResponses;

  for(int j = 0; j < numberOfResponses; j++) {

    List output = univariatePoissonNetworkLerouxBetaUpdate(standardizedX,
                                                             getSubvector(y, j * numberOfRowsInX, ((j + 1) * numberOfRowsInX) - 1),
                                                             numberOfRowsInX,
                                                             numberOfColumnsInX,
                                                             spatialAssignmentMatrixInTripletForm,
                                                             WInTripletForm,
                                                             numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                             numberOfRowsInWTripletForm,
                                                             getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                             getSubvector(spatialRandomEffects, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                             getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1),
                                                             covarianceBetaPrior,
                                                             numberOfBetaBlocks,
                                                             maxBetaBlockSize,
                                                             getSubvector(betaTuningParameter, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                             getSubvector(betaAcceptanceRate, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                             getSubvector(numberOfAcceptedBetaDraws, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                             getSubvector(numberOfAllAcceptedBetaDraws, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                             currentNumberOfIterations);

    NumericVector rthBeta = output[0];
    NumericVector rthBetaTuningParameter = output[1];
    NumericVector rthBetaAcceptanceRate = output[2];
    NumericVector rthNumberOfAcceptedBetaDraws = output[3];
    NumericVector rthNumberOfAllAcceptedBetaDraws = output[4];

    for(int i = 0; i < numberOfColumnsInX; i++){
      beta[i + (j * numberOfColumnsInX)] = rthBeta[i];
      betaTuningParameter[i + (j * numberOfColumnsInX)] = rthBetaTuningParameter[i];
      betaAcceptanceRate[i + (j * numberOfColumnsInX)] = rthBetaAcceptanceRate[i];
      numberOfAcceptedBetaDraws[i + (j * numberOfColumnsInX)] = rthNumberOfAcceptedBetaDraws[i];
      numberOfAllAcceptedBetaDraws[i + (j * numberOfColumnsInX)] = rthNumberOfAllAcceptedBetaDraws[i];
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
