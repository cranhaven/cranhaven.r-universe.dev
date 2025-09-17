#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;
  
NumericVector multivariateGaussianNetworkLerouxBetaUpdate(NumericMatrix standardizedX,
                                                           NumericVector y,
                                                           const int numberOfResponses,
                                                           NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                           NumericMatrix WInTripletForm,
                                                           const int numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                           const int numberOfRowsInWTripletForm,
                                                           NumericVector beta,
                                                           NumericVector spatialRandomEffects,
                                                           NumericVector uRandomEffects,
                                                           NumericVector sigmaSquaredE,
                                                           double covarianceBetaPrior)
{

  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInSpatialAssignmentMatrix = spatialRandomEffects.size() / numberOfResponses;
  int numberOfColumnsInW = uRandomEffects.size() / numberOfResponses;

  for(int j = 0; j < numberOfResponses; j++) {
    

    NumericVector rthBeta  = univariateGaussianNetworkLerouxBetaUpdate(standardizedX,
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
                                                            sigmaSquaredE[j],
                                                            covarianceBetaPrior);


    for(int i = 0; i < numberOfColumnsInX; i++){
      beta[i + (j * numberOfColumnsInX)] = rthBeta[i];
    }

  }

  return beta;

}
