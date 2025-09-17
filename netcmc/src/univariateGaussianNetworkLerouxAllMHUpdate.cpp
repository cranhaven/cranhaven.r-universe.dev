// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

// [[Rcpp::export]]
List univariateGaussianNetworkLerouxAllMHUpdate(NumericMatrix standardizedX,
                                              NumericVector y,
                                              NumericMatrix squareSpatialNeighbourhoodMatrix,
                                              NumericMatrix spatialAssignment,
                                              NumericMatrix W,
                                              NumericMatrix squareSpatialNeighbourhoodMatrixInTripletForm,
                                              NumericMatrix spatialAssignmentMatrixInTripletForm,
                                              NumericMatrix WInTripletForm,
                                              NumericVector beta,
                                              NumericVector spatialRandomEffects,
                                              NumericVector uRandomEffects,
                                              double spatialTauSquared,
                                              double spatialRho,
                                              double sigmaSquaredU,
                                              double sigmaSquaredE,
                                              double covarianceBetaPrior,
                                              NumericVector spatialRandomEffectsTuningParameters,
                                              NumericVector spatialRandomEffectsAcceptanceRate,
                                              NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                              NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                              double spatialRhoTuningParameters,
                                              double spatialRhoAcceptanceRate,
                                              int numberOfAcceptedSpatialRhoDraws,
                                              int numberOfAllAcceptedSpatialRhoDraws,
                                              double a1,
                                              double b1,
                                              double a2,
                                              double b2,
                                              double a3,
                                              double b3,
                                              int currentNumberOfIterations,
                                              const int numberOfSamples,
                                              const int burnin,
                                              const int thin,
                                              bool betaFixed,
                                              bool spatialRandomEffectsFixed,
                                              bool uRandomEffectsFixed,
                                              bool spatialTauSquaredFixed,
                                              bool spatialRhoFixed,
                                              bool sigmaSquaredUFixed,
                                              bool sigmaSquaredEFixed,
                                              NumericVector trueBetaValues,
                                              NumericVector trueSpatialRandomEffectsValues,
                                              NumericVector trueURandomEffectsValues,
                                              double trueSpatialTauSquaredValues,
                                              double trueSpatialRhoValues,
                                              double trueSigmaSquaredUValues,
                                              double trueSigmaSquaredEValues,
                                              bool centerSpatialRandomEffects,
                                              bool centerURandomEffects)
{
  
  NumericMatrix betaSamples((numberOfSamples / thin), standardizedX.cols());
  NumericMatrix spatialRandomEffectsSamples((numberOfSamples / thin), spatialRandomEffects.size());
  NumericMatrix uRandomEffectsSamples((numberOfSamples / thin), uRandomEffects.size());
  NumericVector spatialTauSquaredSamples(numberOfSamples / thin);
  NumericVector spatialRhoSamples(numberOfSamples / thin);
  NumericVector sigmaSquaredUSamples(numberOfSamples / thin);
  NumericVector sigmaSquaredESamples(numberOfSamples / thin);
  
  NumericMatrix fittedValuesSamples((numberOfSamples / thin), standardizedX.rows());
  NumericMatrix logLikelihoodsSamples((numberOfSamples / thin), standardizedX.rows());
  
  int dimensionOfSpatialMatrix = squareSpatialNeighbourhoodMatrix.cols();
  NumericVector vectorOfOnes(dimensionOfSpatialMatrix, 1.0);
  
  NumericMatrix QSpatialMatrixComponent1 = matrixMatrixSubtractionRcpp(getDiagonalMatrix(matrixVectorMultiplicationRcpp(squareSpatialNeighbourhoodMatrix, vectorOfOnes)),
                                                                       squareSpatialNeighbourhoodMatrix);
  NumericVector QSpatialMatrixComponent1EigenValues = eigenValuesRcppConversion(QSpatialMatrixComponent1);
  
  int sampleCounter = 0;
  
  int totalNumberOfIterations = numberOfSamples + burnin;
  
  Progress progressBar(totalNumberOfIterations, true);
  
  for(int s = 0; s < totalNumberOfIterations; s++){
    
    progressBar.increment();
    
    if (Progress::check_abort()) return  Rcpp::List::create(Rcpp::Named("terminated") = -1);
    
    List output = univariateGaussianNetworkLerouxSingleMHUpdate(standardizedX,
                                                                y,
                                                                squareSpatialNeighbourhoodMatrix,
                                                                spatialAssignment,
                                                                W,
                                                                squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                spatialAssignmentMatrixInTripletForm,
                                                                WInTripletForm,
                                                                beta,
                                                                spatialRandomEffects,
                                                                uRandomEffects,
                                                                spatialTauSquared,
                                                                spatialRho,
                                                                sigmaSquaredU,
                                                                sigmaSquaredE,
                                                                covarianceBetaPrior,
                                                                spatialRandomEffectsTuningParameters,
                                                                spatialRandomEffectsAcceptanceRate,
                                                                numberOfAcceptedSpatialRandomEffectsDraws,
                                                                numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                                spatialRhoTuningParameters,
                                                                spatialRhoAcceptanceRate,
                                                                numberOfAcceptedSpatialRhoDraws,
                                                                numberOfAllAcceptedSpatialRhoDraws,
                                                                QSpatialMatrixComponent1,
                                                                QSpatialMatrixComponent1EigenValues,
                                                                a1,
                                                                b1,
                                                                a2,
                                                                b2,
                                                                a3,
                                                                b3,
                                                                currentNumberOfIterations,
                                                                betaFixed,
                                                                spatialRandomEffectsFixed,
                                                                uRandomEffectsFixed,
                                                                spatialTauSquaredFixed,
                                                                spatialRhoFixed,
                                                                sigmaSquaredUFixed,
                                                                sigmaSquaredEFixed,
                                                                trueBetaValues,
                                                                trueSpatialRandomEffectsValues,
                                                                trueURandomEffectsValues,
                                                                trueSpatialTauSquaredValues,
                                                                trueSpatialRhoValues,
                                                                trueSigmaSquaredUValues,
                                                                trueSigmaSquaredEValues,
                                                                centerSpatialRandomEffects,
                                                                centerURandomEffects);
    
    beta = output[0];
    
    spatialRandomEffects = output[1];
    spatialRandomEffectsTuningParameters = output[2];
    spatialRandomEffectsAcceptanceRate = output[3];
    numberOfAcceptedSpatialRandomEffectsDraws = output[4];
    numberOfAllAcceptedSpatialRandomEffectsDraws = output[5];
    
    uRandomEffects = output[6];
    
    spatialTauSquared = output[7];
    
    spatialRho = output[8];
    spatialRhoTuningParameters = output[9];
    spatialRhoAcceptanceRate = output[10];
    numberOfAcceptedSpatialRhoDraws = output[11];
    numberOfAllAcceptedSpatialRhoDraws = output[12];
    
    sigmaSquaredU = output[13];
    
    sigmaSquaredE = output[14];
    
    NumericVector fittedValues = output[15];
    NumericVector logLikelihoods = output[16];
    
    if((currentNumberOfIterations > burnin) && (currentNumberOfIterations % thin == 0)){
      
      betaSamples(sampleCounter, _) = beta;
      spatialRandomEffectsSamples(sampleCounter, _) = spatialRandomEffects;
      uRandomEffectsSamples(sampleCounter, _) = uRandomEffects;
      spatialTauSquaredSamples[sampleCounter] = spatialTauSquared;
      spatialRhoSamples[sampleCounter] = spatialRho;
      sigmaSquaredUSamples[sampleCounter] = sigmaSquaredU;
      sigmaSquaredESamples[sampleCounter] = sigmaSquaredE;
      
      fittedValuesSamples(sampleCounter, _) = fittedValues;
      logLikelihoodsSamples(sampleCounter, _) = logLikelihoods;
      
      sampleCounter++;
      
    }
    
    currentNumberOfIterations++;
    
  }
  
  List computationOfDIC = getUnivariateGaussianNetworkLerouxDIC(standardizedX,
                                                                y,
                                                                spatialAssignment,
                                                                W,
                                                                betaSamples,
                                                                spatialRandomEffectsSamples,
                                                                uRandomEffectsSamples,
                                                                sigmaSquaredESamples,
                                                                logLikelihoodsSamples);
  
  
  double DBar = computationOfDIC[0];
  double posteriorDeviance = computationOfDIC[1];
  double pd = computationOfDIC[2];
  double DIC = computationOfDIC[3];
  
  List modelOutput(17);
  
  modelOutput[0] = betaSamples;
  modelOutput[1] = spatialRandomEffectsSamples;
  modelOutput[2] = uRandomEffectsSamples;
  modelOutput[3] = spatialTauSquaredSamples;
  modelOutput[4] = spatialRhoSamples;
  modelOutput[5] = sigmaSquaredUSamples;
  modelOutput[6] = sigmaSquaredESamples;
  
  modelOutput[7] = spatialRandomEffectsAcceptanceRate;
  modelOutput[8] = spatialRhoAcceptanceRate;
  
  modelOutput[9] = spatialRandomEffectsTuningParameters;
  modelOutput[10] = spatialRhoTuningParameters;
  
  modelOutput[11] = fittedValuesSamples;
  modelOutput[12] = logLikelihoodsSamples;
  
  modelOutput[13] = DBar;
  modelOutput[14] = posteriorDeviance;
  modelOutput[15] = pd;
  modelOutput[16] = DIC;
  
  return modelOutput;
  
}
