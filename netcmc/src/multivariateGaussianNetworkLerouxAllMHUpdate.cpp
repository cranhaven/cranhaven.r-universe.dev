// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

// [[Rcpp::export]]
List multivariateGaussianNetworkLerouxAllMHUpdate(NumericMatrix standardizedX,
                                                NumericVector y,
                                                const int numberOfResponses,
                                                NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                NumericMatrix spatialAssignment,
                                                NumericMatrix W,
                                                const int numberOfSpatialAreas,
                                                NumericMatrix squareSpatialNeighbourhoodMatrixInTripletForm,
                                                NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                NumericMatrix WInTripletForm,
                                                NumericVector beta,
                                                NumericVector spatialRandomEffects,
                                                NumericVector uRandomEffects,
                                                NumericVector spatialTauSquared,
                                                NumericVector spatialRho,
                                                NumericMatrix varianceCovarianceU,
                                                NumericVector sigmaSquaredE,
                                                double covarianceBetaPrior,
                                                NumericVector spatialRandomEffectsTuningParameters,
                                                NumericVector spatialRandomEffectsAcceptanceRate,
                                                NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                                NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                NumericVector spatialRhoTuningParameters,
                                                NumericVector spatialRhoAcceptanceRate,
                                                NumericVector numberOfAcceptedSpatialRhoDraws,
                                                NumericVector numberOfAllAcceptedSpatialRhoDraws,
                                                double a1,
                                                double b1,
                                                double xi,
                                                NumericMatrix omega,
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
                                                bool varianceCovarianceUFixed,
                                                bool sigmaSquaredEFixed,
                                                NumericVector trueBetaValues,
                                                NumericVector trueSpatialRandomEffectsValues,
                                                NumericVector trueURandomEffectsValues,
                                                NumericVector trueSpatialTauSquaredValues,
                                                NumericVector trueSpatialRhoValues,
                                                NumericMatrix trueVarianceCovarianceUValues,
                                                NumericVector trueSigmaSquaredEValues,
                                                bool centerSpatialRandomEffects,
                                                bool centerURandomEffects)
{
  
  NumericMatrix betaSamples((numberOfSamples / thin), standardizedX.cols() * numberOfResponses);
  NumericMatrix spatialRandomEffectsSamples((numberOfSamples / thin), spatialRandomEffects.size());
  NumericMatrix uRandomEffectsSamples((numberOfSamples / thin), uRandomEffects.size());
  NumericMatrix spatialTauSquaredSamples(numberOfSamples / thin, numberOfResponses);
  NumericMatrix spatialRhoSamples(numberOfSamples / thin, numberOfResponses);
  NumericMatrix varianceCovarianceUSamples(numberOfSamples / thin, (numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2));
  NumericMatrix sigmaSquaredESamples(numberOfSamples / thin, numberOfResponses);
  
  NumericMatrix fittedValuesSamples((numberOfSamples / thin), standardizedX.rows() * numberOfResponses);
  NumericMatrix logLikelihoodsSamples((numberOfSamples / thin), standardizedX.rows() * numberOfResponses);
  
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
    
    List output = multivariateGaussianNetworkLerouxSingleMHUpdate(standardizedX,
                                                                y,
                                                                numberOfResponses,
                                                                squareSpatialNeighbourhoodMatrix,
                                                                spatialAssignment,
                                                                W,
                                                                numberOfSpatialAreas,
                                                                squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                spatialAssignmentMatrixInTripletForm,
                                                                WInTripletForm,
                                                                beta,
                                                                spatialRandomEffects,
                                                                uRandomEffects,
                                                                spatialTauSquared,
                                                                spatialRho,
                                                                varianceCovarianceU,
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
                                                                xi,
                                                                omega,
                                                                a3,
                                                                b3,
                                                                currentNumberOfIterations,
                                                                betaFixed,
                                                                spatialRandomEffectsFixed,
                                                                uRandomEffectsFixed,
                                                                spatialTauSquaredFixed,
                                                                spatialRhoFixed,
                                                                varianceCovarianceUFixed,
                                                                sigmaSquaredEFixed,
                                                                trueBetaValues,
                                                                trueSpatialRandomEffectsValues,
                                                                trueURandomEffectsValues,
                                                                trueSpatialTauSquaredValues,
                                                                trueSpatialRhoValues,
                                                                trueVarianceCovarianceUValues,
                                                                trueSigmaSquaredEValues,
                                                                centerSpatialRandomEffects,
                                                                centerURandomEffects);
    
    beta = output[0];
    
    spatialRandomEffects = output[1];
    spatialRandomEffectsTuningParameters = output[2];
    spatialRandomEffectsAcceptanceRate = output[3];
    
    uRandomEffects = output[6];
    
    spatialTauSquared = output[7];
    
    spatialRho = output[8];
    spatialRhoTuningParameters = output[9];
    spatialRhoAcceptanceRate = output[10];
    
    varianceCovarianceU = as<NumericMatrix>(output[13]);
    
    sigmaSquaredE = output[14];
    
    NumericVector fittedValues = output[15];
    NumericVector logLikelihoods = output[16];
    
    if((currentNumberOfIterations > burnin) && (currentNumberOfIterations % thin == 0)){
      
      betaSamples(sampleCounter, _) = beta;
      spatialRandomEffectsSamples(sampleCounter, _) = spatialRandomEffects;
      uRandomEffectsSamples(sampleCounter, _) = uRandomEffects;
      spatialTauSquaredSamples(sampleCounter, _)= spatialTauSquared;
      spatialRhoSamples(sampleCounter, _) = spatialRho;

      int varianceCovarianceSampleIndex = 0;
      NumericVector varianceCovarianceUCurrentSamples(numberOfResponses + (numberOfResponses * numberOfResponses - numberOfResponses) / 2);
      for(int j = 0; j < numberOfResponses; j++) {
        for(int z = j; z < numberOfResponses; z++) {
          varianceCovarianceUCurrentSamples[varianceCovarianceSampleIndex] = varianceCovarianceU(j, z);
          varianceCovarianceSampleIndex ++;
        }
      }
      varianceCovarianceUSamples(sampleCounter, _) = varianceCovarianceUCurrentSamples;

      sigmaSquaredESamples(sampleCounter, _) = sigmaSquaredE;

      fittedValuesSamples(sampleCounter, _) = fittedValues;
      logLikelihoodsSamples(sampleCounter, _) = logLikelihoods;

      sampleCounter++;
      
    }
    
    currentNumberOfIterations++;
    
  }
  
  List computationOfDIC = getMultivariateGaussianNetworkLerouxDIC(standardizedX,
                                                                 y,
                                                                 spatialAssignment,
                                                                 W,
                                                                 betaSamples,
                                                                 spatialRandomEffectsSamples,
                                                                 uRandomEffectsSamples,
                                                                 sigmaSquaredESamples,
                                                                 logLikelihoodsSamples,
                                                                 numberOfResponses);


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
  modelOutput[5] = varianceCovarianceUSamples;
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
