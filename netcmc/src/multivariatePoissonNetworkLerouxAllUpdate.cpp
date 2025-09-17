// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

// [[Rcpp::export]]
List multivariatePoissonNetworkLerouxAllUpdate(NumericMatrix standardizedX,
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
                                                  double covarianceBetaPrior,
                                                  const int numberOfBetaBlocks,
                                                  const int maxBetaBlockSize,
                                                  NumericVector betaTuningParameter,
                                                  NumericVector betaAcceptanceRate,
                                                  NumericVector numberOfAcceptedBetaDraws,
                                                  NumericVector numberOfAllAcceptedBetaDraws,
                                                  NumericVector spatialRandomEffectsTuningParameters,
                                                  NumericVector spatialRandomEffectsAcceptanceRate,
                                                  NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                                  NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                  NumericVector uRandomEffectsTuningParameters,
                                                  NumericVector uRandomEffectsAcceptanceRate,
                                                  NumericVector numberOfAcceptedUREDraws,
                                                  NumericVector numberOfAllAcceptedUREDraws,
                                                  NumericVector spatialRhoTuningParameters,
                                                  NumericVector spatialRhoAcceptanceRate,
                                                  NumericVector numberOfAcceptedSpatialRhoDraws,
                                                  NumericVector numberOfAllAcceptedSpatialRhoDraws,
                                                  double a1,
                                                  double b1,
                                                  double xi,
                                                  NumericMatrix omega,
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
                                                  NumericVector trueBetaValues,
                                                  NumericVector trueSpatialRandomEffectsValues,
                                                  NumericVector trueURandomEffectsValues,
                                                  NumericVector trueSpatialTauSquaredValues,
                                                  NumericVector trueSpatialRhoValues,
                                                  NumericMatrix trueVarianceCovarianceUValues,
                                                  bool centerSpatialRandomEffects,
                                                  bool centerURandomEffects)
{
  
  NumericMatrix betaSamples((numberOfSamples / thin), standardizedX.cols() * numberOfResponses);
  NumericMatrix spatialRandomEffectsSamples((numberOfSamples / thin), spatialRandomEffects.size());
  NumericMatrix uRandomEffectsSamples((numberOfSamples / thin), uRandomEffects.size());
  NumericMatrix spatialTauSquaredSamples(numberOfSamples / thin, numberOfResponses);
  NumericMatrix spatialRhoSamples(numberOfSamples / thin, numberOfResponses);
  NumericMatrix varianceCovarianceUSamples(numberOfSamples / thin, (numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2));
  
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
    
    List output = multivariatePoissonNetworkLerouxSingleUpdate(standardizedX,
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
                                                                 covarianceBetaPrior,
                                                                 numberOfBetaBlocks,
                                                                 maxBetaBlockSize,
                                                                 betaTuningParameter,
                                                                 betaAcceptanceRate,
                                                                 numberOfAcceptedBetaDraws,
                                                                 numberOfAllAcceptedBetaDraws,
                                                                 spatialRandomEffectsTuningParameters,
                                                                 spatialRandomEffectsAcceptanceRate,
                                                                 numberOfAcceptedSpatialRandomEffectsDraws,
                                                                 numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                                 uRandomEffectsTuningParameters,
                                                                 uRandomEffectsAcceptanceRate,
                                                                 numberOfAcceptedUREDraws,
                                                                 numberOfAllAcceptedUREDraws,
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
                                                                 currentNumberOfIterations,
                                                                 betaFixed,
                                                                 spatialRandomEffectsFixed,
                                                                 uRandomEffectsFixed,
                                                                 spatialTauSquaredFixed,
                                                                 spatialRhoFixed,
                                                                 varianceCovarianceUFixed,
                                                                 trueBetaValues,
                                                                 trueSpatialRandomEffectsValues,
                                                                 trueURandomEffectsValues,
                                                                 trueSpatialTauSquaredValues,
                                                                 trueSpatialRhoValues,
                                                                 trueVarianceCovarianceUValues,
                                                                 centerSpatialRandomEffects,
                                                                 centerURandomEffects);
    
    
    beta = output[0];
    betaTuningParameter = output[1];
    betaAcceptanceRate = output[2];
    numberOfAcceptedBetaDraws = output[3];
    numberOfAllAcceptedBetaDraws = output[4];
    
    spatialRandomEffects = output[5];
    spatialRandomEffectsTuningParameters = output[6];
    spatialRandomEffectsAcceptanceRate = output[7];
    numberOfAcceptedSpatialRandomEffectsDraws = output[8];
    numberOfAllAcceptedSpatialRandomEffectsDraws = output[9];
    
    uRandomEffects = output[10];
    uRandomEffectsTuningParameters = output[11];
    uRandomEffectsAcceptanceRate = output[12];
    numberOfAcceptedUREDraws = output[13];
    numberOfAllAcceptedUREDraws = output[14];
    
    spatialTauSquared = output[15];
    
    spatialRho = output[16];
    spatialRhoTuningParameters = output[17];
    spatialRhoAcceptanceRate = output[18];
    numberOfAcceptedSpatialRhoDraws = output[19];
    numberOfAllAcceptedSpatialRhoDraws = output[20];
    
    varianceCovarianceU = as<NumericMatrix>(output[21]);
    
    NumericVector fittedValues = output[22];
    NumericVector logLikelihoods = output[23];
    
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
      
      fittedValuesSamples(sampleCounter, _) = fittedValues;
      logLikelihoodsSamples(sampleCounter, _) = logLikelihoods;
      
      sampleCounter++;
      
    }
    
    currentNumberOfIterations++;
    
  }
  
  List computationOfDIC = getMultivariatePoissonNetworkLerouxDIC(standardizedX,
                                                                   y,
                                                                   spatialAssignment,
                                                                   W,
                                                                   betaSamples,
                                                                   spatialRandomEffectsSamples,
                                                                   uRandomEffectsSamples,
                                                                   logLikelihoodsSamples,
                                                                   numberOfResponses);
  
  
  double DBar = computationOfDIC[0];
  double posteriorDeviance = computationOfDIC[1];
  double pd = computationOfDIC[2];
  double DIC = computationOfDIC[3];
  
  List modelOutput(20);
  
  modelOutput[0] = betaSamples;
  modelOutput[1] = spatialRandomEffectsSamples;
  modelOutput[2] = uRandomEffectsSamples;
  modelOutput[3] = spatialTauSquaredSamples;
  modelOutput[4] = spatialRhoSamples;
  modelOutput[5] = varianceCovarianceUSamples;
  
  modelOutput[6] = betaAcceptanceRate;
  modelOutput[7] = spatialRandomEffectsAcceptanceRate;
  modelOutput[8] = uRandomEffectsAcceptanceRate;
  modelOutput[9] = spatialRhoAcceptanceRate;
  
  modelOutput[10] = betaTuningParameter;
  modelOutput[11] = spatialRandomEffectsTuningParameters;
  modelOutput[12] = uRandomEffectsTuningParameters;
  modelOutput[13] = spatialRhoTuningParameters;
  
  modelOutput[14] = fittedValuesSamples;
  modelOutput[15] = logLikelihoodsSamples;
  
  modelOutput[16] = DBar;
  modelOutput[17] = posteriorDeviance;
  modelOutput[18] = pd;
  modelOutput[19] = DIC;
  
  return modelOutput;
  
}
