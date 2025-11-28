#include "kumaraswamyWarpingClass.h"

unsigned int KumaraswamyWarpingFunction::GetNumberOfParameters()
{
  return 2;
}

arma::rowvec KumaraswamyWarpingFunction::GetInitialPoint()
{
  return { 0, 0 };
}

arma::mat KumaraswamyWarpingFunction::ApplyWarping(const arma::mat &inputGrids,
                                                   const arma::mat &warpingParameters)
{
  arma::mat outputGrids(inputGrids.n_rows, inputGrids.n_cols);

  for (unsigned int i = 0;i < inputGrids.n_rows;++i)
  {
    double aValue = std::exp(warpingParameters(i, 0));
    double bValue = std::exp(warpingParameters(i, 1));
    outputGrids.row(i) = 1.0 - arma::pow(1.0 - arma::pow(inputGrids.row(i), aValue), bValue);
  }

  return outputGrids;
}

void KumaraswamyWarpingFunction::SetParameterBounds(const arma::rowvec &warpingOptions,
                                                    const arma::mat &inputGrids)
{
  m_ParameterLowerBounds = { warpingOptions(0), warpingOptions(0) };
  m_ParameterUpperBounds = { warpingOptions(1), warpingOptions(1) };
}

arma::mat KumaraswamyWarpingFunction::GetFinalWarping(const arma::cube &warpingParametersContainer,
                                                      const arma::urowvec &observationMemberships,
                                                      const arma::urowvec &clusterIndices)
{
  unsigned int numberOfObservations = warpingParametersContainer.n_rows;
  unsigned int numberOfParameters = warpingParametersContainer.n_cols;
  unsigned int numberOfIterations = warpingParametersContainer.n_slices;
  arma::mat warpingParameters(numberOfObservations, numberOfParameters, arma::fill::zeros);
  warpingParameters.col(0).ones();
  arma::colvec dilationParameters;
  arma::colvec shiftParameters;

  for (unsigned int i = 0;i < numberOfIterations;++i)
  {
    dilationParameters = warpingParametersContainer.slice(i).col(0);
    shiftParameters = warpingParametersContainer.slice(i).col(1);
    warpingParameters.col(0) %= dilationParameters;
    warpingParameters.col(1) %= dilationParameters;
    warpingParameters.col(1) += shiftParameters;
  }

  this->Normalize(warpingParameters, clusterIndices, observationMemberships);

  return warpingParameters;
}

void KumaraswamyWarpingFunction::Normalize(arma::mat &warpingParameters,
                                           const arma::urowvec &clusterIndices,
                                           const arma::urowvec &observationMemberships)
{
  return;

  arma::uvec observationIndices;
  arma::rowvec meanParameters;
  arma::mat clusterValues;

  for (unsigned int i = 0;i < clusterIndices.size();++i)
  {
    observationIndices = arma::find(observationMemberships == clusterIndices(i));
    clusterValues = warpingParameters.rows(observationIndices);

    meanParameters = arma::mean(clusterValues, 0);
    clusterValues.col(0) /= meanParameters(0);
    clusterValues.col(1) -= (meanParameters(1) * clusterValues.col(0));
    warpingParameters.rows(observationIndices) = clusterValues;
  }
}
