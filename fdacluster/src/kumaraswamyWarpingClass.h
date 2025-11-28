#ifndef KUMARASWAMYWARPINGCLASS_H
#define KUMARASWAMYWARPINGCLASS_H

#include "baseWarpingClass.h"

class KumaraswamyWarpingFunction : public BaseWarpingFunction
{
  /**
   * A warping class for bounded functional data based on the cumulative
   * distribution function of the Kumaraswamy distribution:
   * F(x; a, b) = 1 - (1 - x^a)^b
   */

public:
  unsigned int GetNumberOfParameters();

  arma::rowvec GetInitialPoint();

  arma::mat ApplyWarping(
      const arma::mat &inputGrids,
      const arma::mat &warpingParameters
  );

  void SetParameterBounds(
      const arma::rowvec &warpingOptions,
      const arma::mat &inputGrids
  );

  arma::mat GetFinalWarping(
      const arma::cube &warpingParametersContainer,
      const arma::urowvec &observationMemberships,
      const arma::urowvec &clusterIndices
  );

  void Normalize(
      arma::mat &warpingParameters,
      const arma::urowvec &clusterIndices,
      const arma::urowvec &observationMemberships
  );
};

#endif /* KUMARASWAMYWARPINGCLASS_H */
