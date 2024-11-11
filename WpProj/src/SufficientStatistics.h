#ifndef SUFFICIENT_STATISTICS_H
#define SUFFICIENT_STATISTICS_H

#include "WpProj_types.h"
#include <vector>
#include "sort.h"
#include "utils.h"
#include "transport.h"

using namespace Rcpp;


// void sufficient_stat(const refMatConst & X, refMat Y,
//                      refMat theta,
//                      const bool not_same,
//                      const int S, const int P, const int N,
//                      const double wt,
//                      matrix & xtx, matrix & xty, matrix & theta_norm,
//                      const Rcpp::CharacterVector & method);// shell function depending on method
void sufficient_stat(const refMatConst & X, refMat Y,
                     refMat theta,
                     const bool not_same,
                     const int S, const int P, const int N,
                     matrix & xtx, matrix & xty,
                     const Rcpp::CharacterVector & method,
                     const std::string & transport_method,
                     double epsilon, int niter);
// 
// 
// void xtxy_update(const refMatConst & X, 
//                  const refMatConst & sorted_Y, //y needs to be pre-sorted for scale/loc.scale
//                  const refMatConst & theta, 
//                  matrix & theta_norm,
//                  const refMatConst & result,
//                  matrix & mu,
//                  const int S, 
//                  const int N,
//                  const int D,
//                  const double pseudo_obs,
//                  const int sampleIdx,
//                  matrix & xtx, 
//                  matrix & xty,
//                  matrixI & idx_mu, 
//                  const Rcpp::CharacterVector & method);
// 
void xty_update(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted for method = scale/loc.scale
                const refMatConst & theta,
                const refMatConst & result,
                matrix & mu,
                const int S, const int N, const int P,
                matrix & xty, matrixI & idx_mu,
                const Rcpp::CharacterVector & method,
                const std::string & transport_method,
                double epsilon, int niter); //shell function depending on method
  
#endif //SUFFICIENT_STATISTICS_H
