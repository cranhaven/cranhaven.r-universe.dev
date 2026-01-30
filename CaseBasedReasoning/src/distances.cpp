// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "distanceAPI.h"

// [[Rcpp::export]]
arma::vec cpp_weightedDistance(arma::mat& x, arma::rowvec& weights) {
  weightedDistanceAPI dist;
  dist.init(weights);
  return dist.calculate_distance(x);
}

// [[Rcpp::export]]
arma::mat cpp_weightedDistanceXY(arma::mat& x, arma::mat& y, arma::rowvec& weights) {
  weightedXYDistanceAPI dist;
  dist.init(weights);
  return dist.calculate_distance(x, y);
}

/**
 * Ranger RandomForest related distances
 */
// [[Rcpp::export]]
Rcpp::DataFrame cpp_TerminalNodeDistance(arma::umat& terminalNodeIDs) {
  rfTerminalNodeDistanceAPI dist;
  dist.init();
  return dist.calculate_distance(terminalNodeIDs);
}

// [[Rcpp::export]]
arma::vec cpp_proximityMatrix(arma::mat& nodeIDs) {
  rfProximityDistanceAPI dist;
  dist.init(nodeIDs);
  return dist.calculate_distance(nodeIDs);
}

// [[Rcpp::export]]
arma::mat cpp_proximityMatrixRangerXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
  rfProximityXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs);
  return dist.calculate_distance(xNodeIDs, yNodeIDs);
}

// [[Rcpp::export]]
arma::vec cpp_depthMatrix(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthDistanceAPI dist;
  dist.init(terminalNodeIDs);
  return dist.calculate_distance(xNodeIDs);
}

// [[Rcpp::export]]
arma::mat cpp_depthMatrixRangerXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthXYDistanceAPI dist;
  dist.init(terminalNodeIDs);
  return dist.calculate_distance(xNodeIDs, yNodeIDs);
}
