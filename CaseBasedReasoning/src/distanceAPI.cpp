#include "distanceAPI.h"

#include "./containers/RfDistanceContainer.h"


void distanceAPI::init() {};

arma::vec distanceAPI::calculate_distance(arma::mat& x) {
  int nrow = x.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  parallelDistance parallelDistance(x, dist_, nrow, output);
  RcppParallel::parallelFor(0, nrow, parallelDistance);
  return output;
};


/**
 * Weighted Distance Calculation
 */
void weightedDistanceAPI::init(arma::rowvec& weights) {
  this->set_distance(weights);
}

void weightedDistanceAPI::set_distance(arma::rowvec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  dist_ = std::make_shared<weightedDistance>(dist);
};


/**
 * Weighted XY Distance Calculation
 */
void weightedXYDistanceAPI::init(arma::rowvec& weights) {
  this->set_distance(weights);
}

void weightedXYDistanceAPI::set_distance(arma::rowvec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  dist_ = std::make_shared<weightedDistance>(dist);
};

arma::mat weightedXYDistanceAPI::calculate_distance(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  int ncols = y.n_rows;
  arma::mat output(nrow, y.n_rows);
  parallelDistanceNM parallelDistanceNM(x, y, dist_, nrow, output);
  parallelFor(0, nrow, parallelDistanceNM);
  return output;
};


/**
* RandomForests Terminal Node Distance
*/
Rcpp::DataFrame rfTerminalNodeDistanceAPI::calculate_distance(arma::umat& nodeIDs) {
  rangerForest rf(nodeIDs);
  return rf.nodeDistance().asDataFrame();
}


/**
 * RandomForests Proximity Matrix
 */
void rfProximityDistanceAPI::init(arma::mat& x) {
  this->set_distance(x);
}

void rfProximityDistanceAPI::set_distance(arma::mat& x) {
  rangerProximity dist;
  dist.set_parameters(x.n_cols);
  dist_ = std::make_shared<rangerProximity>(dist);
}


/**
 * RandomForests XY Proximity Matrix
 */
void rfProximityXYDistanceAPI::init(arma::mat& x, arma::mat& y) {
  this->set_distance(x);
};

arma::mat rfProximityXYDistanceAPI::calculate_distance(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  arma::mat output(nrow, y.n_rows);
  parallelDistanceNM parallelDistanceNM(x, y, dist_, nrow, output);
  parallelFor(0, nrow, parallelDistanceNM);
  return output;
};


/**
* RandomForests Depth Distance
*/
void rfDepthDistanceAPI::init(arma::umat& terminalNodeIDs) {
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  this->set_distance(nodeDists);
}

void rfDepthDistanceAPI::set_distance(RfDistContainer& nodeDists) {
  rfDepthDistance dist;
  dist.set_parameters(nodeDists);
  dist_ = std::make_shared<rfDepthDistance>(dist);
}

arma::vec rfDepthDistanceAPI::calculate_distance(arma::mat& xNodeIDs) {
  int nrow = xNodeIDs.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  parallelDistance parallelDistance(xNodeIDs, dist_, nrow, output);
  parallelFor(0, nrow, parallelDistance);
  return output;
};


/**
 * RandomForests XY Depth Distance
 */
void rfDepthXYDistanceAPI::init(arma::umat& terminalNodeIDs) {
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  this->set_distance(nodeDists);
}

arma::mat rfDepthXYDistanceAPI::calculate_distance(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
  int nrow = xNodeIDs.n_rows;
  arma::mat output(nrow, yNodeIDs.n_rows);
  parallelDistanceNM parallelDistanceNM(xNodeIDs, yNodeIDs, dist_, nrow, output);
  parallelFor(0, nrow, parallelDistanceNM);
  return output;
};
