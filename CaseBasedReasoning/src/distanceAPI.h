#ifndef DISTANCEAPI_H
#define DISTANCEAPI_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "distance/distance.h"
#include "parallelFrameworks.h"

#include "ranger/rangerForest.h"


/**
* Distance Calculation
*/
class distanceAPI {
public:
  distanceAPI() {};
  void init();
  arma::vec calculate_distance(arma::mat& x);
  
protected:
  void set_distance();
  std::shared_ptr<distance> dist_;
};


/**
 * Weighted Distance Calculation
 */
class weightedDistanceAPI : public distanceAPI {
public:
  void init(arma::rowvec& weights);
  
protected:
  void set_distance(arma::rowvec& weights);
};


/**
 * Weighted XY Distance Calculation
 */
class weightedXYDistanceAPI : public distanceAPI {
public:
  void init(arma::rowvec& weights);
  arma::mat calculate_distance(arma::mat& x, arma::mat& y);
  
protected:
  virtual void set_distance(arma::rowvec& weights);
};


/**
 * RandomForests Terminal Node Distance
 */
class rfTerminalNodeDistanceAPI : public distanceAPI {
public:
  Rcpp::DataFrame calculate_distance(arma::umat& nodeIDs); 
};


/**
 * RandomForests Proximity Matrix
 */
class rfProximityDistanceAPI : public distanceAPI {
public:
  void init(arma::mat& x);
  
protected:
  void set_distance(arma::mat& x);
};


/**
 * RandomForests XY Proximity Matrix
 */
class rfProximityXYDistanceAPI : public rfProximityDistanceAPI {
public:
  void init(arma::mat& x, arma::mat& y);
  arma::mat calculate_distance(arma::mat& x, arma::mat& y);
};


/**
 * RandomForests Depth Distance Calculation
 */
class rfDepthDistanceAPI : public distanceAPI {
public:
  void init(arma::umat& terminalNodeIDs);
  arma::vec calculate_distance(arma::mat& xNodeIDs);
  
protected:
  void set_distance(RfDistContainer& nodeDists);
};


/**
 * RandomForests XY Depth Distance Calculation
 */
class rfDepthXYDistanceAPI : public rfDepthDistanceAPI {
public:
  void init(arma::umat& terminalNodeIDs);
  arma::mat calculate_distance(arma::mat& xNodeIDs, arma::mat& yNodeIDs);
};

#endif
