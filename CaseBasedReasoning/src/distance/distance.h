#ifndef DISTANCE_H
#define DISTANCE_H

// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "../containers/nodeDistContainer.h"

class distance {
public:
  distance() {};
  
  virtual double calc_distance(const arma::subview_row<double>& x, const arma::subview_row<double>& y) const {
    return 0.0;
  };
  
  void set_parameters() {};
};

// weighted distance
class weightedDistance final : public distance {
public:
  virtual double calc_distance(const arma::subview_row<double>& x, const arma::subview_row<double>& y) const {
    return arma::sum(arma::abs(weights_ % (x - y)));
  };
  
  void set_parameters(arma::rowvec& weights) {
    weights_ = weights;
  };
private:
  arma::rowvec weights_;
};

// random forest proximity
class rangerProximity : public distance {
public:
  virtual double calc_distance(const arma::subview_row<double>& x, const arma::subview_row<double>& y) const {
    int similarity = 0;
    for (std::size_t i=0;i<x.n_elem;++i) {
      if (x(i) == y(i)) {
        ++similarity;
      }
    }
    return similarity * 1. / nTrees_;
  };
  
  void set_parameters(std::uint32_t nTrees) {
    nTrees_ = nTrees;
  };
private:
  std::uint32_t nTrees_;
};

// random forest depth distance
class rfDepthDistance final : public distance {
public:
  virtual double calc_distance(const arma::subview_row<double>& x, const arma::subview_row<double>& y) const {
    double sum = 0.0;
    double d = 0.0;
    std::size_t nTree = 0;
    for (std::size_t t=0; t<nTrees_;++t) {
      if (x[t] < y[t]) {
        d = nodeDists_.getValue(x[t], y[t], t);
      } else if (x[t] > y[t]) {
        d = nodeDists_.getValue(y[t], x[t], t);
      } else {
        d = 0.0;
        ++nTree;
      }
      if (d > 0.0) {
        // TODO: set trafo
        sum += d;
        ++nTree;
      }
    }
    return sum; // * 1. / nTree
  };
  
  void set_parameters(RfDistContainer nodeDist) {
    nodeDists_ = nodeDist;
    nTrees_ = nodeDist.getNTree();
  };
private:
  RfDistContainer nodeDists_;
  std::uint32_t nTrees_;
};

#endif
