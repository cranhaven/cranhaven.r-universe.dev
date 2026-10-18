#define ARMA_DONT_USE_BLAS
#define ARMA_DONT_USE_OPENMP
#define ARMA_DONT_USE_NEWARP
#define ARMA_DONT_USE_ARPACK

#ifndef attribute_H
#define attribute_H

#include "helper_functions.h"
#include <string>
// #include <random>
// #include <set>
// #include <iostream>
// #include <unordered_map>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

struct IsMoreThan {
  IsMoreThan(int i) : i_{i} {}
  bool operator()(int i) { return i > i_; }
  int i_;
};

class IGLM_API Attribute {
public:
  Attribute (int a, std::string family_, double scale_);
  Attribute (int a, arma::vec attribute_tmp, std::string family_, double scale_);
  
  // Attributes (public)
  arma::vec attribute;
  double scale; 
  std::string family;
  
  // Member functions
  double get_scale() const {
    return(scale);
  }
  
  bool check() const;
  
  int get_sum() const {
    return(attribute.size());
  }
  
  double get_val(int from) const {
    if(from < 1 || from > n_units) return 0.0;
    return(attribute(from-1)/scale);
  }
  
  double get_val_no_scale(int from) const {
    if(from < 1 || from > n_units) return 0.0;
    return(attribute(from-1));
  }
  
  void print();
  
  void set_attr_0(int from) {
    if(from >= 1 && from <= n_units) {
        attribute(from-1) = 0;
    }
  }
  
  void set_attr_value(int from, double val) {
    if(from >= 1 && from <= n_units) {
        attribute(from-1) = val;
    }
  }
  
  void set_attr_from_armavec(arma::vec attribute_tmp) {
    attribute = attribute_tmp;
  }
  
  void set_attr_1(int from) {
    if(from >= 1 && from <= n_units) {
        attribute(from-1) = 1;
    }
  }

private:
  int n_units;
};

#endif

