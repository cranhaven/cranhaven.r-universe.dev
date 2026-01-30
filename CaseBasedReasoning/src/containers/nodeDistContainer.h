#ifndef RFDISTCONTAINER_H
#define RFDISTCONTAINER_H

#include <Rcpp.h>
#include <unordered_map>

#include "hash.h"

// borrowed from: https://github.com/dselivanov/text2vec/blob/master/src/SparseTripletMatrix.h
class RfDistContainer {
public:
  RfDistContainer() {};
  
  void setNTree(uint32_t nTrees) {
    ntrees_ = nTrees;
  };
  
  uint32_t getNTree() {
    return ntrees_;
  };
  
  void addValue(uint32_t i, uint32_t j, uint32_t tree, uint32_t value) {
    // check for key pair
    if (this->container_.find(std::make_pair(i, j)) == this->container_.end()) {
      arma::vec vec(ntrees_);
      vec.fill(-1);
      if (i > j) {
        this->container_[std::make_pair(j, i)] = vec;
      } else {
        this->container_[std::make_pair(i, j)] = vec;
      }
    }
    if (i > j) {
      this->container_[std::make_pair(j, i)][tree] = value;
    } else {
      this->container_[std::make_pair(i, j)][tree] = value;
    }
  };
  
  int getValue(int i, int j, int tree) const {
    // return -1 if there is no pair
    if (this->container_.find(std::make_pair(i, j)) == this->container_.end()) { return -1.;}
    return this->container_.at(std::make_pair(i, j))[tree];
  };
  
  arma::vec getValues(uint32_t i, uint32_t j) const {
    // return -1 vector if there is no pair
    if (this->container_.find(std::make_pair(i, j)) == this->container_.end()) { 
      arma::vec vec(ntrees_);
      vec.fill(-1);
      return vec;
    }
    return this->container_.at(std::make_pair(i, j));
  };
  
  Rcpp::DataFrame asDataFrame() const {
      Rcpp::NumericVector x, y, dist, null;
      Rcpp::CharacterVector namevec;
      Rcpp::List lists(ntrees_ + 2);

      std::vector<double> v(ntrees_, -1);
      std::vector<std::vector <double> > dists(ntrees_);
      namevec.push_back("x");
      namevec.push_back("y");

      std::string namestem = "tree_";
      for(auto it1 = this->container_.begin(); it1 != this->container_.end(); ++it1) {
        x.push_back(it1->first.first);
        y.push_back(it1->first.second);
        for (std::size_t n=0;n<ntrees_;++n) {
          dists[n].push_back(it1->second[n]);
        }
      }
      lists[0] = x;
      lists[1] = y;
      for (std::size_t k=0;k<ntrees_;++k) {
        namevec.push_back(namestem + std::to_string(k + 1));
        lists[k + 2] = dists[k];
      }
      lists.attr("names") = namevec;
      Rcpp::DataFrame df(lists);
      return df;
  }
  
  void setDataFrame(Rcpp::DataFrame df) {
    
  };
  
private:
  uint32_t ntrees_;
  std::unordered_map< std::pair<uint32_t, uint32_t>, arma::vec> container_;
};

#endif

