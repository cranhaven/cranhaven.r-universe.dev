#define STRICT_R_HEADERS

//' @importFrom RcppParallel RcppParallelLibs

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "distance/distance.h"

#include <memory>

typedef tbb::concurrent_unordered_map<std::pair<int, int>, double> tbbUPMap;
typedef tbb::concurrent_unordered_map<int, double> tbbUMap;

// TODO: Representation of Results
// column-wise, row-wise, or full
#if RCPP_PARALLEL_USE_TBB

struct parallelDistance : public RcppParallel::Worker {
  const arma::mat& input_;
  std::shared_ptr<distance> dist_;
  const std::size_t nrow_;
  arma::vec& output_;
  
  parallelDistance(
    const arma::mat& input,
    const std::shared_ptr<distance> dist,
    const std::size_t nrow,
    arma::vec& output
  ) : input_(input), dist_(dist), nrow_(nrow), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin;i<end;++i) {
      for (std::size_t j=i+1;j<nrow_;++j) {
        output_((2 * i * nrow_ - i * i + 2 * j - 3 * i - 2) / 2) = dist_->calc_distance(input_.row(i), input_.row(j));
      }
    }
  }
};


struct parallelDistanceNM : public RcppParallel::Worker {
  const arma::mat& inputX_;
  const arma::mat& inputY_;
  std::shared_ptr<distance> dist_;
  const int nrow_;
  arma::mat& output_;
  
  parallelDistanceNM(
    const arma::mat& inputX,
    const arma::mat& inputY,
    const std::shared_ptr<distance> dist,
    const int nrow,
    arma::mat& output
  ) : inputX_(inputX), inputY_(inputY), dist_(dist), nrow_(nrow), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    std::size_t nrow2 = inputY_.n_rows;
    for (std::size_t i=begin;i<end;++i) {
      for (std::size_t j=0;j<nrow2;++j) {
        output_(i, j) = dist_->calc_distance(inputX_.row(i), inputY_.row(j));
      }
    }
  }
};


struct parallelMatrixNorm : public RcppParallel::Worker {
  const arma::mat& inputX_;
  const arma::mat& inputY_;
  std::shared_ptr<distance> dist_;
  arma::vec& output_;
  
  parallelMatrixNorm(
    const arma::mat& inputX,
    const arma::mat& inputY,
    const std::shared_ptr<distance> dist,
    arma::vec& output
  ) : inputX_(inputX), inputY_(inputY), dist_(dist), output_(output) {}
  
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin;i<end;++i) {
      output_(i) = dist_->calc_distance(inputX_.row(i), inputX_.row(i));
    }
  }
};

#else

// no single threated implementation

#endif
