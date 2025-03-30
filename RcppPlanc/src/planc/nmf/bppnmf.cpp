//
// Created by andrew on 11/29/2023.
//
#include "bppnmf.hpp"
#include "hals.hpp"

using namespace planc;

template<>
inline void BPPNMF<arma::sp_mat>::computeNMF() {
    //   unsigned int currentIteration = 0;
#ifdef COLLECTSTATS
    // this->objective_err;
#endif
    // tic();
    // this->At = this->A.t();  // do it once
    // INFO << "At time::" << toc() << std::endl;
    // run hals once to get proper initializations
    HALSNMF tempHals(this->A, this->W, this->H);
    tempHals.num_iterations(2);
    this->W = tempHals.getLeftLowRankFactor();
    this->H = tempHals.getRightLowRankFactor();
#ifdef _VERBOSE
    INFO << PRINTMATINFO(this->At);
  INFO << " nnz = " << this->At.n_nonzero << std::endl;
  INFO << "Starting BPP for num_iterations()=" << this->num_iterations()
       << std::endl;
#endif
    this->commonSolve();
}
