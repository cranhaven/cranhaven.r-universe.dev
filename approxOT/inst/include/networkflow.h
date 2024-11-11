// from https://github.com/cran/transport/blob/f2cea92550cc15634a38e8163ebc68fc30ec6816/src/networksimplex.cpp
#include "approxOT_types.h"
#ifdef _OPENMP
# include <omp.h>
#endif
#include <iostream>
#include <vector>
#include "network_simplex_simple.h"
#include <stdio.h>

void trans_networkflow(const Eigen::VectorXd & a, const Eigen::VectorXd & b, 
                       const matrix & C, matrix & Tplan, int threads, bool acc,
                       int niter);
