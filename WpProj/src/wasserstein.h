#ifndef WASSERSTEIN_H
#define WASSERSTEIN_H

#include "WpProj_types.h"
#include "sort.h"

double wasserstein(const refVec & mass_a, const refVec & mass_b,
                   const refMat & cost, const double p, 
                   const refVecI & from,  const refVecI & to);

double wasserstein_2_iid_2(refMat X, refMat Y);

#endif //WASSERSTEIN_H
