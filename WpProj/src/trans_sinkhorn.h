#ifndef TRANS_SINKHORN_H
#define TRANS_SINKHORN_H

#include "WpProj_types.h"
#include "utils.h"

void trans_sinkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
                    const matrix & exp_cost, 
                    matrix & A,
                    double eta, double epsilon, int niterations);
#endif //TRANS_SINKHORN_H
