#ifndef TRANS_GREENKHORN_H
#define TRANS_GREENKHORN_H

#include "WpProj_types.h"
#include "utils.h"

void trans_greenkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
                    const matrix & exp_cost, 
                    matrix & A,
                    double eta, double epsilon, int niterations);
#endif //TRANS_GREENKHORN_H
