#ifndef TRANS_APPROX_OT_H
#define TRANS_APPROX_OT_H

#include "WpProj_types.h"
#include "round_feasible.h"
#include "trans_sinkhorn.h"
#include "trans_greenkhorn.h"
#include "utils.h"

void trans_approxOT(const refVecConst & mass_a, const refVecConst & mass_b, 
                    refMat cost_matrix, 
                    matrix & assign_mat,
                    double epsilon, int niterations,
                    const std::string & method);
#endif //TRANS_SINKHORN_H
