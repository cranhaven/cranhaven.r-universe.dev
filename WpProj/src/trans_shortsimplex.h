#ifndef TRANS_SHORTSIMPLEX_H
#define TRANS_SHORTSIMPLEX_H

#include "WpProj_types.h"
extern "C" {
#include "shortsimplex.h"
}

void trans_shortsimplex(vectorI & mass_a, vectorI & mass_b, refMat cost_matrix, 
                        matrixI & assign_mat, matrixI &  basis_mat);
#endif //TRANS_SHORTSIMPLEX_H
