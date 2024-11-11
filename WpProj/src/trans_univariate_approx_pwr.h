#ifndef TRANS_UNIVARIATE_APPROXIMATION_PWR_H
#define TRANS_UNIVARIATE_APPROXIMATION_PWR_H

#include "WpProj_types.h"
#include "sort.h"

void  trans_univariate_approx_pwr(const matrix & A, const matrix & B, int N, int M,
                                  matrixI & idx, vector & mass, bool & a_sort);
#endif //TRANS_UNIVARIATE_APPROXIMATION_PWR_H
