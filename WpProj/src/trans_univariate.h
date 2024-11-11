#ifndef TRANS_UNIVARIATE_H
#define TRANS_UNIVARIATE_H

#include "WpProj_types.h"
#include "sort.h"

void trans_univariate(const vector & A, const vector & B, int N, int M,
                      matrixI & idx, vector & mass, bool & a_sort);
#endif //TRANS_UNIVARIATE_H
