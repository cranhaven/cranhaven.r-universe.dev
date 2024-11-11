#ifndef TRANS_RANK_H
#define TRANS_RANK_H

#include "WpProj_types.h"
#include "sort.h"

void trans_rank(const matrix & A, const matrix & B, int N, int M,
                      matrixI & idx, vector & mass, bool & a_sort);
#endif //TRANS_RANK_H
