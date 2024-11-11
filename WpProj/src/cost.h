#ifndef COST_H
#define COST_H

#include "WpProj_types.h"

void cost_calculation_L2sq(const refMatConst & A, const refMatConst & B, matrix & cost_matrix);
void cost_calculation_L2(const refMatConst & A, const refMatConst & B, matrix & cost_matrix);
void cost_calculation_L1(const refMatConst & A, const refMatConst & B, matrix & cost_matrix);
void cost_calculation_Lp(const refMatConst & A, const refMatConst & B, matrix & cost_matrix, double p);

#endif //COST_H
