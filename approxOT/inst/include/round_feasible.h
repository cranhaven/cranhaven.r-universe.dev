#ifndef TRANS_ROUND_FEASIBLE_H
#define TRANS_ROUND_FEASIBLE_H

#include "approxOT_types.h"

//' Rounds approximate transport plans to the feasible set
//'
//' @param F the transport plan, a matrix of class Eigen::MatrixXd 
//' @param mass_a The sample weights of the first sample of data
//' @param mass_b The sample weights of the second sample of data
//' @return void
//' @keywords internal
void round_feasible(matrix & F, const refVecConst & mass_a, 
                    const refVecConst & mass_b);
#endif //TRANS_ROUND_FEASIBLE_H
