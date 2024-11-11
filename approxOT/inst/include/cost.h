#ifndef COST_H
#define COST_H

#include "approxOT_types.h"

//' Fills in the values of a cost matrix
//'
//' @param A The data matrix for sample 1 with obsrevations unique by column
//' @param B The data matrix for sample 2 with obsrevations unique by column
//' @param cost_matrix The empty cost matrix of class 
//' Eigen::MatrixXd with dimensions A.col() by B.cols()
//' @return void
//' @details Calculates the \eqn{L_2} cost raised to the power of 2
//' @keywords internal
void cost_calculation_L2sq(const refMatConst & A, const refMatConst & B, matrix & cost_matrix);


//' Fills in the values of a cost matrix
//'
//' @param A The data matrix for sample 1 with obsrevations unique by column
//' @param B The data matrix for sample 2 with obsrevations unique by column
//' @param cost_matrix The empty cost matrix of class 
//' Eigen::MatrixXd with dimensions A.col() by B.cols()
//' @return void
//' @details Calculates the \eqn{L_2} cost
//' @keywords internal
void cost_calculation_L2(const refMatConst & A, const refMatConst & B, matrix & cost_matrix);

//' Fills in the values of a cost matrix
//'
//' @param A The data matrix for sample 1 with obsrevations unique by column
//' @param B The data matrix for sample 2 with obsrevations unique by column
//' @param cost_matrix The empty cost matrix of class 
//' Eigen::MatrixXd with dimensions A.col() by B.cols()
//' @return void
//' @details Calculates the \eqn{L_1} cost
//' @keywords internal
void cost_calculation_L1(const refMatConst & A, const refMatConst & B, matrix & cost_matrix);

//' Fills in the values of a cost matrix
//'
//' @param A The data matrix for sample 1 with obsrevations unique by column
//' @param B The data matrix for sample 2 with obsrevations unique by column
//' @param cost_matrix The empty cost matrix of class 
//' Eigen::MatrixXd with dimensions A.col() by B.cols()
//' @return void
//' @details Calculates the \eqn{L_p} cost
//' @keywords internal
void cost_calculation_Lp(const refMatConst & A, const refMatConst & B, matrix & cost_matrix, double p);

#endif //COST_H
