#ifndef SORT_H
#define SORT_H

#include "approxOT_types.h"
#include <vector>


using namespace Rcpp;



//' Returns orders for a vector
//'
//' @param v A vector of values of class Eigen::VectorXd
//' @return a std::vector<size_t> of indexes
//' @keywords internal
std::vector<size_t> sort_indexes( const vector & v);

//' Returns orders for a column of a matrix
//'
//' @param v A vector of values of class Eigen::MatrixXd::ColXpr
//' @param idx A std::vector<size_t> of indexes
//' @return void
//' @keywords internal
void sort_indexes_col(const matrix::ColXpr & v, std::vector<size_t> & idx) ;

//' Returns orders for a vector
//'
//' @param v A vector of values of class Eigen::VectorXd
//' @param idx A std::vector<size_t> of indexes
//' @return void
//' @keywords internal
void sort_indexes( vector & v, std::vector<size_t> & idx) ;


//' Returns orders for a vector
//'
//' @param v A vector of values of class Eigen::VectorXd
//' @param idx A std::vector<size_t> of indexes
//' @return void
//' @keywords internal
void sort_indexes( const vector & v, std::vector<size_t> & idx);

//' Returns orders for the columns of a matrix
//'
//' @param v A reference to an Eigen::MatrixXd
//' @param idx An Eigen::MatrixXi to hold the column orders 
//' @return void
//' @keywords internal
void sort_indexes_bycol_Eigenmat(const refMatConst & v, matrixI & idx); //checked

//' Returns orders for the rows of a matrix
//'
//' @param v A reference to an Eigen::MatrixXd
//' @param idx An Eigen::MatrixXi to hold the row orders 
//' @return void
//' @keywords internal
void sort_indexes_byrow_Eigenmat(const refMatConst & v, matrixI & idx);

//' Returns orders for the rows of a matrix
//'
//' @param v An Eigen::MatrixXd
//' @param idx An Eigen::MatrixXi to hold the row orders 
//' @return void
//' @keywords internal
void sort_indexes_byrow_Eigenmat(const matrix & v, matrixI & idx);

//' Returns orders for the vectorized version of a matrix
//'
//' @param v An Eigen::MatrixXd
//' @param idx An Eigen::MatrixXi to hold the total orders of the matrix
//' @return void
//' @keywords internal
void sort_indexes_byrow_totalentry(const matrix & v, matrixI & idx);

//' Returns ranks for the vectorized version of a matrix
//'
//' @param v An Eigen::MatrixXd
//' @param rank An Eigen::MatrixXi to hold the total ranks of the matrix entries
//' @return void
//' @keywords internal
void rank_mat(const matrix & v, matrixI & rank);

//' Sorts a matrix by column
//'
//' @param v A reference to a Eigen::MatrixXd
//' @return void
//' @keywords internal
void sort_matrix_by_col(refMat v);

//' Sorts a matrix by row
//'
//' @param v A reference to a Eigen::MatrixXd
//' @return void
//' @keywords internal
void sort_matrix_by_row(refMat v);

//' Sorts a matrix by column relative to an index
//'
//' @param v A reference to a Eigen::MatrixXd
//' @param idx A reference to a column of an Eigen::MatrixXi with sort indexes
//' @return void
//' @keywords internal
void rel_sort_matrix_by_col(refMat v, Eigen::DenseBase<Eigen::Matrix<int, -1, -1, 0, -1, -1> >::ColXpr idx);

//' Sorts a matrix by column relative to an index
//'
//' @param v A reference to a Eigen::MatrixXd
//' @param idx A vector of integers with sort indexes
//' @return void
//' @keywords internal
void rel_sort_matrix_by_col(refMat v, vectorI & idx);

//' Sorts a matrix relative to an index
//'
//' @param v A reference to a Eigen::MatrixXd
//' @param idx A reference to a column of an Eigen::MatrixXi with sort indexes
//' @return void
//' @keywords internal
void rel_sort_matrix_by_entry(refMat v, 
                              Eigen::DenseBase<Eigen::Matrix<int, -1, -1, 0, -1, -1> >::ColXpr idx);

//' Sorts a vector relative to an index
//'
//' @param idx A vector<size_t> with sort indexes
//' @param v An Eigen::VectorXd to be sorted
//' @return void
//' @keywords internal
void rel_sort(const std::vector<size_t> & idx, vector & y) ;

//' Sorts a vector relative to an index
//'
//' @param idx A reference to a column of an Eigen::MatrixXi with sort indexes
//' @param v An Eigen::VectorXd to be sorted
//' @return void
//' @keywords internal
void rel_sort(const matrixI::ColXpr & idx, vector & y);

void rel_sorted_1(const refArrayConstI&  idx, 
                  vector & y, const refArrayConst& yorig); //checked

#endif //SORT_H
