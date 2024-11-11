#ifndef SORT_H
#define SORT_H

#include "WpProj_types.h"
#include <vector>


using namespace Rcpp;



// std::vector<size_t> sort_indexes( matrix::ConstColXpr & v);

std::vector<size_t> sort_indexes( const vector & v);

// void sort_indexes(const matrix::ConstColXpr & v, std::vector<size_t> & idx) ;

void sort_indexes_col(const matrix::ColXpr & v, std::vector<size_t> & idx) ;

void sort_indexes( vector & v, std::vector<size_t> & idx) ;

void sort_indexes( const vector & v, std::vector<size_t> & idx);

void sort_indexes_bycol_Eigenmat(const refMatConst & v, matrixI & idx); //checked

void sort_indexes_byrow_Eigenmat(const refMatConst & v, matrixI & idx);

void sort_indexes_byrow_Eigenmat(const matrix & v, matrixI & idx);

void sort_indexes_byrow_totalentry(const matrix & v, matrixI & idx);

// void sort_matrix(refMat v) ;

void rank_mat(const matrix & v, matrixI & rank);

void sort_matrix_by_col(refMat v);
void sort_matrix_by_row(refMat v);

void rel_sort_matrix_by_col(refMat v, Eigen::DenseBase<Eigen::Matrix<int, -1, -1, 0, -1, -1> >::ColXpr idx);

void rel_sort_matrix_by_col(refMat v, vectorI & idx);


void rel_sort_matrix_by_entry(refMat v, 
                              Eigen::DenseBase<Eigen::Matrix<int, -1, -1, 0, -1, -1> >::ColXpr idx);

void rel_sort(const std::vector<size_t> & idx, vector & y) ;

void rel_sort(const matrixI::ColXpr & idx, vector & y);

// template <typename Derived> 
void rel_sorted_1(const refArrayConstI&  idx, 
                  vector & y, const refArrayConst& yorig); //checked

// vector sort_partial(refMat v, int middle);

#endif //SORT_H
