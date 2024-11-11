/********************************************************************************
 *
 * Bayesian Regression and Adaptive Sampling with Gaussian Process Trees
 * Copyright (C) 2005, University of California
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Questions? Contact Robert B. Gramacy (rbgramacy@ams.ucsc.edu)
 *
 ********************************************************************************/


#ifndef __MATRIX_H__
#define __MATRIX_H__

#include <stdio.h>

typedef enum FIND_OP {LT=101, LEQ=102, EQ=103, GEQ=104, GT=105, NE=106} FIND_OP;
typedef enum PRINT_PREC {HUMAN=1001, MACHINE=1002} PRINT_PREC;

void zero(double **M, unsigned int n1, unsigned int n2);
void id(double **M, unsigned int n);
double ** new_id_matrix(unsigned int n);
double ** new_zero_matrix(unsigned int n1, unsigned int n2);
double ** new_matrix(unsigned int n1, unsigned int n2);
int ** new_imatrix(unsigned int n1, unsigned int n2);
double ** new_matrix_bones(double *v, unsigned int n1, unsigned int n2);
double ** new_dup_matrix(double** M, unsigned int n1, unsigned int n2);
void dup_matrix(double** M1, double **M2, unsigned int n1, unsigned int n2);
double ** new_bigger_matrix(double** M, unsigned int n1, unsigned int n2,
			    unsigned int n1_new, unsigned int n2_new);
void delete_matrix(double** m);
void delete_imatrix(int** m);
void wmean_of_columns(double *mean, double **M, unsigned int n1, unsigned int n2,
		      double *weight);
void min_of_columns(double *s, double **M, unsigned int n1, unsigned int n2);
double* ones(unsigned int n, double scale);
int* iseq(double from, double to);
double quick_select_index(double arr[], int iarr[], int n, int k);

void sub_p_matrix(double **V, int *p, double **v,
		  unsigned int nrows, unsigned int lenp,
		  unsigned int col_offset);
void sub_p_matrix_rows(double **V, int *p, double **v,
		       unsigned int ncols, unsigned int lenp,
		       unsigned int row_offset);
double **new_p_submatrix_rows(int *p, double **v, unsigned int nrows,
			      unsigned int ncols, unsigned int row_offset);
double* new_vector(unsigned int n);
double* new_zero_vector(unsigned int n);
double* new_dup_vector(double* vold, unsigned int n);
void dupv(double *v, double* vold, unsigned int n);
double sumv(double *v, unsigned int n);
int sumiv(int *iv, unsigned int n);
int meaniv(int *iv, unsigned int n);
void zerov(double*v, unsigned int n);
int *new_ivector(unsigned int n);
void dupiv(int *iv_new, int *iv, unsigned int n);
void zeroiv(int*v, unsigned int n);
double sq(double x);
void vector_minmax(double*, int, double*, double*);
int *nearest_indices(const unsigned int, const unsigned int, double**,
		     const unsigned int, double**, unsigned int*, const int);
int fracvlen(double*, double, unsigned int);
void sub_p_matrix_rows_col(double*, int*, double**,
			   unsigned int, unsigned int);
void fill_vector(double *vec, double scalar, unsigned int n);
double* new_const_vector(double, unsigned int);
void sum_vector_scalar(double*, double, unsigned int);
int find_int(int*, int, unsigned int);
void prod_vector(double *, double *, unsigned int);
void divid_vector(double *, double *, unsigned int);
double var_vector(double *, double , unsigned int);
double* new_sq_vector(double *, unsigned int);
int ceil_divide(int, int);
void distance_sym_vec(double**, int, int, double*);
int remove_nonpos(double*, int);
void get_col(double*, double **, int, int);
#endif

