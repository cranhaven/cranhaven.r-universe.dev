#ifndef INTEG_MULTI_REG_UTILS_H
#define INTEG_MULTI_REG_UTILS_H

#include <stdbool.h>

#include <R.h>
#include <Rinternals.h>
#include <gsl/gsl_rng.h>

/* R/C conversion helpers. R matrices are copied from column-major storage into
 * row-addressable C arrays because the sampler indexes observations first. */
SEXP array_to_r_list(double **array, int rows, int *cols);
SEXP c_array_to_r_matrix(double **array, int rows, int cols);
SEXP c_array_to_r_matrix_int(_Bool **array, int rows, int cols);

double **r_list_vector_double_to_c(int list_length, SEXP list_vector);
double ***r_list_matrix_to_c(int list_length, SEXP list_matrix);
double ****r_list_list_matrix_to_c(int list_length, SEXP list_list_matrix);
_Bool ****r_list_list_matrix_to_c_bool(int list_length, SEXP list_list_matrix);
void free_r_list_list_matrix_to_c(
    double ****array, int list_length, SEXP list_list_matrix);

/* Regression and distribution helpers. */
void ridge_predict_only(const double *x, const double *y, int n, int p,
                        double lambda, double *y_hat);
void fitted_ols(double *x, double *y, int n, int p, double *y_hat);
double r_lefttruncnorm(double lower, double mean, double sd);
double r_righttruncnorm(double upper, double mean, double sd);
double sample_left_truncated_normal_gsl(
    double mean, double sd, double lower, const gsl_rng *rng);

/* Accuracy metrics and small vector summaries. */
double auc(int n, double *prediction, _Bool *class_label);
double mean_squared_error(int n, double *prediction, double *observed);
double norm(int n, double *x);
double max(int n, double *x);
double min(int n, double *x);
double sum(int n, double *x);
double mean(int n, double *x);
double var(int n, double *x);

/* Matrix and array helpers. */
void mean_array_columns(int n, int n_cols, double **x, double *mean_out);
void mean_3d_array(int n, int n_rows, int n_cols,
                   double (*x)[n_rows][n_cols],
                   double mean_out[n_rows][n_cols]);
void matrix_multiply(int n_rows, int inner_dim, int n_cols,
                     double **left, double **right, double **product);
void matrix_vector_multiply(int n_rows, int n_cols, double **matrix,
                            double *vector, double *product);
_Bool bool_vectors_equal(int n, _Bool *left, _Bool *right);
double *column_means(int n_rows, int n_cols, double **x);
double *column_vars(int n_rows, int n_cols, double **x);
void normalize_columns(int n_rows, int n_cols, double **x);

/* Numerical Recipes style allocators used by the original sampler code. */
double *dvector(int nl, int nh);
double **dmatrix(int nrl, int nrh, int ncl, int nch);
_Bool *bvector(int nl, int nh);
_Bool **bmatrix(int nrl, int nrh, int ncl, int nch);
void free_dvector(double *v, int nl, int nh);
void free_dmatrix(double **m, int nrl, int nrh, int ncl, int nch);
void free_bmatrix(_Bool **m, int nrl, int nrh, int ncl, int nch);
void nrerror(char error_text[]);

#endif
