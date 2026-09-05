#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_multifit.h>
#include <R.h>
#include <Rinternals.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include "utils.h"
#include "my_header.h"
static const double t4 = 0.45;


/*
 * Ridge regression predictor:
 *   y_hat = X (X^T X + λI)^(-1) X^T y
 *
 * Inputs:
 *   X      : pointer to double array (row-major) of size n * p
 *   y      : pointer to double array of length n
 *   n, p   : number of samples and predictors
 *   lambda : ridge penalty
 *
 * Output:
 *   y_hat  : pointer to double array of length n (predicted values)
 */
void ridge_predict_only(const double *X, const double *y,
                        int n, int p, double lambda,
                        double *y_hat)
{
    gsl_matrix_const_view Xv = gsl_matrix_const_view_array(X, n, p);
    gsl_vector_const_view yv = gsl_vector_const_view_array(y, n);
    gsl_vector_view yhatv = gsl_vector_view_array(y_hat, n);

    gsl_matrix *XtX = gsl_matrix_alloc(p, p);
    gsl_matrix *XtX_lambdaI = gsl_matrix_alloc(p, p);
    gsl_vector *Xty = gsl_vector_alloc(p);
    gsl_vector *beta = gsl_vector_alloc(p);

    // XtX = X^T * X
    gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, &Xv.matrix, &Xv.matrix, 0.0, XtX);

    // XtX + λI
    gsl_matrix_memcpy(XtX_lambdaI, XtX);
    for (int i = 0; i < p; i++) {
        double val = gsl_matrix_get(XtX_lambdaI, i, i) + lambda;
        gsl_matrix_set(XtX_lambdaI, i, i, val);
    }

    // Xty = X^T * y
    gsl_blas_dgemv(CblasTrans, 1.0, &Xv.matrix, &yv.vector, 0.0, Xty);

    // Solve (XtX + λI) * beta = Xty
    int signum;
    gsl_permutation *perm = gsl_permutation_alloc(p);
    gsl_linalg_LU_decomp(XtX_lambdaI, perm, &signum);
    gsl_linalg_LU_solve(XtX_lambdaI, perm, Xty, beta);

    // Compute y_hat = X * beta
    gsl_blas_dgemv(CblasNoTrans, 1.0, &Xv.matrix, beta, 0.0, &yhatv.vector);

    // Free memory
    gsl_matrix_free(XtX);
    gsl_matrix_free(XtX_lambdaI);
    gsl_vector_free(Xty);
    gsl_vector_free(beta);
    gsl_permutation_free(perm);
}
// Function to fit OLS and get predicted values

void fitted_ols(double * X_data, double * y_data,int n, int p, double *ypred){
    gsl_matrix_view X = gsl_matrix_view_array(X_data, n, p);
    gsl_vector_view y = gsl_vector_view_array(y_data, n);
    gsl_vector *c = gsl_vector_alloc(p);       // coefficients
    gsl_matrix *cov = gsl_matrix_alloc(p, p);  // covariance matrix
    double chisq;

    gsl_multifit_linear_workspace *work = gsl_multifit_linear_alloc(n, p);

    gsl_multifit_linear(&X.matrix, &y.vector, c, cov, &chisq, work);

    /*
    printf("Estimated coefficients:\n");
    for(int i = 0; i < p; i++) {
        printf("beta[%d] = %g\n", i, gsl_vector_get(c, i));
    }
    */

    gsl_multifit_linear_free(work);

      gsl_vector *y_pred = gsl_vector_alloc(n);
    gsl_blas_dgemv(CblasNoTrans, 1.0, &X.matrix, c, 0.0, y_pred);

    //printf("Predicted values:\n");
    for(int i = 0; i < n; i++) {
       ypred[i]= gsl_vector_get(y_pred, i);
    }

    gsl_vector_free(c);
    gsl_vector_free(y_pred);
    gsl_matrix_free(cov);


}


float generate_normal(const float sigma)
{

  // srand(1);
  float x, y, r2;

  do
  {
    /* choose x,y in uniform square (-1,-1) to (+1,+1) */
  //  x = -1 + 2 * ((double)rand() + 1.) / (1. + (double)RAND_MAX);
    //y = -1 + 2 * ((double)rand() + 1.) / (1. + (double)RAND_MAX);

// Clean, standard R-compatible uniform sampling between -1 and 1
x = -1.0 + 2.0 * unif_rand();
y = -1.0 + 2.0 * unif_rand();
    // printf("X=%2.5f \n",x);
    // printf("Y=%2.5f \n",y);
    /* see if it is in the unit circle */
    r2 = x * x + y * y;
  } while (r2 > 1.0 || r2 == 0);


  /* Box-Muller transform */
  return sigma * y * sqrt(-2.0 * log(r2) / r2);
}

/* The exponential distribution has the form

   p(x) dx = exp(-x/mu) dx/mu

   for x = 0 ... +infty */

double rexponential(const double mu)
{
  //double u = ((double)rand() + 1.) / (1. + (double)RAND_MAX);
  //return -mu * log1p(-u);
  return mu*exp_rand();
}

// Generae from truncated normal distribution

/* Exponential rejection sampling (a,inf) */
double ers_a_inf(double a)
{
  // SAMPLER_DEBUG("ers_a_inf", a, R_PosInf);
  const double ainv = 1.0 / a;
  double x, z, rho;
  do
  {
    // x = rexp(ainv) + a; /* rexp works with 1/lambda */
    x = rexponential(ainv) + a;
    z= x - a;
    rho = exp(-0.5 * z * z);
  } while (unif_rand() > rho);
  return x;
}

/* Normal rejection sampling (a,inf) */
double nrs_a_inf(double a)
{
  // SAMPLER_DEBUG("nrs_a_inf", a, R_PosInf);
  // double x = -DBL_AX;
  double x = generate_normal(1.0);
  // double x = gsl_ran_ugaussian(r);
  while (x < a)
  {
    x = generate_normal(1.0);
    // x = gsl_ran_ugaussian(r);
  }
  return x;
}

double r_lefttruncnorm(double a, double mean, double sd)
{
  const double alpha = (a - mean) / sd;
  if (alpha < t4)
  {
    return mean + sd * nrs_a_inf(alpha);
  }
  else
  {
    return mean + sd * ers_a_inf(alpha);
  }
}
double r_righttruncnorm(double b, double mean, double sd)
{
  const double beta = (b - mean) / sd;
  /* Exploit symmetry: */
  return mean - sd * r_lefttruncnorm(-beta, 0.0, 1.0);
}





SEXP array_to_r_list(double **array, int rows, int *cols)
{
    // Allocate list of length = rows
    SEXP list = PROTECT(allocVector(VECSXP, rows));
    for (int i = 0; i < rows; i++)
    {
        // Allocate numeric vector of length = cols
        SEXP vec = PROTECT(allocVector(REALSXP, cols[i]));
        double *vec_ptr = REAL(vec);

        // Copy row i to numeric vector
        for (int j = 0; j < cols[i]; j++)
        {
            vec_ptr[j] = array[i][j];
        }
        // Set ith element of list
        SET_VECTOR_ELT(list, i, vec);
        UNPROTECT(1); // vec
    }
    UNPROTECT(1); // list
    return list;
}

// Function to convert a 2D C array to an R matrix
SEXP c_array_to_r_matrix_int(_Bool **array, int rows, int cols)
{
    // Step 1: Allocate a numeric vector of size rows * cols
    SEXP matrix = PROTECT(allocVector(INTSXP, rows * cols));
    int *matrix_data = INTEGER(matrix);

    // Step 2: Copy the data into the matrix in column-major order
    for (int col = 0; col < cols; ++col)
    {
        for (int row = 0; row < rows; ++row)
        {
            matrix_data[row + col * rows] = (int)array[row][col]; // R uses column-major order
        }
    }
    // Step 3: Set dimension attribute to convert it into a matrix
    SEXP dims = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dims)[0] = rows;
    INTEGER(dims)[1] = cols;
    setAttrib(matrix, R_DimSymbol, dims);
    UNPROTECT(2); // matrix, dims
    return matrix;
}

// Function to convert a 2D C array to an R matrix
SEXP c_array_to_r_matrix(double **array, int rows, int cols)
{
    // Step 1: Allocate a numeric vector of size rows * cols
    SEXP matrix = PROTECT(allocVector(REALSXP, rows * cols));
    double *matrix_data = REAL(matrix);

    // Step 2: Copy the data into the matrix in column-major order
    for (int col = 0; col < cols; ++col)
    {
        for (int row = 0; row < rows; ++row)
        {
            matrix_data[row + col * rows] = array[row][col]; // R uses column-major order
        }
    }

    // Step 3: Set dimension attribute to convert it into a matrix
    SEXP dims = PROTECT(allocVector(INTSXP, 2));
    INTEGER(dims)
    [0] = rows;
    INTEGER(dims)
    [1] = cols;
    setAttrib(matrix, R_DimSymbol, dims);

    UNPROTECT(2); // matrix, dims
    return matrix;
}

double **r_list_vector_double_to_c(int listlength, SEXP LictVect)
{
    double **ListVect_c = malloc(listlength * sizeof(double *));
    for (int i = 0; i < listlength; i++)
    {
        SEXP mPM = VECTOR_ELT(LictVect, i);
        // Instead of getting dims via getAttrib, use the total length
        int sizeM = LENGTH(mPM);
        ListVect_c[i] = malloc(sizeM * sizeof(double));
        for (int j = 0; j < sizeM; j++)
        {
            ListVect_c[i][j] = REAL(mPM)[j];
        }
    }
    return ListVect_c;
}

double ***r_list_matrix_to_c(int listlength, SEXP ListMat)
{
    // ListMat is a list of matrices from R
    double ***newYY_arr = (double ***)malloc(listlength * sizeof(double **));

    for (int i = 0; i < listlength; i++)
    {
        SEXP mYY = VECTOR_ELT(ListMat, i);
        SEXP dimsYY = getAttrib(mYY, R_DimSymbol);
        //if (dimsYY == R_NilValue)
        //{
          //  UNPROTECT(2);
        //    error("ListMat element %d does not have dimension attributes.", i);
        //}
        int n_rows_yy = INTEGER(dimsYY)[0];
        int n_col_yy = INTEGER(dimsYY)[1];
        double *dataYY = REAL(mYY);
        // Allocate an array for row pointers if you intend to access newCC in 2D fashion.
        newYY_arr[i] = (double **)malloc(n_rows_yy * sizeof(double *));
        for (int r = 0; r < n_rows_yy; r++)
        {
            newYY_arr[i][r] = (double *)malloc(n_col_yy * sizeof(double));
            for (int c = 0; c < n_col_yy; c++)
            {
                newYY_arr[i][r][c] = dataYY[c * n_rows_yy + r]; // Still column-major indexing for newCC.
            }
        }
    }
    return newYY_arr;
}

_Bool ****r_list_list_matrix_to_c_bool(int listlength, SEXP ListListMat)
{
    // ListListMat is a list of list of matrices in R. Make sure that the data type is integer in R
    _Bool ****X0 = (_Bool ****)malloc(listlength * sizeof(_Bool ***));

    // Loop over each subgroup.
    for (int i = 0; i < listlength; i++)
    {
        SEXP subgroup = VECTOR_ELT(ListListMat, i);
        int n_platforms = LENGTH(subgroup);

        // Allocate space for the platforms within the subgroup.
        X0[i] = (_Bool ***)malloc(n_platforms * sizeof(_Bool **));

        // Loop over each platform.
        for (int j = 0; j < n_platforms; j++)
        {
            SEXP df = VECTOR_ELT(subgroup, j);

            // Get the matrix dimensions.
            SEXP dims = getAttrib(df, R_DimSymbol);
            int n_rows = INTEGER(dims)[0];
            int n_cols = INTEGER(dims)[1];

            // Get the pointer to the matrix data (numeric array).
            int *data_ptr = INTEGER(df);

            // Allocate an array to hold pointers to each row.
            X0[i][j] = (_Bool **)malloc(n_rows * sizeof(_Bool *));

            // Assign each row pointer; note R stores matrices in column-major order.
            for (int r = 0; r < n_rows; r++)
            {
                X0[i][j][r] = (_Bool *)malloc(n_cols * sizeof(_Bool));
                for (int c = 0; c < n_cols; c++)
                {
                    X0[i][j][r][c] = (_Bool)data_ptr[c * n_rows + r];
                }
            }
        }
    }
    return X0;
}

void free_r_list_list_matrix_to_c(double ****X0, int listlength, SEXP ListListMat)
{
    for (int i = 0; i < listlength; i++)
    {
        SEXP subgroup = VECTOR_ELT(ListListMat, i);
        int n_platforms = LENGTH(subgroup);

        for (int j = 0; j < n_platforms; j++)
        {
            SEXP df = VECTOR_ELT(subgroup, j);
            SEXP dims = getAttrib(df, R_DimSymbol);
            int n_rows = INTEGER(dims)[0];
            // printf("number of rows: %d \n", n_rows);

            for (int r = 0; r < n_rows; r++)
            {
                free(X0[i][j][r]);
                X0[i][j][r] = NULL;
            }
            free(X0[i][j]);
            X0[i][j] = NULL;
        }
        free(X0[i]);
        X0[i] = NULL;
    }
    free(X0);
    X0 = NULL;
}

double ****r_list_list_matrix_to_c(int listlength, SEXP ListListMat)
{
    // ListListMat is a list of list of matrices in R
    double ****X0 = (double ****)malloc(listlength * sizeof(double ***));

    // Loop over each subgroup.
    for (int i = 0; i < listlength; i++)
    {
        SEXP subgroup = VECTOR_ELT(ListListMat, i);
        int n_platforms = LENGTH(subgroup);

        // Allocate space for the platforms within the subgroup.
        X0[i] = (double ***)malloc(n_platforms * sizeof(double **));

        // Loop over each platform.
        for (int j = 0; j < n_platforms; j++)
        {
            SEXP df = VECTOR_ELT(subgroup, j);

            // If the matrix is not numeric but is logical, coerce it.
            if (!isReal(df) && isLogical(df))
            {
                df = coerceVector(df, REALSXP);
            }

            // Get the matrix dimensions.
            SEXP dims = getAttrib(df, R_DimSymbol);
            int n_rows = INTEGER(dims)[0];
            int n_cols = INTEGER(dims)[1];

            // Get the pointer to the matrix data (numeric array).
            double *data_ptr = REAL(df);

            // Allocate an array to hold pointers to each row.
            X0[i][j] = (double **)malloc(n_rows * sizeof(double *));

            // Assign each row pointer; note R stores matrices in column-major order.
            for (int r = 0; r < n_rows; r++)
            {
                X0[i][j][r] = (double *)malloc(n_cols * sizeof(double));
                for (int c = 0; c < n_cols; c++)
                {
                    X0[i][j][r][c] = data_ptr[c * n_rows + r];
                }
            }
        }
    }
    return X0;
}


void compute_mrf_normalizer(int p, double **theta, double nu, double *mrf)
{
    double mrfc = 0;
    int ss = 1 << p;
    int i, j, j1;
    int bj, bj1;
    for (i = 0; i < ss; i++)
    {
        int b = 0;
        double bc = 0;
        for (j = p - 1; j >= 0; j--)
        {
            bj = ((int)floor(i * (1.0 / (1 << j)))) % 2; // gives all the binary combinations
            b += bj;
            for (j1 = 0; j1 < j; j1++)
            {
                bj1 = (int)floor(i * (1.0 / (1 << j1))) % 2;
                bc += 2 * bj * bj1 * theta[j][j1];
            }
            bc += pow(bj, 2) * theta[j][j];
        }
        mrfc += exp(nu * b + bc);
    }
    *mrf = mrfc;
}

void sort_descending_index(int n, double *x, int *idx)
{
    int i, j;
    double a;
    int id;
    for (i = 0; i < n; i++)
        idx[i] = i;
    for (i = 0; i < n; ++i)
    {
        for (j = i + 1; j < n; ++j)
        {
            if (x[i] <= x[j])
            {
                a = x[i];
                id = idx[i];
                idx[i] = idx[j];
                x[i] = x[j];
                idx[j] = id;
                x[j] = a;
            }
        }
    }
}


double auc(int n, double *esti, _Bool * class)
{
    double fpr[n + 2], tpr[n + 2];
    double auc1 = 0;
    int P = 0; // P=positive instances
    int i, j;
    double esti1[n];
    for (i = 0; i < n; i++)
    {
        esti1[i] = esti[i];
        if (class[i] == 1)
            P += 1;
    }
    int idx[n];
    sort_descending_index(n, esti1, idx);

    fpr[n + 1] = 1;
    tpr[n + 1] = 1;
    fpr[0] = 0;
    tpr[0] = 0;
    for (i = n; i >= 1; --i)
    {
        double af = 0;
        double at = 0;
        for (j = 0; j < n; j++)
        {
            if (esti[j] > esti1[i - 1])
            {
                if (class[j] == 0)
                {
                    af += 1;
                }
                else
                {
                    at += 1;
                }
            }
        }
        tpr[i] = at / P;
        fpr[i] = af / (n - P);
        auc1 += (fpr[i + 1] - fpr[i]) * (tpr[i + 1] + tpr[i]);
    }
    auc1 += (fpr[1] - fpr[0]) * (tpr[1] + tpr[0]);
    auc1 = 0.5 * (auc1);
    return auc1;
}

double sample_left_truncated_normal_gsl(double mu, double sd, double lower, const gsl_rng *r)
{
    // This functon generates a univariate truncate normal distribution at lower. It uses an accept and reject algorithm
    double lowern = (lower - mu) / sd;
    double alphaopt = (lowern + sqrt(pow(lowern, 2) + 4)) / 2;
    double z = lowern + gsl_ran_exponential(r, 1 / alphaopt);
    double qz = exp(-pow(z - alphaopt, 2) / 2);
    double u = gsl_ran_flat(r, 0, 1);
    // int nmax=4;
    // int i=0;
    // while ((u>qz)||(i<nmax)){
    while (u > qz)
    {
        z = lowern + gsl_ran_exponential(r, 1 / alphaopt);
        qz = exp(-pow(z - alphaopt, 2) / 2);
        u = gsl_ran_flat(r, 0, 1);
        // i++;
    }
    return z * sd + mu;
}

void mean_3d_array(int n, int n1, int n2, double (*x)[n1][n2], double me[n1][n2])
{
    int i, j, l;
    for (i = 0; i < n1; i++)
    {
        for (j = 0; j < n2; j++)
        {
            me[i][j] = 0;
            for (l = 0; l < n; l++)
                me[i][j] += x[l][i][j] / n;
        }
    }
}
void mean_array_columns(int n, int n1, double **x, double *me)
{
    int i, l;
    for (i = 0; i < n1; i++)
    {
        me[i] = 0;
        for (l = 0; l < n; l++)
            me[i] += x[l][i] / n;
    }
}

void matrix_multiply(int n, int K1, int p, double **Mat1, double **Mat2, double **ProdMat)
{
    int i, j, k;
    double a;
    for (i = 0; i < n; i++)
    {
        for (j = 0; j < p; j++)
        {
            a = 0;
            for (k = 0; k < K1; k++)
            {
                a += Mat1[i][k] * Mat2[k][j];
            }
            ProdMat[i][j] = a;
        }
    }
}
void matrix_vector_multiply(int n, int K1, double **Mat, double *Vec, double *ProdVec)
{
    int i, k;
    double a;
    for (i = 0; i < n; i++)
    {
        a = 0;
        for (k = 0; k < K1; k++)
        {
            a += Mat[i][k] * Vec[k];
        }
        ProdVec[i] = a;
    }
}

_Bool bool_vectors_equal(int n, _Bool *u, _Bool *v)
{
    int i;
    for (i = 0; i < n; i++)
    {
        if (u[i] != v[i])
            return 0;
    }
    return 1;
}

double norm(int n, double *x)
{
    double normx = 0;
    int i;
    for (i = 0; i < n; i++)
    {
        normx += pow(x[i], 2);
        // printf("NormXXX==%f \n",x[i]);
    }
    return sqrt(normx);
}

double max(int n, double *x)
{
    double xmax = x[0];
    int i;
    for (i = 0; i < n; i++)
    {
        if (x[i] > xmax)
            xmax = x[i];
    }
    return xmax;
}

double min(int n, double *x)
{
    double xmin = x[0];
    int i;
    for (i = 0; i < n; i++)
    {
        if (x[i] < xmin)
            xmin = x[i];
    }
    return xmin;
}

void normalize_columns(int nR, int nC, double **x)
{
    double *Colmea = column_means(nR, nC, x);
    double *ColVar = column_vars(nR, nC, x);
    int i, j;
    for (i = 0; i < nR; i++)
        for (j = 0; j < nC; j++)
            x[i][j] = (x[i][j] - Colmea[j]) / sqrt(ColVar[j]);
    free(Colmea);
    free(ColVar);
}

double *column_means(int nR, int nC, double **x)
{
    int i, j;
    double *Mean = malloc(nC * sizeof(double));
    for (j = 0; j < nC; j++)
    {
        double me = 0;
        for (i = 0; i < nR; i++)
            me += x[i][j];
        Mean[j] = me / nR;
    }
    return Mean;
}
double *column_vars(int nR, int nC, double **x)
{
    int i, j;
    double *Colmea = column_means(nR, nC, x);
    double *ColVar = malloc(nC * sizeof(double));
    for (j = 0; j < nC; j++)
    {
        double va = 0;
        for (i = 0; i < nR; i++)
            va += (x[i][j] - Colmea[j]) * (x[i][j] - Colmea[j]);
        ColVar[j] = va / (nR - 1);
    }
    free(Colmea);
    return ColVar;
}

double sum(int n, double *x)
{
    int i;
    double sum = 0;
    for (i = 0; i < n; i++)
        sum += x[i];
    return sum;
}

double mean(int n, double *x)
{
    int i;
    double me = 0;
    for (i = 0; i < n; i++)
        me += x[i];
    return me / n;
}
double mean_squared_error(int n, double *x, double*y)
{
    int i;
    double mse = 0;
    for (i = 0; i < n; i++)
        mse += (x[i] - y[i]) * (x[i] - y[i]);
    return mse / n;
}


double var(int n, double *x)
{
    int i;
    double me = mean(n, x);
    double va = 0;
    for (i = 0; i < n; i++)
        va += (x[i] - me) * (x[i] - me);
    return va / (n - 1);
}

double *dvector(int nl, int nh)
{
    double *v;

    v = (double *)malloc((unsigned)(nh - nl + 1) * sizeof(double));
    if (!v)
        nrerror("allocation failure in dvector()");
    return v - nl;
}

double **dmatrix(int nrl, int nrh, int ncl, int nch)
{
    int i;
    double **m;

    m = (double **)malloc((unsigned)(nrh - nrl + 1) * sizeof(double *));
    if (!m)
        nrerror("allocation failure 1 in dmatrix()");
    m -= nrl;

    for (i = nrl; i <= nrh; i++)
    {
        m[i] = (double *)malloc((unsigned)(nch - ncl + 1) * sizeof(double));
        if (!m[i])
            nrerror("allocation failure 2 in dmatrix()");
        m[i] -= ncl;
    }
    return m;
}

_Bool *bvector(int nl, int nh)
{
    _Bool *v;

    v = (_Bool *)malloc((nh - nl + 1) * sizeof(_Bool));
    if (!v)
        nrerror("allocation failure in dvector()");
    return v - nl;
}

_Bool **bmatrix(int nrl, int nrh, int ncl, int nch)
{
    int i;
    _Bool **m;

    m = (_Bool **)malloc((nrh - nrl + 1) * sizeof(_Bool *));
    if (!m)
        nrerror("allocation failure 1 in dmatrix()");
    m -= nrl;

    for (i = nrl; i <= nrh; i++)
    {
        m[i] = (_Bool *)malloc((nch - ncl + 1) * sizeof(_Bool));
        if (!m[i])
            nrerror("allocation failure 2 in dmatrix()");
        m[i] -= ncl;
    }
    return m;
}

void free_dvector(double *v, int nl, int nh)
{
    free((char *)(v + nl));
}

void free_dmatrix(double **m, int nrl, int nrh, int ncl, int nch)
{
    int i;

    for (i = nrh; i >= nrl; i--)
        free((char *)(m[i] + ncl));

    free((char *)(m + nrl));
}

void free_bmatrix(_Bool **m, int nrl, int nrh, int ncl, int nch)
{
    int i;

    for (i = nrh; i >= nrl; i--)
        free((char *)(m[i] + ncl));

    free((char *)(m + nrl));
}

void nrerror(char error_text[])
{
    Rprintf("Utils run-time error...\n");
    Rprintf("%s\n", error_text);
    Rf_error("...now exiting to system...\n");
    // exit(1);
}
