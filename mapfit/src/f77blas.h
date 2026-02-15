#ifndef MAPFIT_F77BLASW_H
#define MAPFIT_F77BLASW_H

// level 1
void f77dcopy(int n, const double *x, int incx, double *y, int incy);
void f77dscal(int n, double alpha, double *x, int incx);
void f77daxpy(int n, double alpha, const double *x, int incx, double *y, int incy);

double f77ddot(int n, const double *x, int incx, const double *y, int incy);
double f77dasum(int n, const double *x, int incx);
double f77dsum(int n, const double *x, int incx);
double f77dnrm2(int n, const double *x, int incx);
int f77idamax(int n, const double *x, int incx);

// level 2
void f77dgemv(char trans, int m, int n, double alpha,
              const double *A, int lda, const double *x, int incx,
              double beta, double *y, int incy);

void f77dger(int m, int n, double alpha,
             const double *x, int incx, const double *y, int incy,
             double *A, int lda);

// level 3
void f77dgemm(char transA, char transB,
              int m, int n, int k, double alpha,
              const double *A, int lda, const double *B, int ldb,
              double beta, double *C, int ldc);
// lapack
int f77dgesv(int n, int nrhs, double *A, int lda, int *ipiv, double *B, int ldb);

extern "C" {
  void dcopy_(const int *n, const double *x, const int *incx, double *y, const int *incy);
  void dscal_(const int *n, const double *alpha, double *x, const int *incx);
  void daxpy_(const int *n, const double *alpha, const double *x, const int *incx, double *y, const int *incy);
  double ddot_(const int *n, const double *x, const int *incx, const double *y, const int *incy);
  double dasum_(const int *n, const double *x, const int *incx);
  double dnrm2_(const int *n, const double *x, const int *incx);
  int idamax_(const int *n, const double *x, const int *incx);
  void dgemv_(const char *trans, const int *m, const int *n, const double *alpha, const double *A, const int *lda,
              const double *x, const int *incx, const double *beta, double *y, const int *incy);
  void dger_(const int *m, const int *n, const double *alpha, const double *x, const int *incx,
             const double *y, const int *incy, double *A, const int *lda);
  void dgemm_(const char *transa, const char *transb, const int *m, const int *n, const int *k, const double *alpha,
              const double *A, const int *lda, const double *B, const int *ldb, const double *beta, double *C, const int *ldc);
  void dgesv_(int *n, int *nrhs, double *A, int *lda, int *ipiv, double *B, int *ldb, int *info);
}

// level 1

inline
  void f77dcopy(int n, const double *x, int incx, double *y, int incy) {
    dcopy_(&n, x, &incx, y, &incy);
  }

inline
  void f77dscal(int n, double alpha, double *x, int incx) {
    dscal_(&n, &alpha, x, &incx);
  }

inline
  void f77daxpy(int n, double alpha, const double *x, int incx, double *y, int incy) {
    daxpy_(&n, &alpha, x, &incx, y, &incy);
  }

inline
  double f77ddot(int n, const double *x, int incx, const double *y, int incy) {
    return ddot_(&n, x, &incx, y, &incy);
  }

inline
  double f77dasum(int n, const double *x, int incx) {
    return dasum_(&n, x, &incx);
  }

inline
  double f77dnrm2(int n, const double *x, int incx) {
    return dnrm2_(&n, x, &incx);
  }

inline
  int f77idamax(int n, const double *x, int incx) {
    return idamax_(&n, x, &incx) - 1;
  }

// level 2

inline
  void f77dgemv(char trans, int m, int n, double alpha,
                const double *A, int lda, const double *x, int incx,
                double beta, double *y, int incy) {
    dgemv_(&trans, &m, &n, &alpha, A, &lda, x, &incx, &beta, y, &incy);
  }

inline
  void f77dger(int m, int n, double alpha,
               const double *x, int incx, const double *y, int incy,
               double *A, int lda) {
    dger_(&m, &n, &alpha, x, &incx, y, &incy, A, &lda);
  }

// level 3

inline
  void f77dgemm(char transA, char transB,
                int m, int n, int k, double alpha,
                const double *A, int lda, const double *B, int ldb,
                double beta, double *C, int ldc) {
    dgemm_(&transA, &transB, &m, &n, &k, &alpha, A, &lda, B, &ldb, &beta, C, &ldc);
  }

// lapack
inline
  int f77dgesv(int n, int nrhs, double *A, int lda, int *ipiv, double *B, int ldb) {
    int info;
    dgesv_(&n, &nrhs, A, &lda, ipiv, B, &ldb, &info);
    return info;
  }

#endif
