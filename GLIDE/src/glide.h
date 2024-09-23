
double **double_matrix(int nrow, int ncol);
void print_matrix_double(double **m, int nrow, int ncol, FILE *file);
//double **generate_modlematrix(double **xmat,double *genotype);
void generate_modlematrix(int nrow,int ncol,double *v_xmat,double *v_genotype,int col,char opt, double *resmat);
void compute_cormat(int *nsnp,int *n_subject,int *ncol_xmat,double *v_yfit,double *v_y,double *v_xmat,double *v_genotype,double *v_cormat);
void compute_cormat_col(int *nsnp,int *n_subject,int *colnumber,int *ncol_xmat,double *v_yfit,double *v_y,double *v_xmat,double *v_genotype,double *v_cormat_col);
//static void errmsg(char *string);
double **double_vec_to_mat(double *Yvec, int nrow, int ncol, char opt);
double *double_mat_to_vec(double **Ymat, int nrow, int ncol,char opt);
int *int_vec(int n);
void mydgemm (int M,int N,int K,double *A, double *B, double *C);
//void dgemm_(const char *transa, const char *transb, const int *m, const int *n, const int *k, const double *alpha, const double *a, const int *lda, const double *b, const int *ldb, const double *beta, double *c, const int *ldc);
void dgemm_(const char *transa, const char *transb, const int *m, const int *n, const int *k, const double *alpha, const double *a, const int *lda, const double *b, const int *ldb, const double *beta, double *c, const int *ldc, size_t,  size_t);
void dqrinv(double *xvec, int n, double tol, double *outvec);
void dqrdc2_(double *, int *, int *, int *, double *, int *, double *, int *, double *);
void dqrcf_(double *, int *, int *, double *, double *, int *, double *, int *);

//void dgemm_(char *TRANSA, char*TRANSB, int *M, int *N, int *K, double *ALPHA, double **A, int *LDA, double **B, int *LDB, double *BETA, double **C, int *LDC);
//F77_NAME(dgemm)(const char *transa, const char *transb, const int *m,const int *n, const int *k, const double *alpha,const double *a, const int *lda,const double *b, const int *ldb,const double *beta, double *c, const int *ldc);
double *double_vec(int n);
void print_vector_double(double *m, int n, FILE *file);
void multiplicationbyrow(double *A,int m,int n,double *B,double *C);
void myrnorm(int n,double miu,double sigma, double *z);
double myrnorm1(double miu,double sigma);
int compare (const void * a, const void * b);
