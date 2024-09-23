

#define long int

/********** function prototypes **********************/
 
void profile_NR_noind(long *n_subject, long *subj_id,double *y,double *x,double *z,long *NXYcount,long *extracovar,double *covvec,long *n_extracov,double *beta,double *varmat,double *diff_factor,long *maxit,long *verbose,long *converge);

void profile_NR_ind(long *n_subject, long *subj_id,double *y,double *x,double *z,long *NXYcount,long *extracovar,double *covvec,long *n_extracov,double *beta,double *varmat,double *diff_factor,long *maxit,long *verbose,long *converge);

double *dqrinv(double *xvec, long n, double tol);
void dqrdc2_(double *, long *, long *, long *, double *, long *, double *, long *, double *);
void dqrcf_(double *, long *, long *, double *, double *, long *, double *, long *);

double **double_vec_to_mat(double *Yvec, long nrow, long ncol);

long **long_vec_to_mat(long *Yvec, long nrow, long ncol);

double **double_matrix(long nrow, long ncol);

long **long_matrix(long nrow, long ncol);

double *double_vec(long n);

long *long_vec(long n);

long *long_mat_to_vec(long **Ymat, long nrow, long ncol);

static void errmsg(char *string);

long max_long(long x, long y);
long min_long(long x, long y);

double max_double(double x, double y);
double min_double(double x, double y);

void print_matrix_long(long **m, long nrow, long ncol, FILE *file);
void print_matrix_double(double **m, long nrow, long ncol, FILE *file);
void print_vector_double(double *m, long n, FILE *file);
void print_vector_long(long *m, long n, FILE *file);

