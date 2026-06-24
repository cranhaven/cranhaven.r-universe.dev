
#define MATTRANSMIX_H


void cpyk(double ***a, int nrows, int ncols, int k, double **b);
void cpyk2(double **a, int nrows, int ncols, double ***b, int k);
void cpy(double **a, int nrows, int ncols, double **b);

void array1to2(int a, int b, double *y, double **x);
void array2to1(int a, int b, double *y, double **x);
void array1to3(int a, int b, int c, double *y, double ***x);
void array3to1(int a, int b, int c, double *y, double ***x);

void Trans_trans(int p, int T, double *la, double *nu, double **Y, double **MY, int trans_type);
void Trans_trans_whole(int n, int p, int T, double *la, double *nu, double ***Y, double ***MY, int trans_type);

double mGpdf_Trans_Full(int p, int T, double *la, double *nu, double **Y, double **Mu, double **invS, double **invPsi, double detS, double detPsi, int trans_type);
double mGloglik_Trans_Full(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double ***invS, double ***invPsi, double *detS, double *detPsi, double *scale, int trans_type);
void Estep_Trans_Full(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double ***invS, double ***invPsi, double *detS, double *detPsi, double **gamma, int trans_type);

double Q1_same(int n, int p, int T, double *la_nonzero, double *nu, double ***Y, double *gamma_k, double **invSk, double **invPsik, int Mu_type, int trans_type, int la_type);
double Q1(int n, int p, int T, double *la_nonzero, double *nu, double ***Y, double *gamma_k, double **invSk, double **invPsik, int Mu_type, int trans_type, int la_type);
double Q2(int n, int p, int T, double *nu_nonzero, double *la, double ***Y, double *gamma_k, double **invSk, double **invPsik, int Mu_type, int trans_type);

double simplex1(double (*func)(int, int, int, double *, double *, double ***, double *, double **, double **, int, int, int), int n1, int p, int T, double *nu, double ***X, double *gamma_k, double **invSk, double **invPsik, double *start, double EPSILON, double scale, int Mu_type, int trans_type, int la_type);

double simplex2(double (*func)(int, int, int, double *, double *, double ***, double *, double **, double **, int, int), int n1, int p, int T, double *la, double ***X, double *gamma_k, double **invSk, double **invPsik, double *start, double EPSILON, double scale, int Mu_type, int trans_type);

double Mstep_Trans_Full(int p, int T, int n, int K, double *misc_double, double ***Y, double **la, double **nu, double **gamma, double ***invS, double ***Mu, double ***invPsi, double *detS, double *detPsi, double *tau, int Mu_type, int Sigma_type, int Psi_type, int la_type, int trans_type);
void EM_Trans_Full(int p, int T, int n, int K, double ***Y, double **la, double **nu, int max_iter, double *misc_double, double *tau, double ***Mu, double ***invS, double ***invPsi, double *detS, double *detPsi, double **gamma, int *id, double *ll, int *conv, int Mu_type, int Sigma_type, int Psi_type, int la_type, double *scale, int trans_type);


void modelA1(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);
void modelA2(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);
void modelA3(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);
void modelA5(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);
void modelA6(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);
void modelA4(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);
void svd(double **A, double *S2, int n);
void findPsi2phi(int n, int p, int T, double *par, double ***MY, double **Muk, double *gamma_k, double **invSk, double eps);
double eq3(double x, double *coeff);
void rootfinding(double (*func)(double, double *), double *start, double *coeff, double eps);
void modelA7(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detPsi, int trans_type);


void modelB1(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB2(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB3(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB4(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB5(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB6(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB7(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB8(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB9(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB10(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB11(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB12(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB13(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);
void modelB14(int p, int T, int n, int K, double ***Y, double **la, double **nu, double *tau, double ***Mu, double **gamma, double ***invS, double ***invPsi, double *detS, int trans_type);

void EigValDec(int size, double *W, double **A, double (*determinant));
void Anull3(double ***X, int ax, int bx, int cx);
void Anull(double **X, int ax, int bx);
void anull(double *x, int p);
void anulli(int *x, int p);
void XAXt(double **X, int p, double **A, double **Res);
void cpy1(double ***a, int k, int nrows, int ncols, double **b);
void dsyev_(char *JOBZp, char *UPLOp,int *Np, double *A, int *LDAp, double *Wp, double *WORK, int *LWORK, int *INFOp);
int mat_(int a, int b,double **Res, double **Y);
void tA(double **A, int a, int b, double **Res);
void multiply(double **a, int arows, int acols, double **b, int brows, int bcols, double **c);
void cpyv(double **A, int col, int nrows, double *V);
void matxvec(double **a, int arows, int acols, double *x, int xrows, double *y);




void dsyev_(char *JOBZp, char *UPLOp,int *Np, double *A, int *LDAp, double *Wp, double *WORK, int *LWORK, int *INFOp);
void EigValDec(int size, double *W, double **A, double (*determinant));
