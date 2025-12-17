// ---------------------------------------------------------
//  fanc.c
//	Authors: K. Hirose, M. Yamamoto, H. Nagata
// ---------------------------------------------------------

#define USE_FC_LEN_T
#include <math.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Arith.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Applic.h>
//extern "C" {
	#include "ezprof.h"
	#include "zeroin.h"
//}

#ifndef FCONE
# define FCONE
#endif

struct ctrl_fanc_t {
	double tol_EM, tol_CD, tol_Psi, tol_Phi, eta, zita, delta;
	double *init_coef, *rhos, *gammas, *max_rho, max_gamma, min_gamma, gamma_ebic, *w;
	int num_rhos, num_gammas;
	double alpha_powerseq;
	int maxit_EM, maxit_CD, maxit_Phi;
	int cor_factor, use_penalty, zero_min_rho;
	int cold_init, num_iter_init, num_iter_init_prenet, p_max_for_S;
	int progress, omp, num_threads, model;
};

struct logF_value_t {
	double logF, penalty, logF_pen;
};

// ---------------------------------------------------------
//  Debug utilities
// ---------------------------------------------------------

int g_report_flag = 0;
#define REPIF(b) ((g_report_flag & (b)) == (b))
/*
static void print_dmat(int nrow, int step, int ncol, double *x) {
	for (int i = 0; i < nrow; i++) {
		for (int j = 0; j < ncol; j++) {
			Rprintf("%s%f", j == 0 ? "" : ", ", x[i + j * step]);
		}
		Rprintf("\n");
	}
}
*/
struct ezprof_t g_ezprof_, *g_ezprof = &g_ezprof_;

// ---------------------------------------------------------
//  Common utilities
// ---------------------------------------------------------

const double LARGE_RHO = 8.25;

const int INC_SEQ[1] = {1};
const char JOBZ_VAL[2] = "N";
const char UPLO_UPPER[2] = "U";
const char TRANS_N[2] = "N";
const char TRANS_T[2] = "T";

const double D_ZERO = 0;
const double D_ONE = 1;
const int I_ONE = 1;

#define SWAP(t,p,q) do {t o=p; p=q; q=o;} while(0)

static void dfill(int n, double *buf, double v) {
	for (int i = 0; i < n; i++) {
		buf[i] = v;
	}
}

static void ifill(int n, int *buf, int v) {
	for (int i = 0; i < n; i++) {
		buf[i] = v;
	}
}

static void pfill(int n, void **buf, void *p) {
	for (int i = 0; i < n; i++) {
		buf[i] = p;
	}
}

static double dmax(double a, double b) {
	return a > b ? a : b;
}

static int dsignum(double s) {
	return s > 0 ? 1 : (s < 0 ? -1 : 0);
}

static double dsumsq(int n, double *a) {
	double sum = 0;
	for (int i = 0; i < n; i++) {
		sum += a[i] * a[i];
	}
	return sum;
}

static double dnorm2(int n, double *a, double *b) {
	double sum = 0;
	for (int i = 0; i < n; i++) {
		sum += (a[i] - b[i]) * (a[i] - b[i]);
	}
	return sqrt(sum);
}

static double trace(int n, double *mat) {
	double sum = 0;
	for (int i = 0; i < n; i++) {
		sum += mat[i + i * n];
	}
	return sum;
}

static double logdet(int n, double *mat, int *O_info) {
	void *vmax = vmaxget();
	int nn = n * n;
	int lwork = n * 64;
	double work[lwork];
	int info = 0xDEADBEEF;
	
	// Calculate eigen(mat)
	double *tmp = (double *)R_alloc(nn, sizeof(double));
	double *eigval = (double *)R_alloc(n, sizeof(double));
	F77_CALL(dcopy)(&nn, mat, INC_SEQ, tmp, INC_SEQ);
	F77_CALL(dsyev)(
		JOBZ_VAL, UPLO_UPPER, &n, tmp, &n, eigval, work, &lwork, &info FCONE FCONE);
	
	// Calculate log(det(mat))
	double logdet = 0;
	for (int i = 0; i < n; i++) {
		logdet += log(eigval[i]);
	}
	
	if (O_info != NULL) *O_info = info;
	vmaxset(vmax);
	return logdet;
}

static void inv(int n, double *mat, double *O_inv_mat, int *O_info) {
	int nn = n * n;
	int lwork = n * 64;
	double work[lwork];
	int ipiv_mat[n];
	int info = 0xDEADBEEF;
	F77_CALL(dcopy)(&nn, mat, INC_SEQ, O_inv_mat, INC_SEQ);
	F77_CALL(dgetrf)(&n, &n, O_inv_mat, &n, ipiv_mat, &info);
	F77_CALL(dgetri)(&n, O_inv_mat, &n, ipiv_mat, work, &lwork, &info);
	if (O_info != NULL) *O_info = info;
}

static double logseq(int n, double a, double b, double *arr) {
	double log_a = log(a), log_b = log(b);
	if(n == 1) arr[0] = a;
	if(n > 1){
		for (int i = 0; i < n; i++) {
			arr[i] = exp((log_b - log_a) * i / (n - 1) + log_a);
		}
	}
	return 0; 
}


static double h(double x, double alpha) {
	//return alpha^(-1.0) * C^(1.0-alpha) * ((x + C)^alpha - C^alpha);
	double ans;
	if(alpha == 0.0) ans = log(x);
	if(alpha != 0.0) ans = pow(x, alpha);
	return ans;
}
static double hinv(double y, double alpha) {
	double ans;
	if(alpha == 0.0) ans = exp(y);
	if(alpha != 0.0) ans = exp(log(y)/alpha);
	return ans;
}

static double powerseq(int n, double a, double b, double alpha, double *arr) {
	double h_a = h(a, alpha), h_b = h(b, alpha);
	if(n == 1) arr[0] = a;
	if(n > 1){
		for (int i = 0; i < n; i++) {
			arr[i] = hinv((h_b - h_a) * i / (n - 1) + h_a, alpha);
		}
	}
	return 0; 
}


static double seq(int n, double a, double b, double *arr) {
	for (int i = 0; i < n; i++) {
		arr[i] = (b-a) * i / (n - 1) + a;
	}
	return 0; 
}


// ---------------------------------------------------------
//  solution for MC+ in one dimension
// ---------------------------------------------------------

double MC(double theta, double rho, double gamma) {
	if (rho==0.0) {
		return 0.0;
	} else if (fabs(theta) < rho * gamma) {
		return rho * (fabs(theta) - theta * theta / (2.0 * rho * gamma));
	} else {
		return rho * rho * gamma / 2.0;
	}
}

double MC_onedimension(
	double theta, double theta_tilde, double rho, double gamma)
{
	return 
		0.5 * (theta - theta_tilde) * (theta - theta_tilde) + 
		MC(theta, rho, gamma);
}


double S_MC(double theta, double rho, double gamma) {
	double signtheta = dsignum(theta);
	double delta = signtheta * (fabs(theta) - rho) / (1.0 - 1.0 / gamma);
	if (gamma > 1.0) {
		if (fabs(theta) > rho * gamma) {
			return theta;
		} else if (fabs(theta) <= rho) {
			return 0.0;
		} else {
			return delta;
		}
	} else if (gamma < 1.0){
		warning("wgamma is less than 1.0");
		double kouho1 = MC_onedimension(theta, theta, rho, gamma);
		double kouho2 = MC_onedimension(0.0, theta, rho, gamma);
		double kouho3 = 
			MC_onedimension(signtheta * rho * gamma, theta, rho, gamma);
		if (kouho1 < kouho2 && kouho1 < kouho3){
			return theta;
		} else if (kouho2 < kouho3){
			return 0.0;
		} else {
			return signtheta * rho * gamma;
		}
	} else {
		return 0.0;
	}
}



double S_lasso(double theta, double rho){
	return dsignum(theta) * dmax((fabs(theta) - rho), 0);
}


// ---------------------------------------------------------
//  Update Phi using BFGS
// ---------------------------------------------------------

struct ex_vmmin_logF_Phi_t {
	int m;
	double zita;
	double *A;
};

static double optimfn_logF_Phi(int mm, double *Phi, void *ex0) {
	void *vmax = vmaxget();
	
	// Extract ex
	struct ex_vmmin_logF_Phi_t *ex = (struct ex_vmmin_logF_Phi_t *)ex0;
	int m = ex->m;
	double zita = ex->zita;
	double *A = ex->A;
	
	// Fill lower elements of Phi
	for (int i = 0; i < m - 1; i++) {
		for (int j = i + 1; j < m; j++) {
			Phi[j + i * m] = Phi[i + j * m];
		}
	}
	
	// Calculate log(det(Phi))
	double logdet_Phi = logdet(m, Phi, NULL);
	
	// Calculate inv(Phi) %*% A
	double *inv_Phi = (double *)R_alloc(mm, sizeof(double));
	double *inv_Phi_A = (double *)R_alloc(mm, sizeof(double));
	inv(m, Phi, inv_Phi, NULL);
	F77_CALL(dgemm)(
		"N", "N", &m, &m, &m,
		&D_ONE, inv_Phi, &m, A, &m,
		&D_ZERO, inv_Phi_A, &m FCONE FCONE);
	
	// Calculate tr(inv(Phi) %*% A)
	double tr_inv_Phi_A = 0;
	for (int i = 0; i < m; i++) {
		tr_inv_Phi_A += inv_Phi_A[i + i * m];
	}
	
	// Calculate log-likelihood value
	double logF = (1 - zita) * logdet_Phi + tr_inv_Phi_A;
	
	vmaxset(vmax);
	return logF;
}

static void optimgr_logF_Phi(int mm, double *Phi, double *gr, void *ex0) {
	void *vmax = vmaxget();
	
	// Extract ex
	struct ex_vmmin_logF_Phi_t *ex = (struct ex_vmmin_logF_Phi_t *)ex0;
	int m = ex->m;
	double zita = ex->zita;
	double *A = ex->A;
	
	// Fill lower elements of Phi
	for (int i = 0; i < m - 1; i++) {
		for (int j = i + 1; j < m; j++) {
			Phi[j + i * m] = Phi[i + j * m];
		}
	}
	
	// Calculate inv(Phi) %*% A %*% inv(Phi)
	double *inv_Phi = (double *)R_alloc(mm, sizeof(double));
	double *inv_Phi_A = (double *)R_alloc(mm, sizeof(double));
	double *inv_Phi_A_inv_Phi = (double *)R_alloc(mm, sizeof(double));
	inv(m, Phi, inv_Phi, NULL);
	F77_CALL(dgemm)(
		"N", "N", &m, &m, &m,
		&D_ONE, inv_Phi, &m, A, &m,
		&D_ZERO, inv_Phi_A, &m FCONE FCONE);
	F77_CALL(dgemm)(
		"N", "N", &m, &m, &m,
		&D_ONE, inv_Phi_A, &m, inv_Phi, &m,
		&D_ZERO, inv_Phi_A_inv_Phi, &m FCONE FCONE);
	
	// Calculate gradients
	for (int i = 0; i < m - 1; i++) {
		for (int j = i + 1; j < m; j++) {
			gr[i + j * m] =
				2 * (1 - zita) * inv_Phi[i + j * m]
				- 2 * inv_Phi_A_inv_Phi[i + j * m];
		}
	}
	
	vmaxset(vmax);
}

static void update_Phi(
	int m, int m2, double *Phi, double *A, struct ctrl_fanc_t *ctrl,
	double *O_Phi_new, int *O_conv)
{
	void *vmax = vmaxget();
	int m2m2 = m2 * m2;
	
	// Construct small matrix
	double *Phi_small = (double *)R_alloc(m2m2, sizeof(double));
	for (int i = 0; i < m2; i++) {
		Phi_small[i + i * m2] = 1;
		for (int j = i + 1; j < m2; j++) {
			Phi_small[i + j * m2] = Phi[i + j * m];
		}
	}
	double *A_small = (double *)R_alloc(m2m2, sizeof(double));
	for (int i = 0; i < m2; i++) {
		for (int j = 0; j < m2; j++) {
			A_small[i + j * m2] = A[i + j * m];
		}
	}
	
	// Construct upper-triangle mask
	int *mask = (int *)R_alloc(m2m2, sizeof(int));
	for (int i = 0; i < m2; i++) {
		mask[i + i * m2] = 0;
		for (int j = i + 1; j < m2; j++) {
			mask[i + j * m2] = 1;
			mask[j + i * m2] = 0;
		}
	}
	
	// Minimize logF w.r.t. Phi with BFGS
	struct ex_vmmin_logF_Phi_t ex = {m2, ctrl->zita, A_small};
	double Fmin;
	int fncount, grcount, conv;
	void *vmax2 = vmaxget();
	vmmin(
		m2m2, Phi_small, &Fmin, optimfn_logF_Phi, optimgr_logF_Phi,
		ctrl->maxit_Phi, 0, mask, ctrl->tol_Phi, ctrl->tol_Phi, 1, &ex,
		&fncount, &grcount, &conv);
	vmaxset(vmax2);
	
	// Compose result matrix
	dfill(m * m, O_Phi_new, 0);
	for (int i = m2; i < m; i++) {
		O_Phi_new[i + i * m] = 1;
	}
	for (int i = 0; i < m2; i++) {
		O_Phi_new[i + i * m] = 1;
		for (int j = i + 1; j < m2; j++) {
			O_Phi_new[i + j * m] = O_Phi_new[j + i * m] =
				Phi_small[i + j * m2];
		}
	}
	
	*O_conv |= conv;
	vmaxset(vmax);
}

// ---------------------------------------------------------
//  Update Lambda, Psi
// ---------------------------------------------------------

static void calculate_M_A_B(
	int p, int m, int N,
	double *Lambda, double *diag_Psi, double *Phi,
	double *S, double *X, double *Im,
	int cor_factor, struct ctrl_fanc_t *ctrl,
	double *M, double *A, double *B)
{
	void *vmax = vmaxget();
	int mm = m * m, pm = p * m, Nm = N * m;
	
	// Calculate Lambda %*% inv(Psi)
	double *inv_Psi_Lambda = (double *)R_alloc(pm, sizeof(double));
	double *sqrt_inv_Psi_Lambda = (double *)R_alloc(pm, sizeof(double));
	F77_CALL(dcopy)(&pm, Lambda, INC_SEQ, inv_Psi_Lambda, INC_SEQ);
	F77_CALL(dcopy)(&pm, Lambda, INC_SEQ, sqrt_inv_Psi_Lambda, INC_SEQ);
	for (int i = 0; i < p; i++) {
		double diag_inv_Psi_i = 1 / diag_Psi[i];
		for (int j = 0; j < m; j++) {
			inv_Psi_Lambda[i + j * p] *= diag_inv_Psi_i;
			sqrt_inv_Psi_Lambda[i + j * p] *= sqrt(diag_inv_Psi_i);
		}
	}
	
	// M <- t(Lambda) %*% inv(Psi) %*% Lambda + inv(Phi)
	if (cor_factor && m > 1) {
		inv(m, Phi, M, NULL);
	} else {
		F77_CALL(dcopy)(&mm, Im, INC_SEQ, M, INC_SEQ);
	}
	F77_CALL(dsyrk)(
		UPLO_UPPER, "T",
		&m, &p, &D_ONE, sqrt_inv_Psi_Lambda, &p, &D_ONE, M, &m FCONE FCONE);
	for (int i = 0; i < m - 1; i++) {
		for (int j = i + 1; j < m; j++) {
			M[j + i * m] = M[i + j * m];
		}
	}
	
	// C <- inv(Psi) %*% Lambda %*% inv(M)
	double *inv_M = (double *)R_alloc(mm, sizeof(double));
	double *inv_Psi_Lambda_inv_M = (double *)R_alloc(pm, sizeof(double));
	inv(m, M, inv_M, NULL);
	F77_CALL(dgemm)(
		"N", "N", &p, &m, &m,
		&D_ONE, inv_Psi_Lambda, &p, inv_M, &m,
		&D_ZERO, inv_Psi_Lambda_inv_M, &p FCONE FCONE);
	
	if (p <= N) {
		// B <- t(C) %*% S, A <- t(C) %*% S %*% C
		F77_CALL(dgemm)(
			"T", "N", &m, &p, &p,
			&D_ONE, inv_Psi_Lambda_inv_M, &p, S, &p,
			&D_ZERO, B, &m FCONE FCONE);
		F77_CALL(dcopy)(&mm, inv_M, INC_SEQ, A, INC_SEQ);
		F77_CALL(dgemm)(
			"N", "N", &m, &m, &p,
			&D_ONE, B, &m, inv_Psi_Lambda_inv_M, &p,
			&D_ONE, A, &m FCONE FCONE);
	} else {
		// D <- t(C) %*% t(X), B <- D %*% X, A <- D %*% t(D) + inv(M)
		double *D = (double *)R_alloc(Nm, sizeof(double));
		F77_CALL(dgemm)(
			"T", "T", &m, &N, &p,
			&D_ONE, inv_Psi_Lambda_inv_M, &p, X, &N,
			&D_ZERO, D, &m FCONE FCONE);
		F77_CALL(dgemm)(
			"N", "N", &m, &p, &N,
			&D_ONE, D, &m, X, &N,
			&D_ZERO, B, &m FCONE FCONE);
		F77_CALL(dcopy)(&mm, inv_M, INC_SEQ, A, INC_SEQ);
		F77_CALL(dgemm)(
			"N", "T", &m, &m, &N,
			&D_ONE, D, &m, D, &m,
			&D_ONE, A, &m FCONE FCONE);
	}
	
	vmaxset(vmax);
}

static void update_Lambda_i(
	int p, int m, int m2, int i, double *Lambda, double *diag_Psi,
	double *A, double *B, double rho, double gamma, int type, struct ctrl_fanc_t *ctrl,
	double *O_Lambda_new, int *O_conv)
{
	double Lambda_i_buf1[m], Lambda_i_buf2[m];
	double *Lambda_i = Lambda_i_buf1, *Lambda_i_new = Lambda_i_buf2;
	
	for (int j = 0; j < m; j++) {
		Lambda_i[j] = Lambda[i + j * p];
		Lambda_i_new[j] = Lambda[i + j * p];
	}
	
	// Minimize w.r.t. Lambda[i, ] with coordinate descent
	double err_CD = DBL_MAX;
	int iter_CD = 0;
	while (err_CD > ctrl->tol_CD && iter_CD < ctrl->maxit_CD) {
		for (int j = 0; j < m2; j++) {
			double sum_A_Lambda = 0;
			for (int k = 0; k < m2; k++) {
				double lamb = k < j ? Lambda_i_new[k] : k > j ? Lambda_i[k] : 0;
        		//another code:
				//double lamb = k != j ? Lambda_i_new[k] : 0;
				sum_A_Lambda += A[k + j * m] * lamb;
			}
			double z = (B[j + i * m] - sum_A_Lambda) / A[j + j * m];
			double w = diag_Psi[i] / A[j + j * m];
			double wrho = rho * w;
			double wgamma = gamma / w;

			if(type==1) Lambda_i_new[j] = S_MC(z,wrho,wgamma);
			if(type==3) {
				double beta_enet = 1.0 + wrho * (1.0 - gamma);
				Lambda_i_new[j] = S_MC(z/beta_enet, wrho*gamma/beta_enet, R_PosInf);
			}

		}
		
		// Check convergence
		err_CD = dnorm2(m2, Lambda_i_new, Lambda_i);
		err_CD = err_CD / sqrt(m); //changed the threshold!!
		
		// Step
		SWAP(double*, Lambda_i, Lambda_i_new);
        //another code:
        //F77_CALL(dcopy)(&m2, Lambda_i_new, INC_SEQ, Lambda_i, INC_SEQ);
		iter_CD++;
	}
	
	for (int j = 0; j < m; j++) {
		O_Lambda_new[i + j * p] = Lambda_i[j];
	}
	*O_conv |= err_CD > ctrl->tol_CD ? 1 : 0;
}



static void update_Lambda_i_prenet(
	int p, int m, int m2, int i, double *Lambda, double *diag_Psi,
	double *A, double *B, double rho, double gamma, struct ctrl_fanc_t *ctrl,
	double *O_Lambda_new, int *O_conv)
{
	double Lambda_i_buf1[m], Lambda_i_buf2[m];
	double *Lambda_i = Lambda_i_buf1, *Lambda_i_new = Lambda_i_buf2;
	
	for (int j = 0; j < m; j++) {
		Lambda_i[j] = Lambda[i + j * p];
		Lambda_i_new[j] = Lambda[i + j * p];
	}

			// Minimize w.r.t. Lambda[i, ] with coordinate descent
	double err_CD = DBL_MAX;
	int iter_CD = 0;
	while (err_CD > ctrl->tol_CD && iter_CD < ctrl->maxit_CD) {
		for (int j = 0; j < m2; j++) {
			double sum_A_Lambda = 0;
			double sum_absLambda = 0;
			double sum_Lambda2 = 0;
			for (int k = 0; k < m2; k++) {
				double lamb = k < j ? Lambda_i_new[k] : k > j ? Lambda_i[k] : 0;
        		//another code:
				//double lamb = k != j ? Lambda_i_new[k] : 0;
				sum_A_Lambda += A[k + j * m] * lamb;
				sum_absLambda += fabs(lamb);
				sum_Lambda2 += lamb * lamb;
			}
			double beta_prenet = rho * diag_Psi[i] * (1 - gamma) * sum_Lambda2 * ctrl->w[i] * ctrl->w[i];
			double gamma_prenet = gamma * sum_absLambda * ctrl->w[i];
			double z = (B[j + i * m] - sum_A_Lambda) / (A[j + j * m] + beta_prenet);
			double wrho = diag_Psi[i] * rho * gamma_prenet / (A[j + j * m] + beta_prenet);
			
			Lambda_i_new[j] = S_lasso(z,wrho);
		}
		
		// Check convergence
		err_CD = dnorm2(m2, Lambda_i_new, Lambda_i);
		err_CD = err_CD / sqrt(m); //changed the threshold!!

		// Step
		SWAP(double*, Lambda_i, Lambda_i_new);
        //another code:
        //F77_CALL(dcopy)(&m2, Lambda_i_new, INC_SEQ, Lambda_i, INC_SEQ);
		iter_CD++;
	}
	
	for (int j = 0; j < m; j++) {
		O_Lambda_new[i + j * p] = Lambda_i[j];
	}
	*O_conv |= err_CD > ctrl->tol_CD ? 1 : 0;
}



static void update_Psi(
	int p, int m, double *Lambda, double *A, double *B, double *S,
	struct ctrl_fanc_t *ctrl,
	double *O_diag_Psi_new)
{
	double tmp = 0.0;
	for (int i = 0; i < p; i++) {
		double A_Lambda_A = 0, B_Lambda = 0;
		for (int j = 0; j < m; j++) {
			B_Lambda += B[j + i * m] * Lambda[i + j * p];
			for (int k = 0; k < m; k++) {
				A_Lambda_A +=
					A[k + j * m] * Lambda[i + k * p] * Lambda[i + j * p];
			}
		}
		if(ctrl->model == 1){
			O_diag_Psi_new[i] = dmax(
				(1 + ctrl->eta) * S[i + i * p] - 2 * B_Lambda + A_Lambda_A, 
				ctrl->tol_Psi);
		}else{
			tmp += (1 + ctrl->eta) * S[i + i * p] - 2 * B_Lambda + A_Lambda_A;
		}
	}
	if(ctrl->model == 2){
		for (int i = 0; i < p; i++){
			O_diag_Psi_new[i] = tmp / p;
		}
	}
}

// ---------------------------------------------------------
//  Calculate log-likelihood value
// ---------------------------------------------------------

static void calculate_logF(
	int p, int m, int N, double *Lambda, double *diag_Psi, double *Phi,
	double *S, double *X, double *Im, double rho, double gamma, int type,
	int cor_factor, struct ctrl_fanc_t *ctrl,
	struct logF_value_t *O_logF)
{
	void *vmax = vmaxget();
	int mm = m * m, pm = p * m;
	
	// Prepare to calculate log-likelihood
	double *A = (double *)R_alloc(mm, sizeof(double));
	double *B = (double *)R_alloc(pm, sizeof(double));
	double *M = (double *)R_alloc(mm, sizeof(double));
	double *MA = (double *)R_alloc(mm, sizeof(double));
	calculate_M_A_B(
		p, m, N, Lambda, diag_Psi, Phi, S, X, Im, cor_factor, ctrl, M, A, B);
	F77_CALL(dgemm)(
		"N", "N", &m, &m, &m, &D_ONE, M, &m, A, &m, &D_ZERO, MA, &m FCONE FCONE);
	
	// Calculate log-likelihood (log(det(Sigma)) + tr(Sigma^{-1}S))
	double logF = 0;
	for (int i = 0; i < p; i++) {
		logF += log(diag_Psi[i]) + S[i + i * p] / diag_Psi[i];
	}
	logF += logdet(m, M, NULL);
	if (cor_factor) logF += logdet(m, Phi, NULL);
	for (int i = 0; i < m; i++) {
		logF -= MA[i + i * m] - 1;
	}
	
	// Calculate penalty
	double penalty = 0;
	if(type==1){
		for (int i = 0; i < pm; i++) {
			if (rho == 0) {
				// pass
			} else if (fabs(Lambda[i]) < rho * gamma) {
				penalty += rho *
					(fabs(Lambda[i]) - Lambda[i] * Lambda[i] / (2 * rho * gamma));
			} else {
				penalty += rho * rho * gamma / 2;
			}
		}
	}else if(type==2){
		for (int i = 0; i < p; i++){
			for (int k = 0; k < m-1; k++){
				for (int l = k+1; l < m; l++){
					penalty += rho * 1/2 * (1-gamma) * ctrl->w[i] * ctrl->w[i] *  Lambda[i+k*p] * Lambda[i+k*p] * Lambda[i+l*p] * Lambda[i+l*p];
					penalty += rho  * gamma * ctrl->w[i] * fabs(Lambda[i+k*p] * Lambda[i+l*p]);
				}
			}
		}
	}else if(type==3){
		for (int i = 0; i < pm; i++) {
			penalty += rho * (gamma  * fabs(Lambda[i]) + (1 - gamma) * Lambda[i] * Lambda[i] / 2);
		}
	}

	// Penalty for psi
	for (int i = 0; i < p; i++) {
		penalty += ctrl->eta * S[i + i * p] * (1 / diag_Psi[i]) / 2;
	}
	
	// Penalty for phi
	if (cor_factor) penalty += -ctrl->zita * logdet(m, Phi, NULL);
	

	O_logF->logF = logF;
	O_logF->penalty = 2 * penalty;
	O_logF->logF_pen = logF + 2 * penalty;
	
	vmaxset(vmax);
}

// ---------------------------------------------------------
//  Minimize log-likelihood using EM algorithm
// ---------------------------------------------------------

static void minimize_logF_Lambda_Psi_Phi(
	int p, int m, int N, int m2_init,
	double *Lambda_init, double *diag_Psi_init, double *Phi_init,
	double *S, double *X, double *Im,
	double rho, double gamma, int type, int init_prenet,
	int cor_factor, struct ctrl_fanc_t *ctrl,
	int *O_m2, double *O_Lambda, double *O_diag_Psi, double *O_Phi,
	struct logF_value_t *O_logF, int *O_conv)
{
	void *vmax = vmaxget();
	int pm = p * m, mm = m * m, m2 = m2_init;
	double *Lambda = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_new = (double *)R_alloc(pm, sizeof(double));
	double *diag_Psi = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_new = (double *)R_alloc(p, sizeof(double));
	double *Phi = (double *)R_alloc(mm, sizeof(double));
	double *Phi_new = (double *)R_alloc(mm, sizeof(double));
	double *Phi_tmp = (double *)R_alloc(mm, sizeof(double));
	double *A = (double *)R_alloc(mm, sizeof(double));
	double *inv_A = (double *)R_alloc(mm, sizeof(double));
	double *B = (double *)R_alloc(pm, sizeof(double));
	double *M = (double *)R_alloc(mm, sizeof(double));
	
    double *diagAinv = (double *)R_alloc(m, sizeof(double));
    double *comparison_mv = (double *)R_alloc(pm, sizeof(double));
    int *index_comparison_mv = (int *)R_alloc(p, sizeof(int));
    int *index_nonzerocolumn = (int *)R_alloc(m, sizeof(int));
    int *index_zerocolumn = (int *)R_alloc(m, sizeof(int));

	// Initialize
	F77_CALL(dcopy)(&pm, Lambda_init, INC_SEQ, Lambda, INC_SEQ);
	F77_CALL(dcopy)(&p, diag_Psi_init, INC_SEQ, diag_Psi, INC_SEQ);
	F77_CALL(dcopy)(&mm, Phi_init, INC_SEQ, Phi, INC_SEQ);
	
	// Iterate EM step
	double err_Lambda = DBL_MAX;
	int iter_EM = 0;


	while (err_Lambda > ctrl->tol_EM && iter_EM < ctrl->maxit_EM) {
		// Prepare
		ezprof_start(g_ezprof, 0, "Calculate_M_A_B");
		calculate_M_A_B(
			p, m, N, Lambda, diag_Psi, Phi, S, X, Im, cor_factor, ctrl,
			M, A, B);
		ezprof_stop(g_ezprof, 0);
		
		// Update Lambda
		if(init_prenet==0){
			ezprof_start(g_ezprof, 1, "Update Lambda");
			if (ctrl->use_penalty) {
				// For each coord
	#ifdef _OPENMP
	#pragma omp parallel for num_threads(ctrl->num_threads) if(ctrl->omp)
	#endif
				for (int i = 0; i < p; i++) {
					if(type==1 || type==3) update_Lambda_i(
						p, m, m2, i, Lambda, diag_Psi, A, B, rho, gamma, type, ctrl,
						Lambda_new, &O_conv[1]);

					if(type==2) update_Lambda_i_prenet(
						p, m, m2, i, Lambda, diag_Psi, A, B, rho, gamma, ctrl,
						Lambda_new, &O_conv[1]);
				}

			} else {
				// Calculate inv(A)
				inv(m, A, inv_A, NULL);
				
				// Calculate Lambda <- t(B) %*% inv(A)
				F77_CALL(dgemm)("T", "N", &p, &m, &m,
					&D_ONE, B, &m, inv_A, &m,
					&D_ZERO, Lambda_new, &p FCONE FCONE);
				for (int i = m2 * p; i < pm; i++) {
					Lambda_new[i] = Lambda_init[i];
				}
			}
			ezprof_stop(g_ezprof, 1);
		}else{
			//diagAinv <- diag(A^(-1))
	        for (int i = 0; i < m; i++) {
	            diagAinv[i] = 1.0 / A[i + i*m];
	        }

	        //comparison_mv <- diagAinv * B^2
	        for (int i = 0; i < p; i++) {
	            for (int j = 0; j < m; j++) {
	                comparison_mv[j+i*m] = diagAinv[j] * B[j+i*m] * B[j+i*m];
	            }
	        }

	        //index <- apply(comparison_mv,2,which.max)
	        for (int i = 0; i < p; i++) {
	            double tmp=0.0;
	            for (int j = 0; j < m; j++) {
	                if(comparison_mv[j+i*m] > tmp){
	                    index_comparison_mv[i] = j;
	                    tmp = comparison_mv[j+i*m];
	                }
	            }
	        }

	        //Lambdanew_all <- diagAinv * B
	        //Lambdanew[] <- 0
	        //for(i in 1:p) Lambdanew[i,index[i]] <- Lambdanew_all[index[i],i]
	         for (int i = 0; i < p; i++) {
	            for (int j = 0; j < m; j++) {
	                if(index_comparison_mv[i] == j){
	                    Lambda_new[i+j*p] = diagAinv[j] * B[j+i*m];
	                }else{
	                    Lambda_new[i+j*p] = 0.0;
	                }
	            }
	        }
		}

		// Update Psi
		ezprof_start(g_ezprof, 2, "Update Psi");
		update_Psi(p, m, Lambda_new, A, B, S, ctrl, diag_Psi_new);
		ezprof_stop(g_ezprof, 2);
		
		// Update Phi
		ezprof_start(g_ezprof, 3, "Update Phi");
		if (cor_factor && m2 > 1) {
			update_Phi(m, m2, Phi, A, ctrl, Phi_new, &O_conv[2]);
		} else {
			SWAP(double*, Phi, Phi_new);
		}
		ezprof_stop(g_ezprof, 3);
		
		 // Swap zero columns in Lambda backward
        int j2 = 0;
        int j2_2 = 0;
        ifill(m, index_nonzerocolumn, -1);
        ifill(m, index_zerocolumn, -1);
        for (int j = 0; j < m2; j++) {
            double sum_abs_Lambda_j = 0;
            for (int i = 0; i < p; i++) {
                sum_abs_Lambda_j += fabs(Lambda_new[i + j * p]);
            }
            if (sum_abs_Lambda_j > 0.0) {
                index_nonzerocolumn[j2] = j;
                if (j2 < j) {
                    F77_CALL(dcopy)(
                        &p, &Lambda_new[j * p], INC_SEQ,
                        &Lambda_new[j2 * p], INC_SEQ);
                }
                j2++;
            }else{
                index_zerocolumn[j2_2] = j;
                j2_2++;
            }
        }
        dfill(pm - j2 * p, Lambda_new + j2 * p, 0);

       //int j = 0;
        if(m2 != j2){
            F77_CALL(dcopy)(&mm, Phi_new, INC_SEQ, Phi_tmp, INC_SEQ);
            F77_CALL(dcopy)(&mm, Im, INC_SEQ, Phi_new, INC_SEQ);
            if(j2>1){
                for (int i = 0; i < j2; i++){
                    for (int i0 = i+1; i0 < j2; i0++){
                        Phi_new[i + i0 * m] = Phi_tmp[index_nonzerocolumn[i] + index_nonzerocolumn[i0] * m];
                        Phi_new[i0 + i * m] = Phi_tmp[index_nonzerocolumn[i0] + index_nonzerocolumn[i] * m];
                        //Rprintf("Phinew_sort: %5f\n",Phi_new[i + i0 * m]);
                    }
                }
            }
            for (int i = 0; i < j2; i++){
                for (int i0 = 0; i0 < j2_2; i0++){
                    Phi_new[i + (i0+j2) * m] = Phi_tmp[index_nonzerocolumn[i] + index_zerocolumn[i0] * m];
                    Phi_new[(i0+j2) + i * m] = Phi_tmp[index_zerocolumn[i0] + index_nonzerocolumn[i] * m];
                        //Rprintf("Phinew_sort: %5f\n",Phi_new[(i0+j2) + i * m]);
                }
            }
            if(j2_2>1){
                for (int i = 0; i < j2_2; i++){
                    for (int i0 = i+1; i0 < j2_2; i0++){
                        Phi_new[(i+j2) + (i0+j2) * m] = Phi_tmp[index_zerocolumn[i] + index_zerocolumn[i0] * m];
                        Phi_new[(i0+j2) + (i+j2) * m] = Phi_tmp[index_zerocolumn[i0] + index_zerocolumn[i] * m];
                        //Rprintf("Phinew_sort: %5f\n",Phi_new[(i0+j2) + (i+j2) * m]);
                    }
                }
            }
        }
        m2 = j2;

		// Check convergence
		err_Lambda = dnorm2(pm, Lambda_new, Lambda);
		err_Lambda = err_Lambda / sqrt(pm); //changed the threshold!!
		
		// Step
		SWAP(double*, Lambda, Lambda_new);
		SWAP(double*, diag_Psi, diag_Psi_new);
		SWAP(double*, Phi, Phi_new);
		iter_EM++;
		
		R_CheckUserInterrupt();
	}
	O_conv[0] |= err_Lambda > ctrl->tol_EM;
	

	// Suppress almost zero to zero
	for (int i = 0; i < mm; i++) {
		if (fabs(Phi[i]) < ctrl->tol_Phi) Phi[i] = 0;
	}
	for (int i = 0; i < pm; i++) {
		if (fabs(Lambda[i]) < ctrl->tol_EM) Lambda[i] = 0;
	}
	//For only MC penalty (prenet penalty does not always decrease the penalized log-likelihood function with the following step!!)
	if(type==1){
		for (int j = 0; j < m; j++) {
			int nzidx = -1;
			for (int i = 0; i < p && nzidx != -2; i++) {
				if (Lambda[i + j * p] != 0) {
					nzidx = nzidx == -1 ? i : -2;
				}
			}
			if (nzidx >= 0) {
				int all_zero = 1;
				for (int i = 0; i < m && all_zero; i++) {
					if (i != j && Phi[i + j * m] != 0) all_zero = 0;
				}
				if (all_zero) {
					diag_Psi[nzidx] +=
						Lambda[nzidx + j * p] * Lambda[nzidx + j * p];
					Lambda[nzidx + j * p] = 0;
				}
			}
		}
	}
	for (int i = 0; i < p; i++) {
		int all_zero = 1;
		for (int j = 0; j < m && all_zero; j++) {
			if (Lambda[i + j * p] != 0) all_zero = 0;
		}
		if (all_zero) {
			diag_Psi[i] = S[i + i * p];
		}
	}
	
	// Calculate final m2
	m2 = 0;
	for (int j = 0; j < m; j++) {
		for (int i = 0; i < p; i++) {
			if (Lambda[i + j * p] != 0) {
				m2++;
				break;
			}
		}
	}
	
	// Calculate logF
	struct logF_value_t logF;
	calculate_logF(
		p, m, N, Lambda, diag_Psi, Phi, S, X, Im, rho, gamma, type,
		cor_factor, ctrl, &logF);
	
	// Compose result
	if (O_m2 != NULL) *O_m2 = m2;
	F77_CALL(dcopy)(&pm, Lambda, INC_SEQ, O_Lambda, INC_SEQ);
	F77_CALL(dcopy)(&p, diag_Psi, INC_SEQ, O_diag_Psi, INC_SEQ);
	F77_CALL(dcopy)(&mm, Phi, INC_SEQ, O_Phi, INC_SEQ);
	*O_logF = logF;
	
	vmaxset(vmax);
}



static void minimize_logF_Psi(
	int p, int m, int N,
	double *Lambda, double *diag_Psi_init, double *Phi,
	double *S, double *X, double *Im,
	struct ctrl_fanc_t *ctrl,
	double *O_diag_Psi, double *O_A, double *O_B, int *O_conv)
{
	void *vmax = vmaxget();
	int mm = m * m, pm = p * m;
	double *diag_Psi = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_new = (double *)R_alloc(p, sizeof(double));
	double *A = (double *)R_alloc(mm, sizeof(double));
	double *B = (double *)R_alloc(pm, sizeof(double));
	double *M = (double *)R_alloc(mm, sizeof(double));
	
	// Initialize
	F77_CALL(dcopy)(&p, diag_Psi_init, INC_SEQ, diag_Psi, INC_SEQ);
	
	// Iterate EM step
	double err_Psi = DBL_MAX;
	int iter_EM = 0;
	while (err_Psi > ctrl->tol_EM && iter_EM < ctrl->maxit_EM) {
		// Prepare
		calculate_M_A_B(
			p, m, N, Lambda, diag_Psi, Phi, S, X, Im, ctrl->cor_factor, ctrl,
			M, A, B);
		
		// Update Psi
		update_Psi(p, m, Lambda, A, B, S, ctrl, diag_Psi_new);
		
		// Check convergence
		err_Psi = dnorm2(p, diag_Psi_new, diag_Psi);
		
		// Step
		SWAP(double*, diag_Psi, diag_Psi_new);
		iter_EM++;
	}
	if (iter_EM == ctrl->maxit_EM) O_conv[0] = 1;
	
	// Compose result
	F77_CALL(dcopy)(&p, diag_Psi, INC_SEQ, O_diag_Psi, INC_SEQ);
	F77_CALL(dcopy)(&mm, A, INC_SEQ, O_A, INC_SEQ);
	F77_CALL(dcopy)(&pm, B, INC_SEQ, O_B, INC_SEQ);
	
	vmaxset(vmax);
}

// ---------------------------------------------------------
//  Initialize parameters
// ---------------------------------------------------------

static void init_params_warm(int p, int m, int N,
	double *S, double *X, double *Im, int type, struct ctrl_fanc_t *ctrl,
	double *O_Lambda, double *O_diag_Psi, double *O_rho, int ISNAN_max_rho, int *conv)
{
	void *vmax = vmaxget();
	int m2;
	double *Lambda_opt = (double *)R_alloc(p, sizeof(double));
	double *Lambda_tmp = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_init = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_opt = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_tmp = (double *)R_alloc(p, sizeof(double));
	double *Phi = (double *)R_alloc(1, sizeof(double));
	double *A = (double *)R_alloc(1, sizeof(double));
	double *B = (double *)R_alloc(p, sizeof(double));
	double min_logF_pen = DBL_MAX;
	
	// Search appropriate Lambda
	for (int t = 0; t < ctrl->num_iter_init; t++) {
		// Make parameter candidate at random
		for (int i = 0; i < p; i++) {
			Lambda_tmp[i] = unif_rand();
			diag_Psi_tmp[i] = unif_rand();
		}
		dfill(1, Phi, 0);
		
		// Get better candidate from random parameter (one-factor model)
		struct logF_value_t logF_tmp;
		minimize_logF_Lambda_Psi_Phi(
			p, 1, N, 1, Lambda_tmp, diag_Psi_tmp, Phi,
			S, X, Im, 0, R_PosInf, type, 0, 0, ctrl,
			&m2, Lambda_tmp, diag_Psi_tmp, Phi,
			&logF_tmp, conv);
		
		// Update if better
		if (logF_tmp.logF_pen < min_logF_pen) {
			SWAP(double*, Lambda_tmp, Lambda_opt);
			SWAP(double*, diag_Psi_tmp, diag_Psi_opt);
			min_logF_pen = logF_tmp.logF_pen;
		}
	}
	
	int argmax_Lambda = F77_CALL(idamax)(&p, Lambda_opt, INC_SEQ) - 1;
	double max_Lambda = Lambda_opt[argmax_Lambda];
	dfill(p, Lambda_tmp, 0);
	dfill(p, Lambda_opt, 0);
	SWAP(double*, diag_Psi_opt, diag_Psi_init);
	
	// Search appropriate Lambda (scaled), Psi and rho_max
	double max_rho = DBL_MIN;
	for (int t = 0; t < ctrl->num_iter_init; t++) {
		Lambda_tmp[argmax_Lambda] = max_Lambda * ctrl->init_coef[t];
		F77_CALL(dcopy)(&p, diag_Psi_init, INC_SEQ, diag_Psi_tmp, INC_SEQ);
		
		minimize_logF_Psi(
			p, 1, N, Lambda_tmp, diag_Psi_tmp, Phi, S, X, Im, ctrl,
			diag_Psi_tmp, A, B, conv);
		
		double rho_tmp = DBL_MIN;
		for (int i = 0; i < p; i++) {
			if (i != argmax_Lambda) {
				rho_tmp =
					dmax(rho_tmp, fabs(1 / diag_Psi_tmp[i] * B[i]));
			}
		}
		
		if (rho_tmp > max_rho) {
			SWAP(double*, Lambda_tmp, Lambda_opt);
			SWAP(double*, diag_Psi_tmp, diag_Psi_opt);
			max_rho = rho_tmp;
		}
	}
	
	dfill(p * m - p, O_Lambda + p, 0);
	F77_CALL(dcopy)(&p, Lambda_opt, INC_SEQ, O_Lambda, INC_SEQ);
	F77_CALL(dcopy)(&p, diag_Psi_opt, INC_SEQ, O_diag_Psi, INC_SEQ);
	if (ISNAN_max_rho) O_rho[0] = max_rho;
	
	vmaxset(vmax);
}

static void init_params_warm_prenet(int p, int m, int N,
	double *S, double *X, double *Im, double *gammas, int type, int use_cor_fact, struct ctrl_fanc_t *ctrl,
	int *O_m2, double *O_Lambda, double *O_diag_Psi, double *O_Phi,
	double *O_rho, int ISNAN_max_rho, int *conv)
{
	int pm=p*m;
	int mm=m*m;

	void *vmax = vmaxget();
	int m2, m2_opt;
	double *Lambda_opt = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_tmp = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_opt_2factor = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_tmp_2factor = (double *)R_alloc(pm, sizeof(double));
	double *diag_Psi_opt = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_tmp = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_opt_2factor = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_tmp_2factor = (double *)R_alloc(p, sizeof(double));
	double *Phi_new = (double *)R_alloc(mm, sizeof(double));
	double *Phi_tmp = (double *)R_alloc(mm, sizeof(double));
	double *Phi_opt = (double *)R_alloc(mm, sizeof(double));
	double *Phi_tmp_2factor = (double *)R_alloc(mm, sizeof(double));
	double *Phi_opt_2factor = (double *)R_alloc(mm, sizeof(double));
	double *M = (double *)R_alloc(mm, sizeof(double));
	double *A = (double *)R_alloc(mm, sizeof(double));
	double *B = (double *)R_alloc(pm, sizeof(double));
	double min_logF_pen = DBL_MAX;
	double min_logF_pen_2factor = DBL_MAX;

    int *index_nonzerorow = (int *)R_alloc(p, sizeof(int));
	
    F77_CALL(dcopy)(&mm, Im, INC_SEQ, Phi_new, INC_SEQ);

	// Search appropriate Lambda
	for (int t = 0; t < ctrl->num_iter_init_prenet; t++) {
//	for (int t = 0; t < 100; t++) {//ここ変更！！
		// Make parameter candidate at random
		for (int i = 0; i < pm; i++) {
			Lambda_tmp[i] = unif_rand();
			Lambda_tmp_2factor[i] = unif_rand();
		}
		for (int i = 0; i < p; i++) {
			diag_Psi_tmp[i] = unif_rand();
			diag_Psi_tmp_2factor[i] = unif_rand();
		}
    	F77_CALL(dcopy)(&mm, Im, INC_SEQ, Phi_tmp, INC_SEQ);
/*
	if(t==0){
		for (int i = 0; i < p; i++) {
			for (int j = 0; j < m; j++){
				Rprintf("%5f ", Lambda_tmp[i+j*p]);
			}
			Rprintf("\n");
		}
		for (int j = 0; j < p; j++){
			Rprintf("%5f ", diag_Psi_tmp[j]);
		}

	}		
		*/

		// Get better candidate from random parameter 
		struct logF_value_t logF_tmp;
		// fit uncorrelated model	
		minimize_logF_Lambda_Psi_Phi(
			p, m, N, m, Lambda_tmp, diag_Psi_tmp, Phi_tmp,
			S, X, Im, 0.0, 1.0, type, 1, 0, ctrl,
			&m2, Lambda_tmp, diag_Psi_tmp, Phi_tmp,
			&logF_tmp, conv);
		// fit correlated model	
		if(use_cor_fact) minimize_logF_Lambda_Psi_Phi(
			p, m, N, m, Lambda_tmp, diag_Psi_tmp, Phi_tmp,
			S, X, Im, 0.0, 1.0, type, 1, use_cor_fact, ctrl,
			&m2, Lambda_tmp, diag_Psi_tmp, Phi_tmp,
			&logF_tmp, conv);
    	if(m2>1){
    		F77_CALL(dcopy)(&pm, Lambda_tmp, INC_SEQ, Lambda_tmp_2factor, INC_SEQ);
	    	F77_CALL(dcopy)(&p, diag_Psi_tmp, INC_SEQ, diag_Psi_tmp_2factor, INC_SEQ);
	    	F77_CALL(dcopy)(&mm, Phi_tmp, INC_SEQ, Phi_tmp_2factor, INC_SEQ);
	    }
		
		// Update if better
		if (logF_tmp.logF_pen < min_logF_pen) {
			SWAP(double*, Lambda_tmp, Lambda_opt);
			SWAP(double*, diag_Psi_tmp, diag_Psi_opt);
			SWAP(double*, Phi_tmp, Phi_opt);
			min_logF_pen = logF_tmp.logF_pen;
			m2_opt = m2;
		}
		

		if (logF_tmp.logF_pen < min_logF_pen_2factor && m2>1) {
			SWAP(double*, Lambda_tmp_2factor, Lambda_opt_2factor);
			SWAP(double*, diag_Psi_tmp_2factor, diag_Psi_opt_2factor);
			SWAP(double*, Phi_tmp_2factor, Phi_opt_2factor);
			min_logF_pen_2factor = logF_tmp.logF_pen;
		}

	}
	int n_gam = ctrl->num_gammas;
	double *max_rho = (double *)R_alloc(n_gam, sizeof(double));
	
    double cand_rho = 0.0;
    dfill(n_gam, max_rho, 0.0);
    //nonzeroindex <- apply(Lambda,1,function(x) which(x!=0))
    for (int i = 0; i < p; i++) {
        for (int j = 0; j < m; j++) {
            if(Lambda_opt_2factor[i+j*p] != 0.0){
                index_nonzerorow[i] = j;
            }
        }
    }

	calculate_M_A_B(
			p, m, N, Lambda_opt_2factor, diag_Psi_opt_2factor, Phi_opt_2factor, S, X, Im, ctrl->cor_factor, ctrl,
			M, A, B);

	for(int l = 0; l < n_gam; l++){
        for (int i = 0; i < p; i++) {
            int j = index_nonzerorow[i];
            for (int k = 0; k < m; k++) {
                if(j != k){
                //rho_cand <- (B[k,i] - A[k,j] * Lambda_i[i,j]) / ( alpha * diagPsi_i[i] * Lambda_i[i,j] )
                    cand_rho = (B[k+i*m] - A[k+j*m] * Lambda_opt_2factor[i+j*p]) / ( gammas[l] * diag_Psi_opt_2factor[i] * Lambda_opt_2factor[i+j*p] * ctrl->w[i] );
                //rho_cand <- abs(rho_cand)
                    cand_rho = fabs(cand_rho);
                    if(max_rho[l] < cand_rho){
                        max_rho[l] = cand_rho;
                    }
                }
            }
        }
    }
    	//Rprintf("%5f\n",max_rho[0]);
    	//Rprintf("%5f\n",max_rho[1]);
    	//Rprintf("%5f\n",max_rho[2]);


    //slightly enlarge the maximum value of rho
	for(int l = 0; l < n_gam; l++) max_rho[l] = max_rho[l] +  max_rho[l] * ctrl->delta;
	F77_CALL(dcopy)(&pm, Lambda_opt, INC_SEQ, O_Lambda, INC_SEQ);
	F77_CALL(dcopy)(&p, diag_Psi_opt, INC_SEQ, O_diag_Psi, INC_SEQ);
	F77_CALL(dcopy)(&mm, Phi_opt, INC_SEQ, O_Phi, INC_SEQ);
	if (ISNAN_max_rho)  F77_CALL(dcopy)(&n_gam, max_rho, INC_SEQ, O_rho, INC_SEQ);

	O_m2[0]=m2_opt;

	
	vmaxset(vmax);
}


static void init_params_random(
	int p, int m, int N, int m2,
	double *S, double *X, double *Im,
	double rho, double gamma, int type, int use_cor_fact, struct ctrl_fanc_t *ctrl,
	int *O_m2, double *O_Lambda, double *O_diag_Psi, double *O_Phi, int *O_conv)
{
	void *vmax = vmaxget();
	int pm = p * m, mm = m * m;
	double *Lambda_opt = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_tmp = (double *)R_alloc(pm, sizeof(double));
	double *diag_Psi_opt = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_tmp = (double *)R_alloc(p, sizeof(double));
	double *Phi_opt = (double *)R_alloc(mm, sizeof(double));
	double *Phi_tmp = (double *)R_alloc(mm, sizeof(double));
	int m2_opt = m2, m2_tmp;
	
	double min_logF_pen = DBL_MAX;
	for (int t = 0; t < ctrl->num_iter_init; t++) {
		// Make parameter candidate at random
		dfill(pm - p * m2, Lambda_tmp + p * m2, 0);
		for (int i = 0; i < p * m2; i++) {
			Lambda_tmp[i] = unif_rand();
		}
		for (int i = 0; i < p; i++) {
			diag_Psi_tmp[i] = unif_rand();
		}
		F77_CALL(dcopy)(&mm, Im, INC_SEQ, Phi_tmp, INC_SEQ);
		
		// Get better candidate from random parameter
		struct logF_value_t logF_tmp;
		// First, fit the model with UNCORRELATED factors
		minimize_logF_Lambda_Psi_Phi(
			p, m, N, m2, Lambda_tmp, diag_Psi_tmp, Phi_tmp, S, X, Im,
			rho, gamma, type, 0, 0, ctrl,
			&m2_tmp, Lambda_tmp, diag_Psi_tmp, Phi_tmp,
			&logF_tmp, O_conv);
		// If use_cor_fact == 1, fit the model with CORRELATED factors
		if(use_cor_fact == 1){
			minimize_logF_Lambda_Psi_Phi(
			p, m, N, m2, Lambda_tmp, diag_Psi_tmp, Phi_tmp, S, X, Im,
			rho, gamma, type, 0, use_cor_fact, ctrl,
			&m2_tmp, Lambda_tmp, diag_Psi_tmp, Phi_tmp,
			&logF_tmp, O_conv);
		}
		// Update parameter if it is better 
		if (logF_tmp.logF_pen < min_logF_pen) {
			m2_opt = m2_tmp;
			SWAP(double*, Lambda_tmp, Lambda_opt);
			SWAP(double*, diag_Psi_tmp, diag_Psi_opt);
			SWAP(double*, Phi_tmp, Phi_opt);
			min_logF_pen = logF_tmp.logF_pen;
		}
	}
	
	*O_m2 = m2_opt;
	F77_CALL(dcopy)(&pm, Lambda_opt, INC_SEQ, O_Lambda, INC_SEQ);
	F77_CALL(dcopy)(&p, diag_Psi_opt, INC_SEQ, O_diag_Psi, INC_SEQ);
	F77_CALL(dcopy)(&mm, Phi_opt, INC_SEQ, O_Phi, INC_SEQ);
	
	vmaxset(vmax);
}

// ---------------------------------------------------------
//  Calculate criterions
// ---------------------------------------------------------

static void calculate_Sigma(
	int p, int m, double *Lambda, double *diag_Psi, double *Phi,
	double *O_Sigma)
{
	void *vmax = vmaxget();
	// Sigma <- Lambda %*% Phi %*% t(Lambda) + Psi
	double *LambdaPhi = (double *)R_alloc(p * m, sizeof(double));
	F77_CALL(dgemm)(
		"N", "N", &p, &m, &m,
		&D_ONE, Lambda, &p, Phi, &m, &D_ZERO, LambdaPhi, &p FCONE FCONE);
	F77_CALL(dgemm)(
		"N", "T", &p, &p, &m,
		&D_ONE, LambdaPhi, &p, Lambda, &p, &D_ZERO, O_Sigma, &p FCONE FCONE);
	for (int i = 0; i < p; i++) {
		O_Sigma[i + i * p] += diag_Psi[i];
	}
	vmaxset(vmax);
}

static double calculate_GFI(
	int p, int m, double *S, double *Sigma,
	double *Lambda, double *diag_Psi, double *Phi)
{
	void *vmax = vmaxget();
	int pp = p * p;

	// inv(Sigma)
	double *inv_Sigma = (double *)R_alloc(pp, sizeof(double));
	inv(p, Sigma, inv_Sigma, NULL);

	// inv(Sigma) %*% S
	double *inv_Sigma_S = (double *)R_alloc(pp, sizeof(double));
	F77_CALL(dgemm)(
		"N", "N", &p, &p, &p,
		&D_ONE, inv_Sigma, &p, S, &p, &D_ZERO, inv_Sigma_S, &p FCONE FCONE);

	double sumsq = dsumsq(pp, inv_Sigma_S);
	double tr = trace(p, inv_Sigma_S);
	double gfi = 1 - (sumsq - 2 * tr + p) / sumsq;

	vmaxset(vmax);
	return gfi;
}

static double calculate_AGFI(double gfi, int p, double df) {
	return 1 - (p * (p + 1) * (1 - gfi)) / (p * (p + 1) - 2 * df);
}

static double calculate_SRMR(int p, double *S, double *Sigma) {
	double srmr = 0.0;
	for (int i = 0; i < p; i++) {
		for (int j = i; j < p; j++) {
			double ds = S[i + j * p] - Sigma[i + j * p];
			double ss = S[i + i * p] * S[j + j * p];
			srmr += 2.0 / (p * (p + 1)) * ds * ds / ss;
		}
	}
	return sqrt(srmr);
}
static double calculate_RMSEA(
	int p, int N, double logF, double logdet_S, double df)
{
	double fhat = logF - logdet_S - p;
	double rmsea2 = fhat / df - 1.0 / N;
	return rmsea2 >= 0 ? sqrt(rmsea2) : 0;
}

static double calculate_CFI(
	int p, int N, double logF, double logdet_S, double f0hat, double df)
{
	double fhat = logF - logdet_S - p;
	double df0 = p * ( p - 1.0 ) / 2.0;
	double CFI_n = fhat - df / N;
	double CFI_d = f0hat - df0 / N;
	if(CFI_d < 0) CFI_d = 0.0;
	if(CFI_d < CFI_n) CFI_n = CFI_d;
	if(CFI_n < 0) CFI_n = 0.0;
	return 1 - CFI_n / CFI_d;
}

static double calculate_BIC(int N, double logF, double df) {
	return N * logF + log(N) * df;
}

static double calculate_BICH(
	int p, int m2, int c, int N, double logF, double df)
{
	return N * logF + df * log(N) * log(log(p * m2 + p + c));
}

static double calculate_EBIC(
	double bic, int p, int m, int c, double df, double gamma)
{
	return bic + 2 * gamma * (df - p - c) * log(p * m);
}

static double calculate_AIC(int N, double logF, double df) {
	return N * logF + 2 * df;
}

static double calculate_CAIC(int N, double logF, double df) {
	return N * logF + (log(N) + 1) * df;
}

// ---------------------------------------------------------
//  Total procedure
// ---------------------------------------------------------

struct ex_zeroin_rho_t {
	double gamma, rho0;
};

static double zeroinfn_rho(double x, void *ex0) {
	struct ex_zeroin_rho_t *ex = (struct ex_zeroin_rho_t *)ex0;
	double gamma = ex->gamma, rho0 = ex->rho0;
	return
		pnorm(gamma * x, 0, 1, 1, 0) - gamma * pnorm(x, 0, 1, 1, 0) +
		(gamma - 1) * pnorm(rho0, 0, 1, 1, 0);
}

static void fanc(
	int p, int m, int N, double *S, double *X, struct ctrl_fanc_t *ctrl,
	double *O_rhos, double *O_gammas, int *O_m2,
	int *O_spn_Lambda, int **O_spi_Lambda, double **O_spv_Lambda,
	int *type, //type=1 -> MC, type=2 -> prenet
	double *O_diag_Psi, double *O_Phi, double *O_logF,
	double *O_df, double *O_BIC, double *O_BICH, double *O_EBIC,
	double *O_AIC, double *O_CAIC, double *O_GFI, double *O_AGFI,
	double *O_SRMR, double *O_RMSEA, double *O_CFI,
	double *O_dfnonzero,
	double *O_AIC_dfnonzero, double *O_BIC_dfnonzero, double *O_CAIC_dfnonzero, double *O_EBIC_dfnonzero,
	double *O_AGFI_dfnonzero, double *O_RMSEA_dfnonzero, double *O_CFI_dfnonzero,
	int *O_conv)
{
	void *vmax = vmaxget();
	int pm = p * m, mm = m * m, m2, pp = p * p;
	double *Lambda = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_init = (double *)R_alloc(pm, sizeof(double));
	double *Lambda_full = (double *)R_alloc(pm, sizeof(double));
	double *diag_Psi = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_init = (double *)R_alloc(p, sizeof(double));
	double *diag_Psi_full = (double *)R_alloc(p, sizeof(double));
	double *Phi = (double *)R_alloc(mm, sizeof(double));
	double *Phi_init = (double *)R_alloc(mm, sizeof(double));
	double *Phi_full = (double *)R_alloc(mm, sizeof(double));
	double *Im = (double *)R_alloc(mm, sizeof(double));
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < m; j++) {
			Im[i + j * m] = i == j ? 1 : 0;
		}
	}
	double *Sigma = (double *)R_alloc(pp, sizeof(double));
	int num_rhos = ctrl->num_rhos, num_gammas = ctrl->num_gammas;
	double rhos[num_rhos * num_gammas], gammas[num_gammas];
	double rhos_tmp[num_rhos];
	double df_base[num_rhos];
	double logdet_S = 0.0;
	double logdet_diagS = 0.0;
	if (p <= ctrl->p_max_for_S){
		logdet_S = logdet(p, S, NULL);
		for (int i = 0; i < p; i++) {
			logdet_diagS += log(S[i + i * p]);
		}
	}
	double f0hat = logdet_diagS - logdet_S;

	int ISNAN_rhos = ISNAN(ctrl->rhos[0]);
	int ISNAN_gammas = ISNAN(ctrl->gammas[0]);
	int ISNAN_max_rho = ISNAN(ctrl->max_rho[0]);
	double *max_rho = (double *)R_alloc(num_gammas, sizeof(double));
	int m2_init_prenet;



	// Determine gamma
	if(ISNAN_gammas){
		if(type[0]==1){
			gammas[0] = R_PosInf;
			logseq(num_gammas - 1, ctrl->max_gamma, ctrl->min_gamma, gammas + 1);
		}else if(type[0]==2){
			logseq(num_gammas, ctrl->max_gamma, ctrl->min_gamma, gammas);
		}else if(type[0]==3){
			seq(num_gammas, ctrl->max_gamma, ctrl->min_gamma, gammas);
		}
	}else{
		F77_CALL(dcopy)(&num_gammas, ctrl->gammas, INC_SEQ, gammas, INC_SEQ);
	}

	//determine maxim value of rho
	if(ISNAN_max_rho){
		if(type[0]==1 || type[0]==3){
			max_rho[0] = 0.0;
		}
		if(type[0]==2){
			dfill(num_gammas, max_rho, 0.0);
		}
	}else{
		if(type[0]==1 || type[0]==3) F77_CALL(dcopy)(&I_ONE, ctrl->max_rho, INC_SEQ, max_rho, INC_SEQ);
		if(type[0]==2) F77_CALL(dcopy)(&num_gammas, ctrl->max_rho, INC_SEQ, max_rho, INC_SEQ);
	}

	// Calculate initial parameters
	if (ISNAN_max_rho || !ctrl->cold_init) {
		if(type[0]==1 || type[0]==3){
			init_params_warm(
			p, m, N, S, X, Im, 1, ctrl, 
			//p, m, N, S, X, Im, type[0], ctrl, 
			Lambda_init, diag_Psi_init, 
			max_rho, ISNAN_max_rho, O_conv);
			F77_CALL(dcopy)(&mm, Im, INC_SEQ, Phi_init, INC_SEQ);
		}else{
			init_params_warm_prenet(
			p, m, N, S, X, Im, gammas, type[0], ctrl->cor_factor, ctrl, 
			&m2, Lambda_init, diag_Psi_init, Phi_init,
			max_rho, ISNAN_max_rho, O_conv);
			m2_init_prenet = m2;
		}
		if(type[0]==3){
			if(num_gammas>1){
				for (int gi = 1; gi < num_gammas; gi++) {
					max_rho[gi] = max_rho[0] / gammas[gi];
				}
			}			
		}

	}


/*
	if(type[0]==2){
		for (int i = 0; i < num_gammas; i++){
			Rprintf("%5f ", max_rho[i]);
		}
	}
*/
	if (type[0]==1 && max_rho[0] > LARGE_RHO) {
		warning(
			"\"rho.max\" is greater than 8.25. In such cases, "
			"the reparametrization of the penalty funcion may be failed");
	}
	if (!ctrl->cold_init) {
		if(type[0]==1 || type[0]==3) m2 = 1;
		if(type[0]==2) m2 = m2_init_prenet;
		F77_CALL(dcopy)(&pm, Lambda_init, INC_SEQ, Lambda, INC_SEQ);
		F77_CALL(dcopy)(&p, diag_Psi_init, INC_SEQ, diag_Psi, INC_SEQ);
		F77_CALL(dcopy)(&mm, Phi_init, INC_SEQ, Phi, INC_SEQ);
	}
	
	// Determine rho
	if(ISNAN_rhos){
		if(type[0] == 1){
			logseq(num_rhos, max_rho[0], max_rho[0] * ctrl->delta, rhos);
			F77_CALL(dcopy)(&num_rhos, rhos, INC_SEQ, rhos_tmp, INC_SEQ);
			int gimin;
			if(gammas[0]==R_PosInf){
				gimin = 1;
			}else{
				gimin = 0;
			}
			for (int gi = gimin; gi < num_gammas; gi++) {
				for (int ri = 0; ri < num_rhos; ri++) {
					rhos[ri + gi * num_rhos] = zeroin(
						rhos_tmp[ri], dmax(max_rho[0], 8.3), zeroinfn_rho, DBL_EPSILON,
						&(struct ex_zeroin_rho_t){gammas[gi], rhos_tmp[ri]});
				}
			}
		}else if(type[0]==2){
			for (int gi = 0; gi < num_gammas; gi++) {
				//if(type[0]==1) logseq(num_rhos, max_rho[gi], max_rho[gi] * ctrl->delta, rhos + num_rhos * gi);
				powerseq(num_rhos, max_rho[gi], max_rho[gi] * ctrl->delta * sqrt(gammas[gi]), ctrl->alpha_powerseq, rhos + num_rhos * gi);
			}
		}else if(type[0]==3) {
			for (int gi = 0; gi < num_gammas; gi++) {
				logseq(num_rhos, max_rho[gi], max_rho[gi] * ctrl->delta, rhos + num_rhos * gi);
			}
		}
		if (ctrl->zero_min_rho) {
			for (int gi = 0; gi < num_gammas; gi++) {
				rhos[(num_rhos - 1) + gi * num_rhos] = 0;
			}
		}			
	}else{
		int num_gammas_rhos = num_rhos * num_gammas;
		F77_CALL(dcopy)(&num_gammas_rhos, ctrl->rhos, INC_SEQ, rhos, INC_SEQ);
	}

	memcpy(O_rhos, rhos, num_rhos * num_gammas * sizeof(double));
	memcpy(O_gammas, gammas, num_gammas * sizeof(double));
	
	// Calculate Lambda, Psi, Phi and logF value
	for (int rgi = 0; rgi < num_gammas * num_rhos; rgi++) {
		int ri = rgi % num_rhos, gi = rgi / num_rhos;
		if (ctrl->progress) {
			Rprintf(
				"* rho = %.2e, gamma = %.2e (%d/%d)\n",
				rhos[rgi], gammas[gi], rgi + 1, num_gammas * num_rhos);
		}
		int *conv = O_conv + (ri + gi * num_rhos) * 3;
		ifill(3, conv, 0);
		
		// Initialize parameters
		if (ctrl->cold_init) {
			init_params_random(
				p, m, N, m, S, X, Im,
				rhos[rgi], gammas[gi], type[0], ctrl->cor_factor, ctrl,
				&m2, Lambda, diag_Psi, Phi, conv);
		} else {
			if (gi > 0 && type[0]==1) {
				// Init with (ri, gi-1) result when gi > 0
				m2 = O_m2[ri + (gi - 1) * num_rhos];
				dfill(pm, Lambda, 0);
				int spn_Lambda = O_spn_Lambda[ri + (gi - 1) * num_rhos];
				int *spi_Lambda = O_spi_Lambda[ri + (gi - 1) * num_rhos];
				double *spv_Lambda = O_spv_Lambda[ri + (gi - 1) * num_rhos];
				for (int t = 0; t < spn_Lambda; t++) {
					Lambda[spi_Lambda[t]] = spv_Lambda[t];
				}
				F77_CALL(dcopy)(
					&p, O_diag_Psi + (ri + (gi - 1) * num_rhos) * p,
					INC_SEQ, diag_Psi, INC_SEQ);
				F77_CALL(dcopy)(
					&mm, O_Phi + (ri + (gi - 1) * num_rhos) * mm,
					INC_SEQ, Phi, INC_SEQ);
			} else if (ri == 0 && (type[0]==2 || type[0]==3)) {
				F77_CALL(dcopy)(&pm, Lambda_init, INC_SEQ, Lambda, INC_SEQ);
				F77_CALL(dcopy)(&p, diag_Psi_init, INC_SEQ, diag_Psi, INC_SEQ);
				F77_CALL(dcopy)(&mm, Phi_init, INC_SEQ, Phi, INC_SEQ);
			}else if (F77_CALL(dnrm2)(&pm, Lambda, &I_ONE) < ctrl->tol_EM) {
				// Reinitialize when Lambda is zero matrix
				if(type[0]==1 || type[0]==3) m2 = 1;
				if(type[0]==2) m2 = m2_init_prenet;
				F77_CALL(dcopy)(&pm, Lambda_init, INC_SEQ, Lambda, INC_SEQ);
				F77_CALL(dcopy)(&p, diag_Psi_init, INC_SEQ, diag_Psi, INC_SEQ);
				F77_CALL(dcopy)(&mm, Phi_init, INC_SEQ, Phi, INC_SEQ);
			}
		}

		
		// Minimize with restricted model
		struct logF_value_t logF;
		minimize_logF_Lambda_Psi_Phi(
			p, m, N, m2, Lambda, diag_Psi, Phi,
			S, X, Im, rhos[rgi], gammas[gi], type[0], 0, ctrl->cor_factor, ctrl,
			&m2, Lambda, diag_Psi, Phi, &logF, conv);
		
		// Check if the number of factors should be extended
		if (m2 < m && ((type[0]==1 && gi == 0) || type[0]==2 || type[0]==3)) {
			int m2_full;
			struct logF_value_t logF_full;
			
			// Minimize with the full model
			init_params_random(
				p, m, N, m, S, X, Im, rhos[rgi], gammas[gi], type[0], ctrl->cor_factor, ctrl,
				&m2_full, Lambda_full, diag_Psi_full, Phi_full, conv);
			minimize_logF_Lambda_Psi_Phi(
				p, m, N, m, Lambda_full, diag_Psi_full, Phi_full,
				S, X, Im, rhos[rgi], gammas[gi], type[0], 0, ctrl->cor_factor, ctrl,
				&m2_full, Lambda_full, diag_Psi_full, Phi_full,
				&logF_full, conv);
			
				
			// Update model
			if (logF_full.logF_pen < logF.logF_pen) {
				m2 = m2_full;
				SWAP(double*, Lambda, Lambda_full);
				SWAP(double*, diag_Psi, diag_Psi_full);
				SWAP(double*, Phi, Phi_full);
				logF = logF_full;
			}
		}
		
		
		// Compose result
		int nonzero = 0;
		for (int i = 0; i < pm; i++) {
			if (Lambda[i] != 0) nonzero++;
		}
		O_m2[rgi] = m2;
		O_spn_Lambda[rgi] = nonzero;
		O_spi_Lambda[rgi] = (int *)malloc(nonzero * sizeof(int));
		O_spv_Lambda[rgi] = (double *)malloc(nonzero * sizeof(double));
		if (O_spi_Lambda[rgi] == NULL || O_spv_Lambda[rgi] == NULL) {
			for (int i = 0; i <= rgi; i++) {
				free(O_spi_Lambda[rgi]); O_spi_Lambda[rgi] = NULL;
				free(O_spv_Lambda[rgi]); O_spv_Lambda[rgi] = NULL;
			}
			error("memory exhausted");
			return;
		}
		for (int i = 0, t = 0; i < pm; i++) {
			if (Lambda[i] != 0) {
				O_spi_Lambda[rgi][t] = i;
				O_spv_Lambda[rgi][t] = Lambda[i];
				t++;
			}
		}
		F77_CALL(dcopy)(&p, diag_Psi, INC_SEQ, O_diag_Psi + rgi * p, INC_SEQ);
		F77_CALL(dcopy)(&mm, Phi, INC_SEQ, O_Phi + rgi * mm, INC_SEQ);
		O_logF[0 + rgi * 3] = logF.logF;
		O_logF[1 + rgi * 3] = logF.penalty;
		O_logF[2 + rgi * 3] = logF.logF_pen;
		
		// Calculate df
		if (gi == 0){
			if(ctrl->model == 1) df_base[ri] = p + nonzero;
			if(ctrl->model == 2) df_base[ri] = 1 + nonzero;
		}
		int c = ctrl->cor_factor ? m2 * (m2 - 1) / 2 : 0;
		double df = (O_df[rgi] = df_base[ri] + c);
		int df_nonzero;
		if(ctrl->model == 1) df_nonzero  = nonzero + p + c;
		if(ctrl->model == 2) df_nonzero  = nonzero + 1 + c;

		O_dfnonzero[rgi] = df_nonzero;

		// Calculate Goodness-of-fit indices
		if (p <= ctrl->p_max_for_S) {
			calculate_Sigma(p, m, Lambda, diag_Psi, Phi, Sigma);
			O_GFI[rgi] = calculate_GFI(p, m, S, Sigma, Lambda, diag_Psi, Phi);
			O_AGFI[rgi] = calculate_AGFI(O_GFI[rgi], p, df);
			O_AGFI_dfnonzero[rgi] = calculate_AGFI(O_GFI[rgi], p, df_nonzero);
			O_SRMR[rgi] = calculate_SRMR(p, S, Sigma);
			O_RMSEA[rgi] = calculate_RMSEA(p, N, logF.logF, logdet_S, df);
			O_RMSEA_dfnonzero[rgi] =
				calculate_RMSEA(p, N, logF.logF, logdet_S, df_nonzero);
			O_CFI[rgi] = calculate_CFI(p, N, logF.logF, logdet_S, f0hat, df);
			O_CFI_dfnonzero[rgi] =
				calculate_CFI(p, N, logF.logF, logdet_S, f0hat, df_nonzero);
		}
		O_BIC[rgi] = calculate_BIC(N, logF.logF, df);
		O_BIC_dfnonzero[rgi] = calculate_BIC(N, logF.logF, df_nonzero);
		O_BICH[rgi] = calculate_BICH(p, m2, c, N, logF.logF, df);
		O_EBIC[rgi] =
			calculate_EBIC(O_BIC[rgi], p, m, c, df, ctrl->gamma_ebic);
		O_EBIC_dfnonzero[rgi] =
			calculate_EBIC(O_BIC[rgi], p, m, c, df_nonzero, ctrl->gamma_ebic);
		O_AIC[rgi] = calculate_AIC(N, logF.logF, df);
		O_AIC_dfnonzero[rgi] = calculate_AIC(N, logF.logF, df_nonzero);
		O_CAIC[rgi] = calculate_CAIC(N, logF.logF, df);
		O_CAIC_dfnonzero[rgi] = calculate_CAIC(N, logF.logF, df_nonzero);
	}
	
	vmaxset(vmax);
}

// ---------------------------------------------------------
//  Interface functions
// ---------------------------------------------------------

static SEXP get_list_element(SEXP list, const char *key) {
	SEXP names = getAttrib(list, R_NamesSymbol);
	for (int i = 0; i < length(list); i++) {
		if (strcmp(CHAR(STRING_ELT(names, i)), key) == 0) {
			return VECTOR_ELT(list, i);
		}
	}
	return R_NilValue;
}

static void set_list_element(SEXP list, int i, const char *key, SEXP val) {
	SEXP names = getAttrib(list, R_NamesSymbol);
	SET_VECTOR_ELT(list, i, val);
	SET_STRING_ELT(names, i, mkChar(key));
}

#define __REXTCALL_INIT__ int nprot = 0;
#define REXTCALL_DEF(n,v) SEXP (n); PROTECT((n) = (v)); nprot++;
#define __REXTCALL_CLEAN__ UNPROTECT(nprot); nprot = 0;

SEXP RextCall_fanc(
	SEXP R_p, SEXP R_m, SEXP R_N, SEXP R_S, SEXP R_X, SEXP R_type, SEXP R_ctrl)
{
	__REXTCALL_INIT__
	GetRNGstate();
	ezprof_init(g_ezprof);
	
	int p = INTEGER(R_p)[0], m = INTEGER(R_m)[0], N = INTEGER(R_N)[0];
	struct ctrl_fanc_t ctrl = {
		REAL(get_list_element(R_ctrl, "tol.em"))[0],
		REAL(get_list_element(R_ctrl, "tol.cd"))[0],
		REAL(get_list_element(R_ctrl, "min.uniquevar"))[0],
		REAL(get_list_element(R_ctrl, "tol.bfgs"))[0],
		REAL(get_list_element(R_ctrl, "eta"))[0],
		REAL(get_list_element(R_ctrl, "zita"))[0],
		REAL(get_list_element(R_ctrl, "Delta"))[0],
		REAL(get_list_element(R_ctrl, "init.coef")),
		REAL(get_list_element(R_ctrl, "rho")),
		REAL(get_list_element(R_ctrl, "gamma")),
		REAL(get_list_element(R_ctrl, "max.rho")),
		REAL(get_list_element(R_ctrl, "max.gamma"))[0],
		REAL(get_list_element(R_ctrl, "min.gamma"))[0],
		REAL(get_list_element(R_ctrl, "gamma.ebic"))[0],
		REAL(get_list_element(R_ctrl, "w")),
		INTEGER(get_list_element(R_ctrl, "length.rho"))[0],
		INTEGER(get_list_element(R_ctrl, "length.gamma"))[0],
		REAL(get_list_element(R_ctrl, "alpha.powerseq"))[0],
		INTEGER(get_list_element(R_ctrl, "maxit.em"))[0],
		INTEGER(get_list_element(R_ctrl, "maxit.cd"))[0],
		INTEGER(get_list_element(R_ctrl, "maxit.bfgs"))[0],
		INTEGER(get_list_element(R_ctrl, "cor.factor"))[0],
		1,  // use_penalty
		INTEGER(get_list_element(R_ctrl, "min.rhozero"))[0],
		strcmp(CHAR(STRING_ELT(get_list_element(R_ctrl, "start"), 0)), "cold")
		== 0,
		INTEGER(get_list_element(R_ctrl, "ncand.initial"))[0],
		INTEGER(get_list_element(R_ctrl, "ncand.initial.prenet"))[0],
		INTEGER(get_list_element(R_ctrl, "pmax_for_S"))[0],
		INTEGER(get_list_element(R_ctrl, "progress"))[0],
		INTEGER(get_list_element(R_ctrl, "openmp"))[0],
		INTEGER(get_list_element(R_ctrl, "num.threads"))[0],
		INTEGER(get_list_element(R_ctrl, "model"))[0]
	};
	int num_rhos = ctrl.num_rhos;
	int num_gammas = ctrl.num_gammas;
	
	REXTCALL_DEF(R_rhos, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_gammas, allocVector(REALSXP, num_gammas));
	REXTCALL_DEF(R_m2, allocMatrix(INTSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_spn_Lambda, allocMatrix(INTSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_diag_Psi, allocVector(REALSXP, p * num_rhos * num_gammas));
	REXTCALL_DEF(R_dim_diag_Psi, allocVector(INTSXP, 3));
	INTEGER(R_dim_diag_Psi)[0] = p;
	INTEGER(R_dim_diag_Psi)[1] = num_rhos;
	INTEGER(R_dim_diag_Psi)[2] = num_gammas;
	setAttrib(R_diag_Psi, R_DimSymbol, R_dim_diag_Psi);
	REXTCALL_DEF(R_Phi, allocVector(REALSXP, m * m * num_rhos * num_gammas));
	REXTCALL_DEF(R_dim_Phi, allocVector(INTSXP, 4));
	INTEGER(R_dim_Phi)[0] = INTEGER(R_dim_Phi)[1] = m;
	INTEGER(R_dim_Phi)[2] = num_rhos;
	INTEGER(R_dim_Phi)[3] = num_gammas;
	setAttrib(R_Phi, R_DimSymbol, R_dim_Phi);
	REXTCALL_DEF(R_logF, allocVector(REALSXP, 3 * num_rhos * num_gammas));
	REXTCALL_DEF(R_dim_logF, allocVector(INTSXP, 3));
	INTEGER(R_dim_logF)[0] = 3;
	INTEGER(R_dim_logF)[1] = num_rhos;
	INTEGER(R_dim_logF)[2] = num_gammas;
	setAttrib(R_logF, R_DimSymbol, R_dim_logF);
	REXTCALL_DEF(R_df, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_BIC, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_BICH, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_EBIC, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_AIC, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_CAIC, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_GFI, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_AGFI, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_SRMR, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_RMSEA, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_CFI, allocMatrix(REALSXP, num_rhos, num_gammas));
	
	REXTCALL_DEF(R_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_AIC_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_BIC_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_CAIC_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_EBIC_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_AGFI_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_RMSEA_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_CFI_dfnonzero, allocMatrix(REALSXP, num_rhos, num_gammas));
	
	REXTCALL_DEF(R_cor_factor, allocVector(LGLSXP, 1));
	INTEGER(R_cor_factor)[0] = 
		INTEGER(get_list_element(R_ctrl, "cor.factor"))[0];
	REXTCALL_DEF(R_Npflag, allocVector(LGLSXP, 1));
	INTEGER(R_Npflag)[0] = N >= p;
	REXTCALL_DEF(R_conv, allocVector(INTSXP, 3 * num_rhos * num_gammas)); //R_conv: convergence
	REXTCALL_DEF(R_dim_conv, allocVector(INTSXP, 3));
	INTEGER(R_dim_conv)[0] = 3;
	INTEGER(R_dim_conv)[1] = num_rhos;
	INTEGER(R_dim_conv)[2] = num_gammas;
	setAttrib(R_conv, R_DimSymbol, R_dim_conv);
	
	int *spi_Lambda[num_rhos * num_gammas];
	double *spv_Lambda[num_rhos * num_gammas];
	pfill(num_rhos * num_gammas, (void **)spi_Lambda, NULL); //fill spi_Lambda by NULL
	pfill(num_rhos * num_gammas, (void **)spv_Lambda, NULL);
	
	fanc(p, m, N, REAL(R_S), REAL(R_X), &ctrl, 
		REAL(R_rhos), REAL(R_gammas), INTEGER(R_m2), 
		INTEGER(R_spn_Lambda), spi_Lambda, spv_Lambda, 
		INTEGER(R_type), //type=1 -> MC, type=2 -> prenet, type=3 -> enet
		REAL(R_diag_Psi), REAL(R_Phi), REAL(R_logF), 
		REAL(R_df), REAL(R_BIC), REAL(R_BICH), REAL(R_EBIC),
		REAL(R_AIC), REAL(R_CAIC), REAL(R_GFI), REAL(R_AGFI),
		REAL(R_SRMR), REAL(R_RMSEA), REAL(R_CFI),
		REAL(R_dfnonzero),
		REAL(R_AIC_dfnonzero), REAL(R_BIC_dfnonzero), REAL(R_CAIC_dfnonzero), REAL(R_EBIC_dfnonzero),
		REAL(R_AGFI_dfnonzero), REAL(R_RMSEA_dfnonzero), REAL(R_CFI_dfnonzero),
		INTEGER(R_conv));
	
	REXTCALL_DEF(R_spi_Lambda, allocMatrix(VECSXP, num_rhos, num_gammas));
	REXTCALL_DEF(R_spv_Lambda, allocMatrix(VECSXP, num_rhos, num_gammas));
	for (int i = 0; i < num_rhos * num_gammas; i++) {
		int n = INTEGER(R_spn_Lambda)[i];
		REXTCALL_DEF(R_tmp_spi, allocVector(INTSXP, n));
		memcpy(INTEGER(R_tmp_spi), spi_Lambda[i], n * sizeof(int));
		SET_VECTOR_ELT(R_spi_Lambda, i, R_tmp_spi);
		REXTCALL_DEF(R_tmp_spv, allocVector(REALSXP, n));
		F77_CALL(dcopy)(&n, spv_Lambda[i], INC_SEQ, REAL(R_tmp_spv), INC_SEQ);
		SET_VECTOR_ELT(R_spv_Lambda, i, R_tmp_spv);
		free(spi_Lambda[i]);
		free(spv_Lambda[i]);
	}
	
	REXTCALL_DEF(R_time, allocVector(REALSXP, 4));
	REXTCALL_DEF(R_names_time, allocVector(STRSXP, 4));
	setAttrib(R_time, R_NamesSymbol, R_names_time);
	ezprof_as_SEXP(g_ezprof, R_time, R_names_time);
	
	REXTCALL_DEF(R_rslt, allocVector(VECSXP, 33));
	REXTCALL_DEF(R_names_rslt, allocVector(STRSXP, 33));
	setAttrib(R_rslt, R_NamesSymbol, R_names_rslt);
	
	set_list_element(R_rslt, 0, "factors", R_m);
	set_list_element(R_rslt, 1, "x", R_X);
	set_list_element(R_rslt, 2, "rho", R_rhos);
	set_list_element(R_rslt, 3, "gamma", R_gammas);
	set_list_element(R_rslt, 4, "nonzero.loadings", R_spn_Lambda);
	set_list_element(R_rslt, 5, "spi.loadings", R_spi_Lambda);
	set_list_element(R_rslt, 6, "spv.loadings", R_spv_Lambda);
	set_list_element(R_rslt, 7, "uniquenesses", R_diag_Psi);
	set_list_element(R_rslt, 8, "Phi", R_Phi);
	set_list_element(R_rslt, 9, "likelihood", R_logF);
	set_list_element(R_rslt, 10, "df", R_df);
	set_list_element(R_rslt, 11, "BIC", R_BIC);
	set_list_element(R_rslt, 12, "BIC.H", R_BICH);
	set_list_element(R_rslt, 13, "EBIC", R_EBIC);
	set_list_element(R_rslt, 14, "AIC", R_AIC);
	set_list_element(R_rslt, 15, "CAIC", R_CAIC);
	set_list_element(R_rslt, 16, "GFI", R_GFI);
	set_list_element(R_rslt, 17, "AGFI", R_AGFI);
	set_list_element(R_rslt, 18, "SRMR", R_SRMR);
	set_list_element(R_rslt, 19, "RMSEA", R_RMSEA);
	set_list_element(R_rslt, 20, "CFI", R_CFI);
	set_list_element(R_rslt, 21, "dfnonzero", R_dfnonzero);
	set_list_element(R_rslt, 22, "BIC_dfnonzero", R_BIC_dfnonzero);
	set_list_element(R_rslt, 23, "EBIC_dfnonzero", R_EBIC_dfnonzero);
	set_list_element(R_rslt, 24, "AIC_dfnonzero", R_AIC_dfnonzero);
	set_list_element(R_rslt, 25, "CAIC_dfnonzero", R_CAIC_dfnonzero);
	set_list_element(R_rslt, 26, "AGFI_dfnonzero", R_AGFI_dfnonzero);
	set_list_element(R_rslt, 27, "RMSEA_dfnonzero", R_RMSEA_dfnonzero);
	set_list_element(R_rslt, 28, "CFI_dfnonzero", R_CFI_dfnonzero);
	set_list_element(R_rslt, 29, "cor.factor", R_cor_factor);
	set_list_element(R_rslt, 30, "Npflag", R_Npflag);
	set_list_element(R_rslt, 31, "convergence", R_conv);
	set_list_element(R_rslt, 32, "time", R_time);
	
	ezprof_dispose(g_ezprof);
	PutRNGstate();
	__REXTCALL_CLEAN__
	return R_rslt;
}
