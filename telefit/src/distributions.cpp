#include "distributions.h"

using namespace Rcpp;
using namespace arma;

using Eigen::PermutationMatrix;
using Eigen::Dynamic;
using Eigen::SparseMatrix;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::LLT;
using Eigen::Map;
using Eigen::Lower;

const double log2pi = 1.8378770664;

/*
 log density for intrinsic GMRF with mean 0 and precision given by Q.  Q takes
 the form z * R, where q is a scaling constant and R is the sparse structural
 form for Q.

 Parameters:
  x - vector at which to evaluate density
  n - dimension of Q
  k - rank deficiency of Q
  R - structural form of Q
  q - scale for Q
  ldetR - log of the generalized determinant of R
*/
double mcstat2::ldigmrfSp(double* x, int n, int k,
  const SparseMatrix<double>& R, double q, double ldetR) {

    Map<VectorXd> Xv(x, n);
    double qform = q * Xv.dot(R * Xv);

    return - 0.5 * ( (n-k) * log2pi - ldetR + qform );
}

double mcstat2::ldigmrfSpD(double* x, int m, int n, int k,
  const SparseMatrix<double>& R, double q, double ldetR,
  const LLT<MatrixXd, Lower>& L) {

    // normalizing constant components
    double ldetQ1 = n * log(q) + ldetR;
    double ldetQ2 = - 2 * L.matrixLLT().diagonal().array().log().sum();

    //
    // quadratic form, evaluated efficiently wrt. kronecker structure
    //

    Map<MatrixXd> Xm(x,m,n);
    MatrixXd W = Xm * R * q;
    MatrixXd U = L.solve(W);

    double mn = m*n;
    Map<VectorXd> Xv(x, mn);
    Map<VectorXd> Uv(U.data(), mn);
    double qform = Xv.dot(Uv);

    return - 0.5 * (m * (n-k) * log2pi - m * ldetQ1 - (n-k) * ldetQ2 + qform);
}


double mcstat2::ldinvgamma(double x, double a, double b) {
    using namespace std;
    return a * log(b) - lgamma(a) - (a+1) * log(x) - b / x;
}

double mcstat2::ldunif(double x, double a, double b) {
    using namespace std;
    return - log(b-a);
}

double mcstat2::logdinvgamma_unscaled(double x, double a, double b) {
	return - (a + 1.0) * log(x) - b/x;
}

double mcstat2::logdbeta_unscaled(double x, double a, double b) {
	return (a-1) * log(x) + (b-1) * log(1-x);
}


double mcstat2::loglognormal_unscaled(double x, double mu, double sigma) {
	double logx = log(x);
	return - logx - log(sigma) - .5 * pow(logx - mu, 2.0) / pow(sigma, 2.0);
}

vec mcstat2::qintnorm(const vec & breaks, double mu, double sigma) {
	// compute normal probabilities for intervals specified by breaks

	int n = breaks.n_elem + 1;

	vec res = vec(n, fill::zeros);

	double F0 = 0;
	for(int i=0; i<(n-1); i++) {
		double F = Rf_pnorm5(breaks.at(i), mu, sigma, 1, 0);
		res.at(i) = F - F0;
		F0 = F;
	}
	res.at(n-1) = 1 - F0;

	return res;
}

vec mcstat2::mvrnorm(const mat & sigma) {
	return chol(sigma, "lower") * randn<vec>(sigma.n_rows, 1);
}

vec mcstat2::mvrnorm_chol(const mat & R) {
	// Note: This assumes that the cholesky is stored in uppertri
	// format for more efficient vector operations. it also uses in-place evaluation

	vec z = randn<vec>(R.n_rows, 1);
	double* z_out = z.memptr() + R.n_rows;
	double* z_mem = z.memptr();

	for(int i=R.n_rows-1; i>=0; i--){
		const double* R_mem = R.colptr(i);
		*(--z_out) *= *(R_mem + i);
		for(int j=i-1; j>=0; j--)
			*z_out += *(z_mem+j) * *(R_mem+j);
	}

	return z;
}

vec mcstat2::mvrnorm_cholkron(const mat & Ra, const mat & Rb) {
	//TODO: take advantage of Ra and Rb's triangular forms
	return vectorise( Rb.t() * randn<mat>(Rb.n_rows, Ra.n_rows) * Ra);
}

vec mcstat2::mvrnorm_spchol(const sp_mat & L) {
	// redo this by using a sparse backsolve after determining ordering
	// will likely need to use lapack routines for further efficiency
	return L * randn<vec>(L.n_rows, 1);
}

vec mcstat2::mvrnorm_spchol(const SparseMatrix<double> &QL,
						   const PermutationMatrix<Dynamic,Dynamic> &QPinv,
						   int n) {
	// generate independent normal variates
	vec t_z = randn(n);

	// map variates into eigen
	using Eigen::Map;
	using Eigen::VectorXd;
	Map<VectorXd> z(t_z.memptr(), n);

	// solve using sparse cholesky to generate sample
	using Eigen::Lower;
	VectorXd t_x = QPinv * QL.triangularView<Lower>().transpose().solve(z);

	// convert sample to arma vec
	vec x = vec(t_x.data(), n);

	return x;
}

vec mcstat2::mvrnorm_spcholkron(const SparseMatrix<double> &QL,
							   const PermutationMatrix<Dynamic,Dynamic> &QPinv,
							   int Qn,
							   const MatrixXd &UA,
							   int Un) {
	// generate independent normal variates
	mat t_z = randn<mat>(Un, Qn);

	// map variates into eigen
	using Eigen::Map;
	using Eigen::MatrixXd;
	Map<MatrixXd> z(t_z.memptr(), Un, Qn);

	// solve using sparse cholesky to generate sample
	using Eigen::Lower;
	MatrixXd t_x = (QPinv * QL.triangularView<Lower>().transpose().solve(
								(UA.triangularView<Lower>() * z).transpose()
							   )).transpose();

	// convert sample to arma vec
	vec x = vec(t_x.data(), Qn * Un);

	return x;
}

vec mcstat2::mvrnorm(const vec & mu, const mat & sigma) {
	return mu + chol(sigma, "lower") * randn<vec>(sigma.n_rows, 1);
}

vec mcstat2::mvrnorm_chol(const vec & mu, const mat & L) {
	return mu + L * randn<vec>(L.n_rows, 1);
}

vec mcstat2::mvrnorm(const vec &mu, const mat &sigma, bool precision) {
	if(precision) {
		return solve(chol(sigma, "upper"), randn<vec>(sigma.n_rows, 1)) + mu;
	} else {
		return mu + chol(sigma, "lower") * randn<vec>(sigma.n_rows, 1);
	}
}

mat mcstat2::rwishart(const mat &V, double n) {
	// Params:
	//  n - degrees of freedom
	//  V - (symmetric) scale matrix

	int p = V.n_rows;
	mat A = mat(p, p, fill::zeros);

	// fill diagonal
	for(int i=0; i<p; i++)
		A(i,i) = sqrt(R::rchisq(n-i));

	// fill lower-triangular portion of matrix
	for(int i=1; i<p; i++)
		for(int j=0; j<i; j++)
			A(i,j) = R::rnorm(0,1);

	mat C = chol(V, "lower") * trimatl(A);
	return C * C.t();
}

mat mcstat2::rinvwishart(const mat &V, double n) {
	return inv_sympd( mcstat2::rwishart(inv_sympd(V), n) );
}


mat mcstat2::mvrnorm(mat & Sigma, int nSamples, bool precision) {

	using Eigen::MatrixXd;
	using Eigen::Map;
	using Eigen::LLT;
	using Eigen::Upper;
	using Eigen::Lower;

	int n = Sigma.n_rows;

	// generate independent normal variates
	GetRNGstate();
	mat t_z = randn(n, nSamples);
	PutRNGstate();

	// map variates into eigen
	Map<MatrixXd> z(t_z.memptr(), n, nSamples);

	// internal storage for r.v.s
	MatrixXd t_x;

	if(precision) {

		// read precision matrix into eigen
		Map<MatrixXd> Q(Sigma.memptr(), n, n);

		// factor precision matrix; store upper cholesky
		LLT<MatrixXd, Upper> llt(Q);

		// compute r.v. with precision matrix Q
		t_x = llt.matrixU().solve(z);

	} else {

		// read covariance matrix into eigen
		Map<MatrixXd> S(Sigma.memptr(), n, n);

		// factor covariance matrix; store lower cholesky
		LLT<MatrixXd, Lower> llt(S);

		// compute r.v. with covariance matrix S
		t_x = llt.matrixL() * z;
	}

	// convert samples to arma vec
	mat x = mat(t_x.data(), Sigma.n_rows, nSamples);
	return x;
}


mat mcstat2::mvrnorm_post(vec & y, mat & Sigma, int nSamples, bool precision) {

	using Eigen::MatrixXd;
	using Eigen::VectorXd;
	using Eigen::Map;
	using Eigen::LLT;
	using Eigen::Upper;

	if(precision) {

		// read precision matrix into eigen
		int n = Sigma.n_rows;
		Map<MatrixXd> Q(Sigma.memptr(), n, n);

		// read mean component into eigen
		Map<MatrixXd> t_y(y.memptr(), n, 1);

		// factor precision matrix and compute r.v. mean; store upper cholesky
		LLT<MatrixXd, Upper> llt(Q);
		VectorXd mu = llt.solve(t_y);

		// generate independent normal variates
		GetRNGstate();
		mat t_z = randn(n, nSamples);
		PutRNGstate();

		// map variates into eigen
		Map<MatrixXd> z(t_z.memptr(), n, nSamples);

		// compute r.v. with precision matrix Q and mean mu
		MatrixXd t_x = llt.matrixU().solve(z);
		t_x.colwise() += mu;

		// convert to arma vec
		mat x = mat(t_x.data(), Sigma.n_rows, nSamples);

		return x;
	} else {
		return zeros(1);
	}
}


mat mcstat2::mvrnorm_postKron(vec & t_y, mat & t_A, mat & t_B, int nSamples,
							  bool precision) {
	using Eigen::MatrixXd;
	using Eigen::VectorXd;
	using Eigen::Map;
	using Eigen::LLT;
	using Eigen::Upper;

	if(precision) {

		// read precision matrices into eigen
		int nA = t_A.n_rows;
		int nB = t_B.n_rows;
		Map<MatrixXd> Qa(t_A.memptr(), nA, nA);
		Map<MatrixXd> Qb(t_B.memptr(), nB, nB);

		// read mean component into eigen as matrix
		Map<MatrixXd> Y(t_y.memptr(), nB, nA);

		// factor precision matrix and compute r.v. mean; store upper choleskys
		LLT<MatrixXd, Upper> lltA(Qa);
		LLT<MatrixXd, Upper> lltB(Qb);
		MatrixXd b = lltB.solve(Y);
		MatrixXd muMat = lltA.solve(b.transpose()).transpose();
		VectorXd mu(Map<VectorXd>(muMat.data(), nA * nB));

		// generate independent normal variates
		GetRNGstate();
		mat t_z = randn(nB, nA * nSamples);
		PutRNGstate();

		// map variates into eigen
		Map<MatrixXd> z(t_z.memptr(), nB, nA * nSamples);

		// compute r.v. with precision matrix A x B (but in matrix form)
		MatrixXd t_x(nB, nA * nSamples);
		for(int i=0; i<nSamples; i++) {
			MatrixXd b = lltB.matrixU().solve(z.block(0, i*nA, nB, nA));
			t_x.block(0, i*nA, nB, nA) =
				lltA.matrixU().solve(b.transpose()).transpose();
		}

		// reshape to column vectors, add mean mu
		Map<MatrixXd> t_xV(t_x.data(), nA * nB, nSamples);
		t_xV.colwise() += mu;

		// convert to arma vec
		mat x = mat(t_xV.data(), nA * nB, nSamples);

		return x;
	} else {
		return zeros(1);
	}
}




//
// Rcpp exports
//


// [[Rcpp::export]]
arma::mat r_mc2_rinvwishart(arma::mat V, double n) {
	return mcstat2::rinvwishart(V, n);
}


// [[Rcpp::export]]
arma::mat r_mvrnorm_postKron(arma::vec y, arma::mat A, arma::mat B,
							int nSamples, bool precision) {
	return mcstat2::mvrnorm_postKron(y, A, B, nSamples, precision);
}


// [[Rcpp::export]]
arma::mat r_mvrnorm_post(arma::vec y, arma::mat Sigma, int nSamples,
						 bool precision) {
	return mcstat2::mvrnorm_post(y, Sigma, nSamples, precision);
}


// [[Rcpp::export]]
arma::vec r_qintnorm(arma::vec breaks, double mu, double sigma) {
	return mcstat2::qintnorm(breaks, mu, sigma);
}

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
NumericVector dtest(NumericVector x, int m, int n, int k,
	Eigen::SparseMatrix< double > R, double q, double ldetR,
	Eigen::MatrixXd Sigma) {

		std::vector<double> x_v = Rcpp::as<std::vector<double> >(x);
		double* x_data = &x_v[0];

 		Eigen::LLT<Eigen::MatrixXd, Eigen::Lower> L(Sigma);

	return wrap(mcstat2::ldigmrfSpD(x_data, m, n, k, R, q, ldetR, L));
}
