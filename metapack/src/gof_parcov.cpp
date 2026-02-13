#include <cmath>
#include <Rmath.h>
#include <algorithm>
#include <iterator>
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rdefines.h>
#include "linearalgebra.h"
#ifdef _OPENMP
  #include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo, RcppProgress)]]


/**************************************************
Calculate the goodness of fit measures

+ Dev(theta) = -2 * log L(theta | D_oy)
+ p_D = E[Dev(theta)] - Dev(thetabar)
+ DIC = Dev(thetabar) + 2 * p_D
***************************************************/

double mvnpdf(const arma::vec& x, const arma::vec& mu, const arma::mat& Sig, const bool logp) {
	using namespace arma;

	int k = x.n_elem;
	double logdet_val;
	double logdet_sign;
	log_det(logdet_val, logdet_sign, Sig);
	double lpdf = -static_cast<double>(k) * M_LN_SQRT_2PI	- 0.5 * logdet_val - 0.5 * arma::dot(x - mu, arma::solve(Sig, x - mu));
	
	return logp ? lpdf : std::exp(lpdf);
}

// [[Rcpp::export]]
Rcpp::List lpml_parcov(const arma::mat& Outcome,
			  		   const arma::mat& XCovariate,
			  		   const arma::mat& WCovariate,
			  		   const arma::vec& Npt,
			  		   const arma::cube& Sigma,
			  		   const arma::cube& Omega,
			  		   const arma::mat& theta,
			  		   const arma::vec& thetahat,
			  		   const arma::mat& Sigmahat,
			  		   const arma::mat& Omegahat,
			  		   const int& fmodel,
			  		   const int& nkeep,
			  		   const bool& verbose,
			  		   const bool& grouped,
			  		   const arma::uvec& Second,
			  		   const int& ncores) {
	using namespace arma;
	const int N = Outcome.n_rows;
	const int J = Outcome.n_cols;
	const int xcols = XCovariate.n_cols;
	const int nn = WCovariate.n_cols;
	const int nw = (grouped) ?  nn*2 : nn;
	mat g(N, nkeep, fill::zeros);
	vec alogcpo(N, fill::zeros);
	double alpml = 0.0;
	{
		Progress prog(nkeep, verbose);
		#ifdef _OPENMP
		#pragma omp parallel for schedule(static) num_threads(ncores)
		#endif
		for (int ikeep = 0; ikeep < nkeep; ++ikeep) {
			if (!Progress::check_abort()) {
				mat Sig_ikeep = Sigma.slice(ikeep);
				mat Omega_ikeep = Omega.slice(ikeep);
				vec theta_ikeep = theta.col(ikeep);

				for (int i = 0; i < N; ++i) {
					double ntk = Npt(i);
					rowvec x_i = XCovariate.row(i);
					rowvec w_i = WCovariate.row(i);
					vec y_i = arma::trans(Outcome.row(i));
					mat X(J, xcols * J, fill::zeros);
					mat W(J, nw*J, fill::zeros);
					if (!grouped) {
						for (int j = 0; j < J; ++j) {
							X(j, span(j*xcols, (j+1)*xcols-1)) = x_i;
							W(j, span(j*nw, (j+1)*nw-1)) = w_i;
						}
					} else {
						rowvec wstar_i(nw, fill::zeros);
						if (Second(i) == 0) {
							wstar_i.head(nn) =  w_i;
						} else {
							wstar_i.tail(nn) =  w_i;
						}
						for (int j = 0; j < J; ++j) {
							X(j, span(j*xcols, (j+1)*xcols-1)) = x_i;
							W(j, span(j*nw, (j+1)*nw-1)) = wstar_i;
						}
					}
					mat Xstar = arma::join_horiz(X, W);
					if (fmodel != 2) {
						mat Sig_i;
						if (fmodel == 1) {
							Sig_i = diagmat(Sig_ikeep.row(i));
						} else {
						Sig_i = vechinv(arma::trans(Sig_ikeep.row(i)), J);
						}
						mat Q = Sig_i / ntk + W * Omega_ikeep * W.t();
						g(i,ikeep) -= mvnpdf(y_i, Xstar * theta_ikeep, Q, true);
					} else {
						mat Q = Sig_ikeep / ntk + W * Omega_ikeep * W.t();
						g(i,ikeep) -= mvnpdf(y_i, Xstar * theta_ikeep, Q, true);
					}
				}
				prog.increment();
			}
		}

		vec gmax(N, fill::zeros);
		for (int i = 0; i < N; ++i) {
			gmax(i) = g(i,0);
			for (int j1 = 1; j1 < nkeep; ++j1) {
				if (gmax(i) < g(i, j1)) {
					gmax(i) = g(i, j1);
				}
			}
			double sumrep = 0.0;
			for (int j1 = 1; j1 < nkeep; ++j1) {
				sumrep += std::exp(g(i,j1) - gmax(i));
			}
			alogcpo(i) -= gmax(i) + std::log(sumrep / static_cast<double>(nkeep));
			alpml += alogcpo(i);
		}
	}
	return Rcpp::List::create(Rcpp::Named("lpml")=alpml, Rcpp::Named("logcpo")=alogcpo);
}


// [[Rcpp::export]]
Rcpp::List dic_parcov(const arma::mat& Outcome,
			  		  const arma::mat& XCovariate,
			  		  const arma::mat& WCovariate,
			  		  const arma::vec& Npt,
			  		  const arma::cube& Sigma,
			  		  const arma::cube& Omega,
			  		  const arma::mat& theta,
			  		  const arma::vec& thetahat,
			  		  const arma::mat& Sigmahat,
			  		  const arma::mat& Omegahat,
			  		  const int& fmodel,
			  		  const int& nkeep,
			  		  const bool& verbose,
			  		  const bool& grouped,
			  		  const arma::uvec& Second,
			  		  const int& ncores) {
	using namespace arma;
	double Dev_thetabar = 0.0;
	const int N = Outcome.n_rows;
	const int J = Outcome.n_cols;
	const int xcols = XCovariate.n_cols;
	const int nn = WCovariate.n_cols;
	const int nw = (grouped) ? nn*2 : nn;
	#ifdef _OPENMP
	#pragma omp parallel for schedule(static) num_threads(ncores)
	#endif
	for (int i = 0; i < N; ++i) {
		double ntk = Npt(i);
		rowvec x_i = XCovariate.row(i);
		rowvec w_i = WCovariate.row(i);
		vec y_i = arma::trans(Outcome.row(i));
		mat X(J, xcols * J, fill::zeros);
		mat W(J, nw*J, fill::zeros);
		if (!grouped) {
			for (int j = 0; j < J; ++j) {
				X(j, span(j*xcols, (j+1)*xcols-1)) = x_i;
				W(j, span(j*nw, (j+1)*nw-1)) = w_i;
			}
		} else {
			rowvec wstar_i(nw, fill::zeros);
			if (Second(i) == 0) {
				wstar_i.head(nn) =  w_i;
			} else {
				wstar_i.tail(nn) =  w_i;
			}
			for (int j = 0; j < J; ++j) {
				X(j, span(j*xcols, (j+1)*xcols-1)) = x_i;
				W(j, span(j*nw, (j+1)*nw-1)) = wstar_i;
			}
		}
		mat Xstar = arma::join_horiz(X, W);
		if (fmodel != 2) {
			mat Sighat;
			if (fmodel == 1) {
				Sighat = arma::diagmat(Sigmahat.row(i));
			} else {
				Sighat = vechinv(arma::trans(Sigmahat.row(i)), J);
			}
			mat Q = Sighat / ntk + W * Omegahat * W.t();
			Dev_thetabar -= 2.0 * mvnpdf(y_i, Xstar * thetahat, Q, true);
		} else {
			mat Q = Sigmahat / ntk + W * Omegahat * W.t();
			Dev_thetabar -= 2.0 * mvnpdf(y_i, Xstar * thetahat, Q, true);
		}
	}


	double Dev_bar = 0.0;
	{
		Progress prog(nkeep, verbose);
		#ifdef _OPENMP
		#pragma omp parallel for schedule(static) num_threads(ncores)
		#endif
		for (int ikeep = 0; ikeep < nkeep; ++ikeep) {
			if (!Progress::check_abort()) {
				mat Sig_ikeep = Sigma.slice(ikeep);
				mat Omega_ikeep = Omega.slice(ikeep);
				vec theta_ikeep = theta.col(ikeep);

				for (int i = 0; i < N; ++i) {
					double ntk = Npt(i);
					rowvec x_i = XCovariate.row(i);
					rowvec w_i = WCovariate.row(i);
					vec y_i = arma::trans(Outcome.row(i));
					mat X(J, xcols * J, fill::zeros);
					mat W(J, nw*J, fill::zeros);
					if (!grouped) {
						for (int j = 0; j < J; ++j) {
							X(j, span(j*xcols, (j+1)*xcols-1)) = x_i;
							W(j, span(j*nw, (j+1)*nw-1)) = w_i;
						}
					} else {
						rowvec wstar_i(nw, fill::zeros);
						if (Second(i) == 0) {
							wstar_i.head(nn) =  w_i;
						} else {
							wstar_i.tail(nn) =  w_i;
						}
						for (int j = 0; j < J; ++j) {
							X(j, span(j*xcols, (j+1)*xcols-1)) = x_i;
							W(j, span(j*nw, (j+1)*nw-1)) = wstar_i;
						}
					}
					mat Xstar = arma::join_horiz(X, W);
					if (fmodel != 2) {
						mat Sig_i;
						if (fmodel == 1) {
							Sig_i = diagmat(Sig_ikeep.row(i));
						} else {
							Sig_i = vechinv(arma::trans(Sig_ikeep.row(i)), J);
						}
						mat Q = Sig_i / ntk + W * Omega_ikeep * W.t();
						Dev_bar -= 2.0 * mvnpdf(y_i, Xstar * theta_ikeep, Q, true);
					} else {
						mat Q = Sig_ikeep / ntk + W * Omega_ikeep * W.t();
						Dev_bar -= 2.0 * mvnpdf(y_i, Xstar * theta_ikeep, Q, true);
					}
				}
				prog.increment();
			}
		}
	}
	Dev_bar /= static_cast<double>(nkeep);
	double p_D = Dev_bar - Dev_thetabar;
	double DIC = Dev_thetabar + 2.0 * p_D;
	return Rcpp::List::create(Rcpp::Named("dic") = DIC, Rcpp::Named("Dev") = Dev_thetabar,
							  Rcpp::Named("pD") = p_D);
}

// [[Rcpp::export]]
arma::mat pearson_parcov(const arma::cube& resid,
			  		  const arma::vec& Npt,
			  		  const arma::cube& Sigma,
			  		  const int& fmodel,
			  		  const int& nkeep,
			  		  const bool& verbose) {
	using namespace arma;
	const int N = resid.n_rows;
	const int J = resid.n_cols;
	cube Sig(N, J, nkeep, fill::zeros);
	mat Er = arma::mean(resid, 2);
	mat Varr(N, J, fill::zeros);
	if (fmodel != 1) {
		Progress prog(nkeep, verbose);
		for (int ikeep = 0; ikeep < nkeep; ++ikeep) {
			if (!Progress::check_abort()) {
				Varr += arma::square(resid.slice(ikeep) - Er);
				mat Sig_ikeep(N, J, fill::zeros);
				mat Sigma_ikeep = Sigma.slice(ikeep);
				for (int i = 0; i < N; ++i)
				{
					if (fmodel == 2) {
						// when fmodel == 2
						Sig_ikeep.row(i) = arma::trans(arma::diagvec(Sig_ikeep)) / Npt(i);
					} else {
						mat tmp = vechinv(arma::trans(Sigma_ikeep.row(i)), J);
						Sig_ikeep.row(i) = arma::trans(arma::diagvec(tmp)) / Npt(i);
					}
				}
				Sig.slice(ikeep) = Sig_ikeep;
				prog.increment();
			}
		}
	} else {
		Progress prog(nkeep, verbose);
		for (int ikeep = 0; ikeep < nkeep; ++ikeep)
		{
			if (!Progress::check_abort())
			{
				mat Sigma_ikeep = Sigma.slice(ikeep);
				for (int i = 0; i < N; ++i)
				{
					Sigma_ikeep.row(i) /= Npt(i);
				}
				Sig.slice(ikeep) = Sigma_ikeep;
				prog.increment();
			}
		}
	}
	Varr /= static_cast<double>(nkeep);
	mat Sighat = arma::mean(Sig, 2);
	return Er / arma::sqrt(Varr + Sighat);
}
