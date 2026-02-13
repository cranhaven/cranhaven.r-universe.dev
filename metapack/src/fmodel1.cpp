#include <iostream>
#include <cmath>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <algorithm>
#include <iterator>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "linearalgebra.h"
#include "loglik_POCov.h"
#include "nelmin.h"
#include "random.h"
// [[Rcpp::depends(RcppArmadillo,RcppProgress)]]

// [[Rcpp::export]]
Rcpp::List fmodel1(const arma::mat& Outcome,
				   const arma::mat& SD,
				   const arma::mat& XCovariate,
				   const arma::mat& WCovariate,
				   const arma::uvec& Treat,
				   const arma::uvec& Trial,
				   const arma::vec& Npt,
				   const double& c0,
				   const double& dj0, // hyperparameter for Omega
				   const double& a0, // hyperparameter for Sigma
				   const double& b0, // hyperparameter for Sigma
				   const arma::mat& Omega0,
				   const int& K, // # of Trials
				   const int& T, // # of Treatments
				   const int& ndiscard,
				   const int& nskip,
				   const int& nkeep,
				   const arma::vec& theta_init,
				   const arma::mat& gamR_init,
				   const arma::mat& Omega_init,
				   const bool& verbose) {
	using namespace arma;
	using namespace std;
	using namespace Rcpp;
	using namespace R;

	const int N = Outcome.n_rows;
	const int J = Outcome.n_cols;
	const int xcols = XCovariate.n_cols;
	const int nw = WCovariate.n_cols;
	const int nt = (xcols + nw) * J;

	arma::field<arma::uvec> idxks(K);
	for (int k = 0; k < K; ++k) {
		uvec idx = find(Trial == k);
		idxks(k) = idx;
	}

	/***********************
	Parameter Initialization
	***********************/
	vec theta = theta_init;
	mat gamR = gamR_init;
	mat Omega = Omega_init;
	mat Omegainv = Omega.i();
	mat Sig_diag(N, J, fill::ones);
	mat Siginv_diag(N, J, fill::ones);

	const mat Omega0inv = arma::inv(Omega0);
	const double shape_omega = static_cast<double>(K) + dj0;
	mat resid = Outcome;

	/*********
	Containers
	*********/
	mat theta_save(nt, nkeep, fill::zeros);
	cube Omega_save(nw*J, nw*J, nkeep, fill::zeros);
	cube Sigma_save(N, J, nkeep, fill::zeros);
	cube Rtk_save(N, J * (J - 1) / 2, nkeep, fill::zeros);
	cube resid_save(N, J, nkeep, fill::zeros);
	/*******************
	Begin burn-in period
	*******************/
	if (verbose) {
		Rcout << "Warming up" << endl;
	}
	{
		Progress prog(ndiscard, verbose);
		for (int idiscard = 0; idiscard < ndiscard; ++idiscard) {
			if (Progress::check_abort()) {
				return Rcpp::List::create(Rcpp::Named("error") = "user interrupt aborted");
			}

			// Update theta
			mat Sig_theta(nt, nt, fill::zeros);
			Sig_theta.diag().fill(1.0 / c0);
			vec mu_theta(nt, fill::zeros);
			for (int k = 0; k < K; ++k) {
				uvec idxk = idxks(k);
				int n_k = idxk.n_elem;
				mat XSX(nt, nt, fill::zeros);
				mat WSX(nw*J, nt, fill::zeros);
				vec WSy(nw*J, fill::zeros);
				vec XSy(nt, fill::zeros);
				mat Sig_gamk = Omegainv;
				for (int i = 0; i < n_k; ++i) {
					int i_k = idxk(i);
					rowvec x_i = XCovariate.row(i_k);
					rowvec w_i = WCovariate.row(i_k);
					rowvec y_i = Outcome.row(i_k);
					double ntk = Npt(i_k);
					mat X(J, xcols*J, fill::zeros);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						X(j, arma::span(j*xcols, (j+1)*xcols-1)) = x_i;
						W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
					}
					mat Xstar = arma::join_horiz(X,W);
					mat Siginv = arma::diagmat(Siginv_diag.row(i_k));
					XSX += ntk * (Xstar.t() * Siginv * Xstar);
					XSy += ntk * (Xstar.t() * Siginv * y_i.t());
					mat WS = ntk * (W.t() * Siginv);
					Sig_gamk += WS * W;
					WSX += WS * Xstar;
					WSy += WS * y_i.t();
				}
				mat Sig_gamk_inv = arma::inv(Sig_gamk);
				Sig_theta += XSX - WSX.t() * Sig_gamk_inv * WSX;
				mu_theta += XSy - WSX.t() * Sig_gamk_inv * WSy;
			}
			Sig_theta = 0.5 * (Sig_theta + Sig_theta.t());
			mat Sig_theta_chol = arma::chol(Sig_theta);
			vec ttmp(nt, fill::randn);
			theta = arma::solve(arma::trimatu(Sig_theta_chol), arma::solve(arma::trimatl(Sig_theta_chol.t()), mu_theta) + ttmp);

			for (int i = 0; i < N; ++i) {
				rowvec x_i = XCovariate.row(i);
				rowvec w_i = WCovariate.row(i);
				rowvec y_i = Outcome.row(i);
				mat X(J, xcols*J, fill::zeros);
				mat W(J, nw*J, fill::zeros);
				for (int j = 0; j < J; ++j) {
					X(j, arma::span(j*xcols, (j+1)*xcols-1)) = x_i;
					W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
				}
				mat Xstar = arma::join_horiz(X,W);
				resid.row(i) = arma::trans(y_i.t() - Xstar * theta);
			}

			// Update gamR
			for (int k = 0; k < K; ++k) {
				mat Siggam = Omegainv;
				vec mugam(nw*J, fill::zeros);
				uvec idxk = idxks(k);
				int n_k = idxk.n_elem;
				for (int i = 0; i < n_k; ++i) {
					int i_k = idxk(i);
					rowvec w_i = WCovariate.row(i_k);
					rowvec resid_i = resid.row(i_k);
					double ntk = Npt(i_k);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
					}
					mat Siginv = arma::diagmat(Siginv_diag.row(i_k));
					mat WS = W.t() * Siginv;
					Siggam += ntk * (WS * W);
					mugam += ntk * (WS * resid_i.t());
				}
				Siggam = 0.5 * (Siggam + Siggam.t());
				mat SiggamChol = arma::chol(Siggam);
				vec gtmp(nw*J, fill::randn);
				gamR.col(k) = arma::solve(arma::trimatu(SiggamChol), arma::solve(arma::trimatl(SiggamChol.t()), mugam) + gtmp);
			}

			// Update Omega
			for (int jj = 0; jj < J; ++jj) {
				mat gamstar = gamR.rows(nw*jj, nw*(jj+1)-1);
				mat qq = Omega0inv + (gamstar * gamstar.t());
				mat ominv = RNG::rwish(arma::inv(qq), shape_omega);
				mat om = arma::inv(ominv);
				Omegainv(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = ominv;
				Omega(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = om;
			}

			// Update Sigma
			for (int i = 0; i < N; ++i) {
				int k = Trial(i);
				double ntk = Npt(i);
				double shape = a0 + 0.5 * ntk;
				rowvec sd2_i = arma::square(SD.row(i));
				vec gam_k = gamR.col(k);
				rowvec w_i = WCovariate.row(i);
				mat W(J, nw*J, fill::zeros);
				for (int j = 0; j < J; ++j) {
					W(j, span(j*nw, (j+1)*nw-1)) = w_i;
				}
				vec resid_i2 = arma::square(arma::trans(resid.row(i)) - W * gam_k);
				vec sig2inv(J);
				for (int j = 0; j < J; ++j) {
					double rate = b0 + ntk * 0.5 * resid_i2(j) + (ntk - 1.0) * 0.5 * sd2_i(j);
					sig2inv(j) = ::Rf_rgamma(shape, 1.0) / rate;
				}
				Siginv_diag.row(i) = arma::trans(sig2inv);
			}
			prog.increment();
		}
	}

	/*******************
	Begin Sampling period
	*******************/
	if (verbose) {
		Rcout << "Sampling" << endl;
	}
	{
		Progress prog(nkeep, verbose);
		for (int ikeep = 0; ikeep < nkeep; ++ikeep) {
			if (Progress::check_abort()) {
				return Rcpp::List::create(Rcpp::Named("error") = "user interrupt aborted");
			}
			mat resid_ikeep(N, J, fill::zeros);
			for (int iskip = 0; iskip < nskip; ++iskip) {
				// Update theta
				mat Sig_theta(nt, nt, fill::zeros);
				Sig_theta.diag().fill(1.0 / c0);
				vec mu_theta(nt, fill::zeros);
				for (int k = 0; k < K; ++k) {
					uvec idxk = idxks(k);
					int n_k = idxk.n_elem;
					mat XSX(nt, nt, fill::zeros);
					mat WSX(nw*J, nt, fill::zeros);
					vec WSy(nw*J, fill::zeros);
					vec XSy(nt, fill::zeros);
					mat Sig_gamk = Omegainv;
					for (int i = 0; i < n_k; ++i) {
						int i_k = idxk(i);
						rowvec x_i = XCovariate.row(i_k);
						rowvec w_i = WCovariate.row(i_k);
						rowvec y_i = Outcome.row(i_k);
						double ntk = Npt(i_k);
						mat X(J, xcols*J, fill::zeros);
						mat W(J, nw*J, fill::zeros);
						for (int j = 0; j < J; ++j) {
							X(j, arma::span(j*xcols, (j+1)*xcols-1)) = x_i;
							W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
						}
						mat Xstar = arma::join_horiz(X,W);
						mat Siginv = arma::diagmat(Siginv_diag.row(i_k));
						XSX += ntk * (Xstar.t() * Siginv * Xstar);
						XSy += ntk * (Xstar.t() * Siginv * y_i.t());
						mat WS = ntk * (W.t() * Siginv);
						Sig_gamk += WS * W;
						WSX += WS * Xstar;
						WSy += WS * y_i.t();
					}
					mat Sig_gamk_inv = arma::inv(Sig_gamk);
					Sig_theta += XSX - WSX.t() * Sig_gamk_inv * WSX;
					mu_theta += XSy - WSX.t() * Sig_gamk_inv * WSy;
				}
				Sig_theta = 0.5 * (Sig_theta + Sig_theta.t());
				mat Sig_theta_chol = arma::chol(Sig_theta);
				vec ttmp(nt, fill::randn);
				theta = arma::solve(arma::trimatu(Sig_theta_chol), arma::solve(arma::trimatl(Sig_theta_chol.t()), mu_theta) + ttmp);

				for (int i = 0; i < N; ++i) {
					rowvec x_i = XCovariate.row(i);
					rowvec w_i = WCovariate.row(i);
					rowvec y_i = Outcome.row(i);
					mat X(J, xcols*J, fill::zeros);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						X(j, arma::span(j*xcols, (j+1)*xcols-1)) = x_i;
						W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
					}
					mat Xstar = arma::join_horiz(X,W);
					resid.row(i) = arma::trans(y_i.t() - Xstar * theta);
				}

				// Update gamR
				for (int k = 0; k < K; ++k) {
					mat Siggam = Omegainv;
					vec mugam(nw*J, fill::zeros);
					uvec idxk = idxks(k);
					int n_k = idxk.n_elem;
					for (int i = 0; i < n_k; ++i) {
						int i_k = idxk(i);
						rowvec w_i = WCovariate.row(i_k);
						rowvec resid_i = resid.row(i_k);
						double ntk = Npt(i_k);
						mat W(J, nw*J, fill::zeros);
						for (int j = 0; j < J; ++j) {
							W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
						}
						mat Siginv = arma::diagmat(Siginv_diag.row(i_k));
						mat WS = W.t() * Siginv;
						Siggam += ntk * (WS * W);
						mugam += ntk * (WS * resid_i.t());
					}
					Siggam = 0.5 * (Siggam + Siggam.t());
					mat SiggamChol = arma::chol(Siggam);
					vec gtmp(nw*J, fill::randn);
					gamR.col(k) = arma::solve(arma::trimatu(SiggamChol), arma::solve(arma::trimatl(SiggamChol.t()), mugam) + gtmp);
				}

				// Update Omega
				for (int jj = 0; jj < J; ++jj) {
					mat gamstar = gamR.rows(nw*jj, nw*(jj+1)-1);
					mat qq = Omega0inv + (gamstar * gamstar.t());
					mat ominv = RNG::rwish(arma::inv(qq), shape_omega);
					mat om = arma::inv(ominv);
					Omegainv(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = ominv;
					Omega(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = om;
				}

				// Update Sigma
				for (int i = 0; i < N; ++i) {
					int k = Trial(i);
					double ntk = Npt(i);
					double shape = a0 + 0.5 * ntk;
					rowvec sd2_i = arma::square(SD.row(i));
					vec gam_k = gamR.col(k);
					rowvec w_i = WCovariate.row(i);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						W(j, span(j*nw, (j+1)*nw-1)) = w_i;
					}
					vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
					resid_ikeep.row(i) = resid_i.t();
					vec resid_i2 = arma::square(resid_i);
					vec sig2inv(J);
					for (int j = 0; j < J; ++j) {
						double rate = b0 + ntk * 0.5 * resid_i2(j) + (ntk - 1.0) * 0.5 * sd2_i(j);
						sig2inv(j) = ::Rf_rgamma(shape, 1.0) / rate;
					}
					Siginv_diag.row(i) = arma::trans(sig2inv);
					Sig_diag.row(i) = 1.0 / arma::trans(sig2inv);
				}
			}
			theta_save.col(ikeep) = theta;
			Sigma_save.slice(ikeep) = Sig_diag;
			Omega_save.slice(ikeep) = Omega;
			resid_save.slice(ikeep) = resid_ikeep;
			prog.increment();
		}
	}
	return Rcpp::List::create(
			Rcpp::Named("resid") = resid_save,
			Rcpp::Named("theta") = theta_save,
			Rcpp::Named("Sigma") = Sigma_save,
			Rcpp::Named("Omega") = Omega_save
		);
}



