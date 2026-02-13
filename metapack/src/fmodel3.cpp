#include <iostream>
#include <cmath>
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <algorithm>
#include <iterator>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "ramcmc.h"
#include "linearalgebra.h"
#include "loglik_POCov.h"
#include "nelmin.h"
#include "random.h"
// [[Rcpp::depends(RcppArmadillo,RcppProgress)]]

/*
* - DY February 7, 2023: replaced try-catch blocks for positive-definiteness checking with Armadillo .is_sympd()
*/

// [[Rcpp::export]]
Rcpp::List fmodel3(const arma::mat& Outcome,
				   const arma::mat& SD,
				   const arma::mat& XCovariate,
				   const arma::mat& WCovariate,
				   const arma::uvec& Treat,
				   const arma::uvec& Trial,
				   const arma::vec& Npt,
				   const double& c0,
				   const double& dj0, // hyperparameter for Omega
				   const double& a0, // hyperparameter for Sigma
				   const double& b0,
				   const arma::mat& Omega0,
				   const int& K, // # of Trials
				   const int& T, // # of Treatments
				   const int& ndiscard,
				   const int& nskip,
				   const int& nkeep,
				   const double& delta_stepsize,
				   const double& Rho_stepsize,
				   const double& R_stepsize,
				   const arma::vec& theta_init,
				   const arma::mat& gamR_init,
				   const arma::mat& Omega_init,
				   const arma::mat& Rho_init,
				   const bool& sample_Rho,
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
	const int JJm12 = (J * (J-1)) / 2;

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
	mat Sig_lt(N, (J*(J+1))/2, fill::zeros); // store the diagonal-inclusive lower triangular (lt) elements of Sig
	mat Siginv_lt(N, (J*(J+1))/2, fill::zeros); // store the diagonal-inclusive lower triangular (lt) elements of Siginv
	mat vRtk(N, J * (J - 1) / 2, fill::zeros); // store the off-diagonal lower triangular elements of normal variates for Rtk
	vRtk.fill(0.5);
	mat delta = SD;
	mat Rho = Rho_init;
	mat pRho = Rho_to_pRho(Rho);
	mat Rhoinv = arma::inv(Rho);
	vec vRho = arma::atanh(vecr(pRho));

	for (int i = 0; i < N; ++i) {
		mat Sigma = arma::diagmat(delta.row(i)) * Rho * arma::diagmat(delta.row(i));
		mat Sigmainv = arma::diagmat(1.0 / delta.row(i)) * Rhoinv * arma::diagmat(1.0 / delta.row(i));
		Sig_lt.row(i) = arma::trans(vech(Sigma));
		Siginv_lt.row(i) = arma::trans(vech(Sigmainv));
	}


	const mat Omega0inv = arma::inv_sympd(Omega0);
	const double sumNpt = arma::accu(Npt);
	const double shape_omega = static_cast<double>(K) + dj0;
	mat resid = Outcome;
	mat delta_rates(arma::size(delta), fill::zeros);
	double vRho_rates = 0.0;
	mat vR_rates(N, (J*(J-1))/2, fill::zeros);

	const double alpha_star = 0.234;
	const double gam_exp = 2.0 / 3.0;
	mat SS(J*(J-1)/2, J*(J-1)/2, fill::eye);
	/*********
	Containers
	*********/
	mat theta_save(nt, nkeep, fill::zeros);
	cube Omega_save(nw*J, nw*J, nkeep, fill::zeros);
	cube Sig_save(N, (J*(J+1))/2, nkeep, fill::zeros);
	cube Rtk_save(N, J * (J - 1) / 2, nkeep, fill::zeros);
	cube resid_save(N, J, nkeep, fill::zeros);
	cube pRtk_save(N, J*(J-1)/2, nkeep, fill::zeros);
	cube delta_save(N, J, nkeep, fill::zeros);
	cube Rho_save(J, J, nkeep, fill::zeros);
	/*******************
	Begin burn-in period
	*******************/
	int icount_mh = 0;
	if (verbose) {
		Rcout << "Warming up" << endl;
	}
	{
		Progress prog(ndiscard, verbose);
		for (int idiscard = 0; idiscard < ndiscard; ++idiscard) {
			if (Progress::check_abort()) {
				return Rcpp::List::create(Rcpp::Named("error") = "user interrupt aborted");
			}
			++icount_mh;
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
					mat Siginv = vechinv(arma::trans(Siginv_lt.row(i_k)), J);
					mat X(J, xcols*J, fill::zeros);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						X(j, arma::span(j*xcols, (j+1)*xcols-1)) = x_i;
						W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
					}
					mat Xstar = arma::join_horiz(X,W);
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
					mat Siginv = vechinv(arma::trans(Siginv_lt.row(i_k)), J);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
					}
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
				mat om = arma::inv_sympd(ominv);
				Omegainv(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = ominv;
				Omega(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = om;
			}

			// Update Sigma
			// Update sig2
			for (int i = 0; i < N; ++i) {
				int k = Trial(i);
				double ntk = Npt(i);
				rowvec w_i = WCovariate.row(i);
				mat pRR = vecrinv(trans(arma::tanh(vRtk.row(i))), J);
				pRR.diag().fill(1.0);
				mat R = pRho_to_Rho(pRR);
				vec gam_k = gamR.col(k);
				mat V = arma::diagmat(SD.row(i));
				mat W(J, nw*J, fill::zeros);
				for (int j = 0; j < J; ++j) {
					W(j, span(j*nw, (j+1)*nw-1)) = w_i;
				}
				vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
				mat qq = ntk * resid_i * resid_i.t() + (ntk - 1.0) * V * R * V;
				rowvec delta_i = delta.row(i);

				for (int j = 0; j < J; ++j) {
					auto fx_delta = [&](double delta_input[])->double {
						return -loglik_delta_m3(delta_input[0], delta_i, j, Rhoinv, qq, a0, b0, ntk);
					};

					double dstar = std::log(delta_i(j));
					double start[] = { dstar };
					double xmin[] = { 0.0 };
					double ynewlo = 0.0;
					double reqmin = arma::datum::eps;
					int konvge = 5;
					int kcount = 1000;
					double step[] = { 0.2 };
					int icount = 0;
					int numres = 0;
					int ifault = 0;
					nelmin(fx_delta, 1, start, xmin, &ynewlo, reqmin, step, konvge, kcount, &icount, &numres, &ifault);					
					double xmax = xmin[0];
					double minll = ynewlo;

					mat cl(5,3, fill::zeros);
					vec dl(5, fill::zeros);
					double step_size = delta_stepsize;

					bool cont_flag = true;
					while (cont_flag) {
						for (int iii=0; iii < 5; ++iii) {
							double e1 = static_cast<double>(iii-2);
							cl(iii,0) = std::pow(xmax + e1 * step_size, 2.0);
							cl(iii,1) = xmax + e1 * step_size;
							cl(iii,2) = 1.0;
							dl(iii) = -loglik_delta_m3(xmax + e1 * step_size, delta_i, j, Rhoinv, qq, a0, b0, ntk);
						}
						if (any(dl < minll)) {
							step_size *= 1.2;
						} else {
							cont_flag = false;
						}
					}

					vec fl = arma::solve(cl.t() * cl, cl.t() * dl);
					double sigmaa = std::sqrt(0.5 / fl(0));

					// log-likelihood difference
					double dstar_prop = ::norm_rand() * sigmaa + xmax;
					double ll_diff = loglik_delta_m3(dstar_prop, delta_i, j, Rhoinv, qq, a0, b0, ntk) - loglik_delta_m3(dstar, delta_i, j, Rhoinv, qq, a0, b0, ntk)
							    - 0.5 * (std::pow(dstar - xmax, 2.0) - std::pow(dstar_prop - xmax, 2.0)) / std::pow(sigmaa, 2.0);
					if (std::log(::unif_rand()) < ll_diff) {
						delta_i(j) = std::exp(dstar_prop);
						delta(i, j) = delta_i(j);
						mat siginvm = arma::diagmat(1.0 / delta_i);
						mat Siginv_new = siginvm * Rhoinv * siginvm;
						Siginv_lt.row(i) = arma::trans(vech(Siginv_new));

						++delta_rates(i,j);
					}
				}
			}

			// Update Rho
			mat qq(J, J, fill::zeros);
			for (int i = 0; i < N; ++i) {
				int k = Trial(i);
				double ntk = Npt(i);
				rowvec w_i = WCovariate.row(i);
				mat pRR = vecrinv(trans(arma::tanh(vRtk.row(i))), J);
				pRR.diag().fill(1.0);
				mat R = pRho_to_Rho(pRR);
				vec gam_k = gamR.col(k);
				mat V = arma::diagmat(SD.row(i));
				mat W(J, nw*J, fill::zeros);
				for (int j = 0; j < J; ++j) {
					W(j, span(j*nw, (j+1)*nw-1)) = w_i;
				}
				vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
				mat dAd = ntk * resid_i * resid_i.t() + (ntk - 1.0) * V * R * V;
				mat siginvm = arma::diagmat(1.0 / delta.row(i));
				qq += siginvm * dAd * siginvm;
			}
			vec U(J*(J-1)/2, fill::randn);
			vec vRhop = vRho + SS * U;

			vec z = arma::tanh(vRhop);
			mat pRRho = vecrinv(z, J);
			pRRho.diag().fill(1.0);
			mat Rhop = pRho_to_Rho(pRRho);
			// mat Rhopinv;
			// bool rho_proceed = true;
			// try {
			// 	Rhopinv = arma::inv(Rhop);
			// } catch (std::runtime_error & e) {
			// 	rho_proceed = false;
			// }

			// if (rho_proceed) {
			if (Rhop.is_sympd()) {
				arma::mat Rhopinv = arma::inv(Rhop);
				// log-likelihood difference
				double ll_diff = loglik_vRho_m3(vRhop, Rhopinv, qq, JJm12, sumNpt) - loglik_vRho_m3(vRho, Rhoinv, qq, JJm12, sumNpt);

				if (std::log(::unif_rand()) < ll_diff) {
					vRho = vRhop;
					Rho = Rhop;
					Rhoinv = Rhopinv;
					++vRho_rates;
				} else {
					// delayed rejection
					U.randn();
					arma::vec zzz = vRho + std::sqrt(0.5) * (SS * U);

					arma::mat pRRhozzz = vecrinv(arma::tanh(zzz), J);
					pRRhozzz.diag().fill(1.0);
					arma::mat Rhopzzz = pRho_to_Rho(pRRhozzz);
					// arma::mat Rhopinvzzz;
					// try {
					// 	Rhopinvzzz = arma::inv(Rhopzzz);
					// } catch (std::runtime_error & e) {
					// 	continue;
					// }
					if (Rhopzzz.is_sympd()) {
						arma::mat Rhopinvzzz = arma::inv(Rhopzzz);
						vec ystar = zzz - (vRhop - vRho);
						mat pRRhoystar = vecrinv(arma::tanh(ystar), J);
						pRRhoystar.diag().fill(1.0);
						mat Rhopystar = pRho_to_Rho(pRRhoystar);
						// mat Rhopinvystar;
						// try {
						// 	Rhopinvystar = arma::inv(Rhopystar);
						// } catch (std::runtime_error & e) {
						// 	continue;
						// }
						if (Rhopystar.is_sympd()) {
							arma::mat Rhopinvystar = arma::inv(Rhopystar);
							double log1pxy = std::log1p(-std::min(1.0, std::exp(ll_diff)));
							double ll_diff_zystar = loglik_vRho_m3(zzz, Rhopinvzzz, qq, JJm12, sumNpt) - loglik_vRho_m3(ystar, Rhopinvystar, qq, JJm12, sumNpt);
							double log1pzystar = std::log1p(-std::min(1.0, std::exp(ll_diff_zystar)));
							double ll_diff_zx = loglik_vRho_m3(zzz, Rhopinvzzz, qq, JJm12, sumNpt) - loglik_vRho_m3(vRho, Rhoinv, qq, JJm12, sumNpt);
							ll_diff = ll_diff_zx + log1pzystar - log1pxy;
							if (std::log(::unif_rand()) < ll_diff) {
								vRho = zzz;
								Rho = Rhopzzz;
								Rhoinv = Rhopinvzzz;
								++vRho_rates;
							}
						}
					}
				}
				double alpha_n = std::min(1.0, std::exp(ll_diff));
				adapt_S(SS, U, alpha_n, alpha_star, icount_mh, gam_exp);
				// Update Sigmainvs
				for (int i = 0; i < N; ++i) {
					mat siginvm = arma::diagmat(1.0 / delta.row(i));
					mat Siginv_new = siginvm * Rhoinv * siginvm;
					mat Sig_new = arma::diagmat(delta.row(i)) * Rho * arma::diagmat(delta.row(i));
					Sig_lt.row(i) = arma::trans(vech(Sig_new));
					Siginv_lt.row(i) = arma::trans(vech(Siginv_new));
				}
			}
			

			// Update Rtk
			for (int i = 0; i < N; ++i) {
				rowvec y_i = Outcome.row(i);
				rowvec vrtk = vRtk.row(i);
				mat V = arma::diagmat(SD.row(i));
				double ntk = Npt(i);
				mat Siginv = vechinv(arma::trans(Siginv_lt.row(i)), J);
				mat VSV = V * Siginv * V;
				for (int kk = 0; kk < J*(J-1)/2; ++kk) {
					int iR = J - 2 - static_cast<int>(std::sqrt(-8.0*static_cast<double>(kk) + 4.0*static_cast<double>(J*(J-1))-7.0)/2.0 - 0.5); // row index
					int iC = kk + iR + 1 - (J*(J-1))/2 + ((J-iR)*((J-iR)-1))/2; // column index
					auto fx_rstar = [&](double rstar_input[])->double {
						return -loglik_rik(rstar_input[0], vrtk, kk, J, iR, iC, ntk, VSV);
					};

					double zstar = vrtk(kk);
					double start[] = { zstar };
					double xmin[] = { 0.0 };
					double ynewlo = 0.0;
					double reqmin = arma::datum::eps;
					int konvge = 5;
					int kcount = 1000;
					double step[] = { 0.02 };
					int icount = 0;
					int numres = 0;
					int ifault = 0;
					nelmin(fx_rstar, 1, start, xmin, &ynewlo, reqmin, step, konvge, kcount, &icount, &numres, &ifault);					
					double xmax = xmin[0];
					double minll = ynewlo;


					mat cl(5,3, fill::zeros);
					vec dl(5, fill::zeros);
					double step_size = R_stepsize;

					bool cont_flag = true;
					while (cont_flag) {
						for (int iii=0; iii < 5; ++iii) {
							double e1 = static_cast<double>(iii-2);
							cl(iii,0) = std::pow(xmax + e1 * step_size, 2.0);
							cl(iii,1) = xmax + e1 * step_size;
							cl(iii,2) = 1.0;
							dl(iii) = -loglik_rik(xmax + e1 * step_size, vrtk, kk, J, iR, iC, ntk, VSV);
						}
						if (any(dl < minll)) {
							step_size *= 1.2;
						} else {
							cont_flag = false;
						}
					}

					vec fl = arma::solve(cl.t() * cl, cl.t() * dl);
					double sigmaa = std::sqrt(0.5 / fl(0));

					double zstar_prop = ::norm_rand() * sigmaa + xmax;
					// log-likelihood difference
					double ll_diff = loglik_rik(zstar_prop, vrtk, kk, J, iR, iC, ntk, VSV) - loglik_rik(zstar, vrtk, kk, J, iR, iC, ntk, VSV)
										- 0.5 * (std::pow(zstar - xmax, 2.0) - std::pow(zstar_prop - xmax, 2.0)) / std::pow(sigmaa, 2.0);
							    
					if (std::log(::unif_rand()) < ll_diff) {
						vrtk(kk) = zstar_prop;
						vRtk(i,kk) = zstar_prop;
						++vR_rates(i,kk);
					}
				}
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
				++icount_mh;
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
						mat Siginv = vechinv(arma::trans(Siginv_lt.row(i_k)), J);
						mat X(J, xcols*J, fill::zeros);
						mat W(J, nw*J, fill::zeros);
						for (int j = 0; j < J; ++j) {
							X(j, arma::span(j*xcols, (j+1)*xcols-1)) = x_i;
							W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
						}
						mat Xstar = arma::join_horiz(X,W);
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
						mat Siginv = vechinv(arma::trans(Siginv_lt.row(i_k)), J);
						mat W(J, nw*J, fill::zeros);
						for (int j = 0; j < J; ++j) {
							W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
						}
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
				// Update sig2
				for (int i = 0; i < N; ++i) {
					int k = Trial(i);
					double ntk = Npt(i);
					rowvec w_i = WCovariate.row(i);
					mat pRR = vecrinv(trans(arma::tanh(vRtk.row(i))), J);
					pRR.diag().fill(1.0);
					mat R = pRho_to_Rho(pRR);
					vec gam_k = gamR.col(k);
					mat V = arma::diagmat(SD.row(i));
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						W(j, span(j*nw, (j+1)*nw-1)) = w_i;
					}
					vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
					resid_ikeep.row(i) = resid_i.t();
					mat qq = ntk * resid_i * resid_i.t() + (ntk - 1.0) * V * R * V;
					rowvec delta_i = delta.row(i);

					for (int j = 0; j < J; ++j) {
						auto fx_delta = [&](double delta_input[])->double {
							return -loglik_delta_m3(delta_input[0], delta_i, j, Rhoinv, qq, a0, b0, ntk);
						};

						double dstar = std::log(delta_i(j));
						double start[] = { dstar };
						double xmin[] = { 0.0 };
						double ynewlo = 0.0;
						double reqmin = arma::datum::eps;
						int konvge = 5;
						int kcount = 1000;
						double step[] = { 0.2 };
						int icount = 0;
						int numres = 0;
						int ifault = 0;
						nelmin(fx_delta, 1, start, xmin, &ynewlo, reqmin, step, konvge, kcount, &icount, &numres, &ifault);					
						double xmax = xmin[0];
						double minll = ynewlo;

						mat cl(5,3, fill::zeros);
						vec dl(5, fill::zeros);
						double step_size = delta_stepsize;

						bool cont_flag = true;
						while (cont_flag) {
							for (int iii=0; iii < 5; ++iii) {
								double e1 = static_cast<double>(iii-2);
								cl(iii,0) = std::pow(xmax + e1 * step_size, 2.0);
								cl(iii,1) = xmax + e1 * step_size;
								cl(iii,2) = 1.0;
								dl(iii) = -loglik_delta_m3(xmax + e1 * step_size, delta_i, j, Rhoinv, qq, a0, b0, ntk);
							}
							if (any(dl < minll)) {
								step_size *= 1.2;
							} else {
								cont_flag = false;
							}
						}

						vec fl = arma::solve(cl.t() * cl, cl.t() * dl);
						double sigmaa = std::sqrt(0.5 / fl(0));

						// log-likelihood difference
						double dstar_prop = ::norm_rand() * sigmaa + xmax;
						double ll_diff = loglik_delta_m3(dstar_prop, delta_i, j, Rhoinv, qq, a0, b0, ntk) - loglik_delta_m3(dstar, delta_i, j, Rhoinv, qq, a0, b0, ntk)
								    - 0.5 * (std::pow(dstar - xmax, 2.0) - std::pow(dstar_prop - xmax, 2.0)) / std::pow(sigmaa, 2.0);
						if (std::log(::unif_rand()) < ll_diff) {
							delta_i(j) = std::exp(dstar_prop);
							delta(i, j) = delta_i(j);
							mat siginvm = arma::diagmat(1.0 / delta_i);
							mat Siginv_new = siginvm * Rhoinv * siginvm;
							mat Sig_new = arma::diagmat(delta_i) * Rho * arma::diagmat(delta_i);
							Sig_lt.row(i) = arma::trans(vech(Sig_new));
							Siginv_lt.row(i) = arma::trans(vech(Siginv_new));

							++delta_rates(i,j);
						}
					}
				}

				// Update Rho
				mat qq(J, J, fill::zeros);
				for (int i = 0; i < N; ++i) {
					int k = Trial(i);
					double ntk = Npt(i);
					rowvec w_i = WCovariate.row(i);
					mat pRR = vecrinv(arma::trans(arma::tanh(vRtk.row(i))), J);
					pRR.diag().fill(1.0);
					mat R = pRho_to_Rho(pRR);

					vec gam_k = gamR.col(k);
					mat V = arma::diagmat(SD.row(i));
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						W(j, span(j*nw, (j+1)*nw-1)) = w_i;
					}
					vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
					mat dAd = ntk * resid_i * resid_i.t() + (ntk - 1.0) * V * R * V;
					mat siginvm = arma::diagmat(1.0 / delta.row(i));
					qq += siginvm * dAd * siginvm;
				}
				vec U(J*(J-1)/2, fill::randn);
				vec vRhop = vRho + SS * U;

				vec z = arma::tanh(vRhop);
				mat pRRho = vecrinv(z, J);
				pRRho.diag().fill(1.0);
				mat Rhop = pRho_to_Rho(pRRho);
				// mat Rhopinv;
				// bool rho_proceed = true;
				// try {
				// 	Rhopinv = arma::inv(Rhop);
				// } catch (std::runtime_error & e) {
				// 	rho_proceed = false;
				// }
				// if (rho_proceed) {
				if (Rhop.is_sympd()) {
					arma::mat Rhopinv = arma::inv(Rhop);
					// log-likelihood difference
					double ll_diff = loglik_vRho_m3(vRhop, Rhopinv, qq, JJm12, sumNpt) - loglik_vRho_m3(vRho, Rhoinv, qq, JJm12, sumNpt);

					if (std::log(::unif_rand()) < ll_diff) {
						vRho = vRhop;
						Rho = Rhop;
						Rhoinv = Rhopinv;
						++vRho_rates;
					} else {
						// delayed rejection
						U.randn();
						vec zzz = vRho + std::sqrt(0.5) * (SS * U);

						mat pRRhozzz = vecrinv(arma::tanh(zzz), J);
						pRRhozzz.diag().fill(1.0);
						mat Rhopzzz = pRho_to_Rho(pRRhozzz);
						// mat Rhopinvzzz;
						// try {
						// 	Rhopinvzzz = arma::inv(Rhopzzz);
						// } catch (std::runtime_error & e) {
						// 	continue;
						// }
						if (Rhopzzz.is_sympd()) {
							arma::mat Rhopinvzzz = arma::inv(Rhopzzz);
							vec ystar = zzz - (vRhop - vRho);
							mat pRRhoystar = vecrinv(arma::tanh(ystar), J);
							pRRhoystar.diag().fill(1.0);
							mat Rhopystar = pRho_to_Rho(pRRhoystar);
							// mat Rhopinvystar;
							// try {
							// 	Rhopinvystar = arma::inv(Rhopystar);
							// } catch (std::runtime_error & e) {
							// 	continue;
							// }
							if (Rhopystar.is_sympd()) {
								arma::mat Rhopinvystar = arma::inv(Rhopystar);
								double log1pxy = std::log1p(-std::min(1.0, std::exp(ll_diff)));
								double ll_diff_zystar = loglik_vRho_m3(zzz, Rhopinvzzz, qq, JJm12, sumNpt) - loglik_vRho_m3(ystar, Rhopinvystar, qq, JJm12, sumNpt);
								double log1pzystar = std::log1p(-std::min(1.0, std::exp(ll_diff_zystar)));
								double ll_diff_zx = loglik_vRho_m3(zzz, Rhopinvzzz, qq, JJm12, sumNpt) - loglik_vRho_m3(vRho, Rhoinv, qq, JJm12, sumNpt);
								ll_diff = ll_diff_zx + log1pzystar - log1pxy;
								if (std::log(::unif_rand()) < ll_diff) {
									vRho = zzz;
									Rho = Rhopzzz;
									Rhoinv = Rhopinvzzz;
									++vRho_rates;
								}
							}
						}
					}
					double alpha_n = std::min(1.0, std::exp(ll_diff));
					adapt_S(SS, U, alpha_n, alpha_star, icount_mh, gam_exp);
					// Update Sigmainvs
					for (int i = 0; i < N; ++i) {
						mat siginvm = arma::diagmat(1.0 / delta.row(i));
						mat Siginv_new = siginvm * Rhoinv * siginvm;
						mat Sig_new = arma::diagmat(delta.row(i)) * Rho * arma::diagmat(delta.row(i));
						Sig_lt.row(i) = arma::trans(vech(Sig_new));
						Siginv_lt.row(i) = arma::trans(vech(Siginv_new));
					}
				}

				// Update Rtk
				for (int i = 0; i < N; ++i) {
					rowvec y_i = Outcome.row(i);
					rowvec vrtk = vRtk.row(i);
					mat V = arma::diagmat(SD.row(i));
					double ntk = Npt(i);
					mat Siginv = vechinv(arma::trans(Siginv_lt.row(i)), J);
					mat VSV = V * Siginv * V;
					for (int kk = 0; kk < J*(J-1)/2; ++kk) {
						int iR = J - 2 - static_cast<int>(std::sqrt(-8.0*static_cast<double>(kk) + 4.0*static_cast<double>(J*(J-1))-7.0)/2.0 - 0.5); // row index
						int iC = kk + iR + 1 - (J*(J-1))/2 + ((J-iR)*((J-iR)-1))/2; // column index
						auto fx_rstar = [&](double rstar_input[])->double {
							return -loglik_rik(rstar_input[0], vrtk, kk, J, iR, iC, ntk, VSV);
						};

						double zstar = vrtk(kk);
						double start[] = { zstar };
						double xmin[] = { 0.0 };
						double ynewlo = 0.0;
						double reqmin = arma::datum::eps;
						int konvge = 5;
						int kcount = 1000;
						double step[] = { 0.02 };
						int icount = 0;
						int numres = 0;
						int ifault = 0;
						nelmin(fx_rstar, 1, start, xmin, &ynewlo, reqmin, step, konvge, kcount, &icount, &numres, &ifault);					
						double xmax = xmin[0];
						double minll = ynewlo;


						mat cl(5,3, fill::zeros);
						vec dl(5, fill::zeros);
						double step_size = R_stepsize;

						bool cont_flag = true;
						while (cont_flag) {
							for (int iii=0; iii < 5; ++iii) {
								double e1 = static_cast<double>(iii-2);
								cl(iii,0) = std::pow(xmax + e1 * step_size, 2.0);
								cl(iii,1) = xmax + e1 * step_size;
								cl(iii,2) = 1.0;
								dl(iii) = -loglik_rik(xmax + e1 * step_size, vrtk, kk, J, iR, iC, ntk, VSV);
							}
							if (any(dl < minll)) {
								step_size *= 1.2;
							} else {
								cont_flag = false;
							}
						}

						vec fl = arma::solve(cl.t() * cl, cl.t() * dl);
						double sigmaa = std::sqrt(0.5 / fl(0));

						double zstar_prop = ::norm_rand() * sigmaa + xmax;
						// log-likelihood difference
						double ll_diff = loglik_rik(zstar_prop, vrtk, kk, J, iR, iC, ntk, VSV) - loglik_rik(zstar, vrtk, kk, J, iR, iC, ntk, VSV)
											- 0.5 * (std::pow(zstar - xmax, 2.0) - std::pow(zstar_prop - xmax, 2.0)) / std::pow(sigmaa, 2.0);
								    
						if (std::log(::unif_rand()) < ll_diff) {
							vrtk(kk) = zstar_prop;
							vRtk(i,kk) = zstar_prop;
							++vR_rates(i,kk);
						}
					}
				}
			}
			theta_save.col(ikeep) = theta;
			Omega_save.slice(ikeep) = Omega;
			resid_save.slice(ikeep) = resid_ikeep;
			delta_save.slice(ikeep) = delta;
			Rho_save.slice(ikeep) = Rho;
			Sig_save.slice(ikeep) = Sig_lt;

			mat Rtk(arma::size(vRtk), fill::zeros);
			for (int i = 0; i < N; ++i) {
				mat RR = vecrinv(trans(arma::tanh(vRtk.row(i))), J);
				RR.diag().fill(1.0);
				mat R = pRho_to_Rho(RR);
				Rtk.row(i) = arma::trans(vecr(R));
			}
			Rtk_save.slice(ikeep) = Rtk;
			pRtk_save.slice(ikeep) = arma::tanh(vRtk);
			prog.increment();
		}
	}



	return Rcpp::List::create(
			Rcpp::Named("resid") = resid_save,
			Rcpp::Named("theta") = theta_save,
			Rcpp::Named("Omega") = Omega_save,
			Rcpp::Named("Sigma") = Sig_save,
			Rcpp::Named("delta") = delta_save,
			Rcpp::Named("Rho") = Rho_save,
			Rcpp::Named("R") = Rtk_save,
		    Rcpp::Named("pR") = pRtk_save,
			Rcpp::Named("delta_acceptance") = delta_rates / static_cast<double>(ndiscard + nskip*nkeep),
			Rcpp::Named("vRho_acceptance") = vRho_rates / static_cast<double>(ndiscard + nskip*nkeep),
		    Rcpp::Named("vR_acceptance") = vR_rates / static_cast<double>(ndiscard + nskip*nkeep)
		);
}

