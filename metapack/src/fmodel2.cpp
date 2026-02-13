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
Rcpp::List fmodel2(const arma::mat& Outcome,
				   const arma::mat& SD,
				   const arma::mat& XCovariate,
				   const arma::mat& WCovariate,
				   const arma::uvec& Treat,
				   const arma::uvec& Trial,
				   const arma::vec& Npt,
				   const double& c0,
				   const double& dj0, // hyperparameter for Omega
				   const double& s0, // hyperparameter for Sigma
				   const arma::mat& Omega0,
				   const arma::mat& Sigma0,
				   const int& K, // # of Trials
				   const int& T, // # of Treatments
				   const int& ndiscard,
				   const int& nskip,
				   const int& nkeep,
				   const double& R_stepsize,
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
	mat Sig(J, J, fill::eye);
	mat Siginv(J, J, fill::eye);
	mat vRtk(N, J * (J - 1) / 2, fill::zeros);
	vRtk.fill(0.5);

	const mat Omega0inv = arma::inv_sympd(Omega0);
	const mat Sigma0inv = arma::inv_sympd(Sigma0);
	const double sumNpt = arma::accu(Npt);
	const double df = s0 + sumNpt;
	const double shape_omega = static_cast<double>(K) + dj0;
	mat resid = Outcome;
	mat vR_rates(N, (J*(J-1))/2, fill::zeros);

	/*********
	Containers
	*********/
	mat theta_save(nt, nkeep, fill::zeros);
	cube Omega_save(nw*J, nw*J, nkeep, fill::zeros);
	cube Sigma_save(J, J, nkeep, fill::zeros);
	cube Rtk_save(N, J * (J - 1) / 2, nkeep, fill::zeros);
	cube resid_save(N, J, nkeep, fill::zeros);
	cube pRtk_save(N, J*(J-1)/2, nkeep, fill::zeros);
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
					XSX += ntk * (Xstar.t() * Siginv * Xstar);
					XSy += ntk * (Xstar.t() * Siginv * y_i.t());
					mat WS = ntk * (W.t() * Siginv);
					Sig_gamk += WS * W;
					WSX += WS * Xstar;
					WSy += WS * y_i.t();
				}
				mat Sig_gamk_inv = arma::inv_sympd(Sig_gamk);
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
				mat ominv = RNG::rwish(arma::inv_sympd(qq), shape_omega);
				mat om = arma::inv_sympd(ominv);
				Omegainv(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = ominv;
				Omega(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = om;
			}

			// Update Sigma
			mat qq = Sigma0inv;
			for (int i = 0; i < N; ++i) {
				int k = Trial(i);
				double ntk = Npt(i);
				vec gam_k = gamR.col(k);
				mat pRR = vecrinv(arma::trans(arma::tanh(vRtk.row(i))), J);
				pRR.diag().fill(1.0);
				mat R = pRho_to_Rho(pRR);
				mat V = arma::diagmat(SD.row(i));
				rowvec w_i = WCovariate.row(i);
				mat W(J, nw*J, fill::zeros);
				for (int j = 0; j < J; ++j) {
					W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
				}
				vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
				qq += ntk * resid_i * resid_i.t() + (ntk - 1.0) * V * R * V;
			}
			Siginv = RNG::rwish(arma::inv_sympd(qq), df);

			for (int i = 0; i < N; ++i) {
				rowvec y_i = Outcome.row(i);
				rowvec vrtk = vRtk.row(i);
				mat V = arma::diagmat(SD.row(i));
				double ntk = Npt(i);
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
						XSX += ntk * (Xstar.t() * Siginv * Xstar);
						XSy += ntk * (Xstar.t() * Siginv * y_i.t());
						mat WS = ntk * (W.t() * Siginv);
						Sig_gamk += WS * W;
						WSX += WS * Xstar;
						WSy += WS * y_i.t();
					}
					mat Sig_gamk_inv = arma::inv_sympd(Sig_gamk);
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
					mat ominv = RNG::rwish(arma::inv_sympd(qq), shape_omega);
					mat om = arma::inv_sympd(ominv);
					Omegainv(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = ominv;
					Omega(arma::span(nw*jj, nw*(jj+1)-1), arma::span(nw*jj, nw*(jj+1)-1)) = om;
				}

				// Update Sigma
				mat qq = Sigma0inv;
				for (int i = 0; i < N; ++i) {
					int k = Trial(i);
					double ntk = Npt(i);
					vec gam_k = gamR.col(k);
					mat pRR = vecrinv(arma::trans(arma::tanh(vRtk.row(i))), J);
					pRR.diag().fill(1.0);
					mat R = pRho_to_Rho(pRR);
					mat V = arma::diagmat(SD.row(i));
					rowvec w_i = WCovariate.row(i);
					mat W(J, nw*J, fill::zeros);
					for (int j = 0; j < J; ++j) {
						W(j, arma::span(j*nw, (j+1)*nw-1)) = w_i;
					}
					vec resid_i = arma::trans(resid.row(i)) - W * gam_k;
					resid_ikeep.row(i) = resid_i.t();
					qq += ntk * resid_i * resid_i.t() + (ntk - 1.0) * V * R * V;
				}
				Siginv = RNG::rwish(arma::inv_sympd(qq), df);
				Sig = arma::inv_sympd(Siginv);

				for (int i = 0; i < N; ++i) {
					rowvec y_i = Outcome.row(i);
					rowvec vrtk = vRtk.row(i);
					mat V = arma::diagmat(SD.row(i));
					double ntk = Npt(i);
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
			Sigma_save.slice(ikeep) = Sig;
			Omega_save.slice(ikeep) = Omega;
			resid_save.slice(ikeep) = resid_ikeep;
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
			Rcpp::Named("Sigma") = Sigma_save,
			Rcpp::Named("Omega") = Omega_save,
			Rcpp::Named("R") = Rtk_save,
			Rcpp::Named("pR") = pRtk_save,
			Rcpp::Named("pR_acceptance") = vR_rates / static_cast<double>(ndiscard+nkeep)
		);
}



