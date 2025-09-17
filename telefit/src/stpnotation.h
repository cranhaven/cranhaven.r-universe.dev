#include <RcppArmadillo.h>
#include "mcstat.h"

#ifndef _telefit_stpnotation_h
#define _telefit_stpnotation_h

using namespace Rcpp;
using namespace arma;

struct Priors {
	mcstat::mvnorm beta;
	mcstat::invgamma sigmasq_y, sigmasq_r, sigmasq_eps, sigmasq_r_eps;
	mcstat::uniform rho_y, rho_r;
	
	Priors() { }
	
	Priors(const vec &beta_mu, const mat &beta_Sigma, double sigmasq_y_shape,
		   double sigmasq_y_rate, double sigmasq_r_shape, double sigmasq_r_rate,
		   double sigmasq_eps_shape, double sigmasq_eps_rate, double rho_y_a,
		   double rho_y_b, double rho_r_a, double rho_r_b,
		   double sigmasq_r_eps_shape, double sigmasq_r_eps_rate) {
		
		beta = mcstat::mvnorm(beta_mu, beta_Sigma);
		sigmasq_y = mcstat::invgamma(sigmasq_y_shape, sigmasq_y_rate);
		sigmasq_r = mcstat::invgamma(sigmasq_r_shape, sigmasq_r_rate);
		sigmasq_r_eps = mcstat::invgamma(sigmasq_r_eps_shape, sigmasq_r_eps_rate);
		sigmasq_eps = mcstat::invgamma(sigmasq_eps_shape, sigmasq_eps_rate);
		rho_y = mcstat::uniform(rho_y_a, rho_y_b);
		rho_r = mcstat::uniform(rho_r_a, rho_r_b);
	}
};

struct Data {
	mat X, Z, W;
	vec Y;
	
	Data() { }
	
	// intended to be used for forecasting
	Data(const mat &t_X, const mat &t_Z) {
		X = t_X;
		Z = t_Z;
	}
	
	// intended to be used for fitting
	Data(const mat &t_X, const mat &t_Z, const vec &t_Y) {
		X = t_X;
		Z = t_Z;
		Y = t_Y;
	}
	
	// intended to be used for composition sampling
	Data(const mat &t_X, const mat &t_Z, const vec &t_Y, const mat &t_W) {
		X = t_X;
		Z = t_Z;
		Y = t_Y;
		W = t_W;
	}
};

struct Constants {
	mat Dy, Dz_knots, Dz_to_knots;
	int p, ns, nr, nr_knots, nt;
	double smoothness_y, smoothness_r;
	bool localOnly;
	
	Constants() { }
	
	Constants(const mat &t_Dy, const mat &t_Dz_knots, const mat &t_Dz_to_knots,
			  int t_p, int t_ns, int t_nr, int t_nr_knots, int t_nt,
			  double t_smoothness_y, double t_smoothness_r, bool t_localOnly) {
		Dy = t_Dy;
		Dz_knots = t_Dz_knots;
		Dz_to_knots = t_Dz_to_knots;
		p = t_p;
		ns = t_ns;
		nr = t_nr;
		nr_knots = t_nr_knots;
		nt = t_nt;
		smoothness_y = t_smoothness_y;
		smoothness_r = t_smoothness_r;
		localOnly = t_localOnly;
	}
};



struct CompositionSamples {
	
	bool return_full_alpha, return_forecast, localOnly;
	
	mat alpha_knots;
	running_stat_vec<vec> alpha, eof_alpha_knots, eof_alpha_knots_negprob,
		eof_alpha_knots_posprob;
	cube forecast, local, remote, cat_probs;
	
	CompositionSamples(int nSamples, const Constants &consts,
					   bool t_return_full_alpha, int nt0=-1, int nbreaks=1) {
		
		localOnly = consts.localOnly;
		
		return_forecast = nt0 > 0 ? true : false;
		return_full_alpha = t_return_full_alpha;
		
		if(!localOnly) {
			alpha_knots = mat(nSamples, consts.ns * consts.nr_knots, fill::zeros);
		}
		
		if(return_forecast) {
			forecast = cube(consts.ns, nt0, nSamples, fill::zeros);
			if(!localOnly) {
				local = cube(consts.ns, nt0, nSamples, fill::zeros);
				remote = cube(consts.ns, nt0, nSamples, fill::zeros);
			}
			cat_probs = cube(consts.ns, nbreaks+1, nSamples, fill::zeros);
		}
	}
	
	List toSummarizedList();
	
};


struct Samples {
	
	mat beta;
	vec sigmasq_y, sigmasq_r, sigmasq_eps, rho_y, rho_r, ll, sigmasq_r_eps;
	
	Samples(Constants &consts, int nSamples) {
		beta = mat(nSamples, consts.p, fill::zeros);
		sigmasq_y = vec(nSamples, fill::zeros);
		sigmasq_eps = vec(nSamples, fill::zeros);
		rho_y = vec(nSamples, fill::zeros);
		ll = vec(nSamples, fill::zeros);
		if(!consts.localOnly) {
			sigmasq_r = vec(nSamples, fill::zeros);
			rho_r = vec(nSamples, fill::zeros);
			sigmasq_r_eps = vec(nSamples, fill::zeros);
		}
	}
	
	Samples(const mat &t_beta, const vec &t_sigmasq_y, const vec &t_sigmasq_r,
			const vec &t_sigmasq_eps, const vec &t_rho_y, const vec &t_rho_r,
			const vec &t_ll, const vec &t_sigmasq_r_eps) {
		beta = t_beta;
		sigmasq_y = t_sigmasq_y;
		sigmasq_r = t_sigmasq_r;
		sigmasq_eps = t_sigmasq_eps;
		rho_y = t_rho_y;
		rho_r = t_rho_r;
		ll = t_ll;
		sigmasq_r_eps = t_sigmasq_r_eps;
	}
	
	List toList() {
		return List::create(
			_["beta"] = beta,
			_["sigmasq_y"] = sigmasq_y,
			_["sigmasq_r"] = sigmasq_r,
			_["sigmasq_r_eps"] = sigmasq_r_eps,
			_["sigmasq_eps"] = sigmasq_eps,
			_["rho_y"] = rho_y,
			_["rho_r"] = rho_r,
			_["ll"] = ll
		);
	}
	
};


#endif
