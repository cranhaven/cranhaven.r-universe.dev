/*
	gibbs sampler to estimate parameters of a spatially varying coefficient model.
 */

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include "GibbsSampler.h"
#include "distributions.h"
#include "numAlg.h"
#include "covs.h"
#include "RWSampler.h"

using namespace Rcpp;
using namespace arma;

namespace {

struct Data {
	vec y; // each row has response for one location + timepoint (Nntx1)
	mat X; // each row has local covariates for one location + timepoint (Nntxp)
	mat Z; // each column has remote covariates for one timepoint (kxnt)
	mat* d; // matrix containing interpoint distances (NxN)
};

struct Params {
	vec beta;		   // fixed coefficients
	vec theta;		   // spatially varying coefficients
	mat T; 		       // local covariance for spatially varying coefficients
	double sigmasq,	   // scale for spatial covariance function
		   sigmasqeps, // scale for spatial nugget function (via param. ex.)
		   rho;		   // range for spatial covariance function
};

struct Consts {
	int N;     // number of spatial locations
	int nt;	   // number of timepoints
	int k;	   // spatially varying coefficient dimension
	double nu; // spatial range parameter (fixed)
};

struct Priors {
	double nu;   // T ~ Inv-Wishart(Psi, nu)
	mat Psi;	 //
	mat Linv;	 // beta ~ N(0, L^-1)
	double a, b; // sigmasq, sigmasqeps ~ Inv-Gamma(a, b)
	double L, U; // rho ~ Uniform(L, U)
};

struct Scratch {
	mat H, Hinv;	   // spatial covariance matrix and inverse
	vec zTheta, xBeta; // components of mean for response
	double logdetH;    // log-determinant of H
};

struct Config {
	Data dat; Params params; Consts consts; Priors priors; Scratch scratch;
};


class TSampler : public mcstat2::Sampler {

private:

	Config *cfg;

	double df;
	int dim;
	mat thMat, postSc, T;

public:

	TSampler(Config &t_cfg) { name = "T"; type = VECTOR; cfg = &t_cfg;
		df = cfg->priors.nu + cfg->consts.N;
		dim = cfg->consts.k * cfg->consts.k;
	}

	vec sample() {

		// compute posterior parameters
		thMat = reshape( mat(cfg->params.theta), cfg->consts.k, cfg->consts.N );
		postSc = thMat * cfg->scratch.Hinv * thMat.t() + cfg->priors.Psi;

		// sample from posterior
		T = mcstat2::rinvwishart(postSc, df);

		// update dependencies
		cfg->params.T = T;

		// save MCMC output
		return vectorise(T);
	}
};


class BetaSampler : public mcstat2::Sampler {

private:

	Config *cfg;

	mat Q;
	vec partialPostMu, beta;

public:

	BetaSampler(Config &t_cfg) { name = "beta"; type = VECTOR; cfg = &t_cfg; }

	vec sample() {

		// compute posterior parameters

		Q = cfg->priors.Linv + cfg->dat.X.t() *
			mcstat2::dgeikmm(cfg->consts.nt, cfg->scratch.Hinv, cfg->dat.X);

		partialPostMu = cfg->dat.X.t() *
						mcstat2::dgeikmm(cfg->consts.nt,
										 cfg->scratch.Hinv,
										 cfg->dat.y - cfg->scratch.zTheta);

		// sample from posterior
		beta = mcstat2::mvrnorm_post(partialPostMu, Q, 1, true);

		// update dependencies
		cfg->scratch.xBeta = cfg->dat.X * beta;
		cfg->params.beta = beta;

		// save MCMC output
		return beta;
	}
};


class ThetaSampler : public mcstat2::Sampler {

private:

	Config *cfg;

	mat B, zzt;
	vec partialPostMu, theta, resid;

public:

	ThetaSampler(Config &t_cfg) { name = "theta"; type = VECTOR; cfg = &t_cfg;

		zzt = mat(cfg->consts.k, cfg->consts.k, fill::zeros);
		for(int i=0; i<cfg->consts.nt; i++)
			zzt += cfg->dat.Z.col(i) * cfg->dat.Z.col(i).t();
	}

	vec sample() {

		// compute posterior parameters

		B = inv_sympd(cfg->params.T) + zzt;

		resid = cfg->dat.y - cfg->scratch.xBeta;

		partialPostMu = mcstat2::dgemkmm(cfg->scratch.Hinv,
										 cfg->dat.Z.col(0),
										 resid.rows(0, cfg->consts.N - 1));

		for(int i=1; i<cfg->consts.nt; i++) {
			partialPostMu += mcstat2::dgemkmm(cfg->scratch.Hinv,
								cfg->dat.Z.col(i),
								resid.rows(i * cfg->consts.N,
										   (i+1) * cfg->consts.N - 1));
		}

		// sample from posterior
		theta = mcstat2::mvrnorm_postKron(partialPostMu, cfg->scratch.Hinv, B,
										  1, true);

		// update dependencies

		cfg->params.theta = theta;

		for(int i=0; i<cfg->consts.nt; i++)
			cfg->scratch.zTheta.rows(i * cfg->consts.N,
								 (i+1) * cfg->consts.N - 1) =
				mcstat2::dgeikmm(cfg->consts.N, cfg->dat.Z.col(i).t(), theta);

		// save MCMC output
		return theta;
	}
};


class SigmasqSampler : public mcstat2::Sampler {

private:

	Config *cfg;

	double shp, scale, sigmasq;
	vec resid;


public:

	SigmasqSampler(Config &t_cfg) { name = "sigmasq"; type = REAL; cfg = &t_cfg;
		shp = cfg->priors.a + ( cfg->consts.N * cfg->consts.nt )/ 2.0;
	}

	vec sample() {

		// compute posterior parameters

		resid = cfg->dat.y - cfg->scratch.xBeta - cfg->scratch.zTheta;
		scale = cfg->priors.b + as_scalar(resid.t() *
							mcstat2::dgeikmm(cfg->consts.nt,
											 cfg->scratch.Hinv * cfg->params.sigmasq,
											 resid)) / 2;

		// sample from posterior
		sigmasq = 1.0 / R::rgamma( shp, 1.0 / scale );


		// update dependencies

		double r = sigmasq / cfg->params.sigmasq;
		cfg->scratch.Hinv /= r;
		cfg->scratch.H *= r;
		cfg->scratch.logdetH += cfg->consts.N *
								(log(sigmasq) - log(cfg->params.sigmasq));

		cfg->params.sigmasq = sigmasq;

		// save MCMC output
		vec res = vec(1);
		res.at(0) = sigmasq;
		return res;
	}
};


class RhoSampler : public mcstat2::RWSampler {

private:

	Config *cfg;

	vec resid;
	double logdetH, one, rhoNew;
	mat H, Hinv;

public:

	RhoSampler(Config &t_cfg, double t_sd, double t_cur, double C, double alpha) :
	RWSampler(t_sd, t_cur, C, alpha) { name = "rho"; type = REAL; cfg = &t_cfg;
		propType = LOGIT;
		L = cfg->priors.L;
		U = cfg->priors.U;
		one = 1.0;
		H = mat(cfg->consts.N, cfg->consts.N, fill::zeros);
	}

	// rho is proposed, rho0 is current
	double logR_posterior(double rho, double rho0) {

		rhoNew = rho;

		maternCov( H, *(cfg->dat.d), cfg->params.sigmasq, rhoNew, cfg->consts.nu,
				  cfg->params.sigmasq * cfg->params.sigmasqeps );
		log_det(logdetH, one, H);
		Hinv = inv_sympd(H);

		resid = cfg->dat.y - cfg->scratch.xBeta - cfg->scratch.zTheta;

		return - 0.5 * ( cfg->consts.nt * (logdetH - cfg->scratch.logdetH) +
						as_scalar(resid.t() *
								  mcstat2::dgeikmm(cfg->consts.nt,
												   Hinv - cfg->scratch.Hinv,
												   resid))
						);
	}

	void update() {
		cfg->scratch.H = H;
		cfg->scratch.Hinv = Hinv;
		cfg->scratch.logdetH = logdetH;
		cfg->params.rho = rhoNew;
	}
};


class SigmasqepsSampler : public mcstat2::RWSampler {

private:

	Config *cfg;

	double logdetH, one, sigmasqepsNew;
	mat H, Hinv;
	vec resid;

public:

	SigmasqepsSampler(Config &t_cfg, double t_sd, double t_current, double C,
					  double alpha) :
	RWSampler(t_sd, t_current, C, alpha) { name = "sigmasqeps"; type = REAL;
		cfg = &t_cfg;
		propType = LOG;

		one = 1.0;
		H = mat(cfg->consts.N, cfg->consts.N, fill::zeros);
	}

	// sseps is proposed, sseps0 is current
	double logR_posterior(double sseps, double sseps0) {

		sigmasqepsNew = sseps;

		maternCov( H, *(cfg->dat.d), cfg->params.sigmasq, cfg->params.rho,
				   cfg->consts.nu, cfg->params.sigmasq * sseps );
		log_det(logdetH, one, H);
		Hinv = inv_sympd(H);

		resid = cfg->dat.y - cfg->scratch.xBeta - cfg->scratch.zTheta;

		return - 0.5 * ( cfg->consts.nt * (logdetH - cfg->scratch.logdetH) +
						 as_scalar(resid.t() *
								   mcstat2::dgeikmm(cfg->consts.nt,
													Hinv - cfg->scratch.Hinv,
													resid))
						) +
		 mcstat2::logdinvgamma_unscaled(sseps, cfg->priors.a, cfg->priors.b) -
		 mcstat2::logdinvgamma_unscaled(sseps0, cfg->priors.a, cfg->priors.b);
	}

	void update() {
		cfg->scratch.H = H;
		cfg->scratch.Hinv = Hinv;
		cfg->scratch.logdetH = logdetH;
		cfg->params.sigmasqeps = sigmasqepsNew;
	}
};


RcppExport SEXP r_svcfit (SEXP Y, SEXP X, SEXP Z, SEXP D, SEXP nuT, SEXP Psi,
						 SEXP Linv, SEXP a, SEXP b, SEXP L, SEXP U, SEXP nu,
						 SEXP nSamples, SEXP thin, SEXP t_sd, SEXP t_inits,
						 SEXP t_C, SEXP t_alpha) {

	using namespace Rcpp;

	// extract model configuration

	Config cfg = Config();

	cfg.dat.y = as<vec>(Y);
	cfg.dat.X = as<mat>(X);
	cfg.dat.Z = as<mat>(Z);
	mat d = as<mat>(D);
	cfg.dat.d = &d;

	cfg.consts.N = cfg.dat.d->n_rows;
	cfg.consts.nt = cfg.dat.Z.n_cols;
	cfg.consts.k = cfg.dat.Z.n_rows;
	cfg.consts.nu = as<double>(nu);

	cfg.priors.nu = as<double>(nuT);
	cfg.priors.Psi = as<mat>(Psi);
	cfg.priors.Linv = as<mat>(Linv);
	cfg.priors.a = as<double>(a);
	cfg.priors.b = as<double>(b);
	cfg.priors.L = as<double>(L);
	cfg.priors.U = as<double>(U);

	// initialize parameters and scratch

	List inits = as<List>(t_inits);

	if(inits.containsElementNamed("sigmasq")) {
		cfg.params.sigmasq = as<double>(inits["sigmasq"]);
	} else {
		cfg.params.sigmasq = 1 / R::rgamma( cfg.priors.a, 1 / cfg.priors.b );
	}

	if(inits.containsElementNamed("sigmasqeps")) {
		cfg.params.sigmasqeps = as<double>(inits["sigmasqeps"]);
	} else {
		cfg.params.sigmasqeps = 1 / R::rgamma( cfg.priors.a, 1 / cfg.priors.b );
	}

	if(inits.containsElementNamed("rho")) {
		cfg.params.rho = as<double>(inits["rho"]);
	} else {
		cfg.params.rho = R::runif(cfg.priors.L, cfg.priors.U);
	}

	if(inits.containsElementNamed("beta")) {
		cfg.params.beta = as<vec>(inits["beta"]);
	} else {
		vec z = zeros(cfg.dat.X.n_cols);
		cfg.params.beta = mcstat2::mvrnorm(z, cfg.priors.Linv, true);
	}

	if(inits.containsElementNamed("T")) {
		cfg.params.T = as<mat>(inits["T"]);
	} else {
		cfg.params.T = mcstat2::rinvwishart(cfg.priors.Psi, cfg.priors.nu);
	}

	cfg.scratch.H = mat(cfg.consts.N, cfg.consts.N, fill::zeros);
	maternCov( cfg.scratch.H, *(cfg.dat.d), cfg.params.sigmasq, cfg.params.rho,
			  cfg.consts.nu, cfg.params.sigmasq * cfg.params.sigmasqeps );
	cfg.scratch.Hinv = inv_sympd(cfg.scratch.H);
	double one = 1.0;
	log_det(cfg.scratch.logdetH, one, cfg.scratch.H);

	if(inits.containsElementNamed("theta")) {
		cfg.params.theta = as<vec>(inits["theta"]);
	} else {
		vec z = zeros(cfg.consts.N * cfg.consts.k);
		mat invT = inv_sympd(cfg.params.T);
		cfg.params.theta = mcstat2::mvrnorm_postKron(z,
												 cfg.scratch.Hinv,
												 invT, 1, true);
	}

	cfg.scratch.zTheta = vec(cfg.consts.nt * cfg.consts.N, fill::zeros);
	for(int i=0; i<cfg.consts.nt; i++)
		cfg.scratch.zTheta.rows(i * cfg.consts.N, (i+1) * cfg.consts.N - 1) =
			mcstat2::dgeikmm(cfg.consts.N, cfg.dat.Z.col(i).t(), cfg.params.theta);

	cfg.scratch.xBeta = cfg.dat.X * cfg.params.beta;

	// instantiate and run samplers

	double sd = as<double>(t_sd);
	double C = as<double>(t_C);
	double alpha = as<double>(t_alpha);

	TSampler ts = TSampler(cfg);
	BetaSampler bs = BetaSampler(cfg);
	ThetaSampler ths = ThetaSampler(cfg);
	SigmasqSampler ss = SigmasqSampler(cfg);
	RhoSampler rs = RhoSampler(cfg, sd, cfg.params.rho, C, alpha);
	SigmasqepsSampler ses = SigmasqepsSampler(cfg, sd, cfg.params.sigmasqeps,
											  C, alpha);

	mcstat2::GibbsSampler sampler = mcstat2::GibbsSampler();
	sampler.addSampler(ts);
	sampler.addSampler(bs);
	sampler.addSampler(ths);
	sampler.addSampler(ss);
	sampler.addSampler(rs);
	sampler.addSampler(ses);
	sampler.setThinning(as<int>(thin));

	sampler.run(as<int>(nSamples));

	// return samples
	return sampler.getSamples();
}

}
