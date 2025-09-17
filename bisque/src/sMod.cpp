/*
	gibbs sampler to estimate parameters of a spatial random field.
 */

// disable assertions
#define EIGEN_NO_DEBUG

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include "GibbsSampler.h"
#include "covs.h"
#include "RWSampler.h"
#include "ConjInvGamma.h"


using namespace Rcpp;
using namespace arma;

namespace{

struct Data {
    Eigen::MatrixXd x; // each row has response for one location + timepoint (Nx1)
	mat* d; // matrix containing interpoint distances (NxN)
};

struct Params {
	double sigmasq,	// scale for spatial covariance function
		   rho,     // range for spatial covariance function
		   nu;		// smoothness for spatial covariance function
};

struct Consts {
	int N;     // number of spatial locations
    double logC;  // log of normalizing constant for multivariate normal
};

struct Priors {
	double a, b;     // sigmasq ~ Inv-Gamma(a, b)
	double rL, rU;   // rho ~ Uniform(L, U)
    double nL, nU;   // nu ~ Uniform(L, U)
};

struct Scratch {
	mat Sigma; // spatial covariance matrix
    double logdetSigma, sumsq, ll;
};

struct Config {
	Data dat; Params params; Consts consts; Priors priors; Scratch scratch;
};

void fillScratch(Config  *c, Params *p, Scratch *s) {

    using Eigen::MatrixXd;
    using Eigen::Map;
    using Eigen::LLT;
    using Eigen::Lower;

    // build  covariance  matrix; read into eigen
    maternCov(s->Sigma, *(c->dat.d), p->sigmasq, p->rho, p->nu,  0);
    Map<MatrixXd> Sigma(s->Sigma.memptr(), c->consts.N, c->consts.N);

    // factor covariance matrix; store lower cholesky
    LLT<MatrixXd, Lower> llt(Sigma);

    // compute log-determinant
    s->logdetSigma = llt.matrixLLT().diagonal().array().log().sum();

    // compute log-likelihood
    MatrixXd y;
    y = llt.matrixL().solve(c->dat.x);
    s->sumsq = y.squaredNorm();
    s->ll = c->consts.logC - s->logdetSigma - .5 * s->sumsq;
}


class SigmasqSampler : public mcstat2::ConjInvGamma {

private:

	Config *cfg;

public:

    SigmasqSampler(Config &t_cfg, double a, double b, double n) :
    mcstat2::ConjInvGamma(a, b, n) {
        name = "sigmasq"; type = REAL;
        cfg = &t_cfg;
    }

    vec sample() {

        // sample from posterior
        double s0;
        s0 = mcstat2::ConjInvGamma::sample(
            cfg->scratch.sumsq * cfg->params.sigmasq);

        // update dependencies

        cfg->scratch.logdetSigma +=
            .5 * cfg->consts.N * (std::log(s0) - std::log(cfg->params.sigmasq) );
        cfg->scratch.sumsq *= cfg->params.sigmasq / s0;
        cfg->scratch.ll =  cfg->consts.logC - cfg->scratch.logdetSigma -
            .5 * cfg->scratch.sumsq;
        cfg->params.sigmasq = s0;


        //cfg->params.sigmasq = s0;
        //fillScratch(cfg, &cfg->params, &cfg->scratch);

        // save sample
        vec r = vec(1);
        r.at(0) = s0;
        return r;
    }

};


class RhoSampler : public mcstat2::RWSampler {

private:

	Config *cfg;
    Scratch tmpScratch;
    Params tmpParams;

public:

	RhoSampler(Config &t_cfg, double t_sd, double t_cur, double C,
               double alpha) :
	RWSampler(t_sd, t_cur, C, alpha) { name = "rho"; type = REAL; cfg = &t_cfg;
		propType = LOGIT;
		L = cfg->priors.rL;
		U = cfg->priors.rU;

        tmpScratch = Scratch();
        tmpScratch.Sigma =  mat(cfg->consts.N, cfg->consts.N, fill::zeros);

        tmpParams = Params();
	}

	// rho is proposed, rho0 is current
	double logR_posterior(double rho, double rho0) {

        tmpParams.sigmasq = cfg->params.sigmasq;
        tmpParams.rho = rho;
        tmpParams.nu = cfg->params.nu;

        fillScratch(cfg, &tmpParams, &tmpScratch);

        return tmpScratch.ll - cfg->scratch.ll;
	}

	void update() {
        cfg->scratch.logdetSigma = tmpScratch.logdetSigma;
        cfg->scratch.sumsq = tmpScratch.sumsq;
        cfg->scratch.ll = tmpScratch.ll;
		cfg->params.rho = tmpParams.rho;
	}
};


class NuSampler : public mcstat2::RWSampler {

private:

    Config *cfg;
    Scratch tmpScratch;
    Params tmpParams;

public:

    NuSampler(Config &t_cfg, double t_sd, double t_cur, double C,
               double alpha) :
    RWSampler(t_sd, t_cur, C, alpha) { name = "nu"; type = REAL; cfg = &t_cfg;
        propType = LOGIT;
        L = cfg->priors.nL;
        U = cfg->priors.nU;

        tmpScratch = Scratch();
        tmpScratch.Sigma =  mat(cfg->consts.N, cfg->consts.N, fill::zeros);

        tmpParams = Params();
    }

    // nu is proposed, nu0 is current
    double logR_posterior(double nu, double nu0) {

        tmpParams.sigmasq = cfg->params.sigmasq;
        tmpParams.rho = cfg->params.rho;
        tmpParams.nu = nu;

        fillScratch(cfg, &tmpParams, &tmpScratch);

        return tmpScratch.ll - cfg->scratch.ll;
    }

    void update() {
        cfg->scratch.logdetSigma = tmpScratch.logdetSigma;
        cfg->scratch.sumsq = tmpScratch.sumsq;
        cfg->scratch.ll = tmpScratch.ll;
        cfg->params.nu = tmpParams.nu;
    }
};


class llSampler : public mcstat2::Sampler {

private:

    Config *cfg;

public:

    llSampler(Config &t_cfg) { name = "ll"; type = REAL; cfg = &t_cfg;

    }

    vec sample() {
        vec r = vec(1);
        r.at(0) = cfg->scratch.ll;
        return r;
    }
};


RcppExport SEXP t_sfit  (SEXP X, SEXP D, SEXP a, SEXP b, SEXP rL, SEXP rU,
                         SEXP nL, SEXP nU, SEXP nSamples, SEXP thin,
                         SEXP t_sd, SEXP t_inits, SEXP t_C, SEXP t_alpha) {

	using namespace Rcpp;

	// extract model configuration

	Config cfg = Config();

    // map observations into eigen
    using Eigen::Map;
    using Eigen::MatrixXd;
    vec arma_x = as<vec>(X);
    int N = arma_x.n_elem;
    Map<MatrixXd> x(arma_x.memptr(), N, 1);

    cfg.dat.x = x;
    mat d = as<mat>(D);
    cfg.dat.d = &d;

    cfg.consts.N = N;
    cfg.consts.logC = - .5 * N * log(6.28318530718);

    cfg.priors.a = as<double>(a);
    cfg.priors.b = as<double>(b);
    cfg.priors.rL = as<double>(rL);
    cfg.priors.rU = as<double>(rU);
    cfg.priors.nL = as<double>(nL);
    cfg.priors.nU = as<double>(nU);

	// initialize parameters and scratch

	List inits = as<List>(t_inits);

	if(inits.containsElementNamed("sigmasq")) {
		cfg.params.sigmasq = as<double>(inits["sigmasq"]);
	} else {
		cfg.params.sigmasq = 1 / R::rgamma( cfg.priors.a, 1 / cfg.priors.b );
	}

	if(inits.containsElementNamed("rho")) {
		cfg.params.rho = as<double>(inits["rho"]);
	} else {
		cfg.params.rho = R::runif(cfg.priors.rL, cfg.priors.rU);
	}

    if(inits.containsElementNamed("nu")) {
        cfg.params.nu = as<double>(inits["nu"]);
    } else {
        cfg.params.nu = R::runif(cfg.priors.nL, cfg.priors.nU);
    }

    cfg.scratch.Sigma = mat(cfg.consts.N, cfg.consts.N, fill::zeros);

    fillScratch(&cfg, &cfg.params, &cfg.scratch);


	// instantiate and run samplers

	double sd = as<double>(t_sd);
	double C = as<double>(t_C);
	double alpha = as<double>(t_alpha);

    SigmasqSampler ss = SigmasqSampler(cfg, cfg.priors.a, cfg.priors.b,
                                       cfg.consts.N);
    RhoSampler rs = RhoSampler(cfg, sd, cfg.params.rho, C, alpha);
    NuSampler ns = NuSampler(cfg, sd, cfg.params.nu, C, alpha);
    llSampler  ls = llSampler(cfg);

	mcstat2::GibbsSampler sampler = mcstat2::GibbsSampler();
	sampler.addSampler(ss);
    sampler.addSampler(rs);
    sampler.addSampler(ns);
    sampler.addSampler(ls);
	sampler.setThinning(as<int>(thin));

	sampler.run(as<int>(nSamples));

	// return samples
	return sampler.getSamples();
}

}
