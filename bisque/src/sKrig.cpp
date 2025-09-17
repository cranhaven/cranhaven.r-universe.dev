/*
	gibbs sampler to estimate parameters of a spatial random field.
 */

// disable assertions
#define EIGEN_NO_DEBUG

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include "GibbsSampler.h"
#include "covs.h"
#include "distributions.h"

using namespace Rcpp;
using namespace arma;
using Eigen::MatrixXd;

namespace {

struct Data {
    MatrixXd x1; // each row has response for one location (Nx1)
	mat  *d11,            // matrix containing interpoint distances (NxN)
         *d01,  // matrix containing distances between kriging and sampling locations (N0 x N)
         *d00;

    vec *sigmasq, // posterior samples
        *rho,
        *nu;
};

struct Params {
	double sigmasq,	// scale for spatial covariance function
		   rho,     // range for spatial covariance function
		   nu;		// smoothness for spatial covariance function

    void set(const Data &samples, int i) {
        sigmasq = samples.sigmasq->at(i);
        rho = samples.rho->at(i);
        nu = samples.nu->at(i);
    }
};

struct Consts {
	int nSamples, // number of posterior draws available
        N1,        // number of spatial locations
        N0;       // number of kriging locations
};

struct Config {
    Data dat; Params params; Consts consts;
};


class KrigSampler : public mcstat2::Sampler {

private:

    Config *cfg;
    int it;
    mat S00, S01, S11;

public:

    KrigSampler(Config &t_cfg) {
        name = "x0"; type = VECTOR; cfg = &t_cfg; it = 0;
        S00 = mat(cfg->consts.N0, cfg->consts.N0, fill::zeros);
        S01 = mat(cfg->consts.N0, cfg->consts.N1, fill::zeros);
        S11 = mat(cfg->consts.N1, cfg->consts.N1, fill::zeros);
    }

    int getSize() { return S00.n_rows; }

    vec sample() {

        // extract posterior parameter sample
        cfg->params.set(cfg->dat, it++);

        // build covariance matrices
        maternCov(S00, *(cfg->dat.d00), cfg->params.sigmasq, cfg->params.rho, cfg->params.nu, 0);
        maternCov(S01, *(cfg->dat.d01), cfg->params.sigmasq, cfg->params.rho, cfg->params.nu, 0);
        maternCov(S11, *(cfg->dat.d11), cfg->params.sigmasq, cfg->params.rho, cfg->params.nu, 0);

        // save MCMC output
        return mcstat2::mvnorm_cond(cfg->dat.x1, S00, S01, S11);
    }
};


RcppExport SEXP t_spredict (SEXP X1, SEXP D00, SEXP D01, SEXP D11,
                            SEXP sigmasq, SEXP rho, SEXP nu) {

	using namespace Rcpp;

	// extract model configuration

	Config cfg = Config();

    // map observations into eigen
    using Eigen::Map;
    using Eigen::MatrixXd;
    vec arma_x1 = as<vec>(X1);
    int N1 = arma_x1.n_elem;
    Map<MatrixXd> x1_eigen(arma_x1.memptr(), N1, 1);
    cfg.dat.x1 = x1_eigen;

    mat d00_arma = as<mat>(D00);
    mat d01_arma = as<mat>(D01);
    mat d11_arma = as<mat>(D11);
    vec sigmasq_arma = as<vec>(sigmasq);
    vec rho_arma = as<vec>(rho);
    vec nu_arma = as<vec>(nu);

    cfg.dat.sigmasq = &sigmasq_arma;
    cfg.dat.rho = &rho_arma;
    cfg.dat.nu = &nu_arma;
    cfg.dat.d00 = &d00_arma;
    cfg.dat.d01 = &d01_arma;
    cfg.dat.d11 = &d11_arma;

    int N0 = d00_arma.n_cols;

    cfg.consts.N1 = N1;
    cfg.consts.N0 = N0;
    cfg.consts.nSamples = sigmasq_arma.n_elem;

    // instantiate and run samplers

    KrigSampler ks = KrigSampler(cfg);

    mcstat2::GibbsSampler sampler = mcstat2::GibbsSampler();
    sampler.addSampler(ks);
    sampler.run(cfg.consts.nSamples);

    // return samples
    return sampler.getSamples();
}

}
