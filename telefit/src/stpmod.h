#include <RcppArmadillo.h>
#include "stpnotation.h"

#ifndef _telefit_STPModel_H
#define _telefit_STPModel_H

using namespace Rcpp;
using namespace arma;

//
// spatial teleconnection model with predictive process remote coefficients and
//	fixed effect local coefficients
//

class STPModel {

private:
	
	// data and model
	Data dat;
	Priors prior;
	Constants consts;
	
	// state
	struct Params;
	struct Scratch;
	struct CompositionParams;
	struct CompositionScratch;
	
	// samplers
	class ConjBeta;
	class ConjSigmasq_y;
	class RwRho_y;
	class RwRho_r;
	class RwSigmasq_r;
	class RwSigmasq_r_eps;
	class RwSigmasq_eps;
	class CompAlphaKnot;
	
	double getLL(const Params &params, const Scratch &scratch);
	
public:
	
	STPModel(Data &_dat, Priors &_prior, Constants &_consts);
	
	Samples fit(int maxit, Function errDump, double C, double RWrate,
				double rho_y_sd, double rho_r_sd, double sigmasq_eps_sd,
				double sigmasq_r_sd, double sigmasq_r_eps_sd);
	
	CompositionSamples compositionSample(const Samples &samples,
										 const Data &newDat,
										 bool return_full_alpha,
										 const mat &cat_breaks);
	
	vec getLL(const Samples &samples);
};

#endif
