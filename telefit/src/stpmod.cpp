#include "stpmod.h"
#include "covs.h"
#include <iomanip>
#include "distributions.h"

STPModel::STPModel(Data &t_dat, Priors &t_prior, Constants &t_consts) {
	dat = t_dat;
	prior = t_prior;
	consts = t_consts;
}

struct STPModel::Params {

	Constants consts;
	vec beta;
	double sigmasq_y, sigmasq_r, sigmasq_eps, rho_y, rho_r, sigmasq_r_eps;

	// default constructor
	Params() {};

	Params(const Constants &t_consts) {
		consts = t_consts;
		beta = vec(consts.p, fill::zeros);
	}

	// initialize parameter object by sampling from prior distributions
	Params(const Priors &p, const Constants &consts) {
		beta = mcstat::mvrnorm(p.beta.mu, p.beta.Sigma);
		sigmasq_y = 1.0 / R::rgamma(p.sigmasq_y.shape, 1.0 / p.sigmasq_y.rate);
		sigmasq_eps = 1.0 / R::rgamma(p.sigmasq_eps.shape, 1.0 / p.sigmasq_eps.rate);
		rho_y = R::runif(p.rho_y.a, p.rho_y.b);
		if(!consts.localOnly) {
			sigmasq_r = 1.0 /
				R::rgamma(p.sigmasq_r.shape, 1.0 / p.sigmasq_r.rate);
			sigmasq_r_eps = 1.0 /
				R::rgamma(p.sigmasq_r_eps.shape, 1.0 / p.sigmasq_r_eps.rate);
			rho_r = R::runif(p.rho_r.a, p.rho_r.b);

			sigmasq_r_eps = 0.0;
		}

	};

	void set(const Samples &samples, int i) {
		beta = samples.beta.row(i).t();
		sigmasq_y = samples.sigmasq_y.at(i);
		sigmasq_eps = samples.sigmasq_eps.at(i);
		rho_y = samples.rho_y.at(i);

		if(!consts.localOnly) {
			sigmasq_r = samples.sigmasq_r.at(i);
			sigmasq_r_eps = samples.sigmasq_r_eps.at(i);
			rho_r = samples.rho_r.at(i);
		}
	}

	// convert object to Rcpp List
	List toList() {
		return List::create(
							_["beta"] = beta,
							_["sigmasq_y"] = sigmasq_y,
							_["sigmasq_r"] = sigmasq_r,
							_["sigmasq_r_eps"] = sigmasq_r_eps,
							_["sigmasq_eps"] = sigmasq_eps,
							_["rho_y"] = rho_y,
							_["rho_r"] = rho_r
							);
	}

};


struct STPModel::CompositionParams {

	vec alpha_knots;
	mat cat_breaks;

	CompositionParams(const Constants &consts, const mat & t_cat_breaks ) {
		if(!consts.localOnly)
			alpha_knots = vec(consts.ns * consts.nr_knots, fill::zeros);
		cat_breaks = t_cat_breaks;
	}

};

struct STPModel::CompositionScratch {

	Constants consts;
	Data dat;

	mat RknotsInv, cknots, Zknots, Sigma, RknotsInvZZknots, Sigma_cholU, eye_ns;
	vec eof_alpha_knots, neg_eof_alpha_knots, pos_eof_alpha_knots, zero_eof_alpha_knots;

	CompositionScratch() { }

	CompositionScratch(const Constants &t_consts, const Data &t_dat) {
		consts = t_consts;
		dat = t_dat;

		Sigma = mat(consts.ns, consts.ns, fill::zeros);
		zero_eof_alpha_knots = vec(consts.ns * consts.nt, fill::zeros);

		if(!consts.localOnly) {
			RknotsInv = mat(consts.nr_knots, consts.nr_knots, fill::zeros);
			RknotsInvZZknots = mat(consts.nr_knots, consts.nr_knots, fill::zeros);
			cknots = mat(consts.nr, consts.nr_knots, fill::zeros);
			Zknots = mat(consts.nr_knots, consts.nt, fill::zeros);
			eye_ns = eye(consts.ns, consts.ns);
		}
	}

	void update(const Params &params) {

		maternCov( Sigma, consts.Dy, params.sigmasq_y, params.rho_y,
				  consts.smoothness_y, params.sigmasq_y * params.sigmasq_eps );

		Sigma_cholU = chol(Sigma, "upper");

		if(!consts.localOnly) {
			maternCov( RknotsInv, consts.Dz_knots, params.sigmasq_r, params.rho_r,
					  consts.smoothness_r, params.sigmasq_r * params.sigmasq_r_eps );
			maternCov( cknots, consts.Dz_to_knots, params.sigmasq_r, params.rho_r,
					  consts.smoothness_r, params.sigmasq_r * params.sigmasq_r_eps );
			//cknots = normalise(cknots, 2, 0);
			RknotsInv = inv_sympd(RknotsInv);

			Zknots = RknotsInv * cknots.t() * dat.Z;

			RknotsInvZZknots = RknotsInv + Zknots * Zknots.t();
		}
	}

	void updateEOFAlphaKnots(vec alpha_knots) {

		// map alpha knots to eof space

		eof_alpha_knots = mcstat::dgemkmm(eye_ns, dat.W.t() * cknots * RknotsInv,
										  alpha_knots);

		// identify positive and negative entries

		neg_eof_alpha_knots = conv_to<vec>::from(eof_alpha_knots < zero_eof_alpha_knots);
		pos_eof_alpha_knots = conv_to<vec>::from(eof_alpha_knots > zero_eof_alpha_knots);

	}


};

class STPModel::CompAlphaKnot {

private:
	Data dat;
	Constants consts;
	Params* params;
	CompositionScratch* compositionScratch;
	CompositionParams* compositionParams;

	vec compMean, remoteInfo;

public:
	CompAlphaKnot () { }

	CompAlphaKnot(const Data &t_dat, const Constants &t_consts,
				  CompositionScratch &t_compositionScratch, Params &t_params,
				  CompositionParams &t_compositionParams) {
		dat = t_dat;
		consts = t_consts;
		params = &t_params;
		compositionScratch = &t_compositionScratch;
		compositionParams = &t_compositionParams;

		compMean = vec(consts.ns * consts.nr_knots, fill::zeros);
		remoteInfo = vec(consts.nr_knots, fill::zeros);
	}

	void sample() {

		// compute composition distribution parameters

		compMean.zeros();

		for(int i=0; i<consts.nt; i++) {
			int rStart = i * consts.ns;
			int rEnd = rStart + consts.ns - 1;

			solve(remoteInfo, compositionScratch->RknotsInvZZknots,
				  compositionScratch->Zknots.col(i));

			compMean += kron(dat.Y.rows(rStart, rEnd) - dat.X.rows(rStart, rEnd) *
							 params->beta, remoteInfo );
		}


		// sample parameter

		compositionParams->alpha_knots = compMean +
				  vectorise(solve(chol(compositionScratch->RknotsInvZZknots, "upper"),
								  randn<mat>(consts.nr_knots, consts.ns) *
								  compositionScratch->Sigma_cholU
								  ));

		// map parameter to eof space
		compositionScratch->updateEOFAlphaKnots(compositionParams->alpha_knots);

	}
};



struct STPModel::Scratch {
	mat C, SigmaInv;
	vec resid;
	double SigmaInv_logdet, C_logdet;

	Scratch() { }

	Scratch(const Params &params, const Constants &consts, const Data &dat) {

		double one = 1.0;

		// initialize resid
		resid = dat.Y - dat.X * params.beta;

		if(!consts.localOnly) {

			// initialize C

			C = mat(consts.nt, consts.nt, fill::zeros);
			mat RknotsInv = mat(consts.nr_knots, consts.nr_knots, fill::zeros);
			mat cknots = mat(consts.nr, consts.nr_knots, fill::zeros);
			mat eye_nt;
			eye_nt.eye(consts.nt, consts.nt);

			maternCov( RknotsInv, consts.Dz_knots, params.sigmasq_r, params.rho_r,
					  consts.smoothness_r, params.sigmasq_r * params.sigmasq_r_eps );
			maternCov( cknots, consts.Dz_to_knots, params.sigmasq_r, params.rho_r,
					  consts.smoothness_r, params.sigmasq_r * params.sigmasq_r_eps );
			//cknots = normalise(cknots, 2, 0);

			RknotsInv = inv_sympd(RknotsInv);

			mat cknotsZ = cknots.t() * dat.Z;

			C = inv_sympd( eye_nt + cknotsZ.t() * RknotsInv * cknotsZ );
			log_det(C_logdet, one, C);

		} else if(consts.localOnly) {

			// initialize "empty" C

			C = mat(consts.nt, consts.nt, fill::zeros);
			C.eye(consts.nt, consts.nt);
			log_det(C_logdet, one, C);
		}


		// initialize SigmaInv

		SigmaInv = mat(consts.ns, consts.ns, fill::zeros);

		maternCov( SigmaInv, consts.Dy, params.sigmasq_y, params.rho_y,
				  consts.smoothness_y, params.sigmasq_y * params.sigmasq_eps );

		SigmaInv = inv_sympd(SigmaInv);
		log_det(SigmaInv_logdet, one, SigmaInv);

	}
};

double STPModel::getLL(const Params &params, const Scratch &scratch) {
		vec qform = scratch.resid.t() * mcstat::dgemkmm(scratch.C,
														scratch.SigmaInv,
														scratch.resid);
		return - ( - consts.ns * scratch.C_logdet -
			   consts.nt * scratch.SigmaInv_logdet +
			   qform.at(0) ) / 2.0;
}

vec STPModel::getLL(const Samples &samples) {

	// initialize return
	int nSamples = samples.beta.n_rows;
	vec ret = vec(nSamples, fill::zeros);

	// initialize parameters
	Params current = Params(consts);

	// compute log likelihoods
	for(int i=0; i<nSamples; i++) {

		// update params
		current.set(samples, i);

		// update scratch
		Scratch scratch = Scratch(current, consts, dat);

		// compute log likelihood
		ret.at(i) = getLL(current, scratch);
	}

	return ret;
}

class STPModel::RwSigmasq_r_eps : public mcstat::RWSampler {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	mat CProposed, RknotsInv, eye_nt, cknots, cknotsZ;
	double CProposed_logdet, one;

public:

	RwSigmasq_r_eps() { }

	RwSigmasq_r_eps(const Data &t_dat, const Priors &t_prior,
					const Constants &t_consts, Scratch &t_scratch, Params &t_params,
					double sd) : RWSampler(sd) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		type = LOG;

		CProposed = mat(consts.nt, consts.nt, fill::zeros);
		RknotsInv = mat(consts.nr_knots, consts.nr_knots, fill::zeros);
		cknots = mat(consts.nr, consts.nr_knots, fill::zeros);
		eye_nt.eye(consts.nt, consts.nt);
		one = 1.0;
	}

	double logR_posterior(double sigmasq_r_eps_prop) {

		maternCov( RknotsInv, consts.Dz_knots, params->sigmasq_r, params->rho_r,
				  consts.smoothness_r, params->sigmasq_r * sigmasq_r_eps_prop );
		maternCov( cknots, consts.Dz_to_knots, params->sigmasq_r, params->rho_r,
				  consts.smoothness_r, params->sigmasq_r * sigmasq_r_eps_prop );
		//cknots = normalise(cknots, 2, 0);

		RknotsInv = inv_sympd(RknotsInv);
		cknotsZ = cknots.t() * dat.Z;

		CProposed = inv_sympd( eye_nt + cknotsZ.t() * RknotsInv * cknotsZ );

		log_det(CProposed_logdet, one, CProposed);

		vec qform = scratch->resid.t() *
		mcstat::dgemkmm(CProposed - scratch->C,
						scratch->SigmaInv,
						scratch->resid);

		return 0.5 * ( consts.ns * (CProposed_logdet -
									scratch->C_logdet) -
					  qform.at(0) )  +
		mcstat::logdinvgamma_unscaled(sigmasq_r_eps_prop,
									  prior.sigmasq_r_eps.shape,
									  prior.sigmasq_r_eps.rate) -
		mcstat::logdinvgamma_unscaled(params->sigmasq_r_eps,
									  prior.sigmasq_r_eps.shape,
									  prior.sigmasq_r_eps.rate);
	}

	// operations to perform if proposal is accepted
	void update() {
		scratch->C = CProposed;
		scratch->C_logdet = CProposed_logdet;
	}

	void sample() {
		params->sigmasq_r_eps = mcstat::RWSampler::sample(params->sigmasq_r_eps);
	}

};


class STPModel::RwSigmasq_r : public mcstat::RWSampler {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	mat CProposed, RknotsInv, eye_nt, cknots, cknotsZ;
	double CProposed_logdet, one;

public:

	RwSigmasq_r() { }

	RwSigmasq_r(const Data &t_dat, const Priors &t_prior,
				const Constants &t_consts, Scratch &t_scratch, Params &t_params,
				double sd) : RWSampler(sd) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		type = LOG;

		CProposed = mat(consts.nt, consts.nt, fill::zeros);
		RknotsInv = mat(consts.nr_knots, consts.nr_knots, fill::zeros);
		cknots = mat(consts.nr, consts.nr_knots, fill::zeros);
		eye_nt.eye(consts.nt, consts.nt);
		one = 1.0;
	}

	double logR_posterior(double sigmasq_r_prop) {

		maternCov( RknotsInv, consts.Dz_knots, sigmasq_r_prop, params->rho_r,
				  consts.smoothness_r, sigmasq_r_prop * params->sigmasq_r_eps );
		maternCov( cknots, consts.Dz_to_knots, sigmasq_r_prop, params->rho_r,
				  consts.smoothness_r, sigmasq_r_prop * params->sigmasq_r_eps );
		//cknots = normalise(cknots, 2, 0);

		RknotsInv = inv_sympd(RknotsInv);
		cknotsZ = cknots.t() * dat.Z;

		CProposed = inv_sympd( eye_nt + cknotsZ.t() * RknotsInv * cknotsZ );

		log_det(CProposed_logdet, one, CProposed);

		vec qform = scratch->resid.t() *
		mcstat::dgemkmm(CProposed - scratch->C,
						scratch->SigmaInv,
						scratch->resid);

		return 0.5 * ( consts.ns * (CProposed_logdet -
									scratch->C_logdet) -
					  qform.at(0) )  +
		mcstat::logdinvgamma_unscaled(sigmasq_r_prop,
									  prior.sigmasq_r.shape,
									  prior.sigmasq_r.rate) -
		mcstat::logdinvgamma_unscaled(params->sigmasq_r,
									  prior.sigmasq_r.shape,
									  prior.sigmasq_r.rate);
	}

	// operations to perform if proposal is accepted
	void update() {
		scratch->C = CProposed;
		scratch->C_logdet = CProposed_logdet;
	}

	void sample() {
		params->sigmasq_r = mcstat::RWSampler::sample(params->sigmasq_r);
	}

};

class STPModel::RwRho_r : public mcstat::RWSampler {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	mat CProposed, RknotsInv, eye_nt, cknots, cknotsZ;
	double CProposed_logdet, one, R;

public:
	RwRho_r() { }

	RwRho_r(const Data &t_dat, const Priors &t_prior, const Constants &t_consts,
			Scratch &t_scratch, Params &t_params, double sd) : RWSampler(sd) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		type = LOGIT;
		L = prior.rho_r.a;
		U = prior.rho_r.b;

		CProposed = mat(consts.nt, consts.nt, fill::zeros);
		RknotsInv = mat(consts.nr_knots, consts.nr_knots, fill::zeros);
		cknots = mat(consts.nr, consts.nr_knots, fill::zeros);
		eye_nt.eye(consts.nt, consts.nt);
		one = 1.0;
	}

	double logR_posterior(double rho_r_prop) {

		maternCov( RknotsInv, consts.Dz_knots, params->sigmasq_r, rho_r_prop,
				   consts.smoothness_r, params->sigmasq_r * params->sigmasq_r_eps );
		maternCov( cknots, consts.Dz_to_knots, params->sigmasq_r, rho_r_prop,
				   consts.smoothness_r, params->sigmasq_r * params->sigmasq_r_eps );
		//cknots = normalise(cknots, 2, 0);

		RknotsInv = inv_sympd(RknotsInv);
		cknotsZ = cknots.t() * dat.Z;

		CProposed = inv_sympd( eye_nt + cknotsZ.t() * RknotsInv * cknotsZ );

		log_det(CProposed_logdet, one, CProposed);

		vec qform = scratch->resid.t() *
		mcstat::dgemkmm(CProposed - scratch->C,
						scratch->SigmaInv,
						scratch->resid);

		return 0.5 * ( consts.ns * (CProposed_logdet -
									scratch->C_logdet) -
					  qform.at(0) );
	}

	// operations to perform if proposal is accepted
	void update() {
		scratch->C = CProposed;
		scratch->C_logdet = CProposed_logdet;
	}

	void sample() {
		params->rho_r = mcstat::RWSampler::sample(params->rho_r);
	}

};

class STPModel::RwSigmasq_eps : public mcstat::RWSampler {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	mat SigmaInvProposed;
	double SigmaInvProposed_logdet, one;

public:
	RwSigmasq_eps(const Data &t_dat, const Priors &t_prior,
				  const Constants &t_consts, Scratch &t_scratch, Params &t_params,
				  double sd) : RWSampler(sd) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		type = LOG;

		one = 1.0;
		SigmaInvProposed = mat(consts.ns, consts.ns, fill::zeros);
	}

	double logR_posterior(double sigmasq_eps_prop) {

		maternCov( SigmaInvProposed, consts.Dy, params->sigmasq_y, params->rho_y,
				   consts.smoothness_y, params->sigmasq_y * sigmasq_eps_prop );
		SigmaInvProposed = inv_sympd(SigmaInvProposed);
		log_det(SigmaInvProposed_logdet, one, SigmaInvProposed);

		vec qform = scratch->resid.t() *
			mcstat::dgemkmm(scratch->C,
							SigmaInvProposed - scratch->SigmaInv,
							scratch->resid);

		return ( consts.nt * (SigmaInvProposed_logdet - scratch->SigmaInv_logdet)
				- qform.at(0) ) / 2.0 +
		mcstat::logdinvgamma_unscaled(sigmasq_eps_prop,
									  prior.sigmasq_y.shape,
									  prior.sigmasq_y.rate) -
		mcstat::logdinvgamma_unscaled(params->sigmasq_eps,
									  prior.sigmasq_y.shape,
									  prior.sigmasq_y.rate);
	}

	// operations to perform if proposal is accepted
	void update() {
		scratch->SigmaInv = SigmaInvProposed;
		scratch->SigmaInv_logdet = SigmaInvProposed_logdet;
	}

	void sample() {
		params->sigmasq_eps = mcstat::RWSampler::sample(params->sigmasq_eps);
	}

};

class STPModel::RwRho_y : public mcstat::RWSampler {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	mat SigmaInvProposed;
	double SigmaInvProposed_logdet, one;

public:
	RwRho_y(const Data &t_dat, const Priors &t_prior, const Constants &t_consts,
			Scratch &t_scratch, Params &t_params, double sd) : RWSampler(sd) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		type = LOGIT;
		L = prior.rho_y.a;
		U = prior.rho_y.b;

		SigmaInvProposed = mat(consts.ns, consts.ns, fill::zeros);
		one = 1.0;
	}

	double logR_posterior(double rho_y_prop) {

		maternCov( SigmaInvProposed, consts.Dy, params->sigmasq_y, rho_y_prop,
				   consts.smoothness_y, params->sigmasq_y * params->sigmasq_eps );
		SigmaInvProposed = inv_sympd(SigmaInvProposed);

		log_det(SigmaInvProposed_logdet, one, SigmaInvProposed);

		vec qform = scratch->resid.t() *
			mcstat::dgemkmm(scratch->C,
							SigmaInvProposed - scratch->SigmaInv,
							scratch->resid);

		return 0.5 * ( consts.nt * (SigmaInvProposed_logdet -
									scratch->SigmaInv_logdet) -
					   qform.at(0) );
	}

	// operations to perform if proposal is accepted
	void update() {
		scratch->SigmaInv = SigmaInvProposed;
		scratch->SigmaInv_logdet = SigmaInvProposed_logdet;
	}

	void sample() {

		params->rho_y = mcstat::RWSampler::sample(params->rho_y);
	}

};

class STPModel::ConjBeta {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	mat priorPrecision;

public:
	ConjBeta(const Data &t_dat, const Priors &t_prior, const Constants &t_consts,
			 Scratch &t_scratch, Params &t_params) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		priorPrecision = inv_sympd(prior.beta.Sigma);
	}

	void sample() {

		mat XtCE = mcstat::dgemkmm(scratch->C, scratch->SigmaInv, dat.X).t();

		mat posteriorPrecision = priorPrecision + XtCE * dat.X;
		vec posteriorMean = solve(posteriorPrecision,  XtCE * dat.Y);

		// sample parameter
		params->beta = mcstat::mvrnorm(posteriorMean, posteriorPrecision, true);

		// update dependencies
		scratch->resid = dat.Y - dat.X * params->beta;
	}
};

class STPModel::ConjSigmasq_y {

private:
	Data dat;
	Priors prior;
	Constants consts;
	Params* params;
	Scratch* scratch;

	double posteriorShape, one;

public:
	ConjSigmasq_y(const Data &t_dat, const Priors &t_prior,
				  const Constants &t_consts, Scratch &t_scratch, Params &t_params) {
		dat = t_dat;
		prior = t_prior;
		consts = t_consts;
		scratch = &t_scratch;
		params = &t_params;

		posteriorShape = prior.sigmasq_y.shape + 0.5 * consts.ns * consts.nt;
		one = 1.0;
	}

	void sample() {

		scratch->SigmaInv = scratch->SigmaInv * params->sigmasq_y;


		vec qform = scratch->resid.t() *
			mcstat::dgemkmm( scratch->C, scratch->SigmaInv, scratch->resid );
		double posteriorRate = prior.sigmasq_y.rate + 0.5 * qform.at(0);

		// sample parameter
		params->sigmasq_y = 1.0 / R::rgamma(posteriorShape, 1.0 / posteriorRate);

		// update dependencies
		scratch->SigmaInv = scratch->SigmaInv / params->sigmasq_y;
		log_det(scratch->SigmaInv_logdet, one, scratch->SigmaInv);
	}
};


Samples STPModel::fit(int nSamples, Function errDump, double C, double RWrate,
					  double rho_y_sd, double rho_r_sd, double sigmasq_eps_sd,
					  double sigmasq_r_sd, double sigmasq_r_eps_sd) {

	// use some Rsugar to wake up R's random number generator on crossbow
	rgamma(1, 2.0, 1.0);

	// initialize output
	Samples samples = Samples(consts, nSamples);

	// initialize parameters
	Params current = Params(prior, consts);

	// initialize temporary structures
	Scratch scratch = Scratch(current, consts, dat);

	// initialize samplers
	ConjBeta conjBeta = ConjBeta(dat, prior, consts, scratch, current);
	ConjSigmasq_y conjSigmasq_y = ConjSigmasq_y(dat, prior, consts, scratch, current);
	RwRho_y rwRho_y = RwRho_y(dat, prior, consts, scratch, current, rho_y_sd);
	RwSigmasq_eps rwSigmasq_eps = RwSigmasq_eps(dat, prior, consts, scratch,
												current, sigmasq_eps_sd);
	RwRho_r rwRho_r;
	RwSigmasq_r rwSigmasq_r;
	RwSigmasq_r_eps rwSigmasq_r_eps;
	if(!consts.localOnly) {
		rwRho_r = RwRho_r(dat, prior, consts, scratch, current, rho_r_sd);
		rwSigmasq_r = RwSigmasq_r(dat, prior, consts, scratch,
											  current, sigmasq_r_sd);
		rwSigmasq_r_eps = RwSigmasq_r_eps(dat, prior, consts, scratch,
									  current, sigmasq_r_eps_sd);
	}

	// initialize trackers
	mcstat::MCMCCheckpoint checkpoint = mcstat::MCMCCheckpoint(nSamples);

	// gibbs iterations
	char step;
	int it;
	try{ for(it=0; it < nSamples; it++) {

		checkUserInterrupt();

		// conjugate beta
		step = 'B';
		conjBeta.sample();
		samples.beta.row(it) = current.beta.t();

		// conjugate sigmasq_y
		step = 'S';
		conjSigmasq_y.sample();
		samples.sigmasq_y.at(it) = current.sigmasq_y;

		// RW rho_y
		step = 'R';
		rwRho_y.sample();
		samples.rho_y.at(it) = current.rho_y;

		// RW sigmasq_eps
		step = 'e';
		rwSigmasq_eps.sample();
		samples.sigmasq_eps.at(it) = current.sigmasq_eps;

		if(!consts.localOnly) {
			// RW rho_r
			step = 'r';
			rwRho_r.sample();
			samples.rho_r.at(it) = current.rho_r;

			// RW sigmasq_r
			step = 's';
			rwSigmasq_r.sample();
			samples.sigmasq_r.at(it) = current.sigmasq_r;

			// RW sigmasq_r_eps
			step = 'n';
			//rwSigmasq_r_eps.sample();
			samples.sigmasq_r_eps.at(it) = current.sigmasq_r_eps;
		}

		// ll
		step = 'l';
		samples.ll.at(it) = getLL(current, scratch);

		// adjust tuning
		step = 't';
        double adaptScale = C / std::sqrt( (double) (it + 1) );
		rwRho_y.adapt(adaptScale, RWrate);
		rwSigmasq_eps.adapt(adaptScale, RWrate);
		if(!consts.localOnly) {
			rwRho_r.adapt(adaptScale, RWrate);
			rwSigmasq_r.adapt(adaptScale, RWrate);
			rwSigmasq_r_eps.adapt(adaptScale, RWrate);
		}

		// checkpoint behaviors
		checkpoint.run();


	} } catch(...) {
		Rcout << "An error occured while sampling '" << step << "'"
			<< " in iteration " << it << endl;

		// dump state
		errDump( List::create(
				_["samples"] = samples.toList(),
				//_["proposed"] = proposed.toList(),
				_["current"] = current.toList()
				//_["RW_samplers"] = rw.toList()
		));
	}

	// print final sampler stats
	Rcout << std::setfill('-') << std::setw(80) << "-" << endl;

	// timings
	checkpoint.finish();

	// acceptances and tunings
	Rcout << endl << "RW samplers (accept, sd)" << endl;
	Rcout << "  rho_y: (" <<
		round(rwRho_y.getAcceptanceRate() * 100.0) << "%" << ", " <<
		std::setprecision(3) << rwRho_y.getSd()  << ")" << endl;
	Rcout << "  sigmasq_eps: (" <<
		round(rwSigmasq_eps.getAcceptanceRate() * 100.0) << "%" << ", " <<
		std::setprecision(3) << rwSigmasq_eps.getSd()  << ")" << endl;
	if(!consts.localOnly) {
		Rcout << "  rho_r: (" <<
			round(rwRho_r.getAcceptanceRate() * 100.0) << "%" << ", " <<
			std::setprecision(3) << rwRho_r.getSd()  << ")" << endl;
		Rcout << "  sigmasq_r: (" <<
			round(rwSigmasq_r.getAcceptanceRate() * 100.0) << "%" << ", " <<
			std::setprecision(3) << rwSigmasq_r.getSd()  << ")" << endl;
		Rcout << "  sigmasq_r_eps: (" <<
			round(rwSigmasq_r_eps.getAcceptanceRate() * 100.0) << "%" << ", " <<
			std::setprecision(3) << rwSigmasq_r_eps.getSd()  << ")" << endl << endl;
	}

	return samples;
}


CompositionSamples STPModel::compositionSample(const Samples &samples,
											   const Data &newDat,
											   bool return_full_alpha,
											   const mat &cat_breaks) {

	// use some Rsugar to wake up R's random number generator on crossbow
	rgamma(1, 2.0, 1.0);

	// initialize output
	int nSamples = samples.ll.size();
	CompositionSamples compositionSamples = CompositionSamples(nSamples,
															   consts,
															   return_full_alpha,
															   newDat.Z.n_cols,
															   cat_breaks.n_cols);

	// initialize parameters
	Params current = Params(consts);
	CompositionParams currentComp = CompositionParams(consts, cat_breaks);

	// initialize temporary structures
	CompositionScratch compositionScratch = CompositionScratch(consts, dat);

	// initialize samplers
	CompAlphaKnot compAlphaKnot;
	if(!consts.localOnly) {
		compAlphaKnot = CompAlphaKnot(dat, consts, compositionScratch,
									  current, currentComp);
	}

	// initialize trackers
	mcstat::MCMCCheckpoint checkpoint = mcstat::MCMCCheckpoint(nSamples);

	// composition iterations

	for(int it=0; it < nSamples; it++) {

		checkUserInterrupt();

		// refresh posterior sample
		current.set(samples, it);
		compositionScratch.update(current);

		if(!consts.localOnly) {
			// alpha_knots
			compAlphaKnot.sample();
			compositionSamples.alpha_knots.row(it) = currentComp.alpha_knots.t();

			// eof-mapped teleconnection field
			compositionSamples.eof_alpha_knots(compositionScratch.eof_alpha_knots);
			compositionSamples.eof_alpha_knots_negprob(compositionScratch.neg_eof_alpha_knots);
			compositionSamples.eof_alpha_knots_posprob(compositionScratch.pos_eof_alpha_knots);

			/*
			if(eof_alpha_multipletesting) {

				compositionScratch.neg_eof_alpha_knots
				compositionScratch.pos_eof_alpha_knots
			}
			*/

			// fill in full teleconnection field
			if(return_full_alpha) {
				compositionSamples.alpha(mcstat::dgemkmm(compositionScratch.eye_ns,
														 compositionScratch.cknots *
														 compositionScratch.RknotsInv,
														 currentComp.alpha_knots));
			}
		}


		// forecasts
		if(compositionSamples.return_forecast) {

			// compute remote effects
			mat remote;
			if(!consts.localOnly) {
				remote = reshape(currentComp.alpha_knots, consts.nr_knots,
									 consts.ns).t() *
									 compositionScratch.RknotsInv *
									 compositionScratch.cknots.t() * newDat.Z;
			}

			// compute local effects
			mat local = reshape(newDat.X * current.beta, consts.ns, newDat.Z.n_cols);

			// sample spatially correlated noise, independent across time
			mat noise = compositionScratch.Sigma_cholU.t() *
				randn<mat>(consts.ns, newDat.Z.n_cols);

			// compute logits for each discretized category
			for(int i=0; i<consts.ns; i++) {
				compositionSamples.cat_probs.slice(it).row(i) =
					mcstat2::qintnorm(cat_breaks.row(i).t(),
									  local.at(i),
                                      std::sqrt(compositionScratch.Sigma.at(i,i))).t();
			}

			// save forecast objects
			if(consts.localOnly) {
				compositionSamples.forecast.slice(it) = local + noise;
			} else {
				compositionSamples.local.slice(it) = local;
				compositionSamples.remote.slice(it) = remote;
				compositionSamples.forecast.slice(it) =  local + remote + noise;
			}
		}

		// checkpoint behaviors
		checkpoint.run();
	}

	// print final sampler stats
	Rcout << std::setfill('-') << std::setw(80) << "-" << endl;

	// timings
	checkpoint.finish();

	return compositionSamples;
}


//
// R exports
//

RcppExport SEXP r_stpfit(SEXP p, SEXP X, SEXP Z, SEXP Y, SEXP Lambda,
						SEXP sigmasq_y_shape, SEXP sigmasq_y_rate,
						SEXP sigmasq_r_shape, SEXP sigmasq_r_rate,
						SEXP sigmasq_eps_shape, SEXP sigmasq_eps_rate,
						SEXP rho_y_a, SEXP rho_y_b, SEXP rho_r_a, SEXP rho_r_b,
						SEXP Dy, SEXP Dz_knots, SEXP Dz_to_knots, SEXP ns,
						SEXP nr, SEXP nr_knots, SEXP nt, SEXP smoothness_y,
						SEXP smoothness_r, SEXP nSamples, SEXP errDump,
						SEXP C, SEXP RWrate, SEXP rho_y_sd, SEXP rho_r_sd,
						SEXP sigmasq_eps_sd, SEXP sigmasq_r_sd,
						SEXP localOnly, SEXP sigmasq_r_eps_shape,
						SEXP sigmasq_r_eps_rate, SEXP sigmasq_r_eps_sd) {

	using namespace Rcpp;

	// extract model configuration

	Data dat = Data(as<mat>(X), as<mat>(Z), as<vec>(Y));

	vec zero = vec(as<int>(p), fill::zeros);
	Priors prior = Priors(zero, as<mat>(Lambda), as<double>(sigmasq_y_shape),
						  as<double>(sigmasq_y_rate), as<double>(sigmasq_r_shape),
						  as<double>(sigmasq_r_rate), as<double>(sigmasq_eps_shape),
						  as<double>(sigmasq_eps_rate), as<double>(rho_y_a),
						  as<double>(rho_y_b), as<double>(rho_r_a),
						  as<double>(rho_r_b), as<double>(sigmasq_r_eps_shape),
						  as<double>(sigmasq_r_eps_rate));



	Constants consts = Constants(as<mat>(Dy), as<mat>(Dz_knots),
								 as<mat>(Dz_to_knots), as<int>(p), as<int>(ns),
								 as<int>(nr), as<int>(nr_knots), as<int>(nt),
								 as<double>(smoothness_y), as<double>(smoothness_r),
								 as<bool>(localOnly));

	// instantiate and run sampler

	STPModel stpmod = STPModel(dat, prior, consts);
	Samples samples = stpmod.fit( as<int>(nSamples), as<Function>(errDump),
								  as<double>(C), as<double>(RWrate),
								  as<double>(rho_y_sd), as<double>(rho_r_sd),
								  as<double>(sigmasq_eps_sd),
								  as<double>(sigmasq_r_sd),
								 as<double>(sigmasq_r_eps_sd) );

	// return samples
	return samples.toList();
}

RcppExport SEXP r_stpcomposition(SEXP X, SEXP Z, SEXP Y, SEXP Dy,
								SEXP Dz_knots, SEXP Dz_to_knots, SEXP p,
								SEXP ns, SEXP nr, SEXP nr_knots, SEXP nt,
								SEXP smoothness_y, SEXP smoothness_r,
								SEXP beta_samples, SEXP sigmasq_y_samples,
								SEXP sigmasq_r_samples, SEXP sigmasq_eps_samples,
								SEXP rho_y_samples, SEXP rho_r_samples,
								SEXP ll_samples, SEXP Xnew, SEXP Znew,
								SEXP localOnly, SEXP full_alpha,
								SEXP sigmasq_r_eps_samples, SEXP W,
								SEXP cat_breaks) {

	using namespace Rcpp;

	// extract model configuration

	Data dat = Data(as<mat>(X), as<mat>(Z), as<vec>(Y), as<mat>(W));

	Data newDat = Data(as<mat>(Xnew), as<mat>(Znew));

	Priors prior = Priors();

	Constants consts = Constants(as<mat>(Dy), as<mat>(Dz_knots),
								 as<mat>(Dz_to_knots), as<int>(p), as<int>(ns),
								 as<int>(nr), as<int>(nr_knots), as<int>(nt),
								 as<double>(smoothness_y), as<double>(smoothness_r),
								 as<bool>(localOnly));

	Samples samples = Samples(as<mat>(beta_samples), as<vec>(sigmasq_y_samples),
							  as<vec>(sigmasq_r_samples), as<vec>(sigmasq_eps_samples),
							  as<vec>(rho_y_samples), as<vec>(rho_r_samples),
							  as<vec>(ll_samples), as<vec>(sigmasq_r_eps_samples));

	// instantiate and run sampler
	STPModel stpmod = STPModel(dat, prior, consts);
	CompositionSamples compositionSamples = stpmod.compositionSample(
										samples, newDat, as<bool>(full_alpha),
										as<mat>(cat_breaks));

	// return samples
	return compositionSamples.toSummarizedList();
}

RcppExport SEXP r_ll(SEXP X, SEXP Z, SEXP Y, SEXP Dy,
					SEXP Dz_knots, SEXP Dz_to_knots, SEXP p,
					SEXP ns, SEXP nr, SEXP nr_knots, SEXP nt,
					SEXP smoothness_y, SEXP smoothness_r,
					SEXP beta_samples, SEXP sigmasq_y_samples,
					SEXP sigmasq_r_samples, SEXP sigmasq_eps_samples,
					SEXP rho_y_samples, SEXP rho_r_samples,
					SEXP sigmasq_r_eps_samples) {

	using namespace Rcpp;

	bool localOnly = false;

	// extract model configuration

	Data dat = Data(as<mat>(X), as<mat>(Z), as<vec>(Y));

	Priors prior = Priors();

	Constants consts = Constants(as<mat>(Dy), as<mat>(Dz_knots),
								 as<mat>(Dz_to_knots), as<int>(p), as<int>(ns),
								 as<int>(nr), as<int>(nr_knots), as<int>(nt),
								 as<double>(smoothness_y), as<double>(smoothness_r),
								 localOnly);

	Samples samples = Samples(as<mat>(beta_samples), as<vec>(sigmasq_y_samples),
							  as<vec>(sigmasq_r_samples), as<vec>(sigmasq_eps_samples),
							  as<vec>(rho_y_samples), as<vec>(rho_r_samples),
							  as<vec>(sigmasq_y_samples),
							  as<vec>(sigmasq_r_eps_samples));

	// instantiate model
	STPModel stpmod = STPModel(dat, prior, consts);

	// return likelihoods
	return wrap(stpmod.getLL(samples));
}
