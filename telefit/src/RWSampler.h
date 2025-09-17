#ifndef _RWSAMPLER_H
#define _RWSAMPLER_H

#include <RcppArmadillo.h>
#include "GibbsSampler.h"

namespace mcstat2 {

	//
	// univariate adaptive random walk sampler
	//

	class RWSampler : public Sampler {

	protected:
		enum ProposalType { NORMAL, LOG, LOGIT };
		int nSamples ;
		double accept, sd, L, U, current, C, alpha;
		ProposalType propType;

		// return likelihood and prior, no jacobian; gets a copy of
		// proposed value x and current value x0
		virtual double logR_posterior(double x, double x0) = 0;

		// run this code if the proposal is accepted
		virtual void update() {};

	public:
		// implement Sampler class' sample method
		vec sample();
		// sample a new parameter value given the current value x0
		double sample(double x0);
		// auto-tune proposal s.d.
		void adapt(double adaptScale, double targetRate);
		// return list with acceptance rate and proposal s.d.
		Rcpp::List toList();

		RWSampler () { type = VECTOR; }

		RWSampler(double t_sd, double t_current, double t_C, double t_alpha) {
			nSamples = 0;
			accept = 0;
			sd = t_sd;
			current = t_current;
			type = VECTOR;
			C = t_C;
			alpha = t_alpha;
		}

		double getAcceptanceRate();
		double getSd();
	};

}

#endif
