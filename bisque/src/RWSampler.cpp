#include "RWSampler.h"
#include "transformations.h"

double logitProposal(double x, double min_x, double max_x, double sd) {
	double w = max_x - min_x;
	return mcstat2::invlogit( mcstat2::logit((x - min_x)/w) + R::rnorm(0, sd) ) * w + min_x;
}

double logProposal(double x, double sd) {
	return exp( log(x) + R::rnorm(0, sd) );
}

double loglogJacobian(double x) { return -log( x ); }

double loglogitJacobian(double x) { return -log( x*(1.0-x) ); }

double mcstat2::RWSampler::getAcceptanceRate() { return accept; }

double mcstat2::RWSampler::getSd() { return sd; }

void mcstat2::RWSampler::adapt(double adaptScale, double targetRate) {
	sd *= exp( adaptScale * (accept - targetRate) );
}

Rcpp::List mcstat2::RWSampler::toList() {
	return Rcpp::List::create(
						Rcpp::_["accept"] = accept,
						Rcpp::_["sd"] = sd
						);
}

double mcstat2::RWSampler::sample(double x0) {
	
	double logR = 0;
	double x = x0;
	
	switch(propType) {
		case NORMAL:
			x += R::rnorm(0, sd);
			logR = logR_posterior(x, x0);
			break;
			
		case LOG:
			x = logProposal(x0, sd);
			logR = logR_posterior(x, x0) + loglogJacobian(x0) - loglogJacobian(x);
			break;
			
		case LOGIT:
			x = logitProposal(x0, L, U, sd);
			logR = logR_posterior(x, x0) + loglogitJacobian(x0) - loglogitJacobian(x);
			break;
	}
	
	bool accepted = log(R::runif(0,1)) <= std::min(logR, 0.0);
	
	if(accepted) {
		update();
		current = x;
	} else {
		x = x0;
	}
	
	accept += ((accepted ? 1.0 : 0.0) - accept) / (double) (++nSamples);
	
	adapt( C / sqrt( (double) (nSamples)), alpha);
	
	return x;
}

arma::vec mcstat2::RWSampler::sample() {
	vec r = vec(1);
	r.at(0) = sample(current);
	return r;
}
