#ifndef _telefit_MCSTAT_H
#define _telefit_MCSTAT_H


#include <RcppArmadillo.h>


namespace mcstat {
	
	using namespace Rcpp;
	using namespace arma;
	
	
	//
	// structures to hold distribution parameters
	//
	
	struct mvnorm {
		vec mu;
		mat Sigma;
		
		mvnorm() {}
		
		mvnorm(const vec &_mu, const mat &_Sigma) {
			mu = _mu;
			Sigma = _Sigma;
		}
	};
	
	struct invgamma {
		double shape, rate;
		
		invgamma() {}
		
		invgamma(double _shape, double _rate) {
			shape = _shape;
			rate = _rate;
		}
	};
	
	struct uniform {
		double a, b;
		
		uniform() { }
		
		uniform(double _a, double _b) {
			a = _a;
			b = _b;
		}
	};
	
	
	//
	// efficient linear algebra routines
	//
	
	// evaluate kron(A,B) * C without storing kron(A,B)
	mat dgemkmm(mat A, mat B, mat C);
	mat dsemkmm(mat A, mat B, SpMat<double> C);
	
	
	//
	// transformations
	//
	
	double logit(double x);
	double invlogit(double x);
	

	//
	// densities
	//
	
	// log inverse gamma density without normalization constant
	double logdinvgamma_unscaled(double x, double a, double b);
	
	// log beta density without normalization constant
	double logdbeta_unscaled(double x, double a, double b);
	
	
	//
	// proposal functions
	//
	
	double logitProposal(double x, double min_x, double max_x, double sd);
	double logProposal(double x, double sd);
	
	
	//
	// proposal jacobians
	//
	
	double loglogJacobian(double x);
	double loglogitJacobian(double x);
	
	
	//
	// samplers
	//
	
	// sample a mean 0 multivariate normal r.v.
	vec mvrnorm(const mat & sigma);
	
	// sample a multivariate normal r.v.
	vec mvrnorm(const vec & mu, const mat & sigma);
	
	// sample a multivariate normal r.v. using a precision or covariance matrix
	vec mvrnorm(const vec &mu, const mat &sigma, bool precision);
	
	// sample a wishart matrix using bartlett's decompostion
	mat rwishart(const mat &V, int n);
	
	// sample an inverse wishart matrix
	mat rinvwishart(const mat &V, int n);
	
	
	//
	// utility functions
	//

	
	class MCMCCheckpoint {
	
	private:
		int it, checkPointIt, nSamples;
		std::clock_t lap, start;
		
	public:
		
		MCMCCheckpoint(int nSamples);
		
		void run();
		void finish();
	};
	
	
	//
	// abstract sampling functions
	//
	
	// univariate adaptive random walk sampler
	class RWSampler {
		
	protected:
		enum ProposalType { NORMAL, LOG, LOGIT };
		int nSamples ;
		double accept, sd, L, U;
		ProposalType type;
		
		// likelihood and prior, no jacobian; gets a copy of proposed value, x
		virtual double logR_posterior(double x) = 0;
		
		virtual void update() {};
		
	public:
		double sample(double x0);
		void adapt(double adaptScale, double targetRate);
		List toList();
		
		RWSampler () { }
		
		RWSampler(double _sd);
		
		double getAcceptanceRate();
		double getSd();
	};
}



#endif
