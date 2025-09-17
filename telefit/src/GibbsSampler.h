#ifndef _GIBBSSAMPLER_H
#define _GIBBSSAMPLER_H

#include <RcppArmadillo.h>

namespace mcstat2 {
	
	using namespace Rcpp;
	using namespace arma;
	
	class MCMCCheckpoint {
		
	private:
		int it, thin, checkPointIt, nSamples;
		std::clock_t lap, start;
		
	public:
		
		MCMCCheckpoint(int nSamples, int thin);
		
		void reset();
		void run();
		void finish();
	};
	
	// abstract class for a sampler that can be used with mcstat::GibbsSampler
	class Sampler {
		
	public:
		enum SamplerType { REAL, VECTOR };
		
		Sampler() { };
		
		SamplerType getType();
		std::string getName();
		
		/* sampler should:
			1. update all relevant data parameters and output lists
			2. autotune itself, if necessary
			3. return the value of the sampled parameter
		 */
		virtual vec sample() = 0;
		
		// for printing acceptance rates and tuning parameters, etc. to Rcout
		virtual void printStats() {};
		
		// returns 1 or the size of sampled vectors; by default this is done by
		// running the sample function, but inherited classes can overload this
		virtual int getSize();
		
	protected:
		
		// for initializing MCMC output containers
		SamplerType type;
		// for labeling the samples when sent back to R as a List object
		std::string name;
		
	};
	
	// runs a collection of mcstat::Sampler objects in a Gibbs sampling scheme
	class GibbsSampler {
		
	private:
		std::vector<Sampler*> samplers;
		std::vector<mat> samples;
		int thin;
		
	public:
		
		GibbsSampler() { thin = 1; };
		
		void addSampler(Sampler &);
		void setThinning(int);
		
		// loops over the samplers, calling their sample functions
		void run(int);
		
		List getSamples();
	};
}

#endif
