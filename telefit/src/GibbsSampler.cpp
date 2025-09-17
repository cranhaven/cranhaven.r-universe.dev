#include "GibbsSampler.h"

using namespace Rcpp;

mcstat2::MCMCCheckpoint::MCMCCheckpoint(int t_nSamples, int t_thin) {
	nSamples = t_nSamples;
	thin = t_thin;
	checkPointIt = (int) nSamples * 0.1;
	it = 0;
	start = std::clock();
	lap = start;
}

void mcstat2::MCMCCheckpoint::reset() {
	it = 0;
	start = std::clock();
	lap = start;
}

void mcstat2::MCMCCheckpoint::run() {
	if(it++ % checkPointIt == 0) {
		// compute time since last checkpoint
		std::clock_t tmp_clock = lap;
		lap = std::clock();
		double duration = ( lap - tmp_clock ) / (double) CLOCKS_PER_SEC;
		
		// compute percent complete
		double pctComplete = (double) it / (double) nSamples * 100.0;
		if(it==1) {
			pctComplete /= (double) thin;
		}
		
		// compute remaining time
		double total = (lap - start) / (double) CLOCKS_PER_SEC;
		double remaining = (100.0 - pctComplete) * (total / pctComplete) / 60.0;
		
		// output information
		Rcout << round(pctComplete) << "% complete" << " (" <<
		floor(duration * 10.0) / 10.0 << " seconds; " <<
		floor(remaining * 10.0) / 10.0 << " minutes remaining)" <<
		endl;
	}
}

void mcstat2::MCMCCheckpoint::finish() {
	// compute final time
	lap = std::clock();
	double duration = (lap - start) / (double) CLOCKS_PER_SEC;
	
	// compute samples per second
	double sampSec = it / duration;
	
	// output information
	Rcout << endl << "Total time (min): " << floor(duration / 60.0 * 10.0) / 10.0 <<
	endl << "Samples per second: " << floor(sampSec * 10.0) / 10.0 << endl;
}

void mcstat2::GibbsSampler::setThinning(int t_thin) {
	thin = t_thin < 1 ? 1 : t_thin;
}

void mcstat2::GibbsSampler::addSampler(Sampler &s) {
	samplers.push_back(&s);
}

void mcstat2::GibbsSampler::run(int nSamples) {
	
	GetRNGstate();
	
	// use some Rsugar to wake up R's random number generator on crossbow
	rgamma(1, 2.0, 1.0);
	
	// initialize trackers
	mcstat2::MCMCCheckpoint checkpoint = mcstat2::MCMCCheckpoint(nSamples, thin);
	
	// initialize samples
	for(auto sampler = samplers.begin(); sampler != samplers.end(); ++sampler) {
		switch((*sampler)->getType()) {
			case Sampler::SamplerType::REAL:
				samples.push_back(mat(nSamples, 1, fill::zeros));
				break;
			case Sampler::SamplerType::VECTOR:
				samples.push_back(mat(nSamples, (*sampler)->getSize(), fill::zeros));
				break;
		}
	}
	
	int it;
	int totSamples = nSamples * thin;
	int saved = 0;
	std::string step;
	try{
		// reset timers
		checkpoint.reset();
		
		// gibbs iterations
		for(it=0; it < totSamples; it++) {
			
			checkUserInterrupt();
			
			// gibbs steps: iterate over samplers
			auto sample = samples.begin();
			for(auto sampler = samplers.begin(); sampler != samplers.end(); ++sampler) {
				
				// prep for error handling: identify step
				step = (*sampler)->getName();
				
				// sample and save parameter
				vec s = (*sampler)->sample();
				if(it % thin == 0)
					sample->row(saved) = s.t();
				
				// iterate storage pointer
				++sample;
			}
			
			// update output index
			if(it % thin == 0)
				saved++;
			
			// checkpoint behaviors
			if(it % thin == 0)
				checkpoint.run();
		}
	} catch(...) {
		Rcout << "An error occured while sampling " << step <<
		" in iteration " << it << " for sample " << saved << endl;
		
		// TODO: dump state by cycling through samplers to get their state
	}
	
	// print final sampler stats
	Rcout << std::setfill('-') << std::setw(80) << "-" << endl;
	
	// timings
	checkpoint.finish();
	
	// print sampler stats
	for(auto sampler = samplers.begin(); sampler != samplers.end(); ++sampler) {
		(*sampler)->printStats();
	}
	
	PutRNGstate();
}

List mcstat2::GibbsSampler::getSamples() {
	
	// initialize output containers for parameter names and samples
	int n = samplers.size();
	List res(n);
	CharacterVector parameterNames(n);
	
	// populate output containers
	int i=0;
	auto sampler = samplers.begin();
	for(std::vector<mat>::iterator sample = samples.begin();
		sample != samples.end(); ++sample){
		// prepare sample label
		parameterNames[i] = (*sampler)->getName();
		// export samples
		res[i++] = (*sample);
		// iterate samplers
		++sampler;
	}
	
	// label samples
	res.names() = parameterNames;
	
	return res;
}

mcstat2::Sampler::SamplerType mcstat2::Sampler::getType() {
	return type;
}

std::string mcstat2::Sampler::getName() {
	return name;
}

int mcstat2::Sampler::getSize() {
	return type == REAL ? 1 : sample().size();
}
