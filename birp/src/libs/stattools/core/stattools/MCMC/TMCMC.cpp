//
// Created by Madleina Caduff on 03.04.20.
//

#ifdef WITH_PROFILING
#include <gperftools/profiler.h>
#endif // WITH_PROFILING

#include "coretools/Main/TLog.h"
#include "coretools/Main/TParameters.h"
#include "stattools/DAG/TDAGBuilder.h"
#include "stattools/MCMC/TMCMC.h"

#include "coretools/Main/progressTools.h"
#include "stattools/DAG/TDAGBuilder.h"

namespace stattools {

using namespace coretools::instances;

//-------------------------------------------
// TMCMC
//-------------------------------------------

TMCMC::TMCMC() { _readCommandLineArgs(); }

TMCMC::TMCMC(size_t NumIterations, size_t NumBurnin, size_t NumBurninIterations)
	: _iterations(NumIterations), _iterationsPerBurnin(NumBurninIterations), _numBurnin(NumBurnin) {
	_readCommandLineArgs();
}

void TMCMC::_prepareFiles(std::string_view OutName) {
	instances::dagBuilder().prepareAllMCMCFiles(_writeStateFile, _writeDensitiesTraceFile, OutName);
}

void TMCMC::_readCommandLineArgs() {
	// reading MCMC parameters
	_readMCMCChainParameters();
	_readBurninParameters();
	_readStateFileParameters();
	_readThinningParameters();

	logfile().endIndent();
}

void TMCMC::setUp(std::string_view OutName) {
	instances::dagBuilder().buildDAG();

	// prepare files
	_prepareFiles(OutName);

	// run initialization (MLE etc)
	_runPriorParameterInitialization();
}

void TMCMC::runMCMC(std::string_view OutName) {
	setUp(OutName);

	// run MCMC
	_runMCMC();
}

void TMCMC::_readMCMCChainParameters() {
	logfile().startIndent("Reading MCMC parameters:");
	_iterations = (size_t)parameters().get<double>("iterations", (double)_iterations); // to allow for scientific 1e06
	logfile().list("Will run an MCMC for ", _iterations, " iterations. (parameter 'iterations')");
}

void TMCMC::_readBurninParameters() {
	// run a fixed number of burnins with a fixed length
	_numBurnin = (size_t)parameters().get<double>("numBurnin", (double)_numBurnin); // to allow for scientific 1e06
	_iterationsPerBurnin =
		(size_t)parameters().get<double>("burnin", (double)_iterationsPerBurnin); // to allow for scientific 1e06
	logfile().list("Will run ", _numBurnin, " burnins of ", _iterationsPerBurnin,
				   " iterations each.  (parameters 'numBurnin' and 'burnin')");
}

void TMCMC::_readStateFileParameters() {
	// writing a state file? (= file with values and jumpSizes for all parameters, can be used to re-start MCMC)
	if (parameters().exists("writeState")) {
		_writeStateFile    = true;
		_thinningStateFile = (size_t)parameters().get<double>("thinningStateFile", 1000); // to allow for scientific
		if (_thinningStateFile > 0) {
			logfile().list("Will write the state of the MCMC chain every ", _thinningStateFile,
						   " iterations. (parameters 'writeState' and 'thinningStateFile')");
		}
	}
}

void TMCMC::_readThinningParameters() {
	// write trace files during burnin?
	if (parameters().exists("writeBurnin")) {
		_writeBurnin = true;
		logfile().list("Will write the trace of the burnin to file. (parameter 'writeBurnin')");
	}

	if (parameters().exists("writeDensities")) {
		_writeDensitiesTraceFile = true;
		logfile().list("Will write the trace of the densities. (parameter 'writeDensities')");
	}

	// thinning
	int tmp = (int)parameters().get<double>("thinning", 10.0); // to allow for scientific 1e06
	if (tmp < 1) { throw coretools::TUserError("Parameter 'thinning' must be > 0!"); }
	_thinning = tmp;
	if (_thinning == 1) {
		logfile().list("Will write full chain. (parameter 'thinning')");
	} else if (_thinning == 2) {
		logfile().list("Will write every second iteration. (parameter 'thinning')");
	} else if (_thinning == 3) {
		logfile().list("Will write every third iteration. (parameter 'thinning')");
	} else {
		logfile().list("Will write every ", _thinning, "th iteration. (parameter 'thinning')");
	}
}

void TMCMC::_runPriorParameterInitialization() {
	logfile().startIndent("Running prior parameter initialization:");

	// initialize prior parameters with MLE/MOM/EM etc.
	instances::dagBuilder().guessInitialValues();

	logfile().endIndent();
}

void TMCMC::_runMCMC() {
	// run MCMC
	logfile().startIndent("Running MCMC inference:");

#ifdef WITH_PROFILING
	ProfilerStart("profile.prof");
#endif // WITH_PROFILING

	// write values from initialization (if needed)
	_writeInitialValues();

	// run burnin
	_runFixedBurnin();
	_burninHasFinished();

	// run MCMC chain
	logfile().startIndent("Running MCMC chain:");
	_runMCMCChain();
	_MCMCHasFinished();
	logfile().endIndent();
	logfile().endIndent();

#ifdef WITH_PROFILING
	ProfilerStop();
#endif // WITH_PROFILING
}

void TMCMC::_runBurninIterations(size_t NumIterations, std::string_view ProgressString) {
	coretools::TProgressReporter prog(NumIterations, ProgressString);

	for (size_t i = 0; i < NumIterations; ++i) {
#ifdef USE_RCPP
		if (i % 1000 == 0) { Rcpp::checkUserInterrupt(); }
#endif
		_runMCMCIterationFull(i);
		if (_writeBurnin) { _writeToFiles(i); }
		prog.next();
	}

	prog.done();
}

void TMCMC::_finalizeBurnin() {
	// report acceptance rates
	_reportAcceptanceRates();

	// adjust proposal ranges
	logfile().listFlush("Adjusting proposal parameters ...");
	_adjustProposalRanges();
	logfile().done();
}

void TMCMC::_runFixedBurnin() {
	if (_numBurnin > 0) {
		if (_numBurnin > 1) {
			logfile().startNumbering("Running ", _numBurnin, " burnins:");
		} else {
			logfile().startNumbering("Running ", _numBurnin, " burnin:");
		}
		for (size_t b = 0; b < _numBurnin; ++b) { _runBurnin(b + 1, _iterationsPerBurnin); }
		logfile().endNumbering();
	}
}

void TMCMC::_writeToFiles(size_t Iteration) {
	if (Iteration % _thinning == 0) { _writeToTraceFiles(); }
	if (_writeStateFile && Iteration % _thinningStateFile == 0) { _writeToStateFile(Iteration); }
}

void TMCMC::_writeInitialValues() {
	// write values of initialization to file, if needed
	if (_writeBurnin) { _writeToTraceFiles(); }
}

void TMCMC::_runBurnin(size_t Counter, size_t Iterations) {
	logfile().number("Burnin number ", Counter, ":");

	_runBurninIterations(Iterations, "Running a burnin of " + coretools::str::toString(Iterations) + " iterations");
	_finalizeBurnin();
}

void TMCMC::_burninHasFinished() { instances::dagBuilder().burninHasFinished(); }

void TMCMC::_MCMCHasFinished() {
	instances::dagBuilder().MCMCHasFinished();
	instances::dagBuilder().closeAllMCMCFiles();
}

void TMCMC::_runMCMCChain() {
	std::string report = "Running an MCMC chain of " + coretools::str::toString(_iterations) + " iterations";
	coretools::TProgressReporter prog(_iterations, report);

	for (size_t i = 0; i < _iterations; ++i) {
#ifdef USE_RCPP
		if (i % 1000 == 0) { Rcpp::checkUserInterrupt(); }
#endif
		_runMCMCIterationFull(i);
		_writeToFiles(i);
		prog.next();
	}
	prog.done();

	// write final iteration to state file
	if (_writeStateFile) { _writeToStateFile(_iterations - 1); }

	// report acceptance rates
	_reportAcceptanceRates();
	_writeToMeanVarFiles();
}

void TMCMC::_runMCMCIterationFull(size_t Iteration) { instances::dagBuilder().updateParameters_MCMC(Iteration); }

void TMCMC::_reportAcceptanceRates() { instances::dagBuilder().reportAcceptanceRates(); }

void TMCMC::_adjustProposalRanges() { instances::dagBuilder().adjustProposalRanges(); }

void TMCMC::_writeToTraceFiles() { instances::dagBuilder().writeToTraceFiles(); }

void TMCMC::_writeToMeanVarFiles() { instances::dagBuilder().writeToMeanVarFiles(); }

void TMCMC::_writeToStateFile(size_t Iterations) { instances::dagBuilder().writeToStateFile(Iterations); }

//-------------------------------------------
// TSimulator
//-------------------------------------------

void TSimulator::simulate() {
	// simulate
	logfile().startIndent("Simulating under prior distribution:");

	instances::dagBuilder().buildDAG();
	instances::dagBuilder().simulate();
	instances::dagBuilder().writeSimulationFiles();

	logfile().endIndent();
}

} // end namespace stattools
