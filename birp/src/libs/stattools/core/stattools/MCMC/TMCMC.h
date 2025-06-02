//
// Created by Madleina Caduff on 03.04.20.
//

#ifndef TMCMC_H
#define TMCMC_H

#include <cstddef>
#include <string_view>


namespace stattools {
class TDAGBuilder;

//-------------------------------------------
// TMCMC
//-------------------------------------------
class TMCMC {
protected:
	// about MCMC chain
	size_t _iterations = 100000;
	size_t _thinning   = 0;

	// about state file
	bool _writeStateFile      = false;
	size_t _thinningStateFile = 0;

	// about densities trace file
	bool _writeDensitiesTraceFile = false;

	// about standard burnin
	size_t _iterationsPerBurnin = 1000;
	size_t _numBurnin           = 10;
	bool _writeBurnin           = false;

	// read command line arguments
	void _readCommandLineArgs();
	void _readBurninParameters();
	void _readThinningParameters();
	void _readStateFileParameters();
	void _readMCMCChainParameters();

	// initialize
	void _runPriorParameterInitialization();

	// files
	void _prepareFiles(std::string_view OutName);
	static void _writeToTraceFiles();
	static void _writeToMeanVarFiles();
	static void _writeToStateFile(size_t iterations);
	void _writeToFiles(size_t Iteration);
	void _writeInitialValues();

	// burnin
	void _runBurnin(size_t Counter, size_t Iterations);
	void _runFixedBurnin();
	void _reportAcceptanceRates();
	void _adjustProposalRanges();
	void _runBurninIterations(size_t NumIterations, std::string_view ProgressString);
	void _finalizeBurnin();

	// MCMC
	static void _burninHasFinished();
	void _MCMCHasFinished();
	void _runMCMC();
	void _runMCMCChain();
	static void _runMCMCIterationFull(size_t Iteration);

public:
	TMCMC();
	TMCMC(size_t NumIterations, size_t NumBurnin, size_t NumBurninIterations);
	// run MCMC
	void runMCMC(std::string_view OutName);
	void setUp(std::string_view OutName);
};

//-------------------------------------------
// TSimulator
//-------------------------------------------
struct TSimulator {
public:
	static void simulate();
};

} // end namespace stattools

#endif // TMCMC_H
