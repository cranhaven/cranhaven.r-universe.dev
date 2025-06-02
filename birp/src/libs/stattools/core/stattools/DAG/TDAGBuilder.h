//
// Created by madleina on 01.02.21.
//

#ifndef TDAGBUILDER_H
#define TDAGBUILDER_H

#include <memory>
#include <string>
#include <vector>
#include <functional>

#include "coretools/Main/TParameters.h"
#include "stattools/DAG/TDAG.h"
#include "stattools/MCMC/TMCMCFiles.h"
#include "stattools/ParametersObservations/TNodeBase.h"

namespace stattools {

//-------------------------------------------
// TMCMCUserInterface
//-------------------------------------------

class TMCMCUserInterface {
protected:
	coretools::TInputFile _configFile;

	std::vector<std::string> _expectedColNames = {
	    "name",           "priorParameters", "traceFile",  "meanVarFile",   "statePosteriorsFile", "posteriorModeFile",
	    "simulationFile", "update",          "propKernel", "sharedJumpSize"};

	// open and read files
	void _readParamConfigFile(std::vector<TParameterBase *> &Parameters, std::vector<TObservationBase *> &Observations);
	void _readInitValFile(std::vector<TParameterBase *> &Parameters, std::vector<TObservationBase *> &Observations);

	// parse one line
	void _checkHeaderConfigFile(std::string_view Filename, coretools::TConstView<std::string> ActualColNames);
	void _parseParamConfigurations(std::vector<TParameterBase *> &Parameters,
	                               std::vector<TObservationBase *> &Observations,
	                               const std::vector<std::string_view> &line);
	void _parseInitVals(std::vector<TParameterBase *> &Parameters, std::vector<TObservationBase *> &Observations,
	                    coretools::TInputMaybeRcppFile &File);

	// parse command line CommandLineArgs
	void _parseCommandLineParamConfigs(std::vector<TParameterBase *> &Parameters,
	                                   std::vector<TObservationBase *> &Observations);
	void _parseCommandLineParamInitVals(std::vector<TParameterBase *> &Parameters,
	                                    std::vector<TObservationBase *> &Observations);
	template<typename ParameterOrObservation>
	bool _parseParamConfigurations(ParameterOrObservation &ParamOrObs, std::string_view Name,
	                               const std::vector<std::string_view> &Line) {
		for (auto &it : ParamOrObs) {
			// found matching definition!
			if (it->name() == Name) {
				// now go over all configurations of this parameter
				size_t c = 0;
				for (auto &col : Line) {
					_matchConfig(it->getDefinition(), it->name(), _configFile.header()[c], col);
					c++;
				}
				return true;
			}
		}
		return false;
	}

	template<typename ParameterOrObservation> void _parseCommandLineParamConfigs(ParameterOrObservation &ParamOrObs) {
		for (auto &it : ParamOrObs) {
			// go over all parameter configurations
			for (auto &config : _expectedColNames) {
				// construct name
				std::string argName = it->name() + "." + config;
				// check if argument with this name is given on command line
				if (coretools::instances::parameters().exists(argName)) {
					// yes, it is given -> get corresponding value
					auto val = coretools::instances::parameters().get<std::string>(argName);
					coretools::instances::logfile().list("Setting the ", config, " of parameter ", it->name(),
					                                     " from the command line. (parameter ", argName, ")");
					_matchConfig(it->getDefinition(), it->name(), config, val);
				}
			}
		}
	}

	// match single configuration
	void _matchConfig(TParameterDefinition &ParameterDefinition, std::string_view Name, std::string_view config,
	                  std::string_view val);
	void _matchConfig(TObservationDefinition &ObservationDefinition, std::string_view Name, std::string_view config,
	                  std::string_view val);

public:
	void parseUserConfiguration(std::vector<TParameterBase *> &Parameters,
	                            std::vector<TObservationBase *> &Observations);
	void writeAllConfigurationsToFile(std::string_view Filename, std::vector<TParameterBase *> &Parameters,
	                                  std::vector<TObservationBase *> &Observations);
};

//-------------------------------------------
// TDAGBuilder
//-------------------------------------------

class TDAGBuilder {
protected:
	// declare friend classes
	// user that constructs a TDAGBuilder will only see relevant functions, but within mcmcframework, we can still
	// access maps, dag etc.
	friend class TMCMC;
	friend struct TSimulator;

	// initialized CommandLineArgs and observations
	std::vector<TNodeBase *> _allParametersAndObservations;
	std::vector<TParameterBase *> _allParameters;
	std::vector<TObservationBase *> _allObservations;
	std::vector<TUpdateBase *> _allUpdaters;
	TDAG _dag;

	// files
	std::vector<std::unique_ptr<TMCMCTraceFile>> _traceFiles;
	std::vector<std::unique_ptr<TMCMCMeanVarFile>> _meanVarFiles;
	std::vector<std::unique_ptr<TMCMCStatePosteriorsFile>> _statePosteriorsFiles;
	std::vector<std::unique_ptr<TMCMCPosteriorModeFile>> _posteriorModeFiles;
	std::vector<std::unique_ptr<TMCMCSimulationFile>> _simulationFiles;
	std::unique_ptr<TMCMCStateFile> _stateFile;
	std::unique_ptr<TMCMCDensitiesTraceFile> _densitiesTraceFile;
	bool _writeStateFile    = false;
	bool _writeDensityTrace = false;

	// assemble parameter definitions
	void _checkForUniqueNames(std::string_view name) const;
	void _checkForValidDAG() const;

	// files
	void _prepareStateFiles(std::string_view NameStateFile);
	void _prepareDensitiesTraceFile(std::string_view NameDensitiesTraceFile);

	template<typename T, typename PtrType>
	void _bundleParameterFiles(PtrType *ParamOrObs, MCMCFiles Type, std::vector<std::unique_ptr<T>> &FileVec) {
		if (!ParamOrObs->getDefinition().writesFile(Type)) { return; }

		// go over existing files -> does file already exist?
		std::string prefix     = ParamOrObs->getDefinition().getPrefix(Type);
		const std::string name = prefix + "_" + MCMCFileToString(Type) + ".txt"; // e.g. myParams_trace.txt
		for (auto &it : FileVec) {
			if (it->name() == name) {
				it->add(ParamOrObs);
				return;
			}
		}
		FileVec.emplace_back(std::make_unique<T>(name, ParamOrObs)); // new file
	}

	template<typename T> void _prepareFiles(std::vector<std::unique_ptr<T>> &Vec, MCMCFiles Type) {
		// parameter definitions: search for trace, meanVar, state posterior and simulation files
		for (auto param : _allParameters) { _bundleParameterFiles(param, Type, Vec); }
		// observation definitions: search for simulation files only
		if (Type == MCMCFiles::simulation) {
			for (auto obs : _allObservations) { _bundleParameterFiles(obs, Type, Vec); }
		}

		// write header
		for (auto &it : Vec) { it->writeHeader(); }
	}

public:
	// add parameter and observation pointers to DAG
	void addToDAG(TParameterBase *Parameter);
	void addToDAG(TObservationBase *Observation);
	void addExtraUpdater(TUpdateBase *Update);

	// add functions to DAG that should be called for updating (used if update should not run through nodes)
	void addFuncToUpdate(const std::function<void()> &Func);
	template<class Object, class Func> void addFuncToUpdate(Object &Obj, Func &Fun) {
		const auto fun = [&Obj, &Fun]() { return (Obj.*Fun)(); };
		addFuncToUpdate(fun);
	}

	// build DAG
	void buildDAG();

	// functions called from MCMC / Simulator
	void prepareAllMCMCFiles(bool WriteStateFile, bool WritePosteriorTrace, std::string_view OutName);
	void burninHasFinished();
	void MCMCHasFinished();
	void reportAcceptanceRates();
	void adjustProposalRanges();
	void writeToTraceFiles();
	void writeToMeanVarFiles();
	void writeToStateFile(size_t Iterations);
	void closeAllMCMCFiles();
	void guessInitialValues();
	void updateParameters_MCMC(size_t Iteration);
	void simulate();
	void writeSimulationFiles();

	// clear DAG
	void clear();

	// additional get functions
	const std::vector<TNodeBase *> &getAllParametersAndObservations();
	const std::vector<TParameterBase *> &getAllParameters();

	// for debugging
	const auto &getTraceFiles() const { return _traceFiles; }
	const auto &getMeanVarFiles() const { return _meanVarFiles; }
	const auto &getStateFile() const { return _stateFile; }
	const TDAG &getDAG() const { return _dag; }
};

namespace instances {
inline TDAGBuilder &dagBuilder() {
	static TDAGBuilder dagBuilder;
	return dagBuilder;
}
} // namespace instances

} // end namespace stattools

#endif // TDAGBUILDER_H
