//
// Created by madleina on  01.02.21.
//

#include "stattools/DAG/TDAGBuilder.h" // for TMCMCUserInterface::_parseCom...
#include <algorithm> // for max, find

#include "coretools/Files/TInputFile.h"
#include "coretools/Files/TOutputFile.h"
#include "coretools/Main/TLog.h"                          // for TLog
#include "coretools/Main/TParameters.h"                   // for TParameters
#include "coretools/Strings/fromString.h"                 // for convertString, convertStringC...
#include "stattools/MCMC/TMCMCFiles.h"                    // for TMCMCFile, TMCMCPosteriorTrac...
#include "stattools/ParametersObservations/TDefinition.h" // for TParameterDefinition, TObserv...
#include "stattools/ParametersObservations/TNodeBase.h"   // for TObservationBase, TParameterBase
#include "stattools/Updates/TUpdate.h"

namespace stattools {

using namespace coretools::instances;
using namespace coretools::str;

//--------------------------------------------
// TMCMCUserInterface
//--------------------------------------------

void TMCMCUserInterface::parseUserConfiguration(std::vector<TParameterBase *> &Parameters,
												std::vector<TObservationBase *> &Observations) {
	// first read parameter configuration file
	_readParamConfigFile(Parameters, Observations);
	// then read file with initial values and jump sizes
	_readInitValFile(Parameters, Observations);
	// now check if user specified any command line arguments concerning parameter configurations / initial values
	_parseCommandLineParamConfigs(Parameters, Observations);
	_parseCommandLineParamInitVals(Parameters, Observations);
}

void TMCMCUserInterface::_checkHeaderConfigFile(std::string_view Filename,
												coretools::TConstView<std::string> ActualColNames) {
	// keep track of which colnames have been parsed -> check for duplicate colnames
	std::vector<std::string> parsedColNames;

	for (auto &colName : ActualColNames) {
		// is the given colname valid?
		auto it = std::find(_expectedColNames.begin(), _expectedColNames.end(), colName);
		if (it != _expectedColNames.end()) { // colname is valid
			// did we parse this colname already (i.e. is it a duplicate)?
			auto it2 = std::find(parsedColNames.begin(), parsedColNames.end(), colName);
			if (it2 != parsedColNames.end()) // it is a duplicate -> throw
				UERROR("Duplicate colname '", colName, "' in config file '", Filename, "'!");
			parsedColNames.push_back(colName);
		} else {
			// colName is different from what is expected
			UERROR("Invalid colname '", colName, "' in config file '", Filename, "'!");
		}
	}

	// check if column with "name" exists
	auto it = std::find(parsedColNames.begin(), parsedColNames.end(), "name");
	if (it == parsedColNames.end()) { UERROR("Mandatory colname 'name' is missing in file '", Filename, "'!"); }
}

std::string getErrorStringObservation(std::string_view Name, const std::string &What) {
	return "Error while reading config file: Can not set " + What + " for observation '" + (std::string)Name +
		   "'. Observations are not updated.";
}

void TMCMCUserInterface::_matchConfig(TObservationDefinition &ObservationDefinition, std::string_view Name,
									  std::string_view Config, std::string_view Val) {
	// function to match Configurations of observations
	// user can modify: prior CommandLineArgs, simulationFile.
	// if user attempts to modify attributes that are not defined for observations (e.g. trace file, isUpdated, proposal
	// kernels etc.), an error is thrown
	size_t i = 1;
	if (Config == _expectedColNames.at(i++)) { // prior parameters (only fixed)
		if (Val.empty()) { return; }
		ObservationDefinition.setPriorParameters(Val);
	} else if (Config == _expectedColNames.at(i++)) { // trace
		UERROR(getErrorStringObservation(Name, "trace file"));
	} else if (Config == _expectedColNames.at(i++)) { // meanVar
		UERROR(getErrorStringObservation(Name, "meanVar file"));
	} else if (Config == _expectedColNames.at(i++)) { // state posterior file
		UERROR(getErrorStringObservation(Name, "state posterior file"));
	} else if (Config == _expectedColNames.at(i++)) { // posterior mode file
		UERROR(getErrorStringObservation(Name, "posterior mode file"));
	} else if (Config == _expectedColNames.at(i++)) { // simulationFile
		ObservationDefinition.editFile(MCMCFiles::simulation, Val);
	} else if (Config == _expectedColNames.at(i++)) { // update
		if (Val.empty()) { return; }
		UERROR(getErrorStringObservation(Name, "update"));
	} else if (Config == _expectedColNames.at(i++)) { // propKernel
		if (Val.empty()) { return; }
		UERROR(getErrorStringObservation(Name, "proposal kernel"));
	} else if (Config == _expectedColNames.at(i++)) { // sharedJumpSize
		if (Val.empty()) { return; }
		UERROR(getErrorStringObservation(Name, "shared jump size"));
	}
}

void TMCMCUserInterface::_matchConfig(TParameterDefinition &ParameterDefinition, std::string_view,
									  std::string_view Config, std::string_view val) {
	size_t i = 1;
	if (Config == _expectedColNames.at(i++)) { // prior CommandLineArgs (only fixed)
		if (val.empty()) { return; }
		ParameterDefinition.setPriorParameters(val);
	} else if (Config == _expectedColNames.at(i++)) { // trace
		ParameterDefinition.editFile(MCMCFiles::trace, val);
	} else if (Config == _expectedColNames.at(i++)) { // meanVar
		ParameterDefinition.editFile(MCMCFiles::meanVar, val);
	} else if (Config == _expectedColNames.at(i++)) { // statePosteriors
		ParameterDefinition.editFile(MCMCFiles::statePosteriors, val);
	} else if (Config == _expectedColNames.at(i++)) { // posterior mode
		ParameterDefinition.editFile(MCMCFiles::posteriorMode, val);
	} else if (Config == _expectedColNames.at(i++)) { // simulation
		ParameterDefinition.editFile(MCMCFiles::simulation, val);
	} else if (Config == _expectedColNames.at(i++)) { // update
		if (val.empty()) { return; }
		ParameterDefinition.update(fromString<bool, true>(val));
	} else if (Config == _expectedColNames.at(i++)) { // propKernel
		if (val.empty()) { return; }
		ParameterDefinition.setPropKernel(val);
	} else if (Config == _expectedColNames.at(i++)) { // sharedJumpSize
		if (val.empty()) { return; }
		ParameterDefinition.setJumpSizeForAll(fromString<bool, true>(val));
	}
}

void TMCMCUserInterface::_parseParamConfigurations(std::vector<TParameterBase *> &Parameters,
												   std::vector<TObservationBase *> &Observations,
												   const std::vector<std::string_view> &line) {
	// get definition with this name
	size_t colWithName = _configFile.index("name");
	std::string name{line.at(colWithName)};
	// search inside parameter definitions
	if (!_parseParamConfigurations(Parameters, name, line)) {
		if (!_parseParamConfigurations(Observations, name, line)) {
			UERROR("Error while parsing config file " + _configFile.name() +
					   ": No parameter or observation with name '",
				   name, "' exists!");
		}
	}
}

void TMCMCUserInterface::_parseInitVals(std::vector<TParameterBase *> &Parameters,
										std::vector<TObservationBase *> &Observations,
										coretools::TInputMaybeRcppFile &File) {
	// search inside parameter definitions for this name
	for (auto param : Parameters) {
		// found matching name!
		if (param->name() == File.get(0)) {
			auto &def = param->getDefinition();
			// set initial value and jumpSize
			if (!File.get(1).empty()) { def.setInitVal(File.get(1)); }
			if (!File.get(2).empty()) { def.setInitJumpSizeProposal(File.get(2)); }
			return;
		}
	}
	// search inside observation definitions for this name
	for (auto obs : Observations) {
		// found matching definition -> throw, because we can not set initial values for observations -> design choice,
		// change if needed
		if (obs->name() == File.get(0)) { UERROR("Can not set initial values for observation '" + obs->name() + "'!"); }
	}
	UERROR("No parameter with name '", File.get(0), "' exists!");
}

void TMCMCUserInterface::_readInitValFile(std::vector<TParameterBase *> &Parameters,
										  std::vector<TObservationBase *> &Observations) {
	const std::string fileNameInitVal = parameters().get("initVals", "");
	if (fileNameInitVal.empty()) { return; } // file not given -> no need to do anything

	// file with 3 cols: name of parameter, initial value and initial jump size. Must have this order
	logfile().listFlush("Reading initial values and jumpSizes from file '" + fileNameInitVal + "'...");
	coretools::TInputMaybeRcppFile file(fileNameInitVal, coretools::FileType::Header);
	if (file.numCols() != 3)
		UERROR("Wrong format of file '" + fileNameInitVal + "': expected 3 columns, detected " +
			   toString(file.numCols()) + "!");
	if (file.header()[1] != "value")
		UERROR("Wrong format of file '" + fileNameInitVal +
			   "': column 2 must contain initial values and colname must be 'value'!");
	if (file.header()[2] != "jumpSize")
		UERROR("Wrong format of file '" + fileNameInitVal +
			   "': column 3 must contain initial jumpSizes and colname must be 'jumpSize'!");

	for (; !file.empty(); file.popFront()) {
		// parse line (= one parameter)
		_parseInitVals(Parameters, Observations, file);
	}
	logfile().done();
}

void TMCMCUserInterface::_readParamConfigFile(std::vector<TParameterBase *> &Parameters,
											  std::vector<TObservationBase *> &Observations) {
	const std::string filename = parameters().get("config", "");
	if (filename.empty()) // file not given -> no need to do anything
		return;
	logfile().listFlush("Reading parameter configurations from file '" + filename + "'...");
	_configFile.open(filename, coretools::FileType::Header);

	// check if colnames are ok
	_checkHeaderConfigFile(_configFile.name(), _configFile.header());

	for (; !_configFile.empty(); _configFile.popFront()) {
		// parse line (= one parameter)
		_parseParamConfigurations(Parameters, Observations, _configFile.front());
	}
	logfile().done();
}

void TMCMCUserInterface::_parseCommandLineParamConfigs(std::vector<TParameterBase *> &Parameters,
													   std::vector<TObservationBase *> &Observations) {
	// go over all definitions
	_parseCommandLineParamConfigs(Parameters);
	_parseCommandLineParamConfigs(Observations);
}

void TMCMCUserInterface::_parseCommandLineParamInitVals(std::vector<TParameterBase *> &Parameters,
														std::vector<TObservationBase *> &Observations) {
	// go over all parameter definitions
	for (auto param : Parameters) {
		// construct name
		std::string argName = param->name();
		// check if argument with this name is given on command line
		if (parameters().exists(argName)) {
			// yes, it is given -> get corresponding value
			auto val = parameters().get<std::string>(argName);
			logfile().list("Setting the initial values of parameter ", param->name(),
						   " from the command line. (parameter ", argName, ")");
			// set initial value and jumpSize
			param->getDefinition().setInitVal(val);
		}
		// construct name
		argName = param->name() + "." + "jumpSize";
		// check if argument with this name is given on command line
		if (parameters().exists(argName)) {
			// yes, it is given -> get corresponding value
			auto val = parameters().get<std::string>(argName);
			logfile().list("Setting the initial jump sizes of parameter ", param->name(),
						   " from the command line. (parameter ", argName, ")");
			// set initial value and jumpSize
			param->getDefinition().setInitJumpSizeProposal(val);
		}
	}
	// go over all observation definitions
	for (auto obs : Observations) {
		std::string argName = obs->name();
		if (parameters().exists(argName)) {
			// found matching definition -> throw, because we can not set initial values for observations -> design
			// choice, change if needed
			UERROR("Can not set initial values for observation '" + obs->name() + "' from command line!");
		}
		argName = obs->name() + "." + "jumpSize";
		if (parameters().exists(argName)) {
			UERROR("Can not set initial jump sizes for observation '" + obs->name() + "' from command line!");
		}
	}
}

void TMCMCUserInterface::writeAllConfigurationsToFile(std::string_view OutName,
													  std::vector<TParameterBase *> &Parameters,
													  std::vector<TObservationBase *> &Observations) {
	coretools::TOutputFile file(std::string{OutName}.append("_config.txt"), _expectedColNames);

	// write all configurations for each observation (leave fields that do not apply to observations empty)
	for (auto obs : Observations) {
		auto &def = obs->getDefinition();
		file.writeln(obs->name(), def.priorParameters(), "", "", "", "", "", "", "", "");
	}

	// write all configurations for each parameter
	for (auto param : Parameters) {
		auto &def = param->getDefinition();
		file.writeln(param->name(), def.priorParameters(), def.getPrefix(MCMCFiles::trace),
					 def.getPrefix(MCMCFiles::meanVar), def.getPrefix(MCMCFiles::statePosteriors),
					 def.getPrefix(MCMCFiles::posteriorMode), def.getPrefix(MCMCFiles::simulation), def.isUpdated(),
					 ProposalKernel::proposalKernelToString(def.propKernel()), def.oneJumpSizeForAll());
	}
}

//-------------------------------------------
// TDAGBuilder
//-------------------------------------------

void TDAGBuilder::_checkForUniqueNames(std::string_view Name) const {
	for (const auto &param : _allParametersAndObservations) {
		if (param->name() == Name) {
			DEVERROR("Parameter or observation with name '", Name,
					 "' already exists! Please provide unique names for parameters/observations.");
		}
	}
}

void TDAGBuilder::addToDAG(TParameterBase *Parameter) {
	// do we already have a parameter/observation with this name?
	_checkForUniqueNames(Parameter->name());
	_allParameters.emplace_back(Parameter);
	_allParametersAndObservations.emplace_back(Parameter);
}

void TDAGBuilder::addToDAG(TObservationBase *Observation) {
	// do we already have a parameter/observation with this name?
	_checkForUniqueNames(Observation->name());
	_allObservations.emplace_back(Observation);
	_allParametersAndObservations.emplace_back(Observation);
}

void TDAGBuilder::addExtraUpdater(TUpdateBase *Updater) {
	// usually not needed, only if a parameter has an extra proposal kernel that is not managed by itself
	_allUpdaters.push_back(Updater);
}

void TDAGBuilder::addFuncToUpdate(const std::function<void()> &Func) { _dag.addFuncToUpdate(Func); }

void TDAGBuilder::_checkForValidDAG() const {
	// 1) we need at least one observation
	if (_allObservations.empty()) { DEVERROR("Not a valid DAG! Need at least 1 observation."); }

	// 2) a parameter can not be at the bottom of a DAG
	for (auto it : _allParameters) {
		if (!it->isPartOfBox()) {
			DEVERROR("Not a valid DAG! A parameter (" + it->name() + ") can not be at the bottom of a DAG.");
		}
	}
}

void TDAGBuilder::buildDAG() {
	// 1) parse user CommandLineArgs
	TMCMCUserInterface userInterface;
	userInterface.parseUserConfiguration(_allParameters, _allObservations);

	// 2) initialize DAG
	_checkForValidDAG();
	TDAG temporaryDAG;
	for (auto it : _allObservations) {
		if (!it->isPartOfBox()) { // observation is at bottom of DAG
			it->constructDAG(_dag, temporaryDAG);
		}
	}

	// 3) fill nodes for updating
	_dag.fillNodesToUpdate(_allParameters);

	// 4) initialize storage
	_dag.initializeStorage();

	// 5) get all ptrs to updaters
	_allUpdaters.reserve(_allParameters.size());
	for (auto it : _allParameters) { _allUpdaters.push_back(it->getPtrToUpdater()); }
}

void TDAGBuilder::_prepareStateFiles(std::string_view OutName) {
	if (_writeStateFile) {
		_stateFile = std::make_unique<TMCMCStateFile>(std::string{OutName}.append("_state.txt"));
		// add all parameters to state file (but not observations! These are fix, no sense to write them into state
		// file)
		for (auto it : _allParameters) { _stateFile->add(it); }
	}
}

void TDAGBuilder::_prepareDensitiesTraceFile(std::string_view OutName) {
	if (_writeDensityTrace) {
		_densitiesTraceFile = std::make_unique<TMCMCDensitiesTraceFile>(std::string{OutName}.append("_densities.txt"));
		_densitiesTraceFile->setPrecision(20);
		// add all parameters and observations to densities trace file
		for (auto it : _allObservations) { _densitiesTraceFile->add(it); }
		for (auto it : _allParameters) { _densitiesTraceFile->add(it); }
		_densitiesTraceFile->writeHeader();
	}
}

void TDAGBuilder::prepareAllMCMCFiles(bool WriteStateFile, bool WritePosteriorTrace, std::string_view OutName) {
	_writeStateFile    = WriteStateFile;
	_writeDensityTrace = WritePosteriorTrace;

	// make files ready (initialize, write header etc.)
	_prepareFiles(_traceFiles, MCMCFiles::trace);
	_prepareFiles(_meanVarFiles, MCMCFiles::meanVar);
	_prepareFiles(_statePosteriorsFiles, MCMCFiles::statePosteriors);
	_prepareFiles(_posteriorModeFiles, MCMCFiles::posteriorMode);
	_prepareStateFiles(OutName);
	_prepareDensitiesTraceFile(OutName);

	// finally: write a file where all parameter configurations are listed for each parameter
	TMCMCUserInterface userInterface;
	userInterface.writeAllConfigurationsToFile(OutName, _allParameters, _allObservations);
}

void TDAGBuilder::burninHasFinished() {
	_dag.tellBoxAboveThatBurninFinished();
	for (auto &it : _allParameters) { it->clearMeanVar(); }
}

void TDAGBuilder::MCMCHasFinished() { _dag.tellBoxAboveThatMCMCFinished(); }

void TDAGBuilder::reportAcceptanceRates() {
	for (auto &it : _allUpdaters) { it->printAccRateToLogfile(); }
}

void TDAGBuilder::adjustProposalRanges() {
	for (auto &it : _allUpdaters) { it->adjustProposalWidth(); }
	for (auto &it : _allUpdaters) { it->clear(); }
}

void TDAGBuilder::writeToTraceFiles() {
	for (auto &it : _traceFiles) { it->write(); }
	if (_writeDensityTrace) { _densitiesTraceFile->write(); }
}

void TDAGBuilder::writeToMeanVarFiles() {
	for (auto &it : _meanVarFiles) { it->write(); }
	for (auto &it : _statePosteriorsFiles) { it->write(); }
	for (auto &it : _posteriorModeFiles) { it->write(); }
}

void TDAGBuilder::writeToStateFile(size_t Iterations) {
	if (_writeStateFile) _stateFile->write(Iterations);
}

void TDAGBuilder::closeAllMCMCFiles() {
	for (auto &file : _traceFiles) { file->close(); }
	for (auto &file : _meanVarFiles) { file->close(); }
	for (auto &file : _statePosteriorsFiles) { file->close(); }
	for (auto &file : _posteriorModeFiles) { file->close(); }
	if (_writeStateFile) { _stateFile->close(); }
	if (_writeDensityTrace) { _densitiesTraceFile->close(); }
	_traceFiles.clear();
	_meanVarFiles.clear();
	_statePosteriorsFiles.clear();
	_posteriorModeFiles.clear();
	_stateFile.reset();
	_densitiesTraceFile.reset();
}

void TDAGBuilder::guessInitialValues() { _dag.guessInitialValues(_allParameters); }

void TDAGBuilder::updateParameters_MCMC(size_t Iteration) { _dag.update(Iteration); }

void TDAGBuilder::simulate() { _dag.simulate(_allParameters); }

void TDAGBuilder::writeSimulationFiles() {
	_prepareFiles(_simulationFiles, MCMCFiles::simulation);

	for (auto &it : _simulationFiles) {
		it->write();
		it->close();
	}
	_simulationFiles.clear();
}

void TDAGBuilder::clear() {
	_allParametersAndObservations.clear();
	_allParameters.clear();
	_allObservations.clear();
	_allUpdaters.clear();
	_dag.clear();

	_traceFiles.clear();
	_meanVarFiles.clear();
	_statePosteriorsFiles.clear();
	_posteriorModeFiles.clear();
	_simulationFiles.clear();
	_stateFile.reset();
	_densitiesTraceFile.reset();

	_writeStateFile    = false;
	_writeDensityTrace = false;
}

const std::vector<TNodeBase *> &TDAGBuilder::getAllParametersAndObservations() { return _allParametersAndObservations; }

const std::vector<TParameterBase *> &TDAGBuilder::getAllParameters() { return _allParameters; }

}; // end namespace stattools
