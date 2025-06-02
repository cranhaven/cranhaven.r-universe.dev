#include "TBirpCore.h"
#include "TData.h"
#include "coretools/Types/strongTypes.h"
#include "stattools/MCMC/TMCMC.h"
#include "stattools/Priors/TPriorNormal.h"
#include <set>

using namespace coretools::instances;
using namespace stattools;
using namespace stattools::prior;

//--------------------------------------
// TBirpCore
//--------------------------------------

TBirpCore::TBirpCore() {
	using namespace coretools::instances;
	if (parameters().exists("out")) {
		_outname = parameters().get<std::string>("out");
		logfile().list("Will write files with prefix '", _outname, "'. (argument 'out')");
	} else {
		_outname = "birp";
		logfile().list("Will write files with default prefix '", _outname, "' (use argument 'out' to change).");
	}
}

void TBirpCore::_checkIfAllCountsZero() {
	size_t sumNonZero = 0;
	for (size_t i = 0; i < _data.size(); ++i) {
		for (size_t j = 0; j < _data[i].size(); ++j) {
			for (size_t k = 0; k < _data[i][j].size(); ++k) {
				if (_data[i][j][k].counts_per_species(0) > 0) { sumNonZero++; }
			}
		}
	}
	if (sumNonZero == 0) { UERROR("All counts are zero! Can not infer trends."); }
}

void TBirpCore::_readData() {
	// get all input filenames (one per method)
	std::string input                  = parameters().get("data");
	std::vector<std::string> filenames = _getAllFilenames(input);

	// read files (one per method)
	for (const auto &name : filenames) { _readFile(name); }
	if (_data.size() == 0) { UERROR("No method passes filters!"); }

	// check if all counts are zero
	_checkIfAllCountsZero();

	// sort and finalize
	_sortTimepoints();
	_data.fillMethLocIndices(_speciesNames.size(), _locations, _CIGroupNames);

	logfile().endIndent();
}

void TBirpCore::_clear() {
	_outname = "";

	// locations, methods, timepoints
	_locations.clear();
	_timepoints.clear();
	_CIGroupNames.clear();
	_covariateEffortNames.clear();
	_covariateDetectionNames.clear();
	_speciesNames.clear();

	_data.clear();
}

//-----------------------------------
// Get all filenames

std::string getDelim(std::string_view Filename) {
	std::string delim       = "\t";
	std::string_view ending = coretools::str::readAfterLast(Filename, '.');
	if (ending == "csv") { delim = ","; }
	return delim;
}

std::vector<std::string> readFilenamesFromFile(std::string_view Filename) {
	// open file. Can be either:
	// 1) the file containing the counts for a single method
	// 2) a file containing all filenames for multiple methods
	coretools::TInputMaybeRcppFile file(Filename, coretools::FileType::Header, getDelim(Filename));

	if (file.header().size() != 1) { // case 1: directly counts file
		return {(std::string)Filename};
	}
	// case 2: all filenames
	// open again without header
	coretools::TInputMaybeRcppFile file2(Filename, coretools::FileType::NoHeader, getDelim(Filename));
	std::vector<std::string> filenames;
	for (; !file2.empty(); file2.popFront()) { filenames.emplace_back(file2.get(0)); }
	return filenames;
}

std::vector<std::string> TBirpCore::_getAllFilenames(std::string_view Input) {
	// read --data -> this can be:
	// 1) the file containing the counts for a single method
	// 2) a file containing all filenames for multiple methods
	// 3) a comma-separated list of filenames for multiple methods
	logfile().startIndent("Will read input data from ", Input, " (argument 'data').");

	std::vector<std::string> filenames;
	if (std::filesystem::exists(Input)) { // is a file
		filenames = readFilenamesFromFile(Input);
	} else {
		coretools::str::fillContainerFromString(Input, filenames, ',');
	}

	if (filenames.empty()) { UERROR("Provided filenames are empty (argument 'data')."); }

	return filenames;
}

//-----------------------------------
// Read one method's file

std::vector<std::string> getHeader() { return {"location", "timepoint"}; }

void checkHeader(const coretools::TInputMaybeRcppFile &Infile, const std::vector<size_t> &countIndices,
                 const std::vector<size_t> &CovEffortIndices) {
	std::vector<std::string> headerVec = getHeader();

	// make sure location and timepoint are included in header
	for (const auto &colName : getHeader()) {
		if (std::find(Infile.header().begin(), Infile.header().end(), colName) == Infile.header().end()) {
			UERROR("Error in header of file ", Infile.name(), ": Mandatory column '", colName, "' is missing.");
		}
	}

	// make sure there is at least one column starting with "covEffort" and counts
	if (CovEffortIndices.empty()) {
		UERROR("Error in header of file ", Infile.name(),
		       ": Need at least one column starting with 'covEffort' or 'effort'.");
	}
	if (countIndices.empty()) {
		UERROR("Error in header of file ", Infile.name(), ": Need at least one column starting with 'counts'.");
	}
}

std::vector<size_t> getColumnIndex(const coretools::TInputMaybeRcppFile &infile, const std::string &string) {
	std::vector<size_t> ix;
	for (size_t i = 0; i < infile.numCols(); i++) {
		if (coretools::str::stringStartsWith(infile.header()[i], string)) { ix.push_back(i); }
	}
	return ix;
}

std::vector<std::string> getSpeciesOrCovariateNames(const coretools::TInputMaybeRcppFile &infile,
                                                    const std::vector<size_t> &Indices) {
	std::vector<std::string> names(Indices.size());
	for (size_t s = 0; s < Indices.size(); s++) {
		names[s] = coretools::str::readAfter(infile.header()[Indices[s]], '_');
	}
	return names;
}

std::vector<size_t> fillCovNames(coretools::TInputMaybeRcppFile &infile, const std::vector<size_t> &Indices,
                                 TUniqueContainer<std::string> &UniqueContainer) {
	std::vector<std::string> covNames = getSpeciesOrCovariateNames(infile, Indices);
	std::vector<size_t> cov_ids(covNames.size());
	for (size_t c = 0; c < covNames.size(); ++c) { cov_ids[c] = UniqueContainer.add(covNames[c]); }
	return cov_ids;
}

std::string getMethodName(const std::string &Filename) {
	std::string_view methodNameFull = Filename;
	if (coretools::str::stringContains(Filename, '/')) {
		methodNameFull = coretools::str::readAfterLast(Filename, '/');
	}
	std::string methodName = (std::string)coretools::str::readBeforeLast(methodNameFull, '.');
	coretools::str::eraseAllOccurences(methodName, "_simulatedCounts");
	return methodName;
}

auto getIndicesMethodFile(const coretools::TInputMaybeRcppFile &Infile) {
	// get position of columns that contain counts: might be an empty vector in case the file has no counts
	std::vector<size_t> countIndices = getColumnIndex(Infile, "count");

	// get position of columns that contain covariates (efforts)
	std::vector<size_t> covEffortIndices = getColumnIndex(Infile, "covEffort");
	if (covEffortIndices.empty()) { // try again with other name
		covEffortIndices = getColumnIndex(Infile, "effort");
	}
	std::vector<size_t> covDetectionIndices = getColumnIndex(Infile, "covDetection");
	if (covDetectionIndices.empty()) { covDetectionIndices = getColumnIndex(Infile, "detection"); }

	checkHeader(Infile, countIndices, covEffortIndices);

	auto methodName = getMethodName(Infile.name());
	return std::make_tuple(countIndices, covEffortIndices, covDetectionIndices, methodName);
}

bool allEffortsAreZero(coretools::TInputMaybeRcppFile &InputFile, const std::vector<size_t> &CovEffortIndices) {
	std::set<TypeCovariateEffort> covEffort;
	for (auto c : CovEffortIndices) { covEffort.insert(InputFile.get<TypeCovariateEffort>(c)); }
	return covEffort.size() == 1 && *covEffort.begin() == 0.0; // returns true if all covariates of effort are zero
}

auto TBirpCore::_keepLocations(const std::string &Filename) const {
	// open file and get relevant column indices
	coretools::TInputMaybeRcppFile file(Filename, coretools::FileType::Header, getDelim(Filename));
	auto [countIndices, covEffortIndices, covDetectionIndices, methodName] = getIndicesMethodFile(file);

	// map: per location, how many timepoints with non-zero effort and non-zero counts are there?
	std::map<std::string, std::pair<size_t, size_t>> numTimePointsAndCountsPerLocation;
	for (; !file.empty(); file.popFront()) {
		// read location and timepoint
		std::string location  = file.get<std::string>("location");
		std::string timepoint = file.get<std::string>("timepoint");

		// read effort(s)
		if (allEffortsAreZero(file, covEffortIndices)) {
			// all effort covariates are zero
			logfile().warning("Method ", methodName, ": Ignoring location ", location, " at timepoint ", timepoint,
			                  " because of zero effort.");
			continue;
		}

		// sum counts across all species
		size_t sumCounts = 0;
		for (size_t c = 0; c < countIndices.size(); c++) {
			sumCounts += coretools::str::fromStringCheck<TypeCounts>(file.front()[countIndices[c]]);
		}

		auto it = numTimePointsAndCountsPerLocation.find(location);
		if (it == numTimePointsAndCountsPerLocation.end()) { // new location
			numTimePointsAndCountsPerLocation[location] = {1, sumCounts};
		} else {
			// location already exists
			it->second.first++;
			it->second.second += sumCounts;
		}
	}

	// parsed entire file: now check for each location if there are enough timepoints and non-zero counts
	std::map<std::string, bool> keepLocations;
	size_t numLocationsToKeep = 0;
	for (const auto &loc : numTimePointsAndCountsPerLocation) {
		if (loc.second.first == 1) {
			logfile().warning("Method ", methodName, ": Ignoring location ", loc.first,
			                  " because it only has a single timepoint with non-zero effort.");
			keepLocations[loc.first] = false;
		} else if (loc.second.second == 0) {
			logfile().warning("Method ", methodName, ": Ignoring location ", loc.first,
			                  " because it only has zero counts across all timepoints.");
			keepLocations[loc.first] = false;
		} else {
			keepLocations[loc.first] = true;
			++numLocationsToKeep;
		}
	}

	bool keepMethod = true;
	if (numLocationsToKeep == 0) {
		logfile().warning("Ignoring method ", methodName, " because no location passed filters!");
		keepMethod = false;
	}
	return std::pair(keepMethod, keepLocations);
}

void TBirpCore::_readFile(const std::string &Filename) {
	logfile().listFlushDots("Parsing ", Filename);
	// check which locations should be kept
	auto [keepMethod, keepLocations] = _keepLocations(Filename);
	if (!keepMethod) { return; } // chasch gd hei gah

	// open file and get relevant column indices
	coretools::TInputMaybeRcppFile file(Filename, coretools::FileType::Header, getDelim(Filename));
	auto [countIndices, covEffortIndices, covDetectionIndices, methodName] = getIndicesMethodFile(file);

	// assemble species names and id's
	std::vector<size_t> species_ids       = {};
	std::vector<std::string> speciesNames = getSpeciesOrCovariateNames(file, countIndices);
	for (const auto &name : speciesNames) { species_ids.push_back(_speciesNames.add(name)); }

	// get covariate effort names and store in unique container
	std::vector<size_t> covariateEffort_ids    = fillCovNames(file, covEffortIndices, _covariateEffortNames);
	std::vector<size_t> covariateDetection_ids = fillCovNames(file, covDetectionIndices, _covariateDetectionNames);

	// add new method: name is derived from filename
	TMethods method(methodName, _data.size(), species_ids, covariateEffort_ids, covariateDetection_ids);
	_data.addMethod(method); // the index of the method is the current size of the _methods vector

	// check if file contains information about CI group
	bool has_CI_groups = std::find(file.header().begin(), file.header().end(), "CI_group") != file.header().end();
	if (!has_CI_groups) { _CIGroupNames.add("Group_1"); }

	for (; !file.empty(); file.popFront()) {
		std::string location = (std::string)file.get("location");
		if (!keepLocations[location]) { continue; }                  // skip this location
		if (allEffortsAreZero(file, covEffortIndices)) { continue; } // skip timepoint: effort = 0

		size_t location_id  = _locations.add((std::string)location);
		size_t timepoint_id = _timepoints.add(file.get<TypeTime>("timepoint"));

		size_t CI_group_id = 0;
		if (has_CI_groups) { CI_group_id = _CIGroupNames.add(file.get<std::string>("CI_group")); }

		const auto &row = file.front();

		// boot one vector together (stiflä) of all species counts, and one of all covariates
		std::vector<TypeCounts> countVec(species_ids.size(), 0);
		std::vector<TypeCovariateEffort> covEffortVec(covEffortIndices.size());
		std::vector<TypeCovariateDetection> covDetectionVec(covDetectionIndices.size());
		for (size_t c = 0; c < countVec.size(); c++) {
			countVec[c] = coretools::str::fromStringCheck<TypeCounts>(row[countIndices[c]]);
		}
		for (size_t c = 0; c < covEffortVec.size(); c++) {
			covEffortVec[c] = coretools::str::fromStringCheck<TypeCovariateEffort>(row[covEffortIndices[c]]);
		}
		for (size_t c = 0; c < covDetectionVec.size(); c++) {
			covDetectionVec[c] = coretools::str::fromStringCheck<TypeCovariateDetection>(row[covDetectionIndices[c]]);
		}
		// create timepoint
		TTimepoints pt(countVec, covEffortVec, covDetectionVec, timepoint_id);
		// add timepoint to data, either in existing object or create new one
		_data.back().add(pt, location_id, CI_group_id, _locations, _CIGroupNames);
	}
	// standardize covariates
	_data.back().standardizeCovariates();

	_data.back().fillIDVectors(_speciesNames.size());

	logfile().done();
}

void TBirpCore::_sortTimepoints() {
	// sort timepoints and corresponding locations(?) for every method/species
	// check if data (location) has more than one entry with effort > 0

	std::vector<size_t> sortingIndex = coretools::rankSort(_timepoints);
	_timepoints                      = coretools::sortContainerByRank(_timepoints, sortingIndex);
	for (auto &method : _data) { method.sorttimes(sortingIndex); }
}

std::map<std::string, std::string> TBirpCore::_runMCMC(bool Stochastic,
                                                       const std::map<std::string, std::string> &InitVals) {
	// create model
	stattools::instances::dagBuilder().clear();
	TBirpModel model(_data, _locations, _CIGroupNames, _timepoints, _speciesNames, _covariateEffortNames,
	                 _covariateDetectionNames, _outname, Stochastic, false, InitVals);

	// run MCMC
	stattools::TMCMC mcmc;
	mcmc.runMCMC(_outname);

	// tell model to write gamma summaries to file
	model.writeSummaryGammaPosterior(_outname);

	// also write filtered counts etc
	_writeInference();

	// get initial values for stochastic, if needed
	return model.getFinalValues();
}

void TBirpCore::infer() {
	_readData();

	std::map<std::string, std::string> initVals;
	if (parameters().exists("stochastic")) {
		logfile().list("Will model stochastic trends (argument 'stochastic').");
		// run MCMC (deterministic)
		logfile().startIndent("Running MCMC with deterministic model for initialization:");
		initVals = _runMCMC(false, initVals);
		logfile().endIndent();
		// infer stochastic and use parameter estimates of deterministic to initialize
		logfile().startIndent("Running MCMC with stochastic model:");
		_runMCMC(true, initVals);
		logfile().endIndent();
	} else {
		// run MCMC (deterministic)
		logfile().list("Will model deterministic trends (use 'stochastic' to change).");
		_runMCMC(false, initVals);
	}

	// clean all member variables (for Rcpp)
	_clear();
}

//-----------------------------------
// Simulate

std::vector<std::string> TBirpCore::_getMethodspecificHeader(const TMethods &method) {
	std::vector<std::string> header = getHeader();

	// add CI group id to base header vector
	header.emplace_back("CI_group");

	// add species name to base header vector
	for (const auto &speciesID : method.speciesIDsinUniqueContainer()) {
		header.push_back("counts_" + _speciesNames[speciesID]);
	}
	// add covariate effort name to base header vector
	for (const auto &covID : method.covariateEffortIDsinUniqueContainer()) {
		header.push_back("covEffort_" + _covariateEffortNames[covID]);
	}
	// add covariate detection name to base header vector
	for (const auto &covID : method.covariateDetectionIDsinUniqueContainer()) {
		header.push_back("covDetection_" + _covariateDetectionNames[covID]);
	}
	return header;
}

void TBirpCore::_writeFile(const std::string &Name) {
	// make second file that stores names of all methodFiles generated during the simulation
	coretools::TOutputMaybeRcppFile fileList(_outname + "_" + Name + "_allFilesGenerated.txt");
	// write file line by line
	for (auto &method : _data) {
		// get filename here.
		std::string Filename            = _outname + "_" + method.name() + "_" + Name + "_" + "counts.txt";
		// get header here
		std::vector<std::string> header = _getMethodspecificHeader(method);
		// append to fileList
		fileList << Filename << coretools::endl;
		// now create actual file with data and write
		coretools::TOutputMaybeRcppFile outfile(Filename, header);
		method.write(outfile, _locations, _timepoints, _CIGroupNames);
	}
}

void TBirpCore::_writeMeanParametersState() {
	coretools::TOutputMaybeRcppFile file(_outname + "_state.txt", {"0", "value", "jumpSize"});

	for (auto param : stattools::instances::dagBuilder().getAllParameters()) {
		file << param->name();

		// values (comma-separated)
		file << param->getPosteriorMeans();  // write values as comma-seperated string if array
		param->writeJumpSizeOneString(file); // write jumpSizes as comma-seperated string if array
		file.endln();
	}
}

void TBirpCore::_writeInference() {
	_writeFile("filtered");
	_writeTimePoints();
	_writeCIGroups();
	_writeMeanParametersState();
}

void TBirpCore::_writeTimePoints() {
	coretools::TOutputMaybeRcppFile file(_outname + "_timepoints.txt", {"timepoints"});
	for (auto it : _timepoints) { file << it << coretools::endl; }
}

void TBirpCore::_writeCIGroups() {
	coretools::TOutputMaybeRcppFile file(_outname + "_CI_groups.txt", {"CI_groups"});
	for (auto it : _CIGroupNames) { file << it << coretools::endl; }
}

void TBirpCore::_fillTimepointsFromCommandLine() {
	std::vector<TypeTime> timepoints;
	std::vector<TypeTime> timepointsDefault = {1.0, 2.0, 3.0};
	if (parameters().exists("timepoints")) {
		auto timepointName = parameters().get<std::string>("timepoints");
		if (coretools::str::stringIsProbablyANumber(timepointName)) {
			auto timepointNumber = coretools::str::fromString<size_t>(timepointName);
			for (size_t i = 1; i <= timepointNumber; i++) { timepoints.emplace_back((double)i); }
			if (timepoints.size() == 1) { UERROR("Please provide more than one timepoint"); }
		} else {
			parameters().fill("timepoints", timepoints);
			if (timepoints.size() == 1) { UERROR("Please provide more than one timepoint"); }
		}
	} else {
		timepoints = timepointsDefault;
	}
	logfile().list("Timepoints: ", timepoints, " (argument 'timepoints').");
	// fill into unique container
	for (const auto &timepoint : timepoints) { _timepoints.add(timepoint); }
}

void TBirpCore::_fillLocationsFromCommandLine() {
	size_t numLocations = parameters().get("numLocations", 2);
	logfile().list("Number of locations: ", numLocations, " (argument 'numLocations').");
	for (size_t l = 0; l < numLocations; ++l) { _locations.add("Location_" + coretools::str::toString(l + 1)); }
}

void TBirpCore::_fillCIGroupsFromCommandLine() {
	size_t numCIGroups = parameters().get("numCIGroups", 1);

	if (parameters().exists("BACI")) { // get info on CI groups from BACI file
		auto name = parameters().get<std::string>("BACI");
		coretools::TInputMaybeRcppFile file(name, coretools::FileType::NoHeader);
		for (; !file.empty(); file.popFront()) {
			_CIGroupNames.add(file.get<std::string>(0)); // first column = CI group name
		}
		logfile().list("Number of control-intervention groups: ", _CIGroupNames.size(), " (argument 'BACI').");
	} else {
		if (numCIGroups == 0) {
			UERROR("Number of control-intervention groups can not be zero (argument 'numCIGroups').");
		}
		for (size_t i = 0; i < numCIGroups; ++i) { _CIGroupNames.add("Group_" + coretools::str::toString(i + 1)); }
		logfile().list("Number of control-intervention groups: ", _CIGroupNames.size(), " (argument 'numCIGroups').");
	}
}

std::vector<size_t> TBirpCore::_simulateSpeciesNames() {
	size_t numSpecies = parameters().get("numSpecies", 1);
	logfile().list("Number of species: ", numSpecies, " (argument 'numSpecies').");
	std::vector<size_t> species_ids(numSpecies);
	for (size_t n = 0; n < numSpecies; n++) {
		species_ids[n] = _speciesNames.add("species_" + coretools::str::toString(n + 1));
	}
	return species_ids;
}

std::vector<size_t> TBirpCore::_simulateCovEffortNames() {
	size_t numCovs = parameters().get("numCovariatesEffort", 1);
	if (numCovs == 0) { UERROR("Argument 'numCovariatesEffort' must be at least 1!"); }
	logfile().list("Number of effort covariates: ", numCovs, " (argument 'numCovariatesEffort').");

	// fill names and ids
	std::vector<size_t> covariates_ids(numCovs);
	for (size_t n = 0; n < numCovs; n++) {
		covariates_ids[n] = _covariateEffortNames.add(coretools::str::toString(n + 1));
	}
	return covariates_ids;
}

std::vector<size_t> TBirpCore::_simulateCovDetectionNames() {
	size_t numCovs = parameters().get("numCovariatesDetection", 0);
	logfile().list("Number of detection probability covariates: ", numCovs, " (argument 'numCovariatesDetection').");
	// fill names and ids
	std::vector<size_t> covariates_ids(numCovs);
	for (size_t n = 0; n < numCovs; n++) {
		covariates_ids[n] = _covariateDetectionNames.add(coretools::str::toString(n + 1));
	}
	return covariates_ids;
}

void TBirpCore::simulate() {
	stattools::instances::dagBuilder().clear();

	if (parameters().exists("data")) {
		// read data file and simulate counts for that setup
		_readData();
	} else {
		// else read main dimensions from command line
		_fillTimepointsFromCommandLine();
		_fillLocationsFromCommandLine();
		_fillCIGroupsFromCommandLine();
		size_t numMethods = parameters().get("numMethods", 1);
		logfile().list("Number of methods: ", numMethods, " (argument 'numMethods').");

		auto species_ids            = _simulateSpeciesNames();
		auto covariateEffort_ids    = _simulateCovEffortNames();
		auto covariateDetection_ids = _simulateCovDetectionNames();

		for (size_t m = 0; m < numMethods; ++m) {
			TMethods method("Method_" + coretools::str::toString(m + 1), _locations.size(), _timepoints.size(), m,
			                species_ids, covariateEffort_ids, covariateDetection_ids, _CIGroupNames.size(), _data);
			_data.addMethod(method);
		}
		for (auto &m : _data) { m.fillIDVectors(_speciesNames.size()); }

		_data.fillMethLocIndices(_speciesNames.size(), _locations, _CIGroupNames);
	}

	// create model
	const bool stochastic = parameters().exists("stochastic");
	if (stochastic) {
		logfile().list("Will simulate stochastic trends (argument 'stochastic').");
	} else {
		logfile().list("Will simulate deterministic trends (use 'stochastic' to change).");
	}

	std::map<std::string, std::string> initVals; // empty
	TBirpModel model(_data, _locations, _CIGroupNames, _timepoints, _speciesNames, _covariateEffortNames,
	                 _covariateDetectionNames, _outname, stochastic, true, initVals);

	// run simulator
	stattools::TSimulator simulator;
	simulator.simulate();

	// write Birp file
	_writeFile("simulated");

	// clean all member variables (for Rcpp)
	_clear();
}
