//
// Created by madleina on 11.01.21.
//

#include "stattools/MCMC/TMCMCFiles.h"

#include "coretools/Files/TInputFile.h"
#include "coretools/Files/TOutputFile.h"
#include "coretools/Strings/splitters.h"
#include "stattools/ParametersObservations/TNodeBase.h"

namespace stattools {

//--------------------------------------------
TMCMCTraceFile::TMCMCTraceFile(std::string_view Filename) : _helper(Filename) {}
TMCMCTraceFile::TMCMCTraceFile(std::string_view Filename, TParameterBase *Param) : _helper(Filename, Param) {}
TMCMCTraceFile::TMCMCTraceFile(std::string_view Filename, TObservationBase *Obs) : _helper(Filename, Obs) {}

void TMCMCTraceFile::add(TParameterBase *Param) { _helper.add(Param); }
void TMCMCTraceFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCTraceFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCTraceFile::name() { return _helper.name(); };

void TMCMCTraceFile::writeHeader() {
	std::vector<std::string> header;
	for (auto &param : _helper.paramsInFile()) { param->fillNames(header); }
	_helper.file().writeHeader(header);
}

void TMCMCTraceFile::write() {
	for (auto &param : _helper.paramsInFile()) { param->writeToTrace(_helper.file()); }
	_helper.file().endln();
}

void TMCMCTraceFile::close() { _helper.close(); }

//--------------------------------------------
TMCMCMeanVarFile::TMCMCMeanVarFile(std::string_view Filename) : _helper(Filename) {}
TMCMCMeanVarFile::TMCMCMeanVarFile(std::string_view Filename, TParameterBase *Param) : _helper(Filename, Param) {}
TMCMCMeanVarFile::TMCMCMeanVarFile(std::string_view Filename, TObservationBase *Obs) : _helper(Filename, Obs) {}

void TMCMCMeanVarFile::add(TParameterBase *Param) { _helper.add(Param); }
void TMCMCMeanVarFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCMeanVarFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCMeanVarFile::name() { return _helper.name(); };

void TMCMCMeanVarFile::writeHeader() { _helper.file().writeHeader({"name", "posterior_mean", "posterior_variance"}); }

void TMCMCMeanVarFile::write() {
	for (auto &param : _helper.paramsInFile()) { param->writeToSummary(MCMCFiles::meanVar, _helper.file()); }
}

void TMCMCMeanVarFile::close() { _helper.close(); }

//--------------------------------------------
TMCMCStatePosteriorsFile::TMCMCStatePosteriorsFile(std::string_view Filename) : _helper(Filename) {}
TMCMCStatePosteriorsFile::TMCMCStatePosteriorsFile(std::string_view Filename, TParameterBase *Param)
    : _helper(Filename, Param) {

	_max = Param->getNumStatesForStatePosterior();
}
TMCMCStatePosteriorsFile::TMCMCStatePosteriorsFile(std::string_view Filename, TObservationBase *Obs)
    : _helper(Filename, Obs) {}

void TMCMCStatePosteriorsFile::add(TParameterBase *Param) {
	_helper.add(Param);
	// check if all params in this file have the same max
	if (Param->getNumStatesForStatePosterior() != _max) {
		DEVERROR("Can not write parameter ", Param->name(), " into state posterior file ", this->name(),
		         " since it has a different number of categories (", Param->getNumStatesForStatePosterior(),
		         ") than the other parameters in that file (", _max, ")!");
	}

	// headers of all parameters must match
	std::vector<std::string> header = _getHeaderOneParam(_helper.paramsInFile().front());
	for (auto it : _helper.paramsInFile()) {
		auto h = _getHeaderOneParam(it);
		if (h != header) {
			DEVERROR("Can not write parameter ", it->name(), " into state posterior file ", this->name(),
			         " since it has a different header (", h, ") than the other parameters in that file (", header,
			         ")!");
		}
	}
}

void TMCMCStatePosteriorsFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCStatePosteriorsFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCStatePosteriorsFile::name() { return _helper.name(); };

std::vector<std::string> TMCMCStatePosteriorsFile::_getHeaderOneParam(TParameterBase *Param) {
	auto header = Param->getDefinition().getStateNames();
	if (header.empty()) { // fill with default values
		header.emplace_back("name");
		for (size_t i = 0; i < _max; ++i) { header.emplace_back("state_" + coretools::str::toString(i)); }
		return header;
	}
	if (header.size() == _max) { // else: size must match
		header.insert(header.begin(), "name");
		return header;
	}
	DEVERROR("Size of header (", header.size(), ") does not match the number of states (", _max, ")!");
}

void TMCMCStatePosteriorsFile::writeHeader() {
	std::vector<std::string> header = _getHeaderOneParam(_helper.paramsInFile().front());
	_helper.file().writeHeader(header);
}

void TMCMCStatePosteriorsFile::write() {
	// only write posterior count files for parameters (not for observations, these don't change!)
	for (auto &param : _helper.paramsInFile()) { param->writeToSummary(MCMCFiles::statePosteriors, _helper.file()); }
}

void TMCMCStatePosteriorsFile::close() { _helper.close(); }

//--------------------------------------------
TMCMCPosteriorModeFile::TMCMCPosteriorModeFile(std::string_view Filename) : _helper(Filename) {}
TMCMCPosteriorModeFile::TMCMCPosteriorModeFile(std::string_view Filename, TParameterBase *Param)
    : _helper(Filename, Param) {}

TMCMCPosteriorModeFile::TMCMCPosteriorModeFile(std::string_view Filename, TObservationBase *Obs)
    : _helper(Filename, Obs) {}

void TMCMCPosteriorModeFile::add(TParameterBase *Param) { _helper.add(Param); }
void TMCMCPosteriorModeFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCPosteriorModeFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCPosteriorModeFile::name() { return _helper.name(); };

void TMCMCPosteriorModeFile::writeHeader() { _helper.file().writeHeader({"name", "posterior_mode"}); }

void TMCMCPosteriorModeFile::write() {
	// only write mode count files for parameters (not for observations, these don't change!)
	for (auto &param : _helper.paramsInFile()) { param->writeToSummary(MCMCFiles::posteriorMode, _helper.file()); }
}

void TMCMCPosteriorModeFile::close() { _helper.close(); }

//--------------------------------------------
TMCMCSimulationFile::TMCMCSimulationFile(std::string_view Filename) : _helper(Filename) {}
TMCMCSimulationFile::TMCMCSimulationFile(std::string_view Filename, TParameterBase *Param) : _helper(Filename, Param) {}
TMCMCSimulationFile::TMCMCSimulationFile(std::string_view Filename, TObservationBase *Obs) : _helper(Filename, Obs) {}

void TMCMCSimulationFile::add(TParameterBase *Param) { _helper.add(Param); }
void TMCMCSimulationFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCSimulationFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCSimulationFile::name() { return _helper.name(); };

void TMCMCSimulationFile::writeHeader() { _helper.file().writeHeader({"name", "value"}); }

void TMCMCSimulationFile::write() {
	// write simulation files for both parameters and observations!
	for (auto &param : _helper.paramsInFile()) { param->writeToSummary(MCMCFiles::simulation, _helper.file()); }
	for (auto &obs : _helper.observationsInFile()) { obs->writeToSummary(MCMCFiles::simulation, _helper.file()); }
}

void TMCMCSimulationFile::close() { _helper.close(); }

//--------------------------------------------
TMCMCStateFile::TMCMCStateFile(std::string_view Filename) : _helper(Filename) {}
TMCMCStateFile::TMCMCStateFile(std::string_view Filename, TParameterBase *Param) : _helper(Filename, Param) {}
TMCMCStateFile::TMCMCStateFile(std::string_view Filename, TObservationBase *Obs) : _helper(Filename, Obs) {}

void TMCMCStateFile::add(TParameterBase *Param) { _helper.add(Param); }
void TMCMCStateFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCStateFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCStateFile::name() { return _helper.name(); };

void TMCMCStateFile::write(size_t iteration) {
	// only write state files for parameters (not for observations, these don't change)!

	// idea: overwrite state file each time, write iteration in header
	// will be opened the first time we write, but every time after we will have to re-open it in order to overwrite
	if (!_helper.file().isOpen()) { _helper.file().open(_helper.name()); }
	_writeHeader(iteration);

	// set high precision to be as close to the state as possible
	_helper.file().precision(20);

	for (auto &param : _helper.paramsInFile()) {
		_helper.file() << param->name();
		param->writeValsOneString(_helper.file());     // write values as comma-seperated string if array
		param->writeJumpSizeOneString(_helper.file()); // write jumpSizes as comma-seperated string if array
		_helper.file().endln();
	}
	_helper.close();
}

void TMCMCStateFile::_writeHeader(size_t iteration) {
	std::string itr = coretools::str::toString(iteration);
	_helper.file().writeHeader({itr, "value", "jumpSize"});
}

void TMCMCStateFile::writeHeader() { /* don't write standard header from base class */ }

void TMCMCStateFile::close() { _helper.close(); }

//--------------------------------------------
TMCMCDensitiesTraceFile::TMCMCDensitiesTraceFile(std::string_view Filename) : _helper(Filename) {}
TMCMCDensitiesTraceFile::TMCMCDensitiesTraceFile(std::string_view Filename, TParameterBase *Param)
    : _helper(Filename, Param) {}
TMCMCDensitiesTraceFile::TMCMCDensitiesTraceFile(std::string_view Filename, TObservationBase *Obs)
    : _helper(Filename, Obs) {}

void TMCMCDensitiesTraceFile::add(TParameterBase *Param) { _helper.add(Param); }
void TMCMCDensitiesTraceFile::add(TObservationBase *Obs) { _helper.add(Obs); }

void TMCMCDensitiesTraceFile::setPrecision(size_t Precision) { _helper.setPrecision(Precision); }
std::string TMCMCDensitiesTraceFile::name() { return _helper.name(); };

void TMCMCDensitiesTraceFile::write() {
	std::vector<double> logDensities;
	logDensities.reserve(_helper.paramsInFile().size() + _helper.observationsInFile().size());

	for (auto &obs : _helper.observationsInFile()) { logDensities.push_back(obs->getSumLogPriorDensity()); }
	for (auto &param : _helper.paramsInFile()) { logDensities.push_back(param->getSumLogPriorDensity()); }

	_helper.file().writeln(coretools::containerSum(logDensities), logDensities);
}

void TMCMCDensitiesTraceFile::writeHeader() {
	std::vector<std::string> header = {"sum_log_densities_DAG"};
	for (auto &observation : _helper.observationsInFile()) { header.push_back(observation->name()); }
	for (auto &param : _helper.paramsInFile()) { header.push_back(param->name()); }

	_helper.file().writeHeader(header);
}

void TMCMCDensitiesTraceFile::close() { _helper.close(); }

//--------------------------------------------
// TMCMCFileReader
//--------------------------------------------

TMCMCFileReader::TMCMCFileReader(std::string_view Filename) {
	_name = Filename;
	_file.open(_name, coretools::FileType::Header, "\t");
}

void TMCMCFileReader::close() { _file.close(); }

//--------------------------------------------
TTraceReader::TTraceReader(std::string_view Filename) : TMCMCFileReader(Filename) {}

void TTraceReader::_read(std::string_view) {
	// names = header
	_names.reserve(_file.header().size());
	_names.insert(_names.end(), _file.header().begin(), _file.header().end());

	// parse file until we get to the last row (= last value of chain)
	std::string tmp;
	for (; !_file.empty(); _file.popFront()) { tmp = _file.frontRaw(); }
	coretools::str::TSplitter spl(tmp, '\t');
	_values.clear();
	for (const auto s : spl) { _values.emplace_back(s); }
}

//--------------------------------------------
TMeanVarReader::TMeanVarReader(std::string_view Filename) : TMCMCFileReader(Filename) {
	if (_file.numCols() != 3 || _file.header()[1] != "posterior_mean" || _file.header()[2] != "posterior_variance") {
		UERROR("Invalid format of posterior mean/variance file ", Filename,
		       "! Expected 3 columns with headers 'name', 'posterior_mean' and 'posterior_variance'.");
	}
}

void TMeanVarReader::_read(std::string_view) {
	for (; !_file.empty(); _file.popFront()) {
		_names.emplace_back(_file.get(0));
		_values.emplace_back(_file.get(1));
	}
}

//--------------------------------------------
TStatePosteriorsReader::TStatePosteriorsReader(std::string_view Filename) : TMCMCFileReader(Filename) {
	if (_file.numCols() == 0) {
		UERROR("Invalid format of state posterior file ", Filename,
		       "! Expected at least one column with header 'name'.");
	}
}

void TStatePosteriorsReader::_read(std::string_view ParamName) {
	for (; !_file.empty(); _file.popFront()) {
		_names.emplace_back(_file.get(0));

		std::vector<double> d(_file.numCols() - 1);
		for (size_t i = 0; i < d.size(); ++i) {
			d[i] = _convertStringNiceThrow<double>(_file.get(i + 1), i, ParamName);
		}
		size_t max_ix = std::distance(d.begin(), std::max_element(d.begin(), d.end()));
		_values.emplace_back(coretools::str::toString(max_ix));
	}
}

//--------------------------------------------
TPosteriorModeReader::TPosteriorModeReader(std::string_view Filename) : TMCMCFileReader(Filename) {
	if (_file.numCols() != 2 || _file.header()[1] != "posterior_mode") {
		UERROR("Invalid format of state posterior file ", Filename,
		       "! Expected two columns with headers 'name' and 'posterior_mode'.");
	}
}

void TPosteriorModeReader::_read(std::string_view) {
	for (; !_file.empty(); _file.popFront()) {
		_names.emplace_back(_file.get(0));
		_values.emplace_back(_file.get(1));
	}
}

//--------------------------------------------
TSimulationReader::TSimulationReader(std::string_view Filename) : TMCMCFileReader(Filename) {
	if (_file.numCols() != 2 || _file.header()[1] != "value") {
		UERROR("Invalid format of simulation file ", Filename, "! Expected 2 columns with headers 'name' and 'value'.");
	}
}

void TSimulationReader::_read(std::string_view) {
	for (; !_file.empty(); _file.popFront()) {
		_names.emplace_back(_file.get(0));
		_values.emplace_back(_file.get(1));
	}
}

} // end namespace stattools
