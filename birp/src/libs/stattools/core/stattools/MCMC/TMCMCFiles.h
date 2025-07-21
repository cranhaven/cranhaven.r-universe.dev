//
// Created by madleina on 11.01.21.
//

#ifndef TMCMCFILES_H
#define TMCMCFILES_H

#include <algorithm>
#include <cstddef>
#include <string>
#include <vector>

#include "coretools/Files/TInputFile.h"
#include "coretools/Main/TError.h"
#include "coretools/Storage/TStorage.h"
#include "coretools/Strings/fromString.h"
#include "stattools/ParametersObservations/TDefinition.h"
#include "stattools/ParametersObservations/TNodeBase.h"
#include "stattools/ParametersObservations/TValue.h"

namespace stattools {
//-------------------------------------------
// TMCMCFile
//-------------------------------------------

namespace impl {
template<typename OutputFile> class TMCMCFileHelper {
private:
	// file
	OutputFile _file;

	std::vector<TParameterBase *> _paramsInFile;
	std::vector<TObservationBase *> _observationsInFile;

	// write
	std::string _name;
	size_t _precision = 6;

public:
	TMCMCFileHelper(std::string_view Filename) : _name(Filename) { _file.open(Filename); }
	TMCMCFileHelper(std::string_view Filename, TParameterBase *Param) : _name(Filename) {
		_file.open(Filename);
		add(Param);
	}
	TMCMCFileHelper(std::string_view Filename, TObservationBase *Obs) : _name(Filename) {
		_file.open(Filename);
		add(Obs);
	}

	void add(TParameterBase *Param) {
		_paramsInFile.push_back(Param);
		setPrecision(Param->getDefinition().precision());
	}

	void add(TObservationBase *Obs) {
		_observationsInFile.push_back(Obs);
		setPrecision(Obs->getDefinition().precision());
	}

	void setPrecision(size_t Precision) {
		// set precision of file to maximum value among all parameters inside that file
		_precision = std::max(_precision, Precision);
		_file.precision((int)_precision);
	}

	const std::string &name() const { return _name; }

	const OutputFile &file() const { return _file; }
	OutputFile &file() { return _file; }

	const std::vector<TParameterBase *> &paramsInFile() const { return _paramsInFile; }
	std::vector<TParameterBase *> &paramsInFile() { return _paramsInFile; }

	const std::vector<TObservationBase *> &observationsInFile() const { return _observationsInFile; }
	std::vector<TObservationBase *> &observationsInFile() { return _observationsInFile; }

	void close() { _file.close(); }
};
} // namespace impl

class TMCMCFile {
	// Pure virtual base class
	// only defines interface
public:
	virtual ~TMCMCFile()                        = default;
	virtual void add(TParameterBase *Param)     = 0;
	virtual void add(TObservationBase *Obs)     = 0;
	virtual void setPrecision(size_t Precision) = 0;

	// getters
	virtual std::string name() = 0;

	// write
	virtual void writeHeader() = 0;

	// close
	virtual void close() = 0;
};

class TMCMCTraceFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

public:
	TMCMCTraceFile(std::string_view Filename);
	TMCMCTraceFile(std::string_view Filename, TParameterBase *Param);
	TMCMCTraceFile(std::string_view Filename, TObservationBase *Obs);
	~TMCMCTraceFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write();

	void close() override;
};

class TMCMCMeanVarFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

public:
	TMCMCMeanVarFile(std::string_view Filename);
	TMCMCMeanVarFile(std::string_view Filename, TParameterBase *Param);
	TMCMCMeanVarFile(std::string_view Filename, TObservationBase *Obs);

	~TMCMCMeanVarFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write();

	void close() override;
};

class TMCMCStatePosteriorsFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

	size_t _max = 0;
	std::vector<std::string> _getHeaderOneParam(TParameterBase *Param);

public:
	TMCMCStatePosteriorsFile(std::string_view Filename);
	TMCMCStatePosteriorsFile(std::string_view Filename, TParameterBase *Param);
	TMCMCStatePosteriorsFile(std::string_view Filename, TObservationBase *Obs);
	~TMCMCStatePosteriorsFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write();

	void close() override;
};

class TMCMCPosteriorModeFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

public:
	TMCMCPosteriorModeFile(std::string_view Filename);
	TMCMCPosteriorModeFile(std::string_view Filename, TParameterBase *Param);
	TMCMCPosteriorModeFile(std::string_view Filename, TObservationBase *Obs);
	~TMCMCPosteriorModeFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write();

	void close() override;
};

class TMCMCSimulationFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

public:
	TMCMCSimulationFile(std::string_view Filename);
	TMCMCSimulationFile(std::string_view Filename, TParameterBase *Param);
	TMCMCSimulationFile(std::string_view Filename, TObservationBase *Obs);
	~TMCMCSimulationFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write();

	void close() override;
};

class TMCMCStateFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

protected:
	void _writeHeader(size_t iteration);

public:
	TMCMCStateFile(std::string_view Filename);
	TMCMCStateFile(std::string_view Filename, TParameterBase *Param);
	TMCMCStateFile(std::string_view Filename, TObservationBase *Obs);
	~TMCMCStateFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write(size_t iteration);

	void close() override;
};

class TMCMCDensitiesTraceFile : public TMCMCFile {
private:
	impl::TMCMCFileHelper<coretools::TOutputMaybeRcppFile> _helper;

public:
	TMCMCDensitiesTraceFile(std::string_view Filename);
	TMCMCDensitiesTraceFile(std::string_view Filename, TParameterBase *Param);
	TMCMCDensitiesTraceFile(std::string_view Filename, TObservationBase *Obs);
	~TMCMCDensitiesTraceFile() override = default;

	void add(TParameterBase *Param) override;
	void add(TObservationBase *Obs) override;
	void setPrecision(size_t Precision) override;

	// getters
	std::string name() override;
	void writeHeader() override;
	void write();

	void close() override;
};

//-------------------------------------------
// TMCMCFileReader
//-------------------------------------------

class TMCMCFileReader {
protected:
	// read
	coretools::TInputFile _file;
	std::string _name;

	std::vector<std::string> _names;
	std::vector<std::string> _values;

	// constructors
	explicit TMCMCFileReader(std::string_view Filename);

	// read
	template<typename Type, size_t NumDim>
	std::vector<size_t> _findIndices(std::string_view paramName, const std::vector<std::string> &Names,
									 coretools::TMultiDimensionalStorage<TValueUpdated<Type>, NumDim> &Storage) const {
		std::vector<size_t> colIndices(Storage.size());
		for (size_t i = 0; i < Storage.size(); i++) {
			std::string name = Storage.getFullDimensionNameWithPrefix(i, paramName);

			auto first = std::find_if(Names.cbegin(), Names.cend(), [&name](auto &n) { return name == n; });
			if (first == Names.cend()) {
				throw coretools::TUserError("Error while reading file '", _file.name(), "' for initialization of parameter ", paramName,
					   ": Expected name ", name, " for index ", i, " does not exist!");
			}
			colIndices[i] = std::distance(Names.cbegin(), first);
		}
		return colIndices;
	}

	template<class Type> Type _convertStringNiceThrow(std::string_view str, size_t col, std::string_view paramName) {
		Type val{};
		try {
			val = coretools::str::fromString<Type, true>(str);
		} catch (...) {
			throw coretools::TUserError("Invalid initial value '", str, "' in column ", col, " of file '", _file.name(), "' for parameter ",
				   paramName, "!");
		}
		return val;
	}

	virtual void _read(std::string_view ParamName) = 0;

public:
	std::string name() { return _name; }
	coretools::TConstView<std::string> header() { return _file.header(); }

	// read
	template<typename Type, size_t NumDim>
	void read(std::string_view paramName, coretools::TMultiDimensionalStorage<TValueUpdated<Type>, NumDim> &Storage) {
		if (_names.empty() && _values.empty()) {
			// only need to read stuff the time this function is called (in case of multiple parameters
			_read(paramName);
		}
		// get columns we need to store
		std::vector<size_t> colIndices = _findIndices(paramName, _names, Storage);

		// fill storage vector
		for (size_t i = 0; i < Storage.size(); i++) {
			assert(Storage.size() == colIndices.size());
			// convert to number
			size_t col = colIndices[i];
			Type val   = _convertStringNiceThrow<Type>(_values[col], col, paramName);
			Storage[i].initBoth(val);
		}
	}

	// close
	void close();
};

class TTraceReader : public TMCMCFileReader {
protected:
	void _read(std::string_view ParamName) override;

public:
	explicit TTraceReader(std::string_view Filename);
};

class TMeanVarReader : public TMCMCFileReader {
protected:
	void _read(std::string_view ParamName) override;

public:
	explicit TMeanVarReader(std::string_view Filename);
};

class TStatePosteriorsReader : public TMCMCFileReader {
protected:
	void _read(std::string_view ParamName) override;

public:
	explicit TStatePosteriorsReader(std::string_view Filename);
};

class TPosteriorModeReader : public TMCMCFileReader {
protected:
	void _read(std::string_view ParamName) override;

public:
	explicit TPosteriorModeReader(std::string_view Filename);
};

class TSimulationReader : public TMCMCFileReader {
protected:
	void _read(std::string_view ParamName) override;

public:
	explicit TSimulationReader(std::string_view Filename);
};

} // end namespace stattools

#endif // TMCMCFILES_H
