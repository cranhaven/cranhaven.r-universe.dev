//
// Created by madleina on 01.07.21.
//

#ifndef BANGOLIN_TREADINITIALVALUES_H
#define BANGOLIN_TREADINITIALVALUES_H

#include "coretools/Strings/fillContainer.h"
#include "coretools/Strings/fromString.h"
#include "coretools/Strings/stringProperties.h"
#include "stattools/MCMC/TMCMCFiles.h"

namespace stattools {
template<typename Type, size_t NumDim> class TReadInitialValues {
protected:
	template<class StorageType>
	void _copyValsToStorage(const std::vector<Type> &Vals, StorageType &Storage, std::string_view Name) const {
		// check if size matches
		if (Vals.size() != Storage.size()) {
			UERROR("Size of initial values (", Vals.size(), ") for parameter ", Name, " does not match expected size (",
				   Storage.size(), ")!");
		}
		// copy
		if constexpr (std::is_same_v<StorageType, coretools::TMultiDimensionalStorage<TValueUpdated<Type>, NumDim>>) {
			coretools::TRange range = Storage.getFull();
			for (size_t i = range.begin; i < range.end; i += range.increment) { Storage[i].initBoth(Vals[i]); }
		} else {
			Storage = Vals;
		}
	}

	template<class StorageType>
	void _readValsOnlyOneNumber(std::string_view InitVal, StorageType &Storage, std::string_view Name) const {
		Type val{};
		try {
			std::string s{InitVal};
			coretools::str::eraseAllWhiteSpaces(s);
			val = coretools::str::fromString<Type, true>(s);
		} catch (...) {
			UERROR("Invalid initial value (", InitVal, ") for parameter ", Name,
				   "! Should be a number and inside numeric boundaries of that parameter type.");
		}
		std::vector<Type> vals(Storage.size(), val);
		_copyValsToStorage(vals, Storage, Name);
	}

	template<class StorageType>
	void _readValsFromVec(std::string_view InitVal, StorageType &Storage, std::string_view Name) const {
		std::vector<Type> vals;
		try {
			coretools::str::fillContainerFromString(InitVal, vals, ',', true, true);
		} catch (...) {
			UERROR(
				"Invalid initial value (", InitVal, ") for parameter ", Name,
				"! The vector should only contain numbers that are inside numeric boundaries of that parameter type.");
		}
		_copyValsToStorage(vals, Storage, Name);
	}

	template<class StorageType>
	void _readValsFromFile_oneCol(coretools::TInputFile &File, StorageType &Storage, std::string_view Name) const {
		std::vector<Type> vals;
		try {
			for (; !File.empty(); File.popFront()) {
				vals.push_back(File.get<Type>(0));
			}
		} catch (...) {
			UERROR("Encountered invalid initial value in file '", File.name(), "' for parameter ", Name,
				   "! (Last valid value: ", vals.back(), ").");
		}
		_copyValsToStorage(vals, Storage, Name);
	}

	template<class StorageType>
	void _readValsFromFile_oneRow(coretools::TInputFile &File, StorageType &Storage, std::string_view Name) const {
		std::vector<Type> vals;
		try {
			for (size_t i = 0; i < File.numCols(); ++i) {
				vals.push_back(File.get<Type>(i));
			}
		} catch (...) {
			UERROR("Encountered invalid initial value in file '", File.name(), "' for parameter ", Name,
				   "! (Last valid value: ", vals.back(), ").");
		}
		File.popFront();

		if (!File.empty()) {
			UERROR("Too many lines in file ", File.name(), " (", File.curLine(), ")! Expected one line.");
		}
		_copyValsToStorage(vals, Storage, Name);
	}

	template<class StorageType>
	bool _readValsFromFile_oneColOrRow(std::string_view Filename, StorageType &Storage, std::string_view Name) const {
		// open file: no header
		coretools::TInputFile file(Filename, coretools::FileType::NoHeader, "\t");

		if (file.numCols() == 1) { // 1 col, N lines
			_readValsFromFile_oneCol(file, Storage, Name);
			return true;
		} else if (file.numCols() == Storage.size()) { // N cols, 1 line
			_readValsFromFile_oneRow(file, Storage, Name);
			return true;
		} else {
			return false;
		}
	}

	void _readValsFromFile(std::string_view Filename, std::vector<Type> &Storage, std::string_view Name) const {
		// 2 options:
		// 4) read a file with one column
		// 5) read a file with one row
		if (!_readValsFromFile_oneColOrRow(Filename, Storage, Name)) { // file format: all in one col
			UERROR("Invalid file format of ", Filename, ". Expected a file with either 1 or ", Storage.size(),
				   " lines.");
		}
	}

	template<class T>
	bool _read(MCMCFiles FileType, std::string_view Filename,
			   coretools::TMultiDimensionalStorage<TValueUpdated<Type>, NumDim> &Storage, std::string_view Name) const {
		if (coretools::str::stringContains(Filename, MCMCFileToString(FileType))) {
			T file(Filename);
			file.read(Name, Storage);
			return true;
		}
		return false;
	}

	void _readValsFromFile(std::string_view Filename,
						   coretools::TMultiDimensionalStorage<TValueUpdated<Type>, NumDim> &Storage,
						   std::string_view Name) const {
		// 7 options: trace / simulation / meanVar / statePosterior / posteriorMode / all in one row / all in one col
		bool found = _read<TTraceReader>(MCMCFiles::trace, Filename, Storage, Name);
		if (!found) { found = _read<TSimulationReader>(MCMCFiles::simulation, Filename, Storage, Name); }
		if (!found) { found = _read<TMeanVarReader>(MCMCFiles::meanVar, Filename, Storage, Name); }
		if (!found) { found = _read<TStatePosteriorsReader>(MCMCFiles::statePosteriors, Filename, Storage, Name); }
		if (!found) { found = _read<TPosteriorModeReader>(MCMCFiles::posteriorMode, Filename, Storage, Name); }
		if (!found) { found = _readValsFromFile_oneColOrRow(Filename, Storage, Name); }

		if (!found) {
			UERROR("Invalid file format of ", Filename,
				   ". Expected a file whose filename contains 'trace', 'simulated', 'meanVar', 'statePosteriors', or "
				   "then a file with either 1 or ",
				   Storage.size(), " lines.");
		}
	}

public:
	template<class StorageType> void readVals(std::string_view InitVal, StorageType &Storage, std::string_view Name) {
		// InitVal can either be...
		//          1) a vector of a given size.
		//          2) a single value
		//          3) a filename
		if (coretools::str::stringContains(InitVal, ",")) { // case 1): probably a comma-separated vector
			_readValsFromVec(InitVal, Storage, Name);
		} else {
			if (coretools::str::stringIsProbablyANumber(InitVal) || coretools::str::stringIsProbablyABool(InitVal)) {
				// case 2): probably only one number -> initialize all elements in array to this value
				_readValsOnlyOneNumber(InitVal, Storage, Name);
			} else { // case 3): probably a file -> try opening it!
				_readValsFromFile(InitVal, Storage, Name);
			}
		}
	}
};
}; // end namespace stattools

#endif // BANGOLIN_TREADINITIALVALUES_H
