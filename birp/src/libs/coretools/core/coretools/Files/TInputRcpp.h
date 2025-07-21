//
// Created by madleina on 07.05.24.
//

#ifndef TINPUTRCPP_H
#define TINPUTRCPP_H

#include <iterator>
#include <string_view>
#include <type_traits>

#include "coretools/Containers/TView.h"
#include "coretools/Main/TError.h"
#include "coretools/Main/TRcppData.h"
#include "coretools/Strings/concatenateString.h"
#include "coretools/Strings/fromString.h"
#include "coretools/Strings/stringConstants.h"
#include "coretools/Strings/toString.h"
#include "coretools/traits.h"

#ifdef USE_RCPP
#include "coretools/arma_include.h" // makes sure order of includes is valid for Rcpp
#endif

namespace coretools {
enum class FileType : bool { Header, NoHeader };

#ifdef USE_RCPP

class TInputRcpp {
	size_t _ix     = 0;
	size_t _curLin = 0;

	// store current line as strings (only if needed) -> can return const & from front() -> compatible with TInputFile
	mutable std::vector<std::string> _line;
	mutable std::string _lineConcat;

	std::string _delim{str::whitespaces};

	std::vector<std::string> _header;
	std::vector<std::pair<std::string, size_t>> _map;

	auto _mapIt(std::string_view Key) const {
		return std::lower_bound(_map.begin(), _map.end(), Key,
		                        [](const auto &p, std::string_view K) { return p.first < K; });
	}

	void _fillLine() const {
		// Note: very inefficient, need to copy into vector
		_line.resize(numCols());
		for (size_t i = 0; i < numCols(); ++i) {
			std::string val(Rcpp::as<Rcpp::CharacterVector>(instances::rcppData()[_ix][i])[_curLin]);
			_line[i] = val;
		}
	}

	void _fillLineConcat() const {
		_lineConcat = "";
		for (size_t i = 0; i < numCols(); ++i) {
			std::string val(Rcpp::as<Rcpp::CharacterVector>(instances::rcppData()[_ix][i])[_curLin]);
			if (i > 0) { _lineConcat.append(","); }
			_lineConcat.append(val);
		}
	}

	template<typename T, int RTYPE> T _convert(const Rcpp::Vector<RTYPE> &Vec, size_t i) const {
		// get single value from vector in underlying type
		typedef typename Rcpp::traits::storage_type<RTYPE>::type storage_t;
		const storage_t val = Vec[i];
		if constexpr (std::is_same_v<T, std::string_view> || std::is_same_v<T, std::string>) {
			return coretools::str::toString(val);
		} else {
			return static_cast<T>(val);
		}
	}

public:
	TInputRcpp() = default;

	TInputRcpp(std::string_view FileName, FileType FType, std::string_view Delim = str::whitespaces,
	           std::string_view Comment = "#") {
		open(FileName, FType, Delim, Comment);
	}

	void open(std::string_view FileName, FileType /* FType */, std::string_view Delim = str::whitespaces,
	          std::string_view /* Comment */ = "#") {
		_curLin = 0; // re-set
		_delim  = Delim;
		_ix     = instances::rcppData().index(FileName);

		// get header
		Rcpp::CharacterVector names = instances::rcppData()[_ix].names();
		_header                     = Rcpp::as<std::vector<std::string>>(names);

		// fill map
		for (size_t i = 0; i < _header.size(); ++i) { _map.emplace_back(_header[i], i); }
		std::sort(_map.begin(), _map.end());
	}

	bool isOpen() const noexcept { return true; }
	size_t numCols() const { return instances::rcppData()[_ix].size(); }
	constexpr size_t curLine() const noexcept { return _curLin; }
	const std::string &name() const noexcept { return instances::rcppData().name(_ix); }
	const std::string &delim() const noexcept { return _delim; }
	const std::vector<std::string> &header() const noexcept { return _header; }

	void close() { _curLin = 0; }

	// Ranges interface
	const std::vector<std::string> &front() const {
		_fillLine();
		return _line;
	}

	std::string_view frontRaw() const {
		_fillLineConcat();
		return _lineConcat;
	}

	void popFront() { ++_curLin; }
	bool empty() const { return (int)_curLin > (int)instances::rcppData()[_ix].nrows() - 1; }

	bool hasIndex(std::string_view Key) const {
		auto it = _mapIt(Key);
		return (it != _map.end() && it->first == Key);
	}

	template<typename IContainer = std::vector<size_t>, typename SContainer>
	IContainer indices(const SContainer &Columns) {
		IContainer indices;
		fillIndices(Columns, indices);
		return indices;
	}

	template<typename IContainer, typename SContainer>
	void fillIndices(const SContainer &Columns, IContainer &Indices) {
		static_assert(std::is_integral_v<typename IContainer::value_type>);
		static_assert(isString_v<typename SContainer::value_type>);

		if constexpr (isResizable_v<IContainer>) { // assume vector
			Indices.resize(Columns.size());
		}
		for (size_t i = 0; i < Indices.size(); ++i) { Indices[i] = index(Columns[i]); }
	}

	size_t index(std::string_view Key) const {
		auto it = _mapIt(Key);
		if (it == _map.end() || it->first != Key)
			throw TUserError("Cannot read column with name '", Key, "' in file ", name(), ", on line ", _curLin, "!");
		return it->second;
	}

	// Getters
	template<typename T> T get(size_t I) const { // I is relative position as defined in 'Columns'
		if (I >= numCols())
			throw TUserError("Cannot read column ", I, "' in file ", name(), ", on line ", _curLin, "!");

		// get value as R data type
		const Rcpp::DataFrame &df = instances::rcppData()[_ix];

		// column (vector) types of Rcpp data frame are not known at compile-time
		// -> polymorphism must be achieved via run-time dispatch
		switch (TYPEOF(df[I])) {
		case INTSXP: {
			return _convert<T>(Rcpp::as<Rcpp::IntegerVector>(df[I]), _curLin);
		}
		case REALSXP: {
			return _convert<T>(Rcpp::as<Rcpp::NumericVector>(df[I]), _curLin);
		}
		case LGLSXP: {
			return _convert<T>(Rcpp::as<Rcpp::LogicalVector>(df[I]), _curLin);
		}
		case STRSXP: { // Rcpp::CharacterVector
			std::string val(Rcpp::as<Rcpp::CharacterVector>(df[I])[_curLin]);
			return coretools::str::fromString<T>(val);
		}
		default: {
			// Note: also CPLXSXP (Rcpp::ComplexVector) is invalid here
			throw TDevError("Invalid SEXPTYPE ", TYPEOF(df[I]));
		}
		}
	}

	std::string get(size_t I) const { return get<std::string>(I); }
	template<typename T = std::string> T get(std::string_view Key) const { return get<T>(index(Key)); }

	template<typename T = std::string> void fill(size_t I, T &Value) const { Value = get<T>(I); }
	template<typename T = std::string> void fill(std::string_view Key, T &Value) const { fill(index(Key), Value); }
};

#endif

} // namespace coretools

#endif // TINPUTRCPP_H
