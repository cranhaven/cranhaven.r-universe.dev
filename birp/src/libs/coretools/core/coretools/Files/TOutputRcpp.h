//
// Created by madleina on 08.05.24.
//

#ifndef TOUTPUTRCPP_H
#define TOUTPUTRCPP_H

#include <initializer_list>
#include <iterator>
#include <memory>
#include <type_traits>
#include <string_view>

#include "coretools/Main/TError.h"
#include "coretools/Main/TLog.h"
#include "coretools/Strings/fromString.h"
#include "coretools/Strings/toString.h"
#include "coretools/traits.h"

#ifdef USE_RCPP
#include "coretools/Main/TRcppResults.h"
#include "coretools/arma_include.h" // makes sure order of includes is valid for Rcpp

namespace coretools {

namespace impl {

enum class RType : size_t { Real, Int, Logical, String };

struct TColType {
	size_t positionInDF;
	size_t positionInVec;
	RType type;

	TColType(size_t PositionInDF, size_t PositionInVec, RType Type)
	    : positionInDF(PositionInDF), positionInVec(PositionInVec), type(Type) {}

	bool operator<(const TColType &Other) const { return positionInDF < Other.positionInDF; }
};

} // namespace impl

class TOutputRcpp {

	// 4 vectors for R types
	std::vector<std::vector<double>> _vecReal;
	std::vector<std::vector<int>> _vecInt;
	std::vector<std::vector<bool>> _vecLogical;
	std::vector<std::vector<std::string>> _vecString;

	std::vector<impl::TColType> _cols;

	std::string _filename;
	bool _isOpen    = false;
	size_t _numCols = 0;
	size_t _curCol  = 0;
	size_t _curLin  = 0;
	std::vector<std::string> _header;

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	void _writeHeader(const Container &Header) {
		// just store for later
		for (const auto &h : Header) { _header.push_back((std::string)h); }
	}

	template<typename T> void _addToVec(T &&val) {
		using RealT = std::remove_cv_t<std::remove_reference_t<T>>;

		// add to _cols
		if constexpr (std::is_same_v<RealT, bool>) {
			_cols.emplace_back(_curCol, _vecLogical.size(), impl::RType::Logical);
			_vecLogical.emplace_back();
		} else if constexpr (std::is_floating_point_v<RealT>) {
			_cols.emplace_back(_curCol, _vecReal.size(), impl::RType::Real);
			_vecReal.emplace_back();
		} else if constexpr (std::is_integral_v<RealT>) {
			_cols.emplace_back(_curCol, _vecInt.size(), impl::RType::Int);
			_vecInt.emplace_back();
		} else if constexpr (coretools::isString_v<RealT>) {
			_cols.emplace_back(_curCol, _vecString.size(), impl::RType::String);
			_vecString.emplace_back();
		} else {
			DEVERROR("Incompatible R type for vbalue ", val, "!");
		}
	}

	template<class X, class T> X _convert(T &&val) {
		if constexpr (coretools::isString_v<T>) {
			return coretools::str::fromString<X>(val);
		} else {
			return static_cast<X>(val);
		}
	}

	template<typename T> void _writeValue(T &&val) {
		if (_curLin == 0) { _addToVec(val); }

		switch (_cols[_curCol].type) {
		case impl::RType::Real: {
			_vecReal[_cols[_curCol].positionInVec].push_back(_convert<double>(val));
			break;
		}
		case impl::RType::Int: {
			_vecInt[_cols[_curCol].positionInVec].push_back(_convert<int>(val));
			break;
		}
		case impl::RType::Logical: {
			_vecLogical[_cols[_curCol].positionInVec].push_back(_convert<bool>(val));
			break;
		}
		case impl::RType::String: {
			_vecString[_cols[_curCol].positionInVec].push_back(coretools::str::toString(val));
			break;
		}
		default: DEVERROR("Invalid R type!"); // should never happen
		}
	}

	template<typename T> void _write(T &&val) {
		using RealT = std::remove_cv_t<std::remove_reference_t<T>>;

		if constexpr (hasGetMember_v<RealT>)
			_writeValue(val.get());
		else
			_writeValue(val);
	}

	void _addToGlobalInstances() {
		// create df
		Rcpp::List df(numCols());

		// add header
		if (_header.empty()) { // create default header
			for (size_t i = 0; i < numCols(); ++i) { _header.push_back("V" + coretools::str::toString(i)); }
		}
		df.names() = _header;

		// add columns in correct type
		std::sort(_cols.begin(), _cols.end());
		for (size_t i = 0; i < _cols.size(); ++i) {
			switch (_cols[i].type) {
			case impl::RType::Real: {
				df[_header[i]] = _vecReal[_cols[i].positionInVec];
				break;
			}
			case impl::RType::Int: {
				df[_header[i]] = _vecInt[_cols[i].positionInVec];
				break;
			}
			case impl::RType::Logical: {
				df[_header[i]] = _vecLogical[_cols[i].positionInVec];
				break;
			}
			case impl::RType::String: {
				df[_header[i]] = _vecString[_cols[i].positionInVec];
				break;
			}
			default: {
				DEVERROR("Invalid enum type"); // should never happen
			}
			}
		}

		// add data frame attribute
		df.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

		// add to global instances
		coretools::instances::rcppResults().add(_filename, df);
	}

public:
	TOutputRcpp() = default; // Do we want a default constructor?

	TOutputRcpp(std::string_view Filename, size_t NumCols, std::string_view Delim = "\t") {
		open(Filename, NumCols, Delim);
	}

	// set custom delim without numCols
	TOutputRcpp(std::string_view Filename, std::string_view Delim = "\t") : TOutputRcpp(Filename, 0, Delim) {}

	TOutputRcpp(std::string_view Filename, std::initializer_list<std::string_view> Header,
	            std::string_view Delim = "\t")
	    : TOutputRcpp(Filename, Header.size(), Delim) {
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	TOutputRcpp(std::string_view Filename, const Container &Header, std::string_view Delim = "\t")
	    : TOutputRcpp(Filename, Header.size(), Delim) {
		_writeHeader(Header);
	}

	~TOutputRcpp() { close(); }

	void open(std::string_view Filename, size_t NumCols, std::string_view /*Delim*/ = "\t") {
		if (isOpen()) UERROR("File '", Filename, "' is already open!");

		_filename = Filename;
		_numCols  = NumCols;
		_isOpen   = true;
	}

	void open(std::string_view Filename, std::string_view Delim = "\t") { open(Filename, 0, Delim); }

	void open(std::string_view Filename, std::initializer_list<std::string_view> Header,
	          std::string_view Delim = "\t") {
		open(Filename, Header.size(), Delim);
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	void open(std::string_view Filename, const Container &Header, std::string_view Delim = "\t") {
		open(Filename, Header.size(), Delim);
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	void writeHeader(const Container &Header) {
		_numCols = Header.size();
		_writeHeader(Header);
	}

	void writeHeader(std::initializer_list<std::string_view> Header) {
		_numCols = Header.size();
		_writeHeader(Header);
	}

	void numCols(size_t NumCols) {
		if (_curLin > 0 || (_numCols > 0 && _curCol > 0))
			DEVERROR("Can not set number of columns of file '", name(), "': first line has already been processed!");
		_numCols = NumCols;
	}

	void precision(size_t /*Precision*/) noexcept { /* ignore */ }

	bool isOpen() const noexcept { return _isOpen; }
	constexpr size_t curCol() const noexcept { return _curCol; }
	constexpr size_t numCols() const noexcept { return _numCols; }
	constexpr size_t curLine() const noexcept { return _curLin; }
	const std::string &name() const noexcept { return _filename; }
	constexpr bool isTable() const noexcept { return true; } // only allow table in Rcpp mode

	void close() {
		if (_isOpen) {
			_addToGlobalInstances();

			_numCols = 0;
			_curCol  = 0;
			_curLin  = 0;
			_isOpen  = false;
		}
	}

	void writeDelim() { ++_curCol; }

	template<typename T, typename... Ts> TOutputRcpp &write(T &&val1, Ts &&...vals) {
		if constexpr (isIterable_v<T> && !isString_v<T>) {
			// val1 is a Container but not a string
			for (const auto &c : val1) {
				_write(c);
				writeDelim();
			}
		} else {
			_write(std::forward<T>(val1));
			writeDelim();
		}
		if constexpr (sizeof...(Ts) == 0)
			return *this;
		else
			return write(std::forward<Ts>(vals)...);
	}

	template<typename T, typename... Ts> TOutputRcpp &writeln(T &&val1, Ts &&...vals) {
		write(std::forward<T>(val1), std::forward<Ts>(vals)...);
		endln();
		return *this;
	}

	TOutputRcpp &endln() {
		if (_curLin == 0 && _numCols == 0) { _numCols = _curCol; } // remember now and forever how many cols there are
		if (_curCol != _numCols) {
			DEVERROR("Can not end line in file '", name(), "': expected ", _numCols, " columns, got ", _curCol, "!");
		}

		// Go to next line
		++_curLin;
		_curCol = 0;

		return *this;
	}

	// Imitate iostream nonsense
	template<typename T> TOutputRcpp &operator<<(T &&val) { return write(std::forward<T>(val)); }

	// operator<< to take in coretools::endl
	TOutputRcpp &operator<<(TOutputRcpp &(*manip)(TOutputRcpp &)) { return manip(*this); };

	// operator<< to take in std::endl
	// WARNING! also does endl when given std::flush, std::ends, ...
	[[deprecated("use coretools::endl for safety!")]] TOutputRcpp &operator<<(std::ostream &(*)(std::ostream &)) {
		return endln();
	};
};

// use this instead of std::endl!
inline TOutputRcpp &endl(TOutputRcpp &of) {
	of.endln();
	return of;
}

} // namespace coretools

#endif

#endif // TOUTPUTRCPP_H
