/*
 * TOutputFile.h
 *
 *  Created on: Sep 26, 2022
 *      Author: Andreas
 */

#ifndef CORE_FILES_TOUTPUTFILE_H
#define CORE_FILES_TOUTPUTFILE_H

#include <initializer_list>
#include <type_traits>
#include <string_view>

#ifdef USE_RCPP
#include "coretools/Files/TOutputRcpp.h"
#else
#include <fmt/format.h>
#include <fmt/os.h>
#endif

#include "coretools/Files/TLineWriter.h"
#include "coretools/Main/TError.h"
#include "coretools/traits.h"

namespace coretools {

class TOutputFile {
	TLineWriter _writer;

	std::string _delim = "\t";
	size_t _numCols    = 0;
	size_t _curCol     = 0;
	size_t _curLin     = 0;
	bool _delimAtBack  = false;

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	void _writeHeader(const Container &Header) {
		if (_curLin > 0 || _curCol > 0) // make sure we don't write stuff first and then the header
			DEVERROR("Cannot write header in file '", name(), "': first lines have already been written!");

		for (const auto& h: Header) {
			_writer.write(h);
			writeDelim();
		}
		endln();
	}

public:
	TOutputFile() = default; // Do we want a default constructor?

	// Most general
	TOutputFile(TWriter *Writer, size_t NumCols, std::string_view Delim = "\t")
		: _writer(Writer), _delim(Delim), _numCols(NumCols){}

	TOutputFile(std::string_view Filename, size_t NumCols, std::string_view Delim = "\t")
		: TOutputFile(makeWriter(Filename, "w"), NumCols, Delim){}

	// set custom delim without numCols
	TOutputFile(std::string_view Filename, std::string_view Delim = "\t") : TOutputFile(Filename, 0, Delim) {}
	TOutputFile(TWriter* Writer, std::string_view Delim = "\t") : TOutputFile(Writer, 0, Delim) {}

	// with header
	TOutputFile(TWriter* Writer, std::initializer_list<std::string_view> Header, std::string_view Delim = "\t") : TOutputFile(Writer, Header.size(), Delim) {
		_writeHeader(Header);
	}

	TOutputFile(std::string_view Filename, std::initializer_list<std::string_view> Header, std::string_view Delim = "\t") : TOutputFile(Filename, Header.size(), Delim) {
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	TOutputFile(TWriter* Writer, const Container& Header, std::string_view Delim = "\t") : TOutputFile(Writer, Header.size(), Delim) {
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	TOutputFile(std::string_view Filename, const Container& Header, std::string_view Delim = "\t") : TOutputFile(Filename, Header.size(), Delim) {
		_writeHeader(Header);
	}

	void open(TWriter* Writer, size_t NumCols, std::string_view Delim = "\t") {
		_delim   = Delim;
		_numCols = NumCols;
		_writer.open(Writer);
	}

	void open(std::string_view Filename, size_t NumCols, std::string_view Delim = "\t") {
		if (isOpen()) UERROR("File '", Filename, "' is already open!");
		open(makeWriter(Filename, "w"), NumCols, Delim);
	}

	void open(TWriter* Writer, std::string_view Delim = "\t") {
		open(Writer, 0, Delim);
	}

	void open(std::string_view Filename, std::string_view Delim = "\t") {
		open(Filename, 0, Delim);
	}

	void open(TWriter* Writer, std::initializer_list<std::string_view> Header, std::string_view Delim = "\t") {
		open(Writer, Header.size(), Delim);
		_writeHeader(Header);
	}

	void open(std::string_view Filename, std::initializer_list<std::string_view> Header, std::string_view Delim = "\t") {
		open(Filename, Header.size(), Delim);
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	void open(TWriter* Writer, const Container& Header, std::string_view Delim = "\t") {
		open(Writer, Header.size(), Delim);
		_writeHeader(Header);
	}

	template<typename Container, std::enable_if_t<isIterable_v<Container> && !isString_v<Container>, bool> = true>
	void open(std::string_view Filename, const Container& Header, std::string_view Delim = "\t") {
		open(Filename, Header.size(), Delim);
		_writeHeader(Header);
	}

	// No point to have append function to TWriter, just open it correctly with "a" flag
	void append(std::string_view Filename, size_t NumCols, std::string_view Delim = "\t") {
		if (isOpen()) UERROR("File '", Filename, "' is already open!");
		_numCols = NumCols;
		_delim   = Delim;
		_writer.open(makeWriter(Filename, "a"));
	}

	void append(std::string_view Filename, std::string_view Delim="\t") {
		append(Filename, 0, Delim);
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

	void precision(size_t Precision) noexcept {_writer.precision(Precision);}
	void flush() {_writer.flush();}

	bool isOpen() const noexcept {return _writer.isOpen();}
	constexpr size_t curCol() const noexcept {return _curCol;}
	constexpr size_t numCols() const noexcept {return _numCols;}
	constexpr size_t curLine() const noexcept {return _curLin;}
	const std::string& name() const noexcept {return _writer.name();}
	constexpr bool isTable() const noexcept {return _numCols > 0;}
	const std::string& delim() const noexcept {return _delim;}
	size_t precision() const noexcept {return _writer.precision();}


	void close() {
		_writer.close();
		_delim   = "\t";
		_numCols = 0;
		_curCol  = 0;
		_curLin  = 0;
	}

	void writeDelim() {
		_writer.write(_delim);
		++_curCol;
		_delimAtBack = true;
	}

	template<typename T, typename... Ts>
	TOutputFile& write(T&& val1, Ts && ...vals) {
		if constexpr (isIterable_v<T> && !isString_v<T>) {
			// val1 is a Container but not a string
			for (const auto &c : val1) {
				_writer.write(c);
				writeDelim();
			}
		} else {
			_writer.write(std::forward<T>(val1));
			writeDelim();
		}
		if constexpr (sizeof...(Ts) == 0) return *this;
		else return write(std::forward<Ts>(vals)...);
	}

	template<typename T, typename... Ts>
	TOutputFile& writeln(T&& val1, Ts && ...vals) {
		write(std::forward<T>(val1), std::forward<Ts>(vals)...);
		endln();
		return *this;
	}

	template<typename T, typename... Ts>
	TOutputFile& writeNoDelim(T&& val1, Ts && ...vals) {
		if constexpr (isIterable_v<T> && !isString_v<T>) {
			// val1 is a Container but not a string
			for (const auto &c : val1) {
				_writer.write(c, ',');
			}
		} else {
			_writer.write(std::forward<T>(val1));
		}
		if constexpr (sizeof...(Ts) == 0) {
			_delimAtBack = false;
			return *this;
		} else {
			return writeNoDelim(std::forward<Ts>(vals)...);
		}
	}

	TOutputFile& endln() {
		if (isTable() && (_curCol != _numCols)) {
			// Destructor will write buffer to file
			DEVERROR("Can not end line in file '", name(), "': expected ", _numCols, " columns, got ", _curCol, "!");
		}

		if (_delimAtBack) { // Only pop if data has been writen
			_writer.pop(_delim.size());
			_delimAtBack = false;
		}

		// Add newline
		_writer.endln();
		++_curLin;
		_curCol = 0;

		return *this;
	}

	// Imitate iostream nonsense
	template<typename T>
	TOutputFile& operator<<(T&& val) {
		return write(std::forward<T>(val));
	}

	// operator<< to take in coretools::endl
	TOutputFile &operator<<(TOutputFile& (*manip)(TOutputFile&)) {
		return manip(*this);
	};

	// operator<< to take in std::endl
	// WARNING! also does endl when given std::flush, std::ends, ...
	[[deprecated("use coretools::endl for safety!")]] TOutputFile &operator<<(std::ostream &(*)(std::ostream &)) {
		this->endln();
		return *this;
	};
};

// use this instead of std::endl!
inline TOutputFile &endl(TOutputFile &of) {
	of.endln();
	return of;
}

// compile-time alias for decision reading from Rcpp or file
#ifdef USE_RCPP
using TOutputMaybeRcppFile = TOutputRcpp;
#else
using TOutputMaybeRcppFile = TOutputFile;
#endif

} // namespace coretools

#endif
