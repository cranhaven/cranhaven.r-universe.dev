/*
 * TLog.h
 *
 *  Created on: Oct 17, 2010
 *      Author: wegmannd
 */

#ifndef TLOG_H_
#define TLOG_H_

#include <ctime>
#include <fstream>
#include <ostream>
#include <vector>

#include "coretools/Main/TError.h"
#include "coretools/Strings/toString.h"
#include "coretools/TTimer.h"

#ifdef USE_RCPP
#include "coretools/arma_include.h" // makes sure order of includes is valid for Rcpp
#else
#include <iostream>
#endif

template<typename E> std::enable_if_t<std::is_enum_v<E>, std::ostream &> operator<<(std::ostream &os, E e) {
	using coretools::str::toString;
	os << toString(e);
	return os;
}

namespace coretools {

// define a stream reference for out: works for both Rcpp and command-line mode
inline std::ostream &cout =
#ifdef USE_RCPP
    Rcpp::Rcout;
#else
    std::cout;
#endif

// define a stream reference for errors: works for both Rcpp and command-line mode
inline std::ostream &cerr =
#ifdef USE_RCPP
    Rcpp::Rcerr;
#else
    std::cerr;
#endif

//------------------------------------------------
// TLog
//------------------------------------------------

enum class VerboseLevel { min = 0, silent = min, header_only, verbose, max = 3 };

class TLog {
private:
	std::ofstream _file;
	bool _isFile;
	std::string _filename;
	VerboseLevel _verbose_level;
	bool _printWarnings;
	long _lastLineStartInFile;

	// string constants
	std::string _indentSymbol;
	std::string _listSymbol;
	std::string _concludeSymbol;
	std::string _numberSymbol;

	// Indent
	int _numIndent;
	std::string _indentOnlyTabs;

	void _fillIndentString() {
		_indentOnlyTabs = "";
		for (int i = 0; i < _numIndent; ++i) _indentOnlyTabs += _indentSymbol;
	}

	template<typename... Ts> std::string _listString(const Ts &...out) {
		return _indentOnlyTabs + _indentSymbol + _listSymbol + str::toString(out...);
	}

	template<typename... Ts> std::string _concludeString(const Ts &...out) {
		return _indentOnlyTabs + _indentSymbol + _concludeSymbol + str::toString(out...);
	}

	// numbering
	int _numberingLevel;
	std::vector<int> _numberingIndex;

	// timing
	std::vector<TTimer> _timerVec;
	void _startNewTimer() { _timerVec.emplace_back(); }

	std::string _endMostRecentTimer() {
		DEV_ASSERT(!_timerVec.empty());

		_timerVec.back().stop();
		std::string tmp = _timerVec.back().formattedTime();
		_timerVec.pop_back();
		return tmp;
	}

	template<typename... Ts> std::string _numberString(const Ts &...out) {
		std::string s = _indentOnlyTabs + str::toString(_numberingIndex[_numberingLevel]) + _numberSymbol;
		// make sure numbers up to 99 align
		if (_numberingIndex[_numberingLevel] < 10) s += ' ';

		return s + str::toString(out...);
	}

	// general function to write
	void _write(std::string_view Out, bool ToFile = true, bool ToOut = true) {
		if (ToFile && _isFile) {
			_file << Out << std::endl;
			_lastLineStartInFile = _file.tellp();
		}
		if (ToOut && _verbose_level > VerboseLevel::header_only) { cout << Out << std::endl; }
	}

	void _overWrite(std::string_view out, bool ToFile = true, bool ToOut = true) {
		if (ToFile && _isFile) {
			_file.seekp(_lastLineStartInFile);
			_file << out << std::endl;
			_lastLineStartInFile = _file.tellp();
		}
		if (ToOut && _verbose_level > VerboseLevel::header_only) { cout << '\xd' << out << std::endl; }
	}

	void _flush(std::string_view out, bool ToFile = true, bool ToOut = true) {
		if (ToFile && _isFile) _file << out << std::flush;
		if (ToOut && _verbose_level > VerboseLevel::header_only) {
			if (ToOut && _verbose_level > VerboseLevel::header_only) { cout << out << std::flush; }
		}
	}

	void _overFlush(std::string_view out, bool ToFile = true, bool ToOut = true) {
		if (ToFile && _isFile) {
			_file.seekp(_lastLineStartInFile);
			_file << out << std::flush;
		}
		if (ToOut && _verbose_level > VerboseLevel::header_only) { cout << '\xd' << out << std::flush; }
	}

	void _writeError(std::string_view out) {
		if (_isFile) {
			_file << out << std::endl << std::endl;
			_lastLineStartInFile = _file.tellp();
		}
		// always write, even if verbose level is 0
		cerr << std::endl << out << std::endl << std::endl;
	}

	// Init
	void _init() {
		_isFile              = false;
		_verbose_level       = VerboseLevel::verbose;
		_printWarnings       = true;
		_numIndent           = 0;
		_lastLineStartInFile = 0;
		_fillIndentString();
		_numberingLevel = -1;
	}

public:
	TLog() {
		_init();
		setDefaultSymbols();
	}

	TLog(std::string_view IndentSymbol, std::string_view ListSymbol, std::string_view ConcludeSymbol,
	     std::string_view NumberSymbol) {
		_init();
		setSymbols(IndentSymbol, ListSymbol, ConcludeSymbol, NumberSymbol);
	}

	TLog(std::string_view Filename) {
		_init();
		setDefaultSymbols();
		openFile(Filename);
	}

	void close() {
		newLine();
		if (_isFile) _file.close();
		_isFile = false;
	}

	void setDefaultSymbols() {
		_indentSymbol   = "   ";
		_listSymbol     = "- ";
		_concludeSymbol = "  -> ";
		_numberSymbol   = ") ";
	}

	void setSymbols(std::string_view IndentSymbol = "   ", std::string_view ListSymbol = "- ",
	                std::string_view ConcludeSymbol = "-> ", std::string_view NumberSymbol = ") ") {
		setIndentSymbol(IndentSymbol);
		setListSymbol(ListSymbol);
		setConcludeSymbol(ConcludeSymbol);
		setNumberSymbol(NumberSymbol);
	}

	void setIndentSymbol(std::string_view IndentSymbol) { _indentSymbol = IndentSymbol; }
	void setListSymbol(std::string_view ListSymbol) { _listSymbol = ListSymbol; }
	void setConcludeSymbol(std::string_view ConcludeSymbol) { _concludeSymbol = ConcludeSymbol; }
	void setNumberSymbol(std::string_view NumberSymbol) { _numberSymbol = NumberSymbol; }

	~TLog() { close(); }

	void openFile(std::string_view Filename) {
		list("Writing log to '", Filename, "'");
		_filename = Filename;
		_file.open(_filename.c_str());

		user_assert(_file.is_open(), "Unable to open logfile '", _filename, "'!");
		_isFile              = true;
		_lastLineStartInFile = _file.tellp();
	}

	void setVerboseLevel(VerboseLevel level) { _verbose_level = level; }
	void setVerboseLevel(size_t level) {
		if (level == 0) { // completely silent, never report
			_verbose_level = VerboseLevel::silent;
		} else if (level == 1) {
			_verbose_level = VerboseLevel::header_only;
		} else { // default or level >= 2
			_verbose_level = VerboseLevel::verbose;
		}
	}

	VerboseLevel verbose() const { return _verbose_level; }
	void suppressWarings() { _printWarnings = false; }
	void showWarings() { _printWarnings = true; }

	void newLine() {
		if (_isFile) {
			_file << std::endl;
			_lastLineStartInFile = _file.tellp();
		}
		if (_verbose_level > VerboseLevel::silent) { cout << std::endl; }
	}

	std::string getFilename() {
		if (_isFile)
			return _filename;
		else
			return "";
	}

	template<typename T> void add(const T &out) {
		if (_isFile) _file << out;
		if (_verbose_level > VerboseLevel::header_only) { cout << out; }
	}

	void printHeader(std::string_view title, std::string_view name_executable, std::string_view log_str_task,
	                 std::string_view arg_ver) {
		if (_verbose_level == VerboseLevel::silent) { return; }
		// do not use list() etc. functions here because these would not get printed in header_only mode
		newLine();
		cout << title << std::endl;
		cout << _listString("Used executable: ", name_executable) << std::endl;
		if (!log_str_task.empty()) { cout << _listString(log_str_task) << std::endl; }
		if (_verbose_level == VerboseLevel::header_only) {
			cout << _listString("Running in minimal logging mode (omit argument '", arg_ver,
			                    "' to get a status report on screen)")
			     << std::endl;
		}
	}

	//---------------------------------------------------------
	// write, list, conclude
	//---------------------------------------------------------
	template<typename... Ts> void write(const Ts &...out) { _write(str::toString(out...)); }

	void done() { _write(" done!"); }

	void doneTime() { _write(" done (in " + _endMostRecentTimer() + ")!"); }

	template<typename... Ts> void list(const Ts &...out) { _write(_listString(out...)); }

	template<typename... Ts> void conclude(const Ts &...out) { _write(_concludeString(out...)); }

	//---------------------------------------------------------
	// flush, listFlush, ...
	//---------------------------------------------------------
	template<typename... Ts> void flush(const Ts &...out) { _flush(str::toString(out...)); }

	template<typename... Ts> void listFlush(const Ts &...out) { _flush(_listString(out...)); }

	template<typename... Ts> void listFlushDots(const Ts &...out) { _flush(_listString(out...) + " ..."); }

	template<typename... Ts> void listFlushTime(const Ts &...out) {
		_startNewTimer();
		_flush(_listString(out...));
	}

	//---------------------------------------------------------
	// Indent
	//---------------------------------------------------------
	void addIndent(int n = 1) {
		_numIndent += n;
		_fillIndentString();
	}

	void removeIndent(int n = 1) {
		_numIndent -= n;
		if (_numIndent < 0) _numIndent = 0;
		_fillIndentString();
	}

	void clearIndent() {
		_numIndent = 0;
		_fillIndentString();
	}

	template<typename... Ts> void startIndent(const Ts &...out) {
		list(out...);
		addIndent();
	}

	template<typename... Ts> void endIndent(const Ts &...out) {
		list(out...);
		removeIndent();
	}

	void endIndent() { removeIndent(); }

	//---------------------------------------------------------
	// Numbering
	//---------------------------------------------------------
	void addNumberingLevel() {
		++_numberingLevel;
		_numberingIndex.push_back(1);
		addIndent();
	}

	void removeNumberingLevel() {
		if (_numberingLevel >= 0) {
			_numberingIndex.erase(_numberingIndex.end() - 1);
			--_numberingLevel;
			removeIndent();
		}
	}

	template<typename... Ts> void startNumbering(const Ts &...out) {
		list(out...);
		addNumberingLevel();
	}

	void endNumbering() { removeNumberingLevel(); }

	template<typename... Ts> void endNumbering(const Ts &...out) {
		number(out...);
		removeNumberingLevel();
	}

	template<typename... Ts> void number(const Ts &...out) {
		_write(_numberString(out...));
		++_numberingIndex[_numberingLevel];
	}

	template<typename... Ts> void numberWithIndent(const Ts &...out) {
		number(out...);
		addIndent();
	}

	template<typename... Ts> void numberFlush(const Ts &...out) {
		_flush(_numberString(out...));
		++_numberingIndex[_numberingLevel];
	}

	//---------------------------------------------------------
	// overWrite, overList
	//---------------------------------------------------------
	template<typename... Ts> void overWrite(const Ts &...out) { _overWrite(str::toString(out...)); }

	template<typename... Ts> void overList(const Ts &...out) { _overWrite(_listString(out...)); }

	template<typename... Ts> void overFlush(const Ts &...out) { _overFlush(str::toString(out...)); }

	template<typename... Ts> [[deprecated("use overListFlush")]] void listOverFlush(const Ts &...out) {
		_overFlush(_listString(out...));
	}

	// identical as above but with more meanignful name
	template<typename... Ts> void overListFlush(const Ts &...out) { _overFlush(_listString(out...)); }

	template<typename... Ts> void overNumber(const Ts &...out) { _overWrite(_numberString(out...)); }

	template<typename... Ts> void overNumberFlush(const Ts &...out) { _overFlush(_numberString(out...)); }

	//---------------------------------------------------------
	// file only, std::out only
	//---------------------------------------------------------
	template<typename... Ts> void writeFileOnly(const Ts &...out) { _write(str::toString(out...), true, false); }

	template<typename... Ts> void listFileOnly(const Ts &...out) { _write(_listString(out...), true, false); }

	template<typename... Ts> void flushFileOnly(const Ts &...out) { _flush(str::toString(out...), true, false); }

	template<typename... Ts> void listFlushFileOnly(const Ts &...out) { _flush(_listString(out...), true, false); }

	template<typename... Ts> void writeNoFile(const Ts &...out) { _write(str::toString(out...), false, true); }

	template<typename... Ts> void listNoFile(const Ts &...out) { _write(_listString(out...), false, true); }

	template<typename... Ts> void flushNoFile(const Ts &...out) { _flush(str::toString(out...), false, true); }

	template<typename... Ts> void listFlushNoFile(const Ts &...out) { _flush(_listString(out...), false, true); }

	//---------------------------------------------------------
	// warning / error
	//---------------------------------------------------------
	template<typename... Ts> void warning(const Ts &...out) {
		if (_printWarnings) { _writeError("WARNING: " + str::toString(out...)); }
	}

	template<typename... Ts> void error(const Ts &...out) { _writeError("ERROR: " + str::toString(out...)); }

	//---------------------------------------------------------
	// fixed width
	//---------------------------------------------------------

	template<typename T> void flushFixedWidth(const T &out, std::streamsize width) {
		if (_isFile) {
			std::streamsize old_width = _file.width(width);
			_file << out << std::flush;
			_file.width(old_width);
		}
		if (_verbose_level > VerboseLevel::header_only) {
			std::streamsize old_width = cout.width(width);
			cout << out << std::flush;
			cout.width(old_width);
		}
	}

	template<typename T> void flushNumberFixedWidth(const T &out, std::streamsize precision, std::streamsize width) {
		if (_isFile) {
			std::streamsize old_precision = _file.precision(precision);
			std::streamsize old_width     = _file.width(width);
			_file << std::fixed << out << std::defaultfloat << std::flush;
			_file.width(old_width);
			_file.precision(old_precision);
		}
		if (_verbose_level > VerboseLevel::header_only) {
			std::streamsize old_precision = cout.precision(precision);
			std::streamsize old_width     = cout.width(width);
			cout << std::fixed << out << std::defaultfloat << std::flush;
			cout.width(old_width);
			cout.precision(old_precision);
		}
	}

	template<typename T> void writeFixedWidth(const T &out, std::streamsize width) {
		if (_isFile) {
			std::streamsize old_width = _file.width(width);
			_file << out << std::endl;
			_file.width(old_width);
			_lastLineStartInFile = _file.tellp();
		}
		if (_verbose_level > VerboseLevel::header_only) {
			std::streamsize old_width = cout.width(width);
			cout << out << std::endl;
			cout.width(old_width);
		}
	}

	template<typename T> void flushScientific(const T &out, std::streamsize precision, std::streamsize width) {
		if (_isFile) {
			std::streamsize old_precision = _file.precision(precision);
			std::streamsize old_width     = _file.width(width);
			_file << std::scientific << out << std::defaultfloat << std::flush;
			_file.width(old_width);
			_file.precision(old_precision);
		}
		if (_verbose_level > VerboseLevel::header_only) {
			std::streamsize old_precision = cout.precision(precision);
			std::streamsize old_width     = cout.width(width);
			cout << std::scientific << out << std::defaultfloat << std::flush;
			cout.width(old_width);
			cout.precision(old_precision);
		}
	}

	template<typename T> void writeScientific(const T &out, std::streamsize precision) {
		if (_isFile) {
			std::streamsize old_precision = cout.precision(precision);
			_file << std::scientific << out << std::defaultfloat << std::endl;
			_file.precision(old_precision);
			_lastLineStartInFile = _file.tellp();
		}
		if (_verbose_level > VerboseLevel::header_only) {
			std::streamsize old_precision = cout.precision(precision);
			cout << std::scientific << out << std::defaultfloat << std::endl;
			cout.precision(old_precision);
		}
	}
};

namespace instances {
inline TLog &logfile() {
	static TLog log;
	return log;
}
} // namespace instances

} // namespace coretools

#endif /* TLOG_H_ */
