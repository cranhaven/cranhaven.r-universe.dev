#ifndef CORE_FILES_TINTPUTFILE_H
#define CORE_FILES_TINTPUTFILE_H

#include <string_view>
#include <type_traits>

#include "coretools/Files/TLineReader.h"
#include "coretools/Files/TReader.h"
#include "coretools/Main/TError.h"
#include "coretools/Strings/fromString.h"
#include "coretools/Strings/splitters.h"
#include "coretools/Strings/stringManipulations.h"
#include "coretools/Strings/toString.h"
#include "coretools/traits.h"
#include "coretools/Files/TInputRcpp.h"

namespace coretools {

class TInputFile {
	using Splitter = str::TSplitter<std::string_view, true>;

	TLineReader _reader;
	std::string _delim{str::whitespaces};
	std::string _comment = "#";
	size_t _curLin       = 0;

	std::vector<std::string> _header;
	std::vector<std::pair<std::string, size_t>> _map;

	mutable std::vector<std::string_view> _line;

	auto _mapIt(std::string_view Key) const {
		return std::lower_bound(_map.begin(), _map.end(), Key, [](const auto &p, std::string_view K) { return p.first < K; });
	}

	void _pseudoHeader() {
		Splitter spl(_reader.front(), _delim);
		for (size_t i = 0; !spl.empty(); ++i, spl.popFront()) {
			_header.push_back(str::toString(i));
			_map.emplace_back(_header.back(), i);
		}
		std::sort(_map.begin(), _map.end());
	}

	void _parseHeader() {
		Splitter spl(_reader.front(), _delim);
		for (size_t i = 0; !spl.empty(); ++i, spl.popFront()) {
			_header.emplace_back(spl.front());
			_map.emplace_back(_header.back(), i);
		}
		std::sort(_map.begin(), _map.end());
		_reader.popFront();
	}

	void _parseLine() const {
		if (!_line.empty()) return; // already parsed

		Splitter spl(str::strip(str::readBefore(_reader.front(), _comment)), _delim);
		for (const auto s: spl) _line.push_back(s);
	}

public:
	TInputFile() = default;

	// All lines
	TInputFile(TReader *Reader, FileType FType, std::string_view Delim = str::whitespaces, std::string_view Comment = "#")
		: _reader(Reader), _delim(Delim), _comment(Comment) {
		if (FType == FileType::Header) _parseHeader();
		else _pseudoHeader();
	}
	TInputFile(std::string_view FileName, FileType FType, std::string_view Delim = str::whitespaces, std::string_view Comment = "#")
		: TInputFile(makeReader(FileName), FType, Delim, Comment) {}


	void open(TReader *Reader, FileType FType, std::string_view Delim = str::whitespaces, std::string_view Comment = "#") {
		_reader.open(Reader);
		_delim   = Delim;
		_comment = Comment;
		if (FType == FileType::Header) _parseHeader();
		else _pseudoHeader();
	}
	void open(std::string_view FileName, FileType FType, std::string_view Delim = str::whitespaces, std::string_view Comment = "#") {
		open(makeReader(FileName), FType, Delim, Comment);
	}

	bool isOpen() const noexcept {return _reader.isOpen();}
	size_t numCols() const { return front().size(); }
	constexpr size_t curLine() const noexcept { return _curLin; }
	const std::string &name() const noexcept { return _reader.name(); }
	const std::string& delim() const noexcept {return _delim;}
	const std::vector<std::string>& header() const noexcept {return _header;}

	void close() {
		_reader.close();
	}


	// Ranges interface
	const std::vector<std::string_view>& front() const {
		_parseLine();
		return _line;
	}
	std::string_view frontRaw() const {
		return _reader.front();
	}

	void popFront() {
		// ignore empty lines
		do { _reader.popFront(); } while (!_reader.empty() && _reader.front().empty());

		_line.clear();
		++_curLin;
	}
	bool empty() const {return _reader.empty();}

	bool hasIndex(std::string_view Key) const {
		auto it = _mapIt(Key);
		return (it != _map.end() && it->first == Key);
	}

	template<typename SContainer=std::vector<std::string>>
	size_t hasOneOfTheseIndeces(const SContainer& Columns) const {
		for (size_t i = 0; i < Columns.size(); ++i) {
			if(hasIndex(Columns[i])){
				return true;
			}
		}
		return false;
	}

	template<typename IContainer = std::vector<size_t>, typename SContainer=std::vector<std::string>>
	IContainer indices(const SContainer& Columns) {
		IContainer indices;
		fillIndices(Columns, indices);
		return indices;
	}

	template<typename IContainer, typename SContainer>
	void fillIndices(const SContainer& Columns, IContainer& Indices) {
		static_assert(std::is_integral_v<typename IContainer::value_type>);
		static_assert(isString_v<typename SContainer::value_type>);

		if constexpr (isResizable_v<IContainer>) { // assume vector
			Indices.resize(Columns.size());
		}
		for (size_t i = 0; i < Indices.size(); ++i) {
			Indices[i] = index(Columns[i]);
		}
	}
	
	size_t index(std::string_view Key) const {
		auto it = _mapIt(Key);
		if (it == _map.end() || it->first != Key)
			throw TUserError("File ", name(), " has no column with name '", Key, "'!");

		return it->second;
	}

	template<typename SContainer=std::vector<std::string>>
	size_t indexOfFirstMatch(const SContainer& Columns) const {
		for (size_t i = 0; i < Columns.size(); ++i) {
			if(hasIndex(Columns[i])){
				return index(Columns[i]);
			}
		}
		throw TUserError("File ", name(), " has no column with any of the keys ", str::toString(Columns), "!");
	}

	// Getters
	std::string_view get(size_t I) const { //I is relative position as defined in 'Columns'
		_parseLine();
		user_assert(I < _line.size(), "Cannot read column ", I, "' in file ", name(), ", on line ", _curLin, "!");
		return _line[I];
	}

	template<typename T>
	T get(size_t I) const { return str::fromString<T, true>(get(I)); }
	template<typename T = std::string_view>
	T get(std::string_view Key) const {return get<T>(index(Key));}

	template<typename T = std::string_view> void fill(size_t I, T &Value) const {
		Value = get<T>(I);
	}
	template<typename T = std::string_view>
	void fill(std::string_view Key, T& Value) const {fill(index(Key), Value);}
};

// compile-time alias for decision reading from Rcpp or file
#ifdef USE_RCPP
using TInputMaybeRcppFile = TInputRcpp;
#else
using TInputMaybeRcppFile = TInputFile;
#endif

}

#endif
