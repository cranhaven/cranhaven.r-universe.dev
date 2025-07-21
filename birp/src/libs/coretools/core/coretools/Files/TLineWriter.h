/*
 * TLineWriter.h
 *
 *  Created on: Sep 26, 2022
 *      Author: Andreas
 */

#ifndef CORE_FILES_TLINEWRITER_H
#define CORE_FILES_TLINEWRITER_H

#include <memory>
#include <string_view>
#include <type_traits>

#include "coretools/Files/TWriter.h"
#include "coretools/Main/TLog.h"
#include "coretools/Main/TError.h"
#include "coretools/traits.h"

#ifndef USE_RCPP
#include "fmt/format.h"
#else
#include "coretools/Strings/toBuffer.h"
#include "coretools/Strings/stringConversions.h"

namespace fmt{
template<typename OutputIt, typename T>
OutputIt format_to(OutputIt out, std::string_view, const T& t, size_t precision) {
	return coretools::str::toBuffer(out, coretools::str::toStringWithPrecision(t, precision));
}	
}
#endif

namespace coretools {

class TLineWriter {
	std::string _buffer;
	std::unique_ptr<TWriter> _writer{new TNoWriter};
	size_t _prec       = 6;
	std::string _delim = ",";

	std::back_insert_iterator<std::string> _back() { return std::back_inserter(_buffer); }

	template<typename Container> void _writeIterable(const Container &vals) {
		static_assert(isIterable_v<Container>);
		if (vals.begin() == vals.end()) return;

		write(vals.begin());
		for (auto it = vals.begin() + 1; it != vals.end(); ++it) {
			write(_delim);
			write(*it);
		}
	}

	void _write(std::string_view sv) { std::copy(sv.begin(), sv.end(), _back()); }
	void _write(char c) { _back() = c; }

	void _write(bool b) {
		constexpr char chs[] = "01";
		_back()              = chs[b];
	}

	void _write(float f) { fmt::format_to(_back(), "{:.{}}", f, _prec); }
	void _write(double d) { fmt::format_to(_back(), "{:.{}}", d, _prec); }

	template<typename T> void _write(T &&val) {
		using RealT = std::remove_cv_t<std::remove_reference_t<T>>;

		if constexpr (hasGetMember_v<RealT>)
			_write(val.get());
		else if constexpr (isIterable_v<RealT> && !isString_v<RealT>)
			_writeIterable(val);
		else if constexpr (std::is_enum_v<RealT>)
			_write(toString(val));
		else
			fmt::format_to(_back(), "{}", val);
	}

public:
	TLineWriter() = default;
	TLineWriter(TWriter *Writer) : _writer(Writer) {}
	TLineWriter(std::string_view Filename) : _writer(makeWriter(Filename, "w")) {}

	TLineWriter(const TLineWriter &)            = delete;
	TLineWriter &operator=(const TLineWriter &) = delete;
	TLineWriter(TLineWriter &&)                 = default;
	TLineWriter &operator=(TLineWriter &&)      = default;

	~TLineWriter() {
		try { // Never throw in destructor
			flush();
		} catch (...) {
			instances::logfile().error("Was not able to finish writing buffer ",
									   _buffer, "to file ", name(), "!");
		}
	}

	void open(TWriter *Writer) {
		user_assert(!isOpen(), "File '", Writer->name(), "' is already open!");

		_writer.reset(Writer);
	}

	void open(std::string_view Filename) { open(makeWriter(Filename, "w")); }

	void close() {
		flush();
		_writer.reset(new TNoWriter);
		_prec = 6;
	}

	bool isOpen() const noexcept { return _writer->isOpen(); }
	const std::string &name() const noexcept { return _writer->name(); }

	template<typename T, typename... Ts> TLineWriter &write(T &&val, Ts &&...vals) {
		_write(std::forward<T>(val));
		if constexpr (sizeof...(Ts) == 0)
			return *this;
		else
			return write(std::forward<Ts>(vals)...);
	}

	template<typename T, typename... Ts> TLineWriter &writeln(T &&val1, Ts &&...vals) {
		write(std::forward<T>(val1), std::forward<Ts>(vals)...);
		endln();
		return *this;
	}

	TLineWriter &pop(size_t N = 1) {
		DEBUG_ASSERT(_buffer.size() >= N);
		_buffer.resize(_buffer.size() - N);
		return *this;
	}

	TLineWriter &flush() {
		if (_buffer.size() > 0) _writer->write(_buffer);
		_buffer.clear();
		return *this;
	}

	TLineWriter &endln() {
		constexpr size_t minBufferSize = 4096;
		_back() = '\n';
		if (_buffer.size() > minBufferSize) { flush(); }
		return *this;
	}

	void precision(size_t Precision) noexcept { _prec = Precision; }
	size_t precision() const noexcept { return _prec; }
	size_t getPosition() const noexcept {return _writer->tell() + _buffer.size();}

	void delim(std::string_view Delim) noexcept { _delim = Delim; }
	const std::string &delim() const noexcept { return _delim; }
};

} // namespace coretools

#endif
