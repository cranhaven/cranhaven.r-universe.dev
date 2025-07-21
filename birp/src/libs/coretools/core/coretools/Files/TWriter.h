/*
 * TWriter.h
 *
 *  Created on: Nov 21, 2022
 *      Author: Andreas
 */

#ifndef CORE_FILES_TWRITER_H
#define CORE_FILES_TWRITER_H

#include "coretools/Main/TError.h"
#include "coretools/traits.h"

namespace coretools {

class TWriter {
	std::string _filename;
protected:
	virtual void _write(const void *buffer, size_t size, size_t count) = 0;
	virtual int64_t _tell() const                                      = 0;

public:
	TWriter(std::string_view Filename) : _filename(Filename) {}
	virtual ~TWriter() = default;

	template<typename T> void write(const T &val) {
		if constexpr (isIterable_v<T>) {
			_write(val.data(), sizeof(typename T::value_type), val.size());
		} else if (std::is_pointer_v<T>) {
			static_assert(!std::is_pointer_v<T>, "Use a view instead of a raw pointer");
		} else {
			_write(&val, sizeof(T), 1);
		}
	}

	bool isOpen() const noexcept {return !_filename.empty();}
	const std::string &name() const noexcept {return _filename;}
	int64_t tell() const {return _tell();}
};

class TNoWriter final : public TWriter {
	void _write(const void *, size_t, size_t) override { throw TDevError("No Output File was defined!"); };
	int64_t _tell() const override { throw TDevError("No Output File was defined!");};

public:
	TNoWriter() : TWriter("") {}
};

TWriter* makeWriter(std::string_view Filename, const char* Mode);

} // namespace coretools

#endif
