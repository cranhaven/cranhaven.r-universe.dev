/*
 * TReader.h
 *
 *  Created on: Sep 26, 2022
 *      Author: Andreas
 */

#ifndef CORE_FILES_TREADER_H
#define CORE_FILES_TREADER_H

#include <string_view>

#include "coretools/Main/TError.h"

namespace coretools {

class TReader {
	std::string _filename;

protected:
	virtual size_t _read(void *buffer, size_t size, size_t count) = 0;
	virtual int64_t _tell() const                                 = 0;
	virtual void _seek(int64_t pos)                               = 0;
	virtual bool _eof() const                                     = 0;

public:
	TReader(std::string_view Filename) : _filename(Filename) {}
	virtual ~TReader() = default;

	template<typename T> size_t fill(T &val) {
		if constexpr (isIterable_v<T>) {
			return _read(val.data(), sizeof(typename T::value_type), val.size());
		} else if (std::is_pointer_v<T>) {
			static_assert(!std::is_pointer_v<T>, "Use a view instead of a raw pointer");
		} else {
			return _read(&val, sizeof(T), 1);
		}
	}

	bool isOpen() const noexcept { return !_filename.empty(); }
	const std::string &name() const noexcept { return _filename; }
	int64_t tell() const { return _tell(); }
	void seek(int64_t pos) { return _seek(pos); }
	bool eof() { return _eof(); }
};

class TNoReader final : public TReader {
	size_t _read(void *, size_t, size_t) override { throw TDevError("No Input File was defined!"); }
	int64_t _tell() const override { throw TDevError("No Input File was defined!"); }
	void _seek(int64_t) override { throw TDevError("No Input File was defined!"); }
	bool _eof() const override { throw TDevError("No Input File was defined!"); }

public:
	TNoReader() : TReader("") {}
};

TReader* makeReader(std::string_view Filename);

} // namespace coretools

#endif
