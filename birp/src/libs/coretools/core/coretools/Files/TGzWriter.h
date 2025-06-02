/*
 * TWriter.h
 *
 *  Created on: 2023-07-05
 *      Author: Andreas
 */

#ifndef CORE_FILES_TGZWRITER_H
#define CORE_FILES_TGZWRITER_H

#include <zlib.h>

#include "coretools/Main/TError.h"

#include "TWriter.h"

namespace coretools {

class TGzWriter final : public TWriter {
	gzFile _file;

	void _write(const void *buffer, size_t size, size_t count) override {
		if (gzwrite(_file, buffer, size * count) == 0) { DEVERROR("Was not able to write to gz file!"); };
	};

	int64_t _tell() const override { return gztell(_file); };

public:
	TGzWriter(std::string_view Filename, const char *Mode = "w")
		: TWriter(Filename), _file(gzopen(name().c_str(), Mode)) {
		if (!_file) { UERROR("Was not able to create file ", name(), ". Does the path exist?"); }
	}
	~TGzWriter() { gzclose(_file); }

	TGzWriter(const TGzWriter &)            = delete;
	TGzWriter &operator=(const TGzWriter &) = delete;

	TGzWriter(TGzWriter &&)            = default;
	TGzWriter &operator=(TGzWriter &&) = default;
};

} // namespace coretools

#endif
