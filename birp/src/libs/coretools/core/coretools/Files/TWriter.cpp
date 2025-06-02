#include "TWriter.h"
#include "TStdWriter.h"
#include "TGzWriter.h"
#include "coretools/Strings/stringManipulations.h"

namespace coretools {
TWriter *makeWriter(std::string_view Filename, const char *Mode) {
	const auto extension = str::readAfterLast(Filename, '.');

	if (extension == "gz") return new TGzWriter(Filename, Mode);
	return new TStdWriter(Filename, Mode);
}
} // namespace coretools
