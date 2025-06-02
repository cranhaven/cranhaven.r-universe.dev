#include "TReader.h"
#include "TStdReader.h"
#include "TGzReader.h"
#include "coretools/Strings/stringManipulations.h"


namespace coretools {
TReader *makeReader(std::string_view Filename) {
	const auto extension = str::readAfterLast(Filename, '.');

	if (extension == "gz") return new TGzReader(Filename);
	return new TStdReader(Filename);
}
} // namespace coretools
