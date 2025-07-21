//
// Created by madleina on 25.10.23.
//
#include "commonTestingFunctions.h"

namespace testing {
std::unique_ptr<coretools::TDistancesBinned<uint8_t>> createAndFillDistances(size_t Size, size_t MaxDist) {
	auto distances = std::make_unique<coretools::TDistancesBinned<uint8_t>>(MaxDist);
	for (size_t i = 0; i < Size; i++) { distances->add(i * i, "chunk_1"); }
	distances->finalizeFilling();
	return distances;
}

std::unique_ptr<coretools::TDistancesBinned<uint8_t>> createAndFillDistances(std::string_view Filename, size_t MaxDist) {
	auto distances                = std::make_unique<coretools::TDistancesBinned<uint8_t>>(MaxDist);
	std::vector<size_t> positions = readSingleLineIntoVec<size_t>(Filename);
	for (size_t position : positions) { distances->add(position, "chunk_1"); }
	distances->finalizeFilling();

	return distances;
}
} // namespace testing
