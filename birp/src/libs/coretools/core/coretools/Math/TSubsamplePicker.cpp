/*
 * TDownsamplePicker.cpp
 *
 *  Created on: Aug 10, 2020
 *      Author: phaentu
 */

#include "coretools/Math/TSubsamplePicker.h"

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"

namespace coretools {

//---------------------------------------
// TSubsampleSolutions
//---------------------------------------
TSubsampleSolutions::TSubsampleSolutions(TSubsampleCase DownsamplingCase) : _downsampleCase(DownsamplingCase) {
	// fill solutions
	uint32_t N            = _downsampleCase.N();
	uint32_t k            = _downsampleCase.k();
	uint32_t numSolutions = choose(N, k);
	_solutions.resize(numSolutions);

	// fill first solution
	for (uint32_t i = 0; i < k; ++i) { _solutions[0].emplace_back(i); }

	// then fill next in lexographic order
	for (uint32_t s = 1; s < numSolutions; ++s) {
		std::vector<uint32_t> &next = _solutions[s];
		// copy values
		for (uint32_t i = 0; i < k; ++i) { next.emplace_back(_solutions[s - 1][i]); }

		// update
		uint32_t i = k;
		while (i > 0) {
			--i;
			// find largest element not at maximum
			if (next[i] < N - k + i) {
				next[i]++;
				for (uint32_t j = i + 1; j < k; j++) { next[j] = next[j - 1] + 1; }
				break;
			}
		}
	}
};

bool TSubsampleSolutions::operator<(TSubsampleSolutions other) const {
	return _downsampleCase < other._downsampleCase;
};

const std::vector<uint32_t> &TSubsampleSolutions::pick() const {
	return _solutions[instances::randomGenerator().sample(_solutions.size())];
};

//---------------------------------------
// TSubsamplePicker
//---------------------------------------
TSubsamplePicker::TSubsamplePicker(uint32_t MaxStored) { _maxNStored = MaxStored; };

const std::vector<uint32_t> &TSubsamplePicker::pick(uint32_t N, uint32_t k) const {
	if (N < _maxNStored) {
		// check if case has been calculated before
		TSubsampleCase dc(N, k);
		const auto c = _solutions.emplace(dc).first;
		return c->pick();
	} else {
		// get random combination by rejection sampling
		//TODO: use faster algo for some cases (e.g. if k > N/2).
		_tmpSolution.clear();
		while (_tmpSolution.size() < k) {
			uint32_t p = instances::randomGenerator().sample(N);
			auto it    = std::find(_tmpSolution.begin(), _tmpSolution.end(), p);
			if (it == _tmpSolution.end()) { _tmpSolution.emplace_back(p); }
		}
		return _tmpSolution;
	}
};

}; // namespace coretools
