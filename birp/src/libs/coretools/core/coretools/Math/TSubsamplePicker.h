/*
 * TDownsamplePicker.h
 *
 *  Created on: Aug 10, 2020
 *      Author: phaentu
 */

#ifndef GENOMETASKS_TDOWNSAMPLEPICKER_H_
#define GENOMETASKS_TDOWNSAMPLEPICKER_H_

#include <cstdint>
#include <set>
#include <vector>

namespace coretools {

//---------------------------------------
// TSubsampleSolutions
//---------------------------------------
class TSubsampleCase {
private:
	uint32_t _N, _k;

public:
	TSubsampleCase(uint32_t N, uint32_t k) {
		_N = N;
		_k = k;
	};

	uint32_t N() const { return _N; };
	uint32_t k() const { return _k; };

	bool operator<(TSubsampleCase other) const {
		if (_N < other._N) {
			return true;
		} else if (_N > other._N) {
			return false;
		} else if (_k < other._k) {
			return true;
		} else {
			return false;
		}
	}
};

//---------------------------------------
// TSubsampleSolutions
//---------------------------------------
class TSubsampleSolutions {
private:
	TSubsampleCase _downsampleCase;
	std::vector<std::vector<uint32_t>> _solutions;

public:
	TSubsampleSolutions(TSubsampleCase downsamplingCase);

	bool operator<(TSubsampleSolutions other) const;
	const std::vector<uint32_t> &pick() const;
};

//---------------------------------------
// TSubsamplePicker
//---------------------------------------
class TSubsamplePicker {
private:
	uint32_t _maxNStored;

	// storage for pre-calculated solutions
	mutable std::set<TSubsampleSolutions, std::less<>> _solutions;

	// variables for random picking
	mutable std::vector<uint32_t> _tmpSolution; // used when N > _maxNStored

public:
	TSubsamplePicker(uint32_t MaxStored = 20);

	const std::vector<uint32_t> &pick(uint32_t N, uint32_t k) const;
};

}; // namespace coretools

#endif /* GENOMETASKS_TDOWNSAMPLEPICKER_H_ */
