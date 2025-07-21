//
// Created by caduffm on 5/25/23.
//

#include "TAccept.h"

#include "coretools/Main/TRandomGenerator.h"
#include <math.h>

namespace coretools {

size_t TAccept::_getIndex(const double x) {
	// avoid using floor (slow) -> x/delta_x -> will always be >0, so we can directly cast!
	return static_cast<size_t>((x - _xThreshold) * _inv_delta_x);
}

bool TAccept::_evaluate_right(const double logQ, const size_t right_bin, const double x) {
	if (logQ < _lookup[right_bin]) { return log(x) <= logQ; } // log(x) is close to logQ: evaluate with log
	return true; // log(x) is small, logQ large -> anyways accept, no need for log
}

bool TAccept::_evaluate_left(const double logQ, const size_t left_bin, const double x) {
	if (logQ > _lookup[left_bin]) { return log(x) <= logQ; } // log(x) is close to logQ: evaluate with log
	return false; // log(x) is large, logQ small -> anyways reject, no need for log
}

bool TAccept::accept(const double logQ) {
	DEBUG_ASSERT(logQ <= 0.0);

	// draw random number (uniform in [0,1])
	const double x = coretools::instances::randomGenerator().getRand();

	// if we are outside range of lookup table: don't calculate index of bin, directly use first bin
	if (x < _xThreshold) { return _evaluate_right(logQ, 0, x); }

	// calculate index
	size_t bin = _getIndex(x);
	// accept
	if (bin == 0) { return _evaluate_right(logQ, bin + 1, x); }
	if (bin == _lengthLookup - 1) { return _evaluate_left(logQ, bin - 1, x); }
	if (logQ >= _lookup[bin]) { return _evaluate_right(logQ, bin + 1, x); }
	return _evaluate_left(logQ, bin - 1, x);
};

//---------------------------------------------
// TAcceptOddsRatio
//---------------------------------------------


} // namespace coretools
