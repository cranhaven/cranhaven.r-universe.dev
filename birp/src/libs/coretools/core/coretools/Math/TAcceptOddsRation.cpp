
#include "TAcceptOddsRation.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"

namespace coretools {

size_t TAcceptOddsRatio::_getIndex(const double x) {
	// avoid using floor (slow) -> x/delta_x -> will always be >0, so we can directly cast!
	return static_cast<size_t>((x - _min_x) * _inv_delta_x);
}

bool TAcceptOddsRatio::_evaluate_right(const double logitQ, const size_t right_bin, const double x) {
	if (logitQ < _lookup[right_bin]) {
		// logit(x) is close to logitQ: evaluate with logit
		return logit(x) <= logitQ;
	}
	return true; // logit(x) is small, logitQ large -> anyways accept, no need for log
}

bool TAcceptOddsRatio::_evaluate_left(const double logitQ, const size_t left_bin, const double x) {
	if (logitQ > _lookup[left_bin]) {
		// logit(x) is close to logitQ: evaluate with logit
		return logit(x) <= logitQ;
	}
	return false; // logit(x) is large, logitQ small -> anyways reject, no need for logit
}

bool TAcceptOddsRatio::accept(const double logitQ) {
	// draw random number (uniform in [0,1])
	const double x = coretools::instances::randomGenerator().getRand();

	// if we are outside range of lookup table: don't calculate index of bin, directly use first bin
	if (x < _min_x) { return _evaluate_right(logitQ, 0, x); }
	if (x > _max_x) { return _evaluate_left(logitQ, _lengthLookup - 1, x); }

	// calculate index
	size_t bin = _getIndex(x);
	// accept
	if (bin == 0) { return _evaluate_right(logitQ, bin + 1, x); }
	if (bin == _lengthLookup - 1) { return _evaluate_left(logitQ, bin - 1, x); }
	if (logitQ >= _lookup[bin]) { return _evaluate_right(logitQ, bin + 1, x); }
	return _evaluate_left(logitQ, bin - 1, x);
};

}
