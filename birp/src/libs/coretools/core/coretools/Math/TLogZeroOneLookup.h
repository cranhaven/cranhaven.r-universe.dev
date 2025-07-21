//
// Created by madleina on 03.12.21.
//

#ifndef TLOGZEROONELOOKUP_H
#define TLOGZEROONELOOKUP_H

#include "TLogZeroOneLookup.tpp"
#include "coretools/Types/probability.h"

namespace coretools {

class TLogZeroOneLookup {
	// approximate the log function in interval [0,1] with lookup table
private:
	// size corresponds to lookup table constructed with getLog01LookupTable().
	static constexpr uint16_t _lengthLookup = 2004;
	static constexpr uint16_t _lengthArray  = 1002;
	static constexpr double _xThreshold     = 0.049;
	static constexpr double _inv_delta_x    = (_lengthArray-1) / (1.0 - _xThreshold);
	using array_size                        = std::array<double, _lengthLookup>::size_type;

	static constexpr std::array<double, _lengthLookup> _lookup = getLog01LookupTable();

	static array_size _getIndex(double x) {
		// avoid using floor (slow) -> absx/delta_x -> will always be >0, so we can directly cast!
		return 2 * static_cast<array_size>((x - _xThreshold) * _inv_delta_x);
	}

	static double _getY(const double x, array_size index) {
		return _lookup[index] + _lookup[index + 1] * x; // linear function
	}

public:
	static coretools::LogProbability approxLog01(double x) {
		if (x < _xThreshold) { return logP(std::log(x)); }
		const array_size index = _getIndex(x);
		const double y         = _getY(x, index);
		return logP(y);
	};
};

} // namespace coretools
#endif // TLOGZEROONELOOKUP_H
