/*
 * TMeanVar.h
 *
 *  Created on: Oct 24, 2022
 *      Author: phaentu
 */

#ifndef CORE_CORETOOLS_MATH_TMEANVAR_H_
#define CORE_CORETOOLS_MATH_TMEANVAR_H_

#include <cstdint>
#include <limits>

#include "coretools/Main/TError.h"
#include "coretools/Math/mathFunctions.h"

namespace coretools {

//----------------------------------------------
// TMeanVar
// calculates mean and variance
//----------------------------------------------
template<class T> class TMeanVar {
private:
	uint64_t _counter;
	T _sum;
	T _sumOfSquares;

public:
	TMeanVar() { clear(); };

	void clear() {
		_counter      = 0;
		_sum          = 0;
		_sumOfSquares = 0;
	};

	void add(const T Value) {
		// check for numeric overflow when adding value to sumOfSquares
		// no need to check for overflow of sum, as sumOfSquares > sum
		T square = Value * Value;
		DEV_ASSERT(checkForNumericOverflow_addition(_sumOfSquares, square));

		_sum += Value;
		_sumOfSquares += square;
		_counter++;
	};

	void add(const TMeanVar &other) {
		// check for numeric overflow when adding value to sumOfSquares
		// no need to check for overflow of sum, as sumOfSquares > sum
		DEV_ASSERT(checkForNumericOverflow_addition(_sumOfSquares, other._sumOfSquares));

		_counter += other._counter;
		_sum += other.sum();
		_sumOfSquares = other._sumOfSquares;
	};

	uint64_t counts() const { return _counter; };

	T sum() const { return _sum; };

	double mean() const {
		if (_counter == 0) return 0.;
		return static_cast<double>(_sum) / static_cast<double>(_counter);
	};

	double variance() const {
		// variance here is population variance (not MLE sample variance, as such not the same as in R)
		if (_counter == 0) return 0.;
		const double Mean = mean();
		const double var  = static_cast<double>(_sumOfSquares) / static_cast<double>(_counter) - Mean * Mean;
		return std::max(
			var, std::numeric_limits<double>::min()); // in case of numeric inaccuracies: return very small number
	};

	double sd() const { return sqrt(variance()); };
};

template<typename T> inline bool operator==(const TMeanVar<T> &lhs, const TMeanVar<T> &rhs) {
	return lhs.sum() == rhs.sum() && lhs.counts() == rhs.counts();
}

}; // namespace coretools

#endif /* CORE_CORETOOLS_MATH_TMEANVAR_H_ */
