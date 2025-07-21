//
// Created by caduffm on 5/23/22.
//

#ifndef TSUMLOG_H
#define TSUMLOG_H

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <numeric>

#include "coretools/Main/TError.h"
#include "coretools/Math/mathConstants.h"

namespace coretools {

//-------------------------------------------
// Sum of logs for fixed bins
//-------------------------------------------

namespace impl {
// Note: developer-friendly alias' are provided on the bottom! (outside namespace impl)

template<size_t N> class TBinnedSumLog {
	static_assert(N > 0);
	/*
	 * Calculates the sum of logs
	 *    L = sum_{i=0}^{I-1} log(x_i)
	 * as the log of binned products:
	 *    L = sum_{j=0}^{J-1} log(prod_{i=jN}^{(j+1)N-1} x_i)
	 * for some bin width N.
	 * If N == 1; this is exactly equivalent to the sum of log of each element.
	 * For N > 1, it should be considerable faster, but at a certain bin width there will be underflow or overflow
	 * -> if you know the minimum values of your elements, you can calculate the maximal bin width and use this class
	 * -> if minimum is not known: this approach is not safe (overflow) , use dynamic approach below instead!
	 */
private:
	double _sum  = 0.0;
	double _prod = 1.0;
	size_t _c    = 0;

	void _finalizeBin() {
		_sum += log(_prod);
		_c    = 0;
		_prod = 1.0;
	}

public:
	void add(double Value) {
		if constexpr (N == 1) {
			_sum += log(Value);
		} else {
			if (_c == N) { _finalizeBin(); }
			_prod *= Value;
			++_c;
		}
	}

	double getSum() {
		if constexpr (N == 1) { return _sum; }
		if constexpr (N == 2) {
			if (_c > 0) { _finalizeBin(); }
		} else { // if N > 2: if-statement is true in >50% of cases -> don't check, just calculate
			_finalizeBin();
		}
		return _sum;
	}
};

template<size_t N, typename Container> double getBinnedSumOfLogs(const Container &Vals) {
	/*
	 * Same as TBinnedSumLog, but input values are stored in a container
	 * -> can calculate the number of bins in advance, and loop over them -> faster
	 */
	static_assert(N > 0);

	if constexpr (N == 1) {
		return std::accumulate(Vals.begin(), Vals.end(), 0.0, [](double sum, auto v) { return sum + log(v); });
	}
	const size_t floorNumBins = std::floor(Vals.size() / N);
	double sum                = 0.0;
	for (size_t j = 0; j < floorNumBins; j++) {
		double prod = 1.0;
		for (size_t i = 0; i < N; i++) { prod *= Vals[j * N + i]; }
		sum += log(prod);
	}

	// check: are there remaining values?
	const size_t rest = Vals.size() - floorNumBins * N;
	if constexpr (N == 2) { // if N > 2: if-statement is true in >50% of cases -> don't check, just calculate
		if (rest == 0) { return sum; }
	}

	double prod = 1.0;
	for (size_t i = 0; i < rest; i++) { prod *= Vals[floorNumBins * N + i]; }
	sum += log(prod);
	return sum;
}

//-------------------------------------------
// Sum of logs for dynamic bins
//-------------------------------------------

constexpr int getExponentOfDouble(double Value) noexcept(noDebug) {
	// function to get the exponent of a double by bit-masking
	// idea from https://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node48.html
	DEBUG_ASSERT(Value != 0.0);

	constexpr size_t MSW = 3; // Little-endianness: left-most 16-bit short is sh[3]. If system is big-endian: must use 0
	union {
		double dp;            // the 64-bit double precision value
		unsigned short sh[4]; // overlay an array of 4 16-bit integers
	} X{Value};

	// isolate exponent in 16-bit word, remove 4 unnecessary bits (sign + 3 mantissa) and get unbiased exponent
	return ((X.sh[MSW] & 0x7ff0) >> 4) - 1023;
}

inline bool isBigEndian() noexcept {
	// from https://stackoverflow.com/questions/1001307/detecting-endianness-programmatically-in-a-c-program
	// switch to std::endian as soon as C++20 is out
	union {
		uint32_t i;
		char c[4];
	} v = {0x01020304};
	return v.c[0] == 1;
};

template<bool Probability = false> class TDynamicBinnedSumLog {
	/*
	 * Instead of choosing a fixed bin width N, we dynamically check for under/overflow by adding up the
	 * exponents of the double precision value. Whenever under/overflow would happen: add to total.
	 *
	 * Probability: if input values are known to be in interval (0,1]: can omit some checks, a bit faster
	 */
private:
	static constexpr int MaxAbsExponent = 1020; // exponents range from −1022 to +1023; we add a little safety buffer
	double _curProd                     = 1.0;
	long int _curExponent               = 0;
	long int _totalExponent             = 0;

	[[nodiscard]] constexpr bool _doFinalize(const int NewExponent) const {
		if constexpr (Probability) {
			return NewExponent < -MaxAbsExponent;
		} else {
			return NewExponent < -MaxAbsExponent || NewExponent > MaxAbsExponent;
		}
	}

	constexpr void _finalizeBin() {
		_totalExponent += _curExponent;
		// remove _curExponent from _curProd (x * 2^exponent * 2^(-exponent) = x * 2^0 = x)
		_curProd = std::ldexp(_curProd, -_curExponent);
	}

public:
	TDynamicBinnedSumLog() {
		// Method of getting exponent only works for little-endian systems, as it will always take 3rd short.
		// Currently compilers don't support getting endianness at compile time (only C++20 allows it).
		// As most processors are little endian, it should not be problematic; if it is, then we can add an if-
		// that takes the correct short (0 or 3) depending on architecture
		dev_assert(!impl::isBigEndian(), "System is running on big endian.");
	}

	void add(double Value) {
		const int exponent    = impl::getExponentOfDouble(Value);
		const int newExponent = _curExponent + exponent;
		if (_doFinalize(newExponent)) { _finalizeBin(); }

		_curProd *= Value;
		_curExponent = impl::getExponentOfDouble(_curProd);
	}

	double getSum() {
		_finalizeBin();
		return log(_curProd) + _totalExponent * ln2;
	}
};

template<typename Container, bool Probability = false> double getDynamicBinnedSumOfLogs(const Container &Vals) {
	/*
	 * Same as TDynamicBinnedSumLog, but for containers (vector/array)
	 */
	TDynamicBinnedSumLog<Probability> sumLog;
	for (size_t i = 0; i < Vals.size(); i++) { sumLog.add(Vals[i]); }
	return sumLog.getSum();
}

} // namespace impl

//------------------------------------------
// Alias' for nice usage
//------------------------------------------

template<size_t N = 0>
using TSumLog            = std::conditional_t<N == 0, impl::TDynamicBinnedSumLog<false>, impl::TBinnedSumLog<N>>;
using TSumLogProbability = impl::TDynamicBinnedSumLog<true>;

template<size_t N = 0, typename Container> constexpr double getSumOfLog(const Container &Vals) {
	if constexpr (N == 0) {
		return impl::getDynamicBinnedSumOfLogs<Container, false>(Vals);
	} else {
		return impl::getBinnedSumOfLogs<N, Container>(Vals);
	}
}

template<typename Container> double getSumOfLogProbability(const Container &Vals) {
	return impl::getDynamicBinnedSumOfLogs<Container, true>(Vals);
}

} // namespace coretools

#endif // TSUMLOG_H
