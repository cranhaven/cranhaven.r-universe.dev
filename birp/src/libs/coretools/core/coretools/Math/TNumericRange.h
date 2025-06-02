/*
 * TRange.h
 *
 *  Created on: May 14, 2021
 *      Author: phaentu
 */

#ifndef COMMONUTILITIES_TNUMERICRANGE_H_
#define COMMONUTILITIES_TNUMERICRANGE_H_

#include <limits>

#include "coretools/Main/TError.h"
#include "coretools/Strings/fromString.h"
#include "coretools/Strings/stringManipulations.h"

namespace coretools {

//-------------------------------------
// TNumericRange
//-------------------------------------
template<typename T> class TNumericRange {
private:
	T _min, _max;
	bool _minIncluded, _maxIncluded;

	constexpr void _check() {
		if (_min > _max) { UERROR("Min (", _min, ") > Max (", _max, ")!"); }
	};

public:
	template<typename... Ts> constexpr TNumericRange(const Ts &...input) { set(input...); };

	~TNumericRange() = default;

	// setters
	constexpr void set() {
		_min         = std::numeric_limits<T>::lowest();
		_max         = std::numeric_limits<T>::max();
		_minIncluded = true;
		_maxIncluded = true;
	};

	constexpr void set(T Min, bool MinIncluded, T Max, bool MaxIncluded) {
		_min         = Min;
		_max         = Max;
		_minIncluded = MinIncluded;
		_maxIncluded = MaxIncluded;
		_check();
	};

	void set(std::string_view Range) {
		Range = str::strip(Range);

		// allows for ranges of the form (min, max]
		// check if min is included
		if (Range.front() == '(') {
			_minIncluded = false;
			Range.remove_prefix(1);
		} else if (Range.front() == '[') {
			_minIncluded = true;
			Range.remove_prefix(1);
		} else {
			_minIncluded = true;
		}

		// extract min
		size_t pos = Range.find_first_of(',');
		if (pos == std::string::npos) { UERROR("Range does not contain a ','!"); }

		auto minS = str::strip(Range.substr(0, pos));
		if (minS.length() == 0) {
			_min = std::numeric_limits<T>::lowest();
		} else {
			_min = str::fromString<T, true>(minS);
		}

		// check if max is included
		if (Range.back() == ')') {
			_maxIncluded = false;
			Range.remove_suffix(1);
		} else if (Range.back() == ']') {
			_maxIncluded = true;
			Range.remove_suffix(1);
		} else {
			_maxIncluded = true;
		}

		// extract max
		auto maxS = str::strip(Range.substr(pos + 1, Range.length() - pos - 1));
		if (maxS.length() == 0) {
			_max = std::numeric_limits<T>::max();
		} else {
			_max = str::fromString<T, true>(maxS);
		}

		// check
		_check();
	};

	// getters
	[[nodiscard]] T min() const { return _min; };
	[[nodiscard]] T max() const { return _max; };
	[[nodiscard]] bool minIncluded() const { return _minIncluded; };
	[[nodiscard]] bool maxIncluded() const { return _maxIncluded; };

	[[nodiscard]] std::string rangeString() const {
		std::string s;
		if (_minIncluded) {
			s += '[';
		} else {
			s += '(';
		}

		s += str::toString(_min) + ',' + str::toString(_max);

		if (_maxIncluded) {
			s += ']';
		} else {
			s += ')';
		}

		return s;
	};

	[[nodiscard]] explicit operator std::string() const { return rangeString(); };

	// test if a value is within range
	[[nodiscard]] constexpr bool larger(T Value) const {
		if ((_maxIncluded && Value > _max) || (!_maxIncluded && Value >= _max)) {
			return true;
		} else {
			return false;
		}
	};

	[[nodiscard]] constexpr bool smaller(T Value) const {
		if ((_minIncluded && Value < _min) || (!_minIncluded && Value <= _min)) {
			return true;
		} else {
			return false;
		}
	};

	[[nodiscard]] constexpr bool within(T Value) const {
		if ((_minIncluded && Value < _min) || (!_minIncluded && Value <= _min) || (_maxIncluded && Value > _max) ||
		    (!_maxIncluded && Value >= _max)) {
			return false;
		} else {
			return true;
		}
	};

	[[nodiscard]] constexpr bool outside(T Value) const { return !within(Value); };
};

}; // namespace coretools

#endif /* COMMONUTILITIES_TNUMERICRANGE_H_ */
