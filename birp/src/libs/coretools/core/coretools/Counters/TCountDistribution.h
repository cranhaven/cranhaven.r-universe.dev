#ifndef COUNTERS_TCOUNTDISTRIBUTION_H_
#define COUNTERS_TCOUNTDISTRIBUTION_H_

#include <cstdint>
#include <vector>
#include <algorithm>

#include "coretools/Files/TOutputFile.h"

namespace coretools {

template<typename TypeValue, typename TypeCounts, bool AllowResize>
class TCountDistributionVector; // forward declaration

template<typename TypeValue = uint32_t, typename TypeCounts = uint64_t, bool AllowResize = true>
class TCountDistribution {
	// example: you've got the values:
	//      1 7 4 1 2 7 7 2
	// this class stores them as a histogram ( = count distribution) like this:
	//      0  0
	//      1  2
	//      2  2
	//      3  0
	//      4  1
	//      5  0
	//      6  0
	//      7  3

	// Template parameter TypeValue: the type of input values
	// Template parameter TypeCounts: the type of counts (typically larger than TypeValue, since these are added up)
	// Template parameter AllowResize: In some cases, we know the maximal value of TypeCounts upon construction
	// -> in this case, we don't need to check for resizing every time we add a value -> more efficient

	friend class TCountDistributionVector<TypeValue, TypeCounts, AllowResize>;

private:
	// size of _dist vector is the maximal value + 1 (start counting at 0)
	std::vector<TypeCounts> _dist;

public:
	TCountDistribution() = default;
	explicit TCountDistribution(TypeValue Value) { _dist.resize(underlying(Value), 0); }

	void resize(const TypeValue Value) { _dist.resize(underlying(Value), 0); }
	void clear() { _dist.clear(); }
	void reset() { std::fill(_dist.begin(), _dist.end(), 0); }
	bool empty() const {
		return std::all_of(_dist.begin(), _dist.end(), [](TypeCounts i) { return i == 0; });
	}
	size_t size() const { return _dist.size(); }

	TypeCounts &operator[](const TypeValue Value) {
		// resize if element is outside range
		if constexpr (AllowResize) {
			if (_dist.size() <= Value) { _dist.resize(underlying(Value) + 1, 0); }
		}
		return _dist[underlying(Value)];
	}

	const TypeCounts &operator[](const TypeValue Value) const {
		// throw if element is outside range
		if (Value >= _dist.size()) { DEVERROR("No entry for value ", Value, " in TCountDistribution!"); }
		return _dist[underlying(Value)];
	}

	TypeCounts at(const TypeValue Value) const {
		// return 0 if element is outside range
		if (underlying(Value) >= _dist.size()) {
			return 0;
		} else {
			return _dist[underlying(Value)];
		}
	}

	void add(const TypeValue Value) {
		if constexpr (AllowResize) {
			if (_dist.size() <= underlying(Value)) { _dist.resize(underlying(Value) + 1, 0); }
		}
		++_dist[underlying(Value)];
	}

	void add(const TypeValue Value, const TypeCounts Counts) {
		if constexpr (AllowResize) {
			if (_dist.size() <= underlying(Value)) { _dist.resize(underlying(Value) + 1, 0); }
		}
		_dist[Value] += Counts;
	}

	void add(const TCountDistribution<TypeValue, TypeCounts, AllowResize> &other) {
		if constexpr (AllowResize) {
			if (_dist.size() <= other._dist.size()) { _dist.resize(other._dist.size(), 0); }
		}
		for (size_t i = 0; i < other._dist.size(); ++i) { _dist[i] += other._dist[i]; }
	}

	size_t counts() const {
		size_t c = 0;
		for (auto &i : _dist) { c += i; }
		return c;
	}

	size_t countsLarger(const TypeValue Value) const {
		size_t n = 0;
		for (size_t i = underlying(Value) + 1; i < _dist.size(); ++i) { n += _dist[i]; }
		return n;
	}

	size_t countsLargerZero() const { return countsLarger(0); }

	size_t sum() const {
		size_t s = 0;
		for (size_t i = 1; i < _dist.size(); ++i) {
			s += i * _dist[i]; // histogram -> multiply value with how many times it occurs
		}
		return s;
	}

	double mean() const {
		const auto c = counts();
		if (c == 0) { return 0.0; }
		return (double)sum() / (double)c;
	}

	size_t mode() const { return std::distance(_dist.begin(), std::max_element(_dist.begin(), _dist.end())); }

	TypeCounts min() const {
		// get smallest entry that is non-zero
		for (size_t i = 0; i < _dist.size(); ++i) {
			if (_dist[i] > 0) { return i; }
		}
		return 0;
	}

	TypeCounts max() const {
		// get biggest entry that is non-zero
		for (size_t i = 0; i < _dist.size(); ++i) {
			const auto i_inv = _dist.size() - 1 - i;
			if (_dist[i_inv] > 0) return i_inv;
		}
		return 0;
	}

	double frac(const TypeValue Value) const {
		const auto c = counts();
		if (c == 0) { return 0.0; }
		return (double)at(underlying(Value)) / (double)c;
	}
	double fracLarger(const TypeValue Value) const {
		const auto c = counts();
		if (c == 0) { return 0.0; }
		return (double)countsLarger(Value) / (double)c;
	}
	double fracLargerZero() const {
		const auto c = counts();
		if (c == 0) { return 0.0; }
		return (double)countsLargerZero() / (double)c;
	}

	template<bool write_0 = true> void write(TOutputFile &out) const {
		if constexpr (write_0) {
			for (size_t i = 0; i < _dist.size(); ++i) { out.writeln(i, _dist[i]); }
		} else {
			for (size_t i = 0; i < _dist.size(); ++i) {
				if (_dist[i] > 0) { out.writeln(i, _dist[i]); }
			}
		}
	}

	template<bool write_0 = true> void write(TOutputFile &out, std::string_view name) const {
		if constexpr (write_0) {
			for (size_t i = 0; i < _dist.size(); ++i) { out.writeln(name, i, _dist[i]); }
		} else {
			for (size_t i = 0; i < _dist.size(); ++i) {
				if (_dist[i] > 0) { out.writeln(name, i, _dist[i]); }
			}
		}
	}

	template<bool write_0 = true> void write(std::string_view filename, std::string_view label) const {
		TOutputFile out(filename, {label, "counts"});
		write<write_0>(out);
	}
};

template<typename TypeValue = uint32_t, typename TypeCounts = uint64_t, bool AllowResize = true>
bool operator==(const TCountDistribution<TypeValue, TypeCounts, AllowResize> &lhs,
                       const TCountDistribution<TypeValue, TypeCounts, AllowResize> &rhs) {
	if (lhs.size() != rhs.size()) { return false; }
	for (size_t i = 0; i < lhs.size(); i++) {
		if (lhs[i] != rhs[i]) { return false; }
	}
	return true;
}

}
#endif
