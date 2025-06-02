#ifndef COUNTERS_TCOUNTDISTRIBUTIONMAP_H_
#define COUNTERS_TCOUNTDISTRIBUTIONMAP_H_

#include <cstdint>
#include <map>

#include "coretools/Counters/TCountDistribution.h"

namespace coretools {

template<typename T, typename TypeValue = uint32_t, typename TypeCounts = uint64_t, bool AllowResize = true>
class TCountDistributionMap {
private:
	std::map<T, TCountDistribution<TypeValue, TypeCounts, AllowResize>> _dist;

public:
	TCountDistributionMap() = default;

	void clear() { _dist.clear(); };

	void add(const T ID, const TypeValue Value) { _dist[ID].add(Value); };

	size_t size() const { return _dist.size(); };

	size_t counts() const {
		size_t c = 0.0;
		for (auto &i : _dist) { c += i.second.counts(); }
		return c;
	};

	size_t sum() const {
		size_t s = 0;
		for (auto &i : _dist) { s += i.second.sum(); }
		return s;
	};

	double mean() const { return (double)sum() / (double)counts(); };

	bool exists(const T ID) const { return _dist.find(ID) != _dist.end(); };

	const TCountDistribution<TypeValue, TypeCounts, AllowResize> &operator[](const T ID) {
		if (!exists(ID)) DEVERROR("No entry with key " + str::toString(ID) + " in TCountDistributionMap!");
		return _dist[ID];
	};
};
}
#endif
