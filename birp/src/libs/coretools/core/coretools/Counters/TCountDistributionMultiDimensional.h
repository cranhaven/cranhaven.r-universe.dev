#ifndef COUNTERS_TCOUNTDISTRIBUTIOMULTIDIMENSIONAL_H_
#define COUNTERS_TCOUNTDISTRIBUTIOMULTIDIMENSIONAL_H_

#include <cstdint>

#include "coretools/Counters/TCountDistribution.h"
#include "coretools/Storage/TStorage.h"

namespace coretools {

template<typename TypeValue = uint32_t, typename TypeCounts = uint64_t, bool AllowResize = true, size_t NumDim = 2>
class TCountDistributionMultiDimensional {
	// 3D-histogram

	// Template parameter TypeValue: the type of index values
	// Template parameter TypeCounts: the type of counts (typically larger than TypeValue, since these are added up)
	// Template parameter AllowResize: In some cases, we know the maximal value of TypeCounts upon construction
	// -> in this case, we don't need to check for resizing every time we add a value -> more efficient
	// Note: in contrast to TCountDistributionVector, TCountDistributionMultiDimensional can only be resized if empty!
	// -> no automatic resizing on IDs

private:
	TMultiDimensionalStorage<TCountDistribution<TypeValue, TypeCounts, AllowResize>, NumDim> _distVec;

public:
	TCountDistributionMultiDimensional() = default;
	TCountDistributionMultiDimensional(const std::array<size_t, NumDim> &dimensions) { _distVec.resize(dimensions); };

	void clear() { _distVec.clear(); };
	void resize(const size_t Size) { _distVec.resize(Size); };

	void add(const std::array<size_t, NumDim> &Coord, TypeValue Value) { _distVec[Coord].add(Value); };

	void add(const std::array<size_t, NumDim> &Coord, TypeValue Value, TypeCounts Frequency) {
		_distVec[Coord].add(Value, Frequency);
	};

	void add(const std::array<size_t, NumDim> &Coord,
	         const TCountDistribution<TypeValue, TypeCounts, AllowResize> &Other) {
		_distVec[Coord].add(Other);
	};

	void add(TCountDistributionMultiDimensional<TypeValue, TypeCounts, AllowResize> &Other) {
		// only works if dimensions are equal
		assert(_distVec.dimensions() == Other.dimensions());
		for (size_t i = 0; i < _distVec.size(); ++i) { _distVec[i].add(Other[i]); }
	};

	size_t size() const { return _distVec.size(); };
	size_t counts() const {
		size_t c = 0.0;
		for (auto &i : _distVec) { c += i.counts(); }
		return c;
	};

	size_t countsLarger(TypeValue Value) const {
		size_t c = 0;
		for (auto &i : _distVec) { c += i.countsLarger(Value); }
		return c;
	};

	size_t countsLargerZero() const {
		size_t c = 0;
		for (auto &i : _distVec) { c += i.countsLargerZero(); }
		return c;
	};

	size_t sum() const {
		size_t s = 0;
		for (auto &i : _distVec) { s += i.sum(); }
		return s;
	};

	double mean() const {
		const auto c = counts();
		if (c == 0) { return 0.0; }
		return (double)sum() / (double)c;
	};

	TypeCounts min() const {
		if (_distVec.empty()) { return 0; }
		TypeCounts m = _distVec[0].min();
		for (auto &i : _distVec) {
			if (i.min() < m) { m = i.min(); }
		}
		return m;
	};

	TypeCounts max() const {
		if (_distVec.empty()) { return 0; }
		TypeCounts m = _distVec[0].max();
		for (auto &i : _distVec) {
			if (i.max() > m) { m = i.max(); }
		}
		return m;
	};

	double fracLarger(const TypeValue Value) const { return (double)countsLarger(Value) / (double)counts(); };
	double fracLargerZero() const { return (double)countsLargerZero() / (double)counts(); };

	const TCountDistribution<> &operator[](const std::array<size_t, NumDim> &Coord) const { return _distVec[Coord]; };

	void fillCombinedDistribution(TCountDistribution<TypeValue, TypeCounts, AllowResize> &combined) const {
		combined.clear();
		for (auto &i : _distVec) { combined.add(i); }
	};

	template<bool write_0 = true> void writeCombined(TOutputFile &out) const {
		TCountDistribution<TypeValue, TypeCounts, AllowResize> combined;
		fillCombinedDistribution(combined);
		combined.template write<write_0>(out);
	};

	template<bool write_0 = true> void writeCombined(TOutputFile &out, std::string_view name) const {
		TCountDistribution<TypeValue, TypeCounts, AllowResize> combined;
		fillCombinedDistribution(combined);
		combined.template write<write_0>(out, name);
	};
};

} // namespace coretools
#endif
