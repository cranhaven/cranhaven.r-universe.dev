#ifndef COUNTERS_TCOUNTDISTRIBUTIONVECTOR_H_
#define COUNTERS_TCOUNTDISTRIBUTIONVECTOR_H_

#include <cstdint>
#include <vector>

#include "coretools/Containers/TView.h"
#include "coretools/Counters/TCountDistribution.h"

namespace coretools {

template<typename TypeValue = uint32_t, typename TypeCounts = uint64_t, bool AllowResize = true>
class TCountDistributionVector {
	// 2D-histogram

	// Template parameter TypeValue: the type of index values
	// Template parameter TypeCounts: the type of counts (typically larger than TypeValue, since these are added up)
	// Template parameter AllowResize: In some cases, we know the maximal value of TypeCounts upon construction
	// -> in this case, we don't need to check for resizing every time we add a value -> more efficient
	// Note: could also add template parameter AllowResize for ID's here if needed

private:
	std::vector<TCountDistribution<TypeValue, TypeCounts, AllowResize>> _distVec;

	void _writeAsMatrix(std::string_view filename, TConstView<std::string> header) const {
		// open file
		TOutputFile out(filename, header);

		// write matrix
		size_t dim = header.size() - 1;                // first is label
		for (size_t i = 0; i < _distVec.size(); ++i) { // rows = IDs
			// write label (rowname)
			out.write(i);
			// cols = values
			for (size_t j = 0; j < dim; ++j) { out.write(_distVec[i].at(j)); }
			out.endln();
		}
	};

	void _writeAsMatrixCombined(std::string_view filename, TConstView<std::string> header,
	                            TConstView<std::string> rowNames) const {
		// open file
		TOutputFile out(filename, header);

		// write matrix
		size_t dim = header.size() - 1; // first is label

		// write combined counts
		out.write(rowNames[0]);
		TCountDistribution<TypeValue, TypeCounts, AllowResize> combined;
		fillCombinedDistribution(combined);
		for (size_t j = 0; j < dim; ++j) { out.write(combined.at(j)); }
		out.endln();

		for (size_t i = 0; i < _distVec.size(); ++i) { // rows = IDs
			// write label (rowname)
			out.write(rowNames[i + 1]);
			// cols = values
			for (size_t j = 0; j < dim; ++j) { out.write(_distVec[i].at(j)); }
			out.endln();
		}
		out.close();
	};

public:
	TCountDistributionVector() = default;
	TCountDistributionVector(size_t Size) { _distVec.resize(Size); };

	void clear() { _distVec.clear(); };
	void resize(const size_t Size) { _distVec.resize(Size); };
	void resizeDistributions(const size_t Size) {
		for (auto &s : _distVec) s.resize(Size);
	}

	void add(size_t ID, TypeValue Value) {
		if (_distVec.size() <= ID) { _distVec.resize(ID + 1); }
		_distVec[ID].add(Value);
	};

	void add(size_t ID, TypeValue Value, TypeCounts Frequency) {
		if (_distVec.size() <= ID) { _distVec.resize(ID + 1); }
		_distVec[ID].add(Value, Frequency);
	};

	void add(size_t ID, const TCountDistribution<TypeValue, TypeCounts, AllowResize> &Other) {
		if (_distVec.size() <= ID) { _distVec.resize(ID + 1); }
		_distVec[ID].add(Other);
	};

	void add(TCountDistributionVector<TypeValue, TypeCounts, AllowResize> &vec) {
		for (size_t i = 0; i < vec._distVec.size(); ++i) { add(i, vec._distVec[i]); }
	};

	size_t size() const { return _distVec.size(); };
	size_t counts() const {
		size_t c = 0.0;
		for (auto &i : _distVec) { c += i.counts(); }
		return c;
	};

	size_t horizontalCounts(size_t pos) const {
		size_t c = 0.0;
		for (auto &i : _distVec) { c += i[pos]; }
		return c;
	}

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
	bool exists(const size_t ID) const {
		if (ID < _distVec.size())
			return true;
		else
			return false;
	};

	const TCountDistribution<> &operator[](const size_t ID) const {
		// can not automatically resize, as this would require a value
		DEV_ASSERT(exists(ID));

		return _distVec[ID];
	};

	void fillCombinedDistribution(TCountDistribution<TypeValue, TypeCounts, AllowResize> &combined) const {
		combined.clear();
		for (auto &i : _distVec) { combined.add(i); }
	};

	double RSquared() {
		// calculates correlation (R^2) between ID and values
		double n = counts();
		double sum_x{}, sum_y{}, sum_x2{}, sum_y2{}, sum_xy{};

		// calculate sums
		for (size_t i = 0; i < _distVec.size(); ++i) {
			for (size_t j = 0; j < _distVec[i]._dist.size(); ++j) {
				sum_x += _distVec[i][j] * i;
				sum_y += _distVec[i][j] * j;
				sum_x2 += _distVec[i][j] * i * i;
				sum_y2 += _distVec[i][j] * j * j;
				sum_xy += _distVec[i][j] * i * j;
			}
		}

		// calculate correlation
		double cor = (n * sum_xy - sum_x * sum_y) / sqrt(n * sum_x2 - sum_x * sum_x) / sqrt(n * sum_y2 - sum_y * sum_y);

		// return R^2
		return cor * cor;
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

	template<bool write_0 = true> void write(TOutputFile &out) const {
		for (size_t i = 0; i < _distVec.size(); ++i) {
			if (!_distVec[i].empty()) { _distVec[i].template write<write_0>(out, str::toString(i)); }
		}
	};

	template<bool write_0 = true>
	void write(std::string_view filename, std::string_view label_ID, std::string_view label) const {
		TOutputFile out(filename, {label_ID, label, "counts"});
		write<write_0>(out);
	};

	template<bool write_0 = true>
	void writeIncludingCombined(std::string_view filename, std::string_view label_ID, std::string_view label,
	                            std::string_view combinedName) const {
		TOutputFile out(filename, {label_ID, label, "counts"});
		writeCombined<write_0>(out, combinedName);
		write<write_0>(out);
	};

	template<bool write_0 = true> void write(TOutputFile &out, TConstView<std::string> names) const {
		DEV_ASSERT(_distVec.size() == names.size());
		for (size_t i = 0; i < _distVec.size(); ++i) {
			if (!_distVec[i].empty()) { _distVec[i].template write<write_0>(out, names[i]); }
		}
	};

	void writeAsMatrix(std::string_view filename, std::string_view label_ID, std::string_view label) const {
		// compile header
		std::vector<std::string> header = {std::string{label_ID}.append(1, '/').append(label)};
		for (size_t i = 0; i <= max(); ++i) { header.push_back(str::toString(i)); }

		// write
		_writeAsMatrix(filename, header);
	};

	void writeAsMatrix(std::string_view filename, std::string_view label_ID, TConstView<std::string> valueNames) const {
		// check if dim matches valueNames
		DEV_ASSERT(max() <= valueNames.size());

		// compile header
		std::vector<std::string> header;
		header.emplace_back(label_ID);
		header.insert(header.end(), valueNames.begin(), valueNames.end());

		// write
		_writeAsMatrix(filename, header);
	};

	void writeAsMatrixCombined(std::string_view filename, std::string_view label_ID, TConstView<std::string> valueNames,
	                           TConstView<std::string> rowNames) const {
		// check if dim matches valueNames
		DEV_ASSERT(max() <= valueNames.size());

		// compile header
		std::vector<std::string> header;
		header.emplace_back(label_ID);
		header.insert(header.end(), valueNames.begin(), valueNames.end());

		// write
		_writeAsMatrixCombined(filename, header, rowNames);
	}
};
}
#endif
