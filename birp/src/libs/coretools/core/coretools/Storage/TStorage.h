//
// Created by caduffm on 9/24/20.
//

#ifndef TMULTIDIMENSIONALSTORAGE_H
#define TMULTIDIMENSIONALSTORAGE_H

#include <memory>
#include <numeric>
#include <vector>

#include "coretools/Math/TSumLog.h"
#include "coretools/Storage/TDimension.h"
#include "coretools/Storage/TNames.h"
#include "coretools/Strings/concatenateString.h"

namespace coretools {

//-------------------------------------------
// TMultiDimensionalStorage
//-------------------------------------------
template<typename Type, size_t NumDim> class TMultiDimensionalStorage {
	// stores a linear vector but keeps track of dimensions
protected:
	// vector with a certain Type that stores the linear values
	std::vector<Type> _values;

	// dimensions
	TDimension<NumDim> _dimension;

	// names (one per dimension)
	std::array<std::shared_ptr<TNamesEmpty>, NumDim> _dimensionNames;

	void _initDimensionNames() {
		for (size_t dim = 0; dim < NumDim; dim++) {
			if (!_dimensionNames[dim]) { // no valid ptr yet -> create default one
				if (NumDim == 1 && _dimension.dimensions()[0] == 1) {
					// single elements: dimension names with empty strings -> don't want a single parameter e.g. alpha
					// to be called alpha_1
					std::vector<std::string> dimNamesVec = {""};
					_dimensionNames[dim]                 = std::make_shared<TNamesStrings>(dimNamesVec);
				} else {
					_dimensionNames[dim] = std::make_shared<TNamesIndices>(_dimension.dimensions()[dim]);
				}
			} else { // resize
				_dimensionNames[dim]->resize(_dimension.dimensions()[dim]);
			}
		}
	};

	template<typename T> bool _innerDimensionsAreEqual(const T &dimensions) const {
		// checks if all inner dimensions (= all except last dimension) are equal to argument dimensions
		for (size_t i = 0; i < NumDim - 1; i++) {
			if (_dimension.dimensions()[i] != dimensions[i]) { return false; }
		}
		return true;
	}

	template<bool Resize, typename T> void _resize(const T &dimensions) {
		// T can be vector or array
		_dimension.init(dimensions);
		if constexpr (Resize) {
			_values.resize(_dimension.size());
		} else {
			_values.reserve(_dimension.size());
		}
		_initDimensionNames();
	}

public:
	using iterator               = typename std::vector<Type>::iterator;
	using const_iterator         = typename std::vector<Type>::const_iterator;
	using reverse_iterator       = typename std::vector<Type>::reverse_iterator;
	using const_reverse_iterator = typename std::vector<Type>::const_reverse_iterator;
	using value_type             = Type;
	using size_type              = size_t;

	TMultiDimensionalStorage() = default;

	explicit TMultiDimensionalStorage(const std::array<size_t, NumDim> &Dimensions) { _resize<true>(Dimensions); };

	void clear() {
		// reset dimensions and values
		_dimension.clear();
		_values.resize(0);
	};

	size_t getIndex(const std::array<size_t, NumDim> &coord) const { return _dimension.getIndex(coord); };
	TRange getRange(const std::array<size_t, NumDim> &startCoord, const std::array<size_t, NumDim> &endCoord) const {
		return _dimension.getRange(startCoord, endCoord);
	};
	TRange getFull() const { return _dimension.getFull(); };
	TRange getDiagonal() const { return _dimension.getDiagonal(); };
	TRange get1DSlice(size_t dim, const std::array<size_t, NumDim> &startCoord) const {
		return _dimension.get1DSlice(dim, startCoord);
	};
	auto getSlice(size_t dim, size_t indexInDim) const { return _dimension.getSlice(dim, indexInDim); };
	auto beginSlice(size_t dim, size_t indexInDim) const { return _dimension.beginSlice(dim, indexInDim); };
	std::array<size_t, NumDim> dimensions() const { return _dimension.dimensions(); };
	std::array<size_t, NumDim> getSubscripts(size_t linearIndex) const {
		return _dimension.getSubscripts(linearIndex);
	};

	void push_back(const Type &Value) { _values.push_back(Value); };
	void push_back(Type &&Value) { _values.push_back(std::move(Value)); };
	template<class... Args> void emplace_back(Args &&...args) { _values.emplace_back(args...); }
	void pop_back(size_t Size = 1) {
		for (size_t i = 0; i < Size; ++i) { _values.pop_back(); }
	}

	Type *addMemoryBlock(size_t Size) {
		const auto oldSize = _values.size();
		// add requested junk
		// Note: this must be done before ptr, because vector might be stored elsewhere in memory after resize!
		_values.resize(oldSize + Size);
		return _values.data() + oldSize; // return ptr to first element in requested junk
	}

	bool hasDefaultValues() const {
		// has default values if 1) empty or 2) all values have default value
		// (=Type)
		return (_values.empty() || std::all_of(_values.begin(), _values.end(), [](Type Val) { return Val == Type{}; }));
	}

	bool hasDimensions() const { return !_dimension.empty(); }

	constexpr const Type &operator[](size_t i) const noexcept { return _values[i]; };
	constexpr const Type &operator[](const std::array<size_t, NumDim> &Coord) const noexcept {
		return operator[](getIndex(Coord));
	};
	constexpr Type &operator[](size_t i) { return _values[i]; };
	constexpr Type &operator[](const std::array<size_t, NumDim> &Coord) { return operator[](getIndex(Coord)); };

	constexpr Type &operator[](const TRangeIterator &It) { return _values[It.cur()]; };
	constexpr Type &operator[](const TSliceIterator &It) { return _values[It.cur()]; };

	constexpr const Type &operator()(size_t i, size_t j) const noexcept {
		static_assert(NumDim == 2);
		return operator[]({i, j});
	};
	constexpr Type &operator()(size_t i, size_t j) noexcept {
		static_assert(NumDim == 2);
		return operator[]({i, j});
	};

	constexpr Type &at(size_t i) { return _values.at(i); }
	constexpr Type &at(const std::array<size_t, NumDim> &Coord) { return _values.at(getIndex(Coord)); }
	constexpr const Type &at(size_t i) const { return _values.at(i); }
	constexpr const Type &at(const std::array<size_t, NumDim> &Coord) const { return _values.at(getIndex(Coord)); }

	constexpr Type &front() noexcept { return _values.front(); }
	constexpr const Type &front() const noexcept { return _values.front(); }

	constexpr Type &back() noexcept { return _values.back(); }
	constexpr const Type &back() const noexcept { return _values.back(); }

	constexpr Type *data() noexcept { return _values.data(); }
	constexpr const Type *data() const noexcept { return _values.data(); }

	constexpr size_t size() const noexcept { return _values.size(); }
	size_t capacity() const { return _values.capacity(); };
	size_t numDim() const { return NumDim; };
	constexpr size_t max_size() const noexcept { return _values.max_size(); }

	void fill(const Type &value) noexcept { std::fill(_values.begin(), _values.end(), value); }
	void swap(TMultiDimensionalStorage<Type, NumDim> &other) noexcept(std::is_nothrow_swappable_v<Type>) {
		_values.swap(other._values);
	}

	constexpr iterator begin() noexcept { return _values.begin(); }
	constexpr iterator end() noexcept { return _values.end(); }

	constexpr const_iterator begin() const noexcept { return _values.begin(); }
	constexpr const_iterator end() const noexcept { return _values.end(); }

	constexpr const_iterator cbegin() const noexcept { return begin(); }
	constexpr const_iterator cend() const noexcept { return end(); }

	constexpr reverse_iterator rbegin() noexcept { return _values.rbegin(); }
	constexpr reverse_iterator rend() noexcept { return _values.rend(); }

	constexpr const_reverse_iterator rbegin() const noexcept { return _values.rbegin(); }
	constexpr const_reverse_iterator rend() const noexcept { return _values.rend(); }

	constexpr const_reverse_iterator crbegin() const noexcept { return rbegin(); }
	constexpr const_reverse_iterator crend() const noexcept { return rend(); }

	void resize(const std::vector<size_t> &dimensions) {
		assert(dimensions.size() == NumDim);
		assert(hasDefaultValues() || _innerDimensionsAreEqual(dimensions));
		// ATTENTION: if values have been filled already, this is a very dangerous function!
		// you should only change the last entry of the vector of dimensions
		// -> this does not change the coordinates of the values that have already been filled.
		// If a more inner dimension is changed, an already filled value will have different coordinates
		// before and after this function was called!
		_resize<true>(dimensions);
	};

	void resize(const std::array<size_t, NumDim> &dimensions) {
		assert(hasDefaultValues() || _innerDimensionsAreEqual(dimensions));
		// ATTENTION: if values have been filled already, this is a very dangerous function!
		// you should only change the last entry of the vector of dimensions
		// -> this does not change the coordinates of the values that have already been filled.
		// If a more inner dimension is changed, an already filled value will have different coordinates
		// before and after this function was called!
		_resize<true>(dimensions);
	};

	void reserve(const std::array<size_t, NumDim> &dimensions) {
		assert(hasDefaultValues() || _innerDimensionsAreEqual(dimensions));
		// ATTENTION: if values have been filled already, this is a very dangerous function!
		// you should only change the last entry of the vector of dimensions
		// -> this does not change the coordinates of the values that have already been filled.
		// If a more inner dimension is changed, an already filled value will have different coordinates
		// before and after this function was called!
		_resize<false>(dimensions);
	};

	void prepareFillData(size_t guessLengthOfUnknownDimension,
	                     const std::array<size_t, NumDim - 1> &allKnownDimensions) {
		// idea: usually we know all but one dimension
		// this unknown dimension must be the most outer one
		// (i.e. the first one, for example the number of rows in a matrix) usually we can approximately know how long
		// this unknown dimension will be (e.g. 1 oder 1'000'000) -> give this guess as a second parameter will resize
		// after all data have been filled in

		// check if any dimension is = 0 (invalid)
		if (guessLengthOfUnknownDimension < 1) {
			DEVERROR("Invalid guess of first (unknown) dimension: Size should "
			         "be > 0!");
		}
		for (auto &dim : allKnownDimensions) {
			if (dim == 0) { DEVERROR("Invalid length of dimension vector: Size should be > 0!"); }
		}

		// clear previously allocated memory of dimensions
		clear();

		// create vector of dimensions: first element is unknown dimension, all
		// others are from vector
		std::array<size_t, NumDim> dimensions;
		dimensions[0] = guessLengthOfUnknownDimension;
		for (size_t dim = 1; dim < NumDim; dim++) { dimensions[dim] = allKnownDimensions[dim - 1]; }
		_resize<false>(dimensions);
	}

	void finalizeFillData() {
		// determine length of first dimension

		// 1) get total size of all known dimensions
		double totalSizeAllButFirstDim = 1;
		if (NumDim > 1) {
			for (size_t dim = 1; dim < NumDim; dim++) {
				totalSizeAllButFirstDim *= dimensions()[dim]; // product of all but first dimension
			}
		}

		// 2) from this, we can calculate the size of the first dimension
		double lengthFirst = static_cast<double>(_values.size()) / totalSizeAllButFirstDim;

		// check if integer number
		if (static_cast<size_t>(lengthFirst) != lengthFirst) {
			DEVERROR("Error while filling data: Data seems to be ragged. "
			         "Expected the total number of data points to "
			         "be a multiple of ",
			         totalSizeAllButFirstDim, ", but got a factor of ", lengthFirst, " which is not a integer number.");
		}

		// resize storage to only contain filled values (std::vector will probably allocate more inside push_back than
		// we actually need in the end)
		std::array<size_t, NumDim> newDim = dimensions();
		newDim[0]                         = static_cast<size_t>(lengthFirst);
		_resize<true>(newDim);
		_values.shrink_to_fit(); // reduce std::vector capacity() to size()
	}

	// common arithmetic functions
	auto max_element() const { return std::max_element(_values.begin(), _values.end()); }
	auto min_element() const { return std::min_element(_values.begin(), _values.end()); }

	auto ix_max_element() const {
		return std::distance(_values.begin(), std::max_element(_values.begin(), _values.end()));
	}
	auto ix_min_element() const {
		return std::distance(_values.begin(), std::min_element(_values.begin(), _values.end()));
	}

	size_t numNonZero() const { return coretools::numNonZero(_values); }

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>>
	double sum() const { // do not want to sum in underlyting type -> e.g. bool will overflow very quickly
		return std::accumulate(_values.begin(), _values.end(), 0.0,
		                       [](double sum, auto v) { return sum + underlying(v); });
	}

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>>
	double prod() const { // do not want to multiply in underlyting type -> e.g. bool will overflow very quickly
		return std::accumulate(_values.begin(), _values.end(), 1.0,
		                       [](double prod, auto v) { return prod * underlying(v); });
	}

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>> double sumOfLogs() const {
		// calculates sum log(x)
		TSumLog<> sum;
		std::for_each(std::begin(_values), std::end(_values), [&sum](const auto v) { sum.add(underlying(v)); });
		return sum.getSum();
	}

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>> double sumOfLogsComplement() const {
		// calculates sum log(1-x)
		TSumLog<> sum;
		std::for_each(std::begin(_values), std::end(_values), [&sum](const auto v) { sum.add(1.0 - underlying(v)); });
		return sum.getSum();
	}

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>>
	double sumOfNormalizedSquares(double c) const {
		// calculates sum (x-c)^2
		// do not want to multiply in underlyting type -> e.g. bool will overflow very quickly
		return std::accumulate(_values.begin(), _values.end(), 0.0, [c](double sum, auto v) {
			double tmp = underlying(v) - c;
			return sum + tmp * tmp;
		});
	}

	double mean() const { return (double)sum() / _values.size(); }

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>> double var(double mean) const {
		// Note: calculates the unbiased sample variance (using the Bessel's correction n-1)
		double sumSq = 0.0;
		std::for_each(std::begin(_values), std::end(_values),
		              [&](const auto d) { sumSq += (underlying(d) - mean) * (underlying(d) - mean); });
		return sumSq / (_values.size() - 1);
	}

	double var() const { return var(mean()); }
	double sd() const { return sqrt(var()); }
	double sd(double mean) const { return sqrt(var(mean)); }

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>> auto countLevel(T Value) const {
		size_t c = 0;
		for (size_t i = 0; i < size(); ++i) {
			if (underlying(_values[i]) == Value) { ++c; };
		}
		return c;
	}

	template<typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>>
	auto countLevels(size_t MaxValuePlus1) const {
		std::vector<size_t> counters(MaxValuePlus1, 0);
		for (size_t i = 0; i < size(); ++i) { counters[underlying(_values[i])]++; }
		return counters;
	}

	template<typename Lambda, typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>>
	auto customSum(Lambda lambda) const {
		return std::accumulate(_values.begin(), _values.end(), 0.0,
		                       [&lambda](double sum, auto v) { return sum + lambda(underlying(v)); });
	}

	template<typename Lambda, typename T = Type, typename = std::enable_if_t<isCalculable_v<T>>>
	auto customLogSum(Lambda lambda) const {
		TSumLog<> sum;
		std::for_each(std::begin(_values), std::end(_values),
		              [&sum, &lambda](const auto v) { sum.add(lambda(underlying(v))); });
		return sum.getSum();
	}

	// dimension names

	void setDimensionName(const std::shared_ptr<TNamesEmpty> &Name, size_t Dim) {
		assert(Dim < NumDim);
		_dimensionNames[Dim] = Name;
	};

	void setDimensionNames(const std::array<std::shared_ptr<TNamesEmpty>, NumDim> &Names) { _dimensionNames = Names; };

	const std::array<std::shared_ptr<TNamesEmpty>, NumDim> &getDimensionNames() const { return _dimensionNames; };

	const std::shared_ptr<TNamesEmpty> &getDimensionName(size_t Dim) const {
		assert(Dim < NumDim);
		return _dimensionNames[Dim];
	};

	std::array<std::string, NumDim> getFullDimensionName(const std::array<size_t, NumDim> &Coord) const {
		// get each name per dimension for specific coordinates -> fill into array
		assert(NumDim > 0);
		std::array<std::string, NumDim> name;
		for (size_t dim = 0; dim < NumDim; dim++) { name[dim] = (*_dimensionNames[dim])[Coord[dim]]; }
		return name;
	};

	void appendToVectorOfAllFullDimensionNames(std::vector<std::string> &FullNames,
	                                           std::string_view Delimiter = "_") const {
		// fill vector fullNames with full names of all elements of storage
		assert(NumDim > 0);

		for (size_t i = 0; i < size(); ++i) {
			std::string oneDimName = str::concatenateString(getFullDimensionName(getSubscripts(i)), Delimiter);
			FullNames.push_back(oneDimName);
		}
	};

	std::string getFullDimensionNameWithPrefix(size_t LinearIndex, std::string_view Prefix,
	                                           std::string_view Delimiter = "_") const {
		std::string oneDimName = str::concatenateString(getFullDimensionName(getSubscripts(LinearIndex)), Delimiter);
		if (oneDimName.empty()) {
			// if dimension names are empty -> don't add Delimiter to Prefix
			return std::string{Prefix};
		} else {
			return std::string{Prefix}.append(Delimiter).append(oneDimName);
		}
	}

	void appendToVectorOfAllFullDimensionNamesWithPrefix(std::vector<std::string> &FullNames, std::string_view Prefix,
	                                                     std::string_view Delimiter = "_") const {
		// fill vector fullNames with full names of all elements of storage add prefix + Delimiter to each full Name
		assert(NumDim > 0);

		for (size_t i = 0; i < size(); ++i) {
			FullNames.push_back(getFullDimensionNameWithPrefix(i, Prefix, Delimiter));
		}
	};
};
}; // end namespace coretools

#endif // TMULTIDIMENSIONALSTORAGE_H
