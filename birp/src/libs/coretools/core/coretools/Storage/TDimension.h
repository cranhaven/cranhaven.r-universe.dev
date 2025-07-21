//
// Created by madleina on 20.10.21.
//

#ifndef TDIMENSION_H
#define TDIMENSION_H

#include <array>

#include "coretools/algorithms.h"
#include "coretools/Main/TError.h"

namespace coretools {

//-------------------------------------------
// TRange / TRangeIterator
//-------------------------------------------

class TRange {
	// purpose of class: stores first and last index as well as increment
	// used for looping over multi-dimensional objects
public:
	size_t begin;
	size_t end;
	size_t increment;

	TRange() {
		begin     = 0;
		end       = 0;
		increment = 0;
	};

	explicit TRange(size_t First) {
		// constructor for single elements
		begin     = First;
		end       = begin + 1;
		increment = 1;
	};

	TRange(size_t First, size_t Second) {
		// constructor for pairs
		if (First > Second) { // turn around if order is not valid
			std::swap(First, Second);
		}
		begin     = First;
		end       = Second + 1;
		increment = Second - First;
	};

	TRange(std::pair<size_t, size_t> FirstSecond) : TRange(FirstSecond.first, FirstSecond.second){};

	TRange(size_t First, size_t Last, size_t Increment) { set(First, Last, Increment); };

	void set(size_t First, size_t Last, size_t Increment) {
		DEBUG_ASSERT(Last >= First);
		begin     = First;
		end       = Last;
		increment = Increment;
	}
};

class TRangeIterator {
private:
	TRange _range;
	size_t _cur;

public:
	TRangeIterator(const TRange &Range) {
		_range = Range;
		_cur   = _range.begin;
	}

	TRangeIterator &operator++() {
		_cur += _range.increment;
		return *this;
	}

	size_t cur() const { return _cur; }

	bool end() const { return _cur >= _range.end; }
};

//-------------------------------------------
// TSlice / TSliceIterator
//-------------------------------------------

class TSlice {
	// purpose of class: stores first and last index as well as how to increment across an arbitrary slice
	// used for looping over multi-dimensional objects
public:
	size_t begin;
	size_t end;
	size_t smallIncrement;
	size_t numSmallIncrementsBeforeJump;
	size_t largeIncrement;

	TSlice() {
		begin                        = 0;
		end                          = 0;
		smallIncrement               = 0;
		numSmallIncrementsBeforeJump = 0;
		largeIncrement               = 0;
	};

	TSlice(size_t First) {
		// constructor for single elements
		begin                        = First;
		end                          = begin + 1;
		smallIncrement               = 1;
		numSmallIncrementsBeforeJump = 1;
		largeIncrement               = 0;
	};

	TSlice(size_t First, size_t Last, size_t SmallIncrement, size_t NumSmallIncrementsBeforeJump,
	       size_t LargeIncrement) {
		begin                        = First;
		end                          = Last;
		smallIncrement               = SmallIncrement;
		numSmallIncrementsBeforeJump = NumSmallIncrementsBeforeJump;
		largeIncrement               = LargeIncrement;
	};
};

class TSliceIterator {
private:
	TSlice _slice;
	size_t _cur;
	size_t _numSmallSteps; // counts how often iterator was incremented with small increment

public:
	TSliceIterator(const TSlice &Slice) {
		_slice         = Slice;
		_cur           = _slice.begin;
		_numSmallSteps = 0;
	}

	TSliceIterator &operator++() {
		if (_numSmallSteps == _slice.numSmallIncrementsBeforeJump) {
			_cur += _slice.largeIncrement;
			_numSmallSteps = 0;
		} else {
			_cur += _slice.smallIncrement;
			++_numSmallSteps;
		}
		return *this;
	}

	size_t cur() const { return _cur; }

	bool end() const { return _cur >= _slice.end; }
};

//-------------------------------------------
// TDimension
//-------------------------------------------

template<size_t NumDim> class TDimension {
	// class that manages Dimensions: getting from multi-dimensional coordinates to linear indices and vice versa
	// also: loop over "slices", e.g. over all columns of a row
	// nice explanation of concept: https://eli.thegreenplace.net/2015/memory-layout-of-multi-dimensional-arrays
private:
	std::array<size_t, NumDim> _dimensions;
	size_t _totalSize;

public:
	TDimension() { _totalSize = 0; };

	explicit TDimension(const std::array<size_t, NumDim> &dimensions) { init(dimensions); };

	void init(const std::vector<size_t> &dimensions) {
		DEBUG_ASSERT(dimensions.size() == NumDim);
		std::copy(dimensions.begin(), dimensions.end(), _dimensions.begin());
		// multiplies all dimensions
		if (NumDim == 0) {
			_totalSize = 0;
		} else {
			_totalSize = std::accumulate(std::begin(_dimensions), std::end(_dimensions), 1UL, std::multiplies<>());
		}
	};

	void init(const std::array<size_t, NumDim> &dimensions) {
		_dimensions = dimensions;
		// multiplies all dimensions
		if (NumDim == 0) {
			_totalSize = 0;
		} else {
			_totalSize = std::accumulate(std::begin(_dimensions), std::end(_dimensions), 1UL, std::multiplies<>());
		}
	};

	void clear() {
		std::fill(std::begin(_dimensions), std::end(_dimensions), 0);
		_totalSize = 0;
	};

	// linearize multi-dimensional array
	size_t getIndex(const std::array<size_t, NumDim> &coord) const {
		return getLinearIndex<size_t, NumDim>(coord, _dimensions);
	};

	TRange getDiagonal() const {
		static_assert(NumDim == 2);
		DEV_ASSERT(_dimensions[0] == _dimensions[1]);
		TRange range(0, size(), _dimensions[0] + 1);
		return range;
	};

	TRange getRange(const std::array<size_t, NumDim> &startCoord, const std::array<size_t, NumDim> &endCoord) const {
		size_t first = getIndex(startCoord);
		size_t last  = getIndex(endCoord);

		DEV_ASSERT(startCoord <= endCoord);
		TRange range(first, last + 1, 1); // last + 1, as otherwise we could not access single elements by giving the
		                                  // same start and end coordinates
		return range;
	};

	TRange getFull() const {
		TRange range(0, size(), 1); // no need to do last + 1
		return range;
	};

	TRange get1DSlice(size_t dim, const std::array<size_t, NumDim> &startCoord) const {
		DEBUG_ASSERT(dim < NumDim);

		size_t first                        = getIndex(startCoord);
		// where's the last?
		std::array<size_t, NumDim> endCoord = startCoord;
		endCoord[dim]                       = _dimensions[dim] - 1; // go until the end of this dimension
		size_t last                         = getIndex(endCoord);
		// what is the increment?
		size_t inc                          = incrementInDim(dim);
		TRange range(first, last + 1, inc);
		return range;
	};

	TRangeIterator begin1DSlice(size_t dim, const std::array<size_t, NumDim> &startCoord) const {
		return get1DSlice(dim, startCoord);
	};

	auto getSlice(size_t dim, size_t indexInDim) const {
		DEBUG_ASSERT(dim < NumDim);

		size_t incInDim = incrementInDim(dim);
		size_t first    = incInDim * indexInDim;
		size_t end      = first + incInDim;
		if (dim > 0) { end += incrementInDim(dim - 1) * (sizeOuterDim(dim) - 1); }

		size_t smallIncrement = 1;
		if (dim == NumDim - 1) { smallIncrement = _dimensions[dim]; }

		if constexpr (NumDim > 2) {
			size_t jump = incInDim * (_dimensions[dim] - 1) + 1;

			TSlice range(first, end, smallIncrement, incInDim - 1, jump);
			return range;
		} else {
			TRange range(first, end, smallIncrement);
			return range;
		}
	};

	auto beginSlice(size_t dim, size_t indexInDim) const {
		const auto s = getSlice(dim, indexInDim);
		if constexpr (NumDim > 2) {
			return (TSliceIterator)s;
		} else {
			return (TRangeIterator)s;
		}
	}

	size_t size() const { return _totalSize; };
	bool empty() const { return _totalSize == 0; }

	size_t incrementInDim(size_t dim) const {
		size_t inc = 1;
		for (size_t j = dim + 1; j < NumDim; j++) { inc *= _dimensions[j]; }
		return inc;
	}

	size_t sizeOuterDim(size_t dim) const {
		size_t inc = 1;
		for (size_t j = 0; j < dim; j++) { inc *= _dimensions[j]; }
		return inc;
	}

	size_t numDim() const { return NumDim; };

	std::array<size_t, NumDim> dimensions() const { return _dimensions; };

	// re-construct index in multi-dimensional array from linear array
	std::array<size_t, NumDim> getSubscripts(size_t linearIndex) const {
		DEV_ASSERT(linearIndex < size());

		return getSubscriptsAsArray(linearIndex, _dimensions);
	};
};

//-------------------------------------------
// TMultiIndex
//-------------------------------------------

class TMultiIndex {
	// stores a map: linear index to multi-dimensional index (for all possible combinations)
private:
	std::vector<std::vector<size_t>> _indexMap;

public:
	TMultiIndex() = default;
	TMultiIndex(const std::vector<size_t> &Dimensions) { set(Dimensions); }

	void set(const std::vector<size_t> &Dimensions) {
		// fill map
		const auto N = coretools::containerProduct(Dimensions);
		_indexMap.resize(N);
		for (size_t i = 0; i < N; ++i) { _indexMap[i] = coretools::getSubscripts(i, Dimensions); }
	}

	// getters
	size_t size() const { return _indexMap.size(); }
	coretools::TConstView<size_t> get(size_t i) const { return _indexMap[i]; }
};

}; // end namespace coretools

#endif // TDIMENSION_H
