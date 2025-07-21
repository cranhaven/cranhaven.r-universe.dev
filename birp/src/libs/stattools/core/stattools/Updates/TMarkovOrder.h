//
// Created by madleina on 23.01.24.
//

#ifndef STATTOOLS_TMARKOVORDER_H
#define STATTOOLS_TMARKOVORDER_H

#include "coretools/Storage/TDimension.h"

namespace stattools {

template<size_t NumDim> class TMarkovOrder {
private:
	coretools::TDimension<NumDim> _dimFull;
	coretools::TDimension<NumDim> _markovOrderPlus1;

	size_t _numPickers = 0;
	std::vector<std::array<size_t, NumDim>> _coordStartInFull;
	std::vector<size_t> _linearStartInFull;
	std::vector<coretools::TDimension<NumDim>> _dimPicker;

	bool _allIndependent = false; // true if all markov orders are zero
	bool _allDependent   = false; // true if all markov orders are max-1

	void _constrainMarkovOrder(std::array<size_t, NumDim> &MarkovOrder, const std::array<size_t, NumDim> &Dimension) {
		// make sure Markov order is never larger than the length of that dimension - 1
		for (size_t d = 0; d < NumDim; ++d) { MarkovOrder[d] = std::min(MarkovOrder[d], Dimension[d] - 1); }
	}

	[[nodiscard]] size_t _calculateNumPickers() const {
		size_t n = 1;
		for (size_t d = 0; d < NumDim; ++d) { n *= _markovOrderPlus1.dimensions()[d]; }
		return n;
	}

	[[nodiscard]] std::array<size_t, NumDim> _calculateMarkovOrderPlus1(std::array<size_t, NumDim> MarkovOrder) const {
		for (size_t d = 0; d < NumDim; ++d) { ++MarkovOrder[d]; }
		return MarkovOrder;
	}

	[[nodiscard]] size_t _calculateDimPicker(size_t Picker, size_t dim) const {
		const auto l = _dimFull.dimensions()[dim] - _coordStartInFull[Picker][dim];
		return std::ceil((double)l / (double)_markovOrderPlus1.dimensions()[dim]);
	}

	void _initializeStartPickers() {
		_coordStartInFull.resize(_numPickers);
		_linearStartInFull.resize(_numPickers);

		for (size_t p = 0; p < _numPickers; ++p) {
			_coordStartInFull[p] = _markovOrderPlus1.getSubscripts(p); // get coordinates within (m+1)
			if (_dimFull.empty()) {
				_linearStartInFull[p] = 0;
			} else {
				_linearStartInFull[p] = _dimFull.getIndex(_coordStartInFull[p]); // linearize in full
			}
		}
	}

	void _initializeDimPickers() {
		_dimPicker.resize(_numPickers);

		for (size_t p = 0; p < _numPickers; ++p) {
			std::array<size_t, NumDim> dim;
			for (size_t d = 0; d < NumDim; ++d) { dim[d] = _calculateDimPicker(p, d); }
			_dimPicker[p].init(dim);
		}
	}

public:
	TMarkovOrder() = default;

	TMarkovOrder(std::array<size_t, NumDim> MarkovOrder, const std::array<size_t, NumDim> &Dimension) {
		initialize(MarkovOrder, Dimension);
	}

	void initialize(std::array<size_t, NumDim> MarkovOrder, const std::array<size_t, NumDim> &Dimension) {
		_dimFull.init(Dimension);
		_constrainMarkovOrder(MarkovOrder, Dimension);
		_markovOrderPlus1.init(_calculateMarkovOrderPlus1(MarkovOrder));
		_numPickers     = _calculateNumPickers();
		_allIndependent = std::all_of(MarkovOrder.begin(), MarkovOrder.end(), [](size_t i) { return i == 0; });

		_allDependent = true;
		for (size_t d = 0; d < NumDim; ++d) {
			if (MarkovOrder[d] != (Dimension[d] - 1)) { _allDependent = false; }
		}

		// initialize start pickers
		_initializeStartPickers();

		// initialize dimPickers
		_initializeDimPickers();
	}

	[[nodiscard]] size_t getNumPickers() const { return _numPickers; }

	[[nodiscard]] size_t length(size_t Picker) const { return _dimPicker[Picker].size(); }

	[[nodiscard]] size_t ixInFull(size_t Picker, size_t LinearIndexInPicker) const {
		if (_allIndependent) { return LinearIndexInPicker; } // there is only 1 picker -> just return index
		if (_allDependent) { return _dimFull.getIndex(_coordStartInFull[Picker]); } // there is 1 picker per index

		auto index = _dimPicker[Picker].getSubscripts(LinearIndexInPicker);

		// blow up to 2D index in full
		for (size_t d = 0; d < NumDim; ++d) {
			index[d] = index[d] * _markovOrderPlus1.dimensions()[d] + _coordStartInFull[Picker][d];
		}

		// linearize in full
		return _dimFull.getIndex(index);
	}

	// other getters
	[[nodiscard]] size_t linearStartInFull(size_t Picker) const { return _linearStartInFull[Picker]; }
	[[nodiscard]] auto fullDimensions() const { return _dimFull.dimensions(); }
};

} // namespace stattools

#endif // STATTOOLS_TMARKOVORDER_H
