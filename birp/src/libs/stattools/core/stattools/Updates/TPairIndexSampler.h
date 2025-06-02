//
// Created by caduffm on 10/20/22.
//

#ifndef TINDEXSAMPLER_H
#define TINDEXSAMPLER_H

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Storage/TDimension.h"
#include "coretools/algorithms.h"
#include "stattools/ParametersObservations/TValue.h"
#include <cassert>
#include <vector>

namespace stattools {

//---------------------------------------
// TPairIndexSampler
//---------------------------------------

class TPairIndexSampler {
private:
	size_t _size = 0;

	size_t _len = 0;
	std::vector<size_t> _index_1;
	std::vector<size_t> _index_2;

protected:
	void _sampleIndices(size_t p) {
		// first element
		_index_1[0] = p;
		if (p + 1 == _size) {
			_index_2[0] = 0;
		} else {
			_index_2[0] = p + 1;
		}

		// all other elements
		for (size_t i = 1; i < _len; i++) {
			if (_index_1[i - 1] == 0) {
				_index_1[i] = _size - 1;
			} else {
				_index_1[i] = _index_1[i - 1] - 1;
			}

			if (_index_2[i - 1] == _size - 1) {
				_index_2[i] = 0;
			} else {
				_index_2[i] = _index_2[i - 1] + 1;
			}
		}
	}

public:
	TPairIndexSampler() = default;
	TPairIndexSampler(size_t Size) { set(Size); };

	void set(size_t Size) {
		_size = Size;
		_len  = std::floor(Size / 2);
		_index_1.resize(_len, 0);
		_index_2.resize(_len, 0);
	};

	void sampleIndices() {
		if (_size == 1) { return; }
		// draw a random position
		auto p = coretools::instances::randomGenerator().getRand<size_t>(0, _size);
		_sampleIndices(p);
	}

	size_t length() const { return _len; }

	std::pair<size_t, size_t> getIndexPair(size_t Index) const {
		assert(Index < _len);
		if (_size == 1) { return std::make_pair(0, 0); }
		return std::make_pair(_index_1[Index], _index_2[Index]);
	}
};

//---------------------------------------
// TPairIndexSamplerMultiDim
//---------------------------------------

template<size_t NumDim, size_t AlongDim> class TPairIndexSamplerMultiDim {
	static_assert(AlongDim < NumDim);

private:
	coretools::TDimension<NumDim> _dim;
	coretools::TDimension<NumDim> _dimSamplers;
	coretools::TDimension<NumDim> _dimUpdates;

	std::vector<TPairIndexSampler> _samplers;

	void _initDimSamplers() {
		// initialize _dimSamplers: same as dim, but has size = 1 for dimension 'AlongDim'
		auto dim      = _dim.dimensions();
		dim[AlongDim] = 1;
		_dimSamplers.init(dim);
	}

	void _initSamplers() {
		// set samplers: each runs across the size of 'AlongDim'
		const size_t numSamplers = _dimSamplers.size();
		assert(numSamplers == _dim.size() / _dim.dimensions()[AlongDim]);
		_samplers.resize(numSamplers);
		for (size_t i = 0; i < numSamplers; ++i) { _samplers[i].set(_dim.dimensions()[AlongDim]); }
	}

	void _initDimUpdates() {
		// initialize _dimUpdates: same as dim, but has number of pairs as size for dimension 'AlongDim'
		auto dim      = _dim.dimensions();
		if(_samplers.size() > 0)
			dim[AlongDim] = _samplers.front().length(); // should be the same for all samplers
		_dimUpdates.init(dim);
	}

public:
	TPairIndexSamplerMultiDim() = default;
	TPairIndexSamplerMultiDim(const coretools::TDimension<NumDim> &Dim) { set(Dim); };

	void set(const coretools::TDimension<NumDim> &Dim) {
		_dim = Dim;
		_initDimSamplers();
		_initSamplers();
		_initDimUpdates();
	};

	void sampleIndices() {
		for (auto &it : _samplers) { it.sampleIndices(); }
	}

	size_t length() const { return _dimUpdates.size(); }

	std::pair<size_t, size_t> getIndexPair(size_t Index) const {
		assert(Index < length());

		if constexpr (NumDim == 1) {
			assert(_samplers.size() == 1);
			return _samplers.front().getIndexPair(Index);
		}

		// get coordinates in update space: know which dimensions and which pair index to update
		auto coord                                = _dimUpdates.getSubscripts(Index);
		const auto pairIndex                      = coord[AlongDim];
		// set index in 'AlongDim' to zero to know which sampler to ask
		coord[AlongDim]                           = 0;
		const auto [ix1InAlongDim, ix2InAlongDim] = _samplers[_dimSamplers.getIndex(coord)].getIndexPair(pairIndex);

		// now calculate overall linear index
		coord[AlongDim] = ix1InAlongDim;
		size_t ix1      = _dim.getIndex(coord);
		coord[AlongDim] = ix2InAlongDim;
		size_t ix2      = _dim.getIndex(coord);

		return std::make_pair(ix1, ix2);
	}
};

//---------------------------------------
// TPairIndexSamplerPerType
//---------------------------------------

template<size_t NumTypes, typename ContainerType> class TPairIndexSamplerPerType {
private:
	using UnderlyingType = typename ContainerType::value_type;

	size_t _size     = 0;
	size_t _len      = 0;
	size_t _numPairs = 0;

	std::array<std::vector<size_t>, NumTypes> _indices;
	std::array<size_t, NumTypes + 1> _cumulEnds;
	std::array<size_t, NumTypes> _halfSize;

protected:
	size_t _half(size_t Size) const { return std::floor(Size / 2); }
	void _addIndex(size_t p, ContainerType Container) {
		// add index to vector corresponding to the type at that index
		const size_t type = value(Container[p]);
		_indices[type].push_back(p);
	}

	void _sampleIndicesLeft(size_t p, ContainerType Container) {
		// add first
		_addIndex(p, Container);
		// add all others (going left through circle)
		for (size_t i = 1; i < _len; i++) {
			if (p == 0) {
				p = _size - 1;
			} else {
				--p;
			}
			_addIndex(p, Container);
		}
	}

	void _sampleIndicesRight(size_t p, ContainerType Container) {
		// add all (going right through circle)
		for (size_t i = 0; i < _len; i++) {
			if (p == _size - 1) {
				p = 0;
			} else {
				++p;
			}
			_addIndex(p, Container);
		}
	}

	void _sampleIndices(size_t p, ContainerType Container) {
		clear();

		_sampleIndicesLeft(p, Container);
		_sampleIndicesRight(p, Container);

		// fill half-sizes per type
		for (size_t t = 0; t < NumTypes; ++t) { _halfSize[t] = _half(_indices[t].size()); }
		_numPairs = coretools::containerSum(_halfSize);

		// fill cumulative vector ends
		_cumulEnds[0] = 0;
		for (size_t t = 1; t < NumTypes + 1; ++t) { _cumulEnds[t] = _cumulEnds[t - 1] + _halfSize[t - 1]; }
	}

public:
	TPairIndexSamplerPerType(size_t Size) : _size(Size), _len(_half(Size)) {
		for (auto &it : _indices) { it.reserve(Size); }
	};

	void sampleIndices(ContainerType Container) {
		if (_size == 1) { return; }
		// draw a random position
		auto p = coretools::instances::randomGenerator().getRand<size_t>(0, _size);
		_sampleIndices(p, Container);
	}

	void clear() {
		for (auto &it : _indices) { it.clear(); }
	}

	size_t length() const { return _numPairs; }

	std::pair<size_t, size_t> getIndexPair(size_t Index) const {
		assert(Index < _numPairs);
		if (_size == 1) { return std::make_pair(0, 0); }
		for (size_t t = 1; t < NumTypes + 1; ++t) {
			const size_t type = t - 1;
			if (Index >= _cumulEnds[type] && Index < _cumulEnds[t]) {
				size_t ixInType = Index - _cumulEnds[type];

				const auto first  = _indices[type][ixInType];
				const auto second = _indices[type][ixInType + _halfSize[type]];
				return std::make_pair(first, second);
			}
		}
		DEVERROR("Failed to find Index ", Index, "!");
	}
};

} // namespace stattools

#endif // TINDEXSAMPLER_H
