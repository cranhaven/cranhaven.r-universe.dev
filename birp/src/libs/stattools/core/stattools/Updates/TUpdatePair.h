//
// Created by madleina on 06.12.23.
//

#ifndef STATTOOLS_TUPDATEPAIR_H
#define STATTOOLS_TUPDATEPAIR_H

#include "TPairIndexSampler.h"
#include "stattools/ParametersObservations/spec.h"
#include "stattools/Updates/TUpdateWeights.h"

namespace stattools {

//--------------------------------------
// TUpdatePair
//-------------------------------------

template<size_t NumDim, size_t AlongDim, UpdateWeights... Weights> class TUpdatePair {
	static_assert(AlongDim < NumDim);
	static_assert((true && ... && (Weights == UpdateWeights::regular))); // must all be regular

	/*
	 * Note that if this becomes speed relevant, the code could be changed such that all one-dimensional slices use the
	 * same pair indices, opposite to each slice using random pair indices.
	 */
private:
	TPairIndexSamplerMultiDim<NumDim, AlongDim> _sampler;

public:
	TUpdatePair() = default;
	TUpdatePair(const std::array<size_t, NumDim> &Dimension, size_t NumThreads) { initialize(Dimension, NumThreads); };

	size_t initialize(const std::array<size_t, NumDim> &Dimension, size_t NumThreads) {
		_sampler.set(coretools::TDimension<NumDim>(Dimension));
		return std::min(NumThreads, numUpdates());
	}

	void setWeights(const std::array<std::vector<double>, NumDim> & /*Statistics*/,
					const std::vector<std::string> & /*Args*/, coretools::Positive /*FracUpdates*/) {
		throw coretools::TDevError("Attempt to set weights failed: did you forget to specify update weights in parameter specification?");
	}
	void setWeights(const std::vector<double> & /*Weights*/, coretools::Positive /*FracUpdates*/) {
		throw coretools::TDevError("Attempt to set weights failed: did you forget to specify update weights in parameter specification?");
	}

	void prepareIteration() { _sampler.sampleIndices(); }
	size_t numUpdates() const { return _sampler.length(); }

	coretools::TRange pick(size_t Index, size_t /*ThreadNum*/) const {
		// ignore thread number
		return coretools::TRange(_sampler.getIndexPair(Index));
	}
};

//--------------------------------------
// TUpdatePairWeighted
//-------------------------------------

template<size_t NumDim, size_t AlongDim, UpdateWeights... Weights> class TUpdatePairWeighted {
	static_assert(AlongDim < NumDim);

	/* Note: In case pragma critical turns out to be a bottleneck, we could switch to another algorithm. First, sample
	 * all possible index pairs, as in \class{TUpdatePair}, and then remove index pairs according to the cumulative
	 * weights. This ensures that one index is never present in more than one pair, therefore avoiding race conditions.
	 * However, in order to update sufficiently often, we might need to re-sample weights in an extra loop.
	 */

private:
	coretools::TDimension<NumDim> _dim;
	std::vector<size_t> _curUpdatedIndices;

	// cumulative weights (shared across threads, but per dimension)
	std::array<std::vector<double>, NumDim> _cumWeights;

	// fraction of updates per iteration
	double _fracUpdates = 1.0;

	bool _updateCurIndices(size_t Index, bool FirstOfPair, size_t ThreadNum) {
		assert(ThreadNum * 2 < _curUpdatedIndices.size());
		bool ok = false;
#ifdef _OPENMP
#pragma omp critical
#endif
		{
			if (std::find(_curUpdatedIndices.begin(), _curUpdatedIndices.end(), Index) == _curUpdatedIndices.end()) {
				// index is currently not updated -> ok!
				_curUpdatedIndices[ThreadNum + FirstOfPair] = Index;
				ok                                          = true;
			}
		}
		return ok;
	}

	std::pair<size_t, std::array<size_t, NumDim>> _pickOne() {
		std::array<size_t, NumDim> coord;
		for (size_t d = 0; d < NumDim; ++d) {
			coord[d] = coretools::instances::randomGenerator().pickOne(_cumWeights[d]);
		}
		return {_dim.getIndex(coord), coord};
	}

	size_t _pickSecond(const std::array<size_t, NumDim> &Coord_1) {
		// pick second along the same dimensions (except AlongDim) as ix_1
		const size_t ix_1_alongDim = Coord_1[AlongDim];
		const auto cumAlongDim     = _cumWeights[AlongDim];

		const double q      = cumAlongDim[ix_1_alongDim];
		const double p_prev = (ix_1_alongDim == 0) ? 0.0 : cumAlongDim[ix_1_alongDim - 1];
		const double p      = cumAlongDim[ix_1_alongDim] - p_prev;
		double r            = coretools::instances::randomGenerator().getRand();

		double l_2 = q + r * (1.0 - p);
		if (l_2 > 1.0) { l_2 -= 1.0; }

		size_t ix_2_alongDim =
			std::distance(cumAlongDim.begin(), std::upper_bound(cumAlongDim.begin(), cumAlongDim.end(), l_2));
		auto coord      = Coord_1;
		coord[AlongDim] = ix_2_alongDim;
		return _dim.getIndex(coord);
	}

	std::pair<size_t, size_t> _pick(size_t ThreadNum) {
		// pick first (repeatedly in case this index is already updated)
		auto [ix_1, coord_1] = _pickOne();
		while (!_updateCurIndices(ix_1, true, ThreadNum)) { std::tie(ix_1, coord_1) = _pickOne(); }

		// pick second
		size_t ix_2 = _pickSecond(coord_1);
		while (!_updateCurIndices(ix_2, false, ThreadNum)) { ix_2 = _pickSecond(coord_1); }

		return {ix_1, ix_2};
	}

public:
	TUpdatePairWeighted() = default;
	TUpdatePairWeighted(const std::array<size_t, NumDim> &Dimension, size_t NumThreads) {
		initialize(Dimension, NumThreads);
	}

	size_t initialize(const std::array<size_t, NumDim> &Dimension, size_t NumThreads) {
		_dim.init(Dimension);
		size_t numThreads = std::min(NumThreads, numUpdates());

		// initialize _curUpdatedIndices: 2 * NumThreads since we always update two (pair) at a time
		_curUpdatedIndices.resize(2 * numThreads, std::numeric_limits<size_t>::max());

		return numThreads;
	}

	void setWeights(const std::vector<double> & /*Weights*/, coretools::Positive /*FracUpdates*/) {
		throw coretools::TDevError("Currently not implemented");
	}

	void setWeights(const std::array<std::vector<double>, NumDim> &Statistics, const std::vector<std::string> &Args,
					coretools::Positive FracUpdates) {
		// get cumulative weights
		_cumWeights  = TUpdateWeights<NumDim, Weights...>::calculateCumWeightsPerDim(_dim, Statistics, Args);
		_fracUpdates = FracUpdates;
	}

	void prepareIteration() {}
	[[nodiscard]] size_t numUpdates() const { return (size_t)std::floor(_fracUpdates * std::floor(_dim.size() / 2)); }

	coretools::TRange pick(size_t /*Index*/, size_t ThreadNum) {
		// reset thread indices
		_curUpdatedIndices[ThreadNum]     = std::numeric_limits<size_t>::max();
		_curUpdatedIndices[ThreadNum + 1] = std::numeric_limits<size_t>::max();

		// ignore index, draw according to cum weights
		const auto [ix_1, ix_2] = _pick(ThreadNum);
		assert(ix_1 != ix_2);

		return coretools::TRange(ix_1, ix_2);
	}
};

} // namespace stattools
#endif // STATTOOLS_TUPDATEPAIR_H
