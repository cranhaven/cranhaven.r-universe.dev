//
// Created by madleina on 05.12.23.
//

#ifndef STATTOOLS_TUPDATESINGLE_H
#define STATTOOLS_TUPDATESINGLE_H

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Storage/TDimension.h"
#include "stattools/ParametersObservations/spec.h"
#include "stattools/Updates/TMarkovOrder.h"
#include "stattools/Updates/TUpdateWeights.h"

namespace stattools {

//--------------------------------------
// TUpdateSingle
//-------------------------------------

template<size_t NumDim, UpdateWeights... Weights> class TUpdateSingle {
	static_assert((true && ... && (Weights == UpdateWeights::regular))); // must all be regular
private:
	size_t _pickerIndex = 0;
	TMarkovOrder<NumDim> _markovOrder;

public:
	TUpdateSingle() = default;
	TUpdateSingle(const TMarkovOrder<NumDim> &MarkovOrder, size_t PickerIndex, size_t NumThreads) {
		initialize(MarkovOrder, PickerIndex, NumThreads);
	}

	size_t initialize(const TMarkovOrder<NumDim> &MarkovOrder, size_t PickerIndex, size_t NumThreads) {
		_markovOrder = MarkovOrder;
		_pickerIndex = PickerIndex;
		return std::min(NumThreads, numUpdates()); // make sure there are no more threads than elements
	}

	void setWeights(const std::array<std::vector<double>, NumDim> & /*Statistics*/,
					const std::vector<std::string> & /*Args*/, coretools::Positive /*FracUpdates*/) {
		throw coretools::TDevError("Attempt to set weights failed: did you forget to specify update weights in parameter specification?");
	}
	void setWeights(const std::vector<double> & /*Weights*/, coretools::Positive /*FracUpdates*/) {
		throw coretools::TDevError("Attempt to set weights failed: did you forget to specify update weights in parameter specification?");
	}

	void prepareIteration() {}
	size_t numUpdates() const { return _markovOrder.length(_pickerIndex); }

	coretools::TRange pick(size_t Index, size_t /*ThreadNum*/) const {
		assert(Index < numUpdates());
		// ignore thread number, return index (factor out picker index)
		return coretools::TRange(_markovOrder.ixInFull(_pickerIndex, Index));
	}
};

//--------------------------------------
// TUpdateSingleWeighted
//-------------------------------------

template<size_t NumDim, UpdateWeights... Weights> class TUpdateSingleWeighted {
	/*
	 * Note: If drawing a random number to draw from cumulative weights becomes speed relevant, we might switch to
	 * another algorithm where the indices are repeated by as many times as they occur according to the weights.
	 * E.g. 3 indices A (0.8), B (0.1) and C (0.1): should be updated 10 times
	 * A B C A A A A A A A
	 * -> loop over this vector and take the index to update from this
	 * -> remember last visited element and re-start from there for next iteration
	 */
private:
	// cumulative weights, one per thread
	std::vector<std::vector<double>> _cumWeightsPerThread;

	// number of elements to the left for each thread
	std::vector<size_t> _numElementsLeftOfThread;

	// sizes
	size_t _pickerIndex = 0;
	TMarkovOrder<NumDim> _markovOrder;
	size_t _numThreads = 1;
	coretools::TDimension<NumDim> _dim;

	// fraction of updates per iteration
	double _fracUpdates = 1.0;

	static size_t _getClosestMatch(const std::vector<double> &Cum, double ApproxEnd) {
		auto it = std::lower_bound(Cum.begin(), Cum.end(), ApproxEnd);
		if (it == Cum.end()) { throw coretools::TDevError("Could not find ", *it, " in cumulative weights!"); }
		if (it != Cum.begin()) { // check if the one on the left is actually closer
			if (std::fabs(*it - ApproxEnd) > std::fabs(*(it - 1) - ApproxEnd)) { it--; }
		}
		return std::distance(Cum.begin(), it);
	}

	std::vector<double> _splitWeights(const std::vector<double> &LinWeights) const {
		std::vector<double> vec(_markovOrder.length(_pickerIndex));

		for (size_t i = 0; i < _markovOrder.length(_pickerIndex); ++i) {
			const auto ix = _markovOrder.ixInFull(_pickerIndex, i);
			assert(ix < LinWeights.size());
			vec[i] = LinWeights[ix];
		}
		return vec;
	}

	std::vector<size_t> _getLastIndexPerThread(const std::vector<double> &LinWeights, size_t NumThreads) {
		// fill cumulative of weights
		std::vector<double> cum;
		coretools::fillCumulative(LinWeights, cum);

		// prepare memory
		_numElementsLeftOfThread.resize(NumThreads, 0);
		std::vector<size_t> lastIndexPerThread(NumThreads, 0);

		// bin the cumulative weights such that each thread gets approx. the same cum weights
		// but make sure each threads gets at least one index to update
		double binSize = 1.0 / (double)NumThreads;
		for (size_t t = 0; t < NumThreads; ++t) {
			// get index of last element assigned to thread t
			double approxEnd = binSize * ((double)t + 1.0);
			size_t ix        = _getClosestMatch(cum, approxEnd);

			// thread may not use the same index as the previous thread
			if (t > 0 && ix <= lastIndexPerThread[t - 1]) { ix = lastIndexPerThread[t - 1] + 1; }
			assert(ix < cum.size());

			// thread must leave enough indices for the next threads
			size_t toLeave        = NumThreads - t - 1;
			size_t max_ix         = cum.size() - toLeave - 1;
			lastIndexPerThread[t] = std::min(ix, max_ix);

			if (t > 0) { _numElementsLeftOfThread[t] = lastIndexPerThread[t - 1] + 1; }
		}
		return lastIndexPerThread;
	}

	void _fillCumPerThread(const std::vector<double> &LinWeights, size_t NumThreads) {
		std::vector<size_t> lastIndexPerThread = _getLastIndexPerThread(LinWeights, NumThreads);

		_cumWeightsPerThread.resize(NumThreads);
		auto first = LinWeights.begin();
		for (size_t t = 0; t < NumThreads; ++t) {
			// cut out this bin from the weights
			auto last = LinWeights.begin() + lastIndexPerThread[t] + 1;
			std::vector<double> weightsThisThread(first, last);

			// fill cumulative
			coretools::normalize(weightsThisThread);
			coretools::fillCumulative(weightsThisThread, _cumWeightsPerThread[t]);

			first = last;
		}
	}

public:
	TUpdateSingleWeighted() = default;
	TUpdateSingleWeighted(const TMarkovOrder<NumDim> &MarkovOrder, size_t PickerIndex, size_t NumThreads) {
		initialize(MarkovOrder, PickerIndex, NumThreads);
	}

	size_t initialize(const TMarkovOrder<NumDim> &MarkovOrder, size_t PickerIndex, size_t NumThreads) {
		_markovOrder = MarkovOrder;
		_pickerIndex = PickerIndex;
		_dim.init(MarkovOrder.fullDimensions());

		// make sure we don't use more threads than there are elements
		_numThreads = std::min(NumThreads, numUpdates());
		return _numThreads;
	}

	void setWeights(const std::vector<double> &LinearWeights, coretools::Positive FracUpdates) {
		// Weights are already linearized (per index)
		// only keep weights that are in sequence Start:end, Incr
		const auto w = _splitWeights(LinearWeights);
		// fill cumulative per thread
		_fillCumPerThread(w, _numThreads);

		_fracUpdates = FracUpdates;
	}

	void setWeights(const std::array<std::vector<double>, NumDim> &Statistics, const std::vector<std::string> &Args,
					coretools::Positive FracUpdates) {
		// Weights still need to be calculated: just have a statistics per dimension
		// calculate full linear weights
		auto w = TUpdateWeights<NumDim, Weights...>::calculateLinearWeights(_dim, Statistics, Args);
		setWeights(w, FracUpdates);
	}

	void prepareIteration() {}
	size_t numUpdates() const { return (size_t)std::floor(_fracUpdates * _markovOrder.length(_pickerIndex)); }

	coretools::TRange pick(size_t /*Index*/, size_t ThreadNum) const {
		// ignore index, draw according to cum weights
		const size_t ixInThread = coretools::instances::randomGenerator().pickOne(_cumWeightsPerThread[ThreadNum]);
		// get index in sequence Start/Inc (factor out thread)
		const size_t ixInSeq    = _numElementsLeftOfThread[ThreadNum] + ixInThread;
		// get overall index (factor out picker sequence)
		const size_t ixTotal    = _markovOrder.ixInFull(_pickerIndex, ixInSeq);

		return coretools::TRange(ixTotal);
	}

	const std::vector<double> &cumWeights(size_t Thread) { return _cumWeightsPerThread[Thread]; }
	size_t numElementsLeftOf(size_t Thread) { return _numElementsLeftOfThread[Thread]; }
};

} // namespace stattools

#endif // STATTOOLS_TUPDATESINGLE_H
