//
// Created by madleina on 07.12.23.
//

#ifndef STATTOOLS_TUPDATEJOINT_H
#define STATTOOLS_TUPDATEJOINT_H

#include "coretools/Storage/TDimension.h"
#include "coretools/Types/commonWeakTypes.h"
#include "stattools/ParametersObservations/spec.h"

namespace stattools {

//--------------------------------------
// TUpdateJoint
//-------------------------------------

template<size_t NumDim, size_t AlongDim, UpdateWeights... Weights> class TUpdateJoint {
	static_assert(AlongDim < NumDim);
	static_assert((true && ... && (Weights == UpdateWeights::regular))); // must all be regular

private:
	coretools::TDimension<NumDim> _dim;
	coretools::TDimension<NumDim> _dimUpdates;

public:
	TUpdateJoint() = default;
	TUpdateJoint(const std::array<size_t, NumDim> &Dimension, size_t NumThreads) { initialize(Dimension, NumThreads); };

	size_t initialize(const std::array<size_t, NumDim> &Dimension, size_t NumThreads) {
		_dim.init(Dimension);

		// initialize _dimUpdates: same as dim, but has size = 1 for dimension 'AlongDim'
		auto dim      = _dim.dimensions();
		dim[AlongDim] = 1;
		_dimUpdates.init(dim);

		return std::min(NumThreads, numUpdates());
	}

	void setWeights(const std::array<std::vector<double>, NumDim> & /*Statistics*/,
	                const std::vector<std::string> & /*Args*/, coretools::Positive /*FracUpdates*/) {
		DEVERROR("TUpdateJoint is currently not compatible with update weights!");
	}
	void setWeights(const std::vector<double> & /*Weights*/, coretools::Positive /*FracUpdates*/) {
		DEVERROR("TUpdateJoint is currently not compatible with update weights!");
	}

	void prepareIteration() {}
	size_t numUpdates() const { return _dimUpdates.size(); }

	coretools::TRange pick(size_t Index, size_t /*ThreadNum*/) const {
		// ignore thread number
		auto coord = _dimUpdates.getSubscripts(Index);
		return _dim.get1DSlice(AlongDim, coord);
	}
};

// TUpdateJoint with weights: currently not implemented!

} // namespace stattools
#endif // STATTOOLS_TUPDATEJOINT_H
