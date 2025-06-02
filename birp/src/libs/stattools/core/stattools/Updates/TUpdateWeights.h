//
// Created by madleina on 05.12.23.
//

#ifndef STATTOOLS_TUPDATEWEIGHTS_H
#define STATTOOLS_TUPDATEWEIGHTS_H

#include "coretools/Storage/TDimension.h"
#include "coretools/algorithms.h"
#include "stattools/ParametersObservations/spec.h"
#include <array>
#include <vector>

namespace stattools {

//------------------------------------------
// Calculate update weights in one dimension
//------------------------------------------

namespace UpdateWeights1D {
std::vector<double> calculateWeights(UpdateWeights Weights, const std::vector<double> &Stats, size_t Size,
									 const std::vector<std::string> &Args);
}

//-------------------------------------------
// TUpdateWeights
//-------------------------------------------

template<size_t NumDim, UpdateWeights... Weights> class TUpdateWeights {
private:
	static_assert(sizeof...(Weights) == NumDim);

	static auto _getWeightsPerDim(const coretools::TDimension<NumDim> &Dimension,
								  const std::array<std::vector<double>, NumDim> &Statistics,
								  const std::vector<std::string> &Args) {
		constexpr std::array<UpdateWeights, NumDim> types = {Weights...};

		std::array<std::vector<double>, NumDim> w_per_dim;
		for (size_t d = 0; d < NumDim; ++d) {
			w_per_dim[d] = UpdateWeights1D::calculateWeights(types[d], Statistics[d], Dimension.dimensions()[d], Args);
			coretools::normalize(w_per_dim[d]); // sum to 1
		}

		return w_per_dim;
	}

public:
	TUpdateWeights() = default;

	static std::vector<double> calculateLinearWeights(const coretools::TDimension<NumDim> &Dimension,
													  const std::array<std::vector<double>, NumDim> &Statistics,
													  const std::vector<std::string> &Args) {
		auto w_per_dim = _getWeightsPerDim(Dimension, Statistics, Args);

		// fill weights linearized over all dimensions
		std::vector<double> w_lin(Dimension.size(), 1.0);
		for (size_t i = 0; i < Dimension.size(); ++i) {
			auto coord = Dimension.getSubscripts(i);
			for (size_t d = 0; d < coord.size(); ++d) { w_lin[i] *= w_per_dim[d][coord[d]]; }
		}
		return w_lin;
	}

	static auto calculateCumWeightsPerDim(const coretools::TDimension<NumDim> &Dimension,
										  const std::array<std::vector<double>, NumDim> &Statistics,
										  const std::vector<std::string> &Args) {
		std::array<std::vector<double>, NumDim> w_per_dim = _getWeightsPerDim(Dimension, Statistics, Args);
		std::array<std::vector<double>, NumDim> cum_per_dim;
		for (size_t d = 0; d < NumDim; ++d) { coretools::fillCumulative(w_per_dim[d], cum_per_dim[d]); }
		return cum_per_dim;
	}

	static std::vector<double> cumWeights(const coretools::TDimension<NumDim> &Dimension,
										  const std::array<std::vector<double>, NumDim> &Statistics,
										  const std::vector<std::string> &Args) {
		auto linWeigths = calculateLinearWeights(Dimension, Statistics, Args);
		assert(std::fabs(coretools::containerSum(linWeigths) - 1.0) < 1e-10); // should sum to one
		coretools::normalize(linWeigths); // should already be 1, but make sure it really is for numeric reasons

		std::vector<double> cumWeights(Dimension.size());
		coretools::fillCumulative(linWeigths, cumWeights);
		return cumWeights;
	}
};

} // namespace stattools
#endif // STATTOOLS_TUPDATEWEIGHTS_H
