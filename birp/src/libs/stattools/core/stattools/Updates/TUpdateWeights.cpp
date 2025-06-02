//
// Created by madleina on 05.12.23.
//

#include "TUpdateWeights.h"

namespace stattools {
namespace UpdateWeights1D {
using coretools::P;
//------------------------------------
// free helper functions
//------------------------------------

void checkSize(const std::vector<double> &Stats, size_t Size) {
	if (Stats.size() != Size) {
		DEVERROR("Size of Stats (", Stats.size(), ") does not match size of dimension (", Size, ")!");
	}
}

//------------------------------------
// regular
//------------------------------------

std::vector<double> calculateWeightsRegular(const std::vector<double> & /*Stats*/, size_t Size,
											const std::vector<std::string> & /*Args*/) {
	// just return all equal weights
	return std::vector<double>(Size, 1.0);
}

//------------------------------------
// irregular
//------------------------------------

std::vector<double> calculateWeightsIrregular(const std::vector<double> &Stats, size_t Size,
											  const std::vector<std::string> & /*Args*/) {
	// just use these weights, unchanged
	checkSize(Stats, Size);
	return Stats;
}

//------------------------------------
// geometricUniform
//------------------------------------

auto readArgsGeometricUniform(const std::vector<std::string> &Args) {
	auto propBest = P(0.2);
	auto effort   = P(0.9);
	if (!Args.empty()) {
		if (Args.size() == 2) {
			propBest = P(Args[0]);
			effort   = P(Args[1]);
		} else {
			DEVERROR("Invalid arguments to geometric uniform update weights: expected 2, got ", Args.size(), " (", Args,
					 ").");
		}
	}
	return std::make_pair(propBest, effort);
}

std::vector<double> calculateWeightsGeometricUniform(const std::vector<double> &Stats, size_t Size,
													 const std::vector<std::string> &Args) {

	checkSize(Stats, Size);
	const auto [propBest, effort] = readArgsGeometricUniform(Args);

	// calculate parameter p of geometric distribution
	// calculates the parameter p of the geometric distribution
	// _effort% of my effort should be in the first _propBest% of size
	// parameter p can be computed from CDF of geometric distribution
	// by assuming that size is infinitely large (ok if size > 100)
	double p = 1. - exp((coretools::LogProbability)effort.complement() / (propBest * (double)Size));

	// calculate weight for every index as Q_l = 0.3*U_l + 0.7*G_l where U_l is the uniform distribution
	// (and hence 1 / _size) and G_l is the geometric distribution
	double uniform = 1. / (double)Size;
	std::vector<double> weights(Size, 0.);
	for (size_t l = 0; l < Size; l++) {
		auto match      = std::find(Stats.begin(), Stats.end(), (double)l);
		size_t position = std::distance(Stats.begin(), match);
		weights[l]      = 0.3 * uniform + 0.7 * p * pow(1. - p, (double)position);
	}

	return weights;
}

//------------------------------------
// log10StatePosterior
//------------------------------------

std::vector<double> calculateWeightsLog10StatePosterior(const std::vector<double> &Stats, size_t Size,
														const std::vector<std::string> & /*Args*/) {
	checkSize(Stats, Size);

	std::vector<double> weights(Size, 0.);
	for (size_t l = 0; l < Size; l++) {
		// cap at 0.01 and 0.99
		double p   = std::min(Stats[l], 0.99);
		p          = std::max(p, 0.01);
		weights[l] = -log10(1. - p);
	}

	return weights;
}

//------------------------------------
// powerStatePosterior
//------------------------------------

auto readArgsPowerStatePosterior(const std::vector<std::string> &Args) {
	auto intercept  = P(0.1);
	double exponent = 0.5;
	if (!Args.empty()) {
		if (Args.size() == 2) {
			intercept = P(Args[0]);
			exponent  = coretools::str::fromStringCheck<double>(Args[1]);
		} else {
			DEVERROR("Invalid arguments to power state posterior update weights: expected 2, got ", Args.size(), " (",
					 Args, ").");
		}
	}
	return std::make_pair(intercept, exponent);
}

std::vector<double> calculateWeightsPowerStatePosterior(const std::vector<double> &Stats, size_t Size,
														const std::vector<std::string> &Args) {
	const auto [intercept, exponent] = readArgsPowerStatePosterior(Args);

	std::vector<double> weights(Size, 0.);
	for (size_t l = 0; l < Size; l++) { weights[l] = intercept + (1.0 - intercept) * std::pow(Stats[l], exponent); }

	return weights;
}

//------------------------------------
// calculateWeights
//------------------------------------

std::vector<double> calculateWeights(UpdateWeights Weights, const std::vector<double> &Stats, size_t Size,
									 const std::vector<std::string> &Args) {
	switch (Weights) {
	case UpdateWeights::regular: return calculateWeightsRegular(Stats, Size, Args);
	case UpdateWeights::irregular: return calculateWeightsIrregular(Stats, Size, Args);
	case UpdateWeights::geometricUniform: return calculateWeightsGeometricUniform(Stats, Size, Args);
	case UpdateWeights::log10StatePosterior: return calculateWeightsLog10StatePosterior(Stats, Size, Args);
	case UpdateWeights::powerStatePosterior: return calculateWeightsPowerStatePosterior(Stats, Size, Args);
	default: DEVERROR("Should never get here!");
	}
}
} // namespace UpdateWeights1D
} // namespace stattools
