#include "TPoissonDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {

void TPoissonDistr::_precalculateTmpVars() {
	_logLambda = log(_lambda);
	_expLambda = exp(-_lambda);
}

void TPoissonDistr::set(StrictlyPositive Lambda) {
	_lambda = Lambda;
	_precalculateTmpVars();
}

void TPoissonDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(lambda) with lambda > 0.", _lambda);
	_precalculateTmpVars();
}

Probability TPoissonDistr::density(size_t k) const noexcept {
	// calculates density of a Poisson distribution
	return P(std::pow(_lambda, (double)k) * _expLambda / TFactorial::factorial(k));
}

LogProbability TPoissonDistr::logDensity(size_t k) const noexcept {
	// calculates log density of a Poisson distribution
	return logP((double)k * _logLambda - _lambda - TFactorial::factorialLog(k));
};

Probability TPoissonDistr::cumulativeDensity(double k) const noexcept {
	// Note: accepts k as a double, but will take floor to have next-smaller integer
	// as the CDF is discontinuous at the integers of k and flat everywhere else
	return cumulativeDensity(k, _lambda);
};

StrictlyPositive TPoissonDistr::mean() const noexcept { return mean(_lambda); };

size_t TPoissonDistr::sample() const noexcept { return sample(_lambda); }

}
