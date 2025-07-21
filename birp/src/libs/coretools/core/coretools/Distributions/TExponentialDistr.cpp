#include "TExponentialDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TExponentialDistr::_precalculateTmpVars() {
	_logLambda = log(_lambda);
	_mean      = 1. / _lambda;
}

void TExponentialDistr::set(StrictlyPositive Lambda) {
	_lambda = Lambda;
	_precalculateTmpVars();
}

void TExponentialDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(lambda) with lambda > 0.", _lambda);
	_precalculateTmpVars();
}

Positive TExponentialDistr::density(Positive x) const noexcept { return density(x, _lambda); }

double TExponentialDistr::logDensity(Positive x) const noexcept {
	// calculates log density of a exponential distribution
	return _logLambda - _lambda * x;
}

Probability TExponentialDistr::cumulativeDensity(Positive x) const noexcept { return cumulativeDensity(x, _lambda); }

StrictlyPositive TExponentialDistr::mean() const noexcept { return _mean; }

Positive TExponentialDistr::sample() const noexcept { return sample(_lambda); }

}
