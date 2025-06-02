#include "TNormalDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TNormalDistr::_precalculateTmpVars() {
	_densDenom   = 1. / (sqrt_two_pi * _sd);
	_densExpo    = -1. / (2. * _sd * _sd);
	_var         = _sd * _sd;
	_logDens     = -0.5 * log(two_pi * _var);
	_twoVar      = 2. * _var;
	_negmSQRT2SD = -sqrt_2 * _sd;
}

void TNormalDistr::set(double mean, StrictlyPositive sd) {
	_mean = mean;
	_sd   = sd;
	_precalculateTmpVars();
}

void TNormalDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(mean, sd) with sd>0.", _mean, _sd);
	_precalculateTmpVars();
}

Positive TNormalDistr::density(double x) const noexcept {
	// calculates density of a normal distribution (= dnorm in R)
	return _densDenom * exp(_densExpo * (x - _mean) * (x - _mean));
}

double TNormalDistr::logDensity(double x) const noexcept {
	// calculates density of a normal distribution (= dnorm in R)
	return _logDens - 1. / _twoVar * (x - _mean) * (x - _mean);
}

Probability TNormalDistr::cumulativeDensity(double x) const noexcept { return cumulativeDensity(x, _mean, _sd); }

double TNormalDistr::invCumulativeDensity(ZeroOneOpen p) const noexcept {
	// = qnorm in R
	// see book "numerical recipes" pp 321
	return _negmSQRT2SD * _invNormalComplementaryErrorFunction(2. * p) + _mean;
}

double TNormalDistr::sample() const { return instances::randomGenerator().getNormalRandom(_mean, _sd); }
}
