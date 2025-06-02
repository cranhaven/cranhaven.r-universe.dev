#include "TGammaDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TGammaDistr::_precalculateTmpVars() {
	_dens          = pow(_beta, _alpha) / tgamma(_alpha);
	_logDens       = _alpha * log(_beta) - gammaLog(_alpha);
	_alphaMinusOne = _alpha - 1.;
	_mean          = _alpha / _beta;
}

void TGammaDistr::set(StrictlyPositive alpha, StrictlyPositive beta) {
	_alpha = alpha;
	_beta  = beta;
	_precalculateTmpVars();
}

void TGammaDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(alpha, beta) with alpha,beta>0.", _alpha,
	                   _beta);
	_precalculateTmpVars();
}

Positive TGammaDistr::density(StrictlyPositive x) const noexcept {
	// calculates density of a Gamma distribution
	return _dens * pow(x, _alphaMinusOne) * exp(-_beta * x);
};

double TGammaDistr::logDensity(StrictlyPositive x) const noexcept {
	// calculates log density of a Gamma distribution
	return _logDens + _alphaMinusOne * log(x) - _beta * x;
};

Probability TGammaDistr::cumulativeDensity(StrictlyPositive x) const noexcept {
	return cumulativeDensity(x, _alpha, _beta);
};

StrictlyPositive TGammaDistr::mean() const noexcept { return _mean; }

double TGammaDistr::sample() const { return instances::randomGenerator().getGammaRand(_alpha, _beta); }

}
