#include "TGammaModeDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TGammaModeDistr::_precalculateTmpVars() {
	_beta  = (_mode + sqrt(_mode * _mode + 4.0 * _var)) / (2.0 * _var);
	_alpha = _mode * _beta + 1.0;
	alphaBetaWarning(_alpha, _beta);
	_dens          = pow(_beta, _alpha) / tgamma(_alpha);
	_logDens       = _alpha * log(_beta) - gammaLog(_alpha);
	_alphaMinusOne = _alpha - 1.;
	_mean          = _alpha / _beta;
}

void TGammaModeDistr::set(StrictlyPositive mode, StrictlyPositive var) {
	_mode = mode;
	_var  = var;
	_precalculateTmpVars();
}

void TGammaModeDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(alpha, beta) with alpha,beta>0.", _alpha,
	                   _beta);
	_precalculateTmpVars();
}

Positive TGammaModeDistr::density(StrictlyPositive x) const noexcept {
	// calculates density of a Gamma distribution
	return _dens * pow(x, _alphaMinusOne) * exp(-_beta * x);
};

double TGammaModeDistr::logDensity(StrictlyPositive x) const noexcept {
	// calculates log density of a Gamma distribution
	return _logDens + _alphaMinusOne * log(x) - _beta * x;
};

Probability TGammaModeDistr::cumulativeDensity(StrictlyPositive x) const noexcept {
	return cumulativeDensity(x, _mode, _var);
};

StrictlyPositive TGammaModeDistr::mean() const noexcept { return _mean; }

StrictlyPositive TGammaModeDistr::sample() const { return instances::randomGenerator().getGammaRand(_alpha, _beta); }
}
