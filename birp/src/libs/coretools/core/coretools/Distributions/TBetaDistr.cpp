#include "TBetaDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
TBetaDistr::TBetaDistr() {
	_alpha = 1.0;
	_beta  = 1.0;
	_precalculateTmpVars();
};

void TBetaDistr::_precalculateTmpVars() {
	_tmp1Density    = tgamma(_alpha + _beta) / (tgamma(_alpha) * tgamma(_beta));
	_tmp1LogDensity = gammaLog(_alpha + _beta) - gammaLog(_alpha) - gammaLog(_beta);
	_alphaMinusOne  = _alpha - 1.;
	_betaMinusOne   = _beta - 1.;
	_mean           = mean(_alpha, _beta);
	_var            = var(_alpha, _beta);
}

void TBetaDistr::set(StrictlyPositive alpha, StrictlyPositive beta) {
	_alpha = alpha;
	_beta  = beta;
	_precalculateTmpVars();
}

void TBetaDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(alpha, beta) with alpha,beta>0.", _alpha,
	                   _beta);
	_precalculateTmpVars();
}

Positive TBetaDistr::density(Probability x) const noexcept {
	// calculates density of a beta distribution
	const auto tmp2 = pow(x, _alphaMinusOne) * pow(1. - x, _betaMinusOne);
	return _tmp1Density * tmp2;
}

double TBetaDistr::logDensity(Probability x) const noexcept {
	// calculates log density of a beta distribution
	const auto tmp2 = _alphaMinusOne * log(x) + _betaMinusOne * log(1. - x);
	return _tmp1LogDensity + tmp2;
}

double TBetaDistr::cumulativeDensity(Probability x) const noexcept {
	return cumulativeDensity(x, (double)_alpha, (double)_beta);
}

Probability TBetaDistr::sample() const { return instances::randomGenerator().getBetaRandom(_alpha, _beta); }

Probability TBetaDistr::mean() const { return _mean; }

StrictlyPositive TBetaDistr::var() const { return _var; }

}
