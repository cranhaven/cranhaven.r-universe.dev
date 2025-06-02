#include "TChiDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {

void TChiDistr::_precalculateTmpVars() {
	_kDiv2         = (double)_k / 2;
	_kDiv2Minus    = _kDiv2 - 1.;
	_kDiv2Gamma    = tgamma(_kDiv2);
	_kMinus        = (double)_k - 1.;
	_kDiv2GammaLog = gammaLog(_kDiv2);
	_kLogDens      = -_kDiv2Minus * M_LN2 - _kDiv2GammaLog;
	_mean          = M_SQRT2 * (tgamma(((double)_k + 1) / 2) / tgamma((double)_k / 2));
}

void TChiDistr::set(uint32_t k) {
	_k = k;
	_precalculateTmpVars();
}

void TChiDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(k) with k>0.", _k);
	_precalculateTmpVars();
};

Positive TChiDistr::density(Positive x) const {
	if (_k == 0) { DEVERROR("k = 0 not allowed in Chi distribution!"); }
	// calculates density of a chi distribution
	const auto tmp1 = 1. / (pow(2., _kDiv2Minus) * _kDiv2Gamma);
	const auto tmp2 = pow(x, _kMinus) * exp(-(x * x) / 2.);
	return tmp1 * tmp2;
}

double TChiDistr::logDensity(Positive x) const {
	if (_k == 0) { DEVERROR("k = 0 not allowed in Chi distribution!"); }
	// calculates log density of a chi distribution
	return _kLogDens + _kMinus * log(x) - (x * x) / 2.;
}

Probability TChiDistr::cumulativeDensity(Positive x) const { return cumulativeDensity(x, _k); }

Positive TChiDistr::mean() const { return _mean; }

Positive TChiDistr::sample() const { return sample(_k); }
}
