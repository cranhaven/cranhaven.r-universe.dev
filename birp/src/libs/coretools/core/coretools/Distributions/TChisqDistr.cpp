#include "TChisqDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TChisqDistr::_precalculateTmpVars() {
	_kDiv2         = (double)_k / 2;
	_kDiv2Minus    = _kDiv2 - 1.;
	_kDiv2Gamma    = tgamma(_kDiv2);
	_kDiv2GammaLog = gammaLog(_kDiv2);
	_kLogDens      = -_kDiv2 * M_LN2 - _kDiv2GammaLog;
}

void TChisqDistr::set(uint32_t k) {
	_k = k;
	_precalculateTmpVars();
}

void TChisqDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(k) with k>0.", _k);
	_precalculateTmpVars();
}

Positive TChisqDistr::density(Positive x) const {
	// calculates density of a chisq distribution
	checkArgs(x, _k);

	const auto tmp1 = 1. / (pow(2., _kDiv2) * _kDiv2Gamma);
	const auto tmp2 = pow(x, _kDiv2Minus) * exp(-x / 2.);
	return tmp1 * tmp2;
}

double TChisqDistr::logDensity(Positive x) const {
	// calculates log density of a chisq distribution
	checkArgs(x, _k);

	return _kLogDens + _kDiv2Minus * log(x) - x / 2.;
}

Probability TChisqDistr::cumulativeDensity(Positive x) const { return cumulativeDensity(x, _k); }

uint32_t TChisqDistr::mean() const { return mean(_k); }

Positive TChisqDistr::sample() const { return instances::randomGenerator().getChisqRand(_k); }

std::pair<double, double> TChisqDistr::support() const {
	if (_k == 1)
		return std::make_pair(0, std::numeric_limits<double>::max());
	else
		return std::make_pair(std::numeric_limits<double>::denorm_min(), std::numeric_limits<double>::max());
}

} // namespace coretools::probdist
