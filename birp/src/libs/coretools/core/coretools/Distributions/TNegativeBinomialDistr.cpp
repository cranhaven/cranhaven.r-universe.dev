#include "TNegativeBinomialDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {

void TNegativeBinomialDistr::_precalculateTmpVars() {
	_logProb           = logP(_prob);
	_logProbComplement = logP(_prob.complement());
	_gammaN            = tgamma(_n);
	_powProbN          = pow(_prob, (double)_n);
	_gammaLogN         = gammaLog(_n);
	_logProbN          = _logProb * (double)_n;
	_mean              = _n * _prob.oddsRatio();
}

void TNegativeBinomialDistr::set(Positive n, Probability p) {
	_prob = p;
	_n    = n;
	_precalculateTmpVars();
}

void TNegativeBinomialDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(n,p) with n > 0 and 0 <= p <= 1.", _n,
	                   _prob);
	_precalculateTmpVars();
}

Probability TNegativeBinomialDistr::density(size_t x) const noexcept {
	// calculates density of a negative binomial distribution (same as in R)
	// Note: n is typically an integer, but does not need to be; useful if NB is modelled as conjugate Poisson-Gamma
	if (_n == 0.0 && x == 0.0) { return P(1.0); }
	return P(tgamma((double)x + _n) / (_gammaN * TFactorial::factorial(x)) * pow(_prob.complement(), (double)x) *
			 _powProbN);
}

LogProbability TNegativeBinomialDistr::logDensity(size_t x) const noexcept {
	// calculates log density of a negative binomial distribution (same as in R)
	// Note: n is typically an integer, but does not need to be; useful if NB is modelled as conjugate Poisson-Gamma
	if (_n == 0.0 && x == 0.0) { return logP(0.0); }
	return logP(gammaLog((double)x + _n) - _gammaLogN - TFactorial::factorialLog(x) + ((double)x) * _logProbComplement +
				_logProbN);
}

Probability TNegativeBinomialDistr::cumulativeDensity(size_t x) const { return cumulativeDensity(_n, x, _prob); }

size_t TNegativeBinomialDistr::mean() const noexcept { return _mean; }

size_t TNegativeBinomialDistr::sample() const { return sample(_n, _prob); }
}
