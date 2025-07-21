#include "TNegativeBinomialDistrVariableN.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {

void TNegativeBinomialDistrVariableN::_precalculateTmpVars() {
	_logProb           = logP(_prob);
	_logProbComplement = logP(_prob.complement());
}

void TNegativeBinomialDistrVariableN::set(Probability p) {
	_prob = p;
	_precalculateTmpVars();
}

void TNegativeBinomialDistrVariableN::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(p) with 0 <= p <= 1.", _prob);
	_precalculateTmpVars();
}

Probability TNegativeBinomialDistrVariableN::density(Positive n, size_t x) const noexcept {
	// calculates density of a negative binomial distribution (same as in R)
	// Note: n is typically an integer, but does not need to be; useful if NB is modelled as conjugate Poisson-Gamma
	if (n == 0.0 && x == 0.0) { return P(1.0); }
	return P(tgamma((double)x + n) / (tgamma(n) * TFactorial::factorial(x)) * pow(_prob.complement(), (double)x) *
			 pow(_prob, (double)n));
}

LogProbability TNegativeBinomialDistrVariableN::logDensity(Positive n, size_t x) const noexcept {
	// calculates log density of a negative binomial distribution (same as in R)
	// Note: n is typically an integer, but does not need to be; useful if NB is modelled as conjugate Poisson-Gamma
	if (n == 0.0 && x == 0.0) { return logP(0.0); }
	return logP(gammaLog((double)x + n) - gammaLog(n) - TFactorial::factorialLog(x) + ((double)x) * _logProbComplement +
				(double)n * _logProb);
}

Probability TNegativeBinomialDistrVariableN::cumulativeDensity(Positive n, size_t x) const {
	return cumulativeDensity(n, x, _prob);
}

size_t TNegativeBinomialDistrVariableN::mean(Positive n) const noexcept { return n * _prob.oddsRatio(); }

size_t TNegativeBinomialDistrVariableN::sample(Positive n) const { return sample(n, _prob); }
}
