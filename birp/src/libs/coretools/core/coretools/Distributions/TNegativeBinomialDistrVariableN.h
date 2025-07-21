#ifndef DISTRIBUTIONS_TNEGATIVEBINOMIALDISTRVARIABLEN_H_
#define DISTRIBUTIONS_TNEGATIVEBINOMIALDISTRVARIABLEN_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TNegativeBinomialDistrVariableN {

private:
	Probability _prob;
	LogProbability _logProb;
	LogProbability _logProbComplement;

	void _precalculateTmpVars();

public:
	TNegativeBinomialDistrVariableN(Probability p) { set(p); }

	static constexpr std::string_view name = "negativeBinomialVariableN";
	static constexpr bool isDiscrete() { return true; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<size_t, size_t> support() {
		return std::make_pair(0, std::numeric_limits<size_t>::max());
	}

	void set(Probability p);
	void set(std::string_view parameterString);

	// static function for external use
	static Probability density(Positive n, size_t x, Probability p) noexcept {
		// calculates density of a negative binomial distribution (same as in R)
		// Note: n is typically an integer, but does not need to be; useful if NB is modelled as conjugate Poisson-Gamma
		if (n == 0.0 && x == 0.0) { return P(1.0); }
		return P(tgamma((double)x + n) / (tgamma(n) * TFactorial::factorial(x)) * pow(1. - p, (double)x) *
				 pow(p, (double)n));
	}
	static LogProbability logDensity(Positive n, size_t x, Probability p) noexcept {
		// calculates log density of a negative binomial distribution (same as in R)
		// Note: n is typically an integer, but does not need to be; useful if NB is modelled as conjugate Poisson-Gamma
		if (n == 0.0 && x == 0.0) { return logP(0.0); }
		return logP(gammaLog((double)x + n) - gammaLog(n) - TFactorial::factorialLog(x) + ((double)x) * log(1. - p) +
					(double)n * log(p));
	}

	// as in https://en.wikipedia.org/wiki/Negative_binomial_distribution#Cumulative_distribution_function
	static Probability cumulativeDensity(Positive n, size_t x, Probability p) {
		DEV_ASSERT(n > 0);
		return P(TIncompleteBeta::incompleteBeta((StrictlyPositive)n, x + 1, p));
	}

	static size_t mean(Positive n, Probability p) noexcept { return n * p.oddsRatio(); }

	static size_t sample(Positive n, Probability p) {
		return instances::randomGenerator().getNegativeBinomialRand(p, n);
	}

	// member function
	[[nodiscard]] Probability density(Positive n, size_t x) const noexcept;
	[[nodiscard]] LogProbability logDensity(Positive n, size_t x) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(Positive n, size_t x) const;
	[[nodiscard]] size_t mean(Positive n) const noexcept;
	[[nodiscard]] size_t sample(Positive n) const;
};

	
}
#endif
