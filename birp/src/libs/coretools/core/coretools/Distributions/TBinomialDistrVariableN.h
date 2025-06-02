#ifndef DISTRIBUTIONS_TBINOMIALDISTRVARIABLEN_H_
#define DISTRIBUTIONS_TBINOMIALDISTRVARIABLEN_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {
class TBinomialDistr; // forward declaration

class TBinomialDistrVariableN {
private:
	Probability _prob;
	LogProbability _logProb;
	Probability _ProbComplement;
	LogProbability _logProbComplement;

	void _precalculateTmpVars();

public:
	TBinomialDistrVariableN(Probability p) { set(p); }

	TBinomialDistrVariableN(std::string_view parameterString) { set(parameterString); }

	void set(Probability p);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "binomial";
	static constexpr bool isDiscrete() { return true; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<size_t, size_t> support() {
		return std::make_pair(0, std::numeric_limits<size_t>::max());
	}

	// static function for external use
	static Probability density(size_t n, size_t k, Probability p) {
		// calculates density of a binomial distribution
		if (k > n) { DEVERROR("n > k in binomial distribution (with n = ", n, ", k = ", k, ")!"); }
		return P(choose(n, k) * pow(p, (double)k) * pow(1. - p, (double)n - (double)k));
	}

	static LogProbability logDensity(size_t n, size_t k, Probability p) {
		// calculates log density of a binomial distribution
		if (k > n) { DEVERROR("n > k in binomial distribution (with n = ", n, ", k = ", k, ")!"); }
		return logP(chooseLog(n, k) + (double)k * log(p) + ((double)n - (double)k) * log(1. - p));
	}

	// as in https://en.wikipedia.org/wiki/Beta_function#Incomplete_beta_function
	static Probability cumulativeDensity(size_t n, size_t k, Probability p) {
		if (n == k) { return P(1.0); }
		if (k > n) { DEVERROR("n > k in binomial distribution (with n = ", n, ", k = ", k, ")!"); }
		return P(1 - TIncompleteBeta::incompleteBeta(k + 1, n - k, p));
	}

	static size_t invCumulativeDensity(ZeroOneOpen p, size_t n, Probability prob) noexcept;

	static Positive mean(size_t n, Probability p) { return (double)n * p; }
	static size_t sample(size_t n, Probability p) { return instances::randomGenerator().getBinomialRand(p, n); }

	// member functions
	[[nodiscard]] Probability density(size_t n, size_t k) const;
	[[nodiscard]] LogProbability logDensity(size_t n, size_t k) const;
	[[nodiscard]] Probability cumulativeDensity(uint32_t n, uint32_t k) const;
	[[nodiscard]] size_t invCumulativeDensity(ZeroOneOpen p, size_t n) const;
	[[nodiscard]] Positive mean(size_t n) const;
	[[nodiscard]] size_t sample(size_t n) const;
};
	
}
#endif
