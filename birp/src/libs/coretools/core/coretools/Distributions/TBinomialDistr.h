#ifndef DISTRIBUTIONS_TBINOMIALDISTR_H_
#define DISTRIBUTIONS_TBINOMIALDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TBinomialDistr {
private:
	size_t _trials;
	Probability _prob;
	LogProbability _logProb;
	Probability _ProbComplement;
	LogProbability _logProbComplement;
	Positive _mean;

	// values for invCumulativeDensity
	double _q_invCD     = 0.0;
	double _mu_invCD    = 0.0;
	double _sigma_invCD = 0.0;
	double _gamma_invCD = 0.0;

	void _precalculateTmpVars();

public:
	TBinomialDistr(size_t n, Probability p) { set(n, p); }

	TBinomialDistr(std::string_view parameterString) { set(parameterString); }

	void set(size_t n, Probability p);
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
		DEV_ASSERT(k <= n);
		return P(choose(n, k) * pow(p, (double)k) * pow(1. - p, (double)n - (double)k));
	}

	static LogProbability logDensity(size_t n, size_t k, Probability p) {
		// calculates log density of a binomial distribution
		DEV_ASSERT(k <= n);
		return logP(chooseLog(n, k) + (double)k * log(p) + ((double)n - (double)k) * log(1. - p));
	}

	// as in https://en.wikipedia.org/wiki/Beta_function#Incomplete_beta_function
	static Probability cumulativeDensity(size_t n, size_t k, Probability p) {
		DEV_ASSERT(k <= n);

		if (n == k) { return P(1.0); }
		return P(1 - TIncompleteBeta::incompleteBeta(k + 1, n - k, p));
	}

	static size_t invCumulativeDensity(ZeroOneOpen p, size_t n, Probability prob) noexcept;

	static Positive mean(size_t n, Probability p) { return (double)n * p; }
	static size_t sample(size_t n, Probability p) { return instances::randomGenerator().getBinomialRand(p, n); }

	// member functions
	[[nodiscard]] Probability density(size_t k) const;
	[[nodiscard]] LogProbability logDensity(size_t k) const;
	[[nodiscard]] Probability cumulativeDensity(size_t k) const;
	[[nodiscard]] size_t invCumulativeDensity(ZeroOneOpen p) const;
	[[nodiscard]] Positive mean() const;
	[[nodiscard]] size_t sample() const;

	size_t invCumulativeDensity(ZeroOneOpen p, size_t n, Probability prob, double q, double mu, double sigma,
								double gamma) const;

	std::string functionString() const { return str::toString(name, "(", _trials, _prob, ")"); }

	std::string verbalizedString() const {
		return str::toString("binomial distribution with n = ", _trials, " and p = ", _prob);
	}
};

} // namespace coretools::probdist

#endif
