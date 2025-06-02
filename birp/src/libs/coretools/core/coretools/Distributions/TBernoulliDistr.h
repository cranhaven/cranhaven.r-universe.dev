#ifndef DISTRIBUTIONS_TBERNOULIIDISTR_H_
#define DISTRIBUTIONS_TBERNOULIIDISTR_H_

#include "coretools/Main/TRandomGenerator.h"

namespace coretools::probdist {

class TBernoulliDistr {
private:
	Probability _pi;
	Probability _piComplement;
	LogProbability _logPi;
	LogProbability _logPiComplement;

	void _precalculateTmpVars();

public:
	TBernoulliDistr(Probability pi) { set(pi); }

	TBernoulliDistr(std::string_view parameterString) { set(parameterString); }

	void set(Probability pi);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "bernoulli";
	static constexpr bool isDiscrete() { return true; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<bool, bool> support() { return std::make_pair(0, 1); }

	// static function for external use
	static constexpr Probability density(bool x, Probability pi) noexcept {
		// calculates density of a bernoulli distribution
		return x ? pi : pi.complement();
	}

	static LogProbability logDensity(bool x, Probability pi) {
		// calculates log density of a bernoulli distribution
		if (x) {
			if (pi == 0.) { DEVERROR("pi is exactly 0., and log(pi) results in -Inf!"); }
			return logP(pi);
		} else {
			if (pi == 1.) { DEVERROR("pi is exactly 1., and log(1-pi) results in -Inf!"); }
			return logP(pi.complement());
		}
	}

	static Probability cumulativeDensity(bool x, Probability pi) noexcept {
		return x ? Probability(1.) : pi.complement();
	}

	static constexpr Probability mean(Probability pi) noexcept { return pi; };

	static bool sample(Probability pi) { return instances::randomGenerator().getBernoulliRand(pi); }

	// member functions
	[[nodiscard]] constexpr Probability density(bool x) const noexcept { return density(x, _pi); }
	[[nodiscard]] LogProbability logDensity(bool x) const;
	[[nodiscard]] Probability cumulativeDensity(bool x) const noexcept;
	[[nodiscard]] constexpr Probability mean() const noexcept { return mean(_pi); }
	[[nodiscard]] bool sample() const;
};
	
}
#endif
