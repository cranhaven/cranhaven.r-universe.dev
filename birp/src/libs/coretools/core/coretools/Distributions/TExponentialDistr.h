#ifndef DISTRIBUTIONS_TEXPONENTIALDISTR_H_
#define DISTRIBUTIONS_TEXPONENTIALDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {
	
class TExponentialDistr {
private:
	StrictlyPositive _lambda;
	double _logLambda;
	StrictlyPositive _mean;

	void _precalculateTmpVars();

public:
	TExponentialDistr(StrictlyPositive Lambda) { set(Lambda); }

	TExponentialDistr(std::string_view parameterString) { set(parameterString); }

	void set(StrictlyPositive Lambda);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "exponential";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(0, std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(Positive x, StrictlyPositive lambda) noexcept {
		// calculates density of a exponential distribution
		return lambda * exp(-lambda * x);
	}

	static double logDensity(Positive x, StrictlyPositive lambda) noexcept {
		// calculates log density of a exponential distribution
		return log(lambda) - lambda * x;
	}

	static Probability cumulativeDensity(Positive x, StrictlyPositive lambda) noexcept {
		return P(1. - exp(-lambda * x));
	}

	static constexpr StrictlyPositive mean(StrictlyPositive lambda) noexcept { return 1. / lambda; }

	static Positive sample(StrictlyPositive lambda) {
		return instances::randomGenerator().getExponentialRandom(lambda);
	}

	// functions using member parameters
	[[nodiscard]] Positive density(Positive x) const noexcept;
	[[nodiscard]] double logDensity(Positive x) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(Positive x) const noexcept;
	[[nodiscard]] StrictlyPositive mean() const noexcept;
	[[nodiscard]] Positive sample() const noexcept;

	std::string functionString() const {
		return std::string{name}.append(1, '(').append(str::toString(_lambda)).append(1, ')');
	}

	std::string verbalizedString() const { return "exponential distribution with lambda = " + str::toString(_lambda); }
};
}

#endif
