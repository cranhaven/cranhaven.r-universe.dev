#ifndef DISTRIBUTIONS_TPOISSONDISTR_H_
#define DISTRIBUTIONS_TPOISSONDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TPoissonDistr {
private:
	StrictlyPositive _lambda;
	double _logLambda;
	double _expLambda;

	void _precalculateTmpVars();

public:
	TPoissonDistr(StrictlyPositive Lambda) { set(Lambda); }

	TPoissonDistr(std::string_view parameterString) { set(parameterString); }

	void set(StrictlyPositive Lambda);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "poisson";
	static constexpr bool isDiscrete() { return true; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(0, std::numeric_limits<double>::max());
	}

	// static function for external use
	static Probability density(size_t k, StrictlyPositive lambda) noexcept {
		// calculates density of a Poisson distribution
		return P(std::pow(lambda, (double)k) * exp(-lambda) / TFactorial::factorial(k));
	}

	static LogProbability logDensity(size_t k, StrictlyPositive lambda) noexcept {
		// calculates log density of a Poisson distribution
		return logP((double)k * log(lambda) - lambda - TFactorial::factorialLog(k));
	};

	static Probability cumulativeDensity(double k, StrictlyPositive lambda) noexcept {
		// Note: accepts k as a double, but will take floor to have next-smaller integer
		// as the CDF is discontinuous at the integers of k and flat everywhere else
		return P(TIncompleteGamma::upper(std::floor(k + 1), lambda));
	};

	static constexpr StrictlyPositive mean(StrictlyPositive lambda) noexcept { return lambda; };

	static size_t sample(StrictlyPositive lambda) { return instances::randomGenerator().getPoissonRandom(lambda); }

	// functions using member parameters
	[[nodiscard]] Probability density(size_t k) const noexcept;
	[[nodiscard]] LogProbability logDensity(size_t k) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(double k) const noexcept;
	[[nodiscard]] StrictlyPositive mean() const noexcept;
	[[nodiscard]] size_t sample() const noexcept;

	std::string functionString() const { return str::toString(name, "(", _lambda, ")"); }

	std::string verbalizedString() const { return "poisson distribution with lambda = " + str::toString(_lambda); }
};
}
#endif
