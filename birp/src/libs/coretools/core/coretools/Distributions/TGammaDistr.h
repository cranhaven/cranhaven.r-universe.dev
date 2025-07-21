#ifndef DISTRIBUTIONS_TGAMMADISTR_H_
#define DISTRIBUTIONS_TGAMMADISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {
class TGammaDistr {
private:
	StrictlyPositive _alpha;
	StrictlyPositive _beta;
	Positive _dens;
	double _logDens;
	double _alphaMinusOne;
	StrictlyPositive _mean;

	void _precalculateTmpVars();

public:
	TGammaDistr(StrictlyPositive alpha, StrictlyPositive beta) { set(alpha, beta); }
	TGammaDistr(std::string_view parameterString) { set(parameterString); }

	void set(StrictlyPositive alpha, StrictlyPositive beta);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "gamma";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(std::numeric_limits<double>::denorm_min(), std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(StrictlyPositive x, StrictlyPositive alpha, StrictlyPositive beta) noexcept {
		// calculates density of a Gamma distribution
		return pow(beta, alpha) / tgamma(alpha) * pow(x, alpha - 1.) * exp(-beta * x);
	};
	static double logDensity(StrictlyPositive x, StrictlyPositive alpha, StrictlyPositive beta) noexcept {
		// calculates log density of a Gamma distribution
		return alpha * log(beta) - gammaLog(alpha) + (alpha - 1.) * log(x) - beta * x;
	};
	static Probability cumulativeDensity(StrictlyPositive x, StrictlyPositive alpha,
	                                            StrictlyPositive beta) noexcept {
		// Adapted from kfunc.c of samtools
		return P(TIncompleteGamma::lower(
					 alpha, beta * x)); // do not need to divide by exp(gammaln(alpha)), it is already regularized;
	};

	static StrictlyPositive mean(StrictlyPositive alpha, StrictlyPositive beta) noexcept {
		return alpha / beta;
	};

	static StrictlyPositive sample(StrictlyPositive alpha, StrictlyPositive beta) {
		return instances::randomGenerator().getGammaRand(alpha, beta);
	}

	// member functions
	[[nodiscard]] Positive density(StrictlyPositive x) const noexcept;
	[[nodiscard]] double logDensity(StrictlyPositive x) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(StrictlyPositive x) const noexcept;
	[[nodiscard]] StrictlyPositive mean() const noexcept;
	[[nodiscard]] double sample() const;

	std::string functionString() const { return str::toString(name, "(", _alpha, _beta, ")"); }

	std::string verbalizedString() const {
		return str::toString("gamma distribution with alpha = ", _alpha, " and beta = ", _beta);
	}
};
} // namespace coretools::probdist
#endif
