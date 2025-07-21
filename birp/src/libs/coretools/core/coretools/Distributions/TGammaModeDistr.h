#ifndef DISTRIBUTIONS_TGAMMAMODEDISTR_H_
#define DISTRIBUTIONS_TGAMMAMODEDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TGammaModeDistr {
private:
	// if mode = 0, alpha could be any number between 0 and 1, which is why it has to be set to StrictlyPositive here
	// --> possibly look for workaround
	StrictlyPositive _mode;
	StrictlyPositive _var;
	double _alpha;
	double _beta;
	double _dens;
	double _logDens;
	double _alphaMinusOne;
	StrictlyPositive _mean;

	void _precalculateTmpVars();

public:
	TGammaModeDistr(StrictlyPositive mode, StrictlyPositive var) { set(mode, var); }
	TGammaModeDistr(std::string_view parameterString) { set(parameterString); }

	void set(StrictlyPositive mode, StrictlyPositive var);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "gammaMode";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(std::numeric_limits<double>::denorm_min(), std::numeric_limits<double>::max());
	}

	// static function for external use
	static void alphaBetaWarning(double alpha, double beta) {
		DEV_ASSERT(alpha > 0.);
		DEV_ASSERT(beta > 0.);
	}

	static Positive density(StrictlyPositive x, StrictlyPositive mode, StrictlyPositive var) noexcept {
		double beta  = (mode + sqrt(mode * mode + 4.0 * var)) / (2.0 * var);
		double alpha = mode * beta + 1.0;
		alphaBetaWarning(alpha, beta);
		// calculates density of a Gamma distribution
		return pow(beta, alpha) / tgamma(alpha) * pow(x, alpha - 1.) * exp(-beta * x);
	};
	static double logDensity(StrictlyPositive x, StrictlyPositive mode, StrictlyPositive var) noexcept {
		double beta  = (mode + sqrt(mode * mode + 4.0 * var)) / (2.0 * var);
		double alpha = mode * beta + 1.0;
		alphaBetaWarning(alpha, beta);
		// calculates log density of a Gamma distribution
		return alpha * log(beta) - gammaLog(alpha) + (alpha - 1.) * log(x) - beta * x;
	};
	static Probability cumulativeDensity(StrictlyPositive x, StrictlyPositive mode,
	                                            StrictlyPositive var) noexcept {
		double beta  = (mode + sqrt(mode * mode + 4.0 * var)) / (2.0 * var);
		double alpha = mode * beta + 1.0;
		alphaBetaWarning(alpha, beta);
		// Adapted from kfunc.c of samtools
		return P(TIncompleteGamma::lower(
					 alpha, beta * x)); // do not need to divide by exp(gammaln(alpha)), it is already regularized;
	};

	static StrictlyPositive mean(StrictlyPositive mode, StrictlyPositive var) noexcept {
		double beta  = (mode + sqrt(mode * mode + 4.0 * var)) / (2.0 * var);
		double alpha = mode * beta + 1.0;
		alphaBetaWarning(alpha, beta);
		return (StrictlyPositive)(alpha / beta);
	};

	static StrictlyPositive sample(StrictlyPositive mode, StrictlyPositive var) {
		double beta  = (mode + sqrt(mode * mode + 4.0 * var)) / (2.0 * var);
		double alpha = mode * beta + 1.0;
		alphaBetaWarning(alpha, beta);
		return instances::randomGenerator().getGammaRand(alpha, beta);
	}

	// member functions
	[[nodiscard]] Positive density(StrictlyPositive x) const noexcept;
	[[nodiscard]] double logDensity(StrictlyPositive x) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(StrictlyPositive x) const noexcept;
	[[nodiscard]] StrictlyPositive mean() const noexcept;
	[[nodiscard]] StrictlyPositive sample() const;

	std::string functionString() const { return str::toString(name, "(", _mode, _var, ")"); }

	std::string verbalizedString() const {
		return str::toString("gamma mode distribution with mode = ", _mode, " and variance = ", _var);
	}
};
} // namespace coretools::probdist
#endif
