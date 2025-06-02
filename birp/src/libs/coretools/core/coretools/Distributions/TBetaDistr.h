#ifndef DISTRIBUTIONS_TBETADISTR_H_
#define DISTRIBUTIONS_TBETADISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TBetaDistr {
private:
	StrictlyPositive _alpha;
	StrictlyPositive _beta;
	double _tmp1Density;
	double _tmp1LogDensity;
	double _alphaMinusOne;
	double _betaMinusOne;
	Probability _mean;
	StrictlyPositive _var;

	void _precalculateTmpVars();

public:
	TBetaDistr();
	TBetaDistr(StrictlyPositive alpha, StrictlyPositive beta) : _alpha(alpha), _beta(beta) { _precalculateTmpVars(); }
	TBetaDistr(std::string_view parameterString) { set(parameterString); }

	void set(StrictlyPositive alpha, StrictlyPositive beta);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "beta";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() { return std::make_pair(0., 1.); }

	// static function for external use
	static Positive density(Probability x, StrictlyPositive alpha, StrictlyPositive beta) noexcept {
		// calculates density of a beta distribution
		const auto tmp1 = tgamma(alpha + beta) / (tgamma(alpha) * tgamma(beta));
		const auto tmp2 = pow(x, alpha - 1.) * pow(1. - x, beta - 1.);
		return tmp1 * tmp2;
	}

	static double logDensity(Probability x, StrictlyPositive alpha, StrictlyPositive beta) noexcept {
		// calculates log density of a beta distribution
		const auto tmp1 = gammaLog(alpha + beta) - gammaLog(alpha) - gammaLog(beta);
		const auto tmp2 = (alpha - 1.) * log(x) + (beta - 1.) * log(1. - x);
		return tmp1 + tmp2;
	}

	static double cumulativeDensity(Probability x, StrictlyPositive alpha, StrictlyPositive beta) noexcept {
		return TIncompleteBeta::incompleteBeta(alpha, beta, x);
	}

	static Probability sample(StrictlyPositive alpha, StrictlyPositive beta) {
		return instances::randomGenerator().getBetaRandom(alpha, beta);
	}

	static Probability mean(StrictlyPositive alpha, StrictlyPositive beta) {
		return (Probability)(alpha / (alpha + beta));
	}

	static StrictlyPositive var(StrictlyPositive alpha, StrictlyPositive beta) {
		const double aPlusB = alpha + beta;
		return (StrictlyPositive)(alpha * beta) / (aPlusB * aPlusB * (aPlusB + 1));
	}

	static std::pair<StrictlyPositive, StrictlyPositive> calculateAlphaBetaForGivenMeanVar(Probability Mean,
																						   StrictlyPositive Var) {
		// specify mean and variance of Beta distribution -> calculate alpha and beta that yield this mean and variance
		if (Var >= (Mean * (1. - Mean))) { // check if mean and var fulfil condition
			DEVERROR("Argument Var (", Var, ") must be smaller than '", Mean, "' * (1 - '", Mean, "')!");
		}
		double nu = Mean * (1. - Mean) / Var - 1.;
		return {Mean * nu, (1. - Mean) * nu};
	}

	static StrictlyPositive calculateSymmetricAlphaForGivenMeanVar(StrictlyPositive Var) {
		// mean of symmetric Beta must be 0.5
		// var = 1/(4(2*alpha + 1)) -> solve for alpha
		double MOM = 1. / (8. * Var) - 0.5;
		if (MOM <= 0. || std::isinf(MOM)) {
			// invalid nu -> re-set such that alpha = beta = 0.5 result
			MOM = 1.0;
		}
		return MOM;
	}

	static std::pair<StrictlyPositive, StrictlyPositive>
	calculateAlphaBetaForGivenModeConcentration(Probability Omega, StrictlyPositive Kappa) {
		// specify mode and concentration of Beta distribution -> calculate alpha and beta that yield this mode and
		// concentration
		// TODO: constraint on Kappa?
		double alpha = Omega * (Kappa - 2.) + 1.;
		double beta  = (1. - Omega) * (Kappa - 2.) + 1.;
		return {alpha, beta};
	}

	// member functions
	Positive density(Probability x) const noexcept;
	double logDensity(Probability x) const noexcept;
	double cumulativeDensity(Probability x) const noexcept;
	Probability sample() const;
	Probability mean() const;
	StrictlyPositive var() const;

	std::string functionString() const { return str::toString(name, "(", _alpha, _beta, ")"); }

	std::string verbalizedString() const {
		return str::toString("beta distribution with alpha = ", _alpha, " and beta = ", _beta);
	}
};
} // namespace coretools::probdist
#endif
