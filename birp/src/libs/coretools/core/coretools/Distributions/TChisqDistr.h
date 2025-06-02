#ifndef DISTRIBUTIONS_TCHISQDISTR_H_
#define DISTRIBUTIONS_TCHISQDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathConstants.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TChisqDistr {
private:
	uint32_t _k;
	double _kDiv2;
	double _kDiv2Minus;
	double _kDiv2Gamma;
	double _kDiv2GammaLog;
	double _kLogDens;

	void _precalculateTmpVars();

	static void checkArgs(Positive x, uint32_t k) {
		if (k == 0) { DEVERROR("k = 0 not allowed in Chisq distribution!"); }
		if (k == 1 && x == 0.) { DEVERROR("for k == 1, x must be > 0!"); }
	}

public:
	TChisqDistr(uint32_t k) { set(k); }

	TChisqDistr(std::string_view parameterString) { set(parameterString); }

	void set(uint32_t k);

	void set(std::string_view parameterString);

	static constexpr std::string_view name = "chiSquare";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support(uint32_t k) {
		if (k == 1)
			return std::make_pair(0, std::numeric_limits<double>::max());
		else
			return std::make_pair(std::numeric_limits<double>::denorm_min(), std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(Positive x, uint32_t k) {
		// calculates density of a chisq distribution
		checkArgs(x, k);

		const auto kDiv2 = static_cast<double>(k) / 2.;
		const auto tmp1  = 1. / (pow(2., kDiv2) * tgamma(kDiv2));
		const auto tmp2  = pow(x, kDiv2 - 1.) * exp(-x / 2.);
		return tmp1 * tmp2;
	}

	static double logDensity(Positive x, uint32_t k) {
		// calculates log density of a chisq distribution
		checkArgs(x, k);

		const auto kDiv2 = static_cast<double>(k) / 2.;
		return -kDiv2 * ln2 - gammaLog(kDiv2) + (kDiv2 - 1.) * log(x) - x / 2.;
	}

	static Probability cumulativeDensity(Positive x, uint32_t k) {
		return P(TIncompleteGamma::lower((double)k / 2, x / 2));
	}

	static uint32_t mean(uint32_t k) { return k; }

	static Positive sample(uint32_t k) { return instances::randomGenerator().getChisqRand(k); }

	// member functions
	[[nodiscard]] Positive density(Positive x) const;
	[[nodiscard]] double logDensity(Positive x) const;
	[[nodiscard]] Probability cumulativeDensity(Positive x) const;
	[[nodiscard]] uint32_t mean() const;
	[[nodiscard]] Positive sample() const;
	[[nodiscard]] std::pair<double, double> support() const;

	std::string functionString() const { return str::toString(name, "(", _k, ")"); }

	std::string verbalizedString() const { return str::toString("chi-square distribution with k = ", _k); }
};

} // namespace coretools::probdist
#endif
