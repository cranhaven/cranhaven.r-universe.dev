#ifndef DISTRIBUTIONS_TCHIDISTR_H_
#define DISTRIBUTIONS_TCHIDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathConstants.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TChiDistr {
private:
	uint32_t _k;
	double _kDiv2;
	double _kDiv2Minus;
	double _kDiv2Gamma;
	double _kMinus;
	double _kDiv2GammaLog;
	double _kLogDens;
	Positive _mean;

	void _precalculateTmpVars();

public:
	TChiDistr(uint32_t k) { set(k); }
	TChiDistr(std::string_view parameterString) { set(parameterString); }

	void set(uint32_t k);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "chi";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(0., std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(Positive x, uint32_t k) {
		DEV_ASSERT(k != 0);
		// calculates density of a chi distribution
		const auto kDiv2 = (double)k / 2.;
		const auto tmp1  = 1. / (pow(2., kDiv2 - 1.) * tgamma(kDiv2));
		const auto tmp2  = pow(x, (double)k - 1.) * exp(-(x * x) / 2.);
		return tmp1 * tmp2;
	}

	static double logDensity(Positive x, uint32_t k) {
		DEV_ASSERT(k != 0);
		// calculates log density of a chi distribution
		const auto kDiv2 = (double)k / 2.;
		return -(kDiv2 - 1.) * ln2 - gammaLog(kDiv2) + ((double)k - 1.) * log(x) - (x * x) / 2.;
	}

	static Probability cumulativeDensity(Positive x, uint32_t k) {
		return P(TIncompleteGamma::lower((double)k / 2, (x * x) / 2));
	}

	static Positive mean(uint32_t k) { return sqrt_2 * (tgamma(((double)k + 1.) / 2.) / tgamma((double)k / 2.)); }

	static Positive sample(uint32_t k) { return sqrt(instances::randomGenerator().getChisqRand(k)); }

	// member functions
	[[nodiscard]] Positive density(Positive x) const;
	[[nodiscard]] double logDensity(Positive x) const;
	[[nodiscard]] Probability cumulativeDensity(Positive x) const;
	[[nodiscard]] Positive mean() const;
	[[nodiscard]] Positive sample() const;

	std::string functionString() const { return str::toString(name, "(", _k, ")"); }

	std::string verbalizedString() const { return str::toString("chi distribution with k = ", _k); }
};
} // namespace coretools::probdist
#endif
