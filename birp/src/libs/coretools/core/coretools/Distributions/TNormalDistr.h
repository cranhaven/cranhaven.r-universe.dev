#ifndef DISTRIBUTIONS_TNORMALDISTR_H_
#define DISTRIBUTIONS_TNORMALDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathConstants.h"
#include "coretools/Types/commonWeakTypes.h"
#include <array>

namespace coretools::probdist {

class TNormalDistr {
private:
	double _mean;
	StrictlyPositive _sd;
	double _densDenom;
	double _densExpo;
	double _var;
	double _logDens;
	double _twoVar;
	double _negmSQRT2SD;

	static double _normalComplementaryErrorCheb(double x) {
		// see book "numerical recipes"
		constexpr std::array<double, 28> coef = {-1.3026537197817094,
		                                         6.4196979235649026e-1,
		                                         1.9476473204185836e-2,
		                                         -9.561514786808631e-3,
		                                         -9.46595344482036e-4,
		                                         3.66839497852761e-4,
		                                         4.2523324806907e-5,
		                                         -2.0278578112534e-5,
		                                         -1.624290004647e-6,
		                                         1.303655835580e-6,
		                                         1.5626441722e-8,
		                                         -8.5238095915e-8,
		                                         6.529054439e-9,
		                                         5.059343495e-9,
		                                         -9.91364156e-10,
		                                         -2.27365122e-10,
		                                         9.6467911e-11,
		                                         2.394038e-12,
		                                         -6.886027e-12,
		                                         8.94487e-13,
		                                         3.13092e-13,
		                                         -1.12708e-13,
		                                         3.81e-16,
		                                         7.106e-15,
		                                         -1.523e-15,
		                                         -9.4e-17,
		                                         1.21e-16,
		                                         -2.8e-17};
		int j;
		double t, ty, tmp, d = 0., dd = 0.;
		t  = 2. / (2. + x);
		ty = 4. * t - 2;
		for (j = 27; j > 0; --j) {
			tmp = d;
			d   = ty * d - dd + coef[j];
			dd  = tmp;
		}
		return t * exp(-x * x + 0.5 * (coef[0] + ty * d) - dd);
	}

	static double _normalComplementaryErrorFunction(double x) {
		// see book "numerical recipes"
		if (x >= 0.)
			return _normalComplementaryErrorCheb(x);
		else
			return 2.0 - _normalComplementaryErrorCheb(-x);
	}

	static double _invNormalComplementaryErrorFunction(double p) {
		// see book "numerical recipes"
		double x, err, t, pp;
		if (p >= 2.0) return -100.;
		if (p <= 0.0) return 100.;
		pp = (p < 1.0) ? p : 2. - p;
		t  = sqrt(-2. * log(pp / 2.));
		x  = -0.70711 * ((2.30753 + t * 0.27061) / (1. + t * (0.99229 + t * 0.04481)) - t);
		for (int j = 0; j < 2; j++) {
			err = erfc(x) - pp;
			x += err / (two_sqrt_pi * exp(-(x * x)) - x * err);
		}
		return (p < 1.0 ? x : -x);
	}

	void _precalculateTmpVars();

public:
	TNormalDistr(double mean, StrictlyPositive sd) { set(mean, sd); }
	TNormalDistr(std::string_view parameterString) { set(parameterString); }

	void set(double mean, StrictlyPositive sd);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "normal";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(double x, double mean, StrictlyPositive sd) noexcept {
		// calculates density of a normal distribution (= dnorm in R)
		return 1. / (sqrt_two_pi * sd) * exp(-1. / (2. * sd * sd) * (x - mean) * (x - mean));
	}

	static double logDensity(double x, double mean, StrictlyPositive sd) noexcept {
		// calculates density of a normal distribution (= dnorm in R)
		const auto variance = sd * sd;
		return -0.5 * log(two_pi * variance) - 1. / (2. * variance) * (x - mean) * (x - mean);
	}

	static Probability cumulativeDensity(double x, double mean, StrictlyPositive sd) noexcept {
		// = pnorm in R
		if (x == mean) return P(0.5);
		return P(0.5 * _normalComplementaryErrorFunction(-sqrt1_2 * (x - mean) / sd));
	}

	static double invCumulativeDensity(ZeroOneOpen p, double mean, StrictlyPositive sd) noexcept {
		// = qnorm in R
		// see book "numerical recipes" pp 321
		return -sqrt_2 * sd * _invNormalComplementaryErrorFunction(2. * p) + mean;
	}

	static double mean(double mean) { return mean; }

	static double sample(double mean, StrictlyPositive sd) {
		return instances::randomGenerator().getNormalRandom(mean, sd);
	}

	// member functions
	[[nodiscard]] Positive density(double x) const noexcept;
	[[nodiscard]] double logDensity(double x) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(double x) const noexcept;
	[[nodiscard]] double invCumulativeDensity(ZeroOneOpen p) const noexcept;
	[[nodiscard]] double mean() const { return mean(_mean); }
	[[nodiscard]] double sample() const;

	std::string functionString() const { return str::toString(name, "(", _mean, _sd, ")"); }

	std::string verbalizedString() const {
		return str::toString("normal distribution with mean = ", _mean, " and sd = ", _sd);
	}
};
} // namespace coretools::probdist
#endif
