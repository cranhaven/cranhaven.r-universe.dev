#ifndef DISTRIBUTIONS_TUNIFORMDISTR_H_
#define DISTRIBUTIONS_TUNIFORMDISTR_H_

#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Math/mathFunctions.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TUniformDistr {
private:
	double _min = std::numeric_limits<double>::lowest();
	double _max = std::numeric_limits<double>::max();

	double _dens    = 0.0;
	double _logDens = 0.0;

	static Positive _densityInRange(double Min, double Max) noexcept { return 1.0 / (Max - Min); }
	static double _logDensityInRange(double Min, double Max) noexcept { return -log(Max - Min); }
	static Probability _cdf(double x, double Min, double Max) noexcept { return P((x - Min) / (Max - Min)); }

public:
	TUniformDistr(const TUniformDistr &)            = default;
	TUniformDistr(TUniformDistr &&)                 = default;
	TUniformDistr &operator=(const TUniformDistr &) = default;
	TUniformDistr &operator=(TUniformDistr &&)      = default;
	TUniformDistr(double Min, double Max) { set(Min, Max); }
	TUniformDistr(std::string_view parameterString) { set(parameterString); }

	void set(double Min, double Max);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "uniform";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support() {
		return std::make_pair(std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max());
	}

	// static function for external use
	static Positive density(double x, double Min, double Max) noexcept {
		// calculates density of a uniform distribution
		if (x < Min || x > Max) { return 0.0; }
		if (!coretools::checkForNumericOverflow_subtraction(Min, Max)) { return std::numeric_limits<double>::min(); }
		return _densityInRange(Min, Max);
	}

	static double logDensity(double x, double Min, double Max) noexcept {
		// calculates log density of a uniform distribution
		if (x < Min || x > Max) { return std::numeric_limits<double>::lowest(); } // prevent log(0) = -inf
		if (!coretools::checkForNumericOverflow_subtraction(Min, Max)) {
			return log(std::numeric_limits<double>::min());
		}
		return _logDensityInRange(Min, Max);
	}

	static Probability cumulativeDensity(double x, double Min, double Max) noexcept {
		if (x < Min) {
			return P(0.0);
		} else if (x > Max) {
			return P(1.0);
		} else if (coretools::checkForNumericOverflow_subtraction(Min, Max)) {
			return _cdf(x, Min, Max);
		}
		// interval too large -> divide by two in order to calculate CDF
		Min /= 2.0;
		Max /= 2.0;
		return _cdf(x, Min, Max);
	}

	static Positive sample(double Min, double Max) { return instances::randomGenerator().getRand(Min, Max); }

	// functions using member parameters
	[[nodiscard]] Positive density(double x) const noexcept;
	[[nodiscard]] double logDensity(double x) const noexcept;
	[[nodiscard]] Probability cumulativeDensity(double x) const noexcept;
	[[nodiscard]] double sample() const noexcept;

	std::string functionString() const { return str::toString(name, "(" , _min, _max, ")"); }

	std::string verbalizedString() const {
		return str::toString("uniform distribution with min = ", _min, " and max = ", _max);
	}
};

}
#endif
