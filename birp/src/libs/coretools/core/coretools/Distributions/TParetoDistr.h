#ifndef DISTRIBUTIONS_TPARETODISTR_H_
#define DISTRIBUTIONS_TPARETODISTR_H_

#include "coretools/Main/TError.h"
#include "coretools/Main/TRandomGenerator.h"
#include "coretools/Types/commonWeakTypes.h"

namespace coretools::probdist {

class TParetoDistr {
private:
	double _locationMu;
	StrictlyPositive _scaleSigma;
	double _shapeXi;
	Positive _OneDivSigma;
	double _negOneDivXiMinusOne;
	double _logSigma;
	double _xiSubtract;
	double _xiDenom;

	void _precalculateTmpVars();

	static void checkArgs(double x, double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		DEV_ASSERT(locationMu <= x);

		if (shapeXi < 0.0 && x > locationMu - scaleSigma / shapeXi) {
			throw TDevError("Problem calculating density for generalized pareto: x > mu - sigma/xi!");
		}
	}

public:
	TParetoDistr(double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		set(locationMu, scaleSigma, shapeXi);
	}
	TParetoDistr(std::string_view parameterString) { set(parameterString); }

	void set(double locationMu, StrictlyPositive scaleSigma, double shapeXi);
	void set(std::string_view parameterString);

	static constexpr std::string_view name = "pareto";
	static constexpr bool isDiscrete() { return false; };
	static constexpr bool isMultiVariate() { return false; };
	static constexpr std::pair<double, double> support(StrictlyPositive scaleSigma) {
		return std::make_pair(scaleSigma, std::numeric_limits<double>::max());
	}

	// static function for external use
	static double density(double x, double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		// calculates probability density of a generalized Pareto distribution

		// check for valid arguments
		checkArgs(x, locationMu, scaleSigma, shapeXi);

		return (1. / scaleSigma) * pow(1 + (shapeXi * (x - locationMu)) / scaleSigma, -(1 / shapeXi) - 1);
	}

	static double logDensity(double x, double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		// calculates log density of a generalized Pareto distribution

		// check for valid arguments
		checkArgs(x, locationMu, scaleSigma, shapeXi);

		return locationMu == 0.0
				   ? -log(scaleSigma) - (x - locationMu) / scaleSigma
				   : -log(scaleSigma) + (-1.0 - 1.0 / shapeXi) * log(1.0 + shapeXi * (x - locationMu) / scaleSigma);
	}

	static Probability cumulativeDensity(double x, double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		// check for valid arguments
		checkArgs(x, locationMu, scaleSigma, shapeXi);

		return shapeXi == 0.0 ? P(1.0 - exp((locationMu - x) / scaleSigma))
			: P(1.0 - pow(1.0 + shapeXi * (x - locationMu) / scaleSigma, -1.0 / shapeXi));
	}

	static double mean(double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		DEV_ASSERT(shapeXi < 1);
		return locationMu + (scaleSigma / (1 - shapeXi));
	}

	static double sample(double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
		return instances::randomGenerator().getGeneralizedParetoRand(locationMu, scaleSigma, shapeXi);
	}

	// member functions
	[[nodiscard]] double density(double x) const;
	[[nodiscard]] double logDensity(double x) const;
	[[nodiscard]] Probability cumulativeDensity(double x) const;
	[[nodiscard]] double mean() const;
	[[nodiscard]] double sample() const;
	[[nodiscard]] std::pair<double, double> support() const;

	std::string functionString() const { return str::toString(name, "(", _locationMu, _scaleSigma, _shapeXi, ")"); }

	std::string verbalizedString() const {
		return str::toString("generalized pareto distribution with mu = ", _locationMu, ", sigma = ", _scaleSigma,
							 " and xi = ", _shapeXi);
	}
};
} // namespace coretools::probdist
#endif
