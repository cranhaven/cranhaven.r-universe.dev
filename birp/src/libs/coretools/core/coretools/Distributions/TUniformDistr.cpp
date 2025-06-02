#include "TUniformDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {

void TUniformDistr::set(double Min, double Max) {
	_min     = Min;
	_max     = Max;
	_dens    = _densityInRange(Min, Max);
	_logDens = _logDensityInRange(Min, Max);
}

void TUniformDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(min, max).", _min, _max);
	set(_min, _max);
}

Positive TUniformDistr::density(double x) const noexcept {
	// calculates density of a uniform distribution
	if (x < _min || x > _max) { return 0.0; }
	if (!coretools::checkForNumericOverflow_subtraction(_min, _max)) { return std::numeric_limits<double>::min(); }
	return _dens;
}

double TUniformDistr::logDensity(double x) const noexcept {
	// calculates log density of a uniform distribution
	if (x < _min || x > _max) { return std::numeric_limits<double>::lowest(); } // prevent log(0) = -inf
	if (!coretools::checkForNumericOverflow_subtraction(_min, _max)) { return log(std::numeric_limits<double>::min()); }
	return _logDens;
}

Probability TUniformDistr::cumulativeDensity(double x) const noexcept { return cumulativeDensity(x, _min, _max); }

double TUniformDistr::sample() const noexcept { return sample(_min, _max); }
}
