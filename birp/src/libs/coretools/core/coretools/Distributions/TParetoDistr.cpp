#include "TParetoDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TParetoDistr::_precalculateTmpVars() {
	_OneDivSigma         = 1. / _scaleSigma;
	_negOneDivXiMinusOne = -(1 / _shapeXi) - 1;
	_logSigma            = log(_scaleSigma);
	_xiSubtract          = -1.0 - 1.0 / _shapeXi;
	_xiDenom             = 1.0 / _shapeXi;
}

void TParetoDistr::set(double locationMu, StrictlyPositive scaleSigma, double shapeXi) {
	_locationMu = locationMu;
	_scaleSigma = scaleSigma;
	_shapeXi    = shapeXi;
	_precalculateTmpVars();
}

void TParetoDistr::set(std::string_view parameterString) {
	str::convertString(parameterString,
	                   std::string{"Use "}.append(name) + "(locationMu, scaleSigma, shapeXi) with sigma > 0.",
	                   _locationMu, _scaleSigma, _shapeXi);
	_precalculateTmpVars();
}

double TParetoDistr::density(double x) const {
	// calculates probability density of a generalized Pareto distribution

	// check for valid arguments
	checkArgs(x, _locationMu, _scaleSigma, _shapeXi);

	return _OneDivSigma * pow(1 + (_shapeXi * (x - _locationMu)) / _scaleSigma, _negOneDivXiMinusOne);
}

double TParetoDistr::logDensity(double x) const {
	// calculates log density of a generalized Pareto distribution
	// Note: density function currently not implemented

	// check for valid arguments
	checkArgs(x, _locationMu, _scaleSigma, _shapeXi);

	return _locationMu == 0.0 ? -_logSigma - (x - _locationMu) / _scaleSigma
	                          : -_logSigma + _xiSubtract * log(1.0 + _shapeXi * (x - _locationMu) / _scaleSigma);
}

Probability TParetoDistr::cumulativeDensity(double x) const {
	// check for valid arguments
	checkArgs(x, _locationMu, _scaleSigma, _shapeXi);

	return _shapeXi == 0.0 ? P(1.0 - exp((_locationMu - x) / _scaleSigma))
		: P(1.0 - pow(1.0 + _shapeXi * (x - _locationMu) / _scaleSigma, -_xiDenom));
}

double TParetoDistr::mean() const { return mean(_locationMu, _scaleSigma, _shapeXi); };

double TParetoDistr::sample() const {
	return instances::randomGenerator().getGeneralizedParetoRand(_locationMu, _scaleSigma, _shapeXi);
}

std::pair<double, double> TParetoDistr::support() const {
	return std::make_pair(_scaleSigma, std::numeric_limits<double>::max());
}

}
