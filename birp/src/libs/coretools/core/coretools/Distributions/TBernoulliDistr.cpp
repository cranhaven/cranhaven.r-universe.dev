#include "TBernoulliDistr.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
void TBernoulliDistr::_precalculateTmpVars() {
	_piComplement    = _pi.complement();
	_logPi           = logP(_pi);
	_logPiComplement = logP(_piComplement);
}

void TBernoulliDistr::set(Probability pi) {
	_pi = pi;
	_precalculateTmpVars();
}

void TBernoulliDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(p) with 0 <= p <= 1.", _pi);
	_precalculateTmpVars();
}

LogProbability TBernoulliDistr::logDensity(bool x) const {
	// calculates log density of a bernoulli distribution
	if (x) {
		if (_pi == 0.) { DEVERROR("pi is exactly 0., and log(pi) results in -Inf!"); }
		return _logPi;
	} else {
		if (_pi == 1.) { DEVERROR("pi is exactly 1., and log(1-pi) results in -Inf!"); }
		return _logPiComplement;
	}
}

Probability TBernoulliDistr::cumulativeDensity(bool x) const noexcept { return cumulativeDensity(x, _pi); }

bool TBernoulliDistr::sample() const { return sample(_pi); }
} // namespace coretools::probdist
