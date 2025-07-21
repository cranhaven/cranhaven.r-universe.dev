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
		DEV_ASSERT(_pi != 0.);
		return _logPi;
	} else {
		DEV_ASSERT(_pi != 1.);
		return _logPiComplement;
	}
}

Probability TBernoulliDistr::cumulativeDensity(bool x) const noexcept { return cumulativeDensity(x, _pi); }

bool TBernoulliDistr::sample() const { return sample(_pi); }
} // namespace coretools::probdist
