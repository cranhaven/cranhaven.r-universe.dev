#include "TBinomialDistr.h"
#include "TBinomialDistrVariableN.h"
#include "coretools/Strings/convertString.h"

namespace coretools::probdist {
namespace impl {
auto precalculateQMuSigmaGamma(size_t n, Probability prob) {
	double q     = prob.complement();
	double mu    = (double)n * prob.get();
	double sigma = sqrt((double)n * prob.get() * q);
	double gamma = (q - prob) / sigma;
	return std::make_tuple(q, mu, sigma, gamma);
}

size_t doSearch_invCumulativeDensity(size_t y, double *z, double p, size_t n,
                                                                    size_t incr, const TBinomialDistr &BinomN) {
	if (*z >= p) { // search to the left
		for (;;) {
			double newz;
			if (y == 0 || (newz = BinomN.cumulativeDensity(y - incr)) < p) { return y; }
			y  = std::max<int>(0, (int)y - (int)incr);
			*z = newz;
		}
	} else { // search to the right
		for (;;) {
			y = std::min(y + incr, n);
			if (y == n || (*z = BinomN.cumulativeDensity(y)) >= p) { return y; }
		}
	}
}
}

void TBinomialDistr::_precalculateTmpVars() {
	_logProb           = logP(_prob);
	_ProbComplement    = _prob.complement();
	_logProbComplement = logP(_ProbComplement);
	_mean              = (double)_trials * _prob;

	std::tie(_q_invCD, _mu_invCD, _sigma_invCD, _gamma_invCD) =
	    impl::precalculateQMuSigmaGamma(_trials, _prob);
}

void TBinomialDistr::set(size_t n, Probability p) {
	_prob   = p;
	_trials = n;
	_precalculateTmpVars();
}

void TBinomialDistr::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(n,p) with n >= 0 and 0 <= p <= 1.",
	                   _trials, _prob);
	_precalculateTmpVars();
}

Probability TBinomialDistr::density(size_t k) const {
	// calculates density of a binomial distribution
	DEV_ASSERT(k <= _trials);
	return P(choose(_trials, k) * pow(_prob, (double)k) * pow(_ProbComplement, (double)_trials - (double)k));
}

LogProbability TBinomialDistr::logDensity(size_t k) const {
	// calculates log density of a binomial distribution
	DEV_ASSERT(k <= _trials);
	return logP(chooseLog(_trials, k) + (double)k * _logProb + ((double)_trials - (double)k) * _logProbComplement);
}

Probability TBinomialDistr::cumulativeDensity(size_t k) const {
	DEV_ASSERT(k <= _trials);
	if (_trials == k) { return P(1.0); }
	return P(1 - TIncompleteBeta::incompleteBeta(k + 1, _trials - k, _prob));
}

size_t TBinomialDistr::invCumulativeDensity(ZeroOneOpen p, size_t n, Probability prob) noexcept {
	// = qbinom in R
	return TBinomialDistrVariableN::invCumulativeDensity(p, n, prob);
}

size_t TBinomialDistr::invCumulativeDensity(ZeroOneOpen p, size_t n, Probability prob, double q, double mu,
											double sigma, double gamma) const {
	// The quantile is defined as the smallest value x such that F(x) >= p, where F(x) is the cumulativeDensity function
	// Code adapted from https://github.com/SurajGupta/r-source/blob/master/src/nmath/qbinom.c
	if (prob == 0. || n == 0) { return 0.; }
	if (p + 1.01 * DBL_EPSILON >= 1.) { return n; }
	if (q == 0.) { return n; } // covers the full range of the distribution

	// y = approx.value (Cornish-Fisher expansion)
	double z = invCumulativeDensity(p, 0., P(1.));
	size_t y = floor(mu + sigma * (z + gamma * (z * z - 1.0) / 6.0) + 0.5);
	if (y > n) { y = n; } // way off

	z = cumulativeDensity(y);

	// fuzz to ensure left continuity
	p *= 1 - 64 * DBL_EPSILON;

	if ((double)n < 1e5) {
		return impl::doSearch_invCumulativeDensity(y, &z, p, n, 1, *this);
	} else { // Otherwise be a bit cleverer in the search
		size_t incr = floor((double)n * 0.001), oldincr;
		do {
			oldincr = incr;
			y       = impl::doSearch_invCumulativeDensity(y, &z, p, n, incr, *this);
			incr    = std::max<size_t>(1, (size_t)floor((double)incr / 100.0));
		} while (oldincr > 1 && (double)incr > (double)n * 1e-15);
		return y;
	}
}

size_t TBinomialDistr::invCumulativeDensity(ZeroOneOpen p) const {
	// = qbinom in R
	return invCumulativeDensity(p, _trials, _prob, _q_invCD, _mu_invCD, _sigma_invCD,
	                                                           _gamma_invCD);
}

Positive TBinomialDistr::mean() const { return _mean; }

size_t TBinomialDistr::sample() const { return sample(_trials, _prob); }

void TBinomialDistrVariableN::_precalculateTmpVars() {
	_logProb           = logP(_prob);
	_ProbComplement    = _prob.complement();
	_logProbComplement = logP(_ProbComplement);
}

void TBinomialDistrVariableN::set(Probability p) {
	_prob = p;
	_precalculateTmpVars();
}

void TBinomialDistrVariableN::set(std::string_view parameterString) {
	str::convertString(parameterString, std::string{"Use "}.append(name) + "(p) with 0 <= p <= 1.", _prob);
	_precalculateTmpVars();
}

Probability TBinomialDistrVariableN::density(size_t n, size_t k) const {
	// calculates density of a binomial distribution
	DEV_ASSERT(k <= n);
	return P(choose(n, k) * pow(_prob, (double)k) * pow(_ProbComplement, (double)n - (double)k));
}

LogProbability TBinomialDistrVariableN::logDensity(size_t n, size_t k) const {
	// calculates log density of a binomial distribution
	DEV_ASSERT(k <= n);
	return logP(chooseLog(n, k) + (double)k * _logProb + ((double)n - (double)k) * _logProbComplement);
}

size_t TBinomialDistrVariableN::invCumulativeDensity(ZeroOneOpen p, size_t n, Probability prob) noexcept {
	// = qbinom in R
	TBinomialDistr binomN(n, prob);
	auto [q, mu, sigma, gamma] = impl::precalculateQMuSigmaGamma(n, prob);
	return binomN.invCumulativeDensity(p, n, prob, q, mu, sigma, gamma);
}

Probability TBinomialDistrVariableN::cumulativeDensity(uint32_t n, uint32_t k) const {
	return cumulativeDensity(n, k, _prob);
}

size_t TBinomialDistrVariableN::invCumulativeDensity(ZeroOneOpen p, size_t n) const {
	return invCumulativeDensity(p, n, _prob);
}

Positive TBinomialDistrVariableN::mean(size_t n) const { return mean(n, _prob); }

size_t TBinomialDistrVariableN::sample(size_t n) const {
	return instances::randomGenerator().getBinomialRand(_prob, n);
}
}

