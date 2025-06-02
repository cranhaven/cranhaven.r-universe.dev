/*
 * TRandomGenerator.cpp
 *
 *  Created on: Sep 24, 2009
 *      Author: wegmannd
 */

#include "coretools/Main/TRandomGenerator.h"

#include <chrono>
#include <cmath>

namespace coretools {

void TRandomGenerator::setSeed(long addToSeed, bool seedIsFixed) {
	using namespace std::chrono;
	_seed =
	    (seedIsFixed
	         ? static_cast<unsigned long>(std::abs(addToSeed))
	         : static_cast<unsigned long>(std::abs(
	               addToSeed + duration_cast<milliseconds>(high_resolution_clock::now().time_since_epoch()).count())));
#ifdef _OPENMP
	_integerGen.seed(_seed + omp_get_thread_num());
#else
	_integerGen.seed(_seed);
#endif
};

//--------------------------------------------------------
// Beta Distribution
//--------------------------------------------------------
/*
   returns a variate that is Beta distributed on the interval [a,b]
   with shape parameters alpha and beta.

   The Beta function has two shaping parameters, alpha and beta.
   Setting these parameters to 1.5 and 1.5 yields a normal-like
   distribution, but without tails. If alpha and beta are equal to
   1 it is a uniform distribution.

   If alpha and beta are less than 1, use a rejection algorithm;
   Otherwise use the fact that if x is distributed Gamma(alpha) and y
   Gamma(beta) then x/(x+y) is Beta(alpha, beta).

   The rejection algorithm first a Beta variate is found over the
   interval [0, 1] with not the most efficient algorithm.  This is then
   scaled at the end to desired range.

   It may be tempting to re-use the second number drawn as the first
   random number of the next iteration, and simply draw one more.
   *** Don't do it.  You will produce an incorrect distribution.  You
   must draw two new numbers for the rejection sampling to be correct.

   References:
   - Ripley, Stochastic Simulations, John Wiley and Sons, 1987, p 90.
   - J.H.Maindonald, Statistical Computation, John Wiley and Sons,
     1984, p 370.
*/
Probability TRandomGenerator::getBetaRandom(StrictlyPositive alpha, StrictlyPositive beta, double a,
                                            double b) {
	if (b <= a) DEVERROR("Bad shape or range for a beta variate!");
	// Scale to interval [a, b]
	return P(a + getBetaRandom(alpha, beta) * (b - a));
}

Probability TRandomGenerator::getBetaRandom(StrictlyPositive alpha, StrictlyPositive beta) {
	const auto alpha_inv = 1. / alpha; // alpha and beta are > 0
	const auto beta_inv  = 1. / beta;

	if ((alpha < 1) && (beta < 1)) {
		// use rejection
		while (true) {
			const auto u1 = std::pow(getRand(), alpha_inv);
			const auto w  = u1 + std::pow(getRand(), beta_inv);
			if (w < 1.0 && w != 0.0) return P(u1 / w);
		}
	}
	// Else use relation to Gamma
	const auto u1 = getGammaRand(alpha);
	return P(u1 / (u1 + getGammaRand(beta)));
};

//--------------------------------------------------------
// Dirichlet Distribution
//--------------------------------------------------------
void TRandomGenerator::fillDirichletRandom(size_t K, StrictlyPositive *alpha, ZeroOneOpen *res) {
	assert(K > 0);

	double sum = 0.0;
	for (size_t k = 0; k < K; ++k) {
		res[k] = getGammaRand(alpha[k]);
		sum += res[k];
	}
	for (size_t k = 0; k < K; ++k) res[k] /= sum;
};

double TRandomGenerator::getExponentialRandomTruncated(StrictlyPositive lambda, double lowerBound,
                                                       double upperBound) {
	// copied from stack overflow
	return -std::log(std::exp(-lambda * lowerBound) -
	                 (std::exp(-lambda * lowerBound) - std::exp(-lambda * upperBound)) * getRand()) /
	       lambda;
}

double TRandomGenerator::getGeneralizedParetoRand(double locationMu, StrictlyPositive scaleSigma,
                                                  double shapeXi) {
	constexpr double MAGIC_NUMBER = 1e-6;
	if (std::abs(shapeXi) < MAGIC_NUMBER)
		return locationMu - scaleSigma * log(getRand());
	else
		return locationMu + scaleSigma * (std::pow(getRand(), -shapeXi) - 1.0) / shapeXi;
}

}; // namespace coretools
