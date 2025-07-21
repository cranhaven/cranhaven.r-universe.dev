/*
 * math_functions.cpp
 *
 *  Created on: Dec 11, 2018
 *      Author: phaentu
 */

#include "coretools/Math/mathFunctions.h"
#include "coretools/Main/TError.h"

namespace coretools {

constexpr auto kf_gamma_eps = 1e-14;
constexpr auto kf_tiny      = 1e-14;

double gammaLog(double x) noexcept(noDebug) {
	// copied (with minor formatting changes) from https://people.sc.fsu.edu/~jburkardt/cpp_src/asa245/asa245.cpp
	// based on algorithm from:
	//     Allan Macleod, Algorithm AS 245,
	//     A Robust and Reliable Algorithm for the Logarithm of the Gamma Function,
	//     Applied Statistics, Volume 38, Number 2, 1989, pages 397-402.
	// alternative: lgamma from cmath library, however, lgamma is not thread-safe
	const double alr2pi = 0.918938533204673;
	const double r1[9]  = {-2.66685511495, -24.4387534237, -21.9698958928, 11.1667541262, 3.13060547623,
	                       0.607771387771, 11.9400905721,  31.4690115749,  15.2346874070};
	const double r2[9]  = {-78.3359299449, -142.046296688, 137.519416416, 78.6994924154, 4.16438922228,
	                       47.0668766060,  313.399215894,  263.505074721, 43.3400022514};
	const double r3[9]  = {-2.12159572323E+05, 2.30661510616E+05,  2.74647644705E+04,
	                       -4.02621119975E+04, -2.29660729780E+03, -1.16328495004E+05,
	                       -1.46025937511E+05, -2.42357409629E+04, -5.70691009324E+02};
	const double r4[5]  = {0.279195317918525, 0.4917317610505968, 0.0692910599291889, 3.350343815022304,
	                       6.012459259764103};

	const double xlge = 510000.0;

	// check interval
	DEBUG_ASSERT(x >= 0.0);
	DEBUG_ASSERT(x <= 1.0E+30);

	// Cases: 0 < x < 0.5 and 0.5 <= x < 1.5
	if (x < 1.5) {
		double value;
		double y;
		if (x < 0.5) {
			value = -log(x);
			y     = x + 1.0;
			if (y == 1.0) { return value; } // test whether x < machine epsilon.
		} else {
			value = 0.0;
			y     = x;
			x     = (x - 0.5) - 0.5;
		}
		return value + x * ((((r1[4] * y + r1[3]) * y + r1[2]) * y + r1[1]) * y + r1[0]) /
		                   ((((y + r1[8]) * y + r1[7]) * y + r1[6]) * y + r1[5]);
	}
	if (x < 4.0) {
		// Case: 1.5 <= x < 4.0
		const double y = (x - 1.0) - 1.0;
		return y * ((((r2[4] * x + r2[3]) * x + r2[2]) * x + r2[1]) * x + r2[0]) /
		       ((((x + r2[8]) * x + r2[7]) * x + r2[6]) * x + r2[5]);
	}
	if (x < 12.0) {
		// Case: 4.0 <= x < 12.0
		return ((((r3[4] * x + r3[3]) * x + r3[2]) * x + r3[1]) * x + r3[0]) /
		       ((((x + r3[8]) * x + r3[7]) * x + r3[6]) * x + r3[5]);
	}
	// Case: 12.0 <= x
	const double y     = log(x);
	const double value = x * (y - 1.0) - 0.5 * y + alr2pi;

	if (x > xlge) { return value; }
	const double x1 = 1.0 / x;
	const double x2 = x1 * x1;
	return value + x1 * ((r4[2] * x2 + r4[1]) * x2 + r4[0]) / ((x2 + r4[4]) * x2 + r4[3]);
}

//------------------------------------------------
// Difference of Gamma functions and betaLog
//------------------------------------------------

constexpr double gammaLog_stirling_diff_useful = 10;

double gammaLogStirling(double x) {
	// Return the Stirling approximation to the lgamma function:
	// \frac{1}{2} \log(2\pi) + (x-\frac{1}{2})*\log(x) - x
	// copied from https://github.com/stan-dev/math/blob/develop/stan/math/prim/fun/lgamma_stirling.hpp
	return 0.5 * log_two_pi + (x - 0.5) * log(x) - x;
}

double gammaLogStirlingDiff(double x) {
	// Return the difference between log of the gamma function and its Stirling approximation.
	// This is useful to stably compute log of ratios of gamma functions with large arguments where the Stirling
	// approximation allows for analytic solution and the (small) differences can be added afterwards. This is for
	// example used in the implementation of lbeta. The function will return correct value for all arguments, but using
	// it can lead to a loss of precision when x < lgamma_stirling_diff_useful. returns \log(\Gamma(x)) - \frac{1}{2}
	// \log(2\pi) + (x-\frac{1}{2})*\log(x) - x copied from
	// https://github.com/stan-dev/math/blob/develop/stan/math/prim/fun/lgamma_stirling_diff.hpp

	DEBUG_ASSERT(x >= 0.0);
	if (x == 0.0) {
		return std::numeric_limits<double>::infinity();
	} else if (x < gammaLog_stirling_diff_useful) {
		return gammaLog(x) - gammaLogStirling(x);
	} else {
		constexpr double stirling_series[]{0.0833333333333333333333333,   -0.00277777777777777777777778,
		                                   0.000793650793650793650793651, -0.000595238095238095238095238,
		                                   0.000841750841750841750841751, -0.00191752691752691752691753,
		                                   0.00641025641025641025641026,  -0.0295506535947712418300654};

		constexpr int n_stirling_terms = 6;
		double result                  = 0.0;
		double multiplier              = 1.0 / x;
		const double inv_x_squared     = multiplier * multiplier;
		for (size_t n = 0; n < n_stirling_terms; n++) {
			if (n > 0) { multiplier *= inv_x_squared; }
			result += stirling_series[n] * multiplier;
		}
		return result;
	}
}

double diffGammaLog(double a, double b) noexcept(noDebug) {
	// calculates:
	// lnGamma(a) - lnGamma(a + b)
	// implemented in a numerically stable way to prevent catastrophic cancellation for large values of x or y
	// inspired from https://github.com/stan-dev/math/blob/develop/stan/math/prim/fun/lbeta.hpp

	DEBUG_ASSERT(a >= 0.0);
	DEBUG_ASSERT(b >= 0.0);

	if (std::max(a, b) < gammaLog_stirling_diff_useful) {
		// both small: calculate as usual
		return gammaLog(a) - gammaLog(a + b);
	}
	if (a < gammaLog_stirling_diff_useful) {
		// a is small, b is large --> only lnGamma(a + b) is an issue
		const double stirling_diff = gammaLogStirlingDiff(a + b);
		const double stirling      = 0.5 * log_two_pi + (a + b - 0.5) * log(a + b) - (a + b);
		return gammaLog(a) - stirling - stirling_diff;
	}
	// a is large (b can be large or small) --> use Stirling for both terms
	const double stirling_diff = gammaLogStirlingDiff(a) - gammaLogStirlingDiff(a + b);
	const double stirling      = (a - 0.5) * log1p(-b / (a + b)) + b * (1.0 - log(a + b));
	return stirling + stirling_diff;
}

double betaLog(double a, double b) noexcept(noDebug) {
	// calculate log of Beta function:
	// lnGamma(x) + lnGamma(y) - lnGamma(x + y)
	// implemented in a numerically stable way to prevent catastrophic cancellation for large values of x or y
	// copied from https://github.com/stan-dev/math/blob/develop/stan/math/prim/fun/lbeta.hpp

	DEBUG_ASSERT(a >= 0.0);
	DEBUG_ASSERT(b >= 0.0);

	const double x = std::min(a, b); // x is the smaller of the two
	const double y = std::max(a, b);

	if (x == 0.0) { return std::numeric_limits<double>::infinity(); }

	// For large x or y, separate the lgamma values into Stirling approximations
	// and appropriate corrections. The Stirling approximations allow for
	// analytic simplifaction and the corrections are added later.
	//
	// The overall approach is inspired by the code in R, where the algorithm is
	// credited to W. Fullerton of Los Alamos Scientific Laboratory
	if (y < gammaLog_stirling_diff_useful) {
		// both small
		return gammaLog(x) + gammaLog(y) - gammaLog(x + y);
	}
	const double x_over_xy = x / (x + y);
	if (x < gammaLog_stirling_diff_useful) {
		// y large, x small
		const double stirling_diff = gammaLogStirlingDiff(y) - gammaLogStirlingDiff(x + y);
		const double stirling      = (y - 0.5) * log1p(-x_over_xy) + x * (1.0 - log(x + y));
		return stirling + gammaLog(x) + stirling_diff;
	}
	// both large
	const double stirling_diff = gammaLogStirlingDiff(x) + gammaLogStirlingDiff(y) - gammaLogStirlingDiff(x + y);
	const double stirling      = (x - 0.5) * log(x_over_xy) + y * log1p(-x_over_xy) + 0.5 * (log_two_pi - log(y));
	return stirling + stirling_diff;
}

//------------------------------------------------
// Digamma function
//------------------------------------------------
namespace TDigamma {
namespace impl {
double psiSeries(double z) noexcept {
	const double z1 = 1. / z;
	const double z2 = z1 * z1;
	const double z4 = z2 * z2;
	const double z6 = z4 * z2;
	const double z8 = z4 * z4;
	return log(z) - 0.5 * z1 - 0.0833333333333333287074 * z2 + 0.00833333333333333287074 * z4 - 0.00390625 * z6 +
	       0.004166666666666666608843 * z8 - 0.007575757575757575967845 * z4 * z6 +
	       0.02109279609279609418726 * z6 * z6 - 0.0833333333333333287074 * z8 * z6;
}
} // namespace impl

double digamma(double x) noexcept {
	const size_t numShifts = std::floor(std::max(6. - x, 0.));
	double psiShifted      = impl::psiSeries(x + numShifts);
	if (numShifts > 0) {
		for (uint8_t i = 1; i <= numShifts; i++) { psiShifted -= 1. / (x + numShifts - i); }
	}
	return psiShifted;
}
} // namespace TDigamma

//--------------------------------------------------------
// Incomplete Gamma function
//--------------------------------------------------------

/* The following computes regularized incomplete gamma functions.
 * Taken from kfunc.c from samtools. Original comment:
 * Formulas are taken from Wiki, with additional input from Numerical
 * Recipes in C (for modified Lentz's algorithm) and AS245
 * (http://lib.stat.cmu.edu/apstat/245).
 */
namespace TIncompleteGamma {

namespace impl {
// regularized lower incomplete gamma function, by series expansion
double lower(double alpha, double z) noexcept {
	double sum, x;
	int k;
	for (k = 1, sum = x = 1.; k < 100; ++k) {
		sum += (x *= z / (alpha + k));
		if (x / sum < kf_gamma_eps) break;
	}
	return exp(alpha * log(z) - z - gammaLog(alpha + 1.0) + log(sum));
}

// regularized upper incomplete gamma function, by continued fraction
double upper(double alpha, double z) noexcept {
	double f = 1. + z - alpha;
	double C = f;
	double D = 0.;
	// Modified Lentz's algorithm for computing continued fraction
	// See Numerical Recipes in C, 2nd edition, section 5.2
	for (int j = 1; j < 100; ++j) {
		const double a = j * (alpha - j), b = (j << 1) + 1 + z - alpha;
		D = b + a * D;
		if (D < kf_tiny) D = kf_tiny;
		C = b + a / C;
		if (C < kf_tiny) C = kf_tiny;
		D              = 1. / D;
		const double d = C * D;
		f *= d;
		if (fabs(d - 1.) < kf_gamma_eps) break;
	}
	return exp(alpha * log(z) - z - gammaLog(alpha) - log(f));
}
} // namespace impl

double lower(double alpha, double z) noexcept {
	if (z <= 1.0 || z < alpha)
		return impl::lower(alpha, z);
	else
		return 1.0 - impl::upper(alpha, z);
}

double upper(double alpha, double z) noexcept {
	if (z <= 1.0 || z < alpha)
		return 1.0 - impl::lower(alpha, z);
	else
		return impl::upper(alpha, z);
}
} // namespace TIncompleteGamma

//--------------------------------------------------------
// Incomplete Beta function
// Reference: Press et al., Numerical Recipes, 3rd edition, pp. 270-273
//--------------------------------------------------------
namespace TIncompleteBeta {
namespace impl {
double betacf(StrictlyPositive a, StrictlyPositive b, Probability x) {
	// evaluates continued fraction for incomplete beta function by modified Lentz's method
	const double EPS   = std::numeric_limits<double>::epsilon();
	const double FPMIN = std::numeric_limits<double>::min() / EPS;
	const double qab   = a + b;
	const double qap   = a + 1.0;
	const double qam   = a - 1.0;
	double c           = 1.0;
	double d           = 1.0 - qab * x / qap;
	if (fabs(d) < FPMIN) d = FPMIN;
	d        = 1.0 / d;
	double h = d;
	for (int m = 1; m < 10000; m++) {
		const int m2 = 2 * m;
		double aa    = m * (b - m) * x / ((qam + m2) * (a + m2));
		d            = 1.0 + aa * d;
		if (fabs(d) < FPMIN) d = FPMIN;
		c = 1.0 + aa / c;
		if (fabs(c) < FPMIN) c = FPMIN;
		d = 1.0 / d;
		h *= d * c;
		aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
		d  = 1.0 + aa * d;
		if (fabs(d) < FPMIN) d = FPMIN;
		c = 1.0 + aa / c;
		if (fabs(c) < FPMIN) c = FPMIN;
		d                = 1.0 / d;
		const double del = d * c;
		h *= del;
		if (fabs(del - 1.0) <= EPS) break;
	}
	return h;
}

double betaiapprox(StrictlyPositive a, StrictlyPositive b, Probability x) {
	// incomplete beta by quadrature; returns I_x(a, b)
	const double a1        = a - 1.0;
	const double b1        = b - 1.0;
	const double mu        = a / (a + b);
	const double lnmu      = log(mu);
	const double lnmuc     = log(1. - mu);
	// y and w are coefficients for Gauss-Legendre quadrature
	constexpr std::array y = {0.0021695375159141994, 0.011413521097787704, 0.027972308950302116, 0.051727015600492421,
	                          0.082502225484340941,  0.12007019910960293,  0.16415283300752470,  0.21442376986779355,
	                          0.27051082840644336,   0.33199876341447887,  0.39843234186401943,  0.46931971407375483,
	                          0.54413605556657973,   0.62232745288031077,  0.70331500465597174,  0.78649910768313447,
	                          0.87126389619061517,   0.95698180152629142};
	constexpr std::array w = {0.0055657196642445571, 0.012915947284065419, 0.020181515297735382, 0.027298621498568734,
	                          0.034213810770299537,  0.040875750923643261, 0.047235083490265582, 0.053244713977759692,
	                          0.058860144245324798,  0.064039797355015485, 0.068745323835736408, 0.072941885005653087,
	                          0.076598410645870640,  0.079687828912071670, 0.082187266704339706, 0.084078218979661945,
	                          0.085346685739338721,  0.085983275670394821};

	const double t  = sqrt(a * b / (pow(a + b, 2) * (a + b + 1.0)));
	const double xu = x > a / (a + b) ? x >= 1.0 ? 1. : std::min(1., std::max(mu + 10. * t, x + 5.0 * t))
	                  : x <= 0.0 ? 0.
	                                  : std::max(0., std::min(mu - 10. * t, x - 5.0 * t));
	double sum      = 0;
	for (int j = 0; j < 18; j++) {
		const double t = x + (xu - x) * y[j];
		sum += w[j] * exp(a1 * (log(t) - lnmu) + b1 * (log(1 - t) - lnmuc));
	}
	const double ans = sum * (xu - x) * exp(a1 * lnmu - gammaLog(a) + b1 * lnmuc - gammaLog(b) + gammaLog(a + b));
	return ans > 0.0 ? 1.0 - ans : -ans;
}
} // namespace impl

double incompleteBeta(StrictlyPositive a, StrictlyPositive b, Probability x) noexcept {
	// returns (regularized) incomplete beta function I_x(a, b) for positive a and b, and x between 0 and 1
	constexpr int SWITCH = 3000; // when to switch to quadrature method
	if (x == 0.0 || x == 1.0) return x;
	if (a > SWITCH && b > SWITCH) return impl::betaiapprox(a, b, x);
	const double bt = exp(gammaLog(a + b) - gammaLog(a) - gammaLog(b) + a * log(x) + b * log(1.0 - x));
	return x < (a + 1.0) / (a + b + 2.0) ? bt * impl::betacf(a, b, x) / a
	                                     : 1.0 - bt * impl::betacf(b, a, x.complement()) / b;
}

double inverseIncompleteBeta(Probability p, StrictlyPositive a, StrictlyPositive b) noexcept {
	// inverse of incomplete beta function
	// Returns x such that I_x(a,b) = p for argument p between 0 and 1. Uses Halley's method
	if (p <= 0.) return 0.;
	if (p >= 1.) return 1.;

	const double EPS = 1.e-8;
	const double a1  = a - 1.;
	const double b1  = b - 1.;
	double x;

	if (a >= 1. && b >= 1.) {
		const double pp = (p < 0.5) ? p.get() : 1. - p;
		const double t  = sqrt(-2. * log(pp));
		x               = (2.30753 + t * 0.27061) / (1. + t * (0.99229 + t * 0.04481)) - t;
		if (p < 0.5) x = -x;
		const double al = (pow(x, 2) - 3.) / 6.;
		const double h  = 2. / (1. / (2. * a - 1.) + 1. / (2. * b - 1.));
		const double w =
		    (x * sqrt(al + h) / h) - (1. / (2. * b - 1) - 1. / (2. * a - 1.)) * (al + 5. / 6. - 2. / (3. * h));
		x = a / (a + b * exp(2. * w));
	} else {
		double lna = log(a / (a + b)), lnb = log(b / (a + b));
		const double t = exp(a * lna) / a;
		const double u = exp(b * lnb) / b;
		const double w = t + u;
		if (p < t / w)
			x = pow(a * w * p, 1. / a);
		else
			x = 1. - pow(b * w * (1. - p), 1. / b);
	}
	const double afac = -gammaLog(a) - gammaLog(b) + gammaLog(a + b);
	for (int j = 0; j < 10; j++) {
		if (x == 0. || x == 1.) return x;
		const double err = incompleteBeta(a, b, P(x)) - p;
		double t         = exp(a1 * log(x) + b1 * log(1. - x) + afac);
		const double u   = err / t;
		x -= (t = u / (1. - 0.5 * std::min(1., u * (a1 / x - b1 / (1. - x)))));
		if (x <= 0.) x = 0.5 * (x + t);
		if (x >= 1.) x = 0.5 * (x + t + 1.);
		if (fabs(t) < EPS * x && j > 0) break;
	}
	return x;
}
} // namespace TIncompleteBeta

//------------------------------------------------
// binomial p-values
//------------------------------------------------
namespace TBinomPValue {
namespace impl {

double binomPValue(uint32_t k, uint32_t l) {
	// p = 0.5
	double cumul   = 0.0;
	// int n = k + l;
	const auto end = std::min(k, l);
	for (uint32_t i = 0; i <= end; i++) {
		cumul += exp(chooseLog(l, i) + -0.6931472 * l); // -0.6931472 is log(0.5)
	}
	return cumul;
}
} // namespace impl

double binomPValue(size_t k, size_t l) {
	const static std::array<std::vector<double>, 100> table = []() {
		std::array<std::vector<double>, 100> fs;
		for (size_t i = 0; i < 100; ++i) {
			const auto jmax = std::floor(i / 2) + 1;
			for (size_t j = 0; j < jmax; ++j) { fs[i].push_back(impl::binomPValue(j, i - j)); }
		}
		return fs;
	}();
	const uint32_t n = l + k;
	if (n < table.size()) { return table[n][std::min(k, l)]; }
	return impl::binomPValue(k, l);
}

} // namespace TBinomPValue
//--------------------------------------------------------
// Kolmogorov-Smirnov Distribution
//--------------------------------------------------------
namespace TKolmogorovSmirnovDistr {
namespace impl {

double invxlogx(double y) {
	// For negative y, 0 > y > -e^(-1), return x such that y = x log(x).
	// The value returned is always the smaller of the two roots and is in the range 0 < x < e^(-1) .
	// From Book Numerical Recipes, pp. 309 (chapter 6.11, Inverse of the Function x log(x))
	constexpr double ooe = 0.367879441171442322;
	double t, u, to = 0.;
	DEV_ASSERT(y < 0 && y > -ooe);
	if (y < -0.2)
		u = log(ooe - sqrt(2 * ooe * (y + ooe))); // First approximation by inverse of Taylor series.
	else
		u = -10.;
	do {
		u += (t = (log(y / u) - u) * (u / (1. + u)));
		if (t < 1.e-8 && std::fabs(t + to) < 0.01 * std::fabs(t)) break;
		to = t;
	} while (std::fabs(t / u) > 1.e-15);
	return exp(u);
}
} // namespace impl

Probability cumulativeDistrFunction(Positive z) {
	// Return cumulative distribution function
	// From Book Numerical Recipes, pp. 334 (chapter 6.14.12, Kolmogorov-Smirnov Distribution)
	if (z == 0.) return P(0.);
	if (z < 1.18) {
		const double y = exp(-1.23370055013616983 / sqrt(z));
		return P(2.25675833419102515 * sqrt(-log(y)) * (y + pow(y, 9) + pow(y, 25) + pow(y, 49)));
	}
	const double x = exp(-2. * sqrt(z));
	return P(1. - 2. * (x - pow(x, 4) + pow(x, 9)));
}

Probability complementaryCumulativeDistrFunction(Positive z) {
	// Return complementary cumulative distribution function.
	// From Book Numerical Recipes, pp. 334 (chapter 6.14.12, Kolmogorov-Smirnov Distribution)
	if (z == 0.) return P(1.);
	if (z < 1.18) return P(1. - cumulativeDistrFunction(z));
	const double x = exp(-2. * sqrt(z));
	return P(2. * (x - pow(x, 4) + pow(x, 9)));
}

double invComplementaryCumulativeDistrFunction(Probability q) {
	// Return inverse of the complementary cumulative distribution function.
	// From Book Numerical Recipes, pp. 334 (chapter 6.14.12, Kolmogorov-Smirnov Distribution)

	DEV_ASSERT(q > 0.);
	if (q == 1.) return 0.;
	if (q > 0.3) {
		const double f = -0.392699081698724155 * sqrt(1. - q);
		double y       = impl::invxlogx(f);
		double t       = 0;
		// Initial guess.
		do {
			const double logy = log(y);
			const double ff   = f / sqrt(1. + pow(y, 4) + pow(y, 12));
			const double u    = (y * logy - ff) / (1. + logy);
			// Newton’s method correction.
			t                 = u / std::max(0.5, 1. - 0.5 * u / (y * (1. + logy))); // Halley.
			y -= t;
		} while (std::fabs(t / y) > 1.e-15);
		return 1.57079632679489662 / sqrt(-log(y));
	} else {
		double xp;
		double x = 0.03;
		do {
			// Iteration (6.14.59).
			xp = x;
			x  = 0.5 * q + pow(x, 4) - pow(x, 9);
			if (x > 0.06) x += pow(x, 16) - pow(x, 25);
		} while (std::fabs((xp - x) / x) > 1.e-15);
		return sqrt(-0.5 * log(x));
	}
}

double invCumulativeDistrFunction(Probability p) {
	// Return inverse of the cumulative distribution function.
	// From Book Numerical Recipes, pp. 334 (chapter 6.14.12, Kolmogorov-Smirnov Distribution)
	return invComplementaryCumulativeDistrFunction(p.complement());
}
} // namespace TKolmogorovSmirnovDistr

void runOneSampleKolmogorovSmirnovTest(std::vector<double> &Data, double CumulFunc(double), double &d,
                                       Probability &prob) {
	// tests if a sample of a probability distribution comes from the same distribution as a reference probability
	// distribution Given an array Data [0..n-1], and given a function of a single variable CumulFunc that is a
	// cumulative distribution function ranging from 0 (for smallest values of its argument) to 1 (for largest
	// values of its argument), this routine returns the Kolmogorov–Smirnov statistic d and the p-value prob. Small
	// values of prob show that the cumulative distribution function of data is significantly different from func.
	// The array data is modified by being sorted into ascending order. Algorithm from Book Numerical Receipes, pp.
	// 737 (4.3.3 Kolmogorov-Smirnov Test)

	// sort the data
	std::sort(Data.begin(), Data.end());

	// loop over sorted data points
	double cdfAtMaxDistance = 0.;
	d                       = 0.;
	for (size_t j = 0; j < Data.size(); j++) {
		double cdf      = static_cast<double>(j + 1) / static_cast<double>(Data.size()); // cdf of data
		double cdf_func = CumulFunc(Data[j]);                                            // cdf of function
		double tmp_dist = std::max(std::fabs(cdfAtMaxDistance - cdf_func), std::fabs(cdf - cdf_func));
		if (tmp_dist > d) { // Maximum distance
			d                = tmp_dist;
			cdfAtMaxDistance = cdf;
		}
	}

	// compute p-value
	double sqrtN = sqrt(Data.size());
	prob         = TKolmogorovSmirnovDistr::complementaryCumulativeDistrFunction((sqrtN + 0.12 + 0.11 / sqrtN) * d);
}

}; // namespace coretools
