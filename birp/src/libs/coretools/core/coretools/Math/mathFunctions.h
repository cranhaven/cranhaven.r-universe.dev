/*
 * math_functions.h
 *
 *  Created on: Dec 10, 2018
 *      Author: phaentu
 */

#ifndef MATHFUNCTIONS_H_
#define MATHFUNCTIONS_H_

#include "coretools/Types/commonWeakTypes.h"
#include "coretools/Types/probability.h"
#include <array>

namespace coretools {

// Only use with positive exponents. For negative exponants, use upow(1./base, exp) or 1./upow(base, exp)
template<typename T> constexpr T uPow(T base, unsigned int exp) noexcept {
	if (exp == 0) return static_cast<T>(1);
	if (exp == 1) return base;

	// assuming mostly small exponants, this will speed up things
	// however, if exp is mostly > 3, these ifs could hurt
	if (exp == 2) return base * base;
	if (exp == 3) return base * base * base;

	T result = static_cast<T>(1);

	do {                             // at start, exp is always > 1
		if (exp & 1) result *= base; // is odd
		exp >>= 1;
		base *= base;
	} while (exp > 1);
	return result * base;
}

template<unsigned int exp, typename T> constexpr T uPow(T base) noexcept {
	if constexpr (exp == 0)
		return static_cast<T>(1);
	else if constexpr (exp == 1)
		return base;
	else
		return (exp & 1 ? base : 1) * uPow<exp / 2>(base * base);
}

//------------------------------------------------
// Useful conversions
//------------------------------------------------
[[nodiscard]] constexpr double logToLog10(double LogVal) noexcept { return 0.43429448190325181667 * LogVal; };

[[nodiscard]] constexpr double log10ToLog(double Log10Val) noexcept { return 2.30258509299404590109 * Log10Val; };

//------------------------------------------------
// Gamma function
//------------------------------------------------
double gammaLog(double z) noexcept;

//------------------------------------------------
// Difference of Gamma functions and betaLog
//------------------------------------------------

double diffGammaLog(double a, double b) noexcept;
double betaLog(double a, double b) noexcept;

//----------------------------------------------------------------
// Factorials, see Numerical recipes (third edition), chapter 6.1
//----------------------------------------------------------------
namespace TFactorial {
template<typename T> constexpr double factorial(T n) {
	// Returns the value n! as a floating-point number.
	static_assert(std::is_integral<T>::value, "Integral required.");
	if constexpr (std::is_signed_v<T>)
		if (n < 0) DEVERROR("Argument n (= ", n, ") cannot be smaller than 0!");
	if (n > 170) DEVERROR("Argument n (= ", n, ") in factorial must be <= 170!");

	constexpr std::array<double, 171> factorialTable = []() {
		std::array<double, 171> fs{1.};
		for (int i = 1; i < 171; i++) fs[i] = i * fs[i - 1];
		return fs;
	}();
	return factorialTable[n];
}

template<typename T> double factorialLog(T n) {
	// Returns ln(n!)
	static_assert(std::is_integral<T>::value, "Integral required.");
	if constexpr (std::is_signed_v<T>)
		if (n < 0) DEVERROR("Argument n (= ", n, ") in factorialLog must be >= 0!");
	if (n == 0) { return 0.0; } // gammaLog is slightly inprecise

	constexpr size_t NTOP                                = 2000;
	const static std::array<double, 2000> factorialTable = []() {
		std::array<double, NTOP> fs;
		fs.front() = 1.0; // gammaLog is slightly inprecise
		for (size_t i = 1; i < NTOP; i++) fs[i] = gammaLog(i + 1.);
		return fs;
	}();
	if (static_cast<size_t>(n) < NTOP) return factorialTable[n];
	return gammaLog(n + 1.);
}
}; // namespace TFactorial

namespace TFallingFactorial {
template<typename T> double fallingFactorial(T x, T n) {
	static_assert(std::is_integral<T>::value, "Integral required.");
	// calculates x * (x-1) * (x-2) * ... * (x - n + 1) = prod_{k=0}^{n-1} (x-k)
	// Not stored as a table, because n may vary -> would need a table per n
	if (n == 0) { return 1.0; }
	if (n > x) { return 0.0; }
	double fac = x;
	for (size_t k = 1; k < (size_t)n; k++) { fac *= (x - k); }
	return fac;
}

template<typename T> double fallingFactorialLog(T x, T n) {
	static_assert(std::is_integral<T>::value, "Integral required.");
	// calculates log(x * (x-1) * (x-2) * ... * (x - n + 1)) = sum_{k=0}^{n-1} log(x-k)
	// Not stored as a table, because n may vary -> would need a table per n
	if (n == 0) { return 0.0; }
	if (n > x) { return std::numeric_limits<double>::lowest(); }
	return gammaLog(x + 1) - gammaLog(x - n + 1);
}
} // namespace TFallingFactorial

//------------------------------------------------
// choose function (binomial coefficient)
//------------------------------------------------
template<typename T> double chooseLog(T n, T k) {
	return TFactorial::factorialLog(n) - TFactorial::factorialLog(k) - TFactorial::factorialLog(n - k);
}

template<typename T> constexpr size_t choose(T n, T k) {
	// note: results are not accurate if n is large (if factorial(n) reaches numerical limits of double)
	return TFactorial::factorial(n) / TFactorial::factorial(k) / TFactorial::factorial(n - k);
}

//------------------------------------------------
// Digamma function
//------------------------------------------------
namespace TDigamma {
double digamma(double x) noexcept;
}

//------------------------------------------------
// Incomplete gamma function
//------------------------------------------------
namespace TIncompleteGamma {
double lower(double alpha, double z) noexcept;
double upper(double alpha, double z) noexcept;
} // namespace TIncompleteGamma

//------------------------------------------------
// Incomplete beta function
//------------------------------------------------

namespace TIncompleteBeta {
double incompleteBeta(StrictlyPositive a, StrictlyPositive b, Probability x) noexcept;
double inverseIncompleteBeta(Probability p, StrictlyPositive a, StrictlyPositive b) noexcept;
} // namespace TIncompleteBeta

//--------------------------------------------------------
// Kolmogorov-Smirnov Distribution
//--------------------------------------------------------

namespace TKolmogorovSmirnovDistr {
Probability cumulativeDistrFunction(Positive z);
Probability complementaryCumulativeDistrFunction(Positive z);
double invComplementaryCumulativeDistrFunction(Probability q);
double invCumulativeDistrFunction(Probability z);
} // namespace TKolmogorovSmirnovDistr

//--------------------------------------------------------
// Kolmogorov-Smirnov Test
//--------------------------------------------------------

void runOneSampleKolmogorovSmirnovTest(std::vector<double> &Data, double CumulFunc(double), double &d,
                                       Probability &prob);

//------------------------------------------------
// binomial p-value
//------------------------------------------------
namespace TBinomPValue {
double binomPValue(size_t k, size_t l);
};

//------------------------------------------------
// Logit and logistic function
//------------------------------------------------
inline double logit(Probability x) noexcept { return log(x.oddsRatio()); }
inline double logit(double x) noexcept { return logit(P(x)); }
inline Probability expit(double x) noexcept { return Probability(1. / (1. + exp(-x))); }
inline Probability logistic(double x) noexcept { return expit(x); }

//------------------------------------------------
// types and numeric limits
//------------------------------------------------

//------------------------------------------------
// addition
// for unsigned integers: y argument is passed as integer, otherwise we could never subtract!
template<typename T>
constexpr bool checkForNumericOverflow_addition(T x, std::conditional_t<std::is_unsigned_v<T>, int, T> y) noexcept {
	// overflow
	if (y > 0) { return x <= std::numeric_limits<T>::max() - y; }
	// underflow
	if (y < 0) { return x >= std::numeric_limits<T>::lowest() - y; }
	return true;
}

//------------------------------------------------
// subtraction

// for unsigned integers: y argument is passed as integer, otherwise we could never subtract!
template<typename T>
constexpr bool checkForNumericOverflow_subtraction(T x, std::conditional_t<std::is_unsigned_v<T>, int, T> y) noexcept {
	// overflow
	if (y < 0) { return x <= std::numeric_limits<T>::max() + y; }
	// underflow
	if (y > 0) { return x >= std::numeric_limits<T>::lowest() + y; }
	return true;
}

//------------------------------------------------
// multiplication
//------------------------------------------------
// things to realize:

//        I-------------------------|-------------------------I
//       min                        0                        max

//      -> can cross numeric max by...
//          1) x>0 * y>0
//          2) x<0 * y<0
//      -> can cross numeric min by...
//          1) x>0 * y<0
//          2) x>0 * y>0

//      -> numeric_min is (usually) negative and numeric_max is always positive

//      -> when multiplying/dividing two negative values, a positive value results -> this complicates things, as
//          comparison with < and > then changes -> we need to take these different cases into account!

template<typename T> bool constexpr checkForNumericOverflow_multiplication(T x, T y) noexcept {
	if (y == 0) { // avoid division by zero errors
		return true;
	}
	// for signed integers: also need to check if one number is -1 and another is min(), because multiplying them we get
	// abs(min()) which is 1 higher than max()
	if constexpr (std::is_integral_v<T> && std::is_signed_v<T>) {
		if (x == -1) { return y != std::numeric_limits<T>::lowest(); }
		if (y == -1) { return x != std::numeric_limits<T>::lowest(); }
	}
	// overflow
	if (x > 0) {
		if (y > 0)
			return x <= std::numeric_limits<T>::max() / y;
		else
			return x <= std::numeric_limits<T>::lowest() / y;
	} // overflow
	if (x < 0) {
		if (y < 0) {
			return x >= std::numeric_limits<T>::max() / y;
		} else
			return x >= std::numeric_limits<T>::lowest() / y;
	}
	return true;
}

}; // namespace coretools

#endif /* MATHFUNCTIONS_H_ */
