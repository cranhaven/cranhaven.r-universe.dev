/*
 * mathConstants.h
 *
 *  Created on: Apr 7, 2022
 *      Author: wegmannd
 */

#ifndef CORETOOLS_CORE_MATHCONSTANTS_H_
#define CORETOOLS_CORE_MATHCONSTANTS_H_

// TODO: include <numbers> once we switch to c++20 and remove those defined there

// Math constants not defined in <numbers> but commonly used
constexpr double lnOneHalf   = -0.69314718055994528623; /* ln(0.5) */
constexpr double log10e      = 0.43429448190325181667;  /* log_10(e) */
constexpr double ln10        = 2.3025850929940459011;   /* ln(10) */
constexpr double two_pi      = 6.283185307179586231996; /* 2*pi */
constexpr double sqrt_two_pi = 2.506628274631000241612; /* sqrt(2*pi) */
constexpr double log_two_pi  = 1.837877066409345339082; /* log(2*pi) */
constexpr double ln2         = 0.69314718055994530942;  /* ln(2) */
constexpr double sqrt_2      = 1.41421356237309504880;  /* sqrt(2) */
constexpr double two_sqrt_pi = 1.12837916709551257390;  /* 2/sqrt(pi) */
constexpr double sqrt1_2     = 0.70710678118654752440;  /*  1/sqrt(2) */

#endif /* CORETOOLS_CORE_MATHCONSTANTS_H_ */
