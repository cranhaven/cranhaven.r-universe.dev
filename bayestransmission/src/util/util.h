
#ifndef _UTIL_DEFINED

	#define _UTIL_DEFINED

	#include <stdio.h>
	#include <iostream>
	#include <stdlib.h>
	#include <math.h>
	#include <stdexcept>
	#include <vector>
	using namespace std;

	#include "Allocator.h"
	#include "Object.h"
	#include "Random.h"
	#include "Integer.h"
	#include "Vector.h"
	#include "Map.h"
	#include "IntMap.h"
	#include "List.h"
	#include "SortedList.h"
	#include "Markov.h"

	namespace util
	{
	    template <typename real> real digamma(real x){
		    if (x < 6)
		        return digamma(x+1) - 1/x;

		    real d = log(x) - 0.5 / x;
		    real x2 = 1.0/x/x;

		    d += (((((((-0.08333333*x2) + 0.0210928)*x2 - 0.007575758)*x2 + 0.00416666667)*x2 -0.002968254)*x2 +0.0083333333)*x2 -0.08333333)*x2;

		    return d;
		}
	    template <typename real> real trigamma(real x){
	        if (x < 6)
	            return trigamma(x+1) + 1/x/x;

	        real d = 1/x + 0.5 *1/x/x;
	        real x2 = 1.0/x/x;

	        d += (((((((7.0/6.0*x2) - 691.0/2730.0)*x2 + 5.0/66.0)*x2 - 1/30.0)*x2 + 1.0/42.0)*x2 -1/30.0)*x2 + 1.0/6.0)*x2/x;

	        return d;
	    }

	    template <typename real> real lbeta(real a, real b){
	        return lgamma(a) + lgamma(b) - lgamma(a+b);
	    };
	    template <typename real> real logit(real x){
		    return log(x/(1-x));
		};
	    template <typename real> real logistic(real x){
	        return exp(x)/(1+exp(x));
	    };
	} // namespace util

	using namespace util;
#endif
