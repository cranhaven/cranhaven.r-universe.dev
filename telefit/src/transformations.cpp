#include "transformations.h"

double mcstat2::logit(double x) { return log( x / (1.0 - x) ); }

double mcstat2::invlogit(double x) {
	double expX = exp(x);
	return std::isinf(expX)!=0 ? 1 : expX / (1.0 + expX);
}
