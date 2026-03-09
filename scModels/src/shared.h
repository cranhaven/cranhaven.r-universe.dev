#include <Rcpp.h>
#include <cmath>
#include <cstdlib>
#include "mpreal.h"

// constants

#define Q_LIMIT 256

// functions

bool validKummerParameters(double a, double b, bool warn = true);
bool isInteger(double x, bool warn = true);
bool validProbability(double p, bool warn = false);
bool isInadmissible(double x, bool warn = false);
bool validPbParameters(double alpha, double beta, double c, bool warn = false);
void reportGslError(int status);

// macros
#define GETV(x, i)      x[i % x.length()]    // wrapped indexing of vector
#define GETM(x, i, j)   x(i % x.nrow(), j)   // wrapped indexing of matrix
