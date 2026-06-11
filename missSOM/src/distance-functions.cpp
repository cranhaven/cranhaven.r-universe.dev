/* Definition of built-in distance functions for use in the som, xyf
   and supersom functions.

   Authors: Johannes Kruisselbrink and Ron Wehrens
*/

#include "distance-functions.h"

#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rcpp.h>
#include <cfloat>

#define UNIF unif_rand()

/*
 * Creates an array of n distance function pointers according to the specified
 * types.
 */
// [[Rcpp::export]]
Rcpp::ExpressionVector CreateStdDistancePointers(const Rcpp::IntegerVector &types) {
  Rcpp::ExpressionVector distanceFunctions(types.size());
  for (int l = 0; l < types.size(); l++) {
    distanceFunctions[l] = CreateStdDistancePointer(types[l]);
  }
  return distanceFunctions;
}

/*
 * Returns a distance function XPtr pointer of the specified type.
 */
// [[Rcpp::export]]
Rcpp::XPtr<DistanceFunctionPtr> CreateStdDistancePointer(int type) {
  return CreateNonNaNDistanceFunctionXPtr(type);
}

/*
 * Returns an XPtr pointer to a distance function of the specified type that
 * does not account for NaNs.
 */
Rcpp::XPtr<DistanceFunctionPtr> CreateNonNaNDistanceFunctionXPtr(int type) {
  switch ((DistanceType)type) {
    case EUCLIDEAN:
      return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&EuclideanDistance)));
    case SUMOFSQUARES:
      return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&SumOfSquaresDistance)));
    case MANHATTAN:
      return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&ManhattanDistance)));
    case TANIMOTO:
      return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&TanimotoDistance)));
    default:
      return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&EuclideanDistance)));
  }
  return (Rcpp::XPtr<DistanceFunctionPtr>(new DistanceFunctionPtr(&EuclideanDistance)));
}


/*
 * Creates an array of n distance function pointers according to the specified
 * types.
 */
std::vector<DistanceFunctionPtr> GetDistanceFunctions(const Rcpp::ExpressionVector &distanceFunctionXPtrs) {
  std::vector<DistanceFunctionPtr> distanceFunctions;
  for (int l = 0; l < distanceFunctionXPtrs.size(); l++) {
    distanceFunctions.push_back(AsDistanceFunctionPtr(distanceFunctionXPtrs[l]));
  }
  return distanceFunctions;
}

/*
 * Computes the distances between all objects of the data matrix. Returns the
 * lower triangle of the distance matrix as a vector.
 */
Rcpp::NumericVector ObjectDistances(Rcpp::NumericMatrix data,
            Rcpp::ExpressionVector distanceFunctions) {
  
  int numObjects = data.ncol();
  int totalVars = data.nrow();

  //Rcpp::NumericVector offsets(numLayers);
  Rcpp::NumericVector distances((numObjects * (numObjects - 1)) / 2);

  double *pDistances = REAL(distances);

  /* Get the distance function pointers. */
  std::vector<DistanceFunctionPtr> distanceFunctionPtrs =
    GetDistanceFunctions(distanceFunctions);

  int ix = 0;
  for (int i = 0; i < numObjects - 1; ++i) {
    for (int j = i + 1; j < numObjects; ++j) {
      pDistances[ix] = 0.0;
     
      pDistances[ix] += (*distanceFunctionPtrs[0])(
        &data[i * totalVars],
        &data[j * totalVars],
        totalVars);
      
      ix++;
    }
  }
  
  return distances;
}

/*
 * Finds the best matching codebook unit for the given data object and stores
 * its index and distance in the specified nearest unit index and nearest unit
 * distance references.
 */
void FindBestMatchingUnit(
  double *object,
  double *codes,
  int numCodes,
  int totalVars,
  const std::vector<DistanceFunctionPtr> &distanceFunctions,
  int &index,
  double &distance) {
  int nind = 1;
  double dist;

  index = NA_INTEGER;
  distance = DBL_MAX;
  for (int cd = 0; cd < numCodes; ++cd) {

    /* Calculate current unit distance */
    dist = 0.0;
 
    dist += (*distanceFunctions[0])(
      &object[0],
      &codes[cd * totalVars],
      totalVars);
    

    /* Update best matching unit */
    if (dist <= distance * (1 + EPS)) {
      if (dist < distance * (1 - EPS)) {
        nind = 1;
        index = cd;
      } else {
        if (++nind * UNIF < 1.0) {
          index = cd;
        }
      }
      distance = dist;
    }
  }
  
  if (distance == DBL_MAX) {
    distance = NA_REAL;
    index = NA_INTEGER;
  }
}


/*
 * Returns a function pointer to compute the euclidean distance between a data
 * vector and a codebook vector.
 */
double EuclideanDistance(double *data, double *codes, int n) {
  double tmp, d = 0.0;
  int nNA = n;
  for (int i = 0; i < n; ++i) {
    if (!std::isnan(data[i])) {
      tmp = data[i] - codes[i];
      d += tmp * tmp;
      nNA = nNA-1;
    }
  }
  if(nNA!=n){
    d *= n / (double)(n - nNA);
  }
  d = sqrt(d);
  return d;
}


/*
 * Returns a function pointer to compute the distance as the the sum of squared
 * differences between a data vector and a codebook vector.
 */
double SumOfSquaresDistance(double *data, double *codes, int n) {
  double tmp, d = 0.0;
  int nNA = n; 
  for (int i = 0; i < n; ++i) {
    if (!std::isnan(data[i])) {
      tmp = data[i] - codes[i];
      d += tmp * tmp;
      nNA = nNA-1;
    }
  }
  if(nNA!=n){
    d *= n / (double)(n - nNA);
  }
  return d;
}


/*
 * Returns a function pointer to compute the tanimoto distance between a data
 * vector and a codebook vector.
 */
double TanimotoDistance(double *data, double *codes, int n) {
  double d = 0.0;
  for (int i = 0; i < n; ++i) {
    if ((data[i] > .5 && codes[i] < .5)
      || (data[i] <= .5 && codes[i] >= .5)) {
      d += 1.0;
    }
  }
  return d / n;
}


/*
 * Returns a function pointer to compute the Manhattan or taxicab distance
 * between a data vector and a codebook vector.
 */
double ManhattanDistance(double *data, double *codes, int n) {
  double d = 0.0;
  int nNA = n;
  for (int i = 0; i < n; ++i) {
    if (!std::isnan(data[i])) {
      d += fabs(data[i] - codes[i]);
    }
  }
  if(nNA!=n){
    d *= n / (double)(n - nNA);
  }
  return d;
}
 
