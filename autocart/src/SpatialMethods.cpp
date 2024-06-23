/*
 * This file contains various helper methods for spatial data as to not
 * clutter up the AutoTree.cpp file too much.
 *
 * @author Ethan Ancell
 */
#include <RcppArmadillo.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppParallel)]]

#include <math.h>
#include "SpatialMethods.h"

using namespace Rcpp;
using namespace RcppParallel;

/* Simple euclidean distance implementation. */
double euclidDistance(double x1, double y1, double x2, double y2) {
  return sqrt(pow(x2-x1, 2) + pow(y2-y1, 2));
}

/* Given a NumericMatrix of point locations, calculate a weights matrix
 * using a simple inverse distance formula. We assume that the "x" part of the
 * location is the first column of the matrix and the "y" part of the location
 * is in the second column of the matrix. For this to properly work, we should
 * assume that the locations matrix contains points that have already been
 * projected.
 */
NumericMatrix getInvWeights(NumericMatrix locations, bool islonglat, int power) {
  // First get a matrix with the distances from all points to all other points
  int matrixSize = locations.rows();
  NumericMatrix invDist(matrixSize, matrixSize);

  for (int i=0; i<matrixSize; i++) {
    double x1 = locations(i, 0);
    double y1 = locations(i, 1);
    for (int j=0; j<matrixSize; j++) {
      double x2 = locations(j, 0);
      double y2 = locations(j, 1);
      invDist(i, j) = euclidDistance(x1, y1, x2, y2);

      // Add optional power
      if (power != 1) {
        invDist(i, j) = pow(invDist(i, j), power);
      }

      // Avoid a divide by zero error by only inverting when i != j.
      if (i != j) {
        invDist(i, j) = 1.0 / invDist(i, j);
      }
    }
  }

  return invDist;
}

double moranIVariance(NumericVector response, NumericMatrix weights) {
  // Check that the input is valid
  if (weights.rows() != weights.cols()) {
    stop("Weights matrix supplied to moranI function is not a square matrix.");
  }
  if (response.size() != weights.cols()) {
    Rcout << "Response length: " << response.size() << ", weights matrix size: " << weights.nrow() << std::endl;
    stop("In moranI function, the response vector length is not the same as the matrix.");
  }

  int nn = response.size();
  double n = (double) nn;

  // ybar
  double ybar = 0.0;
  for (int i=0; i<n; i++) {
    ybar += response[i];
  }
  ybar /= n;

  // Z vector (deviations of response from mean)
  std::vector<double> z;
  for (int i=0; i<n; i++) {
    z.push_back(response[i] - ybar);
  }

  // S0 (sum of all weights)
  double s0 = 0.0;
  for (int i=0; i<nn; i++) {
    for (int j=0; j<nn; j++) {
      s0 += weights(i, j);
    }
  }

  // S1
  double s1 = 0.0;
  for (int i=0; i<nn; i++) {
    for (int j=0; j<nn; j++) {
      s1 += pow(weights(i, j) + weights(j, i), 2.0);
    }
  }
  s1 /= 2.0;

  // S2
  double s2 = 0.0;
  for (int i=0; i<nn; i++) {
    double sumColumn1 = 0.0;
    for (int j=0; j<nn; j++) {
      sumColumn1 += weights(i, j);
    }

    double sumColumn2 = 0.0;
    for (int j=0; j<nn; j++) {
      sumColumn2 += weights(j, i);
    }

    s2 += pow(sumColumn1 + sumColumn2, 2.0);
  }

  // A
  double a = n * ((pow(n, 2) - 3*n + 3)*s1 - n*s2 + 3*pow(s0, 2.0));

  // D
  double d_num = 0.0;
  for (int i=0; i<n; i++) {
    d_num += pow(z[i], 4.0);
  }
  double d_denom = 0.0;
  for (int i=0; i<n; i++) {
    d_denom += pow(z[i], 2.0);
  }
  d_denom = pow(d_denom, 2.0);
  double d = d_num / d_denom;

  // B
  double b = d * ((pow(n, 2.0) - n) * s1 - 2*n*s2 + 6*pow(s0, 2.0));

  // C
  double c = (n-1.0) * (n - 2.0) * (n - 3.0) * pow(s0, 2.0);

  // E[I^2]
  double eIsquared = (a - b) / c;

  // E[I]^2
  double eIQuantSquared = -1 / (n - 1);
  eIQuantSquared = pow(eIQuantSquared, 2.0);

  return eIsquared - eIQuantSquared;
}

/* Calculate Moran's I statistic on the data supplied. We assume that a weights
 * matrix has already been supplied by the user. The "getInvWeights" function
 * can create a default inverse weight matrix for you.
 *
 * In the implementation of this function, we will assume that there is no
 * weight between an observation and itself. (i.e. assume that the diagonal
 * of the weights matrix is filled with entries of zero)
 */
double moranI(NumericVector response, NumericMatrix weights) {
  // Check that the input is valid
  if (weights.rows() != weights.cols()) {
    stop("Weights matrix supplied to moranI function is not a square matrix.");
  }
  if (response.size() != weights.cols()) {
    Rcout << "Response length: " << response.size() << ", weights matrix size: " << weights.nrow() << std::endl;
    stop("In moranI function, the response vector length is not the same as the matrix.");
  }

  int nObs = response.size();

  double responseMean = 0;
  for (int i=0; i<nObs; i++) {
    responseMean += response[i];
  }
  responseMean = responseMean / nObs;

  /* If we assume that the weights matrix is symmetrical, then you can double
   * the speed of calculations of Moran's I by doubling the sum of the values on the
   * upper diagonal of the matrix
   */
  // Numerator calculations
  double numerator = 0;
  for (int i=0; i<nObs; i++) {
    for (int j=0; j<nObs; j++) {
      numerator += weights(i, j) * (response[i] - responseMean) * (response[j] - responseMean);
    }
  }
  numerator *= nObs;

  // Denominator calculations
  double sumWeights = 0;
  for (int i=0; i<nObs; i++) {
    for (int j=0; j<nObs; j++) {
      sumWeights += weights(i, j);
    }
  }
  double denominator = 0;
  for (int i=0; i<nObs; i++) {
    denominator += pow(response[i] - responseMean, 2);
  }
  denominator *= sumWeights;

  return numerator / denominator;
}

/* Another alternative measure of spatial autocorrelation that is more sensitive to local spatial autocorrelation rather than
 * global spatial autocorrelation.
 */
double gearyC(NumericVector response, NumericMatrix weights) {
  // Check that the input is valid
  if (weights.rows() != weights.cols()) {
    stop("Weights matrix supplied to moranI function is not a square matrix.");
  }
  if (response.size() != weights.cols()) {
    stop("In moranI function, the response vector length is not the same as the matrix.");
  }

  int nObs = response.size();

  double responseMean = 0;
  for (int i=0; i<nObs; i++) {
    responseMean += response[i];
  }
  responseMean = responseMean / nObs;

  // SUMWEIGHTS
  double sumWeights = 0.0;
  for (int i=0; i<nObs; i++) {
    for (int j=0; j<nObs; j++) {
      sumWeights += weights(i, j);
    }
  }

  // NUMERATOR
  double numerator = 0.0;

  for (int i=0; i<nObs; i++) {
    for (int j=0; j<nObs; j++) {
      numerator += weights(i, j) * pow((response[i] - response[j]), 2);
    }
  }
  numerator *= ((double) (nObs - 1));

  // DENOMINATOR
  double denominator = 0.0;

  for (int i=0; i<nObs; i++) {
    denominator += pow(response[i] - responseMean, 2);
  }
  denominator *= (2 * sumWeights);

  return numerator / denominator;
}

// Helper function to compare two NumericVectors. The default "==" operator
// returns a logical vector that compares the two vectors elementwise, which is different
// than this function.
bool compareNumericVector(NumericVector v1, NumericVector v2) {
  int n = v1.size();
  if (v2.size() != n) {
    stop("in \"compareNumericVector\", the two vectors are not of the same size.");
  }

  for (int i=0; i<n; i++) {
    if (v1[i] != v2[i]) {
      return(false);
    }
  }
  return(true);
}

// Convex hull algorithms

/* Use the "gift wrapping" or "Jarvis march" algorithm to calculate
 * the convex hull from a cloud of points given in "locations".
 */
List jarvisConvexHull(NumericMatrix locations) {
  // S = locations
  // P = returnMatrix

  List P;

  int n = locations.nrow();

  // Find a point guaranteed to be in the convex hull (i.e. leftmost location)
  double leftmostX = locations(0, 0);
  NumericVector pointOnHull = locations(0, _);
  for (int i=0; i<n; i++) {
    if (locations(i, 0) < leftmostX) {
      leftmostX = locations(i, 0);
      pointOnHull = locations(i, _);
    }
  }

  // i is the iterator through all the locations
  int i = 0;
  NumericVector endpoint;
  do {
    P.push_back(pointOnHull);
    endpoint = locations(0, _);

    for (int j=0; j<n; j++) {
      // Find out if S[j] is on the left of the line from P[i] to endpoint
      // Ax: pointOnHull[0]
      // Ay: pointOnHull[1]
      // Bx: endpoint[0]
      // By: endpoint[1]
      // X: Sj[0]
      // Y: Sj[1]
      NumericVector Sj = locations(j, _);
      double result = ((endpoint[0] - pointOnHull[0]) * (Sj[1] - pointOnHull[1]) - (endpoint[1] - pointOnHull[1]) * (Sj[0] - pointOnHull[0]));
      bool isLeft = result > 0;
      if ((compareNumericVector(endpoint, pointOnHull)) || isLeft) {
        endpoint = Sj;
      }
    }

    i++;
    pointOnHull = clone(endpoint);
  }
  while (!compareNumericVector(endpoint, P[0]));

  return(P);
}

/* Given a list that represents the points of a convex hull, calculate the area of the polygon
 * that it forms.
 */
double getAreaOfConvexHull(List convexHull) {
  double sum = 0.0;
  int n = convexHull.size();
  for (int i=0; i<n; i++) {
    NumericVector one = convexHull[i];
    NumericVector two = convexHull[(i+1) % n];
    sum += (one[0]*two[1] - one[1]*two[0]);
  }
  sum = fabs(sum/2.0);

  return sum;
}


// =====================================
// ===== PARALLELIZED CALCULATIONS =====
// =====================================

// Worker function to add up all weights in a weights matrix
struct PSumWeights : public Worker
{
  // source matrix
  const RMatrix<double> weights;

  // destination
  double sumWeights;

  // constructors
  PSumWeights(const NumericMatrix weights) : weights(weights), sumWeights(0) {}
  PSumWeights(const PSumWeights& pSumWeights, Split) : weights(pSumWeights.weights), sumWeights(0) {}

  void operator()(std::size_t begin, std::size_t end) {
    sumWeights += std::accumulate(weights.begin() + begin, weights.begin() + end, 0.0);
  }

  // Join value with other sum
  void join(const PSumWeights& rhs) {
    sumWeights += rhs.sumWeights;
  }
};

// Worker function to add up response
struct PSumVector : public Worker
{
  const RVector<double> input;
  double value;
  PSumVector(const NumericVector input) : input(input), value(0) {}
  PSumVector(const PSumVector& sum, Split) : input(sum.input), value(0) {}

  void operator()(std::size_t begin, std::size_t end) {
    value += std::accumulate(input.begin() + begin, input.begin() + end, 0.0);
  }
  void join(const PSumVector& rhs) {
    value += rhs.value;
  }
};

// Worker function to get numerator of Moran I
struct NumMI : public Worker
{
  // input
  const RVector<double> y;
  const double yBar;
  const RMatrix<double> w;
  const std::size_t n;

  // output
  double num;

  NumMI(const NumericVector y, const double yBar, const NumericMatrix w, const std::size_t n)
   : y(y), yBar(yBar), w(w), n(n), num(0) {}
  NumMI(const NumMI& sum, Split)
   : y(sum.y), yBar(sum.yBar), w(sum.w), n(sum.n), num(0) {}

  void operator()(std::size_t begin, std::size_t end) {
    // Work for the rows from [begin, end]
    for (std::size_t i = begin; i<end; i++) {
      double iOffset = y[i] - yBar;
      for (std::size_t j = 0; j<n; j++) {
        double jOffset = y[j] - yBar;
        num += w(i, j) * iOffset * jOffset;
      }
    }
  }

  void join(const NumMI& rhs) {
    num += rhs.num;
  }
};

// Worker function to get denominator of Moran I
struct DenMI : public Worker
{
  // input
  const RVector<double> y;
  const double yBar;

  // output
  double den;

  DenMI(const NumericVector y, const double yBar) : y(y), yBar(yBar), den(0) {}
  DenMI(const DenMI& sum, Split) : y(sum.y), yBar(sum.yBar), den(0) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      double offset = y[i] - yBar;
      den += offset * offset;
    }
  }

  void join(const DenMI& rhs) {
    den += rhs.den;
  }
};

double moranIParallel(NumericVector response, NumericMatrix weights) {

  int n = response.size();
  double nn = (double) n;

  // Get the sum of weights
  PSumWeights pSumWeights(weights);
  parallelReduce(0, weights.length(), pSumWeights);
  double sumWeights = pSumWeights.sumWeights;

  // Get the average response
  PSumVector pSumVector(response);
  parallelReduce(0, n, pSumVector);
  double avgResponse = pSumVector.value / nn;

  // Get numerator
  NumMI numMI(response, avgResponse, weights, n);
  parallelReduce(0, n, numMI);
  double num = numMI.num;

  // Get denominator
  DenMI denMI(response, avgResponse);
  parallelReduce(0, n, denMI);
  double den = denMI.den;

  return (nn / sumWeights) * (num / den);
}
