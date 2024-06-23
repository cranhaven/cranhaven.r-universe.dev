/*
 * This file contains all the code that generates the "goodness" values of each
 * split. This code was originally contained in the AutoTree.cpp class, but it
 * quickly got very large, so I have moved all the splitting functions into this file.
 *
 * @author Ethan Ancell
 */

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <math.h>
#include "AutoTree.h"
#include "SpatialMethods.h"
#include "SplittingMethods.h"

using namespace Rcpp;
using namespace arma;

// ======================================
// ========== HELPER FUNCTIONS ==========
// ======================================

/**
 * In many cases we wish to retrieve our weights matrix, but we calculate the weights according to different methodologies.
 * This function will select the appropriate weights matrix function and return that.
 * @param locations A matrix of coordinates
 * @param distpower the power to use on distance if using default weights
 * @param islonglat Use great circle distance or not
 * @param spatialBandwidth the maximum distance that we consider there to be any spatial influence
 * @param spatialWeightsType the type of weights matrix calculation that we perform.
 */
NumericMatrix getWeightsMatrix(NumericMatrix locations, int distpower, bool islonglat, double spatialBandwidth, SpatialWeights::Type spatialWeightsType, bool useParallelCalculations) {
  NumericMatrix myWeights;

  switch (spatialWeightsType) {
  case SpatialWeights::Regular:
  {
    myWeights = getDefaultWeightsMatrix(locations, distpower, islonglat, spatialBandwidth, useParallelCalculations);
    break;
  }
  case SpatialWeights::Gaussian:
  {
    myWeights = getGaussianWeightsMatrix(locations, islonglat, spatialBandwidth, useParallelCalculations);
    break;
  }
  default:
  {
    stop("In \"getWeightsMatrix\" method, trying to use a weighting type that is not registered.");
  }
  }

  return myWeights;
}

/**
 * This helper function will create a weights matrix for use with the Moran I
 * function or otherwise. It takes a matrix of locations, a power to use with
 * distance (i.e. inverse distance squared), and whether it is longitude and
 * latitude coordinates (use Great circle distance or not?). It will return
 * an inverse distance based weights matrix.
 * @param locations The matrix of coordaintes used in the spatial weighting
 * @param islonglat A boolean indicating if the coordiantes are longitude/latitude
 * @param distpower The power to use in distance, such as distance squared.
 * @param spatialBandwidth Past this distance, we assume zero spatial influence and set it to 0
 * @return A spatial weights matrix that uses inverse distance.
 */
NumericMatrix getDefaultWeightsMatrix(NumericMatrix locations, int distpower, bool islonglat, double spatialBandwidth, bool useParallelCalculations) {
  int n = locations.nrow();
  NumericMatrix weights;
  if (islonglat) {
    Function greatCircleDistance("rdist.earth");
    weights = greatCircleDistance(locations);
  }
  else {
    Function euclidDistMatrix("rdist");
    weights = euclidDistMatrix(locations);
  }
  for (int i=0; i<n; i++) {
    for (int j=0; j<n; j++) {
      if (weights(i, j) < spatialBandwidth) {
        // Apply the power then flip if on a non-diagonal
        if (distpower != 1) {
          weights(i, j) = pow(weights(i, j), distpower);
        }
        if (i != j) {
          weights(i, j) = 1.0 / weights(i, j);
        }
      }
      else {
        weights(i, j) = 0.0;
      }
    }
  }

  return weights;
}

/**
 * Return a spatial weights matrix that uses Gaussian weighting and a bandwidth.
 * @param locations The matrix of coordinates used in the spatial weighting
 * @param islonglat A boolean indicating if the coordinates in "locations" are longitude and latitude coordinates to be used with great circle distance
 * @param spatialBandwidth A double with the maximum distance where a spatial effect is said to occur
 * @return A spatial weights matrix that uses Gaussian weighting and a bandwidth.
 */
NumericMatrix getGaussianWeightsMatrix(NumericMatrix locations, bool islonglat, double spatialBandwidth, bool useParallelCalculations) {
  int n = locations.nrow();
  NumericMatrix weights;
  if (islonglat) {
    Function gcd("rdist.earth");
    weights = gcd(locations);
  }
  else {
    Function ed("rdist");
    weights = ed(locations);
  }
  for (int i=0; i<n; i++) {
    for (int j=0; j<n; j++) {
      if (weights(i, j) < spatialBandwidth) {
        double thisDistance = weights(i, j);
        weights(i, j) = exp( (-1 * pow(thisDistance, 2.0)) / (pow(spatialBandwidth, 2.0)) );
      }
      else {
        // We assume no influence past the bandwidth
        weights(i, j) = 0.0;
      }
    }
  }

  return weights;
}

// ======================================
// ======== CONTINUOUS VARIABLES ========
// ======================================

/* Use the reduction in variance to evaluate goodness for a continuous split.
 * Returns a vector ordered by x that evaluates the split from 1:i vs i+1:n
 */
NumericVector continuousGoodnessByVariance(NumericVector response, NumericVector x_vector, NumericVector wt, int minbucket, bool useParallelCalculations) {
  // Make copies as to not modify the original vectors
  NumericVector y = clone(response);
  NumericVector x = clone(x_vector);
  int n = y.size();

  // Center y at zero to make calculations simpler
  y = y - sum(y*wt) / sum(wt);

  // Calculate reduction in variance
  NumericVector temp = cumsum(y);
  temp = temp[Rcpp::Range(0, n-2)];
  NumericVector leftWt = cumsum(wt);
  leftWt = leftWt[Rcpp::Range(0, n-2)];
  NumericVector rightWt = sum(wt) - leftWt;

  NumericVector lMean = temp / leftWt;
  NumericVector rMean = -temp / rightWt;

  NumericVector goodness = (leftWt*pow(lMean, 2) + rightWt*pow(rMean, 2)) / sum(wt * pow(y, 2));

  // Using the minbucket parameter, we can set the first minbucket and last minbucket number
  // of observations in goodness to be 0 so that those splits are not chosen.
  for (int i=0; i<minbucket-1; i++) {
    goodness[i] = 0;
    goodness[n-i-2] = 0;
  }

  return goodness;
}

// Calculate Moran's I statistic for each of the two halves
NumericVector continuousGoodnessByAutocorrelation(NumericVector response, NumericVector x_vector, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericVector wt, int minbucket, int distpower, bool islonglat, bool useGearyC, bool saddlepointApproximation, double spatialBandwidth, SpatialWeights::Type spatialWeightsType, bool useParallelCalculations) {

  // Order the locations matrix rows in the same order as x.
  int n = response.size();

  NumericVector goodness(n-1, 0.0);

  // Using the minbucket parameter, we can only calculate the splits which start at
  // "minbucket-1", and then only calculate up to "n-minbucket"
  // By leaving everything at 0 elsewhere, we guarantee those splits are never chosen.
  //for (int splitLocation = 0; splitLocation < n-1; splitLocation++) {
  for (int splitLocation = minbucket-1; splitLocation < n-minbucket; splitLocation++) {
    // Get the E1 and E2 partitions
    NumericVector y1 = response[Range(0, splitLocation)];
    NumericVector y2 = response[Range(splitLocation+1, n-1)];

    // E1
    // (Skip over splitLocation 0 because otherwise Moran's I will fail. Just
    // leave it at the default value of 0)
    if (splitLocation != 0) {
      NumericMatrix weightsE1 = spatialWeightsMatrix(Range(0, splitLocation), Range(0, splitLocation));

      // GEARY C
      if (useGearyC) {
        double gc = gearyC(y1, weightsE1);
        // Scale to [0, 1]
        gc = (2.0 - gc) / 2.0;
        goodness[splitLocation] = gc * (splitLocation + 1);
      }
      // MORAN I
      else {
        double mi;
        if (useParallelCalculations) {
          mi = moranIParallel(y1, weightsE1);
        }
        else {
          mi = moranI(y1, weightsE1);
        }

        /*
         // Optional saddlepoint approximation to Moran I
         if (saddlepointApproximation) {
         Function sdpAprox("saddlepointMoranI");
         NumericVector miResult = sdpAprox(y1, weightsE1);
         mi = miResult[0];
         }
         else {
         mi = moranI(y1, weightsE1);
         }
         */

        // Scale so that it fits between 0 and 1
        mi = (mi + 1.0) / 2.0;
        goodness[splitLocation] = mi * (splitLocation + 1);
      }
    }

    // E2
    // (As in E2, skip over splitLocation == n-2 where only one observation exists)
    if (splitLocation != n-2) {
      NumericMatrix weightsE2 = spatialWeightsMatrix(Range(splitLocation+1, n-1), Range(splitLocation+1, n-1));

      // GEARY C
      if (useGearyC) {
        double gc = gearyC(y2, weightsE2);
        // Scale to [0, 1]
        gc = (2.0 - gc) / 2.0;
        goodness[splitLocation] = gc * (splitLocation + 1);
      }
      // MORAN I
      else {
        double mi;
        if (useParallelCalculations) {
          mi = moranIParallel(y2, weightsE2);
        }
        else {
          mi = moranI(y2, weightsE2);
        }

        /*
         // Optional saddlepoint approximation to Moran I
         if (saddlepointApproximation) {
         Function sdpAprox("saddlepointMoranI");
         NumericVector miResult = sdpAprox(y2, weightsE2);
         mi = miResult[0];
         }
         else {
         mi = moranI(y2, weightsE2);
         }
         */

        // Scale to [0, 1]
        mi = (mi + 1.0) / 2.0;
        goodness[splitLocation] += (mi * (n - splitLocation - 1));
      }
    }
    goodness[splitLocation] /= n;
  }

  return goodness;
}


// Use the size of the regions to encourage grouped up observations
NumericVector continuousGoodnessBySize(NumericVector x_vector, NumericMatrix locations, NumericMatrix distanceMatrix, NumericVector wt, int minbucket, bool islonglat, bool useParallelCalculations) {

  int n = x_vector.size();
  NumericVector goodness(x_vector.size()-1, 0.0);

  // Total sum of squares (denominator in ultimate goodness value)
  // start j at i, as counting the other triangular half of the matrix
  // would be double counting all distances
  double TSS = 0.0;
  for (int i=0; i<n; i++) {
    for (int j=i; j<n; j++) {
      //TSS += pow(allDistances(i, j), 2);
      TSS += pow(distanceMatrix(i, j), 2);
    }
  }

  // Optimization to put in:
  // Calculate the corner of the matrix sum of distances squared in the beginning
  // and then inside this next for loop, just access the vector above at the right
  // spot.

  for (int splitLocation = minbucket-1; splitLocation < n-minbucket; splitLocation++) {
    NumericMatrix distancesAcross = distanceMatrix(Range(0, splitLocation), Range(splitLocation+1, n-1));

    // Using the identity that sum(pairwise(A->B)) + sum(pairwise(A->A)) + sum(pairwise(B->B)) = sum(pairwise(all))
    // High goodness = high values is good splits. To minimize the pairwise differences within regions,
    // then we can just use sum(pairwise(A->B)) over TSS as the goodness
    double BSS = 0.0;
    // For every observation between i and j, get the squared distance
    for (int i=0; i<distancesAcross.nrow(); i++) {
      for (int j=0; j<distancesAcross.ncol(); j++) {
        BSS += pow(distancesAcross(i, j), 2);
      }
    }

    goodness[splitLocation] = BSS / TSS;
  }

  return goodness;
}


// ========================================
// ======== CATEGORICAL VARIABLES =========
// ========================================

// Reduction in variance
NumericVector categoricalGoodnessByVariance(NumericVector response, IntegerVector x_vector, NumericVector wt, int minbucket, bool useParallelCalculations) {

  NumericVector y = clone(response);
  IntegerVector x = clone(x_vector);
  int n = y.size();

  // Center y at zero to make calculations simpler
  y = y - sum(y*wt) / sum(wt);

  CharacterVector lvls = x.attr("levels");
  int numLevels = lvls.size();

  // wtSum = {apple: 2, orange: 4, pineapple: 3}
  NumericVector wtSum(numLevels);
  NumericVector ySum(numLevels);
  for (int i=0; i<n; i++) {
    wtSum[x[i] - 1] += wt[i];
    ySum[x[i] - 1] += (wt[i] * y[i]);
  }
  NumericVector means = ySum / wtSum;

  NumericVector goodness(numLevels, 0.0);

  // For each factor level, group observations into left (not that factor)
  // and right (that factor), then calculate the goodness for each of those
  // splits. (Calculated with SSB / TSS)
  for (int i=0; i<numLevels; i++) {
    // Only calculate a number for t1 if the number of items with that factor
    // is at least as big as minbucket
    if (wtSum[i] >= minbucket) {
      // Calculate the mean of the non-factor group
      double nonFactorMean = 0.0;
      double totalNonFactorWeights = 0.0;
      for (int j=0; j<numLevels; j++) {
        if (j != i) {
          totalNonFactorWeights += wtSum[j];
          nonFactorMean += wtSum[j] * means[j];
        }
      }
      nonFactorMean /= totalNonFactorWeights;
      goodness[i] = (totalNonFactorWeights * pow(nonFactorMean, 2)) + (wtSum[i] * pow(means[i], 2));
      goodness[i] /= sum(wt * pow(y, 2));
    }
  }

  return goodness;
}

// Spatial autocorrelation splitting
NumericVector categoricalGoodnessByAutocorrelation(NumericVector response, IntegerVector x_vector, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericVector wt, int minbucket, int distpower, bool islonglat, bool useGearyC, bool saddlepointApproximation, double spatialBandwidth, SpatialWeights::Type spatialWeightsType, bool useParallelCalculations) {

  // Useful information that will be used by splitting
  CharacterVector lvls = x_vector.attr("levels");
  int numLevels = lvls.size();
  int n = response.size();

  NumericVector goodness(numLevels, 0.0);

  // This weights matrix will be used in MoranI
  Function greatCircleDistance("rdist.earth");
  Function euclidDistMatrix("rdist");
  NumericMatrix allWeights;
  if (islonglat) {
    allWeights = greatCircleDistance(locations);
  }
  else {
    allWeights = euclidDistMatrix(locations);
  }

  // wtSum = {apple: 2, orange: 4, pineapple: 3}
  NumericVector wtSum(numLevels);
  NumericVector ySum(numLevels);
  for (int i=0; i<n; i++) {
    wtSum[x_vector[i] - 1] += wt[i];
    ySum[x_vector[i] - 1] += (wt[i] * response[i]);
  }
  NumericVector means = ySum / wtSum;

  for (int factorLevel = 0; factorLevel < numLevels; factorLevel++) {
    // Only calculate this if the number of observations in the factor level
    // is bigger than minbucket
    if (wtSum[factorLevel] >= minbucket) {
      // Create E1 and E2 partitions by using the indices of the factor levels
      LogicalVector factorIndices = (x_vector == (factorLevel+1));

      // Create the e1 and e2 Numeric matrices, as the subsetting with factorIndices does
      // not work at all since it is a logical vector..... :(
      NumericMatrix e1(wtSum[factorLevel], 2);
      NumericMatrix e2(n - wtSum[factorLevel], 2);
      int e1n = 0;
      int e2n = 0;
      for (int i=0; i<n; i++) {
        if (factorIndices[i]) {
          e1(e1n, _) = locations(i, _);
          e1n++;
        }
        else {
          e2(e2n, _) = locations(i, _);
          e2n++;
        }
      }
      NumericVector y1 = response[factorIndices];
      NumericVector y2 = response[!factorIndices];

      // INFO:
      // wtSum[factorLevel] = the number of observations in this factor
      // (n - wtSum[factorLevel]) = the number of observations not in the factor

      // E1
      // Skip if only one observation with this factor
      if (wtSum[factorLevel] > 1.0) {
        NumericMatrix weightsE1 = getWeightsMatrix(e1, distpower, islonglat, spatialBandwidth, spatialWeightsType, useParallelCalculations);

        // GEARY C
        if (useGearyC) {
          double gc = gearyC(y1, weightsE1);
          // Scale to [0, 1]
          gc = (2.0 - gc) / 2.0;
          goodness[factorLevel] = gc * (wtSum[factorLevel]);
        }
        // MORAN I
        else {
          double mi;
          if (useParallelCalculations) {
            mi = moranIParallel(y1, weightsE1);
          }
          else {
            mi = moranI(y1, weightsE1);
          }
          // Scale to [0, 1]
          mi = (mi + 1.0) / 2.0;
          goodness[factorLevel] = mi * (wtSum[factorLevel]);
        }
      }

      // E2
      if ((n - wtSum[factorLevel]) > 1.0) {
        NumericMatrix weightsE2 = getWeightsMatrix(e2, distpower, islonglat, spatialBandwidth, spatialWeightsType, useParallelCalculations);

        // GEARY C
        if (useGearyC) {
          double gc = gearyC(y2, weightsE2);
          // Scale to [0, 1]
          gc = (2.0 - gc) / 2.0;
          goodness[factorLevel] += (gc * (n - wtSum[factorLevel]));
        }
        // MORAN I
        else {
          double mi;
          if (useParallelCalculations) {
            mi = moranIParallel(y2, weightsE2);
          }
          else {
            mi = moranI(y2, weightsE2);
          }
          // Scale to [0, 1]
          mi = (mi + 1.0) / 2.0;
          goodness[factorLevel] += (mi * (n - wtSum[factorLevel]));
        }
      }

      goodness[factorLevel] /= n;
    }
  }

  return goodness;
}

// Splitting by the shape of the regions
NumericVector categoricalGoodnessBySize(IntegerVector x_vector, NumericMatrix locations, NumericMatrix distanceMatrix, NumericVector wt, int minbucket, bool islonglat, bool useParallelCalculations) {
  // Find size
  CharacterVector lvls = x_vector.attr("levels");
  int numLevels = lvls.size();
  int n = x_vector.size();

  NumericVector goodness(numLevels, 0.0);

  // wtSum is the number of observations in each of the factor levels
  NumericVector wtSum(numLevels);
  for (int i=0; i<n; i++) {
    wtSum[x_vector[i] - 1] += wt[i];
  }

  // Get distance matrix according to whether this is longlat or projected
  Function greatCircleDistance("rdist.earth");
  Function euclidDistMatrix("rdist");
  NumericMatrix allDistances;
  if (islonglat) {
    allDistances = greatCircleDistance(locations);
  }
  else {
    allDistances = euclidDistMatrix(locations);
  }

  // Calculate Total Sum of Squares (denominator in goodness value)
  double TSS = 0.0;
  for (int i=0; i<n; i++) {
    for (int j=0; j<n; j++) {
      TSS += pow(allDistances(i, j), 2);
    }
  }

  for (int factorLevel = 0; factorLevel < numLevels; factorLevel++) {
    // Only calculate if num of obs in this factor is more than minbucket
    if (wtSum[factorLevel] >= minbucket) {
      // Create E1 and E2 partitions by using the indices of the factor levels
      LogicalVector factorIndices = (x_vector == (factorLevel+1));

      // Create the e1 and e2 Numeric matrices, as the subsetting with factorIndices does
      // not work at all since it is a logical vector..... :(
      NumericMatrix e1Locations(wtSum[factorLevel], 2);
      NumericMatrix e2Locations(n - wtSum[factorLevel], 2);
      int e1n = 0;
      int e2n = 0;
      for (int i=0; i<n; i++) {
        if (factorIndices[i]) {
          e1Locations(e1n, _) = locations(i, _);
          e1n++;
        }
        else {
          e2Locations(e2n, _) = locations(i, _);
          e2n++;
        }
      }

      NumericMatrix betweenDist;
      if (islonglat) {
        betweenDist = greatCircleDistance(e1Locations, e2Locations);
      }
      else {
        betweenDist = euclidDistMatrix(e1Locations, e2Locations);
      }

      // Find the "between sum of squares of pairwise differences"
      // this will form the numerator of the goodness value for this split
      double BSS = 0;
      for (int i=0; i<betweenDist.nrow(); i++) {
        for (int j=0; j<betweenDist.ncol(); j++) {
          BSS += pow(betweenDist(i, j), 2);
        }
      }

      goodness[factorLevel] = BSS / TSS;
    }
  }

  return goodness;
}



// ===============================================================
// ===== UNUSED SPLITTING FUNCTIONS THAT DIDN'T MAKE THE CUT =====
// ===============================================================

/*
 NumericVector continuousGoodnessBySeparationOld(NumericMatrix locations, NumericMatrix distanceMatrix, int minbucket, bool islonglat) {

 int n = locations.nrow();
 NumericVector goodness(n-1, 0.0);

 Function greatCircleDistance("rdist.earth");
 Function euclidDistance("rdist");

 for (int splitLocation = minbucket-1; splitLocation < n-minbucket; splitLocation++) {
 // If we assume the distance matrix is already sorted by x_vector...
 NumericMatrix e1 = locations(Rcpp::Range(0, splitLocation), Rcpp::Range(0, 1));
 NumericMatrix e2 = locations(Rcpp::Range(splitLocation+1, n-1), Rcpp::Range(0, 1));

 // Find average long/lat for each half
 NumericMatrix e1X = e1(_, Rcpp::Range(0,0));
 NumericMatrix e1Y = e1(_, Rcpp::Range(1,1));
 NumericMatrix e2X = e2(_, Rcpp::Range(0,0));
 NumericMatrix e2Y = e2(_, Rcpp::Range(1,1));

 // Avg points (centroid point) will have 1 row and 2 columns
 NumericVector e1AvgV = NumericVector::create(sum(e1X) / e1X.nrow(), sum(e1Y) / e1Y.nrow());
 NumericVector e2AvgV = NumericVector::create(sum(e2X) / e2X.nrow(), sum(e2Y) / e2Y.nrow());
 NumericMatrix e1Avg(1, 2, e1AvgV.begin());
 NumericMatrix e2Avg(1, 2, e2AvgV.begin());

 // Get the distance matrix from all points in E1 and E2 to e1Avg and e2Avg
 NumericMatrix e1ToE1Avg;
 NumericMatrix e1ToE2Avg;
 NumericMatrix e2ToE1Avg;
 NumericMatrix e2ToE2Avg;

 if (islonglat) {
 e1ToE1Avg = greatCircleDistance(e1, e1Avg);
 e1ToE2Avg = greatCircleDistance(e1, e2Avg);
 e2ToE1Avg = greatCircleDistance(e2, e1Avg);
 e2ToE2Avg = greatCircleDistance(e2, e2Avg);
 }
 else {
 e1ToE1Avg = euclidDistance(e1, e1Avg);
 e1ToE2Avg = euclidDistance(e1, e2Avg);
 e2ToE1Avg = euclidDistance(e2, e1Avg);
 e2ToE2Avg = euclidDistance(e2, e2Avg);
 }

 int pointsCloserToOwnCentroid = 0;
 for (int i=0; i<e1.nrow(); i++) {
 //Rcout << e1ToE1Avg(i, 0) << "   :   " << e1ToE2Avg(i, 0) << std::endl;
 if (e1ToE1Avg(i, 0) <= e1ToE2Avg(i, 0)) {
 //Rcout << "e1ToE1Avg was less than e1ToE2Avg, so add one." << std::endl;
 pointsCloserToOwnCentroid++;
 }
 }
 for (int i=0; i<e2.nrow(); i++) {
 if (e2ToE2Avg(i, 0) <= e2ToE1Avg(i, 0)) {
 pointsCloserToOwnCentroid++;
 }
 }

 goodness[splitLocation] = ((double) pointsCloserToOwnCentroid) / ((double) n);
 }

 return goodness;
 }


 NumericVector continuousGoodnessBySeparation(const arma::mat locations, int n, int minbucket) {

 NumericVector goodness(n-1, 0.0);

 // Project the locations onto the first principal component of the locations matrix
 arma::mat coeff;
 arma::mat score;
 arma::mat latent;
 arma::vec tsquared;

 princomp(coeff, score, latent, tsquared, locations);

 arma::mat largestEigenvector = coeff.col(0);
 arma::mat projectedLocations = (locations * largestEigenvector) * trans(largestEigenvector);

 // Find the maximum and minimum point in the data to get total length of "slice" O(n)
 bool useHorizontal = true;
 // if the 1st principal component has an x component of 0, then all the
 // data gets projected vertically which means we need to use a slightly different approach
 if (largestEigenvector(0, 0) == 0) {
 Rcout << "First principal component has an x-component of 0..." << std::endl;
 useHorizontal = false;
 stop("Correctly handling a first principal component having an x-component of 0 has not been implemented yet...");
 }

 arma::mat maxEndpoint = locations.row(0);
 arma::mat minEndpoint = locations.row(0);

 // O(n)
 for (int i=0; i<n; i++) {
 if (locations(i, 0) < minEndpoint(0, 0)) {
 minEndpoint = locations.row(i);
 }
 else if (locations(i, 0) > maxEndpoint(0, 0)) {
 maxEndpoint = locations.row(i);
 }
 }

 for (int splitLocation = minbucket-1; splitLocation < n-minbucket; splitLocation++) {
 arma::mat e1 = projected_locations(span(0, splitLocation), span(0, 1));
 arma::mat e2 = projected_locations(span(splitLocation+1, n-1), span(0, 1));

 // Find the endpoints of both e1 and e2
 arma::mat maxEndpointE1 = e1.row(0);
 arma::mat maxEndpointE2 = e2.row(0);
 arma::mat minEndpointE1 = e1.row(0);
 arma::mat minEndpointE2 = e2.row(0);

 for (int i=0; i<e1.n_rows; i++) {
 if (e1(i, 0) < minEndpointE1(0, 0)) {
 minEndpointE1 = e1.row(i);
 }
 else if (e1(i, 0) > maxEndpointE1(0, )) {
 maxEndpointE1 = e1.row(i);
 }
 }
 for (int i=0; i<e2.n_rows; i++) {
 if (e2(i, 0) < minEndpointE2(0, 0)) {
 minEndpointE2 = e2.row(i);
 }
 else if (e2(i, 0) > maxEndpointE2(0, )) {
 maxEndpointE2 = e2.row(i);
 }
 }


 }

 return(goodness);
 }
 */

 // CONVEX HULL BETA FUNCTION
 // Use the area of the convex hulls around each partition geometry to optimize for compactness of regions
 NumericVector continuousGoodnessBySizeConvexHull(NumericMatrix locations, int minbucket) {

   int n = locations.nrow();
   NumericVector goodness(n-1, 0.0);

   double totalArea = 2.0 * getAreaOfConvexHull(jarvisConvexHull(locations));
   //Rcout << "totalArea: " << totalArea << std::endl;

   for (int splitLocation = minbucket-1; splitLocation < n-minbucket; splitLocation++) {

   // Create partitions
   NumericMatrix e1 = locations(Range(0, splitLocation), Rcpp::Range(0, 1));
   NumericMatrix e2 = locations(Range(splitLocation+1, n-1), Rcpp::Range(0, 1));
   // Rcout << "locations: " << std::endl;
   // Rcout << locations << std::endl;
   // Rcout << "e1: " << std::endl;
   // Rcout << e1 << std::endl;
   // Rcout << "e2: " << std::endl;
   // Rcout << e2 << std::endl;

   // The algorithm that is used here is the "Jarvis algorithm", also known as the
   // gift wrapping algorithm to find the convex hull of a set of points. We then use the
   // accompanying area function to find the area of said convex hull, and that forms our goodness.
   double areaE1 = getAreaOfConvexHull(jarvisConvexHull(e1));
   double areaE2 = getAreaOfConvexHull(jarvisConvexHull(e2));
   // Rcout << "areaE1: " << areaE1 << std::endl;
   // Rcout << "areaE2: " << areaE2 << std::endl;

   goodness[splitLocation] = 1.0 - ((areaE1 + areaE2) / totalArea);
   // Rcout << "goodness: " << goodness[splitLocation] << std::endl;
   // Rcout << "--------------" << std::endl;

   //stop("bruh2");
   }

   return goodness;
}

/*
   // A density-based beta algorithm. It might be a good idea to try both regular density as well as density compared to the global density at that
   // specific partition location. If you look at global density, you simply just need to find the number of original observations
   NumericVector continuousGoodnessByDensity(NumericMatrix locations, int minbucket) {
   int n = locations.nrow();
   NumericVector goodness(n-1, 0.0);

   double totalNodeArea = 2.0 * getAreaOfConvexHull(jarvisConvexHull(locations));

   for (int splitLocation = minbucket-1; splitLocation < n-minbucket; splitLocation++) {
   // Create partitions
   NumericMatrix e1 = locations(Range(0, splitLocation), Rcpp::Range(0, 1));
   NumericMatrix e2 = locations(Range(splitLocation+1, n-1), Rcpp::Range(0, 1));
   double areaE1 = getAreaOfConvexHull(jarvisConvexHull(e1));
   double areaE2 = getAreaOfConvexHull(jarvisConvexHull(e2));

   // Number of observations in each partition
   int numobsE1 = e1.nrow();
   int numobsE2 = e2.nrow();
   double e1Density =
   }
 }
*/
