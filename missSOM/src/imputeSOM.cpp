/* supersom2.c: supervised SOMs for data fusion, started Sept 15, 2006
 Revision, July 2016
 
 Rcpp version of supersoms
 
 Addition of handling NAs, following code in R_euclidean
 (distance.c). We ignore individual NAs and rescale the distance
 according to the number of non-NA variables. The R-function
 supersom should make sure that every object at least has one non-NA
 value for every matrix. There should be no NAs in the codebook
 vectors, nor in the unit.distances that are calculated here: if an
 object has more NAs than allowed (indicated as a fraction of the
 total number), it is removed from the data set in the pre-C R
 code. It can be mapped later using the whatmaps argument in the
 map.kohonen function. 
 NOTE: this means that our distance functions never should return NAs...
 
 Author: Ron Wehrens and Johannes Kruisselbrink
 */

/* Oct. 18 2007: copied the check for equality of distances from BDR's
 class library */


#include <Rcpp.h>
#include <Rmath.h>

#include "missSOM.h"
#include "distance-functions.h"
#include "neighbourhood-functions.h"

#define RANDIN  GetRNGstate()
#define RANDOUT PutRNGstate()
#define UNIF unif_rand()


// [[Rcpp::export]]
Rcpp::List RcppImputeSOM(
    Rcpp::NumericMatrix data,
    Rcpp::NumericMatrix missData,
    Rcpp::NumericMatrix codes,
    Rcpp::ExpressionVector distanceFunctions,
    Rcpp::NumericMatrix neighbourhoodDistances,
    int neighbourhoodFct,
    Rcpp::NumericVector alphas,
    Rcpp::NumericVector radii,
    int numEpochs,
    bool bool_impute,                                                              
    Rcpp::IntegerVector missingCol,                                               
    Rcpp::IntegerVector missingRow                                             
) 
{
  int
    numObjects = data.ncol(),     /* number of objects */
    numCodes = codes.ncol(),      /* number of units in the map */
    totalVars = data.nrow(),      /* total number of variables sum(numVars) */
    cd,                           /* counter over units */
    i,                            /* randomly drawn object */
    j,                            /* counter over variables */
    k,                            /* counter over iterations */
    m,                            /* counter over epochs */
    nearest, totalIters, curIter = 0;
  double distance, tmp, threshold, alpha;
  
  Rcpp::NumericVector changes(numEpochs);

  double
    *pCodes = REAL(codes),
      *pChanges = REAL(changes),
      *pData = REAL(data),
      *pmissData = REAL(missData),
      *pNeighbourhoodDistances = REAL(neighbourhoodDistances),
      *pObjectMiss,
      *pObject;
      

  /* Get the distance function pointers. */
  std::vector<DistanceFunctionPtr> distanceFunctionPtrs =
    GetDistanceFunctions(distanceFunctions);
  
  /* Create the neighborhood influence function pointer. */
  NeighbourhoodFunctionPtr neighbourhoodFunctionPtr =
    CreateNeighbourhoodFunction((NeighbourhoodFunctionType)neighbourhoodFct);
  
  
  totalIters = numEpochs * numObjects;
  
  RANDIN;
  
  /* Outer loop: number of iterations */
  for (m = 0; m < numEpochs; m++) {
    
    /* Inner loop: loop over (bootstrapped) objects */
    for (k = 0; k < numObjects; k++) {
      
      /* Select random object */
      i = (int)(numObjects * UNIF);
      
      /* Find best matching unit index and distance */
      pObject = &pData[i * totalVars];                                          
      pObjectMiss = &pmissData[i*totalVars];
      
      /* Find best matching unit index and distance */
      FindBestMatchingUnit(
        pObjectMiss,
        pCodes,
        numCodes,
        totalVars,
        distanceFunctionPtrs,
        nearest,
        distance);
      
      if (nearest < 0) {
        ::Rf_error("No nearest neighbour found...");
      }
      
      /* Linear decays for radius and learning parameter */
      tmp = (double)(curIter) / (double)(totalIters);
      threshold = radii[0] - (radii[0] - radii[1]) * tmp;
      
      if (threshold < 1.0) {
        threshold = 0.5;
      }
      
      alpha = alphas[0] - (alphas[0] - alphas[1]) * tmp;
      
      /* Update changes */
      
      distance = 0.0;
      for (j = 0; j < totalVars; j++) {
        if (!std::isnan(pObject[j])) {
          tmp = pObject[j] - pCodes[nearest * totalVars + j];
          distance += tmp * tmp;
        }
      }
      pChanges[m] += distance;
      
      
      /* Update vector codes */
      for (cd = 0; cd < numCodes; cd++) {
        tmp = neighbourhoodFunctionPtr(pNeighbourhoodDistances[numCodes * nearest + cd], threshold);
        if (tmp > 0) {
          for (j = 0; j < totalVars; j++) {                                                          
            if (!std::isnan(pObject[j])) {
              pCodes[cd * totalVars + j] += tmp * alpha * (pObject[j] - pCodes[cd * totalVars + j]);  
            }
          }
        }
      }
      ++curIter;
      
    }
    
    /* Update missing data */
    if (bool_impute){
      double weights_sum;
      double neighbourhood_sum;
      for(i=0; i < missingRow.size(); i++){
        pObjectMiss = &pmissData[(missingCol[i] - 1) * totalVars]; 
        FindBestMatchingUnit(
          pObjectMiss,
          pCodes,
          numCodes,
          totalVars,
          distanceFunctionPtrs,
          nearest,
          distance);
        
        if (nearest < 0) {
          ::Rf_error("No nearest neighbour found...");
        }
        
        weights_sum = 0.0;
        neighbourhood_sum = 0.0;
        double tmp_bis;
        for(cd=0; cd < numCodes; cd++){
          tmp_bis = neighbourhoodFunctionPtr(pNeighbourhoodDistances[numCodes * nearest + cd], threshold);
          weights_sum += pCodes[cd*totalVars + (missingRow[i]-1)]*tmp_bis; 
          neighbourhood_sum += tmp_bis;
        }
        pData[(missingCol[i] - 1) * totalVars + (missingRow[i] - 1)] = weights_sum / neighbourhood_sum;
      }
      
    }
    
    /* Mean of the nearest layer distances of this iteration */
    pChanges[m] =
      sqrt(pChanges[m] / totalVars) / numObjects;
    
  }
  
  RANDOUT;
  
  return Rcpp::List::create(
    Rcpp::Named("codes") = codes,
    Rcpp::Named("changes") = changes,
    Rcpp::Named("ximp") = data); 
}
