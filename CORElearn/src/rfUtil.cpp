#include <cstdlib>

#include "general.h"
#include "utils.h"
#include "error.h"
#include "contain.h"
#include "rfUtil.h"

using namespace std ;


// get bootstrap replicates for random forests
void bootstrapSample(int size, marray<int> &data, marray<int> &ib, marray<booleanT> &oob, marray<int> &oobIdx) {
	ib.create(size);
 	oob.create(size, mTRUE) ;
	oobIdx.create(size) ;
    int i, sel ;
    // prepare data for the bag
	for (i = 0 ; i < size ; i++) {
       sel = randBetween(0, size) ;
	   ib[i] = data[sel] ;
	   oob[sel] = mFALSE ;
	}
	for (i = 0 ; i < size ; i++) 
		if (oob[i])
			oobIdx.addEnd(data[i]) ;
}

// get random samples for random forests
void randomSample(int size, double prop, marray<int> &data, marray<int> &ib, marray<booleanT> &oob, marray<int> &oobIdx) {
    int ibSize = int(prop * size) ;
	ib.create(ibSize);
 	oob.create(size, mTRUE) ;
	oobIdx.create(size) ;
	marray<int> selector(size) ;
    int i, sel, upper=size ;
    for (i=0 ; i < size ; i++)
    	selector[i] = i ;
    // prepare data for the bag
	for (i = 0 ; i < ibSize ; i++) {
	   sel = randBetween(0, upper) ;
	   ib[i] = data[selector[sel]] ;
	   oob[selector[sel]] = mFALSE ;
	   -- upper ;
	   selector[sel] = selector[upper] ;
	}
	for (i = 0 ; i < size ; i++) 
		if (oob[i])
			oobIdx.addEnd(data[i]) ;
}

// probabilisticaly shuffles the values of valArray, so that every value is changed and 
//the distribution of values is approximately the same 
void shuffleChange(int noValues, marray<int> &valArray) {
   marray<int> distr(noValues+1, 0) ;
   int i, j, value ;
   for (i=0 ; i < valArray.len() ; i++) 
	   ++ distr[valArray[i]] ;
   // change to cumulative distribution
   distr[0] = 0 ;
   for (j=1 ; j <= noValues ; j++) 
	   distr[j] += distr[j-1] ;
   int all = distr[noValues] ;
   for (i=0 ; i < valArray.len() ; i++) {
	   do {
		   value = randBetween(0, all) ;
		   j=1 ;
		   while (value > distr[j])
              j++ ;
		   value = j ;
	   } while (value == valArray[i]) ;
	   valArray[i] = value ;
   }
}
