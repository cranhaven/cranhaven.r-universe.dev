#include <cstdlib>
#include <cfloat>
#include <cmath>

#include "general.h"
#include "error.h"
#include "contain.h"
#include "estimator.h"
#include "utils.h"
#include "options.h"

using namespace std ;

// ***************************************************************************
//
//                       ordAVdAeqNorm
//                       ---------
//
//     evaluation of ordered attribute-values normalized with
//     changes count  of random attribute: version used
//
// ***************************************************************************
void estimation::ordAVdAeqNorm(int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) {

   // prepare random normalization attributes
   int noBasicEstimated =  discAttrTo - discAttrFrom ;
   int noEstimated =  noBasicEstimated * eopt.ordEvalNoRandomNormalizers ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR, rn,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
	   for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
		  if (eopt.ordEvalBootstrapNormalize) {
	          DiscValues.bootstrapColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn) ;
		  }
		  else {
	          DiscValues.copyColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
			  DiscValues.shuffleColumn(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
		  }
		  prepareDiscAttr(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn, discNoValues[iA]) ;
	   }
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }
   // prepare space for raw scores of random attributes
   mmatrix<marray<double> > reinfPosRndRaw(noBasicEstimated, maxNoValues+1), reinfNegRndRaw(noBasicEstimated, maxNoValues+1), anchorRndRaw(noBasicEstimated, maxNoValues+1) ;
   // empty the results arrays and matrixes
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
	   for (iV = 0 ; iV <= discNoValues[iA] ; ++iV) {
	      reinfPosRnd(iA, iV).init(0.0) ;
     	  reinfNegRnd(iA,iV).init(0.0) ;
     	  anchorRnd(iA,iV).init(0.0) ;
    	  // prepare space for random individual evaluations (raw)
	      reinfPosRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      reinfNegRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      anchorRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	   }
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0) ;

   int i, aVal ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff ;
   int current, neighbourIdx, iAttr ;
   diffEsorted.create(TrainSize) ;
   distanceEarray.create(TrainSize) ;


   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main iterative loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       //currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      EprepareDistanceFactors(distanceType) ;


		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;

         for (i=0 ; i < distanceEarray.filled() ; i++) {
            neighbourIdx = distanceEarray[i].value ;
            normDistance = distanceEarray[i].key ;
            distanceSum += normDistance ;
  		    clDiff = DAdiffSign(0, neighbourIdx, current) ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (clDiff==0.0) { // same class
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff==1.0) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff > 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest

		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes, in case we do not estimate the whole array
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
 	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
   }
   // compute expected values
   marray<marray<double> > expReinfPos(discAttrTo), expReinfNeg(discAttrTo), expAnchor(discAttrTo);
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++){
	   expReinfPos[iA].create(maxNoValues+1,0) ;
	   expReinfNeg[iA].create(maxNoValues+1,0) ;
	   expAnchor[iA].create(maxNoValues+1,0) ;
   }
   oeExpDistr(discAttrFrom, discAttrTo, expReinfPos, expReinfNeg, expAnchor) ;

   // compute reinforcements, random normalizations and their variance
   double denom, denomR;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CnAp(iV, iA) + CeAp(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAp(iV, iR) + CnAp(iV, iR) + CeAp(iV, iR) ;
		   if (denomR > 0 && CpAp(iV, iR) > 0)  // otherwise zero
		       reinfPosRndRaw(iA-discAttrFrom,iV)[rn] = (CpAp(iV, iR) / denomR)  ;
		   else reinfPosRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfPosRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfPosRnd(iA,iV), eopt.ordEvalNormalizingPercentile,reinfPos[iA][iV]) ;
         reinfPosRnd(iA,iV)[noOEstats-1] = expReinfPos[iA][iV] ;
		 // negative reinforcement
		 denom = CpAn(iV, iA) + CnAn(iV, iA) + CeAn(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAn(iV, iR) + CnAn(iV, iR) + CeAn(iV, iR) ;
		   if (denomR > 0 && CnAn(iV, iR) > 0)  // otherwise zero
		       reinfNegRndRaw(iA-discAttrFrom,iV)[rn] = (CnAn(iV, iR) / denomR)  ;
		   else reinfNegRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfNegRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfNegRnd(iA,iV), eopt.ordEvalNormalizingPercentile,reinfNeg[iA][iV]) ;
         reinfNegRnd(iA,iV)[noOEstats-1] = expReinfNeg[iA][iV] ;

		 // anchoring
		 denom = CpAe(iV, iA) + CnAe(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAe(iV, iR) + CnAe(iV, iR) + CeAe(iV, iR) ;
		   if (denomR > 0 && CeAe(iV, iR) > 0)  // otherwise zero
		       anchorRndRaw(iA-discAttrFrom,iV)[rn] = (CeAe(iV, iR) / denomR)  ;
		   else anchorRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
 	     statOE(anchorRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, anchorRnd(iA,iV), eopt.ordEvalNormalizingPercentile,anchor[iA][iV]) ;
         anchorRnd(iA,iV)[noOEstats-1] = expAnchor[iA][iV] ;
	   }
   }
}

// ***************************************************************************
//
//                       ordAVdAeqNormClDiff1
//                       ---------
//
//     evaluation of ordered attribute-values normalized with
//     changes of random attributes
//    additionally only class values 1 lower or higher are taken into account
//
// ***************************************************************************
void estimation::ordAVdAeqNormClDiff1(int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) {

   // prepare random normalization attributes
   int noBasicEstimated =  discAttrTo - discAttrFrom ;
   int noEstimated =  noBasicEstimated * eopt.ordEvalNoRandomNormalizers ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR, rn,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
	   for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
		  if (eopt.ordEvalBootstrapNormalize) {
	          DiscValues.bootstrapColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn) ;
		  }
		  else {
	          DiscValues.copyColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
			  DiscValues.shuffleColumn(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
		  }
		  prepareDiscAttr(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn, discNoValues[iA]) ;
	   }
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }
   // prepare space for raw scores of random attributes
   mmatrix<marray<double> > reinfPosRndRaw(noBasicEstimated, maxNoValues+1), reinfNegRndRaw(noBasicEstimated, maxNoValues+1), anchorRndRaw(noBasicEstimated, maxNoValues+1) ;
   // empty the results arrays and matrixes
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
	   for (iV = 0 ; iV <= discNoValues[iA] ; ++iV) {
	      reinfPosRnd(iA, iV).init(0.0) ;
     	  reinfNegRnd(iA,iV).init(0.0) ;
     	  anchorRnd(iA,iV).init(0.0) ;
    	  // prepare space for random individual evaluations (raw)
	      reinfPosRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      reinfNegRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      anchorRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	   }
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0) ;

   int i, aVal ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff ;
   int current, neighbourIdx, iAttr ;
   diffEsorted.create(TrainSize) ;
   distanceEarray.create(TrainSize) ;


   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main iterative loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       // currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrdClDiff1(current) ;

      // compute distance factors
      EprepareDistanceFactors(distanceType) ;


		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;

         for (i=0 ; i < distanceEarray.filled() ; i++) {
            neighbourIdx = distanceEarray[i].value ;
            normDistance = distanceEarray[i].key ;
            distanceSum += normDistance ;
  		    clDiff = DAdiffSign(0, neighbourIdx, current) ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (clDiff==0.0) { // same class
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff==1.0) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff > 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest

		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes, in case we do not estimate the whole array
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
 	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
   }
   // compute expected values
   marray<marray<double> > expReinfPos(discAttrTo), expReinfNeg(discAttrTo), expAnchor(discAttrTo);
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++){
	   expReinfPos[iA].create(maxNoValues+1,0) ;
	   expReinfNeg[iA].create(maxNoValues+1,0) ;
	   expAnchor[iA].create(maxNoValues+1,0) ;
   }
   oeExpDistr(discAttrFrom, discAttrTo, expReinfPos, expReinfNeg, expAnchor) ;

   // compute reinforcements, random normalizations and their variance
   double denom, denomR;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CnAp(iV, iA) + CeAp(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAp(iV, iR) + CnAp(iV, iR) + CeAp(iV, iR) ;
		   if (denomR > 0 && CpAp(iV, iR) > 0)  // otherwise zero
		       reinfPosRndRaw(iA-discAttrFrom,iV)[rn] = (CpAp(iV, iR) / denomR)  ;
		   else reinfPosRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfPosRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfPosRnd(iA,iV), eopt.ordEvalNormalizingPercentile, reinfPos[iA][iV]) ;
         reinfPosRnd(iA,iV)[noOEstats-1] = expReinfPos[iA][iV] ;
		 // negative reinforcement
		 denom = CpAn(iV, iA) + CnAn(iV, iA) + CeAn(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAn(iV, iR) + CnAn(iV, iR) + CeAn(iV, iR) ;
		   if (denomR > 0 && CnAn(iV, iR) > 0)  // otherwise zero
		       reinfNegRndRaw(iA-discAttrFrom,iV)[rn] = (CnAn(iV, iR) / denomR)  ;
		   else reinfNegRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfNegRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfNegRnd(iA,iV), eopt.ordEvalNormalizingPercentile,reinfNeg[iA][iV]) ;
         reinfNegRnd(iA,iV)[noOEstats-1] = expReinfNeg[iA][iV] ;

		 // anchoring
		 denom = CpAe(iV, iA) + CnAe(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAe(iV, iR) + CnAe(iV, iR) + CeAe(iV, iR) ;
		   if (denomR > 0 && CeAe(iV, iR) > 0)  // otherwise zero
		       anchorRndRaw(iA-discAttrFrom,iV)[rn] = (CeAe(iV, iR) / denomR)  ;
		   else anchorRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
 	     statOE(anchorRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, anchorRnd(iA,iV), eopt.ordEvalNormalizingPercentile,anchor[iA][iV]) ;
         anchorRnd(iA,iV)[noOEstats-1] = expAnchor[iA][iV] ;
	   }
   }
}

// ***************************************************************************
//
//                       ordAVdAeqNormAttrDiff1
//                       ---------
//
//     evaluation of ordered attribute-values normalized with
//     changes of random attributes
//    additionally only attribute values 1 lower or higher are taken into account
//     when selecting neighborhood
//
// ***************************************************************************
void estimation::ordAVdAeqNormAttrDiff1(int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) {

   // prepare random normalization attributes
   int noBasicEstimated =  discAttrTo - discAttrFrom ;
   int noEstimated =  noBasicEstimated * eopt.ordEvalNoRandomNormalizers ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR, rn,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
	   for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
		  if (eopt.ordEvalBootstrapNormalize) {
	          DiscValues.bootstrapColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn) ;
		  }
		  else {
	          DiscValues.copyColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
			  DiscValues.shuffleColumn(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
		  }
		  prepareDiscAttr(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn, discNoValues[iA]) ;
	   }
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }
   // prepare space for raw scores of random attributes
   mmatrix<marray<double> > reinfPosRndRaw(noBasicEstimated, maxNoValues+1), reinfNegRndRaw(noBasicEstimated, maxNoValues+1), anchorRndRaw(noBasicEstimated, maxNoValues+1) ;
   // empty the results arrays and matrixes
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
	   for (iV = 0 ; iV <= discNoValues[iA] ; ++iV) {
	      reinfPosRnd(iA, iV).init(0.0) ;
     	  reinfNegRnd(iA,iV).init(0.0) ;
     	  anchorRnd(iA,iV).init(0.0) ;
    	  // prepare space for random individual evaluations (raw)
	      reinfPosRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      reinfNegRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      anchorRndRaw(iA-discAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	   }
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0) ;

   int i, aVal ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff ;
   int current, neighbourIdx, iAttr ;
   diffEsorted.create(TrainSize) ;
   distanceEarray.create(TrainSize) ;


   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0) ;
   marray<double> weightSum(discSize) ; // sum of near instance weights for each attribute separately

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main iterative loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       // currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      EprepareDistanceFactors(distanceType) ;


		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;
         weightSum.init(0.0) ;

         for (i=0 ; i < distanceEarray.filled() ; i++) {
            neighbourIdx = distanceEarray[i].value ;
            normDistance = distanceEarray[i].key ;
            distanceSum += normDistance ;
  		    clDiff = DAdiffSign(0, neighbourIdx, current) ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
      		   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
			      // only take actual distance less or equal 1 into acount
			      if (DiscValues(neighbourIdx, iAttr) != NAdisc &&
			            abs(DiscValues(current, iAttr) - DiscValues(neighbourIdx, iAttr)) >1)
			                  continue ;
			      weightSum[iAttr-discAttrFrom] += normDistance ;
  			      if (clDiff==0.0) { // same class
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff==1.0) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff > 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               } // not NA
			}  // for all attributes
		 }  // for all nearest

		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes, in case we do not estimate the whole array
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
				if (weightSum[iAttr-discAttrFrom] <= 0) // no sample for this attribute
					continue ;
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
 	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
   }
   // compute expected values
   marray<marray<double> > expReinfPos(discAttrTo), expReinfNeg(discAttrTo), expAnchor(discAttrTo);
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++){
	   expReinfPos[iA].create(maxNoValues+1,0) ;
	   expReinfNeg[iA].create(maxNoValues+1,0) ;
	   expAnchor[iA].create(maxNoValues+1,0) ;
   }
   oeExpDistr(discAttrFrom, discAttrTo, expReinfPos, expReinfNeg, expAnchor) ;

   // compute reinforcements, random normalizations and their variance
   double denom, denomR;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CnAp(iV, iA) + CeAp(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAp(iV, iR) + CnAp(iV, iR) + CeAp(iV, iR) ;
		   if (denomR > 0 && CpAp(iV, iR) > 0)  // otherwise zero
		       reinfPosRndRaw(iA-discAttrFrom,iV)[rn] = (CpAp(iV, iR) / denomR)  ;
		   else reinfPosRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfPosRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfPosRnd(iA,iV), eopt.ordEvalNormalizingPercentile,reinfPos[iA][iV]) ;
         reinfPosRnd(iA,iV)[noOEstats-1] = expReinfPos[iA][iV] ;
		 // negative reinforcement
		 denom = CpAn(iV, iA) + CnAn(iV, iA) + CeAn(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAn(iV, iR) + CnAn(iV, iR) + CeAn(iV, iR) ;
		   if (denomR > 0 && CnAn(iV, iR) > 0)  // otherwise zero
		       reinfNegRndRaw(iA-discAttrFrom,iV)[rn] = (CnAn(iV, iR) / denomR)  ;
		   else reinfNegRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfNegRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfNegRnd(iA,iV), eopt.ordEvalNormalizingPercentile,reinfNeg[iA][iV]) ;
         reinfNegRnd(iA,iV)[noOEstats-1] = expReinfNeg[iA][iV] ;

		 // anchoring
		 denom = CpAe(iV, iA) + CnAe(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAe(iV, iR) + CnAe(iV, iR) + CeAe(iV, iR) ;
		   if (denomR > 0 && CeAe(iV, iR) > 0)  // otherwise zero
		       anchorRndRaw(iA-discAttrFrom,iV)[rn] = (CeAe(iV, iR) / denomR)  ;
		   else anchorRndRaw(iA-discAttrFrom,iV)[rn] = 0.0  ;
		 }
 	     statOE(anchorRndRaw(iA-discAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, anchorRnd(iA,iV), eopt.ordEvalNormalizingPercentile, anchor[iA][iV]) ;
         anchorRnd(iA,iV)[noOEstats-1] = expAnchor[iA][iV] ;
	   }
   }
}

// ***************************************************************************
//
//                       ordEvalInst
//                       -----------
//
//     measuring the impact of attributes on a single instance
//     this function does it for a selected instance
//
// ***************************************************************************
void estimation::ordEvalInst(int selectedInstance, int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
		marray<double> &reinfPos, marray<double> &reinfNeg, marray<double> &anchor,
		marray<marray<double> > &reinfPosRnd, marray<marray<double> > &reinfNegRnd, marray<marray<double> > &anchorRnd)
{
	// prepare random normalization attributes
	int noBasicEstimated =  discAttrTo - discAttrFrom ;
	int noEstimated =  noBasicEstimated * eopt.ordEvalNoRandomNormalizers ;
	int discSize = noDiscrete + noEstimated ;
	int iA, iV, iR, rn,  maxNoValues = 0 ;
	adjustTables(0, discSize) ;
	for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			if (eopt.ordEvalBootstrapNormalize) {
				DiscValues.bootstrapColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn) ;
			}
			else {
				DiscValues.copyColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
				DiscValues.shuffleColumn(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
			}
			prepareDiscAttr(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn, discNoValues[iA]) ;
		}
		if (discNoValues[iA] > maxNoValues)
			maxNoValues = discNoValues[iA] ;
	}
	// prepare space for raw scores of random attributes
	marray<marray<double> > reinfPosRndRaw(noBasicEstimated), reinfNegRndRaw(noBasicEstimated), anchorRndRaw(noBasicEstimated) ;
	for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
		reinfPosRndRaw[iA-discAttrFrom].create(eopt.ordEvalNoRandomNormalizers, 0.0);
		reinfNegRndRaw[iA-discAttrFrom].create(eopt.ordEvalNoRandomNormalizers, 0.0);
		anchorRndRaw[iA-discAttrFrom].create(eopt.ordEvalNoRandomNormalizers, 0.0);
		reinfPosRnd[iA-discAttrFrom].init(0.0) ;
		reinfNegRnd[iA-discAttrFrom].init(0.0) ;
		anchorRnd[iA-discAttrFrom].init(0.0) ;
	}

	// empty the results arrays and matrixes
	reinfPos.init(0.0) ;
	reinfNeg.init(0.0) ;
	anchor.init(0.0) ;
	marray<double> CpAe(discSize, 0.0), CpAp(discSize, 0.0),CpAn(discSize, 0.0), CnAe(discSize, 0.0),
	               CnAp(discSize, 0.0), CnAn(discSize, 0.0),CeAe(discSize, 0.0), CeAp(discSize, 0.0),
	               CeAn(discSize, 0.0) ;

	int i, aVal ;

	// we have to compute distances up to the following attributes
	discUpper = noDiscrete ;
	numUpper = noNumeric ;

	double distanceSum, normDistance, Adiff, clDiff ;
	int current, neighbourIdx, iAttr ;
	diffEsorted.create(TrainSize) ;
	distanceEarray.create(TrainSize) ;

    // find which instance we have
	current = -1 ;
	for (i=0 ; i < TrainSize ; i++)
		if (originalDTrain[i]==selectedInstance) {
			current = i;
			break ;
		}
	if (i>=TrainSize){
		merror("estimation::ordEvalInst","violation of internal assumption about the instance indexes, results are invalid ");
	    return ;
	}

	//currentClass =  DiscValues(current, 0) ;
	// first we compute distances of  all other examples to current
	computeDistancesOrd(current) ;

	// compute distance factors
	EprepareDistanceFactors(distanceType) ;

	distanceSum = 0.0 ;

	for (i=0 ; i < distanceEarray.filled() ; i++) {
		neighbourIdx = distanceEarray[i].value ;
		normDistance = distanceEarray[i].key ;
		distanceSum += normDistance ;
		clDiff = DAdiffSign(0, neighbourIdx, current) ;

		for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
			if (iAttr==discAttrTo)
				iAttr=noDiscrete ;
			Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			//aVal = DiscValues(neighbourIdx, iAttr) ;
			aVal = DiscValues(current, iAttr) ;
			if (aVal != NAdisc) {
				if (clDiff==0.0) { // same class
					if (Adiff==0) {
						CeAe[iAttr] +=  normDistance  ;
					}
					else if (Adiff > 0.0) {
						CeAp[iAttr] +=  Adiff * normDistance  ;
					}
					else {
						CeAn[iAttr] -=  Adiff * normDistance  ;
					}
				}
				else if (clDiff==1.0) { // current from larger class
					if (Adiff==0) {
						CpAe[iAttr] +=  normDistance  ;
					}
					else if (Adiff > 0.0) {
						CpAp[iAttr] +=  Adiff * normDistance  ;
					}
					else {
						CpAn[iAttr] -=  Adiff * normDistance  ;
					}
				}
				else {   // clDiff > 0, current from lower class
					if (Adiff==0) {
						CnAe[iAttr] +=  normDistance  ;
					}
					else if (Adiff > 0.0) {
						CnAp[iAttr] +=  Adiff * normDistance  ;
					}
					else {
						CnAn[iAttr] -=  Adiff * normDistance  ;
					}

				}
			}
		}  // for all attributes
	}  // for all nearest

	// normalization of increments
	if (distanceSum > 0) {
		for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
			if (iAttr==discAttrTo)  // also for random normalizing attributes, in case we do not estimate the whole array
				iAttr=noDiscrete ;
			iV = DiscValues(current, iAttr) ;
			if (iV==NAdisc)
				continue ;
			CpAp[iAttr] /=  distanceSum ;
			CpAn[iAttr] /=  distanceSum ;
			CpAe[iAttr] /=  distanceSum ;
			CnAp[iAttr] /=  distanceSum ;
			CnAn[iAttr] /=  distanceSum ;
			CnAe[iAttr] /=  distanceSum ;
			CeAp[iAttr] /=  distanceSum ;
			CeAn[iAttr] /=  distanceSum ;
			CeAe[iAttr] /=  distanceSum ;
		}
	}
    // compute expected values
    marray<marray<double> > expReinfPos(discAttrTo), expReinfNeg(discAttrTo), expAnchor(discAttrTo);
    for (iA=discAttrFrom ; iA < discAttrTo ; iA++){
	   expReinfPos[iA].create(maxNoValues+1,0) ;
	   expReinfNeg[iA].create(maxNoValues+1,0) ;
	   expAnchor[iA].create(maxNoValues+1,0) ;
    }
    oeExpDistr(discAttrFrom, discAttrTo, expReinfPos, expReinfNeg, expAnchor) ;

	// compute reinforcements, random normalizations and their variance
	double denom, denomR;
	for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
		// positive reinforcement
		denom = CpAp[iA]+ CnAp[iA] + CeAp[iA] ;
		if (denom > 0)
			reinfPos[iA] = CpAp[iA] / denom ;
		else reinfPos[iA] = 0.0 ;
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
			denomR = CpAp[iR] + CnAp[iR] + CeAp[iR] ;
			if (denomR > 0 && CpAp[iR] > 0)  // otherwise zero
				reinfPosRndRaw[iA-discAttrFrom][rn] = (CpAp[iR] / denomR)  ;
			else reinfPosRndRaw[iA-discAttrFrom][rn] = 0.0  ;
		}
		statOE(reinfPosRndRaw[iA-discAttrFrom], eopt.ordEvalNoRandomNormalizers, reinfPosRnd[iA], eopt.ordEvalNormalizingPercentile, reinfPos[iA]) ;
        reinfPosRnd[iA][noOEstats-1] = expReinfPos[iA][DiscValues(current,iA)] ;

		// negative reinforcement
		denom = CpAn[iA] + CnAn[iA] + CeAn[iA] ;
		if (denom > 0)
			reinfNeg[iA] = CnAn[iA] / denom ;
		else reinfNeg[iA] = 0.0 ;
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
			denomR = CpAn[iR] + CnAn[iR] + CeAn[iR] ;
			if (denomR > 0 && CnAn[iR] > 0)  // otherwise zero
				reinfNegRndRaw[iA-discAttrFrom][rn] = (CnAn[iR] / denomR)  ;
			else reinfNegRndRaw[iA-discAttrFrom][rn] = 0.0  ;
		}
		statOE(reinfNegRndRaw[iA-discAttrFrom], eopt.ordEvalNoRandomNormalizers, reinfNegRnd[iA], eopt.ordEvalNormalizingPercentile, reinfNeg[iA]) ;
        reinfNegRnd[iA][noOEstats-1] = expReinfNeg[iA][DiscValues(current,iA)] ;

		// anchoring
		denom = CpAe[iA] + CnAe[iA] + CeAe[iA] ;
		if (denom > 0)
			anchor[iA] = CeAe[iA] / denom ;
		else anchor[iA] = 0.0 ;
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
			denomR = CpAe[iR] + CnAe[iR] + CeAe[iR] ;
			if (denomR > 0 && CeAe[iR] > 0)  // otherwise zero
				anchorRndRaw[iA-discAttrFrom][rn] = (CeAe[iR] / denomR)  ;
			else anchorRndRaw[iA-discAttrFrom][rn] = 0.0  ;
		}
		statOE(anchorRndRaw[iA-discAttrFrom], eopt.ordEvalNoRandomNormalizers, anchorRnd[iA], eopt.ordEvalNormalizingPercentile, anchor[iA]) ;
		anchorRnd[iA][noOEstats-1] = expAnchor[iA][DiscValues(current,iA)] ;

	}
}


// ***************************************************************************
//
//                       ordEvalInst3
//                       -----------
//
//     measuring the impact of attributes on a single instance
//     this function does it for a selected instance
//     the neighborhood is split into 3 parts: equal, lower, and higher class
//
// ***************************************************************************
void estimation::ordEvalInst3(int selectedInstance, int discAttrFrom, int discAttrTo, oeDistanceType distanceType,
		marray<double> &reinfPos, marray<double> &reinfNeg, marray<double> &anchor,
		marray<marray<double> > &reinfPosRnd, marray<marray<double> > &reinfNegRnd, marray<marray<double> > &anchorRnd)
{
	// prepare random normalization attributes
	int noBasicEstimated =  discAttrTo - discAttrFrom ;
	int noEstimated =  noBasicEstimated * eopt.ordEvalNoRandomNormalizers ;
	int discSize = noDiscrete + noEstimated ;
	int iA, iV, iR, rn,  maxNoValues = 0 ;
	adjustTables(0, discSize) ;
	for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			if (eopt.ordEvalBootstrapNormalize) {
				DiscValues.bootstrapColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn) ;
			}
			else {
				DiscValues.copyColumn(iA, noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
				DiscValues.shuffleColumn(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
			}
			prepareDiscAttr(noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn, discNoValues[iA]) ;
		}
		if (discNoValues[iA] > maxNoValues)
			maxNoValues = discNoValues[iA] ;
	}
	// prepare space for raw scores of random attributes
	marray<marray<double> > reinfPosRndRaw(noBasicEstimated), reinfNegRndRaw(noBasicEstimated), anchorRndRaw(noBasicEstimated) ;
	for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
		reinfPosRndRaw[iA-discAttrFrom].create(eopt.ordEvalNoRandomNormalizers, 0.0);
		reinfNegRndRaw[iA-discAttrFrom].create(eopt.ordEvalNoRandomNormalizers, 0.0);
		anchorRndRaw[iA-discAttrFrom].create(eopt.ordEvalNoRandomNormalizers, 0.0);
		reinfPosRnd[iA-discAttrFrom].init(0.0) ;
		reinfNegRnd[iA-discAttrFrom].init(0.0) ;
		anchorRnd[iA-discAttrFrom].init(0.0) ;
	}

	// empty the results arrays and matrixes
	reinfPos.init(0.0) ;
	reinfNeg.init(0.0) ;
	anchor.init(0.0) ;
	marray<double> CpAe(discSize, 0.0), CpAp(discSize, 0.0),CpAn(discSize, 0.0), CnAe(discSize, 0.0),
	               CnAp(discSize, 0.0), CnAn(discSize, 0.0),CeAe(discSize, 0.0), CeAp(discSize, 0.0),
	               CeAn(discSize, 0.0) ;

	int i, aVal ;

	// we have to compute distances up to the following attributes
	discUpper = noDiscrete ;
	numUpper = noNumeric ;

	double distanceSum, normDistance, Adiff, clDiff ;
	int current, neighbourIdx, iAttr, iClass ;
	//diffEsorted.create(TrainSize) ;
	//distanceEarray.create(TrainSize) ;

    // find which instance we have
	current = -1 ;
	for (i=0 ; i < TrainSize ; i++)
		if (originalDTrain[i]==selectedInstance) {
			current = i;
			break ;
		}
	if (i>=TrainSize){
		merror("estimation::ordEvalInst","violation of internal assumption about the instance indexes, results are invalid ");
	    return ;
	}

	// currentClass =  DiscValues(current, 0) ;
	// first we compute distances of  all other examples to current
	computeDistancesOrd(current) ;

	// compute distance factors
	  // data structure to hold equal, lower and upper class neighbors
	 for (iClass = 0 ; iClass <= 2; iClass++)  {
	      distanceArray[iClass].create(TrainSize) ;
	      diffSorted[iClass].create(TrainSize) ;
	 }
	prepare3clDistanceFactors(current, distanceType) ;


	for (iClass=0 ; iClass <= 2 ; ++iClass) {
		// assumption: distanceArray[iClass] contains neighbors with equal, lower and higher classes
		distanceSum = 0.0 ;
		for (i=0 ; i < distanceArray[iClass].filled() ; i++) {
			neighbourIdx = distanceArray[iClass][i].value ;
			normDistance = distanceArray[iClass][i].key ;
			distanceSum += normDistance ;
			clDiff = DAdiffSign(0, neighbourIdx, current) ;

			for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
				if (iAttr==discAttrTo)
					iAttr=noDiscrete ;
				Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
				//aVal = DiscValues(neighbourIdx, iAttr) ;
				aVal = DiscValues(current, iAttr) ;
				if (aVal != NAdisc) {
					if (clDiff==0.0) { // same class
						if (Adiff==0) {
							CeAe[iAttr] +=  normDistance  ;
						}
						else if (Adiff > 0.0) {
							CeAp[iAttr] +=  Adiff * normDistance  ;
						}
						else {
							CeAn[iAttr] -=  Adiff * normDistance  ;
						}
					}
					else if (clDiff==1.0) { // current from larger class
						if (Adiff==0) {
							CpAe[iAttr] +=  normDistance  ;
						}
						else if (Adiff > 0.0) {
							CpAp[iAttr] +=  Adiff * normDistance  ;
						}
						else {
							CpAn[iAttr] -=  Adiff * normDistance  ;
						}
					}
					else {   // clDiff > 0, current from lower class
						if (Adiff==0) {
							CnAe[iAttr] +=  normDistance  ;
						}
						else if (Adiff > 0.0) {
							CnAp[iAttr] +=  Adiff * normDistance  ;
						}
						else {
							CnAn[iAttr] -=  Adiff * normDistance  ;
						}

					}
				}
			}  // for all attributes
		}  // for all nearest
		// normalization of increments
		if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes, in case we do not estimate the whole array
					iAttr=noDiscrete ;
				iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
				if (iClass==2) {
					CpAp[iAttr] /=  distanceSum ;
					CpAn[iAttr] /=  distanceSum ;
					CpAe[iAttr] /=  distanceSum ;
				}
				else if (iClass==1){
					CnAp[iAttr] /=  distanceSum ;
					CnAn[iAttr] /=  distanceSum ;
					CnAe[iAttr] /=  distanceSum ;
				}
				else {
					CeAp[iAttr] /=  distanceSum ;
					CeAn[iAttr] /=  distanceSum ;
					CeAe[iAttr] /=  distanceSum ;
				}
			}
		}
	}// for all three classes

    // compute expected values
    marray<marray<double> > expReinfPos(discAttrTo), expReinfNeg(discAttrTo), expAnchor(discAttrTo);
    for (iA=discAttrFrom ; iA < discAttrTo ; iA++){
	   expReinfPos[iA].create(maxNoValues+1,0) ;
	   expReinfNeg[iA].create(maxNoValues+1,0) ;
	   expAnchor[iA].create(maxNoValues+1,0) ;
    }
    oeExpDistr(discAttrFrom, discAttrTo, expReinfPos, expReinfNeg, expAnchor) ;

	// compute reinforcements, random normalizations and their variance
	double denom, denomR;
	for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
		// positive reinforcement
		denom = CpAp[iA]+ CnAp[iA] + CeAp[iA] ;
		if (denom > 0)
			reinfPos[iA] = CpAp[iA] / denom ;
		else reinfPos[iA] = 0.0 ;
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
			denomR = CpAp[iR] + CnAp[iR] + CeAp[iR] ;
			if (denomR > 0 && CpAp[iR] > 0)  // otherwise zero
				reinfPosRndRaw[iA-discAttrFrom][rn] = (CpAp[iR] / denomR)  ;
			else reinfPosRndRaw[iA-discAttrFrom][rn] = 0.0  ;
		}
		statOE(reinfPosRndRaw[iA-discAttrFrom], eopt.ordEvalNoRandomNormalizers, reinfPosRnd[iA], eopt.ordEvalNormalizingPercentile, reinfPos[iA]) ;
        reinfPosRnd[iA][noOEstats-1] = expReinfPos[iA][DiscValues(current,iA)] ;

		// negative reinforcement
		denom = CpAn[iA] + CnAn[iA] + CeAn[iA] ;
		if (denom > 0)
			reinfNeg[iA] = CnAn[iA] / denom ;
		else reinfNeg[iA] = 0.0 ;
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
			denomR = CpAn[iR] + CnAn[iR] + CeAn[iR] ;
			if (denomR > 0 && CnAn[iR] > 0)  // otherwise zero
				reinfNegRndRaw[iA-discAttrFrom][rn] = (CnAn[iR] / denomR)  ;
			else reinfNegRndRaw[iA-discAttrFrom][rn] = 0.0  ;
		}
		statOE(reinfNegRndRaw[iA-discAttrFrom], eopt.ordEvalNoRandomNormalizers, reinfNegRnd[iA], eopt.ordEvalNormalizingPercentile, reinfNeg[iA]) ;
        reinfNegRnd[iA][noOEstats-1] = expReinfNeg[iA][DiscValues(current,iA)] ;

		// anchoring
		denom = CpAe[iA] + CnAe[iA] + CeAe[iA] ;
		if (denom > 0)
			anchor[iA] = CeAe[iA] / denom ;
		else anchor[iA] = 0.0 ;
		for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
			iR = noDiscrete + (iA - discAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
			denomR = CpAe[iR] + CnAe[iR] + CeAe[iR] ;
			if (denomR > 0 && CeAe[iR] > 0)  // otherwise zero
				anchorRndRaw[iA-discAttrFrom][rn] = (CeAe[iR] / denomR)  ;
			else anchorRndRaw[iA-discAttrFrom][rn] = 0.0  ;
		}
		statOE(anchorRndRaw[iA-discAttrFrom], eopt.ordEvalNoRandomNormalizers, anchorRnd[iA], eopt.ordEvalNormalizingPercentile, anchor[iA]) ;
		anchorRnd[iA][noOEstats-1] = expAnchor[iA][DiscValues(current,iA)] ;

	}
}


// ***************************************************************************
//
//                       ordAVdAeq
//                       ---------
//
//     evaluation of ordered attribute-values: published version without normalizatiom
//
// ***************************************************************************
void estimation::ordAVdAeq(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, oeDistanceType distanceType) {

   //int noEstimated =  discAttrTo - discAttrFrom ;
   int discSize = noDiscrete ;
   int iA, iV,  maxNoValues = 0 ;

   // maximal number of values
   for (iA=discAttrFrom; iA < discAttrTo ; iA++)
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0) ;

   int i, aVal ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff ;
   int current, neighbourIdx, iAttr ;
   diffEsorted.create(TrainSize) ;
   distanceEarray.create(TrainSize) ;


   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ordEval loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       // currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      EprepareDistanceFactors(distanceType) ;


		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;

         for (i=0 ; i < distanceEarray.filled() ; i++) {
            neighbourIdx = distanceEarray[i].value ;
            normDistance = distanceEarray[i].key ;
            distanceSum += normDistance ;
  		    clDiff = DAdiffSign(0, neighbourIdx, current) ;

            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++) {
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (clDiff==0.0) { // same class
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff==1.0) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff > 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discAttrTo ; ++iAttr) {
			    iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
 	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;

			}
	     }
   }
   // compute reinforcements
   double denom ;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
         denom = CpAp(iV, iA)+CnAp(iV, iA)+CeAp(iV, iA) ;
 	     if (denom > 0)
			 reinfPos[iA][iV] = (CpAp(iV, iA)) / denom ;
		 denom = CpAn(iV, iA)+CnAn(iV, iA)+CeAn(iV, iA) ;
 	     if (denom > 0)
    	     reinfNeg[iA][iV] = (CnAn(iV, iA)) / denom ;
		 denom = CpAe(iV, iA)+CnAe(iV, iA)+CeAe(iV, iA) ;
 	     if (denom > 0)
   	        anchor[iA][iV] = (CeAe(iV, iA)) / denom ;
	   }
   }
}



// ***************************************************************************
//
//                       distrOE
//                       ---------
//
//     probabilities of reinforcement facors based on attribute value distribution
//
// ***************************************************************************
void estimation::oeExpDistr(int discAttrFrom, int discAttrTo, marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor) {

   int iA, iV, i, iC, k, u, noValid, sumAp,sumAn, sumCrAk, maxNoValues = 0 ;

   // maximal number of values
   for (iA=discAttrFrom; iA < discAttrTo ; iA++)
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }

    // compute reinforcements
   double pCpAp,pCnAn,pCeAe ;
   marray<int> aCount(maxNoValues+1) ;
   mmatrix<int> caCount(noClasses+1,maxNoValues+1);
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   aCount.init(0);
	   caCount.init(0);
	   noValid = 0 ;
       for (i=0 ; i < TrainSize ; i++)
	      caCount(DiscValues(i, 0), DiscValues(i, iA)) ++ ;
       for (iV = 1 ; iV <= discNoValues[iA]; ++iV) {
    	   for (iC = 1; iC <= noClasses; iC++)
	          aCount[iV] += caCount(iC,iV) ;
    	   noValid += aCount[iV] ;
       }
       // computation of expected positive reinforcement
	   sumAp =  0 ;
       for (iV = 1 ; iV <= discNoValues[iA]; ++iV) {
		  // reinfPos = CpAp/Ap
       	  if (sumAp>0) {
       		  sumCrAk = 0 ;
       		  pCpAp = 0 ;
       		  for (u=2 ; u <= noClasses; u++) {
       			  for (k=1 ; k < iV; k++)
       				  sumCrAk += caCount(u-1,k);
       			  if (aCount[iV]>0)
       				  pCpAp += double(caCount(u,iV))/aCount[iV] * sumCrAk ;
       		  }
       		  reinfPos[iA][iV]=pCpAp/sumAp ;
       	  }
		  sumAp += aCount[iV] ;
       }
       // computation of expected negative reinforcement
       sumAn =  0 ;
       for (iV = discNoValues[iA]; iV >= 1 ; --iV) {
    	   // reinfNeg = CnAn/An
    	   if (sumAn>0) {
    		   sumCrAk = 0 ;
    		   pCnAn = 0 ;
    		   for (u = noClasses-1; u >=1; u--) {
    			   for (k=1 ; k < iV; k++)
    				   sumCrAk += caCount(u+1,k);
    			   if (aCount[iV]>0)
    				   pCnAn += double(caCount(u,iV))/aCount[iV] * sumCrAk ;
    		   }
    		   reinfNeg[iA][iV]=pCnAn/sumAn ;
    	   }
    	   sumAn += aCount[iV] ;
       }
       // computation of expected anchoring
       for (iV = 1 ; iV <= discNoValues[iA]; ++iV) {
 		  // anchor = CeAe/Ae
    	   pCeAe = 0.0 ;
		   if (aCount[iV]>0){
         	   for (u=1 ; u <= noClasses ; u++)
  		         pCeAe += sqr(caCount(u,iV)) ;
   		       anchor[iA][iV]=pCeAe/sqr(aCount[iV]) ;
		   }
       }
   }
}


/*
// ***************************************************************************
//
//                       avReliefF
//                       -------
//
//   contains the version of ReliefF for attribute values
//   1. with k nearest with equal influence
//   2. with k nearest with exponentially decreasing influence
//
//
// ***************************************************************************
void estimation::aVReliefF(int discAttrFrom, int discAttrTo, marray<marray<double> > &result,
						   int distanceType) {

   int iA, iV ;
   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++)
	   result[iA].init(0.0) ;

   // number of examples belonging to each of the classes
   marray<int> noExInClass(noClasses+1) ;
   marray<double> probClass(noClasses+1) ;
   noExInClass.init(0) ;
   probClass.init(0.0) ;
   int i, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++)
   {
      noExInClass[ DiscValues(i,0) ]++ ;
      probClass[ DiscValues(i,0) ] += weight[i] ;
   }

   // obtain the greatest sensible k (nubmer of nearest hits/misses)
   // and the total weight of examples
   int maxK = noExInClass[1] ;
   double wAll = probClass[1] ;
   for (idx=2 ; idx <= noClasses ; idx++)
   {
      if (noExInClass[idx] > maxK)
         maxK = noExInClass[idx] ;
      wAll += probClass[idx] ;
   }

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / wAll ;

   // data structure to hold nearest hits/misses
   for (int iClss = 1 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(noExInClass[iClss]) ;
      diffSorted[iClss].create(noExInClass[iClss]) ;
   }

   // normalization of contribution of misses
   mmatrix<double> clNorm(noClasses+1,noClasses+1) ;
   for (int j=1 ; j<=noClasses ; j++)
     for (i=1 ; i<=noClasses ; i++)
        clNorm.Set(j,i, probClass[j]/(1.0-probClass[i]) ) ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   marray<marray<double> > incDiscDiffA(noDiscrete+1) ;
   for (iA=1 ; iA < noDiscrete ; iA++)
	   incDiscDiffA[iA].create(discNoValues[iA]+1, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;

       // initialize (optimization reasons)
      currentClass =  DiscValues(current, 0) ;


      // first we compute distances of  all other examples to current
      computeDistances(current) ;

      // compute distance factors
      prepareDistanceFactors(distanceType) ;

      for (cl=1 ; cl<=noClasses ; cl++) {
         // compute sum of diffs
        for (iA=discAttrFrom ; iA < discAttrTo ; ++iA)
           incDiscDiffA[iA].init(0.0) ;
         distanceSum = 0.0 ;
         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++) {
               Adiff = DiscDistance(neighbourIdx, iAttr) ;
			   aVal = DiscValues(neighbourIdx, iAttr) ;
			   if (aVal != NAdisc) {
			      incDiscDiffA[iAttr][aVal] +=  Adiff * normDistance  ;
			      incDiscDiffA[iAttr][0] +=  Adiff * normDistance  ;
			   }
            }
         }
		 if (cl == currentClass) { // hit or miss
            // hit
            // normalization of increments
            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; ++iAttr)
	           for (iV=0 ; iV <= discNoValues[iAttr] ; iV++)
                  if (incDiscDiffA[iAttr][iV] > epsilon)
                    result[iAttr][iV] -= incDiscDiffA[iAttr][iV]/distanceSum ;
          }
          else {
             // miss
             // normalization of increments
             for (iAttr=discAttrFrom ; iAttr < discAttrTo ; ++iAttr)
	           for (iV=0 ; iV <= discNoValues[iAttr] ; iV++)
                  if (incDiscDiffA[iAttr][iV] > epsilon)
                      result[iAttr][iV] += clNorm(cl, currentClass) * incDiscDiffA[iAttr][iV]/distanceSum ;
          }
      }
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
	   for (iV=0 ; iV <= discNoValues[iAttr] ; iV++) {
          result[iAttr][iV] /= double(NoIterations) ;
          #if defined(DEBUG)
          if (result[iAttr][iV] > 1.00001 || result[iAttr][iV] < -1.00001)
             merror("estimation::avReliefF", "computed nominal weights are out of scope") ;
          #endif
       }

}


// ***************************************************************************
//
//                       ordAvReliefF
//                       -------
//
//
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAvReliefF(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &resultCpAp, marray<marray<double> > &resultCpAn,
			marray<marray<double> > &resultCpAe,
			marray<marray<double> > &resultCnAp, marray<marray<double> > &resultCnAn,
			marray<marray<double> > &resultCnAe,
			marray<marray<double> > &resultCeAp, marray<marray<double> > &resultCeAn,
			marray<marray<double> > &resultCeAe,
			int distanceType) {

   int iA, iV ;
   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   resultCpAe[iA].init(0.0) ;
	   resultCpAp[iA].init(0.0) ;
	   resultCpAn[iA].init(0.0) ;
 	   resultCnAe[iA].init(0.0) ;
	   resultCnAp[iA].init(0.0) ;
	   resultCnAe[iA].init(0.0) ;
 	   resultCeAe[iA].init(0.0) ;
	   resultCeAp[iA].init(0.0) ;
	   resultCeAe[iA].init(0.0) ;
  }

   // number of examples belonging to each of the classes
   marray<int> noExInClass(noClasses+1) ;
   marray<double> probClass(noClasses+1) ;
   noExInClass.init(0) ;
   probClass.init(0.0) ;
   int i, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++) {
      noExInClass[ DiscValues(i,0) ]++ ;
      probClass[ DiscValues(i,0) ] += weight[i] ;
   }

   // obtain the greatest sensible k (nubmer of nearest hits/misses)
   // and the total weight of examples
   int maxK = noExInClass[1] ;
   double wAll = probClass[1] ;
   for (idx=2 ; idx <= noClasses ; idx++)
   {
      if (noExInClass[idx] > maxK)
         maxK = noExInClass[idx] ;
      wAll += probClass[idx] ;
   }

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / wAll ;

   // data structure to hold nearest hits/misses
   for (int iClss = 1 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(noExInClass[iClss]) ;
      diffSorted[iClss].create(noExInClass[iClss]) ;
   }

   // normalization of contribution of misses
   mmatrix<double> clNorm(noClasses+1,noClasses+1) ;
   for (int j=1 ; j<=noClasses ; j++)
     for (i=1 ; i<=noClasses ; i++)
		if (i==j)
          clNorm.Set(j,i, 1.0) ; //hit
		else
          clNorm.Set(j,i, probClass[j]/(1.0-probClass[i]) ) ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   marray<marray<double> > incCpAe(noDiscrete+1), incCpAn(noDiscrete+1), incCpAp(noDiscrete+1),
	                       incCnAe(noDiscrete+1), incCnAn(noDiscrete+1), incCnAp(noDiscrete+1),
	                       incCeAe(noDiscrete+1), incCeAn(noDiscrete+1), incCeAp(noDiscrete+1)  ;

   for (iA=0 ; iA < noDiscrete ; iA++) {
	   incCpAp[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCpAn[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCpAe[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCnAp[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCnAn[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCnAe[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCeAp[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCeAn[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCeAe[iA].create(discNoValues[iA]+1, 0.0) ;
   }

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      prepareDistanceFactors(distanceType) ;

      for (cl=1 ; cl<=noClasses ; cl++) {
         // compute sum of diffs
		 for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
             incCpAp[iA].init(0.0) ;
             incCpAn[iA].init(0.0) ;
			 incCpAe[iA].init(0.0) ;
			 incCnAp[iA].init(0.0) ;
             incCnAn[iA].init(0.0) ;
			 incCnAe[iA].init(0.0) ;
			 incCeAp[iA].init(0.0) ;
             incCeAn[iA].init(0.0) ;
			 incCeAe[iA].init(0.0) ;
		 }
         distanceSum = 0.0 ;
         clDiff = DAdiffSign(0, current, distanceArray[cl][0].value) ;

         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++) {
               Adiff = DAdiffSign(iAttr, current, neighbourIdx) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
				   if (clDiff==0) { //hit
					   if (Adiff==0) {
					      incCeAe[iAttr][aVal] +=  normDistance  ;
			              incCeAe[iAttr][0] +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp[iAttr][aVal] +=  Adiff * normDistance  ;
			              incCeAp[iAttr][0] +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn[iAttr][aVal] -=  Adiff * normDistance  ;
			              incCeAn[iAttr][0] -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff>0) {
					   if (Adiff==0) {
					      incCpAe[iAttr][aVal] +=  normDistance  ;
			              incCpAe[iAttr][0] +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp[iAttr][aVal] +=  Adiff * normDistance  ;
			              incCpAp[iAttr][0] +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn[iAttr][aVal] -=  Adiff * normDistance  ;
			              incCpAn[iAttr][0] -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff < 0
					   if (Adiff==0) {
					      incCnAe[iAttr][aVal] +=  normDistance  ;
			              incCnAe[iAttr][0] +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp[iAttr][aVal] +=  Adiff * normDistance  ;
			              incCnAp[iAttr][0] +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn[iAttr][aVal] -=  Adiff * normDistance  ;
			              incCnAn[iAttr][0] -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 //for (iAttr=discAttrFrom ; iAttr < discAttrTo ; ++iAttr) {
			// for (iV=0 ; iV <= discNoValues[iAttr] ; iV++) {
   //              resultCpAp[iAttr][iV] += clNorm(cl, currentClass) * incCpAp[iAttr][iV]/distanceSum ;
   //              resultCpAn[iAttr][iV] += clNorm(cl, currentClass) * incCpAn[iAttr][iV]/distanceSum ;
   //              resultCpA0[iAttr][iV] += clNorm(cl, currentClass) * incCpA0[iAttr][iV]/distanceSum ;
   //              resultCnAp[iAttr][iV] += clNorm(cl, currentClass) * incCnAp[iAttr][iV]/distanceSum ;
   //              resultCnAn[iAttr][iV] += clNorm(cl, currentClass) * incCnAn[iAttr][iV]/distanceSum ;
   //              resultCnA0[iAttr][iV] += clNorm(cl, currentClass) * incCnA0[iAttr][iV]/distanceSum ;
			// }
		 //}
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discAttrTo ; ++iAttr) {
			     iV = DiscValues(current, iAttr) ;
                 resultCpAp[iAttr][iV] += clNorm(cl, currentClass) * incCpAp[iAttr][iV]/distanceSum ;
                 resultCpAn[iAttr][iV] += clNorm(cl, currentClass) * incCpAn[iAttr][iV]/distanceSum ;
                 resultCpAe[iAttr][iV] += clNorm(cl, currentClass) * incCpAe[iAttr][iV]/distanceSum ;
                 resultCnAp[iAttr][iV] += clNorm(cl, currentClass) * incCnAp[iAttr][iV]/distanceSum ;
                 resultCnAn[iAttr][iV] += clNorm(cl, currentClass) * incCnAn[iAttr][iV]/distanceSum ;
                 resultCnAe[iAttr][iV] += clNorm(cl, currentClass) * incCnAe[iAttr][iV]/distanceSum ;
                 resultCeAp[iAttr][iV] += clNorm(cl, currentClass) * incCeAp[iAttr][iV]/distanceSum ;
                 resultCeAn[iAttr][iV] += clNorm(cl, currentClass) * incCeAn[iAttr][iV]/distanceSum ;
                 resultCeAe[iAttr][iV] += clNorm(cl, currentClass) * incCeAe[iAttr][iV]/distanceSum ;

				 iV = 0 ;  // for averaging
                 resultCpAp[iAttr][iV] += clNorm(cl, currentClass) * incCpAp[iAttr][iV]/distanceSum ;
                 resultCpAn[iAttr][iV] += clNorm(cl, currentClass) * incCpAn[iAttr][iV]/distanceSum ;
                 resultCpAe[iAttr][iV] += clNorm(cl, currentClass) * incCpAe[iAttr][iV]/distanceSum ;
                 resultCnAp[iAttr][iV] += clNorm(cl, currentClass) * incCnAp[iAttr][iV]/distanceSum ;
                 resultCnAn[iAttr][iV] += clNorm(cl, currentClass) * incCnAn[iAttr][iV]/distanceSum ;
                 resultCnAe[iAttr][iV] += clNorm(cl, currentClass) * incCnAe[iAttr][iV]/distanceSum ;
                 resultCeAp[iAttr][iV] += clNorm(cl, currentClass) * incCeAp[iAttr][iV]/distanceSum ;
                 resultCeAn[iAttr][iV] += clNorm(cl, currentClass) * incCeAn[iAttr][iV]/distanceSum ;
                 resultCeAe[iAttr][iV] += clNorm(cl, currentClass) * incCeAe[iAttr][iV]/distanceSum ;
			}
		 }
      } // for all classes
   }
   //for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   //   for (iV=0 ; iV <= discNoValues[iAttr] ; iV++) {
          // result[iAttr][iV] /= double(NoIterations) ;
          // result[iAttr][iV] =  resultP[iAttr][iV] - resultN[iAttr][iV] ;

   //    }

}
*/

 // ***************************************************************************
//
//                   DAdiffSign
//              diff function of discrete attribute
//
// ***************************************************************************
inline double estimation::DAdiffSign(int AttrIdx, int I1, int I2) {

  // we assume that missing value has value 0
  int dV1 = DiscValues(I1, AttrIdx) ;
  int dV2 = DiscValues(I2, AttrIdx) ;
  if (dV1 == NAdisc || dV2 == NAdisc || dV2 == dV1)
     return 0 ; //NAdiscValue(DiscValues(I1,0),AttrIdx)[int(dV2)] ;
  else
     return sign(dV2-dV1) ;
}

 // ***************************************************************************
//
//                   CAdiffSign
//              diff function of discrete attribute
//
// ***************************************************************************
inline double estimation::CAdiffSign(int AttrIdx, int I1, int I2) {
   double cV1 = NumValues(I1, AttrIdx) ;
   double cV2 = NumValues(I2, AttrIdx) ;
   if (isNAcont(cV1) || isNAcont(cV2))
      return 0.0 ;
    else
      return sign(cV2-cV1) ;
}

inline double estimation::CAdiffSignRamp(int AttrIdx, int I1, int I2) {
   double cV1 = NumValues(I1, AttrIdx) ;
   double cV2 = NumValues(I2, AttrIdx) ;
   if (isNAcont(cV1) || isNAcont(cV2))
      return 0.0 ;
   else {
	  double d = cV2-cV1 ;
      return sign(d) * CARamp(AttrIdx, fabs(d) ) ;
   }
}

inline double estimation::DAdiffOrd(int AttrIdx, int I1, int I2) {

  // we assume that missing value has value 0
  int dV1 = DiscValues(I1, AttrIdx) ;
  int dV2 = DiscValues(I2, AttrIdx) ;
  if (dV1 == NAdisc)
     return NAdiscValue(DiscValues(I1,0),AttrIdx)[int(dV2)] ;
  else
    if (dV2 == NAdisc)
      return NAdiscValue(DiscValues(I2,0),AttrIdx)[int(dV1)] ;
     else
       return double(dV2-dV1)/double(discNoValues[AttrIdx]-1) ;
}

void estimation::computeDistancesOrd(int Example) {
   int i ;
   for (int j=0 ; j < TrainSize ; j++)
   {
      if (Example == j)
      {
         for (i=0; i<numUpper; i++)
           NumDistance.Set(j, i, 0.0) ;
         for (i=0 ; i < discUpper ; i++)
           DiscDistance.Set(j, i, 0.0) ;
      }
      else {
        for (i=0; i<numUpper; i++)
          NumDistance.Set(j, i, CAdiff(i,Example,j)) ;
        for (i=0 ; i < discUpper ; i++)
          DiscDistance.Set(j, i, fabs(DAdiffOrd(i,Example,j))) ;
      }
   }
}

// only compute for instances with class difference less than 1
void estimation::computeDistancesOrdClDiff1(int Example) {
   int i ;
   for (int j=0 ; j < TrainSize ; j++)
   {
      if (Example == j || abs(DiscValues(Example, 0)-DiscValues(j, 0))>1)
      {
         for (i=0; i<numUpper; i++)
           NumDistance.Set(j, i, 0.0) ;
         for (i=0 ; i < discUpper ; i++)
           DiscDistance.Set(j, i, 0.0) ;
      }
      else {
        for (i=0; i<numUpper; i++)
          NumDistance.Set(j, i, CAdiff(i,Example,j)) ;
        for (i=0 ; i < discUpper ; i++)
          DiscDistance.Set(j, i, fabs(DAdiffOrd(i,Example,j))) ;
      }
   }
}


/*
// ***************************************************************************
//
//                       ordAV3
//                       -------
//
//
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAV3(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &resultCpAp, marray<marray<double> > &resultCpAn,
			marray<marray<double> > &resultCpAe,
			marray<marray<double> > &resultCnAp, marray<marray<double> > &resultCnAn,
			marray<marray<double> > &resultCnAe,
			marray<marray<double> > &resultCeAp, marray<marray<double> > &resultCeAn,
			marray<marray<double> > &resultCeAe,
			int distanceType) {

   int iA, iV ;
   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   resultCpAe[iA].init(0.0) ;
	   resultCpAp[iA].init(0.0) ;
	   resultCpAn[iA].init(0.0) ;
 	   resultCnAe[iA].init(0.0) ;
	   resultCnAp[iA].init(0.0) ;
	   resultCnAn[iA].init(0.0) ;
 	   resultCeAe[iA].init(0.0) ;
	   resultCeAp[iA].init(0.0) ;
	   resultCeAn[iA].init(0.0) ;
  }

   // number of examples belonging to each of the classes
   marray<double> probClass(noClasses+1) ;
   probClass.init(0.0) ;
   int i, j, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++)
      probClass[ DiscValues(i,0) ] += weight[i] ;

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / double(TrainSize)  ;

   // data structure to hold nearest hits, + and - misses
   for (int iClss = 0 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(TrainSize) ;
      diffSorted[iClss].create(TrainSize) ;
   }

   // normalization of contribution of misses
   double pLower, pHigher ;
   mmatrix<double> clNorm(noClasses+1,3) ;
   for (i=1 ; i<=noClasses ; i++)  {
	 pLower = pHigher = 0.0 ;
	 for (j=1 ; j < i ; j++)
		 pLower +=   probClass[j] ;
	 for (j=i+1; j<=noClasses ; j++)
		 pHigher +=   probClass[j] ;
     clNorm(i, 0) = probClass[i] ; // 1.0 ; //hit
     clNorm(i, 1) = pLower ; // /(1.0-probClass[i])  ;
	 clNorm(i, 2) = pHigher ;// /(1.0-probClass[i]) ;
   }
   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   marray<marray<double> > incCpAe(noDiscrete+1), incCpAn(noDiscrete+1), incCpAp(noDiscrete+1),
	                       incCnAe(noDiscrete+1), incCnAn(noDiscrete+1), incCnAp(noDiscrete+1),
	                       incCeAe(noDiscrete+1), incCeAn(noDiscrete+1), incCeAp(noDiscrete+1)  ;

   for (iA=0 ; iA < noDiscrete ; iA++) {
	   incCpAp[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCpAn[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCpAe[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCnAp[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCnAn[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCnAe[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCeAp[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCeAn[iA].create(discNoValues[iA]+1, 0.0) ;
	   incCeAe[iA].create(discNoValues[iA]+1, 0.0) ;
   }

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      prepare3clDistanceFactors(current, distanceType) ;

      for (cl=0 ; cl<=2 ; cl++) {
         // compute sum of diffs
		 for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
             incCpAp[iA].init(0.0) ;
             incCpAn[iA].init(0.0) ;
			 incCpAe[iA].init(0.0) ;
			 incCnAp[iA].init(0.0) ;
             incCnAn[iA].init(0.0) ;
			 incCnAe[iA].init(0.0) ;
			 incCeAp[iA].init(0.0) ;
             incCeAn[iA].init(0.0) ;
			 incCeAe[iA].init(0.0) ;
		 }
         distanceSum = 0.0 ;
		 // clDiff = DAdiffSign(0, current, distanceArray[cl][0].value) ;

         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++) {
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (cl==0) { //hit
				      if (Adiff==0) {
					      incCeAe[iAttr][aVal] +=  normDistance  ;
			              incCeAe[iAttr][0] +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp[iAttr][aVal] +=  Adiff * normDistance  ;
			              incCeAp[iAttr][0] +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn[iAttr][aVal] -=  Adiff * normDistance  ;
			              incCeAn[iAttr][0] -=  Adiff * normDistance  ;
				       }
				   }
				   else if (cl==1) { // current from larger class
					   if (Adiff==0) {
					      incCpAe[iAttr][aVal] +=  normDistance  ;
			              incCpAe[iAttr][0] +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp[iAttr][aVal] +=  Adiff * normDistance  ;
			              incCpAp[iAttr][0] +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn[iAttr][aVal] -=  Adiff * normDistance  ;
			              incCpAn[iAttr][0] -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // cl == 2, clDiff < 0, current from lower class
					   if (Adiff==0) {
					      incCnAe[iAttr][aVal] +=  normDistance  ;
			              incCnAe[iAttr][0] +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp[iAttr][aVal] +=  Adiff * normDistance  ;
			              incCnAp[iAttr][0] +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn[iAttr][aVal] -=  Adiff * normDistance  ;
			              incCnAn[iAttr][0] -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discAttrTo ; ++iAttr) {
			    iV = DiscValues(current, iAttr) ;
                resultCpAp[iAttr][iV] += clNorm(currentClass,cl) * incCpAp[iAttr][iV]/distanceSum ;
	            resultCpAn[iAttr][iV] += clNorm(currentClass,cl) * incCpAn[iAttr][iV]/distanceSum ;
		        resultCpAe[iAttr][iV] += clNorm(currentClass,cl) * incCpAe[iAttr][iV]/distanceSum ;
			    resultCnAp[iAttr][iV] += clNorm(currentClass,cl) * incCnAp[iAttr][iV]/distanceSum ;
			    resultCnAn[iAttr][iV] += clNorm(currentClass,cl) * incCnAn[iAttr][iV]/distanceSum ;
                resultCnAe[iAttr][iV] += clNorm(currentClass,cl) * incCnAe[iAttr][iV]/distanceSum ;
			    resultCeAp[iAttr][iV] += clNorm(currentClass,cl) * incCeAp[iAttr][iV]/distanceSum ;
			    resultCeAn[iAttr][iV] += clNorm(currentClass,cl) * incCeAn[iAttr][iV]/distanceSum ;
                resultCeAe[iAttr][iV] += clNorm(currentClass,cl) * incCeAe[iAttr][iV]/distanceSum ;
	  			iV = 0 ;  // for averaging
                resultCpAp[iAttr][iV] += clNorm(currentClass,cl) * incCpAp[iAttr][iV]/distanceSum ;
	            resultCpAn[iAttr][iV] += clNorm(currentClass,cl) * incCpAn[iAttr][iV]/distanceSum ;
		        resultCpAe[iAttr][iV] += clNorm(currentClass,cl) * incCpAe[iAttr][iV]/distanceSum ;
			    resultCnAp[iAttr][iV] += clNorm(currentClass,cl) * incCnAp[iAttr][iV]/distanceSum ;
			    resultCnAn[iAttr][iV] += clNorm(currentClass,cl) * incCnAn[iAttr][iV]/distanceSum ;
                resultCnAe[iAttr][iV] += clNorm(currentClass,cl) * incCnAe[iAttr][iV]/distanceSum ;
			    resultCeAp[iAttr][iV] += clNorm(currentClass,cl) * incCeAp[iAttr][iV]/distanceSum ;
			    resultCeAn[iAttr][iV] += clNorm(currentClass,cl) * incCeAn[iAttr][iV]/distanceSum ;
                resultCeAe[iAttr][iV] += clNorm(currentClass,cl) * incCeAe[iAttr][iV]/distanceSum ;
			}
	     }
      } // for all classes
   }
   //for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   //   for (iV=0 ; iV <= discNoValues[iAttr] ; iV++) {
          // result[iAttr][iV] /= double(NoIterations) ;
          // result[iAttr][iV] =  resultP[iAttr][iV] - resultN[iAttr][iV] ;

   //    }

}

*/

 // ***************************************************************************
//
//                          prepare3clDistanceFactors
// computation of distance probability weight factors for given example
//
// ***************************************************************************
void estimation::prepare3clDistanceFactors(int current, oeDistanceType distanceType) {
// we use only original attributes to obtain distance in attribute space

   int kSelected = 0 ;
   switch (distanceType)   {
      case kEqual:
              kSelected = kNearestEqual ;
              break ;
      case expRank:
              kSelected = kDensity ;
              break ;
      case bestK:
               kSelected = TrainSize ;
               break ;
         default: merror("estimation::prepare3clDistanceFactors","invalid distance type") ;
   }
   int i, cl ;
   sortRec tempSort ;
   for (cl = 0 ; cl <= 2; cl++) {
      // empty data structures
      distanceArray[cl].clear() ;
      diffSorted[cl].clear() ;
   }

   // distances in attributes space
   int bunch, currentCl = DiscValues(current, 0) ;
   for (i=0 ; i < TrainSize; i++)  {
      tempSort.key =  CaseDistance(i) ;
	  if (tempSort.key == 0.0) // we skip current and identical examples
		  continue ;
      tempSort.value = i ;
	  if (DiscValues(i,0) < currentCl)   // lower
		  bunch = 1 ;
	  else if (DiscValues(i,0) > currentCl)	// higher
		  bunch = 2 ;
	  else bunch = 0 ; // hits

      diffSorted[bunch].addEnd(tempSort) ;
   }

   // sort examples
   for (cl=0 ; cl <= 2 ; cl++)    {
      // we sort groups of examples according to ascending distance from current
      if (diffSorted[cl].filled() > 1)
         diffSorted[cl].sortKsmallest(Mmin(kSelected, diffSorted[cl].filled())) ;
   }

   int upper, idx ;
   double factor ;
   // depending on tpe of distance, copy the nearest cases
   // and their distance factors into resulting array
   switch (distanceType)   {
        case kEqual:
        case bestK:
          {
            for (cl=0; cl <= 2 ; cl++)    {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++) {
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  idx -- ;
                  distanceArray[cl][i].key = 1.0  ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        case expRank:
          {
            for (cl=0; cl <= 2 ; cl++)
            {
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               distanceArray[cl].setFilled(upper) ;
               if (upper < 1)  // are there any elements
                  continue ;
               idx =  diffSorted[cl].filled() -1;
               factor = 1.0  ;
               distanceArray[cl][0].key =  factor ;
               distanceArray[cl][0].value = diffSorted[cl][idx].value ;
               idx -- ;
               for (i=1 ; i < upper ; i++) {
                  if (diffSorted[cl][idx].key != diffSorted[cl][idx+1].key)
                     factor = double(exp(-sqr(double(i))/varianceDistanceDensity)) ;
                  distanceArray[cl][i].key =  factor ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  idx -- ;
               }
            }
          }
          break ;
        default: merror("estimation::prepare3clDistanceFactors","invalid distanceType detected") ;
   }
}

/*
// ***************************************************************************
//
//                       ordAV3dAnorm
//                       -------------------
//
//     evaluation of ordered attribute-values normalized with
//     reinforcement of random attribute
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAV3dAnorm(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) {

   // prepare ranomized normalization attributes
   int noEstimated =  discAttrTo - discAttrFrom ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
      DiscValues.copyColumn(iA, noDiscrete+iA-discAttrFrom) ;
      DiscValues.shuffleColumn(noDiscrete+iA-discAttrFrom) ;
      prepareDiscAttr(noDiscrete+iA-discAttrFrom, discNoValues[iA]) ;
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0);

   // count number of examples belonging to each of the classes
   marray<double> probClass(noClasses+1) ;
   probClass.init(0.0) ;
   int i, j, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++)
      probClass[ DiscValues(i,0) ] += weight[i] ;

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / double(TrainSize)  ;

   // data structure to hold nearest hits, + and - misses
   for (int iClss = 0 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(TrainSize) ;
      diffSorted[iClss].create(TrainSize) ;
   }

   // normalization of contribution of misses
   double pLower, pHigher ;
   mmatrix<double> clNorm(noClasses+1,3) ;
   for (i=1 ; i<=noClasses ; i++)  {
	 pLower = pHigher = 0.0 ;
	 for (j=1 ; j < i ; j++)
		 pLower +=   probClass[j] ;
	 for (j=i+1; j<=noClasses ; j++)
		 pHigher +=   probClass[j] ;
     clNorm(i, 0) = probClass[i] ; // 1.0 ; //hit
     clNorm(i, 1) = pLower ; // /(1.0-probClass[i])  ;
	 clNorm(i, 2) = pHigher ;// /(1.0-probClass[i]) ;
   }
   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0)  ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      prepare3clDistanceFactors(current, distanceType) ;

      for (cl=0 ; cl<=2 ; cl++) {

		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;
		 // clDiff = DAdiffSign(0, current, distanceArray[cl][0].value) ;

         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (cl==0) { //hit
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (cl==1) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // cl == 2, clDiff < 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
  			    if (iV==NAdisc)
				   continue ;
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
      } // for all classes
   }
   // compute reinforcements and normalize them with random counterparts
   double denom, denomR ;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   iR = noDiscrete + iA - discAttrFrom ;
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CnAp(iV, iA) + CeAp(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
   	     denomR = CpAp(iV, iR) + CnAp(iV, iR) + CeAp(iV, iR) ;
         if (denomR > 0 && CpAp(iV, iR) > 0)
		   reinfPos[iA][iV] /= (CpAp(iV, iR) / denomR) ;
		 else reinfPos[iA][iV]= 0.0 ;

		 // negative reinforcement
		 denom = CpAn(iV, iA) + CnAn(iV, iA) + CeAn(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
   	     denomR = CpAn(iV, iR) + CnAn(iV, iR) + CeAn(iV, iR) ;
         if (denomR > 0 && CnAn(iV, iR) > 0)
		   reinfNeg[iA][iV] /= (CnAn(iV, iR) / denomR) ;
		 else reinfNeg[iA][iV] = 0.0 ;

		 // anchoring
		 denom = CpAe(iV, iA) + CnAe(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     denomR = CpAe(iV, iR) + CnAe(iV, iR) + CeAe(iV, iR) ;
         if (denomR > 0 && CeAe(iV, iR)> 0)
		   anchor[iA][iV] /= (CeAe(iV, iR) / denomR) ;
		 else anchor[iA][iV] = 0.0 ;
	   }
   }
}


// ***************************************************************************
//
//                       ordAV3dA
//                       ---------
//
//     evaluation of ordered attribute-values normalized with
//     changes count  of random attribute
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAV3dA(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) {

   // prepare ranomized normalization attributes
   int noEstimated =  discAttrTo - discAttrFrom ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
      DiscValues.copyColumn(iA, noDiscrete+iA-discAttrFrom) ;
      DiscValues.shuffleColumn(noDiscrete+iA-discAttrFrom) ;
      prepareDiscAttr(noDiscrete+iA-discAttrFrom, discNoValues[iA]) ;
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0) ;

   // count number of examples belonging to each of the classes
   marray<double> probClass(noClasses+1) ;
   probClass.init(0.0) ;
   int i, j, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++)
      probClass[ DiscValues(i,0) ] += weight[i] ;

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / double(TrainSize)  ;

   // data structure to hold nearest hits, + and - misses
   for (int iClss = 0 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(TrainSize) ;
      diffSorted[iClss].create(TrainSize) ;
   }

   // normalization of contribution of misses
   double pLower, pHigher ;
   mmatrix<double> clNorm(noClasses+1,3) ;
   for (i=1 ; i<=noClasses ; i++)  {
	 pLower = pHigher = 0.0 ;
	 for (j=1 ; j < i ; j++)
		 pLower +=   probClass[j] ;
	 for (j=i+1; j<=noClasses ; j++)
		 pHigher +=   probClass[j] ;
     clNorm(i, 0) = probClass[i] ; // 1.0 ; //hit
     clNorm(i, 1) = pLower ; // /(1.0-probClass[i])  ;
	 clNorm(i, 2) = pHigher ;// /(1.0-probClass[i]) ;
   }
   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      prepare3clDistanceFactors(current, distanceType) ;

      for (cl=0 ; cl<=2 ; cl++) {

		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;
		 // clDiff = DAdiffSign(0, current, distanceArray[cl][0].value) ;

         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (cl==0) { //hit
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (cl==1) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // cl == 2, clDiff < 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
 	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;

			}
	     }
      } // for all classes
   }
   // compute reinforcements and normalize them with random counterparts
   double denom ;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   iR = noDiscrete + iA - discAttrFrom ;
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
         denom = CpAp(iV, iA)+CnAp(iV, iA)+CeAp(iV, iA) ;
 	     if (denom > 0)
			 reinfPos[iA][iV] = (CpAp(iV, iA)) / denom ;
		 denom = CpAn(iV, iA)+CnAn(iV, iA)+CeAn(iV, iA) ;
 	     if (denom > 0)
    	     reinfNeg[iA][iV] = (CnAn(iV, iA)) / denom ;
		 denom = CpAe(iV, iA)+CnAe(iV, iA)+CeAe(iV, iA) ;
 	     if (denom > 0)
   	        anchor[iA][iV] = (CeAe(iV, iA)) / denom ;
	   }
   }
}



// ***************************************************************************
//
//                       ordAV3dCnorm
//                       -------------------
//
//     evaluation of ordered attribute-values normalized with
//     reinforcement of random attribute
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAV3dCnorm(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) {

   // prepare ranomized normalization attributes
   int noEstimated =  discAttrTo - discAttrFrom ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
      DiscValues.copyColumn(iA, noDiscrete+iA-discAttrFrom) ;
      DiscValues.shuffleColumn(noDiscrete+iA-discAttrFrom) ;
      prepareDiscAttr(noDiscrete+iA-discAttrFrom, discNoValues[iA]) ;
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0);

   // count number of examples belonging to each of the classes
   marray<double> probClass(noClasses+1) ;
   probClass.init(0.0) ;
   int i, j, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++)
      probClass[ DiscValues(i,0) ] += weight[i] ;

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / double(TrainSize)  ;

   // data structure to hold nearest hits, + and - misses
   for (int iClss = 0 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(TrainSize) ;
      diffSorted[iClss].create(TrainSize) ;
   }

   // normalization of contribution of misses
   double pLower, pHigher ;
   mmatrix<double> clNorm(noClasses+1,3) ;
   for (i=1 ; i<=noClasses ; i++)  {
	 pLower = pHigher = 0.0 ;
	 for (j=1 ; j < i ; j++)
		 pLower +=   probClass[j] ;
	 for (j=i+1; j<=noClasses ; j++)
		 pHigher +=   probClass[j] ;
     clNorm(i, 0) = probClass[i] ; // 1.0 ; //hit
     clNorm(i, 1) = pLower ; // /(1.0-probClass[i])  ;
	 clNorm(i, 2) = pHigher ;// /(1.0-probClass[i]) ;
   }
   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0)  ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      prepare3clDistanceFactors(current, distanceType) ;

      for (cl=0 ; cl<=2 ; cl++) {

		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;
		 // clDiff = DAdiffSign(0, current, distanceArray[cl][0].value) ;

         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (cl==0) { //hit
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (cl==1) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // cl == 2, clDiff < 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
  			    if (iV==NAdisc)
				   continue ;
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
      } // for all classes
   }
   // compute reinforcements and normalize them with random counterparts
   double denom, denomR ;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   iR = noDiscrete + iA - discAttrFrom ;
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
   	     denomR = CpAp(iV, iR) + CpAn(iV, iR) + CpAe(iV, iR) ;
         if (denomR > 0 && CpAp(iV, iR)>0)
		   reinfPos[iA][iV] /= CpAp(iV, iR) / denomR ;
		 else reinfPos[iA][iV] = 0.0 ;

		 // negative reinforcement
		 denom = CnAp(iV, iA) + CnAn(iV, iA) + CnAe(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
   	     denomR = CnAp(iV, iR) + CnAn(iV, iR) + CnAe(iV, iR) ;
         if (denomR > 0 && CnAn(iV, iR)>0)
		   reinfNeg[iA][iV] /= CnAn(iV, iR) / denomR ;
		 else reinfNeg[iA][iV] = 0.0 ;

		 // anchoring
		 denom = CeAp(iV, iA) + CeAn(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     denomR = CeAp(iV, iR) + CeAn(iV, iR) + CeAe(iV, iR) ;
         if (denomR > 0 && CeAe(iV, iR)>0)
		   anchor[iA][iV] /= CeAe(iV, iR) / denomR ;
 		 else anchor[iA][iV] = 0.0 ;
	   }
   }
}


// ***************************************************************************
//
//                       ordAV3dC
//                       -------------------
//
//     evaluation of ordered attribute-values normalized with
//     reinforcement of random attribute
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAV3dC(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) {

   // prepare ranomized normalization attributes
   int noEstimated =  discAttrTo - discAttrFrom ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
      DiscValues.copyColumn(iA, noDiscrete+iA-discAttrFrom) ;
      DiscValues.shuffleColumn(noDiscrete+iA-discAttrFrom) ;
      prepareDiscAttr(noDiscrete+iA-discAttrFrom, discNoValues[iA]) ;
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0);

   // count number of examples belonging to each of the classes
   marray<double> probClass(noClasses+1) ;
   probClass.init(0.0) ;
   int i, j, idx, aVal ;
   for (i=0 ; i < TrainSize ; i++)
      probClass[ DiscValues(i,0) ] += weight[i] ;

   // compute estimations of class value probabilities with their relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / double(TrainSize)  ;

   // data structure to hold nearest hits, + and - misses
   for (int iClss = 0 ; iClss <= noClasses; iClss++)  {
      distanceArray[iClss].create(TrainSize) ;
      diffSorted[iClss].create(TrainSize) ;
   }

   // normalization of contribution of misses
   double pLower, pHigher ;
   mmatrix<double> clNorm(noClasses+1,3) ;
   for (i=1 ; i<=noClasses ; i++)  {
	 pLower = pHigher = 0.0 ;
	 for (j=1 ; j < i ; j++)
		 pLower +=   probClass[j] ;
	 for (j=i+1; j<=noClasses ; j++)
		 pHigher +=   probClass[j] ;
     clNorm(i, 0) = probClass[i] ; // 1.0 ; //hit
     clNorm(i, 1) = pLower ; // /(1.0-probClass[i])  ;
	 clNorm(i, 2) = pHigher ;// /(1.0-probClass[i]) ;
   }
   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0)  ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      prepare3clDistanceFactors(current, distanceType) ;

      for (cl=0 ; cl<=2 ; cl++) {

		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;
		 // clDiff = DAdiffSign(0, current, distanceArray[cl][0].value) ;

         for (i=0 ; i < distanceArray[cl].filled() ; i++) {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (cl==0) { //hit
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (cl==1) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // cl == 2, clDiff < 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
  			    if (iV==NAdisc)
				   continue ;
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) += clNorm(currentClass,cl) * incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) += clNorm(currentClass,cl) * incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) += clNorm(currentClass,cl) * incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) += clNorm(currentClass,cl) * incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) += clNorm(currentClass,cl) * incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) += clNorm(currentClass,cl) * incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) += clNorm(currentClass,cl) * incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) += clNorm(currentClass,cl) * incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) += clNorm(currentClass,cl) * incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
      } // for all classes
   }
   // compute reinforcements and normalize them with random counterparts
   double denom ;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   iR = noDiscrete + iA - discAttrFrom ;
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;

		 // negative reinforcement
		 denom = CnAp(iV, iA) + CnAn(iV, iA) + CnAe(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;

		 // anchoring
		 denom = CeAp(iV, iA) + CeAn(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
	   }
   }
}
*/


/*
// ***************************************************************************
//
//                       ordAVdAeqNorm1
//                       ---------
//
//     evaluation of ordered attribute-values normalized with
//     changes count  of single random attribute
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordAVdAeqNorm1(int discAttrFrom, int discAttrTo,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg,
			marray<marray<double> > &anchor, int distanceType) {

   // prepare ranomized normalization attributes
   int noEstimated =  discAttrTo - discAttrFrom ;
   int discSize = noDiscrete + noEstimated ;
   int iA, iV, iR,  maxNoValues = 0 ;
   adjustTables(0, discSize) ;
   for (iA=discAttrFrom; iA < discAttrTo ; iA++) {
      DiscValues.copyColumn(iA, noDiscrete+iA-discAttrFrom) ;
      DiscValues.shuffleColumn(noDiscrete+iA-discAttrFrom) ;
      prepareDiscAttr(noDiscrete+iA-discAttrFrom, discNoValues[iA]) ;
	   if (discNoValues[iA] > maxNoValues)
		   maxNoValues = discNoValues[iA] ;
   }

   // empty the results arrays
   for (iA=discAttrFrom ; iA < discAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
   }
   mmatrix<double> CpAe(maxNoValues+1, discSize, 0.0), CpAp(maxNoValues+1, discSize, 0.0),
	               CpAn(maxNoValues+1, discSize, 0.0), CnAe(maxNoValues+1, discSize, 0.0),
				   CnAp(maxNoValues+1, discSize, 0.0), CnAn(maxNoValues+1, discSize, 0.0),
				   CeAe(maxNoValues+1, discSize, 0.0), CeAp(maxNoValues+1, discSize, 0.0),
				   CeAn(maxNoValues+1, discSize, 0.0) ;

   int i, aVal ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff ;
   int current, neighbourIdx, iAttr, currentClass ;
   diffEsorted.create(TrainSize) ;
   distanceEarray.create(TrainSize) ;


   mmatrix<double> incCpAe(maxNoValues+1, discSize, 0.0), incCpAn(maxNoValues+1, discSize, 0.0),
	               incCpAp(maxNoValues+1, discSize, 0.0), incCnAe(maxNoValues+1, discSize, 0.0),
				   incCnAn(maxNoValues+1, discSize, 0.0), incCnAp(maxNoValues+1, discSize, 0.0),
	               incCeAe(maxNoValues+1, discSize, 0.0), incCeAn(maxNoValues+1, discSize, 0.0),
				   incCeAp(maxNoValues+1, discSize, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      EprepareDistanceFactors(distanceType) ;


		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;

         for (i=0 ; i < distanceEarray.filled() ; i++) {
            neighbourIdx = distanceEarray[i].value ;
            normDistance = distanceEarray[i].key ;
            distanceSum += normDistance ;
  		    clDiff = DAdiffSign(0, neighbourIdx, current) ;

            for (iAttr=discAttrFrom ; iAttr < discSize ; iAttr ++) {
               if (iAttr==discAttrTo)
				   iAttr=noDiscrete ;
               Adiff = DAdiffSign(iAttr, neighbourIdx, current) ;
			   //aVal = DiscValues(neighbourIdx, iAttr) ;
			   aVal = DiscValues(current, iAttr) ;
			   if (aVal != NAdisc) {
  			      if (clDiff==0.0) { // same class
				      if (Adiff==0) {
					      incCeAe(aVal, iAttr) +=  normDistance  ;
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCeAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCeAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff==1.0) { // current from larger class
					   if (Adiff==0) {
					      incCpAe(aVal, iAttr) +=  normDistance  ;
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCpAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCpAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff > 0, current from lower class
					   if (Adiff==0) {
					      incCnAe(aVal, iAttr) +=  normDistance  ;
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
					      incCnAp(aVal, iAttr) +=  Adiff * normDistance  ;
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
					      incCnAn(aVal, iAttr) -=  Adiff * normDistance  ;
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest
		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=discAttrFrom ; iAttr < discSize ; ++iAttr) {
				if (iAttr==discAttrTo)  // also for random normalizing attributes
					iAttr=noDiscrete ;
			    iV = DiscValues(current, iAttr) ;
				if (iV==NAdisc)
					continue ;
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
 	  			iV = 0 ;  // for averaging
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;

			}
	     }
   }
   // compute reinforcements and normalize them with random counterparts
   double denom, denomR ;
   for (iA=discAttrFrom ; iA < discAttrTo ; ++iA) {
	   iR = noDiscrete + iA - discAttrFrom ;
	   for (iV = 0 ; iV <= discNoValues[iA]; ++iV) {
   	     // positive reinforcement
		 //denom = CpAp(iV, iA) + CpAn(iV, iA) + CpAe(iV, iA) ;
		 denom = CpAp(iV, iA) + CnAp(iV, iA) + CeAp(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
   	     denomR = CpAp(iV, iR) + CnAp(iV, iR) + CeAp(iV, iR) ;
         if (denomR > 0 && CpAp(iV, iR) > 0)
		   reinfPos[iA][iV] /= (CpAp(iV, iR) / denomR) ;
		 else reinfPos[iA][iV]= 0.0 ;

		 // negative reinforcement
		 denom = CpAn(iV, iA) + CnAn(iV, iA) + CeAn(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
   	     denomR = CpAn(iV, iR) + CnAn(iV, iR) + CeAn(iV, iR) ;
         if (denomR > 0 && CnAn(iV, iR) > 0)
		   reinfNeg[iA][iV] /= (CnAn(iV, iR) / denomR) ;
		 else reinfNeg[iA][iV] = 0.0 ;

		 // anchoring
		 denom = CpAe(iV, iA) + CnAe(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     denomR = CpAe(iV, iR) + CnAe(iV, iR) + CeAe(iV, iR) ;
         if (denomR > 0 && CeAe(iV, iR)> 0)
		   anchor[iA][iV] /= (CeAe(iV, iR) / denomR) ;
		 else anchor[iA][iV] = 0.0 ;
	   }
   }
}
 */


 /*
// ***************************************************************************
//
//                       ordClassdAeqNorm
//                       ---------
//
//     evaluation of attributes, where the class is ordered attributes,
//    and attribute values are positively correlated with it,
//    attributes are treated as numeric values
//     additionaly normalization with random attributes is computed
//
// ***************************************************************************
// ***************************************************************************
void estimation::ordClassdAeqNorm(int contAttrFrom, int contAttrTo, int distanceType,
	        marray<marray<double> > &reinfPos, marray<marray<double> > &reinfNeg, marray<marray<double> > &anchor,
	        mmatrix<marray<double> > &reinfPosRnd, mmatrix<marray<double> > &reinfNegRnd, mmatrix<marray<double> > &anchorRnd) {

   // prepare ranom normalization attributes
   int noBasicEstimated =  contAttrTo - contAttrFrom ;
   int noEstimated =  noBasicEstimated * eopt.ordEvalNoRandomNormalizers ;
   int contSize = noNumeric + noEstimated ;
   int iA, iV, iR, rn ;
   adjustTables(contSize, 1) ;
   for (iA=contAttrFrom; iA < contAttrTo ; iA++) {
	   for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
          NumValues.copyColumn(iA, noNumeric + (iA - contAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
		  NumValues.shuffleColumn(noNumeric + (iA - contAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ) ;
		  prepareContAttr(noNumeric + (iA - contAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn) ;
	   }
   }
   // prepare space for raw scores of random attributes
   mmatrix<marray<double> > reinfPosRndRaw(noBasicEstimated, 1), reinfNegRndRaw(noBasicEstimated, 1), anchorRndRaw(noBasicEstimated, 1) ;
   // empty the results arrays and matrixes
   for (iA=contAttrFrom ; iA < contAttrTo ; iA++) {
	   reinfPos[iA].init(0.0) ;
	   reinfNeg[iA].init(0.0) ;
	   anchor[iA].init(0.0) ;
	   for (iV = 0 ; iV < 1 ; ++iV) {
	      reinfPosRnd(iA, iV).init(0.0) ;
     	  reinfNegRnd(iA,iV).init(0.0) ;
     	  anchorRnd(iA,iV).init(0.0) ;
    	  // prepare space for random individual evaluations (raw)
	      reinfPosRndRaw(iA-contAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      reinfNegRndRaw(iA-contAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	      anchorRndRaw(iA-contAttrFrom, iV).create(eopt.ordEvalNoRandomNormalizers) ;
	   }
   }
   mmatrix<double> CpAe(1, contSize, 0.0), CpAp(1, contSize, 0.0),
	               CpAn(1, contSize, 0.0), CnAe(1, contSize, 0.0),
				   CnAp(1, contSize, 0.0), CnAn(1, contSize, 0.0),
				   CeAe(1, contSize, 0.0), CeAp(1, contSize, 0.0),
				   CeAn(1, contSize, 0.0) ;

   int i ;

   // we have to compute distances up to the folowing attributes
   discUpper = noDiscrete ;
   numUpper = noNumeric ;

   double distanceSum, normDistance, Adiff, clDiff, aVal ;
   int current, neighbourIdx, iAttr, currentClass ;
   diffEsorted.create(TrainSize) ;
   distanceEarray.create(TrainSize) ;


   mmatrix<double> incCpAe(1, contSize, 0.0), incCpAn(1, contSize, 0.0),
	               incCpAp(1, contSize, 0.0), incCnAe(1, contSize, 0.0),
				   incCnAn(1, contSize, 0.0), incCnAp(1, contSize, 0.0),
	               incCeAe(1, contSize, 0.0), incCeAn(1, contSize, 0.0),
				   incCeAp(1, contSize, 0.0) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main iterative loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)  {

	   current = sampleIdx[iterIdx] ;
       currentClass =  DiscValues(current, 0) ;
      // first we compute distances of  all other examples to current
      computeDistancesOrd(current) ;

      // compute distance factors
      EprepareDistanceFactors(distanceType) ;


		 // initialize
		 incCpAp.init(0.0) ;
         incCpAn.init(0.0) ;
 		 incCpAe.init(0.0) ;
		 incCnAp.init(0.0) ;
         incCnAn.init(0.0) ;
		 incCnAe.init(0.0) ;
		 incCeAp.init(0.0) ;
         incCeAn.init(0.0) ;
		 incCeAe.init(0.0) ;

		 distanceSum = 0.0 ;

         for (i=0 ; i < distanceEarray.filled() ; i++) {
            neighbourIdx = distanceEarray[i].value ;
            normDistance = distanceEarray[i].key ;
            distanceSum += normDistance ;
  		    clDiff = DAdiffSign(0, neighbourIdx, current) ;

            for (iAttr=contAttrFrom ; iAttr < contSize ; iAttr ++) {
               if (iAttr==contAttrTo)
				   iAttr=noNumeric ;
               Adiff = CAdiffSignRamp(iAttr, neighbourIdx, current) ;
			   aVal = NumValues(current, iAttr) ;
			   if ( ! isNAcont(aVal)) {
  			      if (clDiff==0.0) { // same class
				      if (Adiff==0) {
			              incCeAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
			              incCeAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
			              incCeAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else if (clDiff==1.0) { // current from larger class
					   if (Adiff==0) {
			              incCpAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
			              incCpAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
			              incCpAn(0, iAttr) -=  Adiff * normDistance  ;
				       }
				   }
				   else {   // clDiff > 0, current from lower class
					   if (Adiff==0) {
			              incCnAe(0, iAttr) +=  normDistance  ;
				       }
				       else if (Adiff > 0.0) {
			              incCnAp(0, iAttr) +=  Adiff * normDistance  ;
				       }
				       else {
			              incCnAn(0, iAttr) -=  Adiff * normDistance  ;
				       }

				   }
               }
			}  // for all attributes
		 }  // for all nearest

		 // normalization of increments
		 if (distanceSum > 0) {
			for (iAttr=contAttrFrom ; iAttr < contSize ; ++iAttr) {
				if (iAttr==contAttrTo)  // also for random normalizing attributes, in case we do not estimate the whole array
					iAttr=noNumeric ;
				if (isNAcont(NumValues(current, iAttr)))
					continue ;
   	  			iV = 0 ;  // only on attribute level
                CpAp(iV, iAttr) +=  incCpAp(iV, iAttr)/distanceSum ;
	            CpAn(iV, iAttr) +=  incCpAn(iV, iAttr)/distanceSum ;
		        CpAe(iV, iAttr) +=  incCpAe(iV, iAttr)/distanceSum ;
			    CnAp(iV, iAttr) +=  incCnAp(iV, iAttr)/distanceSum ;
			    CnAn(iV, iAttr) +=  incCnAn(iV, iAttr)/distanceSum ;
                CnAe(iV, iAttr) +=  incCnAe(iV, iAttr)/distanceSum ;
			    CeAp(iV, iAttr) +=  incCeAp(iV, iAttr)/distanceSum ;
			    CeAn(iV, iAttr) +=  incCeAn(iV, iAttr)/distanceSum ;
                CeAe(iV, iAttr) +=  incCeAe(iV, iAttr)/distanceSum ;
			}
	     }
   }
   // compute reinforcements, random normalizations and their variance
   double denom, denomR;
   for (iA=contAttrFrom ; iA < contAttrTo ; ++iA) {
	     iV = 0 ;
   	     // positive reinforcement
		 denom = CpAp(iV, iA) + CnAp(iV, iA) + CeAp(iV, iA) ;
         if (denom > 0)
		   reinfPos[iA][iV] = CpAp(iV, iA) / denom ;
		 else reinfPos[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noNumeric + (iA -  - contAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAp(iV, iR) + CnAp(iV, iR) + CeAp(iV, iR) ;
		   if (denomR > 0 && CpAp(iV, iR) > 0)  // otherwise zero
		       reinfPosRndRaw(iA-contAttrFrom,iV)[rn] = (CpAp(iV, iR) / denomR)  ;
		   else reinfPosRndRaw(iA-contAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfPosRndRaw(iA-contAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfPosRnd(iA,iV), eopt.ordEvalNormalizingPercentile, reinfPos[iA][iV]) ;

		 // negative reinforcement
		 denom = CpAn(iV, iA) + CnAn(iV, iA) + CeAn(iV, iA) ;
         if (denom > 0)
		   reinfNeg[iA][iV] = CnAn(iV, iA) / denom ;
		 else reinfNeg[iA][iV] = 0.0 ;
		 for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noNumeric + (iA  - contAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAn(iV, iR) + CnAn(iV, iR) + CeAn(iV, iR) ;
		   if (denomR > 0 && CnAn(iV, iR) > 0)  // otherwise zero
		       reinfNegRndRaw(iA-contAttrFrom,iV)[rn] = (CnAn(iV, iR) / denomR)  ;
		   else reinfNegRndRaw(iA-contAttrFrom,iV)[rn] = 0.0  ;
		 }
	     statOE(reinfNegRndRaw(iA-contAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, reinfNegRnd(iA,iV), eopt.ordEvalNormalizingPercentile, reinfNeg[iA][iV]) ;

		 // anchoring
		 denom = CpAe(iV, iA) + CnAe(iV, iA) + CeAe(iV, iA) ;
         if (denom > 0)
		   anchor[iA][iV] = CeAe(iV, iA) / denom ;
		 else anchor[iA][iV] = 0.0 ;
   	     for (rn=0 ; rn < eopt.ordEvalNoRandomNormalizers ; rn++) {
      	   iR = noNumeric + (iA  - contAttrFrom) * eopt.ordEvalNoRandomNormalizers + rn ;
   	       denomR = CpAe(iV, iR) + CnAe(iV, iR) + CeAe(iV, iR) ;
		   if (denomR > 0 && CeAe(iV, iR) > 0)  // otherwise zero
		       anchorRndRaw(iA-contAttrFrom,iV)[rn] = (CeAe(iV, iR) / denomR)  ;
		   else anchorRndRaw(iA-contAttrFrom,iV)[rn] = 0.0  ;
		 }
 	     statOE(anchorRndRaw(iA-contAttrFrom,iV), eopt.ordEvalNoRandomNormalizers, anchorRnd(iA,iV), eopt.ordEvalNormalizingPercentile, anchor[iA][iV]) ;
   }
}
*/
