#include <cstdlib>
#include <cfloat>

#if defined(_OPENMP)
#include <omp.h>
#endif

#include "general.h"
#include "error.h"
#include "estimator.h"
#include "contain.h"
#include "utils.h"
#include "binpart.h"
#include "options.h"

using namespace std ;


// ***************************************************************************
//
//                     estimate
//     estimate selected attributes with chosen measure
//     and returns the index and type of the best estimated attribute
//
// ***************************************************************************
int estimation::estimate(int selectedEstimator, int contAttrFrom, int contAttrTo,
                         int discAttrFrom, int discAttrTo, attributeCount &bestType) {

   if (eopt.binaryEvaluation) {
	  // estimate all of them as binary, so first prepare binarized attributes, then come here again
      eopt.binaryEvaluation = mFALSE ;
      estBinarized(selectedEstimator, contAttrFrom, contAttrTo, discAttrFrom, discAttrTo, discAttrTo) ;
	  eopt.binaryEvaluation = mTRUE ;
   }
   else if (isMyopic(selectedEstimator)){ // these estimate only discrete non-Relief-like attributes
  		 int beforeEstimator = eopt.selectionEstimator ;
  		 eopt.selectionEstimator = selectedEstimator ;

  		 prepareImpurityFunction(selectedEstimator) ; // set the right function pointer
  		 //int idx ;
  		 //construct discAttrib ;
 		 // discrete attributes
         #pragma omp parallel for
  		 for (int idx=discAttrFrom ; idx < discAttrTo ; idx++) {
			  DiscEstimation[idx] = estImpurityDisc(idx) ;
		 }

  		 // numeric attributes
    	 if (eopt.binaryEvaluateNumericAttributes)
  		 {
    		 // binarize numeric attributes and the estimate of the best split
  	      	 // is the estimate of the attribute
    		double result ;
  			construct contAttrib ;
            #pragma omp parallel for private(contAttrib,result)
            for (int idx=contAttrFrom ; idx < contAttrTo ; idx++)
  			{
     		   contAttrib.init(fTree) ;
  			   contAttrib.createSingle(idx, aCONTINUOUS) ;
               if (selectedEstimator == estMDLsmp)
  			     splitPoint[idx] = impuritySplitSample(contAttrib, result) ;
  			   else
  			     splitPoint[idx] = impuritySplit(contAttrib, result) ;
  			   NumEstimation[idx] = result ;
  			}
  		 }
  		 else { // this is an expensive operation, we have to discretize
  			marray<double> Bounds ;
            #pragma omp parallel for private(Bounds)
  			for (int idx=contAttrFrom ; idx < contAttrTo ; idx++)	{
  			   NumEstimation[idx] = discretizeGreedy(idx, 0, Bounds, discAttrTo) ;
  			}
  		 }
  		 eopt.selectionEstimator = beforeEstimator ;
       }
       else {
    	   // estimate ReliefF and its variants
	   switch (selectedEstimator)  // for nominal attributes
	   {
		   case estReliefFkEqual:
		   case estReliefFexpRank:
		   case estReliefFdistance:
		   case estReliefFsqrDistance:
				   ReliefF(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				   break ;

		   case estReliefFbestK:
				   ReliefFbestK(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo,selectedEstimator) ;
				   break ;

		   case estRelief:
				   Relief(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo) ;
				   break ;
 		   case estReliefFmerit:
					   ReliefFmerit(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo,2) ;
					   break ;
 	      case estReliefKukar:
 				   ReliefFcostKukar(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo) ;
 				   break ;
 		  case estReliefFexpC:
				   ReliefFexpC(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
                   break ;
          case estReliefFavgC:
				   ReliefFavgC(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				   break ;

          case estReliefFpe:
				   ReliefFpe(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				   break ;
	      case estReliefFpa:
				   ReliefFpa(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				   break ;
	      case estReliefFsmp:
				   ReliefFsmp(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				   break ;
		  default:  merror("estimation::estimate", "selected estimator is out of range") ;
	   }
    }

   // find best attribute
   double bestContEst = - DBL_MAX, bestDiscEst = - DBL_MAX ;
   int i, bestContIdx = -1, bestDiscIdx = -1 ;
   for (i=contAttrFrom ; i < contAttrTo; i++)
   {
      if (NumEstimation[i] > bestContEst)
      {
          bestContEst =  NumEstimation[i] ;
          bestContIdx = i ;
      }
   }
   for (i=discAttrFrom ; i < discAttrTo; i++)
   {
      if (DiscEstimation[i] > bestDiscEst)
      {
         bestDiscEst =  DiscEstimation[i] ;
         bestDiscIdx = i ;
      }
   }
   if (bestContEst > bestDiscEst)
   {
      bestType = aCONTINUOUS ; // numeric
      return bestContIdx ;
   }
   else
   {
      bestType = aDISCRETE ; // discrete
      return bestDiscIdx ;
   }
}


// ***************************************************************************
//
//                     estimateConstruct
//     estimate selected constructs with choosen measure
//     and returns the index and type of the best estimated attribute
//     the description of constructs serves for possible complexity based measures
//
// ***************************************************************************
int estimation::estimateConstruct(int selectedEstimator,
    int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo,
    attributeCount &bestType)
{
   return estimate(selectedEstimator, contAttrFrom,contAttrTo,
                       discAttrFrom, discAttrTo, bestType) ;
}


void estimation::prepareImpurityFunction(int selectedEstimator) {
	preparedEstimator = selectedEstimator ;
	switch (selectedEstimator) {
	   case estInfGain:
		   fImpurity = &estimation::infGainImpurity ;
		   fImpurityGain = &estimation::infGain ;
		   break ;
	   case estGainRatio:
		   fImpurity = &estimation::infGainImpurity ;
		   fImpurityGain = &estimation::gainRatio ;
		   break ;
	   case estGainRatioCost:
		   fImpurity = &estimation::infGainCostImpurity ;
		   fImpurityGain = &estimation::gainRatio ;
		   break ;
	   case estMDL:
	   case estMDLsmp:
		   fImpurity = &estimation::MDLimpurity ;
		   fImpurityGain = &estimation::MDLgain ;
		   break ;
	   case estGini:
		   fImpurity = &estimation::giniImpurity ;
		   fImpurityGain = &estimation::giniGain ;
		   break ;
	   case estMyopicReliefF:
		   fImpurity = &estimation::giniImpurity ;
		   fImpurityGain = &estimation::ReliefMyopicFast ;
		   break ;
	   case estAccuracy:
		   fImpurity = &estimation::accuracyImpurity ;
		   fImpurityGain = &estimation::accuracyGain ;
		   break ;
	   case estDKM:
		   fImpurity = &estimation::DKMImpurity ;
		   fImpurityGain = &estimation::DKMgain ;
		   break ;
	   case estDKMcost:
		   fImpurity = &estimation::DKMcostImpurity ;
		   fImpurityGain = &estimation::DKMgain ;
		   break ;
	   case estUniformDKM:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityUniform = &estimation::DKMonDistribution ;
		   fImpurityGain = &estimation::gainUniform ;
		   break ;
	   case estUniformGini:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityUniform = &estimation::giniOnDistribution ;
		   fImpurityGain = &estimation::gainUniform ;
		   break ;
	   case estUniformInf:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityUniform = &estimation::infOnDistribution ;
		   fImpurityGain = &estimation::gainUniform ;
		   break ;
	   case estUniformAccuracy:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityUniform = &estimation::accuracyOnDistribution ;
		   fImpurityGain = &estimation::accUniform ;
		   break ;
	   case estEqualDKM:
		   fImpurity = &estimation::DKMImpurity ;
		   fImpurityGain = &estimation::EqualDKM ;
		   break ;
	   case estEqualGini:
		   fImpurity = &estimation::giniImpurity ;
		   fImpurityGain = &estimation::giniEqual ;
		   break ;
	   case estEqualInf:
		   fImpurity = &estimation::infGainImpurity ;
		   fImpurityGain = &estimation::infEqual ;
		   break ;
	   case estImpurityHellinger:
		   fImpurity = &estimation::EuclidHellingerImpurity ;
		   fImpurityGain = &estimation::distanceImpGain ;
		   break ;
	   case estImpurityEuclid:
		   fImpurity = &estimation::EuclidHellingerImpurity ;
		   fImpurityGain = &estimation::distanceImpGain ;
		   break ;
	   case estEqualHellinger:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::EqualHellinger ;
		   break ;
	   case estDistHellinger:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::distMulticlassEvaluation ;
		   fDistStep = &estimation::stepHellinger ;
		   break ;
	   case estDistEuclid:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::distMulticlassEvaluation ;
		   fDistStep = &estimation::stepEuclid ;
		   break ;
	   case estDistAUC:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::distMulticlassEvaluation ;
		   fDistStep = &estimation::stepAUC ;
		   break ;
	   case estDistAngle:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::distMulticlassEvaluation ;
		   fDistStep = &estimation::stepAngle ;
		   break ;
/*
	   case estDistHellinger:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::DistHellinger ;
		   break ;
	   case estDistEuclid:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::DistEuclid ;
		   break ;
	   case estDistAUC:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::DistAUC ;
		   break ;
	   case estDistAngle:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::DistAngle ;
		   break ;
*/
/* case estBhattacharyyaImp:
		   fImpurity = &estimation::BhattacharyyaImpurity ;
		   fImpurityGain = &estimation::BhattacharyyaImpFast ;
		   break ;
	   case estBhattacharyya:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::BhattacharyyaFast ;
		   break ;
	   case estBhattacharyyaCond:
		   fImpurity = &estimation::zeroImpurity ;
		   fImpurityGain = &estimation::BhattacharyyaCond ;
		   break ;
*/
	   default:merror("estimation::prepareImpurityFunction", "forbidden estimator");

	}
}



// ***************************************************************************
//
//                       CVVilalta
//                       ---------
//
//         computes measure of problem difficulty called concept variation
//              (Vilalta, R., 1999)
//
//
// ***************************************************************************
double estimation::CVVilalta(int contAttrFrom, int contAttrTo,
                                    int discAttrFrom, int discAttrTo)
{

   const double alpha = 2.0 ;


   double NoUsed = contAttrTo - contAttrFrom + discAttrTo - discAttrFrom;


   // we have to compute distances up to the following attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double weightSum, w, sigma, ConVar = 0.0 , distance, denominator ;
   int current, m ;


   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {

       if (NoIterations == TrainSize)
          current = iterIdx ;
       else
           current =  randBetween(0, TrainSize) ;

       // first we compute distances of all other examples to current
      computeDistances(current) ;

     weightSum = 0.0 ;
     sigma = 0.0 ;
     for (m=0 ; m < NoIterations ; m++)
     {
        if (m==current)
           continue ;

        distance = CaseDistance(m) ;
        denominator = NoUsed - distance ;
        if (denominator > epsilon)
          w = 1.0 / pow(2.0, alpha * distance/denominator) ;
        else
          w = 0.0 ;

        weightSum += w ;

        sigma += w * DiscDistance(m, 0) ;
     }

     ConVar += sigma/weightSum ;

  }

  return ConVar/double(NoIterations) ;

}


// ***************************************************************************
//
//                       CVmodified
//                       ---------
//
//         computes measure of problem difficulty called concept variation
//          based on our modification
//
//
// ***************************************************************************
double estimation::CVmodified(int contAttrFrom, int contAttrTo,
                                    int discAttrFrom, int discAttrTo)
{


   int NoUsed = contAttrTo - contAttrFrom + discAttrTo - discAttrFrom;


   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double ConVar = 0.0, incConVar ;
   int current, i, iDisc, iCont, k ;
   sortRec tempSort ;
   marray<sortRec> distSort(TrainSize) ;


   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {

       if (NoIterations == TrainSize)
          current = iterIdx ;
       else
           current =  randBetween(0, TrainSize) ;

       // first we compute distances of all other examples to current
      computeDistances(current) ;

      //  sort all the examples with descending distance
      distSort.clear() ;
      for (i=0 ; i < TrainSize; i++)
      {
        if (i==current)  // we skip current example
          continue ;
        tempSort.key =  CaseDistance(i) ;
        tempSort.value = i ;
        distSort.addEnd(tempSort) ;
      }

      distSort.qsortAsc() ;

      for (iDisc=discAttrFrom ; iDisc < discAttrTo ; iDisc++)
      {
         incConVar = 0.0 ;
         k = 0 ;
         for (i=0 ; i < distSort.filled() ; i++)
            if (DiscDistance(distSort[i].value,iDisc) > 0)
            {
                incConVar += DiscDistance(distSort[i].value, 0) ;
                k++ ;
                if (k >= kNearestEqual)
                  break ;
            }
         if (k > 0)
             ConVar += incConVar / double(k) ;
      }

      for (iCont=contAttrFrom ; iCont < contAttrTo ; iCont++)
      {
         incConVar = 0.0 ;
         k = 0 ;
         for (i=0 ; i < distSort.filled() ; i++)
            if (NumDistance(distSort[i].value, iCont) > 0)
            {
                incConVar += DiscDistance(distSort[i].value, 0) ;
                k++ ;
                if (k >= kNearestEqual)
                   break ;
            }
         if (k>0)
             ConVar += incConVar / double(k) ;
       }
   }

  return ConVar/double(NoIterations)/double(NoUsed) ;
}


//************************************************************
//
//                        estImpurityDisc
//                        --------------
//
//     evaluates discrete attribute with one of with selected impurity estimators
//     assuming pointer to it is set already
//
//************************************************************
double estimation::estImpurityDisc(int discIdx)
{
   marray<int> noAttrVal(discNoValues[discIdx]+1, 0) ;
   noAttrVal.setFilled(discNoValues[discIdx]+1) ;
   mmatrix<int> noClassAttrVal(noClasses+1, discNoValues[discIdx]+1, 0) ;
   int j, iC, iV;
   int OKvalues = 0 ;
   int attrValue, classValue ;
   for (j=0 ; j < TrainSize ; j++)
   {
      attrValue = DiscValues(j, discIdx) ;
      if (attrValue != NAdisc) {
    	  classValue = DiscValues(j, 0) ;
          noClassAttrVal(classValue, attrValue)++ ;
          OKvalues ++ ;
      }
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
      return -DBL_MAX ;

   // store sum over all attribute values in index 0
   for (iC = 1; iC <= noClasses; iC++){
	   noClassAttrVal(iC,0)=0 ;
	   for (iV = 1 ; iV <= discNoValues[discIdx] ; ++iV) {
		   noClassAttrVal(iC,0) += noClassAttrVal(iC,iV);
		   noAttrVal[iV] += noClassAttrVal(iC,iV);
	   }
   }
   int noValidSplits = 0 ;
   for (iV = 1 ;  iV <= discNoValues[discIdx] ; ++iV){
	   if (noAttrVal[iV] >0)
		    noValidSplits++ ;
   }
   if (noValidSplits <=1)
	   return -DBL_MAX ;

   double priorImpurity = (this->*fImpurity)(OKvalues, noClassAttrVal, 0) ;

   return (this->*fImpurityGain)(priorImpurity, OKvalues, noAttrVal, noClassAttrVal) ;
}



//************************************************************
//
//                        impuritySplit
//                        --------------
//
//     finds best split for numeric attribute with selected impurity estimator
//
//************************************************************
double estimation::impuritySplit(construct &nodeConstruct, double &bestEstimation)
{
   marray<sortRec> sortedAttr(TrainSize) ;
   marray<int> noAttrVal(2+1, 0) ; // number of cases in each branch i.e. the sum over class values for each branch in noClassAttrVal, index 0 unused
   noAttrVal.setFilled(3) ;
   mmatrix<int> noClassAttrVal(noClasses+1, 2+1, 0) ; // number of cases for each class and branch, indexes 0 unused
   int j ;
   int OKvalues = 0 ;
   double attrValue ;
   for (j=0 ; j < TrainSize ; j++)
   {
      attrValue = nodeConstruct.continuousValue(DiscValues, NumValues, j) ;
      if (isNAcont(attrValue))
        continue ;
      sortedAttr[OKvalues].key = attrValue ;
      sortedAttr[OKvalues].value = j ;
      noClassAttrVal(DiscValues(j, 0), 2)++ ; // initially we store all for the right side
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      bestEstimation = - DBL_MAX ;
      return - DBL_MAX ; // smaller than any value, so all examples will go into one branch
   }
   double priorImpurity = (this->*fImpurity)(OKvalues, noClassAttrVal, 2) ;
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.qsortAsc() ;
   bestEstimation = - DBL_MAX ;
   double est = 0, splitValue = - DBL_MAX ; // smaller than any value, so all examples will go into one branch
   // initially we move some left instance from right to left
   for (j=0 ; j < eopt.minNodeWeightEst ; j++) {
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 1)++ ; // increase on the left
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 2)-- ;  // decrease on right
   }
   int upperLimit = int(OKvalues - eopt.minNodeWeightEst) ;
   for ( ; j < upperLimit ; j++)
   {
   	   // only estimate for unique values 
       if (sortedAttr[j].key != sortedAttr[j-1].key) {
          //compute heuristic measure
    	  noAttrVal[1] = j ;
    	  noAttrVal[2] = OKvalues - j ;
    	  est = (this->*fImpurityGain)(priorImpurity, OKvalues, noAttrVal, noClassAttrVal) ;
    	  if (est > bestEstimation) {
    		  bestEstimation = est ;
     		  splitValue = (sortedAttr[j].key + sortedAttr[j-1].key)/2.0 ;
    	  }
       }
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 1)++ ; // increase on the left
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 2)-- ;  // decrease on right
   }
   return splitValue ;
}


//************************************************************
//
//                        impuritySplitSample
//                        --------------
//
//     finds best split for numeric attribute with selected impurity estimator
//     based on cost based sampling
//
//************************************************************
double estimation::impuritySplitSample(construct &nodeConstruct, double &bestEstimation)
{
    // first estimate class probabilities and prepare sample
	// start with number of examples belonging to each of the classes
	marray<int> noExInClass(noClasses+1);
	marray<double> probClass(noClasses+1);
	noExInClass.init(0) ;
	probClass.init(0.0) ;
	int i, idx;
	for (i=0; i < TrainSize; i++) {
		noExInClass[ DiscValues(i,0) ]++;
		probClass[ DiscValues(i,0) ] += weight[i];
	}
	double wAll = 0.0;
	for (idx=1; idx <= noClasses; idx++)
		wAll += probClass[idx];
	// compute estimations of class value probabilities with their relative frequencies
	for (idx=1; idx <= noClasses; idx++)
		probClass[idx] = probClass[idx] / wAll;

	// prepare order of iterations
	marray<int> sampleIdx(TrainSize);
	stratifiedExpCostSample(sampleIdx, NoIterations, TrainSize, probClass, noExInClass) ;

   // following code is identical to impuritySplit function except for sampled data
   marray<sortRec> sortedAttr(TrainSize) ;
   marray<int> noAttrVal(2+1, 0) ;
   noAttrVal.setFilled(3) ;
   mmatrix<int> noClassAttrVal(noClasses+1, 2+1, 0) ;
   int j ;
   int OKvalues = 0 ;
   double attrValue ;
   for (j=0 ; j < TrainSize ; j++)
   {
      attrValue = nodeConstruct.continuousValue(DiscValues, NumValues, sampleIdx[j]) ;
      if (isNAcont(attrValue))
        continue ;
      sortedAttr[OKvalues].key = attrValue ;
      sortedAttr[OKvalues].value = sampleIdx[j] ;
      noClassAttrVal(DiscValues(sampleIdx[j], 0), 2)++ ; // initially we store all for the right side
      OKvalues ++ ;
   }
   if (OKvalues <= 1)    // all the cases have missing value of the attribute or only one OK
   {
      bestEstimation = - DBL_MAX ;
      return - DBL_MAX ; // smaller than any value, so all examples will go into one branch
   }
   double priorImpurity = (this->*fImpurity)(OKvalues, noClassAttrVal, 2) ;
   sortedAttr.setFilled(OKvalues) ;
   sortedAttr.qsortAsc() ;
   bestEstimation = - DBL_MAX ;
   double est = 0, splitValue = - DBL_MAX ; // smaller than any value, so all examples will go into one branch

  // initially we move some left instance from right to left
   for (j=0 ; j < eopt.minNodeWeightEst ; j++) {
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 1)++ ; // increase on the left
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 2)-- ;  // decrease on right
   }
   int upperLimit = int(OKvalues - eopt.minNodeWeightEst) ;
   for ( ; j < upperLimit ; j++)
   {
   	   // only estimate for unique values 
       if (sortedAttr[j].key != sortedAttr[j-1].key) {
          //compute heuristic measure
    	  noAttrVal[1] = j ;
    	  noAttrVal[2] = OKvalues - j ;
    	  est = (this->*fImpurityGain)(priorImpurity, OKvalues, noAttrVal, noClassAttrVal) ;
    	  if (est > bestEstimation) {
    		  bestEstimation = est ;
     		  splitValue = (sortedAttr[j].key + sortedAttr[j-1].key)/2.0 ;
    	  }
       }
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 1)++ ; // increase on the left
	   noClassAttrVal(DiscValues(sortedAttr[j].value, 0), 2)-- ;  // decrease on right
   }
   return splitValue ;
}



// ***************************************************************************
//
//                     estimateSelected
//    - estimate selected attributes with chosen measure
//    - and returns the index and type of the best estimated attribute
//    - intended only for fast estimation with RF and  with non-Releif measures
//    - attributes are ranked and only first  noSelected shall be estimated, but
//        invalid attributes are not numbered
//
//
// ***************************************************************************
int estimation::estimateSelected(marray<int> &rankList, int noSelected, attributeCount &bestType) {
	//int selectedEstimator = activeEstimator ;
	attributeCount bT ; // dummy
	double bestEst = - DBL_MAX;
	int bestIdx = -1, iRL=1, iA;
	while (iRL <= noSelected) {
		iA = rankList[iRL] ;
		if (fTree->AttrDesc[iA].continuous) {
			estimate(eopt.selectionEstimator, fTree->AttrDesc[iA].tablePlace, fTree->AttrDesc[iA].tablePlace +1, 0, 0, bT) ;
			if (NumEstimation[fTree->AttrDesc[iA].tablePlace] > bestEst){
				bestEst = NumEstimation[fTree->AttrDesc[iA].tablePlace] ;
				bestType = aCONTINUOUS ;
				bestIdx = fTree->AttrDesc[iA].tablePlace ;
			}
			else if (NumEstimation[fTree->AttrDesc[iA].tablePlace] == -DBL_MAX) {
				// invalid attribute
				if (noSelected < rankList.filled()-1)
					++noSelected ;
			}
		}
		else {
			estimate(eopt.selectionEstimator, 0, 0, fTree->AttrDesc[iA].tablePlace, fTree->AttrDesc[iA].tablePlace +1,  bT) ;
			if (DiscEstimation[fTree->AttrDesc[iA].tablePlace] > bestEst){
				bestEst = DiscEstimation[fTree->AttrDesc[iA].tablePlace] ;
				bestType = aDISCRETE ;
				bestIdx = fTree->AttrDesc[iA].tablePlace ;
			}
			else if (DiscEstimation[fTree->AttrDesc[iA].tablePlace] == -DBL_MAX) {
				// invalid attribute
				if (noSelected < rankList.filled()-1)
					++noSelected ;
			}
		}
		++iRL ;
	}
	return bestIdx ;
}
booleanT estimation::isMyopic(int selectedEstimator) {
	if (selectedEstimator == estInfGain || selectedEstimator == estGainRatio
			|| selectedEstimator == estMDL || selectedEstimator == estGini	|| selectedEstimator == estMyopicReliefF
			|| selectedEstimator == estAccuracy
			|| selectedEstimator == estDKM || selectedEstimator == estDKMcost
			|| selectedEstimator == estGainRatioCost || selectedEstimator == estMDLsmp || selectedEstimator == estImpurityEuclid
			|| selectedEstimator == estImpurityHellinger || selectedEstimator == estEqualHellinger || selectedEstimator == estDistHellinger
			// || selectedEstimator == estBhattacharyyaImp || selectedEstimator == estBhattacharyya || selectedEstimator == estBhattacharyyaCond
			|| selectedEstimator == estUniformDKM || selectedEstimator == estUniformGini || selectedEstimator == estUniformInf || selectedEstimator == estUniformAccuracy
			|| selectedEstimator == estEqualDKM|| selectedEstimator == estEqualGini|| selectedEstimator == estEqualInf
			|| selectedEstimator == estDistAUC|| selectedEstimator == estDistEuclid|| selectedEstimator == estDistAngle
	     )
		return mTRUE;
	else
		return mFALSE;
}

double estimation::infGainImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
   double tempP, Hc = 0.0 ;
   for (int classIdx=1 ; classIdx <= noClasses ;classIdx++)	   {
	      if (noClassAttrVal(classIdx, valIdx) > 0)
	      {
	         tempP = ((double)noClassAttrVal(classIdx, valIdx)) / weightNode ;
	         Hc -= tempP * mlog2(tempP) ;
	      }
   }
   return Hc ;
}
double estimation::infOnDistribution(marray<double> &dist) {
   double Hc = 0.0 ;
   for (int classIdx=1 ; classIdx <= noClasses ;classIdx++)	   {
	      if (dist[classIdx]>0.0)
	         Hc -= dist[classIdx] * mlog2(dist[classIdx]) ;
   }
   return Hc ;
}

double estimation::accuracyOnDistribution(marray<double> &dist) {
   int maxClassIdx = 1 ;
   for (int classIdx=2 ; classIdx <= noClasses ;classIdx++)
	   if (dist[classIdx] > dist[maxClassIdx])
		    	  maxClassIdx = classIdx ;
   return dist[maxClassIdx] ;
}


// computation of gain ratio
double estimation::gainRatio(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double tempP, Ha=0.0, Hc_a=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
	   tempP = ((double)attrVal[valIdx])/ weightNode ;
	   Ha -=  tempP * mlog2(tempP);
	   if (attrVal[valIdx] >0)
          Hc_a += tempP * (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
	if (Ha >0)
       return (priorImpurity - Hc_a) / Ha ;
	else return -1.0 ;
}

// computation of Informaion gain
double estimation::infGain(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double tempP, Hc_a=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
	   tempP = double(attrVal[valIdx])/ weightNode ;
	   if (attrVal[valIdx] >0)
       Hc_a += tempP * (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return (priorImpurity - Hc_a)  ;
}

// computation of uniform Informaion gain
double estimation::infEqual(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double Hc_a=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		if (attrVal[valIdx] >0)
          Hc_a +=  (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return -Hc_a  ;
}

// computation of accuracy
double estimation::accuracyGain(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double tempP, acc=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
	   tempP = double(attrVal[valIdx])/ weightNode ;
	   if (attrVal[valIdx] >0)
         acc += tempP * (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return acc-priorImpurity  ;
}

double estimation::accuracyImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
   int maxClassIdx = 1 ;
   for (int classIdx=2 ; classIdx <= noClasses ;classIdx++)
	      if (noClassAttrVal(classIdx, valIdx) > noClassAttrVal(maxClassIdx, valIdx))
	    	  maxClassIdx = classIdx ;
   return double(noClassAttrVal(maxClassIdx, valIdx))/weightNode ;
}

double estimation::DKMImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
      int maxClassIdx = 1 ;
      for (int classIdx=2 ; classIdx <= noClasses ;classIdx++)
   	      if (noClassAttrVal(classIdx, valIdx) > noClassAttrVal(maxClassIdx, valIdx))
   	    	  maxClassIdx = classIdx ;
      double q = double(noClassAttrVal(maxClassIdx, valIdx))/weightNode ;
      return  2.0 * sqrt(q*(1.0-q)) ;
}
double estimation::DKMonDistribution(marray<double> &dist) {
      int maxClassIdx = 1 ;
      for (int classIdx=2 ; classIdx <= noClasses ;classIdx++)
   	      if (dist[classIdx] > dist[maxClassIdx])
   	    	  maxClassIdx = classIdx ;
      return  2.0 * sqrt(dist[maxClassIdx]*(1.0-dist[maxClassIdx])) ;
}

// computation of DKM
double estimation::DKMgain(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double tempP, dkm=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
	   tempP = double(attrVal[valIdx])/ weightNode ;
	   if (attrVal[valIdx] >0)
          dkm += tempP * (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return (priorImpurity - dkm)  ;
}

double estimation::EqualDKM(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  dkm=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		if (attrVal[valIdx] >0)
           dkm += (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return -dkm  ;
}
// top level estimator for uniform priors, calling appropriate distribution function
double estimation::gainUniform(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double pvj, gain=0.0  ;
	int i,valIdx;
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	marray<double> dist(noClasses+1, 0);
	for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		pvj = 0 ;
	    for (i=1 ; i <= noClasses ;i++)
		  if (noClassAttrVal(i,0) > 0) {
		     pvj += double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0);
	    }

	    if (pvj>0){
	    	dist.init(0.0);
	      for (i=1 ; i <= noClasses ;i++) {
		    if (noClassAttrVal(i,0) > 0)
		     dist[i] = double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0)/pvj;
	      }
	      pvj /= noClasses ;
	      gain += pvj * (this->*fImpurityUniform)(dist) ;
	    }
	}
	return 1.0-gain ; ;
}
// top level estimator for uniform accuracy, calling appropriate distribution function
double estimation::accUniform(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double pvj, gain=0.0  ;
	int i,valIdx;
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	marray<double> dist(noClasses+1, 0);
	for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		pvj = 0 ;
	    for (i=1 ; i <= noClasses ;i++)
		  if (noClassAttrVal(i,0) > 0) {
		     pvj += double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0);
	    }

	    if (pvj>0){
	    	dist.init(0.0);
	      for (i=1 ; i <= noClasses ;i++) {
		    if (noClassAttrVal(i,0) > 0)
		     dist[i] = double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0)/pvj;
	      }
	      pvj /= noClasses ;
	      gain += pvj * (this->*fImpurityUniform)(dist) ;
	    }
	}
	return gain-priorImpurity ; ;
}



double estimation::giniImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
      double pc2 = 0.0 ;
      for (int classIdx=1 ; classIdx <= noClasses ;classIdx++)
          pc2 += sqr(double(noClassAttrVal(classIdx,valIdx))/weightNode) ;
      return  pc2 ; // the actual impurity is 1- pc2, but this is handled in gain function giniGain
}

double estimation::giniOnDistribution(marray<double> &dist) {
      double pc2 = 0.0 ;
      for (int classIdx=1 ; classIdx <= noClasses ;classIdx++)
          pc2 += sqr(dist[classIdx]) ;
      return  1-pc2 ; // the actual impurity is 1- pc2, but this is handled in the calling function
}

double estimation::giniGain(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double tempP, gini=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
	   tempP = double(attrVal[valIdx])/ weightNode ;
	   if (attrVal[valIdx] >0)
         gini += tempP * (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return (gini - priorImpurity )  ;
}

double estimation::giniEqual(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double gini=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		if (attrVal[valIdx] > 0)
       gini += (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return gini  ;
}



double estimation::zeroImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
  return 0.0 ;
}

// for two class problem
// extended to multi-class problem by all pairs of classes difference
//double estimation::hellingerImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
//     double hi = 0.0 ;
//     int i, j ;
//     for (i=1 ; i <= noClasses ;i++)
//    	 for (j=i+1 ; j <= noClasses ;j++)
//           hi += sqr(sqrt(double(noClassAttrVal(i,valIdx))/weightNode) - sqrt(double(noClassAttrVal(j,valIdx))/weightNode)) ;
//     return  hi ;
//}

// for two class problem
// extended to multi-class problem by all pairs of classes difference
double estimation::EuclidHellingerImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
	double da, t ;
	switch (eopt.multiclassEvaluation) {
	case 1: // average over all-pairs
	case 3: // average over one-against-all
		da = 0.0 ;
		break;
	case 2: // maximum over all pairs
	case 4: // maximum over one-against-all
		da = - DBL_MAX ;
		break ;
	default: merror("estimation::EuclidHellingerImpurity","invalid multi-class extension") ;
		return -1 ;
	}
	int i, j, noComb=0 ;
	if (eopt.multiclassEvaluation==1 || eopt.multiclassEvaluation==2) {
		for (i=1 ; i <= noClasses ;i++){
			//if (noClassAttrVal(i,valIdx)>0) {
				for (j=i+1 ; j <= noClasses ;j++) {
					//if (noClassAttrVal(j,valIdx)>0) {
						if (preparedEstimator == estImpurityEuclid)
							t =  sqr(double(noClassAttrVal(i,valIdx))/weightNode - double(noClassAttrVal(j,valIdx))/weightNode ) ;
						else if (preparedEstimator == estImpurityHellinger)
							t= sqr(sqrt(double(noClassAttrVal(i,valIdx))/weightNode) - sqrt(double(noClassAttrVal(j,valIdx))/weightNode)) ;
						else {
							merror("estimation::EuclidHellingerImpurity","invalid estimator detected") ;
							t = -1.0 ;
						}
						if (eopt.multiclassEvaluation==1) {
							// average of all-pairs
							da +=t ;
							++noComb;
						}
						else if (eopt.multiclassEvaluation==2) 	{
							// best of all-pairs
							if (t > da)
								da = t ;
						}
					//}
				}
			//}
		}
	}
	else if (eopt.multiclassEvaluation==3 || eopt.multiclassEvaluation==4){
		for (i=1 ; i <= noClasses ;i++) {
			//if (noClassAttrVal(i,valIdx)>0) {
				// form the virtual others class with index 0
				noClassAttrVal(0,valIdx) = 0 ;
				for (j=1 ; j <= noClasses ;j++)
					if (j != i)
						noClassAttrVal(0,valIdx) += noClassAttrVal(j, valIdx) ;
				//if (noClassAttrVal(0,valIdx)>0) {
					if (preparedEstimator==estImpurityEuclid)
						t =  sqr(double(noClassAttrVal(i,valIdx))/weightNode - double(noClassAttrVal(0,valIdx))/weightNode ) ;
					else if (preparedEstimator == estImpurityHellinger)
						t= sqr(sqrt(double(noClassAttrVal(i,valIdx))/weightNode) - sqrt(double(noClassAttrVal(0,valIdx))/weightNode)) ;
					else {
						merror("estimation::EuclidHellingerImpurity","invalid estimator detected") ;
						t = -1.0 ;
				    }
					if (eopt.multiclassEvaluation == 3) {
						// average of all-pairs
						da +=t ;
						++noComb;
				    }
					else if (eopt.multiclassEvaluation == 4) {
						// best of all-pairs
						if (t > da)
							da = t ;
				    }
			   //}
			//}
		}
	}
	switch (eopt.multiclassEvaluation) {
	case 1: // average over all-pairs
	case 3: // average over one-against-all
		if (noComb > 0)
		  return da / double(noComb) ;
		else return - DBL_MAX ;
	case 2: // maximum over all pairs
	case 4: // maximum over one-against-all
		return da ;
	default: merror("estimation::EuclidHellingerImpurity","invalid multi-class extension") ;
		return -1 ;
	}
}

// variant of distribution distance between two classes in each split
// extended to multi-class problem by  summing over all all pairs of classes
double estimation::distanceImpGain(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  h=0.0  ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		if (attrVal[valIdx] >0)
           h += (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return sqrt(h)  ;
}

// variant of distribution distance between different splits
// extended to multi-split problem by  averaging over all pairs of attribute splits
double estimation::EqualHellinger(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  h=0.0,hi  ;
	int c, vi,vj ;
	int noComb = 0 ;
	for (vi = 1 ; vi < attrVal.filled() ; vi++){
		if (attrVal[vi]>0) {
			for (vj = vi+1 ; vj < attrVal.filled() ; vj++){
				if (attrVal[vj]>0) {
					++noComb ;
					hi = 0.0 ;
					for (c=1 ; c <= noClasses ;c++) {
						hi += sqr(sqrt(double(noClassAttrVal(c,vi))/attrVal[vi]) - sqrt(double(noClassAttrVal(c,vj))/attrVal[vj])) ;
					}
					h += sqrt(hi) ;
				}
			}
		}
	}
	if (noComb>0)
		return h / double(noComb) ; // average over all combinations of splits
	else return -DBL_MAX ;
}


double estimation::MDLimpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
 	  marray<double> Multinom(noClasses) ;
 	  // encoding number of examples in each class
      for (int classIdx=1 ; classIdx <= noClasses ;classIdx++)
         Multinom[classIdx-1] = noClassAttrVal(classIdx,valIdx) ;
      Multinom.setFilled(noClasses) ;
      double MDL = multinomLog2(Multinom) ;

      // encoding prior decoder
      Multinom[0] = noClasses  -1 ;
      Multinom[1] = weightNode ;
      Multinom.setFilled(2) ;
      MDL += multinomLog2(Multinom) ;

      return MDL;
}

double estimation::MDLgain(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double mdl=0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
       mdl +=  (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    return (priorImpurity - mdl)/weightNode ;
}

double estimation::ReliefMyopicFast(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	// prior impurity is sum of p_c^2
	double tempP, giniPost=0.0, pEqualA = 0.0 ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
 	   tempP = double(attrVal[valIdx])/ weightNode ;
 	   pEqualA += sqr(tempP) ;
       giniPost +=  sqr(tempP) * (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    giniPost = giniPost/pEqualA - priorImpurity ;
    return pEqualA/priorImpurity/(1.0 - priorImpurity) * giniPost ;
}


double estimation::DKMcostImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
      marray<double> pC(noClasses+1, 0) ;  // store class probabilities
      int i, j ;
      for (i=1 ; i <= noClasses ;i++)
    	  pC[i] = double(noClassAttrVal(i, valIdx))/weightNode ;
      marray<double> eC(noClasses+1, 0) ; //expected cost
      double denom, eCsum = 0.0 ;
      for (i=1 ; i<=noClasses ; i++) {
        for (j=1 ; j<=noClasses ; j++)
            if (j != i)
              eC[i] +=  pC[j]* fTree->CostMatrix(i, j) ;
        denom = (1.0 - pC[i]) ;
        if (denom > 0)
        	eC[i] /= denom ;
        else
        	eC[i] = 0.0 ;
        eCsum +=  pC[i] * eC[i] ;
      }
      double q = -1.0 ;
      marray<double> pC1(noClasses+1, 0) ; // weightNodeed probability
      for (i=1 ; i <= noClasses ;i++)   {
         pC1[i] = pC[i] * eC[i] / eCsum ;
         if (pC1[i] > q)
            q = pC1[i] ;
      }
      return  2.0 * sqrt(q*(1.0-q)) ;
}

double estimation::infGainCostImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
   int i, j;
	// probabilities of the classes
   marray<double> pC(noClasses+1, double(0)) ;
   for (i=1 ; i <= noClasses ;i++)
      pC[i] = double(noClassAttrVal(i,valIdx)) / weightNode ;

   marray<double> eC(noClasses+1, 0) ;
   double denom, eCsum = 0.0 ;
   for (i=1 ; i<=noClasses ; i++) {
     for (j=1 ; j<=noClasses ; j++)
         if (j != i)
           eC[i] +=  pC[j]* fTree->CostMatrix(i, j) ;
     denom = (1.0 - pC[i]) ;
     if (denom > 0)
       eC[i] /= (1.0 - pC[i]) ;
     else eC[i] = 0.0 ;
     eCsum +=  pC[i] * eC[i] ;
   }
   double Ec = 0.0 ;  // entropy
   marray<double> pC1(noClasses+1, 0) ;
   for (i=1 ; i <= noClasses ;i++)   {
      pC1[i] = pC[i] * eC[i] / eCsum ;
      if (pC1[i] > 0 && pC[i] < 1.0)
         Ec -= pC1[i] * mlog2(pC1[i]) ;
      //else merror("estimation::infGainCostImpurity","invalid probability") ;
   }
   return Ec ;
 }


double estimation::stepAngle(int c1, int c2, mmatrix<int> &noClassAttrVal) {
	double sumC1=0, sumC2=0 ;
	for (int valIdx = 1 ; valIdx < noClassAttrVal.getDim2() ; valIdx++) {
		    sumC1 += sqr(double(noClassAttrVal(c1,valIdx)) / noClassAttrVal(c1,0)) ;
		    sumC2 += sqr(double(noClassAttrVal(c2,valIdx)) / noClassAttrVal(c2,0)) ;
	}
	sumC1 = sqrt(sumC1) ;
	sumC2 = sqrt(sumC2) ;
	double di = 0.0 ;
	for (int valIdx = 1 ; valIdx < noClassAttrVal.getDim2() ; valIdx++)
		di += sqr(double(noClassAttrVal(c1,valIdx))/noClassAttrVal(c1,0)/sumC1 - double(noClassAttrVal(c2,valIdx))/noClassAttrVal(c2,0)/sumC2) ;
	return sqrt(di/2.0) ;
}

// variant of Chieslak In Chawla, ECML 2008
double estimation::stepHellinger(int c1, int c2, mmatrix<int> &noClassAttrVal) {
	double di = 0.0 ;
	for (int valIdx = 1 ; valIdx < noClassAttrVal.getDim2() ; valIdx++)
		di += sqr(sqrt(double(noClassAttrVal(c1,valIdx))/noClassAttrVal(c1,0)) - sqrt(double(noClassAttrVal(c2,valIdx))/noClassAttrVal(c2,0))) ;
	return  sqrt(di);
}

double estimation::stepEuclid(int c1, int c2, mmatrix<int> &noClassAttrVal) {
	double di = 0.0 ;
	for (int valIdx = 1 ; valIdx < noClassAttrVal.getDim2() ; valIdx++)
		di += sqr(double(noClassAttrVal(c1,valIdx))/noClassAttrVal(c1,0) - double(noClassAttrVal(c2,valIdx))/noClassAttrVal(c2,0)) ;
	return sqrt(di/2.0) ;
}

double estimation::stepAUC(int c1, int c2, mmatrix<int> &noClassAttrVal) {
	double di = 0.0 ;
	int u, v ;
	for (u = 1 ; u <  noClassAttrVal.getDim2() ; u++)
		for (v = u+1 ; v <  noClassAttrVal.getDim2() ; v++)
			di += fabs(double(noClassAttrVal(c1,u))/noClassAttrVal(c1,0)*noClassAttrVal(c2,v)/noClassAttrVal(c2,0)
			- double(noClassAttrVal(c1,v))/noClassAttrVal(c1,0)*noClassAttrVal(c2,u)/noClassAttrVal(c2,0) ) ;
	return di ;
}


double estimation::distMulticlassEvaluation(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  d=0.0, di  ;
	int i,j,valIdx,noComb=0 ;
	switch (eopt.multiclassEvaluation) {
	case 1: // average over all-pairs
	case 3: // average over one-against-all
		d = 0.0 ;
		break;
	case 2: // maximum over all pairs
	case 4: // maximum over one-against-all
	    d = - DBL_MAX ;
		break ;
	default: merror("estimation::distMulticlassEvaluation","invalid multi-class extension") ;
		return -1 ;
	}

	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	// for distances based on two distributions of conditional probabilities  p(a_valIdx | c_i) and p(a_valIdx | c_j)
	if (eopt.multiclassEvaluation == 1 || eopt.multiclassEvaluation == 2) {  // all-pairs extension
		for (i=1 ; i <= noClasses ;i++) {
			//if (noClassAttrVal(i,0)>0) {
				for (j=i+1 ; j <= noClasses ;j++) {
					//if (noClassAttrVal(j,0) >0) {
						if (eopt.multiclassEvaluation == 1) { // average over all-pairs
							++noComb ;
							d += (this->*fDistStep)(i, j, noClassAttrVal) ;
						}
						else { // multiclassEvaluation == 2 i.e., maximum over all pairs
							di = (this->*fDistStep)(i, j, noClassAttrVal) ;
							if (di > d)
								d = di ;
						}
					//}
				}
			//}
		}
	}
	else if (eopt.multiclassEvaluation == 3 || eopt.multiclassEvaluation == 4) {  // one-against-all extension
		for (i=1 ; i <= noClasses ;i++) {
			//if (noClassAttrVal(i,0)>0) {
				// form the virtual others class with index 0
				noClassAttrVal(0,0) = 0 ;
				for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
					noClassAttrVal(0, valIdx) = 0 ;
					for (j=1 ; j <= noClasses ;j++)
						if (j != i)
							noClassAttrVal(0,valIdx) += noClassAttrVal(j, valIdx) ;
					noClassAttrVal(0,0) += noClassAttrVal(0,valIdx) ;
				}
				//if (noClassAttrVal(0,0) >0) {
					if (eopt.multiclassEvaluation == 3) { // average over one-against-all
						++noComb ;
						d += (this->*fDistStep)(i, 0, noClassAttrVal) ;
					}
					else { // multiclassEvaluation == 4 i.e., maximum over one-against-all
						di = (this->*fDistStep)(i, 0, noClassAttrVal) ;
						if (di > d)
							d = di ;

					}
				//}
			}
		//}
	}

	switch (eopt.multiclassEvaluation) {
	case 1: // average over all-pairs
	case 3: // average over one-against-all
		if (noComb>0)
			return d / double(noComb) ;
		else return -DBL_MAX ;
	case 2: // maximum over all pairs
	case 4: // maximum over one-against-all
		return d ;
	default: merror("estimation::distMulticlassEvaluation","invalid multi-class extension") ;
		return -1 ;
	}

}



/*  obsolete estimation code

// variant of distribution distance between conditional probabilities in different splits
//  this is a variant of Chieslak & Chawla, 2008
// extended to multi-class problem by  averaging over all all pairs of classes
double estimation::DistHellinger(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  h=0.0, hi  ;
	int i,j,valIdx,noComb=0 ;
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	// hellinger based on conditional probabilities
	for (i=1 ; i <= noClasses ;i++) {
		if (noClassAttrVal(i,0)>0) {
			for (j=i+1 ; j <= noClasses ;j++) {
				if (noClassAttrVal(j,0) >0) {
					hi = 0.0 ;
					for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
						hi += sqr(sqrt(double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0)) - sqrt(double(noClassAttrVal(j,valIdx))/noClassAttrVal(j,0))) ;
					++noComb ;
					h+=sqrt(hi) ;
				}
			}
		}
	}
	return h / double(noComb) ; ;
}


// Euclidean distance of distribution
// extended to multi-class problem by  averaging over all pairs of classes
double estimation::DistEuclid(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  d=0.0, di  ;
	int i,j,valIdx,noComb=0 ;
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	// euclidean distance based on two distributions of conditional probabilities
	//  p(a_valIdx | c_i) and p(a_valIdx | c_j)
	for (i=1 ; i <= noClasses ;i++) {
		if (noClassAttrVal(i,0)>0) {
			for (j=i+1 ; j <= noClasses ;j++) {
				if (noClassAttrVal(j,0) >0) {
					di = 0.0 ;
					for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
						di += sqr(double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0) - double(noClassAttrVal(j,valIdx))/noClassAttrVal(j,0)) ;
					++noComb ;
					d+=sqrt(di/2.0) ;
				}
			}
		}
	}
	return d / double(noComb) ; ;
}


// AUC distance of distribution
// extended to multi-class problem by  averaging over all pairs of classes
double estimation::DistAUC(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  d=0.0, di  ;
	int i,j,u,v,valIdx,noComb=0 ;
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	// AUC distance based on two distributions of conditional probabilities
	//  p(a_u | c_i) and p(a_u | c_j)
	for (i=1 ; i <= noClasses ;i++) {
		if (noClassAttrVal(i,0)>0) {
			for (j=i+1 ; j <= noClasses ;j++) {
				if (noClassAttrVal(j,0) >0) {
					di = 0.0 ;
					for (u = 1 ; u < attrVal.filled() ; u++)
						for (v = u+1 ; v < attrVal.filled() ; v++)
						  di += fabs(double(noClassAttrVal(i,u))/noClassAttrVal(i,0)*noClassAttrVal(j,v)/noClassAttrVal(j,0)
								   - double(noClassAttrVal(i,v))/noClassAttrVal(i,0)*noClassAttrVal(j,u)/noClassAttrVal(j,0) ) ;
					++noComb ;
					d += di ;
				}
			}
		}
	}
	return d / double(noComb) ; ;
}


// cosine of angle distance
// extended to multi-class problem by  averaging over all pairs of classes
double estimation::DistAngle(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  d=0.0, di, sumSqr  ;
	int i,j,valIdx,noComb=0 ;
	marray<double> sum(noClasses+1, 0.0);
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
		if (noClassAttrVal(i,0)>0) {
			sumSqr = 0 ;
			for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
		      sumSqr += sqr(double(noClassAttrVal(i,valIdx)) / noClassAttrVal(i,0)) ;
			sum[i] = sqrt(sumSqr) ;
		}
	}
	// cosine of angle distance based on two distributions of conditional probabilities
	//  p(a_valIdx | c_i) and p(a_valIdx | c_j)
	for (i=1 ; i <= noClasses ;i++) {
		if (noClassAttrVal(i,0)>0) {
			for (j=i+1 ; j <= noClasses ;j++) {
				if (noClassAttrVal(j,0) >0) {
					di = 0.0 ;
					for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
						di += sqr(double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0)/sum[i] - double(noClassAttrVal(j,valIdx))/noClassAttrVal(j,0)/sum[j]) ;
					++noComb ;
					d+=sqrt(di/2.0) ;
				}
			}
		}
	}
	return d / double(noComb) ; ;
}


double estimation::BhattacharyyaImpurity(int weightNode, mmatrix<int> &noClassAttrVal, int valIdx) {
     double hi = 0.0 ;
     int i, j ;
     for (i=1 ; i <= noClasses ;i++)
    	 for (j=i+1 ; j <= noClasses ;j++)
           hi += sqrt(double(noClassAttrVal(i,valIdx))/weightNode * double(noClassAttrVal(j,valIdx))/weightNode) ;
     return  hi ;
}
double estimation::BhattacharyyaImpFast(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  h=0.0  ;
    for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++) {
		if (attrVal[valIdx] > 0.0)
          h += (this->*fImpurity)(attrVal[valIdx], noClassAttrVal, valIdx) ;
    }
    return -log(h)  ;
}

// variant of distribution distance between different splits
// extended to multi-split problem by  averaging over all all pairs of attribute splits
double estimation::BhattacharyyaFast(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  h=0.0,hi  ;
	int c, vi,vj ;
	int noComb = 0 ;
	for (vi = 1 ; vi < attrVal.filled() ; vi++){
		if (attrVal[vi]>0) {
		for (vj = vi+1 ; vj < attrVal.filled() ; vj++) {
			if (attrVal[vj]>0) {
			hi=0.0;
			for (c=1 ; c <= noClasses ;c++)
				hi += sqrt(double(noClassAttrVal(c,vi))/attrVal[vi] * double(noClassAttrVal(c,vj))/attrVal[vj]) ;
			h += -log(hi) ; // for two split s this is it
			noComb++ ;
			}
		}
	}
	}
   return h / double(noComb) ; // average over all combinations of attribute splits
}

// variant of distribution distance between conditional probabilities in different splits
// extended to multiclass problem by  averaging over all all pairs of classes
double estimation::BhattacharyyaCond(double priorImpurity, int weightNode, marray<int> &attrVal, mmatrix<int> &noClassAttrVal){
	double  h=0.0, hi  ;
	int i,j,valIdx,noComb=0 ;
	// compute unconditional class probabilities
	for (i=1 ; i <= noClasses ;i++) {
		noClassAttrVal(i,0) = 0 ;
		for (valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
			noClassAttrVal(i,0) += noClassAttrVal(i,valIdx) ;
	}
	// hellinger based on conditional probabilities
	for (i=1 ; i <= noClasses ;i++) {
		if (noClassAttrVal(i,0)>0) {
			for (j=i+1 ; j <= noClasses ;j++) {
				if (noClassAttrVal(j,0) >0) {
					hi = 0.0 ;
					for (int valIdx = 1 ; valIdx < attrVal.filled() ; valIdx++)
						hi += sqrt(double(noClassAttrVal(i,valIdx))/noClassAttrVal(i,0) * double(noClassAttrVal(j,valIdx))/noClassAttrVal(j,0)) ;
					++noComb ;
					h+= -log(hi) ;
				}
			}
		}
	}
	return h / double(noComb) ; ;
}


// ***************************************************************************
//
//                      ReliefMyopic
//                      ------------
//
//        estimator myopic Relief (Gini corelated)
//
// ***************************************************************************
void estimation::ReliefMyopic(int discAttrFrom, int discAttrTo)
{

   // prepare estimations array
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // int NoDiscEstimated = discAttrTo - discAttrFrom ;


   marray<int> noExInClass(noClasses+1, 0) ;

   // number of examples belonging to each of the classes
   int i;
   for (i=0 ; i < TrainSize ; i++)
      noExInClass[ DiscValues(i,0) ]++ ;

   // probabilities of the classes
   double pEqualC = 0.0 ;
   int classIdx ;
   for (classIdx=1 ; classIdx <= noClasses ;classIdx++)
      pEqualC += sqr(double(noExInClass[classIdx]) / double(TrainSize)) ;

   if (pEqualC == 0.0 || pEqualC == 1.0)
   {
      DiscEstimation.init(discAttrFrom,discAttrTo,-1.0) ;
      return ;
   }

   double pEqualA, GiniR, condSum ;
   int valIdx, noOK ;
   int discIdx ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
	  noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
     valNo.create(discNoValues[discIdx]+1, 0) ;


	  // compute number of examples with each value of attribute and class
	  for (i=0 ; i < TrainSize ; i++)
        noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
       {
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;
       }
     }
     noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }

     // probability of equal attribute values
     pEqualA = 0.0 ;
	  for (valIdx = 1 ; valIdx <= discNoValues[discIdx]  ; valIdx++)
     {
        pEqualA +=   sqr(valNo[valIdx]/double(noOK)) ;
     }

     // computation of Gini'
     GiniR = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {

        condSum = 0.0 ;
        if (valNo[valIdx] > 0)
        {
          for (classIdx = 1 ; classIdx <= noClasses ; classIdx++)
             condSum += sqr(double(noClassAttrVal(classIdx,valIdx))/double(valNo[valIdx])) ;
        }

        GiniR += sqr(valNo[valIdx]/double(noOK)) * condSum ;

     }
     GiniR = GiniR / pEqualA - pEqualC ;

     DiscEstimation[discIdx] = pEqualA/pEqualC/(1.0 - pEqualC) * GiniR ;

   }

}


// ***************************************************************************
//
//                      Gini
//                      ----
//
//        estimator Gini index
//
// ***************************************************************************
void estimation::Gini(int discAttrFrom, int discAttrTo)
{

   // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // int NoDiscEstimated = discAttrTo - discAttrFrom ;


   marray<int> noExInClass(noClasses+1, 0) ;

   // number of examples belonging to each of the classes
   int i;
   for (i=0 ; i < TrainSize ; i++)
      noExInClass[ DiscValues(i,0) ]++ ;

   // probabilities of the classes
   double pEqualC = 0.0 ;
   int classIdx ;
   for (classIdx=1 ; classIdx <= noClasses ;classIdx++)
      pEqualC += sqr(double(noExInClass[classIdx]) / double(TrainSize)) ;

   if (pEqualC == 0.0 || pEqualC == 1.0)
   {
      DiscEstimation.init(discAttrFrom,discAttrTo,-1.0) ;
      return ;
   }

   double Gini, condSum ;
   int valIdx, noOK ;
   int discIdx ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
	  noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
      valNo.create(discNoValues[discIdx]+1, 0) ;

	  // compute number of examples with each value of attribute and class
	  for (i=0 ; i < TrainSize ; i++)
        noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
       {
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;
       }
     }
     noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }


     // computation of Gini
     Gini = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {

        condSum = 0.0 ;
        if (valNo[valIdx] > 0)
        {
          for (classIdx = 1 ; classIdx <= noClasses ; classIdx++)
             condSum += sqr(double(noClassAttrVal(classIdx,valIdx))/double(valNo[valIdx])) ;
        }

        Gini += valNo[valIdx]/double(noOK) * condSum ;

     }
     Gini -= pEqualC ;

     DiscEstimation[discIdx] = Gini ;

   }

}



void estimation::mdl(int discAttrFrom, int discAttrTo)
{
   // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // int NoDiscEstimated = discAttrTo - discAttrFrom ;


   marray<int> noExInClass(noClasses+1, 0) ;

   // number of examples belonging to each of the classes
   int i, classIdx;
   for (i=0 ; i < TrainSize ; i++)
      noExInClass[ DiscValues(i,0) ]++ ;

   marray<double> Multinom(noClasses) ;

   // encoding prior number of examples in each class
   for (classIdx=1 ; classIdx <= noClasses ;classIdx++)
      Multinom[classIdx-1] = noExInClass[classIdx] ;
   Multinom.setFilled(noClasses) ;
   double priorMDL = multinomLog2(Multinom) ;

   // encoding prior decoder
   Multinom[0] = noClasses  -1 ;
   Multinom[1] = TrainSize ;
   Multinom.setFilled(2) ;
   priorMDL += multinomLog2(Multinom) ;


   // compute postMDL
   int valIdx, noOK ;
   int discIdx ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   double postClass, postDecoder ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
      noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
      valNo.create(discNoValues[discIdx]+1, 0) ;

	   // compute number of examples with each value of attribute and class
	   for (i=0 ; i < TrainSize ; i++)
         noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
       {
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;
       }
     }
     noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }
     // computation of postMDL
     postClass = postDecoder = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {

        if (valNo[valIdx] > 0)
        {
          for (classIdx = 1 ; classIdx <= noClasses ; classIdx++)
             Multinom[classIdx-1] = noClassAttrVal(classIdx,valIdx) ;
          Multinom.setFilled(noClasses) ;
          postClass += multinomLog2(Multinom) ;

          Multinom[0] = noClasses - 1 ;
          Multinom[1] = valNo[valIdx] ;
          Multinom.setFilled(2) ;
          postDecoder += multinomLog2(Multinom) ;
        }
     }

     DiscEstimation[discIdx] = (priorMDL - postClass - postDecoder) / double(TrainSize) ;

   }
}

void estimation::gainRatio(int discAttrFrom, int discAttrTo)
{
     // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   marray<int> noExInClass(noClasses+1, 0) ;

   // number of examples belonging to each of the classes
   int i;
   for (i=0 ; i < TrainSize ; i++)
      noExInClass[ DiscValues(i,0) ]++ ;

   // probabilities of the classes
   double Hc = 0.0 ;
   double tempP ;
   int classIdx ;
   for (classIdx=1 ; classIdx <= noClasses ;classIdx++)
   {
      if (noExInClass[classIdx] > 0)
      {
         tempP = double(noExInClass[classIdx]) / double(TrainSize) ;
         Hc -= tempP * log2(tempP) ;
      }
   }

   double Hca, Ha ;
   int valIdx, noOK ;
   int discIdx ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
	   noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
      valNo.create(discNoValues[discIdx]+1, 0) ;

	  // compute number of examples with each value of attribute and class
	  for (i=0 ; i < TrainSize ; i++)
        noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
       {
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;
       }
     }
     noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }

     // computation of Informaion gain
     Hca = Ha = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {

        if (valNo[valIdx] > 0)
        {
          for (classIdx = 1 ; classIdx <= noClasses ; classIdx++)
             if (noClassAttrVal(classIdx,valIdx) > 0)
             {
                tempP = double(noClassAttrVal(classIdx,valIdx))/double(noOK) ;
                Hca -= tempP * log2(tempP) ;
             }

          if (valNo[valIdx] != noOK)
          {
             tempP = double(valNo[valIdx]) / double(noOK) ;
             Ha -= tempP * log2(tempP) ;
          }
        }
     }
     if (Ha > 0.0)
       DiscEstimation[discIdx] = (Hc + Ha - Hca) / Ha ;
     else
       DiscEstimation[discIdx] = -1.0 ;
   }

}

void estimation::infGain(int discAttrFrom, int discAttrTo)
{
   // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // int NoDiscEstimated = discAttrTo - discAttrFrom ;


   marray<int> noExInClass(noClasses+1, 0) ;

   // number of examples belonging to each of the classes
   int i;
   for (i=0 ; i < TrainSize ; i++)
      noExInClass[ DiscValues(i,0) ]++ ;

   // probabilities of the classes
   double Hc = 0.0 ;
   double tempP ;
   int classIdx ;
   for (classIdx=1 ; classIdx <= noClasses ;classIdx++)
   {
      if (noExInClass[classIdx] > 0)
      {
         tempP = double(noExInClass[classIdx]) / double(TrainSize) ;
         Hc -= tempP * log2(tempP) ;
      }
   }

   double Hca, Ha ;
   int valIdx, noOK ;
   int discIdx ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
	   noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
      valNo.create(discNoValues[discIdx]+1, 0) ;

	  // compute number of examples with each value of attribute and class
	  for (i=0 ; i < TrainSize ; i++)
        noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
       {
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;
       }
     }
     noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }

     // computation of Informaion gain
     Hca = Ha = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {

        if (valNo[valIdx] > 0)
        {
          for (classIdx = 1 ; classIdx <= noClasses ; classIdx++)
             if (noClassAttrVal(classIdx,valIdx) > 0)
             {
                tempP = double(noClassAttrVal(classIdx,valIdx))/double(noOK) ;
                Hca -= tempP * log2(tempP) ;
             }

          tempP = double(valNo[valIdx]) / double(noOK) ;
          Ha -= tempP * log2(tempP) ;
        }
     }
     DiscEstimation[discIdx] = Hc + Ha - Hca ;

   }

}





// ***************************************************************************
//
//                      Accuracy
//                      --------
//
//        estimator Accuracy
//
// ***************************************************************************
void estimation::Accuracy(int discAttrFrom, int discAttrTo)
{

   // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // int NoDiscEstimated = discAttrTo - discAttrFrom ;

   double Accuracy ;
   int discIdx, classIdx, maxClassIdx, valIdx, i, noOK ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
      noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
      valNo.create(discNoValues[discIdx]+1, 0) ;

	  // compute number of examples with each value of attribute and class
	  for (i=0 ; i < TrainSize ; i++)
        noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
       {
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;
       }
     }
     noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }


     // computation of Accuracy
     Accuracy = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
     {

        if (valNo[valIdx] > 0)
        {
          maxClassIdx = 1 ;
          for (classIdx = 2 ; classIdx <= noClasses ; classIdx++)
             if (noClassAttrVal(classIdx,valIdx) > noClassAttrVal(maxClassIdx,valIdx) )
                maxClassIdx = classIdx ;

          Accuracy += double(noClassAttrVal(maxClassIdx,valIdx))/double(noOK) ;
        }

     }

     DiscEstimation[discIdx] = Accuracy ;

   }

}


// ***************************************************************************
//
//                      BinAccuracy
//                      -----------
//
//        estimator binarized accuracy (accuracy on a binary split)
//
// ***************************************************************************
void estimation::BinAccuracy(int discAttrFrom, int discAttrTo)
{

   // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // int NoDiscEstimated = discAttrTo - discAttrFrom ;

   int discIdx, i, j, maxSample ;
   mmatrix<int> noClassAttrVal(noClasses+1, 3) ;

   if (eopt.discretizationSample==0)
     maxSample = TrainSize -1;
   else
      maxSample = eopt.discretizationSample ;


   double est, maxEst, maxRound ;
   int greedyPositions, exhaustivePositions, idxRound, filled ;
   int attrValue ;
   marray<int> dataColumn(TrainSize) ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
      if (discNoValues[discIdx] > 2)  // demands binarization
      {
         // binarize attributes and the estimate of the best binarization is the estimate of the attribute
         binPartition Generator( discNoValues[discIdx]) ;
         greedyPositions = discNoValues[discIdx] * (discNoValues[discIdx]+1)/2 ;
         if ( discNoValues[discIdx] < maxVal4ExhDisc)
			 exhaustivePositions = Generator.noPositions() ;
		 else
			 exhaustivePositions = -1 ; // invalid value
         if ( (discNoValues[discIdx] < maxVal4ExhDisc) && (exhaustivePositions * 0.8 <= greedyPositions || exhaustivePositions < maxSample))
         {
            // exhaustive search
            maxEst = -1.0 ;
            while (Generator.increment() )
            {
               // compute data column
               for (i=0 ; i < TrainSize ; i++)
               {
                  attrValue = DiscValues(i, discIdx) ;
                  if (attrValue == NAdisc)
                     dataColumn[i] = NAdisc ;
                  else
                     if (Generator.leftPartition[attrValue])
                        dataColumn[i] = 1 ;
                      else
                        dataColumn[i] = 2 ;
               }
               noClassAttrVal.init(0) ;

          	  // compute number of examples with each value of attribute and class
      	     for (i=0 ; i < TrainSize ; i++)
                noClassAttrVal(DiscValues(i, 0), dataColumn[i] ) ++ ;

              est = binAccEst(noClassAttrVal, 2) ;
              if (est > maxEst)
                 maxEst = est ;
            }
         }
         else { // greedy search

            marray<booleanT> currentBest(discNoValues[discIdx]+1, mFALSE) ;
            maxEst = -1.0 ;
            for (filled=1 ; filled < discNoValues[discIdx] ; filled++)
            {
               maxRound = -1.0 ;
               idxRound = -1 ;
               for (j=1 ; j <= discNoValues[discIdx]; j++)
               if (currentBest[j] == mFALSE)
               {
                  currentBest[j] = mTRUE ;

                  // compute data column
                  for (i=0 ; i < TrainSize ; i++)
                  {
                     attrValue = DiscValues(i, discIdx) ;
                    if (attrValue == NAdisc)
                       dataColumn[i] = NAdisc ;
                    else
                     if (currentBest[attrValue])
                        dataColumn[i] = 1 ;
                      else
                        dataColumn[i] = 2 ;
                  }
                  noClassAttrVal.init(0) ;

                  // compute number of examples with each value of attribute and class
      	         for (i=0 ; i < TrainSize ; i++)
                    noClassAttrVal(DiscValues(i, 0), dataColumn[i] ) ++ ;

                  est = binAccEst(noClassAttrVal, 2) ;
                  if (est > maxRound)
                  {
                     maxRound = est ;
                     idxRound = j ;
                  }
                  currentBest[j] = mFALSE ;
               }
               if (maxRound > maxEst)
                    maxEst = maxRound ;
               currentBest[idxRound] = mTRUE ;
            }
         }
         DiscEstimation[discIdx] = maxEst ;

      }
      else
      {
        noClassAttrVal.init(0) ;

   	  // compute number of examples with each value of attribute and class
	     for (i=0 ; i < TrainSize ; i++)
          noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

        DiscEstimation[discIdx] = binAccEst(noClassAttrVal, 2) ;
      }
   }

}


double estimation::binAccEst(mmatrix<int> &noClassAttrVal, int noValues)
{
   marray<int> valNo(noValues+1, 0) ;
   int valIdx, classIdx ;
   // compute number of examples with each value of attribute
   for (valIdx = 0 ; valIdx <= noValues ; valIdx++)
       for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;

   int noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
   if (noOK <= 0 )
      return -1.0 ;

   // computation of accuracy
    double AccuracyEst = 0.0 ;
    int maxClassIdx ;
    for (valIdx = 1 ; valIdx <= noValues ; valIdx++)
    {
       if (valNo[valIdx] > 0)
       {
         maxClassIdx = 1 ;
         for (classIdx = 2 ; classIdx <= noClasses ; classIdx++)
            if (noClassAttrVal(classIdx,valIdx) > noClassAttrVal(maxClassIdx,valIdx) )
               maxClassIdx = classIdx ;

         AccuracyEst += double(noClassAttrVal(maxClassIdx,valIdx))/double(noOK) ;
       }
    }
    return AccuracyEst ;
}

// ***************************************************************************
//
//                      DKM
//                      ----
//
//        estimator Dietterich, Kearns, Mansour (DKM): theoretically justified
//        in authors ICML'96 paper; basically it is  impurity function
//        G(q)=2*sqrt(q*(1-q)) where q represents probabillity of the majority class
//
// ***************************************************************************
void estimation::DKM(int discAttrFrom, int discAttrTo)
{

   // prepare estimations arrays
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // int NoDiscEstimated = discAttrTo - discAttrFrom ;


   marray<int> noExInClass(noClasses+1, 0) ;

   // number of examples belonging to each of the classes
   int i;
   for (i=0 ; i < TrainSize ; i++)
      noExInClass[ DiscValues(i,0) ]++ ;

   // probability of the majority class
   double q, DKMprior ;
   int maxEx = noExInClass[1] ;
   int classIdx ;
   for (classIdx=2 ; classIdx <= noClasses ; classIdx++)
      if (noExInClass[classIdx] > maxEx)
          maxEx = noExInClass[classIdx] ;
   q = double(maxEx)/TrainSize ;

   if (q == 0.0 || q == 1.0)
   {
      DiscEstimation.init(discAttrFrom,discAttrTo,-1.0) ;
      return ;
   }
   else DKMprior = 2.0 * sqrt(q*(1.0-q)) ;

   double DKMpost, qCond ;
   int valIdx, noOK ;
   int discIdx ;
   mmatrix<int> noClassAttrVal ;
   marray<int> valNo ;
   for (discIdx = discAttrFrom ; discIdx < discAttrTo ; discIdx++)
   {
	  noClassAttrVal.create(noClasses+1, discNoValues[discIdx]+1, 0) ;
      valNo.create(discNoValues[discIdx]+1, 0) ;

	  // compute number of examples with each value of attribute and class
	  for (i=0 ; i < TrainSize ; i++)
        noClassAttrVal(DiscValues(i, 0), DiscValues(i, discIdx) ) ++ ;

	  // compute number of examples with each value of attribute
	  for (valIdx = 0 ; valIdx <= discNoValues[discIdx] ; valIdx++)
        for (classIdx = 1 ; classIdx <= noClasses ; classIdx ++)
          valNo[valIdx] += noClassAttrVal(classIdx, valIdx) ;

      noOK = TrainSize - valNo[0] ;  // we do not take missing values into account
     if (noOK <= 0 )
     {
        DiscEstimation[discIdx] = -1.0 ;
        continue ;
     }

     // computation of DKM
     DKMpost = 0.0 ;
     for (valIdx = 1 ; valIdx <= discNoValues[discIdx] ; valIdx++)
        if (valNo[valIdx] > 0) {
          // find majority value
          maxEx = noClassAttrVal(1,valIdx) ;
          for (classIdx = 2 ; classIdx <= noClasses ; classIdx++)
             if (noClassAttrVal(classIdx,valIdx)>maxEx)
                 maxEx = noClassAttrVal(classIdx,valIdx) ;
           qCond = double(maxEx)/ valNo[valIdx] ;
           if (qCond > 0.0 && qCond < 1.0)
             DKMpost += double(valNo[valIdx])/noOK * sqrt(qCond*(1.0-qCond)) ;
        }
     DiscEstimation[discIdx] = DKMprior-2*DKMpost ;

   }

}
*/
