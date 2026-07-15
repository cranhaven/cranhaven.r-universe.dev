#include <cstdlib>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "estimatorReg.h"
#include "contain.h"
#include "utils.h"
#include "binpart.h"

using namespace std ;


// ***************************************************************************
//
//                     estimate
//     estimate selected attributes with chosen measure
//     and returns the index and type of the best estimated attribute
//
// ***************************************************************************
int estimationReg::estimate(int selectedEstimator, int contAttrFrom, int contAttrTo,
                         int discAttrFrom, int discAttrTo, attributeCount &bestType)
{

   if (eopt.binaryEvaluation)
   {
      eopt.binaryEvaluation = mFALSE ;
      estBinarized(selectedEstimator, contAttrFrom, contAttrTo, discAttrFrom, discAttrTo, discAttrTo) ;
	  eopt.binaryEvaluation = mTRUE ;
   }
   else {

	   switch (selectedEstimator)
	   {
		   case estRReliefFbestK:
				RReliefFbestK(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo,selectedEstimator) ;
				break;

		   case estRReliefFexpRank:
		   case estRReliefFkEqual:  // Relief with density and with equal K-nearest
		   case estRReliefFdistance:
		   case estRReliefFsqrDistance:
				//RReliefF(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo,selectedEstimator) ;
				CReliefDensity(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				break ;

		   case estRReliefFwithMSE: // combination of Relief and MSE
				Combination(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, estRReliefFexpRank) ;
				break ;
  		   case estMSEofMean: // standard deviation
				MSE(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo) ;
				break ;

		   case estMSEofModel: // MSE of model
				MEofModel(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				break ;

		   case estMAEofModel:  // mean absolute error of model
				MEofModel(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo, selectedEstimator) ;
				break ;

		   default:  merror("estimationReg::estimate", "selected estimator is out of range") ;

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
      bestType = aCONTINUOUS ; // continuous
      return bestContIdx ;
   }
   else
   {
      bestType = aDISCRETE ; // discrete
      return bestDiscIdx ;
   }
}

booleanT estimationReg::isMyopic(int selectedEstimator) {
	if (selectedEstimator == estMSEofMean  || selectedEstimator == estMSEofModel
			|| selectedEstimator == estMAEofModel)
		return mTRUE;
	else
		return mFALSE;
}


// ***************************************************************************
//
//                     estimateConstruct
//     estimate selected constructs with choosen measure
//     and returns the index and type of the best estimated attribute
//
// ***************************************************************************
int estimationReg::estimateConstruct(int selectedEstimator,
    int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo,
    attributeCount &bestType, marray<constructReg> &DiscConstruct,
    marray<constructReg> &ContConstruct)
{
   switch (selectedEstimator)
   {
      case estRReliefFbestK:
      case estRReliefFexpRank:
      case estRReliefFdistance:
      case estRReliefFsqrDistance:
      case estRReliefFkEqual:
      case estRReliefFwithMSE:
      case estMSEofMean:
      case estMSEofModel:
      case estMAEofModel:
              return estimate(selectedEstimator, contAttrFrom,contAttrTo,
                              discAttrFrom, discAttrTo, bestType) ;
      /*
      case estRReliefFconstr: // MDL with RREliefF
         {
            #if defined(DEBUG)
              if (DiscConstruct.filled() != discAttrTo - discAttrFrom ||
                  ContConstruct.filled() != contAttrTo - contAttrFrom)
                 merror("estimationReg::estimateConstruct", "call with invlid parameters") ;
            #endif
               CReliefDensity(contAttrFrom, contAttrTo, discAttrFrom,
                              discAttrTo, 2) ;
              int i ;
              for (i=discAttrFrom ; i < discAttrTo ; i++)
              {
                 if (discDiffA[i-discAttrFrom] + epsilon > 1.0)
                    discDiffA[i-discAttrFrom] = 1.0 - epsilon ;
                 DiscEstimation[i] *= (1.0 - diffC) * diffC/(1.0 - discDiffA[i-discAttrFrom])  ;
                 DiscEstimation[i] *= -Gini2EntropyConst ;
                 DiscEstimation[i] += DiscConstruct[i-discAttrFrom].mdlConstructCode() ;
                 DiscEstimation[i] *= -1.0 ;
              }
              for (i=contAttrFrom ; i < contAttrTo ; i++)
              {
                 if (contDiffA[i-contAttrFrom] + epsilon > 1.0)
                    contDiffA[i-contAttrFrom] = 1.0 - epsilon ;
                 NumEstimation[i] *= (1.0 - diffC) * diffC/(1.0 - contDiffA[i-contAttrFrom])  ;
                 NumEstimation[i] *= -Gini2EntropyConst ;
                 NumEstimation[i] += ContConstruct[i-contAttrFrom].mdlConstructCode() ;
                 NumEstimation[i] *= -1.0 ;
              }
         }
         break ;
      case estMSEmeanConstr: // MDL with MSE
           {
            #if defined(DEBUG)
              if (DiscConstruct.filled() != discAttrTo - discAttrFrom ||
                  ContConstruct.filled() != contAttrTo - contAttrFrom)
                 merror("estimationReg::estimateConstruct", "call with invlid parameters") ;
            #endif
              MSE(contAttrFrom,contAttrTo,discAttrFrom,discAttrTo) ;
              int i ;
              for (i=discAttrFrom ; i < discAttrTo ; i++)
              {
                 DiscEstimation[i] += priorMSE ;
                 DiscEstimation[i] *= -Gini2EntropyConst ;
                 DiscEstimation[i] += DiscConstruct[i-discAttrFrom].mdlConstructCode() ;
                 DiscEstimation[i] *= -1.0 ;
              }
              for (i=contAttrFrom ; i < contAttrTo ; i++)
              {
                 NumEstimation[i] += priorMSE ;
                 NumEstimation[i] *= -Gini2EntropyConst ;
                 NumEstimation[i] += ContConstruct[i-contAttrFrom].mdlConstructCode() ;
                 NumEstimation[i] *= -1.0 ;
              }
           }
           break ;
       */
       default:  merror("estimationReg::estimateConstruct", "selected estimator is out of range") ;

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
      bestType = aCONTINUOUS ; // continuous
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
//                     adjustTables
//        prepare tables for increased number of attributes
//
// ***************************************************************************
void estimationReg::adjustTables(int newContSize, int newDiscSize)
{
   if (newContSize > currentNumSize)
   {
      NumValues.addColumns(newContSize) ;
      NumEstimation.enlarge(newContSize) ;
      // NumDistance.addColumns(newContSize) ;
      splitPoint.enlarge(newContSize) ;

      minValue.enlarge(newContSize) ;
      maxValue.enlarge(newContSize) ;
      valueInterval.enlarge(newContSize) ;
      step.enlarge(newContSize) ;
      NAnumValue.enlarge(newContSize) ;

#if defined(RAMP_FUNCTION)
      DifferentDistance.enlarge(newContSize) ;
      EqualDistance.enlarge(newContSize) ;
      CAslope.enlarge(newContSize) ;
#endif

      currentNumSize = newContSize ;

   }

   if (newDiscSize > currentDiscSize)
   {
      DiscValues.addColumns(newDiscSize) ;
      DiscEstimation.enlarge(newDiscSize) ;
      // DiscDistance.addColumns(newDiscSize) ;
      discNoValues.enlarge(newDiscSize) ;

      NAdiscValue.enlarge(newDiscSize) ;

      currentDiscSize = newDiscSize ;
   }
}


// ***************************************************************************
//
//                         MSE
//        computing estimate by mean square error
//
// ***************************************************************************
void estimationReg::MSE(int contAttrFrom, int contAttrTo,
                               int discAttrFrom, int discAttrTo)
{

   // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo, 0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo, 0.0) ;
   splitPoint.init(contAttrFrom,contAttrTo, DBL_MAX) ;

   int i, j ;
   marray<double> valueClass ;
   marray<double> valueWeight ;
   marray<double> squaredValues ;
   marray<sortRec> sortedMean ;
   int idx, OKvalues ;
   double totalWeight, value, bestEstimate, estimate, pLeft, variance ;
   double LeftValues, LeftSquares, LeftWeight, RightValues, RightSquares, RightWeight ;
   // estimationReg of discrete attributtes

   for (i=discAttrFrom ; i < discAttrTo ; i++)
   {
      valueClass.create(discNoValues[i]+1, 0.0) ;
      valueWeight.create(discNoValues[i]+1, 0.0) ;
      squaredValues.create(discNoValues[i] + 1, 0.0) ;
      for (j=0 ; j < TrainSize ; j++)
      {
         idx = DiscValues(j,i) ;
         value = NumValues(j,0) ;
         valueClass[idx] += weight[j]* value ;
         valueWeight[idx] += weight[j] ;
         squaredValues[idx] += weight[j] * sqr(value) ;
      }
      sortedMean.create(discNoValues[i]) ;
      RightWeight = RightSquares = RightValues = 0.0 ;
      OKvalues = 0 ;
      for (j=1 ; j <= discNoValues[i] ; j++)
      {
         if (valueWeight[j] > epsilon)
         {
            sortedMean[OKvalues].key = valueClass[j] / valueWeight[j] ;
            sortedMean[OKvalues].value = j ;
            OKvalues ++ ;

            RightWeight += valueWeight[j] ;
            RightSquares +=  squaredValues[j] ;
            RightValues += valueClass[j] ;
         }
      }
      totalWeight = RightWeight ;
      sortedMean.setFilled(OKvalues) ;
      sortedMean.qsortAsc() ;
      bestEstimate = DBL_MAX ;
      LeftWeight = LeftSquares = LeftValues = 0.0 ;
      int upper = OKvalues-1 ;
      for (j=0 ; j < upper ; j++)
      {
          idx = sortedMean[j].value ;
          LeftSquares += squaredValues[idx] ;
          LeftValues += valueClass[idx] ;
          LeftWeight += valueWeight[idx] ;
          RightSquares -= squaredValues[idx] ;
          RightValues -= valueClass[idx] ;
          RightWeight -= valueWeight[idx] ;
          pLeft = LeftWeight/totalWeight ;
          variance = LeftSquares/LeftWeight - sqr(LeftValues/LeftWeight) ;
          if (LeftWeight > epsilon && variance > 0.0)
            estimate = pLeft * variance ;
          else
            estimate = 0.0 ;

          variance = RightSquares/RightWeight -sqr(RightValues/RightWeight) ;
          if (LeftWeight > epsilon && variance > 0.0)
             estimate +=  (double(1.0) - pLeft) * variance ;

          if (estimate < bestEstimate)
             bestEstimate = estimate ;
      }
      DiscEstimation[i] = - bestEstimate ;
   }


   // continuous values
   double dVal ;
   marray<sortRec> sortedAttr(TrainSize) ;
   for (i=contAttrFrom ; i < contAttrTo ; i++)
   {
      RightWeight = RightSquares = RightValues = 0.0 ;
      OKvalues = 0 ;
      for (j=0 ; j < TrainSize ; j++)
      {
         if (isNAcont(NumValues(j,i)))
           continue ;
         sortedAttr[OKvalues].key = NumValues(j,i) ;
         sortedAttr[OKvalues].value = j ;
         RightWeight += weight[j] ;
         dVal = weight[j] * NumValues(j,0) ;
         RightValues += dVal ;
         dVal *=  NumValues(j,0) ;
         RightSquares += dVal  ;
         OKvalues ++ ;
      }
      totalWeight = RightWeight ;
      sortedAttr.setFilled(OKvalues) ;
      sortedAttr.qsortAsc() ;
      bestEstimate = DBL_MAX ;
      LeftWeight = LeftSquares = LeftValues = 0.0 ;
      j=0 ;
      while (j < OKvalues)
      {
         // collect cases with the same value of the attribute - we cannot split between them
         do {
           idx = sortedAttr[j].value ;
           dVal = weight[idx] * NumValues(idx, 0) ;
           LeftValues += dVal ;
           RightValues -= dVal ;
           dVal *= NumValues(idx, 0) ;
           LeftSquares += dVal ;
           RightSquares -= dVal ;
           LeftWeight += weight[idx] ;
           RightWeight -= weight[idx] ;
           j++ ;
         } while (j < OKvalues &&  sortedAttr[j].key ==  sortedAttr[j-1].key) ;
         if (j==OKvalues)
            break ;
         pLeft = LeftWeight/totalWeight ;
         variance = LeftSquares/LeftWeight - sqr(LeftValues/LeftWeight) ;
         if (LeftWeight > epsilon && variance > 0.0 )
            estimate = pLeft * variance ;
         else
            estimate = 0.0 ;
         variance = RightSquares/RightWeight -sqr(RightValues/RightWeight) ;
         if (RightWeight > epsilon && variance > 0.0)
            estimate += (1.0 - pLeft) * variance ;
         if (estimate < bestEstimate) {
            bestEstimate = estimate ;
            splitPoint[i] = (sortedAttr[j].key + sortedAttr[j-1].key)/2.0 ;
         }
      }
      NumEstimation[i] = - bestEstimate ;
   }
}



// ***************************************************************************
//
//                       combination
//        combing estimationRegs of RReliefF and MSE
//
// ***************************************************************************
void estimationReg::Combination(int contAttrFrom, int contAttrTo,
                               int discAttrFrom, int discAttrTo, int selectedEstimator)
{
   // first do estimationReg with RReliefF
   CReliefDensity(contAttrFrom, contAttrTo, discAttrFrom, discAttrTo,
                            selectedEstimator) ;

   // store the estimationRegs
   int estContinuous = contAttrTo - contAttrFrom ;
   int estDiscrete = discAttrTo - discAttrFrom ;

   marray<double> contRelief(estContinuous), discRelief(estDiscrete) ;
   double minReliefEst=DBL_MAX, maxReliefEst=-DBL_MAX ;
   int i ;
   for (i=contAttrFrom ; i < contAttrTo; i++)
   {
      contRelief[i - contAttrFrom] = NumEstimation[i] ;
      if (NumEstimation[i] > maxReliefEst)
         maxReliefEst =  NumEstimation[i] ;
      if (NumEstimation[i] < minReliefEst)
         minReliefEst = NumEstimation[i] ;
   }
   for (i=discAttrFrom ; i < discAttrTo; i++)
   {
      discRelief[i-discAttrFrom] = DiscEstimation[i] ;
      if (DiscEstimation[i] > maxReliefEst)
         maxReliefEst =  DiscEstimation[i] ;
      if (DiscEstimation[i] < minReliefEst)
         minReliefEst = DiscEstimation[i] ;
   }
   double rangeReliefEst = maxReliefEst - minReliefEst ;

   // estimate with MSE
   MSE(contAttrFrom, contAttrTo, discAttrFrom, discAttrTo) ;

   // find ranges of MSE estimationReg
   double minMSEest=DBL_MAX, maxMSEest=-DBL_MAX ;
   for (i=contAttrFrom ; i < contAttrTo; i++)
   {
      if (NumEstimation[i] > maxMSEest)
         maxMSEest =  NumEstimation[i] ;
      if (NumEstimation[i] < minMSEest)
         minMSEest = NumEstimation[i] ;
   }
   for (i=discAttrFrom ; i < discAttrTo; i++)
   {
      if (DiscEstimation[i] > maxMSEest)
         maxMSEest =  DiscEstimation[i] ;
      if (DiscEstimation[i] < minMSEest)
         minMSEest = DiscEstimation[i] ;
   }
   double rangeMSEest = maxMSEest - minMSEest ;

   // now combine both estimationRegs into one, leave the result in NumEstimation
   // and DiscEstimattion
   double ReliefEst, MSEest ;
   const int neccessaryReliefExamples = 50 ;
   double ReliefWeight = exp(-1.0/sqr(TrainSize/double(neccessaryReliefExamples))) ;
   for (i=contAttrFrom ; i < contAttrTo; i++)
   {
      ReliefEst = (contRelief[i-contAttrFrom] - minReliefEst)/rangeReliefEst ;
      MSEest = (NumEstimation[i] - minMSEest) / rangeMSEest ;
      NumEstimation[i] = ReliefWeight*ReliefEst + (1.0-ReliefWeight)*MSEest ;
   }
   for (i=discAttrFrom ; i < discAttrTo; i++)
   {
      ReliefEst = (discRelief[i-discAttrFrom] - minReliefEst)/rangeReliefEst ;
      MSEest = (DiscEstimation[i] - minMSEest) / rangeMSEest ;
      DiscEstimation[i] = ReliefWeight*ReliefEst + (1.0-ReliefWeight)*MSEest ;
   }
}




// ***************************************************************************
//
//                      prepareContAttr
//                      ----------------
//
//        creating continuous data representation of feature
//
// ***************************************************************************
void estimationReg::prepareContAttr(int attrIdx)
{
    // data column
//    Expression.data(NumValues, attrIdx) ;

    // min, max, interval
    int j=0 ;
    while (isNAcont(NumValues(j,attrIdx)) && j < TrainSize)
       j++ ;
    if (j >= TrainSize)
    {
      minValue[attrIdx] = maxValue[attrIdx] = NAcont ;
      // merror("estimationReg::prepareContAttr", "all values of the attribute are missing") ;
    }
     else
        minValue[attrIdx] = maxValue[attrIdx] = NumValues(j, attrIdx) ;

    for (j=j+1 ; j < TrainSize ; j++)
       if (!isNAcont(NumValues(j, attrIdx)))
       {
         if (NumValues(j, attrIdx) < minValue[attrIdx])
            minValue[attrIdx] = NumValues(j, attrIdx) ;
         else
           if (NumValues(j, attrIdx) > maxValue[attrIdx])
             maxValue[attrIdx] = NumValues(j, attrIdx) ;
       }

    valueInterval[attrIdx] = maxValue[attrIdx] - minValue[attrIdx] ;

    if (valueInterval[attrIdx] < epsilon)
      valueInterval[attrIdx] = epsilon ;



   // step
   int noIntervals = constNAdiscretizationIntervals ;
   if (TrainSize/noIntervals < constAverageExamplesPerInterval )
        noIntervals = Mmax(2,int(TrainSize/constAverageExamplesPerInterval)) ;
   step[attrIdx] =  valueInterval[attrIdx]/noIntervals*double(1.000001) ; // 1.000001 - to avoid overflows due to numerical aproximation

   // missing values probabilities
   NAnumValue[attrIdx].create(noIntervals+1, 0.0) ;
   for (j=0 ; j < TrainSize ; j++)
     if (isNAcont(NumValues(j,attrIdx)))
        NAnumValue[attrIdx][0] += 1.0 ;
     else
        NAnumValue[attrIdx][int((NumValues(j,attrIdx)- minValue[attrIdx])/step[attrIdx])+1] += 1 ;

   double denominator = TrainSize + noIntervals - NAnumValue[attrIdx][0] ;
   double valueProb ;
   NAnumValue[attrIdx][0] = 0.0 ;
   for (j=1; j < NAnumValue[attrIdx].len() ; j++)
   {
      valueProb = (NAnumValue[attrIdx][j]+double(1.0))/denominator ;
      NAnumValue[attrIdx][j] =  double(1.0) - valueProb ;
      // both are missing - compute probability of  same values
      NAnumValue[attrIdx][0] += valueProb * valueProb  ;
   }
   NAnumValue[attrIdx][0] = double(1.0) - NAnumValue[attrIdx][0] ;



#if defined(RAMP_FUNCTION)
   // differemt, equal, slope
    DifferentDistance[attrIdx] = valueInterval[attrIdx] * eopt.numAttrProportionEqual ;
    EqualDistance[attrIdx] = valueInterval[attrIdx] * eopt.numAttrProportionDifferent  ;
   if (DifferentDistance[attrIdx] > EqualDistance[attrIdx])
      CAslope[attrIdx] = double(1.0)/(DifferentDistance[attrIdx] - EqualDistance[attrIdx]) ;
    else
      CAslope[attrIdx] = DBL_MAX ;
#endif

}



// ***************************************************************************
//
//                      prepareDiscAttr
//                      ----------------
//
//        creating discrete data representation of feature
//
// ***************************************************************************
void estimationReg::prepareDiscAttr(int attrIdx, int noValues)
{

     discNoValues[attrIdx] = noValues ;

    // diff for missing values
	double denominator, valueProb ;
    NAdiscValue[attrIdx].create(discNoValues[attrIdx] +1, 0.0) ;

    int j ;
    for (j=0 ; j < TrainSize ; j++)
      NAdiscValue[attrIdx][DiscValues(j,attrIdx)] += 1.0 ;

    denominator = TrainSize+discNoValues[attrIdx]-NAdiscValue[attrIdx][0] ;
    NAdiscValue[attrIdx][0] = 0.0 ;
    for (j=1; j < NAdiscValue[attrIdx].len() ; j++)
    {
       valueProb = (NAdiscValue[attrIdx][j]+double(1.0))/denominator ;
       NAdiscValue[attrIdx][j] =  double(1.0) - valueProb ;
       // both are missing - computing same value probability
       NAdiscValue[attrIdx][0] += valueProb * valueProb  ;
    }
    NAdiscValue[attrIdx][0] = double(1.0) - NAdiscValue[attrIdx][0] ;
}




// ***************************************************************************
//
//                         MEofModel
//        computing estimate by mean (squared or absolute) error of the selected model
//
// ***************************************************************************
void estimationReg::MEofModel(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int selectedEstimator)
{
      // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo, 0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo, 0.0) ;
   splitPoint.init(contAttrFrom,contAttrTo, DBL_MAX) ;

   int i, j, idx;
   marray<double> valueWeight ;
   double bestEstimate, estimate, minRound, totalWeight ;
   int val, idxRound ;

   // estimationReg of discrete attributes
   marray<int> leftTrain(TrainSize), rightTrain(TrainSize) ;
   marray<double> leftWeight(TrainSize), rightWeight(TrainSize) ;
   binnodeReg *leftNode = new binnodeReg ;
   binnodeReg *rightNode = new binnodeReg ;
   marray<booleanT> leftValues ;
   double leftError=0, rightError=0;

   for (i=discAttrFrom ; i < discAttrTo ; i++) {
      valueWeight.create(discNoValues[i]+1, 0.0) ;
      leftValues.create(discNoValues[i]+1, mFALSE) ;
      for (j=0 ; j < TrainSize ; j++)
      {
         idx = DiscValues(j,i) ;
         valueWeight[idx] += weight[j] ;
      }
	   totalWeight = 0.0 ;
      for (j=1 ; j <= discNoValues[i] ; j++)
         totalWeight += valueWeight[j] ;

      bestEstimate = DBL_MAX ;

      if ( discNoValues[i] <= eopt.maxValues4Exhaustive) { // }&& (exhaustivePositions * 0.8 <= greedyPositions || exhaustivePositions < maxSample))
    	// exhaustive search through all possible partitions

    	binPartition Generator(discNoValues[i]) ;

        while (Generator.increment() )
        {
           // save partition
           leftValues = Generator.leftPartition ;
	        // split the data
	        leftTrain.setFilled(0) ;
           leftWeight.setFilled(0) ;
   	     rightTrain.setFilled(0) ;
           rightWeight.setFilled(0) ;
           for (j=0 ; j < TrainSize ; j++)
           {
             if (DiscValues(j,i) != NAdisc && weight[j] > epsilon)
             {
                if (leftValues[DiscValues(j,i)])
                {
  	    	       leftTrain.addEnd(OriginalDTrain[j]) ;
                   leftWeight.addEnd(weight[j]) ;
                }
                else {
                  rightTrain.addEnd(OriginalDTrain[j]) ;
                  rightWeight.addEnd(weight[j]) ;
                }
             }
           }
           if (leftTrain.filled() <= 0 || rightTrain.filled() <= 0)
             continue ;

           // compute models and their errors
           fTree->buildTreeNode(leftNode, leftTrain, leftWeight, leftTrain.filled()) ;
           fTree->buildTreeNode(rightNode, rightTrain, rightWeight, rightTrain.filled()) ;
           switch (selectedEstimator)
           {
              case  estMSEofModel:
                                  leftError = leftNode->MSE ;
                                  rightError = rightNode->MSE ;
                                  break ;

              case estMAEofModel:
                                  leftError = leftNode->MAE ;
                                  rightError = rightNode->MAE ;
                                  break ;

              default:
                       merror("estimationReg::MEofModel", "violation of the internal assumption about type of estimator") ;
           }

           // compute estimate
		     estimate = (leftNode->weight * leftError + rightNode->weight*rightError)/totalWeight ;
           if (estimate < bestEstimate)
		        bestEstimate = estimate ;
        }
      }
      else {
         // greedy search through possible partitions

         leftValues.create(discNoValues[i]+1, mFALSE) ;
         for (int filled=1 ; filled < discNoValues[i] ; filled++)
         {
            minRound = DBL_MAX ;
            idxRound = -1 ;
            for (val=1 ; val <= discNoValues[i] ; val++)
            {
               if (leftValues[val] == mFALSE)
               {
                  leftValues[val] = mTRUE ;

                  leftTrain.setFilled(0) ;
                  leftWeight.setFilled(0) ;
         	      rightTrain.setFilled(0) ;
                  rightWeight.setFilled(0) ;
                  for (j=0 ; j < TrainSize ; j++)
                  {
                     if (DiscValues(j,i) != NAdisc && weight[j] > epsilon)
                     {
                        if (leftValues[DiscValues(j,i)])
                        {
         			         leftTrain.addEnd(OriginalDTrain[j]) ;
                           leftWeight.addEnd(weight[j]) ;
                        }
                        else {
                          rightTrain.addEnd(OriginalDTrain[j]) ;
                          rightWeight.addEnd(weight[j]) ;
                        }
                     }
                  }

                  if (leftTrain.filled() <= 0 || rightTrain.filled() <= 0)
                     continue ;

                  // compute models and their errors
                  fTree->buildTreeNode(leftNode, leftTrain, leftWeight, leftTrain.filled()) ;
                  fTree->buildTreeNode(rightNode, rightTrain, rightWeight, rightTrain.filled()) ;
                  switch (selectedEstimator)
                  {
                       case  estMSEofModel:
                                  leftError = leftNode->MSE ;
                                  rightError = rightNode->MSE ;
                                  break ;

                       case estMAEofModel:
                                  leftError = leftNode->MAE ;
                                  rightError = rightNode->MAE ;
                                  break ;

                        default:
                              merror("estimationReg::MEofModel", "internal misassumption about type of estimator") ;
                  }

                  estimate = (leftNode->weight * leftError + rightNode->weight*rightError)/totalWeight ;
                  leftValues[val] = mFALSE ;
                  if (estimate < minRound)
                  {
                     minRound = estimate ;
                     idxRound = val ;
                  }
               }
            }
            if (idxRound == -1)
               break ;

            leftValues[idxRound] = mTRUE ;
            if (minRound < bestEstimate)
               bestEstimate = minRound ;

         }
      }
      DiscEstimation[i] = - bestEstimate ;
   }


   // numeric attributes
   marray<sortRec> sortedAttr(TrainSize) ;
   int OKvalues;
   for (i=contAttrFrom ; i < contAttrTo ; i++)
   {

      OKvalues = 0 ;
      totalWeight = 0.0 ;
      for (j=0 ; j < TrainSize ; j++)
      {
         if (isNAcont(NumValues(j,i)))
           continue ;
         sortedAttr[OKvalues].key = NumValues(j,i) ;
         sortedAttr[OKvalues].value = j ;
         totalWeight += weight[j] ;
         OKvalues ++ ;
      }
      sortedAttr.setFilled(OKvalues) ;
      sortedAttr.qsortAsc() ;

      marray<int> uniqueIdx(OKvalues) ;
      int noUnique, lastUnique ;
      if (OKvalues > 0)
      {
         // count unique values
         noUnique = 1 ;
         lastUnique = 0 ;
         uniqueIdx[0] = 0 ;
         for (j=1 ; j < OKvalues ; j++)
         {
            if (sortedAttr[j].key != sortedAttr[lastUnique].key)
            {
               uniqueIdx[noUnique] = j ;
               noUnique ++ ;
               lastUnique = j ;
            }
         }
      }
      else
         noUnique = 0 ;

      int sampleSize ;
      if (eopt.discretizationSample==0)
        sampleSize = noUnique ;
      else
        sampleSize = Mmin(eopt.discretizationSample, noUnique) ;

      marray<int> splits(sampleSize) ;

      if (noUnique > sampleSize)
      {
         // do sampling
         marray<int> uniqueSelected(noUnique) ;
         for (j=0 ; j < noUnique ; j++)
            uniqueSelected[j] = j ;

         int upper = noUnique  ;
         int selected ;
         for (j=0 ; j < sampleSize ; j++)
         {
            selected = randBetween(0, upper) ;
            splits[j] = uniqueIdx[uniqueSelected[selected]] ;
            upper -- ;
            uniqueSelected[selected] = uniqueSelected[upper] ;
         }
         splits.setFilled(sampleSize) ;
         splits.qsortAsc()  ;
      }
      else
        for (j=0 ; j < sampleSize ; j++)
          splits[j] = uniqueIdx[j] ;

      bestEstimate = DBL_MAX ;
      double leftSum=0, rightSum=0 ;
      // prepare for splitting
      for (j=0; j < OKvalues ; j++)
      {
         rightTrain[j] = OriginalDTrain[sortedAttr[OKvalues-1-j].value] ;
         rightWeight[j] = weight[sortedAttr[OKvalues-1-j].value] ;
         rightSum +=  rightWeight[j] ;
      }
      rightTrain.setFilled(OKvalues) ;
      leftTrain.setFilled(0) ;
      leftWeight.setFilled(0) ;
      j=0 ;
      for (int splitIdx=1; splitIdx < sampleSize ; splitIdx++)
       {
         // collect cases between splits[j]: they are non unique or sampled out
         do {
           leftTrain.addEnd(rightTrain[rightTrain.filled()-1]) ;
           leftWeight.addEnd(rightWeight[rightTrain.filled()-1]) ;
           leftSum += rightWeight[rightTrain.filled()-1] ;
           rightSum -= rightWeight[rightTrain.filled()-1] ;
           rightTrain.decEdge() ;
           j++ ;
         } while (j < splits[splitIdx]) ;

         if (leftTrain.filled() <= 0 || rightTrain.filled() <= 0 ||
             leftSum < eopt.minNodeWeightEst || rightSum < eopt.minNodeWeightEst)
             continue ;
         // compute models and their errors
         fTree->buildTreeNode(leftNode, leftTrain, leftWeight, leftTrain.filled()) ;
         fTree->buildTreeNode(rightNode, rightTrain, rightWeight, rightTrain.filled()) ;
         switch (selectedEstimator)
         {
              case  estMSEofModel:
                                  leftError = leftNode->MSE ;
                                  rightError = rightNode->MSE ;
                                  break ;

              case estMAEofModel:
                                  leftError = leftNode->MAE ;
                                  rightError = rightNode->MAE ;
                                  break ;

              default:
                       merror("estimationReg::MEofModel", "internal misassumption about type of estimator") ;
         }
         estimate = (leftNode->weight * leftError + rightNode->weight*rightError)/totalWeight ;
         if (estimate < bestEstimate) {
		     bestEstimate = estimate ;
		     splitPoint[i] = (sortedAttr[splits[splitIdx]].key + sortedAttr[splits[splitIdx]-1].key) /2.0 ;
         }
		}

      NumEstimation[i] = - bestEstimate ;
   }
   delete leftNode ;
   delete rightNode ;
}
