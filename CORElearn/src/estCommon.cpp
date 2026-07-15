//#include <stdlib.h>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "estimator.h"
#include "contain.h"
#include "options.h"

using namespace std ;

int NoEstimators = 37;
estDsc estName[38]={
    {"", ""} ,
    {"ReliefFequalK",     "ReliefF with equal k-nearest" },           // 1
    {"ReliefFexpRank",    "ReliefF with exponential rank distance" }, // 2
    {"ReliefFbestK",      "ReliefF with best of equal k-nearest" },   // 3
    {"Relief",            "Original Relief" },                // 4 original
    {"InfGain",           "Information gain" },               // 5
    {"GainRatio",         "Gain ratio" },                     // 6
    {"MDL",               "MDL" },                            // 7
    {"Gini",              "Gini index"},                      // 8
    {"MyopicReliefF",     "Myopic Relief" },                  // 9
    {"Accuracy",          "Accuracy" },                       // 10
    {"ReliefFmerit",      "ReliefF with merit"},              // 11
    {"ReliefFdistance",   "ReliefF with direct distance" },   // 12
    {"ReliefFsqrDistance","ReliefF with direct squared distance"},  // 13
    {"DKM",               "Dietterich-Kearns-Mansour (DKM)"}, // 14
    {"ReliefFexpC",       "ReliefF with expected costs of misclassification"}, // 15
    {"ReliefFavgC",       "ReliefF with average costs of misclassification"}, // 16
    {"ReliefFpe",         "ReliefF with expected cost probabilities"},// 17
    {"ReliefFpa",         "ReliefF with average cost probabilities"},//18
    {"ReliefFsmp",        "ReliefF with sampling by expected cost"},// 19
    {"GainRatioCost",     "Gain ration with cost information"},// 20
    {"DKMcost",           "DKM with cost information"},// 21
    {"ReliefKukar",       "ReliefF with Kukar's variant of costs"}, // 22
    {"MDLsmp",            "MDL with expected cost sampling"}, // 23
	{"ImpurityEuclid",    "Euclidean distance as impurity function on within node class distributions"}, // 24
	{"ImpurityHellinger", "Hellinger distance as impurity function on within node class distributions"}, // 25
    {"UniformDKM",        "Dietterich-Kearns-Mansour (DKM) with uniform priors" }, // 26
    {"UniformGini",       "Gini index with uniform priors"},                      // 27
    {"UniformInf",        "Information gain with uniform priors" }, // 28
    {"UniformAccuracy",   "Accuracy with uniform priors" }, // 29
    {"EqualDKM",          "Dietterich-Kearns-Mansour (DKM) with equal weights for splits" }, // 30
    {"EqualGini",         "Gini index with equal weights for splits"},                      // 31
    {"EqualInf",          "Information gain with equal weights for splits" },               // 32
    {"EqualHellinger",    "Two equally weighted splits based Hellinger distance"}, // 33
    {"DistHellinger",     "Hellinger distance between class distributions in branches"}, // 34
    {"DistAUC",           "AUC distance between splits" },                // 35
    {"DistAngle",         "Cosine of angular distance between splits" },               // 36
    {"DistEuclid",        "Euclidean distance between splits" }               // 37
} ;

int NoEstimatorsReg = 9;
estDsc estNameReg[10]={
	{"", ""},                    // 0
    {"RReliefFequalK",     "RReliefF with equal k-nearest" },           // 1
    {"RReliefFexpRank",    "RReliefF with exponential rank distance" }, // 2
    {"RReliefFbestK",      "RReliefF with best of equal k-nearest" },   // 3
	{"RReliefFwithMSE",    "Combined RReliefF and MSE"}, // 4
//	{"RReliefFconstr","RReliefF for construction"},      // 5
//	{"MSEmeanConstr","Mean Squared Error of mean class value for construction"},  // 6
	{"MSEofMean","Mean Squared Error of mean class value"},           // 7
	{"MSEofModel","Mean Squared Error of given model"},          // 8
	{"MAEofModel","Mean Absolute Error of given model"},          // 9
    {"RReliefFdistance",   "RReliefF with direct distance" },   // 13
    {"RReliefFsqrDistance","RReliefF with direct squared distance"}  // 14
} ;



// ***************************************************************************
//
//                         constructor
//
// ***************************************************************************
estimation::estimation(const featureTree *fTreeParent, marray<int> &inDTrain,
                 marray<double> &inpDTrain, int inTrainSize)
{
   fTree = fTreeParent ;
   eopt.copy( *(fTree -> opt)) ;

   initialize(inDTrain, inpDTrain, inTrainSize) ;
}

void estimation::initialize(marray<int> &inDTrain, marray<double> &inpDTrain,
							int inTrainSize)  {

   //-------------------------------------------------------------
   // copy essential values
   //-------------------------------------------------------------
   currentNumSize = noNumeric = fTree->noNumeric ;
   currentDiscSize = noDiscrete = fTree->noDiscrete ;

   //-------------------------------------------------------------
   // select the training examples for ReliefF's estimation
   //-------------------------------------------------------------
     int i, j, k ;
   // will we use all examples or just a subsample
   if (inTrainSize <= eopt.attrEvaluationInstances || eopt.attrEvaluationInstances == 0)
   {
      TrainSize = inTrainSize ;
      originalDTrain.copy(inDTrain) ;
      weight.copy(inpDTrain) ;
   }
   else
   {
       //-------------------------------------------------------------
       // randomly select exactly attrEvaluationInstances without repetitions
       //-------------------------------------------------------------

       marray<int> selected(inTrainSize) ;
       for (i=0 ; i < inTrainSize ; i++)
           selected[i] = i ;
       selected.setFilled(inTrainSize) ;

       TrainSize = eopt.attrEvaluationInstances ;
       originalDTrain.create(TrainSize) ;
       weight.create(TrainSize) ;

       for (i=0 ; i < TrainSize ; i++)  {
           j = randBetween(0, selected.filled()) ;
           originalDTrain[i] = inDTrain[selected[j]] ;
           weight[i] = inpDTrain[selected[j]]  ;
           selected[j] = selected[selected.filled()-1] ;
           selected.setFilled(selected.filled()-1) ;
       }
   }

   //-------------------------------------------------------------
   //  copy discrete and numeric data
   //-------------------------------------------------------------
   DiscValues.create(TrainSize, noDiscrete) ;
   for (i=0 ; i < noDiscrete ; i++)
     for (j=0 ; j < TrainSize ; j++)
         DiscValues.Set(j,i,fTree->DiscData(originalDTrain[j],i) );

   NumValues.create(TrainSize, noNumeric) ;
   for (i=0 ; i < noNumeric ; i++)
      for (j=0 ; j < TrainSize ; j++)
           NumValues.Set(j,i,fTree->NumData(originalDTrain[j],i) );

   //-------------------------------------------------------------
   // create estimation arrays
   //-------------------------------------------------------------
   DiscEstimation.create(noDiscrete, -2.0) ;
   NumEstimation.create(noNumeric, -2.0) ;
   splitPoint.create(noNumeric, DBL_MAX) ;
   //-------------------------------------------------------------
   // create distance matrix
   //-------------------------------------------------------------
   NumDistance.create(TrainSize, noNumeric) ;
   DiscDistance.create(TrainSize, noDiscrete) ;


   //-------------------------------------------------------------
   // set number of iterations in main reliefF loop
   //-------------------------------------------------------------
   if (eopt.ReliefIterations == 0 || eopt.ReliefIterations > TrainSize)
       NoIterations = TrainSize ;
   else if (eopt.ReliefIterations == -1)
       NoIterations = (int)log(double(TrainSize)) ;
   else if (eopt.ReliefIterations == -2)
       NoIterations = (int)sqrt(double(TrainSize)) ;
   else
      NoIterations = eopt.ReliefIterations ;


   //-------------------------------------------------------------
   // set slopes and distances for ramp function of numeric attributes and class
   //-------------------------------------------------------------
#if defined(RAMP_FUNCTION)
   DifferentDistance.create(noNumeric) ;
   EqualDistance.create(noNumeric) ;
   CAslope.create(noNumeric) ;
   for (i=0 ; i < noNumeric ; i++)
   {
     DifferentDistance[i] = fTree->AttrDesc[fTree->ContIdx[i]].DifferentDistance ;
     EqualDistance[i] = fTree->AttrDesc[fTree->ContIdx[i]].EqualDistance ;
     if (DifferentDistance[i] != EqualDistance[i])
         CAslope[i] = double(1.0)/(DifferentDistance[i] - EqualDistance[i]) ;
     else
        CAslope[i] = DBL_MAX ;
   }
#endif

   //-------------------------------------------------------------
   //  set number of values for discrete
   //-------------------------------------------------------------
   discNoValues.create(noDiscrete) ;
   for (i=0 ; i < noDiscrete ; i++)
      discNoValues[i] = fTree->AttrDesc[fTree->DiscIdx[i]].NoValues  ;

   noClasses = discNoValues[0] ;

   //-------------------------------------------------------------
   // compute probabilities for nominal missing values (knowing the class value)
   //-------------------------------------------------------------

   double denominator, valueProb ;

   NAdiscValue.create(noClasses+1, noDiscrete) ;

   for (i=1 ; i < noDiscrete ; i++)
      for (j=1 ; j <= noClasses ; j++)
          NAdiscValue(j,i).create(discNoValues[i] +1, 0.0) ;

   for (i=1; i < noDiscrete ; i++)
     for (j=0 ; j < TrainSize ; j++)
        NAdiscValue(DiscValues(j,0), i)[DiscValues(j,i)] += 1.0 ;

   for (i=1 ; i < noDiscrete ; i++)
   {
      for (k=1 ; k <= noClasses ; k++)
      {
         // denominator initially equals Laplacian correction
         denominator = discNoValues[i]  ;
         for (j=1; j < NAdiscValue(k,i).len() ; j++)
            denominator += NAdiscValue(k,i)[j] ;

         NAdiscValue(k,i)[0] = 0.0 ; //initially for both missing
         for (j=1; j < NAdiscValue(k,i).len() ; j++)
         {
            valueProb = (NAdiscValue(k,i)[j]+double(1.0))/denominator ;
            NAdiscValue(k,i)[j] =  double(1.0) - valueProb ; // diff =  1 - prob
            // both are missing
            NAdiscValue(k,i)[0] += valueProb * valueProb  ;
         }
         NAdiscValue(k,i)[0] = double(1.0) - NAdiscValue(k,i)[0] ;
      }
   }

   //-------------------------------------------------------------
   //  numeric attribute missing values
   //   it would be better to use density estimation with kernel functions
   //-------------------------------------------------------------

   minValue.copy(fTree->minValue) ;
   maxValue.copy(fTree->maxValue) ;
   valueInterval.copy(fTree->valueInterval) ;
   step.create(noNumeric) ;
   NAnumValue.create(noClasses+1,noNumeric) ;

   if (TrainSize/constNAdiscretizationIntervals < constAverageExamplesPerInterval )
      noNAdiscretizationIntervals = Mmax(2,int(TrainSize/constAverageExamplesPerInterval)) ;
   else
      noNAdiscretizationIntervals = constNAdiscretizationIntervals ;

   for (i=0; i < noNumeric ; i++)
   {
      step[i] =  valueInterval[i]/noNAdiscretizationIntervals*double(1.000001) ; // 1.000001 - to avoid overflows due to numerical approximation
      for (j=1 ; j <= noClasses ; j++)
        NAnumValue(j,i).create(noNAdiscretizationIntervals +1, 0.0) ;
   }

   double intervalIdx ;
   for (i=0; i < noNumeric ; i++)
     for (j=0 ; j < TrainSize ; j++)
       if ( ! isNAcont(NumValues(j,i))) {
        intervalIdx =  (NumValues(j,i)-minValue[i])/step[i] ;
         #if defined(DEBUG)
         if (isNAcont(intervalIdx))
        	 merror("Mismatch between NA values or incorrect data.","") ;
         #endif
         NAnumValue( DiscValues(j,0), i )[1+int(intervalIdx)] += 1 ;
       }

   for (i=0 ; i < noNumeric ; i++)
   {
      for (k=1 ; k <= noClasses ; k++)
      {
         // denominator initialy equals Laplacian correction
         denominator = noNAdiscretizationIntervals ; ;
         for (j=1; j < NAnumValue(k,i).len() ; j++)
             denominator += NAnumValue(k,i)[j] ;

         NAnumValue(k,i)[0] = 0.0 ;  // for both missing
         for (j=1; j < NAnumValue(k,i).len() ; j++)
         {
            valueProb = (NAnumValue(k,i)[j]+double(1.0))/denominator ;
            NAnumValue(k,i)[j] =  double(1.0) - valueProb ;
            // both are missing - computing same value probability
            NAnumValue(k,i)[0] += valueProb * valueProb  ;
         }
         NAnumValue(k,i)[0] = double(1.0) - NAnumValue(k,i)[0] ;
      }
   }

   //-------------------------------------------------------------
   //  set k nearest with distance density and standard deviation for distance density
   //-------------------------------------------------------------
   if (eopt.kNearestEqual <= 0)
     kNearestEqual = TrainSize-1 ;
   else
     kNearestEqual = Mmin(eopt.kNearestEqual, TrainSize-1) ;

   if (eopt.kNearestExpRank <= 0)
     kDensity = TrainSize - 1 ;
   else
     kDensity = Mmin(eopt.kNearestExpRank, TrainSize-1) ;


   //-------------------------------------------------------------
   // variance of distance density
   //-------------------------------------------------------------
   varianceDistanceDensity = sqr(eopt.quotientExpRankDistance) ;

   //-------------------------------------------------------------
   // weights of the attributes
   //-------------------------------------------------------------
   weightDisc.create(noDiscrete, 1.0) ;
   weightNum.create(noNumeric, 1.0) ;
   if (eopt.attrWeights.defined()) {
	   int ic = 0, id= 1 ;
       for (i=1 ; i <= fTree->noAttr ; ++i) {
		   if (fTree->AttrDesc[i].continuous)
			   weightNum[ic++] = eopt.attrWeights[i] ;
		   else
			   weightDisc[id++] = eopt.attrWeights[i] ;
	   }
   }

   //-------------------------------------------------------------
   // structure for sorting/selecting cases by distance
   //-------------------------------------------------------------
   distanceArray.create(noClasses+1) ;
   diffSorted.create(noClasses+1) ;

}

// ***************************************************************************
//
//                     setActive
//        sets the active estimator variable
//
// ***************************************************************************
/* void estimation::setActive(int estimator)
{
	if (estimator > 0 && estimator <= NoEstimators)
		activeEstimator = estimator ;
	else merror("estimation::setActive","invalid estimator index");
}
*/

// ***************************************************************************
//
//                     adjustTables
//        prepare tables for increased number of attributes
//
// ***************************************************************************
void estimation::adjustTables(int newContSize, int newDiscSize)
{
   if (newContSize > currentNumSize)
   {
      NumValues.addColumns(newContSize) ;
      NumEstimation.enlarge(newContSize) ;
      splitPoint.enlarge(newContSize) ;
      NumDistance.addColumns(newContSize) ;

      minValue.enlarge(newContSize) ;
      maxValue.enlarge(newContSize) ;
      valueInterval.enlarge(newContSize) ;
      step.enlarge(newContSize) ;
      NAnumValue.addColumns(newContSize) ;

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
      DiscDistance.addColumns(newDiscSize) ;
      discNoValues.enlarge(newDiscSize) ;

      NAdiscValue.addColumns(newDiscSize) ;

      currentDiscSize = newDiscSize ;
   }
}



// ***************************************************************************
//
//                      prepareContAttr
//                      ----------------
//
//        creating numeric data representation of feature
//
// ***************************************************************************
void estimation::prepareContAttr(int attrIdx)
{

    // min, max, interval
    int j=0 ;
    while (isNAcont(NumValues(j,attrIdx)) && j < TrainSize)
       j++ ;
    if (j >= TrainSize)
    {
      minValue[attrIdx] = maxValue[attrIdx] = NAcont ;
      // merror("estimation::prepareContAttr", "all values of the attribute are missing") ;
    }
     else
        minValue[attrIdx] = maxValue[attrIdx] = NumValues(j, attrIdx) ;

    for (j=j+1 ; j < TrainSize ; j++)
       if ( ! isNAcont(NumValues(j, attrIdx)))
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
   step[attrIdx] =  valueInterval[attrIdx]/noNAdiscretizationIntervals*double(1.000001) ; // 1.000001 - to avoid overflows due to numerical aproximation

   int k ;
   // missing values probabilities
   for (k=1 ; k <= noClasses ; k++)
      NAnumValue(k,attrIdx).create(noNAdiscretizationIntervals+1, 0.0) ;

   for (j=0 ; j < TrainSize ; j++)
     if ( ! isNAcont(NumValues(j,attrIdx)))
       NAnumValue(DiscValues(j,0),attrIdx)[int((NumValues(j,attrIdx)-minValue[attrIdx])/step[attrIdx])+1] += 1 ;

   double denominator, valueProb ;
   for (k=1 ; k <= noClasses ; k++)
   {
       denominator = noNAdiscretizationIntervals;
       for (j=1; j < NAnumValue(k, attrIdx).len() ; j++)
          denominator += NAnumValue(k, attrIdx)[j] ;

       NAnumValue(k, attrIdx)[0] = 0.0 ;
       for (j=1; j < NAnumValue(k, attrIdx).len() ; j++)
       {
          valueProb = (NAnumValue(k, attrIdx)[j] + double(1.0)) / denominator ;
          NAnumValue(k, attrIdx)[j] =  double(1.0) - valueProb ;
          // both are missing - compute same value probability
          NAnumValue(k, attrIdx)[0] += valueProb * valueProb  ;
       }
       NAnumValue(k, attrIdx)[0] = double(1.0) - NAnumValue(k, attrIdx)[0] ;
   }

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
void estimation::prepareDiscAttr(int attrIdx, int noValues)
{

     discNoValues[attrIdx] = noValues ;

    // diff for missing values
    double denominator, valueProb ;
    int j, k ;
    for (k=1 ; k <= noClasses ; k++)
       NAdiscValue(k,attrIdx).create(discNoValues[attrIdx] +1, 0.0) ;

    for (j=0 ; j < TrainSize ; j++)
      NAdiscValue(DiscValues(j,0),attrIdx)[DiscValues(j,attrIdx)] += 1.0 ;

    for (k=1 ; k <= noClasses ; k++)
    {
      denominator = discNoValues[attrIdx]  ;
      for (j=1; j < NAdiscValue(k,attrIdx).len() ; j++)
         denominator += NAdiscValue(k,attrIdx)[j] ;

      NAdiscValue(k,attrIdx)[0] = 0.0 ;
      for (j=1; j < NAdiscValue(k, attrIdx).len() ; j++)
      {
         valueProb = (NAdiscValue(k,attrIdx)[j]+double(1.0))/denominator ;
         NAdiscValue(k, attrIdx)[j] =  double(1.0) - valueProb ;
         // both are missing - compute same value probability
         NAdiscValue(k,attrIdx)[0] += valueProb * valueProb  ;
      }
      NAdiscValue(k, attrIdx)[0] = double(1.0) - NAdiscValue(k, attrIdx)[0] ;
    }
 }




// ***************************************************************************
//
//                    computeDistances
// difference between two training instances in attribute space
//
// ***************************************************************************
void estimation::computeDistances(int Example)
{
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
          DiscDistance.Set(j, i, DAdiff(i,Example,j)) ;
      }
   }
}

void estimation::computeDistances(int Example, mmatrix<double> &DiscDist, mmatrix<double> &NumDist)
{
   int i ;
   for (int j=0 ; j < TrainSize ; j++)
   {
      if (Example == j)
      {
         for (i=0; i<numUpper; i++)
           NumDist.Set(j, i, 0.0) ;
         for (i=0 ; i < discUpper ; i++)
           DiscDist.Set(j, i, 0.0) ;
      }
      else {
        for (i=0; i<numUpper; i++)
          NumDist.Set(j, i, CAdiff(i,Example,j)) ;
        for (i=0 ; i < discUpper ; i++)
          DiscDist.Set(j, i, DAdiff(i,Example,j)) ;
      }
   }
}

// ***************************************************************************
//
//                    CaseDistance
// difference between two training instances in attribute space
//
// ***************************************************************************
double estimation::CaseDistance(int I1)
{
   double Distance = 0.0;

   int i ;
   for (i=1 ; i < noDiscrete ; i++)
      Distance += DiscDistance(I1,i) ;

   for (i=0; i<noNumeric; i++)
      Distance += NumDistance(I1,i) ;

   return  Distance ;
}

double estimation::CaseDistance(int I1, mmatrix<double> &DiscDist, mmatrix<double> &NumDist)
{
   double Distance = 0.0;

   int i ;
   for (i=1 ; i < noDiscrete ; i++)
      Distance += DiscDist(I1,i) ;

   for (i=0; i<noNumeric; i++)
      Distance += NumDist(I1,i) ;

   return  Distance ;
}

// ***************************************************************************
//
//                    WeightedCaseDistance
// weighted difference between two training instances in attribute space
//
// ***************************************************************************
double estimation::WeightedCaseDistance(int I1)
{
   double Distance = 0.0;

   int i ;
   for (i=1 ; i < noDiscrete ; i++)
      Distance += weightDisc[i] * DiscDistance(I1,i) ;

   for (i=0; i<noNumeric; i++)
      Distance += weightNum[i] * NumDistance(I1,i) ;

   return  Distance ;
}


// ***************************************************************************
//
//                    CARamp
//          ramp function of numeric attribute (or class)
//
// ***************************************************************************
double estimation::CARamp(int AttrIdx, double distance)
{
  if (distance >= DifferentDistance[AttrIdx])
     return 1.0 ;
  if (distance <= EqualDistance[AttrIdx])
     return 0.0 ;

  return  (distance - EqualDistance[AttrIdx]) * CAslope[AttrIdx] ;
}

// ***************************************************************************
//
//                   CAdiff
//              diff function for numeric attribute
//
// ***************************************************************************
double estimation::CAdiff(int AttrIdx, int I1, int I2)
{
   double cV1 = NumValues(I1, AttrIdx) ;
   double cV2 = NumValues(I2, AttrIdx) ;
   if (isNAcont(cV1))
      return NAnumDiff(AttrIdx,DiscValues(I1,0), cV2) ;
    else
      if (isNAcont(cV2))
        return NAnumDiff(AttrIdx, DiscValues(I2,0), cV1) ;
       else
         #if defined(RAMP_FUNCTION)
           return CARamp(AttrIdx, fabs(cV2 - cV1) ) ;
        #else
           return  fabs(cV2 - cV1) / valueInterval[AttrIdx] ;
        #endif
}



// ***************************************************************************
//
//                   DAdiff
//              diff function of discrete attribute
//
// ***************************************************************************
inline double estimation::DAdiff(int AttrIdx, int I1, int I2)
{

  // we assume that missing value has value 0
  int dV1 = DiscValues(I1, AttrIdx) ;
  int dV2 = DiscValues(I2, AttrIdx) ;
  if (dV1 == NAdisc)
     return NAdiscValue(DiscValues(I1,0),AttrIdx)[int(dV2)] ;
  else
    if (dV2 == NAdisc)
      return NAdiscValue(DiscValues(I2,0),AttrIdx)[int(dV1)] ;
     else
       if (dV1 == dV2)
         return  0.0 ;
       else
         return 1.0 ;
}

// ***************************************************************************
//
//                   NAnumDiff
//         diff function for missing values at numeric attribute
//
// ***************************************************************************
double estimation::NAnumDiff(int AttrIdx, int ClassValue, double Value)
{
   if (isNAcont(Value))
      return NAnumValue(ClassValue, AttrIdx)[0] ;

   return NAnumValue(ClassValue, AttrIdx)[int((Value-minValue[AttrIdx])/step[AttrIdx]) +1] ;
}




// ***************************************************************************
//
//                          prepareDistanceFactors
// computation of distance probability weight factors for given example
//
// ***************************************************************************
//void estimation::prepareDistanceFactors(int current, int distanceType)
void estimation::prepareDistanceFactors(int distanceType, marray<marray<sortRec> > &distanceArray,
		 marray<marray<sortRec> > &diffSorted, mmatrix<double> &DiscDistance, mmatrix<double> &NumDistance )
// explicit current is not needed, because we eliminate it based on distance
{

// we use only original attributes to obtain distance in attribute space

   int kSelected = 0 ;
   switch (distanceType)
   {
      case estReliefFkEqual:
              kSelected = kNearestEqual ;
              break ;

      case estReliefFexpRank:
      case estReliefFdistance:
      case estReliefFsqrDistance:
      case estReliefFexpC:
      case estReliefFavgC:
      case estReliefFpe:
      case estReliefFpa:
      case estReliefFsmp:
              kSelected = kDensity ;
              break ;

      case estReliefFbestK:
              kSelected = TrainSize ;  // we have to consider all neighbours
              break ;


      default: merror("estimation::prepareDistanceFactors","invalid distance type") ;
   }

   int i, cl ;
   sortRec tempSort ;

   for (cl = 1 ; cl <= noClasses; cl++)
   {
      // empty data structures
      distanceArray[cl].clear() ;
      diffSorted[cl].clear() ;
   }

   // distances in attributes space
   for (i=0 ; i < TrainSize; i++)   {
      tempSort.key =  CaseDistance(i,DiscDistance,NumDistance ) ;
 	  if (tempSort.key == 0.0) // we skip current and identical examples
   	      continue ;
	  tempSort.value = i ;
      diffSorted[DiscValues(i,0)].addEnd(tempSort) ;
   }

   // sort examples
   for (cl=1 ; cl <= noClasses ; cl++)
   {
      // we sort groups of examples of the same class according to
      // ascending distance from current
      if (diffSorted[cl].filled() > 1)
         diffSorted[cl].sortKsmallest(Mmin(kSelected, diffSorted[cl].filled())) ;
   }

   int upper, idx ;
   double factor ;
   // depending on tpe of distance, copy the nearest cases
   // and their distance factors into resulting array
   switch (distanceType)
   {

        case estReliefFkEqual:
        case estReliefFbestK:
          {
            for (cl=1; cl <= noClasses ; cl++)
            {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  idx -- ;
                  distanceArray[cl][i].key = 1.0  ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        case estReliefFexpRank:
        case estReliefFexpC:
        case estReliefFavgC:
        case estReliefFpe:
        case estReliefFpa:
        case estReliefFsmp:
          {
            for (cl=1; cl <= noClasses ; cl++)
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
               for (i=1 ; i < upper ; i++)
               {
                  if (diffSorted[cl][idx].key != diffSorted[cl][idx+1].key)
                     factor = double(exp(-sqr(double(i))/varianceDistanceDensity)) ;
                  distanceArray[cl][i].key =  factor ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  idx -- ;
               }
            }
          }
          break ;
        case estReliefFdistance:
          {
            double minNonZero = DBL_MAX ; // minimal non zero distance
            for (cl=1; cl <= noClasses ; cl++)
               for (i= diffSorted[cl].filled() -1 ; i >= 0 ; i--)
                  if (diffSorted[cl][i].key > 0.0)
                  {
                     if (diffSorted[cl][i].key < minNonZero)
                        minNonZero = diffSorted[cl][i].key ;
                     break;
                  }
            if (minNonZero == DBL_MAX)
               minNonZero = 1.0 ;

            for (cl=1; cl <= noClasses ; cl++)
            {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  if (diffSorted[cl][idx].key > 0)
                     factor = 1.0 / diffSorted[cl][idx].key ;
                  else
                     factor = 2.0 / minNonZero ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  distanceArray[cl][i].key = factor  ;
                  idx -- ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        case estReliefFsqrDistance:
          {
            double minNonZero = DBL_MAX ; // minimal non zero distance
            for (cl=1; cl <= noClasses ; cl++)
               for (i= diffSorted[cl].filled() -1 ; i >= 0 ; i--)
                  if (diffSorted[cl][i].key > 0.0)
                  {
                     if (diffSorted[cl][i].key < minNonZero)
                        minNonZero = diffSorted[cl][i].key ;
                     break;
                  }
            if (minNonZero == DBL_MAX)
               minNonZero = 1.0 ;

            for (cl=1; cl <= noClasses ; cl++)
            {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  if (diffSorted[cl][idx].key > 0)
                     factor = 1.0 / sqr(diffSorted[cl][idx].key) ;
                  else
                     factor = 2.0 / sqr(minNonZero) ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  distanceArray[cl][i].key = factor  ;
                  idx -- ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        default: merror("estimation::prepareDistanceFactors","invalid distanceType detected") ;
   }
}

void estimation::prepareDistanceFactors(int distanceType)
// explicit current is not needed, because we eliminate it based on distance
{

// we use only original attributes to obtain distance in attribute space

   int kSelected = 0 ;
   switch (distanceType)
   {
      case estReliefFkEqual:
              kSelected = kNearestEqual ;
              break ;

      case estReliefFexpRank:
      case estReliefFdistance:
      case estReliefFsqrDistance:
      case estReliefFexpC:
      case estReliefFavgC:
      case estReliefFpe:
      case estReliefFpa:
      case estReliefFsmp:
              kSelected = kDensity ;
              break ;

      case estReliefFbestK:
              kSelected = TrainSize ;  // we have to consider all neighbours
              break ;


      default: merror("estimation::prepareDistanceFactors","invalid distance type") ;
   }

   int i, cl ;
   sortRec tempSort ;

   for (cl = 1 ; cl <= noClasses; cl++)
   {
      // empty data structures
      distanceArray[cl].clear() ;
      diffSorted[cl].clear() ;
   }

   // distances in attributes space
   for (i=0 ; i < TrainSize; i++)   {
      tempSort.key =  CaseDistance(i) ;
 	  if (tempSort.key == 0.0) // we skip current and identical examples
   	      continue ;
	  tempSort.value = i ;
      diffSorted[DiscValues(i,0)].addEnd(tempSort) ;
   }

   // sort examples
   for (cl=1 ; cl <= noClasses ; cl++)
   {
      // we sort groups of examples of the same class according to
      // ascending distance from current
      if (diffSorted[cl].filled() > 1)
         diffSorted[cl].sortKsmallest(Mmin(kSelected, diffSorted[cl].filled())) ;
   }

   int upper, idx ;
   double factor ;
   // depending on tpe of distance, copy the nearest cases
   // and their distance factors into resulting array
   switch (distanceType)
   {

        case estReliefFkEqual:
        case estReliefFbestK:
          {
            for (cl=1; cl <= noClasses ; cl++)
            {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  idx -- ;
                  distanceArray[cl][i].key = 1.0  ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        case estReliefFexpRank:
        case estReliefFexpC:
        case estReliefFavgC:
        case estReliefFpe:
        case estReliefFpa:
        case estReliefFsmp:
          {
            for (cl=1; cl <= noClasses ; cl++)
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
               for (i=1 ; i < upper ; i++)
               {
                  if (diffSorted[cl][idx].key != diffSorted[cl][idx+1].key)
                     factor = double(exp(-sqr(double(i))/varianceDistanceDensity)) ;
                  distanceArray[cl][i].key =  factor ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  idx -- ;
               }
            }
          }
          break ;
        case estReliefFdistance:
          {
            double minNonZero = DBL_MAX ; // minimal non zero distance
            for (cl=1; cl <= noClasses ; cl++)
               for (i= diffSorted[cl].filled() -1 ; i >= 0 ; i--)
                  if (diffSorted[cl][i].key > 0.0)
                  {
                     if (diffSorted[cl][i].key < minNonZero)
                        minNonZero = diffSorted[cl][i].key ;
                     break;
                  }
            if (minNonZero == DBL_MAX)
               minNonZero = 1.0 ;

            for (cl=1; cl <= noClasses ; cl++)
            {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  if (diffSorted[cl][idx].key > 0)
                     factor = 1.0 / diffSorted[cl][idx].key ;
                  else
                     factor = 2.0 / minNonZero ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  distanceArray[cl][i].key = factor  ;
                  idx -- ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        case estReliefFsqrDistance:
          {
            double minNonZero = DBL_MAX ; // minimal non zero distance
            for (cl=1; cl <= noClasses ; cl++)
               for (i= diffSorted[cl].filled() -1 ; i >= 0 ; i--)
                  if (diffSorted[cl][i].key > 0.0)
                  {
                     if (diffSorted[cl][i].key < minNonZero)
                        minNonZero = diffSorted[cl][i].key ;
                     break;
                  }
            if (minNonZero == DBL_MAX)
               minNonZero = 1.0 ;

            for (cl=1; cl <= noClasses ; cl++)
            {
               idx =  diffSorted[cl].filled() -1;
               upper = Mmin(kSelected, diffSorted[cl].filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  if (diffSorted[cl][idx].key > 0)
                     factor = 1.0 / sqr(diffSorted[cl][idx].key) ;
                  else
                     factor = 2.0 / sqr(minNonZero) ;
                  distanceArray[cl][i].value = diffSorted[cl][idx].value ;
                  distanceArray[cl][i].key = factor  ;
                  idx -- ;
               }
               distanceArray[cl].setFilled(upper) ;
            }
          }
          break ;
        default: merror("estimation::prepareDistanceFactors","invalid distanceType detected") ;
   }
}


 void estimation::EprepareDistanceFactors(oeDistanceType distType)
{

// we use only original attributes to obtain distance in attribute space

   int kSelected = 0 ;
   switch (distType)
   {
       case kEqual:
              kSelected = kNearestEqual ;
              break ;

      case expRank:
              kSelected = kDensity ;
              break ;
      default: merror("estimation::prepareDistanceFactors","invalid distance type") ;
   }

   int i ;
   sortRec tempSort ;

   // empty data structures
   distanceEarray.clear() ;
   diffEsorted.clear() ;

   // distances in attributes space
   for (i=0 ; i < TrainSize; i++)
   {
      #if defined(WEIGHTED_ORDEVAL_DISTANCE)
      tempSort.key =  WeightedCaseDistance(i) ;
      #else
 	  tempSort.key =  CaseDistance(i) ;
      #endif

	  if (tempSort.key == 0.0) // we skip current and identical examples
		  continue ;
      tempSort.value = i ;
      diffEsorted.addEnd(tempSort) ;
   }

   // sort examples according to ascending distance from current
      if (diffEsorted.filled() > 1)
         diffEsorted.sortKsmallest(Mmin(kSelected, diffEsorted.filled())) ;


   int upper, idx ;
   double factor ;
   // depending on type of distance, copy the nearest cases
   // and their distance factors into resulting array
   switch (distType)
   {
        case kEqual:
          {
               idx =  diffEsorted.filled() -1;
               upper = Mmin(kSelected, diffEsorted.filled()) ;
               for (i=0 ; i < upper ; i++)
               {
                  distanceEarray[i].value = diffEsorted[idx].value ;
                  idx -- ;
                  distanceEarray[i].key = 1.0  ;
               }
               distanceEarray.setFilled(upper) ;
          }
          break ;
        case expRank:
          {
               upper = Mmin(kSelected, diffEsorted.filled()) ;
               distanceEarray.setFilled(upper) ;
               if (upper < 1)  // are there any elements
                  break ;
               idx =  diffEsorted.filled() -1;
               factor = 1.0  ;
               distanceEarray[0].key =  factor ;
               distanceEarray[0].value = diffEsorted[idx].value ;
               idx -- ;
               for (i=1 ; i < upper ; i++)
               {
                  if (diffEsorted[idx].key != diffEsorted[idx+1].key)
                     factor = double(exp(-sqr(double(i))/varianceDistanceDensity)) ;
                  distanceEarray[i].key =  factor ;
                  distanceEarray[i].value = diffEsorted[idx].value ;
                  idx -- ;
               }
          }
          break ;
        default: merror("estimation::EprepareDistanceFactors","invalid distanceType detected") ;
   }
}



