#include <cstdlib>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "estimatorReg.h"
#include "contain.h"
#include "utils.h"
#include "mathutil.h"

//#define KDTREE
//#define PRINT_EACH_K
//#define PRINT_EACH_ITERATION
//#define EXPLORE

//#define CIRCULAR_DIST
//#define CIRCULAR_MOD 20

using namespace std ;

#if !defined(R_PORT)
#define Rprintf printf
#endif
// ***************************************************************************
//
//                         constructor
//
// ***************************************************************************
estimationReg::estimationReg(regressionTree *fTreeParent, marray<int> &inDTrain,
                 marray<double> &inpDTrain, int inTrainSize)
{
   //-------------------------------------------------------------
   // copy essential values
   //-------------------------------------------------------------
   fTree = fTreeParent ;
   eopt.copy( *(fTree -> opt)) ;

   currentNumSize = noNumeric = fTree->noNumeric ;
   currentDiscSize = noDiscrete = fTree->noDiscrete ;

   //-------------------------------------------------------------
   // select the training examples for ReliefF's estimationReg
   //-------------------------------------------------------------
   marray<int> DTrain ;
   int i, j ;
   // will we use all exmples or just a subsample
   if (inTrainSize <= eopt.attrEvaluationInstances || eopt.attrEvaluationInstances == 0)
   {
      TrainSize = inTrainSize ;
      DTrain.copy(inDTrain) ;
      weight.copy(inpDTrain) ;
   }
   else
   {
       //-------------------------------------------------------------
       // randomly select attrEvaluationInstances
       //-------------------------------------------------------------
       TrainSize = eopt.attrEvaluationInstances ;
       DTrain.create(TrainSize) ;
       weight.create(TrainSize) ;
       marray<int> selected(eopt.attrEvaluationInstances) ;
       randomizedSample(selected, eopt.attrEvaluationInstances, inTrainSize) ;
       for (i=0; i < eopt.attrEvaluationInstances ; i++)
       {
               DTrain[i] = inDTrain[selected[i]] ;
               weight[i] = inpDTrain[selected[i]]  ;
        }
   }

   OriginalDTrain.copy(DTrain) ;


   // copy meta data about continuous attributes
   minValue.copy(fTree->minValue) ;
   maxValue.copy(fTree->maxValue) ;
   valueInterval.copy(fTree->valueInterval) ;


   //-------------------------------------------------------------
   //  copy discrete and contunuous data
   //-------------------------------------------------------------
   // but first set slopes and distances for ramp function
   // of continuous attributes and class
   // which are needed for [0,1] normalization
   //-------------------------------------------------------------
#if defined(RAMP_FUNCTION)
   //-------------------------------------------------------------
   // but first set slopes and distances for ramp function
   // of continuous attributes and class
   // which are needed for [0,1] normalization of diff
   //-------------------------------------------------------------
   DifferentDistance.create(noNumeric) ;
   EqualDistance.create(noNumeric) ;
   CAslope.create(noNumeric) ;
   for (i=0 ; i < noNumeric ; i++)
   {
     DifferentDistance[i] = fTree->AttrDesc[fTree->ContIdx[i]].DifferentDistance   ;
     EqualDistance[i] = fTree->AttrDesc[fTree->ContIdx[i]].EqualDistance  ;
     if (DifferentDistance[i] != EqualDistance[i])
         CAslope[i] = double(1.0)/(DifferentDistance[i] - EqualDistance[i]) ;
     else
        CAslope[i] = DBL_MAX ;
   }
#endif

   DiscValues.create(TrainSize, noDiscrete) ;
   for (i=0 ; i < noDiscrete ; i++)
     for (j=0 ; j < TrainSize ; j++)
         DiscValues.Set(j,i,fTree->DiscData(DTrain[j],i) );

   NumValues.create(TrainSize, noNumeric) ;
   for (i=0 ; i < noNumeric ; i++)
      for (j=0 ; j < TrainSize ; j++)
           NumValues.Set(j,i, fTree->NumData(DTrain[j],i) );

   //-------------------------------------------------------------
   // create estimationReg arrays
   //-------------------------------------------------------------
   DiscEstimation.create(noDiscrete, -2.0) ;
   NumEstimation.create(noNumeric, -2.0) ;
   splitPoint.create(noNumeric, DBL_MAX) ;
   //-------------------------------------------------------------
   // create distance matrix
   //-------------------------------------------------------------
   // NumDistance.create(TrainSize, noNumeric) ;
   // DiscDistance.create(TrainSize, noDiscrete) ;


   //-------------------------------------------------------------
   // set number of iterations in main ReliefF loop
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
   // set k
   //-------------------------------------------------------------
   if (eopt.kNearestEqual <= 0)
     kNearestEqual = TrainSize-1 ;
   else
     kNearestEqual = Mmin(eopt.kNearestEqual, TrainSize-1) ;


   //-------------------------------------------------------------
   //  set number of values for discrete
   //-------------------------------------------------------------
   discNoValues.create(noDiscrete) ;
   for (i=0 ; i < noDiscrete ; i++)
      discNoValues[i] = fTree->AttrDesc[fTree->DiscIdx[i]].NoValues  ;

   //-------------------------------------------------------------
   // compute probabilities for discrete missing values
   //-------------------------------------------------------------

   double denominator, valueProb ;

   NAdiscValue.create(noDiscrete) ;

   for (i=0 ; i < noDiscrete ; i++)
      NAdiscValue[i].create(discNoValues[i] +1, 0.0) ;

   for (i=0; i < noDiscrete ; i++)
     for (j=0 ; j < TrainSize ; j++)
        NAdiscValue[i][DiscValues(j,i)] += 1.0 ;

   for (i=0 ; i < noDiscrete ; i++)
   {
      denominator = TrainSize+discNoValues[i]-NAdiscValue[i][0] ;
      NAdiscValue[i][0] = 0.0 ;
      for (j=1; j < NAdiscValue[i].len() ; j++)
      {
         valueProb = (NAdiscValue[i][j]+double(1.0))/denominator ;
         NAdiscValue[i][j] =  double(1.0) - valueProb ;
         // both are missing
         NAdiscValue[i][0] += valueProb * valueProb  ;
      }
      NAdiscValue[i][0] = double(1.0) - NAdiscValue[i][0] ;
    }

   //-------------------------------------------------------------
   //  continuous attribute missing values
   //   it would be better to use density estimationReg with kernel functions
   //-------------------------------------------------------------

   step.create(noNumeric) ;
   NAnumValue.create(noNumeric) ;

   int noIntervals =0;
   for (i=1; i < noNumeric ; i++)
   {
      noIntervals = constNAdiscretizationIntervals ;
      if (TrainSize/noIntervals < constAverageExamplesPerInterval )
        noIntervals = Mmax(2,int(TrainSize/constAverageExamplesPerInterval)) ;
      step[i] =  valueInterval[i]/noIntervals*double(1.000001) ; // 1.000001 - to avoid overflows due to numerical aproximation
      NAnumValue[i].create(noIntervals+1, 0.0) ;
   }
   double intervalIdx ;
   for (i=1; i < noNumeric ; i++)
     for (j=0 ; j < TrainSize ; j++)
       if (isNAcont(NumValues(j,i)))
         NAnumValue[i][0] += 1.0 ;
       else {
    	  intervalIdx =  (NumValues(j,i)-minValue[i])/step[i] ;
    	  #if defined(DEBUG)
    	  if (isNAcont(intervalIdx))
    	   	 merror("Mismatch between NA values or incorrect data.","") ;
    	  #endif
         NAnumValue[i][1+int(intervalIdx)] += 1 ;
       }
   for (i=1 ; i < noNumeric ; i++)
   {
      denominator = TrainSize+noIntervals-NAnumValue[i][0] ;
      NAnumValue[i][0] = 0.0 ;
      for (j=1; j < NAnumValue[i].len() ; j++)
      {
         valueProb = (NAnumValue[i][j]+double(1.0))/denominator ;
         NAnumValue[i][j] =  double(1.0) - valueProb ;
         // both are missing
         NAnumValue[i][0] += valueProb * valueProb  ;
      }
      NAnumValue[i][0] = double(1.0) - NAnumValue[i][0] ;
    }
   //-------------------------------------------------------------
   //  k nearest with distance density and standard deviation for distance density
   //-------------------------------------------------------------
   if (eopt.kNearestExpRank <= 0)
     kDensity = TrainSize - 1 ;
   else
     kDensity = Mmin(eopt.kNearestExpRank, TrainSize-1) ;

   //-------------------------------------------------------------
   // structure for sorting/selecting cases by distance
   //-------------------------------------------------------------
   distSort.create(TrainSize) ;
   distanceArray.create(TrainSize) ;

   //-------------------------------------------------------------
   // variance of distance density
   //-------------------------------------------------------------
   //varianceDistanceDensity = sqr(double(kDensity) / fTree->quotientExpRankDistance) ;
   //varianceDistanceDensity = sqr(fTree->quotientExpRankDistance) / sqrt(-log(0.9)) ;
   varianceDistanceDensity = sqr(eopt.quotientExpRankDistance) ;

   //-------------------------------------------------------------
   // prepare for construction with MDL
   //-------------------------------------------------------------
   // compute prior MSE
   double sum=0.0, sumSq = 0.0 ;
   for (i=0 ; i < TrainSize ; i++)
   {
      sum += NumValues(i,0) ;
      sumSq += sqr(NumValues(i,0)) ;
   }

   priorMSE = sumSq / double(TrainSize) - sqr(sum/double(TrainSize)) ;
   if (priorMSE > 0)
     priorMSE = sqrt(priorMSE) ;
   else
     priorMSE = 0.0 ;

   // copy Gini2EntropyConst
   //Gini2EntropyConst = fTree->Gini2EntropyConst ;


#if defined(KDTREE)
   kdT.setBucketSize(10) ;
   marray<int> DataTrain(TrainSize) ;
   for (i=0 ; i<TrainSize ; i++)
      DataTrain[i] = i ;

   kdT.insertAll(DataTrain, TrainSize, &NumValues, &DiscValues, &discNoValues,
                 &minValue, &maxValue,  &valueInterval,
                 &step, &NAdiscValue, &NAnumValue,
                 #if defined(RAMP_FUNCTION)
                   &DifferentDistance, &EqualDistance, &CAslope,
                 #endif
                  1, noNumeric, 0, noDiscrete)  ;
#endif
#if defined(MAHALANOBIS)
   // compute inverse of covariance matrix
   double **covM = new Pdouble[noNumeric];
   for (i=0 ; i < noNumeric ; i++)
   {
	   covM[i] = new double[noNumeric] ;
       for (j=0 ; j < noNumeric; j++)
		   covM[i][j] = 0.0 ;
   }

   int k ;

   // compute mean values
   for (i=1 ; i < noNumeric; i++)
   {
	  for (k=0 ; k < TrainSize ; k++)
		 covM[i][0] += NumValues(k,i) ;
	  covM[i][0] /= double(TrainSize) ;
   }
   // compute covariances
   for (i=1 ; i < noNumeric; i++)
     for (j=1 ; j < noNumeric; j++)
     {
		 if (j < i)
			covM[i][j] = covM[j][i] ;
		 else {
		   for (k=0 ; k < TrainSize ; k++)
             covM[i][j] += (NumValues(k,i) - covM[i][0])*(NumValues(k,j) - covM[j][0]) ;
		   covM[i][j] /= double(TrainSize) ;
		 }
	 }

   // now for inverse of elements 1..N, 1..N, where N=noNumeric-1
   // from Numerical Recipes in C
   double *col = new double[noNumeric];
   int *indx = new int[noNumeric] ;


   covMI.create(noNumeric, noNumeric, 0.0) ;
   double d ;

   ludcmp(covM,noNumeric-1,indx,&d) ;
   for (j=1; j < noNumeric ;j++)
   {
	   for (i=1; i<noNumeric; i++)
          col[i] = 0.0 ;
	   col[j]=1 ;
	   lubksb(covM,noNumeric-1,indx,col) ;
	   for (i=1 ; i < noNumeric ; i++)
		   covMI(i,j) = col[i] ;
   }
   delete [] indx ;
   delete [] col ;
   for (i=0 ; i < noNumeric ; i++)
	   delete [] covM[i] ;

   // print out the matrix
//   for (i=1 ; i < noNumeric; i++)
//   {
//     for (j=1 ; j < noNumeric; j++)
//       printf("%8.5f ",covMI(i,j)) ;
//     printf("\n") ;
//   }


#endif

}





// ***************************************************************************
//
//                          CReliefDensity
//      probability densities version with probailities from
//                      Bayes rule
//
// ***************************************************************************
void estimationReg::CReliefDensity(int contAttrFrom, int contAttrTo,
                                int discAttrFrom, int discAttrTo,  int distanceType)
{

   // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimationRegs arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
   marray<double> incContDiffA(NoContEstimated), incDiscDiffA(NoDiscEstimated) ;
   double incDiffC ;
   marray<double> incContDiffCdiffA(NoContEstimated), incDiscDiffCdiffA(NoDiscEstimated) ;

   // these variables might be used for normalization of GiniGain'
   contDiffA.create(NoContEstimated, 0.0) ;
   discDiffA.create(NoDiscEstimated, 0.0) ;
   diffC = 0.0 ;

   marray<double> contDiffCdiffA(NoContEstimated, 0.0), discDiffCdiffA(NoDiscEstimated, 0.0) ;

   // main ReliefF loop
   int current, i, iAttr, k ;

   double distanceSum, normDistance ;
   double  Adiff, ClassAndDistanceFactor ;
   int idx, example ;

   // we have to compute distances for the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   #if defined(PRINT_EACH_ITERATION)
     char path[MaxPath] ;
     int iPrint, contCount=0, discCount=0 ;
     FILE *fileRelief ;
     double dfC ;
     snprintf(path, MaxPath, "%s%s.%02dei",fTree->resultsDirectory, fTree->domainName,fTree->splitIdx) ; // estimationReg of weights at each iteration
     if ((fileRelief = fopen(path,"w"))==NULL)
     {
        merror("estimationReg::ReliefF cannot open file for writting weights of each iteration: ", path)  ;
     }
     else {
        fprintf(fileRelief, "\nRelief weights changing with number of iterations\n") ;
        fTree->printEstimationHead(fileRelief) ;
     }

   #endif

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;


   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       current = sampleIdx[iterIdx] ;

      // compute distances
      // computeDistances(current) ;

      #if defined(KDTREE)
         // compute distance densities with kd trees
         prepareDistanceFactorsKD(current, distanceSum, distanceType) ;
      #else
         // compute distance density
         prepareDistanceFactors(current, distanceSum, distanceType) ;
      #endif
      incContDiffA.init(0.0) ;
      incDiscDiffA.init(0.0) ;
      incContDiffCdiffA.init(0.0) ;
      incDiscDiffCdiffA.init(0.0) ;
      incDiffC = 0.0 ;

      for (i=0 ; i < distanceArray.filled() ; i++)  // instead of kDensity
      {
         example = distanceArray[i].value ;
         normDistance = distanceArray[i].key ;
         // variants of computing class distance
         //   currentCdiff = double(fabs( NumValues(current,0) - NumValues(i,0) )) ;
         //   currentCdiff = CAdiff(0,current,i) ;
         // currentCdiff = CAdiff(0, current, example) ;
         // currentCdiff = NumDistance(example, 0) ;
         ClassAndDistanceFactor = normDistance * CAdiff(0, current, example)  ;
         incDiffC += ClassAndDistanceFactor ;
         // adjust the weights for all the attributes and values
         // computation is done on input attributes (whose values are in Values matrix)
         for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
         {
            idx = iAttr - contAttrFrom ;
            Adiff = CAdiff(iAttr, current, example) ;
            // Adiff = NumDistance(example, iAttr) ;
            incContDiffCdiffA[idx] +=  ClassAndDistanceFactor * Adiff  ;
            incContDiffA [idx] += Adiff * normDistance ;
         }
         for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
         {
            idx = iAttr - discAttrFrom ;
            Adiff = DAdiff(iAttr, current, example) ;
            // Adiff = DiscDistance(example, iAttr) ;
            incDiscDiffCdiffA[idx] +=  ClassAndDistanceFactor * Adiff  ;
            incDiscDiffA[idx] += Adiff * normDistance ;
         }
      }
      // normalization of increment
      diffC += incDiffC/distanceSum ;
      for (idx=0 ; idx < NoContEstimated ; idx++)
      {
         contDiffCdiffA[idx] += incContDiffCdiffA[idx]/distanceSum ;
         contDiffA[idx] += incContDiffA[idx]/distanceSum ;
      }
      for (idx=0 ; idx <  NoDiscEstimated ; idx++)
      {
         discDiffCdiffA[idx] += incDiscDiffCdiffA[idx]/distanceSum ;
         discDiffA[idx] += incDiscDiffA[idx]/distanceSum ;
      }

      #if defined(PRINT_EACH_ITERATION)
        fprintf(fileRelief, "%18d,", iterIdx+1) ;
        contCount = discCount = 0 ;
        for (iPrint=1 ; iPrint <= fTree->noAttr; iPrint++)
        {
           dfC = diffC ;
           if (dfC < epsilon)
            dfC = epsilon ;
           if (diffC + epsilon > iterIdx+1)
              dfC = iterIdx+1 - epsilon ;
           if (fTree->AttrDesc[iPrint].continuous)
           {
             fprintf(fileRelief, "%10.5f, ",  contDiffCdiffA[contCount]/dfC -
                         (contDiffA[contCount] - contDiffCdiffA[contCount]) / double(iterIdx+1 - dfC)) ;
             contCount++ ;
           }
           else {
             fprintf(fileRelief, "%10.5f, ", discDiffCdiffA[discCount]/dfC -
                (discDiffA[discCount] - discDiffCdiffA[discCount]) / double(iterIdx+1 - dfC) ) ;
             discCount++ ;
           }
        }
        fprintf(fileRelief, "\n") ;
      #endif

   }
   if (diffC < epsilon)
     diffC = epsilon ;
   if (diffC + epsilon > NoIterations)
      diffC = NoIterations - epsilon ;
   for (k=contAttrFrom ; k < contAttrTo ; k++)
   {
      idx = k - contAttrFrom ;
      NumEstimation[k] =  contDiffCdiffA[idx]/diffC -
                (contDiffA[idx] - contDiffCdiffA[idx]) / (NoIterations - diffC)  ;
      contDiffA[idx]  /= double(NoIterations) ; // if used in gini gain
	  #if defined(EXPLORE)
        Rprintf("%10.5f %10.5f %10.5f %10d\n",diffC,contDiffCdiffA[idx],contDiffA[idx]*NoIterations,NoIterations) ;
	  #endif
   }
   for (k=discAttrFrom ; k < discAttrTo ; k++)
   {
      idx = k - discAttrFrom ;
      DiscEstimation[k] =  discDiffCdiffA[idx]/diffC -
                (discDiffA[idx] - discDiffCdiffA[idx]) / (NoIterations - diffC)   ;
      discDiffA[idx]  /= double(NoIterations) ; // if used in gini gain
	  #if defined(EXPLORE)
        Rprintf("%10.5f %10.5f %10.5f %10d\n",diffC,discDiffCdiffA[idx],discDiffA[idx]*NoIterations,NoIterations) ;
	  #endif
   }
   diffC /=  double(NoIterations) ; // if used in gini gain

   #if defined(PRINT_EACH_ITERATION)
     fclose(fileRelief) ;
   #endif

}

// ***************************************************************************
//
//                          prepareDistanceFactors
// computation of distance probability weight factors for given example
//
// ***************************************************************************
void estimationReg::prepareDistanceFactors(int current, double &distanceSum, int distanceType)
{

// we use only original attributes to obtain distance in attribute space

   int kSelected = 0 ;
   switch (distanceType)
   {
      case estRReliefFbestK:
           kSelected = TrainSize-1 ;
           break ;

      case estRReliefFexpRank:
      case estRReliefFdistance:
      case estRReliefFsqrDistance:
           kSelected = kDensity ;
           break ;

      case estRReliefFkEqual:
           kSelected = kNearestEqual ;
           break ;

      default: merror("estimationReg::prepareDistanceFactors","invalid distance type") ;
   }
   int i ;
   // distances in attributes space
   for (i=0 ; i < TrainSize; i++)
   {
      distSort[i].key =  caseDist(current, i) ;
      distSort[i].value = i ;
   }
   distSort.setFilled(TrainSize) ;
   // finding K best in increasing order with heapsort method
   distSort.sortKsmallest(kSelected+1) ;

   // we eliminate the current element - it has too be the one of the closest
   for( i = distSort.filled() - 1 ; i >= 0 ; i--)
   {
      if (current == distSort[i].value)
      {
         distSort[i] = distSort[distSort.filled() - 1] ;
         break ;
      }
   }

   // now we have the needed elements sorted
   // and we can copy them into resulting array
   distanceArray.setFilled(kSelected) ;
   int idx = distSort.filled() - 2 ; // one less than the boundary - the last is the current itself
   double factor ;
   // depending on tpe of distance, copy the nearest cases
   // and their distance factors into resulting array
   switch (distanceType)
   {
        case estRReliefFexpRank: // RREliefF with distance density
          {
            factor = 1.0  ;
            distanceArray[0].key =  factor ;
            distanceArray[0].value = distSort[idx].value ;
            distanceSum = factor ;
            idx -- ;
            for (i=1 ; i < distanceArray.filled() ; i++)
            {
               if (distSort[idx].key != distSort[idx+1].key)
                 factor = double(exp(-sqr(double(i))/varianceDistanceDensity)) ;
                 //factor = double(1.0) - double(i)/TrainSize ;
               distanceArray[i].key =  factor ;
               distanceArray[i].value = distSort[idx].value ;
               distanceSum += factor ;
               idx -- ;
            }
          }
          break ;
        case estRReliefFdistance: // RReliefF with actual distances
          {
            double minNonZero = 1.0 ;
            for (i=idx ; i >= 0 ; i--)
               if (distSort[i].key  > 0.0)
               {
                  minNonZero = distSort[i].key ;
                  break ;
               }
            distanceSum = 0.0 ;
            for (i=0 ; i < distanceArray.filled() ; i++)
            {
              if (distSort[idx].key > 0)
                 factor = 1.0 / distSort[idx].key ;
              else
                 factor = 2.0 / minNonZero ;
              distanceArray[i].key = factor ;
              distanceArray[i].value = distSort[idx].value ;
              distanceSum += factor ;
              idx -- ;
            }
          }
          break ;
        case estRReliefFsqrDistance: // RReliefF with actual distances squared
          {
            double minNonZero = 1.0 ;
            for (i=idx ; i >= 0 ; i--)
               if (distSort[i].key  > 0.0)
               {
                  minNonZero = distSort[i].key ;
                  break ;
               }
            distanceSum = 0.0 ;
            for (i=0 ; i < distanceArray.filled() ; i++)
            {
              if (distSort[idx].key > 0)
                 factor = 1.0 / sqr(distSort[idx].key) ;
              else
                 factor = 2.0 / sqr(minNonZero) ;
              distanceArray[i].key = factor ;
              distanceArray[i].value = distSort[idx].value ;
              distanceSum += factor ;
              idx -- ;
            }
          }
          break ;
        case estRReliefFbestK: // RReliefF with best of k
        case estRReliefFkEqual: // RReliefF with equal k nearest
          {
            for (i=0 ; i < distanceArray.filled() ; i++)
            {
              distanceArray[i].key = 1.0  ;
              distanceArray[i].value = distSort[idx].value ;
              idx -- ;
            }
            distanceSum =  distanceArray.filled() ;

          }
          break ;
   }
}




#if defined(MANHATTAN)
//************************************************************
//
//                           caseDist
//                           --------
//
//        computes distance between two instances
//
//************************************************************
double estimationReg::caseDist(int I1, int I2)
{
   double Distance = 0.0;

   int i ;
   for (i=0 ; i < noDiscrete ; i++)
      Distance += DAdiff(i,I1, I2) ;

   for (i=1; i<noNumeric; i++)
      Distance += CAdiff(i, I1, I2) ;

   return  Distance ;
}
#endif


#if defined(EUCLID)
//************************************************************
//
//                           caseDist
//                           --------
//
//        computes distance between two instances
//
//************************************************************
double estimationReg::caseDist(int I1, int I2)
{
   double Distance = 0.0;

   int i ;
   for (i=0 ; i < noDiscrete ; i++)
      Distance += sqr(DAdiff(i,I1, I2)) ;

   for (i=1; i<noNumeric; i++)
      Distance += sqr(CAdiff(i, I1, I2)) ;

   return  sqrt(Distance) ;
}
#endif

#if defined(MAHALANOBIS)
//************************************************************
//
//                           caseDist
//                           --------
//
//        computes distance between two instances
//
//************************************************************
double estimationReg::caseDist(int I1, int I2)
{
   double Distance = 0.0;

   int i,j ;
   for (i=0 ; i < noDiscrete ; i++)
      Distance += DAdiff(i,I1, I2) ;

   marray<double> df(noNumeric, 0.0), d(noNumeric, 0.0) ;
   for (i=1; i<noNumeric; i++)
       df[i] = NumValues(I1, i) - NumValues(I2, i);

   for (i=1; i<noNumeric; i++)
      for (j=1; j<noNumeric; j++)
         d[i] += df[j] * covMI(j,i) ;

   for (i=1; i<noNumeric; i++)
      Distance += d[i] * df[i] ;

   return  Distance ;
}
#endif


// ***************************************************************************
//
//                    CARamp
//          ramp function of continuous attribute (or class)
//
// ***************************************************************************
#if defined(RAMP_FUNCTION)
inline double estimationReg::CARamp(int AttrIdx, double distance)
{
  if (distance >= DifferentDistance[AttrIdx])
     return 1.0 ;
  if (distance <= EqualDistance[AttrIdx])
     return 0.0 ;

  return  (distance - EqualDistance[AttrIdx]) * CAslope[AttrIdx] ;
}
#endif


#if ! defined(CIRCULAR_DIST)
// ***************************************************************************
//
//                   CAdiff
//              diff function for continuous attribute
//
// ***************************************************************************
double estimationReg::CAdiff(int AttrIdx, int I1, int I2)
{
   double cV1 = NumValues(I1, AttrIdx) ;
   double cV2 = NumValues(I2, AttrIdx) ;
   if (isNAcont(cV1))
      return NAnumDiff(AttrIdx,cV2) ;
    else
      if (isNAcont(cV2))
        return NAnumDiff(AttrIdx,cV1) ;
       else
         #if defined(RAMP_FUNCTION)
           return CARamp(AttrIdx, fabs(cV2 - cV1) ) ;
        #else
           return  fabs(cV2 - cV1) / valueInterval[AttrIdx] ;
        #endif
}
#endif

// ***************************************************************************
//
//                   DAdiff
//              diff function of discrete attribute
//
// ***************************************************************************
double estimationReg::DAdiff(int AttrIdx, int I1, int I2)
{

  // we assume that missing value has value 0
  int dV1 = DiscValues(I1, AttrIdx) ;
  int dV2 = DiscValues(I2, AttrIdx) ;
  if (dV1 == NAdisc)
     return NAdiscValue[AttrIdx][int(dV2)] ;
  else
    if (dV2 == NAdisc)
      return NAdiscValue[AttrIdx][int(dV1)] ;
     else
       if (dV1 == dV2)
         return  0.0 ;
       else
         return 1.0 ;
}


#if defined(CIRCULAR_DIST)
// ***************************************************************************
//
//                   CAdiff
//              diff function for continuous attribute
//
// ***************************************************************************
inline double estimationReg::CAdiff(int AttrIdx, int I1, int I2)
{
   double cV1 = NumValues(I1, AttrIdx) ;
   double cV2 = NumValues(I2, AttrIdx) ;
   if (isNAcont(cV1)
      return NAnumDiff(AttrIdx,cV2) ;
    else
      if (isNAcont(cV2))
        return NAnumDiff(AttrIdx,cV1) ;
	  else {
		   double d1 = cV1 - cV2 ;
	       if (d1 < 0)
			  d1 += CIRCULAR_MOD  ;
	       double d2 = cV2 - cV1 ;
	       if (d2 < 0)
			  d2 += CIRCULAR_MOD  ;
		   double d = Mmin(d1,d2) ;

           #if defined(RAMP_FUNCTION)
             return CARamp(AttrIdx, d ) ;
           #else
             return  d / valueInterval[AttrIdx] ;
        #endif
	  }
}
#endif

// ***************************************************************************
//
//                   NAnumDiff
//         diff function for missing values at continuous attribute
//
// ***************************************************************************
double estimationReg::NAnumDiff(int AttrIdx, double Value)
{
   if (isNAcont(Value))
      return NAnumValue[AttrIdx][0] ;

   return NAnumValue[AttrIdx][int((Value-minValue[AttrIdx])/step[AttrIdx]) +1] ;
}



// ***************************************************************************
//
//                          prepareDistanceFactorsKD
// computation of distance probability weight factors for given example
//
// ***************************************************************************
void estimationReg::prepareDistanceFactorsKD(int current, double &distanceSum, int usedEstimator)
{

// we use only original attributes to obtain distance in attribute space

   int kSelected = 0 ;
   switch (usedEstimator)
   {
      case estRReliefFbestK:
           kSelected = TrainSize-1 ;
           break ;

      case estRReliefFexpRank:
      case estRReliefFdistance:
      case estRReliefFsqrDistance:
           kSelected = kDensity ;
           break ;

      case estRReliefFkEqual:
           kSelected = kNearestEqual ;
           break ;

      default: merror("estimationReg::prepareDistanceFactors","invalid distance type") ;
   }
   int i ;

   // find k nearest
   kdT.findK(current, kSelected+1) ;

   // now we have the needed elements sorted
   // and we cen set the size of resulting array
   distanceArray.setFilled(kdT.PQnear.filled()-1) ; // one less than the nearest
   double factor ;
   sortRec lastRec ;
   //  copy the nearest cases  into resulting array
   for (i=distanceArray.filled()-1 ; i>=0 ; i--)
       kdT.PQnear.deleteMaxPQmax(distanceArray[i]) ;
   kdT.PQnear.deleteMaxPQmax(lastRec) ;

   // now we eliminate the current element
   if (current != lastRec.value)
   {
      i=0 ;
      for ( i = 0 ; i < distanceArray.filled() ; i++)
         if (current == distanceArray[i].value)
         {
            distanceArray[i] = lastRec ;
            break ;
         }
   }

   // depending on tpe of distance set the distance factors
   switch (usedEstimator)
   {
        case estRReliefFexpRank: // RREliefF with distance density
          {

            factor = 1.0 ;
            distanceSum = factor ;
            for (i=1 ; i < distanceArray.filled() ; i++)
            {
              if (distanceArray[i].key != distanceArray[i-1].key)
              {
                 distanceArray[i-1].key =  factor ;
                 factor = double(exp(-sqr(double(i))/varianceDistanceDensity)) ;
              }
              else
                 distanceArray[i-1].key =  factor ;

               distanceSum += factor ;
            }
            distanceArray[i-1].key =  factor ;

          }
          break ;
       case estRReliefFdistance: // RReliefF with actual distances
         {
            // as we give weight of 1/distance, distance =0 would give infinite;
            // to prevent that we find minimal non-zero distance and give the
            // instances with zero distance double weight of minimal non-zero
            double minNonZero = 1.0 ;
            for (i=0 ; i < distanceArray.filled() ; i++)
               if (distanceArray[i].key  > 0.0)
               {
                  minNonZero = distanceArray[i].key ;
                  break ;
               }
            distanceSum = 0.0 ;
            for (i=0 ; i < distanceArray.filled() ; i++)
            {
              if (distanceArray[i].key > 0)
                 factor = 1.0 / distanceArray[i].key ;
              else
                 factor = 2.0 / minNonZero ;
              distanceArray[i].key = factor ;
              distanceSum += factor ;
            }
          }
          break ;
        case estRReliefFsqrDistance: // RReliefF with actual distances
          {
            // as we give weight of 1/distance, distance =0 would give infinite;
            // to prevent that we find minimal non-zero distance and give the
            // instances with zero distance double weight of minimal non-zero
            double minNonZero = 1.0 ;
            for (i=0 ; i < distanceArray.filled() ; i++)
               if (distanceArray[i].key  > 0.0)
               {
                  minNonZero = distanceArray[i].key ;
                  break ;
               }
            distanceSum = 0.0 ;
            for (i=0 ; i < distanceArray.filled() ; i++)
            {
              if (distanceArray[i].key > 0)
                 factor = 1.0 / sqr(distanceArray[i].key) ;
              else
                 factor = 2.0 / sqr(minNonZero) ;
              distanceArray[i].key = factor ;
              distanceSum += factor ;
            }
          }
          break ;

        case estRReliefFbestK:  // RReliefF with best of k
        case estRReliefFkEqual: // RReliefF with equal k nearest
          {
            for (i=0 ; i < distanceArray.filled() ; i++)
               distanceArray[i].key = 1.0  ;
            distanceSum =  distanceArray.filled() ;

          }
          break ;
   }
}





// ***************************************************************************
//
//                       ReliefFbestK
//
//                       ------------
//   contains the version of ReliefF:
//   - with best estimate of all possible k nearest
//
//
// ***************************************************************************
void estimationReg::RReliefFbestK(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int usedEstimator)
{
   // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimationRegs arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
   int maxK = TrainSize -1 ;

   marray<double> NdC(maxK, 0.0) ;
   mmatrix<double> NdAcont(NoContEstimated, maxK, 0.0), NdAdisc(NoDiscEstimated, maxK, 0.0) ;
   mmatrix<double> NdCdAcont(NoContEstimated, maxK, 0.0), NdCdAdisc(NoDiscEstimated, maxK, 0.0) ;

   marray<double> contCAcorrection(NoContEstimated), discCAcorrection(NoDiscEstimated),
                  contAcorrection(NoContEstimated), discAcorrection(NoDiscEstimated) ;
   double Ccorrection ;

   // main ReliefF loop
   int current, i, iAttr, k ;

   double distanceSum, exampleDistance ;
   double  Cdiff, Adiff ;
   int idx, exampleIdx ;

   // we have to compute distances for the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
      current = sampleIdx[iterIdx] ;

      #if defined(KDTREE)
         // compute distance densities with kd trees
         prepareDistanceFactorsKD(current, distanceSum, usedEstimator) ;
      #else
         // compute distance density
         prepareDistanceFactors(current, distanceSum, usedEstimator) ;
      #endif

      discCAcorrection.init(0.0) ;
      contCAcorrection.init(0.0) ;
      discAcorrection.init(0.0) ;
      contAcorrection.init(0.0) ;
      Ccorrection = 0.0 ;
      distanceSum = 0.0 ;
      for (i=0 ; i < distanceArray.filled() ; i++)  // instead of kDensity
      {
         exampleIdx = distanceArray[i].value ;
         exampleDistance = distanceArray[i].key ;
         distanceSum += exampleDistance ;
         Cdiff = CAdiff(0, current, exampleIdx ) ;

         Ccorrection += Cdiff * exampleDistance;
         NdC[i] += Ccorrection / distanceSum ;
         // adjust the weights for all the attributes and values
         for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
         {
            idx = iAttr - contAttrFrom ;
            Adiff = CAdiff(iAttr, current, exampleIdx) ;

            contAcorrection[idx] += Adiff * exampleDistance ;
            NdAcont(idx, i) += contAcorrection[idx]/distanceSum ;
            contCAcorrection[idx] += Cdiff * Adiff * exampleDistance ;
            NdCdAcont(idx, i) += contCAcorrection[idx]/distanceSum ;
         }
         for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
         {
            idx = iAttr - discAttrFrom ;
            Adiff = DAdiff(iAttr, current, exampleIdx) ;

            discAcorrection[idx] += Adiff * exampleDistance ;
            NdAdisc(idx, i) += discAcorrection[idx] /distanceSum;
            discCAcorrection[idx] += Cdiff * Adiff * exampleDistance ;
            NdCdAdisc(idx, i) += discCAcorrection[idx]/distanceSum ;
         }
      }
   }


   double est, bestEst ;
   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      bestEst = -1.0 ;
      for (k=0 ; k < maxK ; k++)
      {
         est = NdCdAcont(idx, k)/NdC[k] - (NdAcont(idx, k) - NdCdAcont(idx, k))/(NoIterations - NdC[k]) ;
         if (est > bestEst)
             bestEst = est ;
      }
      NumEstimation[iAttr] = bestEst ;
      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimationReg::RReliefFbestK", "computed continuous weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      bestEst = -1.0 ;
      for (k=0 ; k < maxK ; k++)
      {
         est = NdCdAdisc(idx, k)/NdC[k] - (NdAdisc(idx, k) - NdCdAdisc(idx, k))/(NoIterations - NdC[k]) ;
         if (est > bestEst)
             bestEst = est ;
      }
      DiscEstimation[iAttr] = bestEst ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimationReg::RReliefFbestK", "computed estimationRegs are out of scope") ;
      #endif
   }

   #if defined(PRINT_EACH_K)
     char path[MaxPath] ;
     FILE *fileRelief ;
     snprintf(path, MaxPath, "%s%s.%02dek",fTree->resultsDirectory, fTree->domainName,fTree->splitIdx) ; // estimationReg of weights for each k
     if ((fileRelief = fopen(path,"w"))==NULL)
     {
        merror("estimationReg::RReliefFbestK cannot open file for writting estimationReg for each k: ", path)  ;
     }
     fprintf(fileRelief, "\nRReliefF weights changing with k nearest neighbours\n") ;
     fTree->printEstimationHead(fileRelief) ;

     int contCount,discCount;
     for (k=0 ; k < maxK ; k++)
     {
       fprintf(fileRelief, "%18d,",k+1) ;
       contCount = discCount = 0 ;
       for (i=1 ; i <= fTree->noAttr; i++)
       if (fTree->AttrDesc[i].continuous)
       {
          fprintf(fileRelief, "%10.5f, ", NdCdAcont(contCount, k)/NdC[k] - (NdAcont(contCount, k) - NdCdAcont(contCount, k))/(NoIterations - NdC[k]) ) ;
          contCount++ ;
       }
       else {
         fprintf(fileRelief, "%10.5f, ", NdCdAdisc(discCount, k)/NdC[k] - (NdAdisc(discCount, k) - NdCdAdisc(discCount, k))/(NoIterations - NdC[k])) ;
         discCount++ ;
       }
       fprintf(fileRelief, "\n") ;
     }
     fclose(fileRelief) ;
   #endif



}


// ***************************************************************************
//
//                       RReliefF
//
//                       ------------
//   contains the implemantation of RReliefF from ICML97 article
//
//
// ***************************************************************************
void estimationReg::RReliefF(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int usedEstimator)
{
   // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimationRegs arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;

   double NdC =  0.0 ;
   marray<double> NdAcont(NoContEstimated, 0.0), NdAdisc(NoDiscEstimated, 0.0) ;
   marray<double> NdCdAcont(NoContEstimated, 0.0), NdCdAdisc(NoDiscEstimated, 0.0) ;

   // main ReliefF loop
   int current, i, iAttr;

   double distanceSum, normDistance ;
   double  Cdiff, Adiff ;
   int idx, exampleIdx ;

   // we have to compute distances for the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       current = sampleIdx[iterIdx] ;

      #if defined(KDTREE)
         // compute distance densities with kd trees
         prepareDistanceFactorsKD(current, distanceSum, usedEstimator) ;
      #else
         // compute distance density
         prepareDistanceFactors(current, distanceSum, usedEstimator) ;
      #endif

      for (i=0 ; i < distanceArray.filled() ; i++)  // instead of kDensity
      {
         exampleIdx = distanceArray[i].value ;
         normDistance = distanceArray[i].key / distanceSum ;
         Cdiff = CAdiff(0, current, exampleIdx) ;

         NdC += Cdiff * normDistance;    ;
         // adjust the weights for all the attributes and values
         for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
         {
            idx = iAttr - contAttrFrom ;
            Adiff = CAdiff(iAttr, current, exampleIdx) ;

            NdAcont[idx] += Adiff * normDistance ;
            NdCdAcont[idx] += Cdiff * Adiff * normDistance  ;
         }
         for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
         {
            idx = iAttr - discAttrFrom ;
            Adiff = DAdiff(iAttr, current, exampleIdx) ;

            NdAdisc[idx] += Adiff * normDistance ;
            NdCdAdisc[idx] += Cdiff * Adiff * normDistance  ;

         }
      }
   }

   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      NumEstimation[iAttr] = NdCdAcont[idx]/NdC - (NdAcont[idx] - NdCdAcont[idx])/(NoIterations - NdC) ;

      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimationReg::RReliefFbestK", "computed continuous weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      DiscEstimation[iAttr] = NdCdAdisc[idx]/NdC - (NdAdisc[idx] - NdCdAdisc[idx])/(NoIterations - NdC) ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimationReg::RReliefFbestK", "computed estimationRegs are out of scope") ;
      #endif
   }
}




// ***************************************************************************
//
//                          CRsimilarity
//    continuous class, discrete and continuous attributes
//      probability densities version with similarities
//
// ***************************************************************************
/*
void estimationReg::CRsimilarity(int contAttrFrom, int contAttrTo,
                               int discAttrFrom, int discAttrTo)
{

   // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;
   // contIncrement.create(noNumeric) ;
   // discIncrement.create(noDiscrete) ;

   // main ReliefF loop
   int current, i, iAttr, k, example ;

   marray<double>  classSimilarityArray(kDensity) ;
   double distanceSum, avgClassSimilarity ;
   double ClassAndDistanceFactor, normFactor ;

   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       if (NoIterations == TrainSize)
          current = iterIdx ;
       else
           current =  randBetween(0, TrainSize) ;

      // compute distances
      computeDistances(current) ;

      // compute distance density
      prepareDistanceFactors(distanceSum, 2 ) ;
      prepareClassDistanceFactors(classSimilarityArray, avgClassSimilarity ) ;

      contIncrement.init(contAttrFrom,contAttrTo,0.0) ;
      discIncrement.init(discAttrFrom,discAttrTo,0.0) ;

      for (i=0 ; i < kDensity ; i++)
      {

         example = distanceArray[i].value ;

         ClassAndDistanceFactor = distanceArray[i].key * classSimilarityArray[i] ;
         // adjust the weights for all the attributes and values
         // computation is done on input attributes (whose values are in Values matrix)
         for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
         {
            //contIncrement[iAttr] -= ClassAndDistanceFactor * CAdiff(iAttr, current, i) ;
            contIncrement[iAttr] -= ClassAndDistanceFactor * NumDistance(example,iAttr) ;
         }
         for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
         {
            // discIncrement[iAttr] -= ClassAndDistanceFactor * DAdiff(iAttr, current, i) ;
            discIncrement[iAttr] -= ClassAndDistanceFactor * DiscDistance(example, iAttr) ;
         }
      }
      // normalisation of increment
      normFactor = distanceSum * avgClassSimilarity ;
      for (k=contAttrFrom ; k < contAttrTo ; k++)
        NumEstimation[k] += contIncrement[k]/normFactor ;
      for (k=discAttrFrom ; k < discAttrTo ; k++)
        DiscEstimation[k] += discIncrement[k]/normFactor ;
   }

   // normalization of estimate
   for (k=contAttrFrom ; k < contAttrTo ; k++)
      NumEstimation[k] /=  double(NoIterations) ;
   for (k=discAttrFrom ; k < discAttrTo ; k++)
      DiscEstimation[k] /= double(NoIterations) ;
}
*/

// ***************************************************************************
//
//                     prepareClassDistanceFactors
// computation of distances in class space for given example
//
// ***************************************************************************
/*
void estimationReg::prepareClassDistanceFactors(
         marray<double> &classSimilarityArray, double &avgClassSimilarity)
{
   // distances in class space
   avgClassSimilarity = 0.0 ;
   int i ;
   for (i=0 ; i < kDensity; i++)
      avgClassSimilarity += NumDistance(distanceArray[i].value, 0) ;
   avgClassSimilarity /= double(kDensity) ;
   if (avgClassSimilarity < epsilon)   // degenerated case - prevent divide by zero
      avgClassSimilarity = epsilon ;

   for (i=0 ; i < kDensity; i++)
     classSimilarityArray[i] = avgClassSimilarity - NumDistance(i,0) ;

}
*/

// ***************************************************************************
//
//                          CReliefK
//    continuous class, discrete and continuous attributes
//
//                  Relief with k-nearest
//
// ***************************************************************************
/*
void estimationReg::CReliefK(int contAttrFrom, int contAttrTo,
                          int discAttrFrom, int discAttrTo)
{

   // initialization of estimationRegs
   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimationRegs arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
   marray<double> incContDiffA(NoContEstimated), incDiscDiffA(NoDiscEstimated) ;
   double incDiffC ;
   marray<double> incContDiffCdiffA(NoContEstimated), incDiscDiffCdiffA(NoDiscEstimated) ;

   marray<double> contDiffA(NoContEstimated), discDiffA(NoDiscEstimated) ;
   double diffC ;
   marray<double> contDiffCdiffA(NoContEstimated), discDiffCdiffA(NoDiscEstimated) ;

   // main ReliefF loop
   int current, i, iAttr, k ;

   marray<sortRec> distanceArray(TrainSize) ;
   double  currentCdiff, Adiff ;
   int idx ;

   contDiffA.init(0.0) ;
   discDiffA.init(0.0) ;
   contDiffCdiffA.init(0.0) ;
   discDiffCdiffA.init(0.0) ;
   diffC = 0.0 ;

   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       if (NoIterations == TrainSize)
          current = iterIdx ;
       else
          current =  randBetween(0, TrainSize) ;

      // compute distances
      computeDistances(current) ;

      // compute distances in attribute space and sort them in ascending order
      for (i=0 ; i < TrainSize; i++)
      {
        distanceArray[i].key = CaseDistance(i) ;
        distanceArray[i].value = i ;
      }
      // disable the current
      distanceArray[current].key = noNumeric + noDiscrete +100 ;
      distanceArray.setFilled(TrainSize) ;
      distanceArray.sort(ascSortComp) ;

      incContDiffA.init(0.0) ;
      incDiscDiffA.init(0.0) ;
      incContDiffCdiffA.init(0.0) ;
      incDiscDiffCdiffA.init(0.0) ;
      incDiffC = 0.0 ;

      i=0 ;
      while ( i < kNearestEqual || (distanceArray[i].key == distanceArray[i-1].key && i < TrainSize -1) )
      {
        // currentCdiff = double(fabs( NumValues(current,0) - NumValues(i,0) )) ;
        // currentCdiff = CAdiff(0,current,distanceArray[i].value) ;
        currentCdiff = NumDistance(distanceArray[i].value, 0) ;
        incDiffC += currentCdiff ;
        // adjust the weights for all the attributes and values
        // computation is done on input attributes (whose values are in Values matrix)
        for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
        {
          idx = iAttr - contAttrFrom ;
          //Adiff = CAdiff(iAttr, current, distanceArray[i].value) ;
          Adiff = NumDistance(distanceArray[i].value, iAttr) ;
          incContDiffCdiffA[idx] +=  currentCdiff * Adiff  ;
          incContDiffA [idx] += Adiff  ;
        }
        for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
        {
          idx = iAttr - discAttrFrom ;
          //Adiff = DAdiff(iAttr, current, distanceArray[i].value) ;
          Adiff = DiscDistance(distanceArray[i].value, iAttr) ;
          incDiscDiffCdiffA[idx] +=  currentCdiff * Adiff  ;
          incDiscDiffA[idx] += Adiff ;
        }
        i++ ;
      }
      // normalisation of increment

      diffC += incDiffC/double(i) ;
      for (idx=0 ; idx < NoContEstimated ; idx++)
      {
        if (incContDiffA[idx] > epsilon)
          contDiffCdiffA[idx] += incContDiffCdiffA[idx]/incContDiffA[idx] ;
        contDiffA[idx] += incContDiffA[idx]/double(i) ;
      }
      for (idx=0 ; idx <  NoDiscEstimated ; idx++)
      {
        if (incDiscDiffA[idx] > epsilon)
          discDiffCdiffA[idx] += incDiscDiffCdiffA[idx]/incDiscDiffA[idx] ;
        discDiffA[idx] += incDiscDiffA[idx]/double(i) ;
      }
   }
   diffC /= double(NoIterations) ;
   // normalization of estimate
   if (diffC < epsilon)
     diffC = epsilon ;
   if (diffC + epsilon > 1.0)
      diffC = double(1.0) - epsilon ;
   for (k=contAttrFrom ; k < contAttrTo ; k++)
   {
      idx = k - contAttrFrom ;
      contDiffA[idx]  /= double(NoIterations) ;
      contDiffCdiffA[idx] /= double(NoIterations) ;
      NumEstimation[k] =  contDiffA[idx]*(contDiffCdiffA[idx]/diffC -
      (double(1.0) - contDiffCdiffA[idx]) / (double(1.0) - diffC) )  ;
   }
   for (k=discAttrFrom ; k < discAttrTo ; k++)
   {
      idx = k - discAttrFrom ;
      discDiffA[idx]  /= double(NoIterations) ;
      discDiffCdiffA[idx] /= double(NoIterations) ;
      DiscEstimation[k] =  discDiffA[idx]*(discDiffCdiffA[idx]/diffC -
      (double(1.0) - discDiffCdiffA[idx]) / (double(1.0) - diffC) )  ;
   }
}
*/



// ***************************************************************************
//
//                       ConceptVariation
//                       ----------------
//
//         computes measure of problem difficulty called concept variation
//              (Vilalta, R., 1999)
//
//
// ***************************************************************************
double estimationReg::ConceptVariation(int contAttrFrom, int contAttrTo,
                                    int discAttrFrom, int discAttrTo)
{

   const double alpha = 2.0 ;


   double NoUsed = contAttrTo - contAttrFrom + discAttrTo - discAttrFrom;


   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double wghtSum, wght, sigma, ConVar = 0.0 , distance, denominator ;
   int current, m ;

   marray<int> sampleIdx(NoIterations) ;
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
      current =  sampleIdx[iterIdx] ;

     wghtSum = 0.0 ;
     sigma = 0.0 ;
     for (m=0 ; m < NoIterations ; m++)
     {
        if (m==current)
           continue ;

        distance = caseDist(current, m) ;
        denominator = NoUsed - distance ;
        if (denominator > epsilon)
          wght = 1.0 / pow(2.0, alpha * distance/denominator) ;
        else
          wght = 0.0 ;

        wghtSum += wght ;

        sigma += wght * this->CAdiff(0, current, m) ;
     }

     ConVar += sigma/wghtSum ;

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
double estimationReg::CVmodified(int contAttrFrom, int contAttrTo,
                                    int discAttrFrom, int discAttrTo)
{


   double NoUsed = contAttrTo - contAttrFrom + discAttrTo - discAttrFrom;


   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double ConVar = 0.0, incConVar ;
   int current, i, iDisc, iCont, k ;
   sortRec tempSort ;
   marray<sortRec> distanceSort(TrainSize) ;

   marray<int> sampleIdx(NoIterations) ;
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       current =  sampleIdx[iterIdx] ;

       // first we compute distances of all other examples to current
      // computeDistances(current) ;

      //  sort all the examples with descending distance
      distanceSort.clear() ;
      for (i=0 ; i < TrainSize; i++)
      {
        if (i==current)  // we skip current example
          continue ;
        tempSort.key =  caseDist(current, i) ;
        tempSort.value = i ;
        distanceSort.addEnd(tempSort) ;
      }

      distanceSort.qsortAsc() ;

      for (iDisc=discAttrFrom ; iDisc < discAttrTo ; iDisc++)
      {
         incConVar = 0.0 ;
         k = 0 ;
         for (i=0 ; i < distanceSort.filled() ; i++)
            if (DAdiff(iDisc,current, distanceSort[i].value) > 0)
            {
                incConVar += CAdiff(0,current,distanceSort[i].value) ;
                k++ ;
                if (k >= kNearestEqual)
                  break ;
            }
         ConVar += incConVar / double(k) ;
      }

      for (iCont=contAttrFrom ; iCont < contAttrTo ; iCont++)
      {
         incConVar = 0.0 ;
         k = 0 ;
         for (i=0 ; i < distanceSort.filled() ; i++)
            if (CAdiff(iCont, current, distanceSort[i].value) > 0)
            {
                incConVar += CAdiff(0,current,distanceSort[i].value) ;
                k++ ;
                if (k >= kNearestEqual)
                   break ;
            }
         ConVar += incConVar / double(k) ;
       }
   }

  return ConVar/double(NoIterations)/double(NoUsed) ;

}

