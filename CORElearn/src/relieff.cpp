#include <float.h>

//#define PRINT_EACH_K
//#define PRINT_EACH_ITERATION

#include "general.h"
#include "error.h"
#include "contain.h"
#include "options.h"
#include "estimator.h"                

using namespace std ;



// ***************************************************************************
//
//                       ReliefF
//                       -------
//
//   contains two versions of ReliefF:
//   1. with k nearest with equal influence
//   2. with k nearest with exponentially decreasing influence
//                   
//
// ***************************************************************************
// ***************************************************************************
void estimation::ReliefF(int contAttrFrom, int contAttrTo,
                  int discAttrFrom, int discAttrTo, int distanceType){

   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimations arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
  
   // number of examples belonging to each of the classes
   marray<int> noExInClass(noClasses+1) ;
   marray<double> probClass(noClasses+1) ;
   noExInClass.init(0) ;
   probClass.init(0.0) ;
   int i, idx, iClss ;
   for (i=0 ; i < TrainSize ; i++)
   {
      noExInClass[ DiscValues(i,0) ]++ ;
      probClass[ DiscValues(i,0) ] += weight[i] ;
   }

   // obtain the greatest sensible k (number of nearest hits/misses)
   // and the total weight of examples
   int maxK = noExInClass[1] ;
   double wAll = probClass[1] ;
   for (idx=2 ; idx <= noClasses ; idx++)
   {
      if (noExInClass[idx] > maxK)
         maxK = noExInClass[idx] ;
      wAll += probClass[idx] ;
   }

   // compute estimations of class value probabilities with their 
   // relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / wAll ;

   // initialize weights for all the attributes and all the k
   marray<double> PhitDisc(NoDiscEstimated, 0.0) ;
   marray<double> PmissDisc(NoDiscEstimated, 0.0) ;
   marray<double> PhitCont(NoContEstimated, 0.0) ;
   marray<double> PmissCont(NoContEstimated, 0.0) ;
 

   // normalization of contribution of misses
   mmatrix<double> clNorm(noClasses+1,noClasses+1) ;
   for (int j=1 ; j<=noClasses ; j++)
     for (i=1 ; i<=noClasses ; i++)
        clNorm.Set(j,i, probClass[j]/(1.0-probClass[i]) ) ;

   // we have to compute distances up to the following attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass,iterIdx ;

   #if defined(PRINT_EACH_ITERATION)
     char path[MaxPath] ;
     int iPrint, contCount=0, discCount=0 ; 
     FILE *fileRelief ;
     snprintf(path, MaxPath, "%s%s.%02dei",fTree->resultsDirectory, fTree->domainName,fTree->currentSplitIdx) ; // estimation of weights at each iteration
     if ((fileRelief = fopen(path,"w"))==NULL)
     {
        merror("estimation::ReliefF cannot open file for writing weights of each iteration: ", path)  ;
     }
     else {
        fprintf(fileRelief, "\nRelief weights changing with number of iterations\n") ;
        fTree->printEstimationHead(fileRelief) ;
     }

   #endif

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;
   mmatrix<double> NumDistance, DiscDistance ;
   marray<marray<sortRec> > distanceArray, diffSorted ; // manipulation of the nearest examples
   marray<double> incContDiffA, incDiscDiffA ;
   #if !defined(_OPENMP)
	   // create data structures for a single thread
       NumDistance.create(TrainSize, numUpper) ;
	   DiscDistance.create(TrainSize, discUpper) ;
	   distanceArray.create(noClasses+1) ;
	   diffSorted.create(noClasses+1) ;
	   for (iClss = 1 ; iClss <= noClasses; iClss++)
		{
		   distanceArray[iClss].create(noExInClass[iClss]) ;
		   diffSorted[iClss].create(noExInClass[iClss]) ;
		}
	   incContDiffA.create(NoContEstimated) ;
	   incDiscDiffA.create(NoDiscEstimated) ;
   #endif

   // main ReliefF loop
   #pragma omp parallel for private(NumDistance,DiscDistance,current,currentClass,distanceSum, \
		                            normDistance, Adiff,neighbourIdx, cl, iAttr, idx, i,iterIdx,\
		                            distanceArray, diffSorted,iClss, incContDiffA, incDiscDiffA ) \
		                    shared(PhitDisc,PmissDisc,PhitCont,PmissCont,clNorm,sampleIdx,noExInClass, \
		                    		NoContEstimated,NoDiscEstimated,distanceType, \
		                    		contAttrFrom, contAttrTo, discAttrFrom, discAttrTo) \
                            default(none)
   for (iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       #if defined(_OPENMP)
	       // create data structures for each thread separately
		   NumDistance.create(TrainSize, numUpper) ;
		   DiscDistance.create(TrainSize, discUpper) ;
		   distanceArray.create(noClasses+1) ;
		   diffSorted.create(noClasses+1) ;
		   for (iClss = 1 ; iClss <= noClasses; iClss++)
			{
			   distanceArray[iClss].create(noExInClass[iClss]) ;
			   diffSorted[iClss].create(noExInClass[iClss]) ;
			}
		   incContDiffA.create(NoContEstimated) ;
		   incDiscDiffA.create(NoDiscEstimated) ;
       #endif

	   current = sampleIdx[iterIdx] ;
 
       // initialize (optimization reasons)
       
       currentClass =  DiscValues(current, 0) ;
      
        
      // first we compute distances of  all other examples to current
      computeDistances(current,DiscDistance,NumDistance) ;

      // compute distance factors
      prepareDistanceFactors(distanceType, distanceArray,diffSorted,DiscDistance,NumDistance) ;

      for (cl=1 ; cl<=noClasses ; cl++)
      {
         // compute sum of diffs
         incContDiffA.init(0.0) ;
         incDiscDiffA.init(0.0) ;
         distanceSum = 0.0 ;
         for (i=0 ; i < distanceArray[cl].filled() ; i++)
         {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;
                 
            // adjust the weights for all the estimated attributes and values
            for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
            {
               idx = iAttr - contAttrFrom ;
               Adiff = NumDistance(neighbourIdx, iAttr) ;
               incContDiffA[idx] += Adiff * normDistance ;
            }
            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
            {
               idx = iAttr - discAttrFrom ;
               Adiff = DiscDistance(neighbourIdx, iAttr) ;
               incDiscDiffA[idx] +=  Adiff * normDistance  ;
            }
         }
         if (cl == currentClass) // hit or miss
         {
            // hit
            // normalization of increments
            for (idx=0 ; idx < NoContEstimated ; idx++)
              if (incContDiffA[idx] > epsilon)
                 #pragma omp atomic
                 PhitCont[idx] += incContDiffA[idx]/distanceSum ;
            for (idx=0 ; idx < NoDiscEstimated ; idx++)
              if (incDiscDiffA[idx] > epsilon)
                #pragma omp atomic
                PhitDisc[idx] += incDiscDiffA[idx]/distanceSum ;
          }
          else
          {
             // miss
             // normalization of increments
             for (idx=0 ; idx < NoContEstimated ; idx++)
               if (incContDiffA[idx] > epsilon)
                 #pragma omp atomic
                 PmissCont[idx] += clNorm(cl, currentClass) * incContDiffA[idx]/distanceSum ;
             for (idx=0 ; idx < NoDiscEstimated ; idx++)
               if (incDiscDiffA[idx] > epsilon)
                 #pragma omp atomic
                 PmissDisc[idx] += clNorm(cl, currentClass) * incDiscDiffA[idx]/distanceSum ;
          }
      }
      #if defined(PRINT_EACH_ITERATION)
        fprintf(fileRelief, "%18d,", iterIdx+1) ;
        contCount = discCount = 0 ; 
        for (iPrint=1 ; iPrint <= fTree->noAttr; iPrint++)
        if (fTree->AttrDesc[iPrint].continuous)
        {
          fprintf(fileRelief, "%10.5f, ", (PmissCont[contCount] - PhitCont[contCount])/double(iterIdx+1)) ;
          contCount++ ;
        }
        else {
          fprintf(fileRelief, "%10.5f, ", (PmissDisc[discCount] - PhitDisc[discCount])/double(iterIdx+1)) ;
          discCount++ ;
        }
        fprintf(fileRelief, "\n") ;
      #endif
	  #if defined(_OPENMP)
		   // destroy data structures for each thread separately
		   NumDistance.destroy() ;
		   DiscDistance.destroy() ;
		   distanceArray.destroy() ;
		   diffSorted.destroy() ;
		   incContDiffA.destroy() ;
		   incDiscDiffA.destroy() ;
	   #endif
   }  
   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      NumEstimation[iAttr] = (PmissCont[idx] - PhitCont[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed numeric weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      DiscEstimation[iAttr] = (PmissDisc[idx] - PhitDisc[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed nominal weights are out of scope") ;
      #endif
   }
   #if defined(PRINT_EACH_ITERATION)
     fclose(fileRelief) ;
   #endif
 
}
/* without OpenMP
void estimation::ReliefF(int contAttrFrom, int contAttrTo,
                  int discAttrFrom, int discAttrTo, int distanceType)
{

   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimations arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;


   // number of examples belonging to each of the classes
   marray<int> noExInClass(noClasses+1) ;
   marray<double> probClass(noClasses+1) ;
   noExInClass.init(0) ;
   probClass.init(0.0) ;
   int i, idx ;
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

   // compute estimations of class value probabilities with their
   // relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / wAll ;

   // initialize weights for all the attributes and all the k
   marray<double> PhitDisc(NoDiscEstimated, 0.0) ;
   marray<double> PmissDisc(NoDiscEstimated, 0.0) ;
   marray<double> PhitCont(NoContEstimated, 0.0) ;
   marray<double> PmissCont(NoContEstimated, 0.0) ;

   // data structure to hold nearest hits/misses
   for (int iClss = 1 ; iClss <= noClasses; iClss++)
   {
      distanceArray[iClss].create(noExInClass[iClss]) ;
      diffSorted[iClss].create(noExInClass[iClss]) ;
   }

   // normalization of contribution of misses
   mmatrix<double> clNorm(noClasses+1,noClasses+1) ;
   for (int j=1 ; j<=noClasses ; j++)
     for (i=1 ; i<=noClasses ; i++)
        clNorm.Set(j,i, probClass[j]/(1.0-probClass[i]) ) ;

   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;

   marray<double> incContDiffA(NoContEstimated), incDiscDiffA(NoDiscEstimated) ;

   #if defined(PRINT_EACH_ITERATION)
     char path[MaxPath] ;
     int iPrint, contCount=0, discCount=0 ;
     FILE *fileRelief ;
     snprintf(path,MaxPath,"%s%s.%02dei",fTree->resultsDirectory, fTree->domainName,fTree->currentSplitIdx) ; // estimation of weights at each iteration
     if ((fileRelief = fopen(path,"w"))==NULL)
     {
        merror("estimation::ReliefF cannot open file for writting weights of each iteration: ", path)  ;
     }
     else {
        fprintf(fileRelief, "\nRelief weights changing with number of iterations\n") ;
        fTree->printEstimationHead(fileRelief) ;
     }

   #endif

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;

   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
       current = sampleIdx[iterIdx] ;

       // initialize (optimization reasons)

       currentClass =  DiscValues(current, 0) ;


      // first we compute distances of  all other examples to current
      computeDistances(current) ;

      // compute distance factors
      prepareDistanceFactors(distanceType) ;

      for (cl=1 ; cl<=noClasses ; cl++)
      {
         // compute sum of diffs
         incContDiffA.init(0.0) ;
         incDiscDiffA.init(0.0) ;
         distanceSum = 0.0 ;
         for (i=0 ; i < distanceArray[cl].filled() ; i++)
         {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;

            // adjust the weights for all the estimated attributes and values
            for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
            {
               idx = iAttr - contAttrFrom ;
               Adiff = NumDistance(neighbourIdx, iAttr) ;
               incContDiffA[idx] += Adiff * normDistance ;
            }
            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
            {
               idx = iAttr - discAttrFrom ;
               Adiff = DiscDistance(neighbourIdx, iAttr) ;
               incDiscDiffA[idx] +=  Adiff * normDistance  ;
            }
         }
         if (cl == currentClass) // hit or miss
         {
            // hit
            // normalization of increments
            for (idx=0 ; idx < NoContEstimated ; idx++)
              if (incContDiffA[idx] > epsilon)
                 PhitCont[idx] += incContDiffA[idx]/distanceSum ;
            for (idx=0 ; idx < NoDiscEstimated ; idx++)
              if (incDiscDiffA[idx] > epsilon)
                PhitDisc[idx] += incDiscDiffA[idx]/distanceSum ;
          }
          else
          {
             // miss
             // normalization of increments
             for (idx=0 ; idx < NoContEstimated ; idx++)
               if (incContDiffA[idx] > epsilon)
                 PmissCont[idx] += clNorm(cl, currentClass) * incContDiffA[idx]/distanceSum ;
             for (idx=0 ; idx < NoDiscEstimated ; idx++)
               if (incDiscDiffA[idx] > epsilon)
                 PmissDisc[idx] += clNorm(cl, currentClass) * incDiscDiffA[idx]/distanceSum ;
          }
      }
      #if defined(PRINT_EACH_ITERATION)
        fprintf(fileRelief, "%18d,", iterIdx+1) ;
        contCount = discCount = 0 ;
        for (iPrint=1 ; iPrint <= fTree->noAttr; iPrint++)
        if (fTree->AttrDesc[iPrint].continuous)
        {
          fprintf(fileRelief, "%10.5f, ", (PmissCont[contCount] - PhitCont[contCount])/double(iterIdx+1)) ;
          contCount++ ;
        }
        else {
          fprintf(fileRelief, "%10.5f, ", (PmissDisc[discCount] - PhitDisc[discCount])/double(iterIdx+1)) ;
          discCount++ ;
        }
        fprintf(fileRelief, "\n") ;
      #endif
   }
   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      NumEstimation[iAttr] = (PmissCont[idx] - PhitCont[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed numeric weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      DiscEstimation[iAttr] = (PmissDisc[idx] - PhitDisc[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed nominal weights are out of scope") ;
      #endif
   }
   #if defined(PRINT_EACH_ITERATION)
     fclose(fileRelief) ;
   #endif

}
*/

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
void estimation::ReliefFbestK(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo, int distanceType)
{
   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimations arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
  
   // number of examples belonging to each of the classes
   marray<int> noExInClass(noClasses+1) ;
   marray<double> probClass(noClasses+1) ;
   noExInClass.init(0) ;
   probClass.init(0.0) ;
   int i, idx ;
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

   // compute estimations of class value probabilities with their 
   // relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / wAll ;

   // initialize weights for all the attributes and all the k
   mmatrix<double> PhitDisc(maxK, NoDiscEstimated, 0.0) ;
   mmatrix<double> PmissDisc(maxK, NoDiscEstimated, 0.0) ;
   mmatrix<double> PhitCont(maxK, NoContEstimated, 0.0) ;
   mmatrix<double> PmissCont(maxK, NoContEstimated, 0.0) ;
 
   // data structure to hold nearest hits/misses
   for (int iClss = 1 ; iClss <= noClasses; iClss++)
   {
      distanceArray[iClss].create(noExInClass[iClss]) ;
      diffSorted[iClss].create(noExInClass[iClss]) ;
   }

   // normalization of contribution of misses
   mmatrix<double> clNorm(noClasses+1,noClasses+1) ;
   for (int j=1 ; j<=noClasses ; j++)
     for (i=1 ; i<=noClasses ; i++)
        clNorm.Set(j,i, probClass[j]/(1.0-probClass[i]) ) ;

   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double distanceSum, normDistance, Adiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;
   marray<double> contCorrection(NoContEstimated), discCorrection(NoDiscEstimated) ;

   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;
  
   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
   
       current = sampleIdx[iterIdx] ;
 
       // initialize (optimization reasons)
       
       currentClass =  DiscValues(current, 0) ;
     
        
      // first we compute distances of  all other examples to current
      computeDistances(current) ;

      // compute distance factors
      prepareDistanceFactors(distanceType) ;

      for (cl=1 ; cl<=noClasses ; cl++)
      {
         distanceSum = 0.0 ;
         discCorrection.init(0.0) ;
         contCorrection.init(0.0) ;

         if (cl == currentClass)
         {
            // hit
            for (i=0 ; i < distanceArray[cl].filled() ; i++)
            {
              neighbourIdx = distanceArray[cl][i].value ;
              normDistance = distanceArray[cl][i].key ;
              distanceSum += normDistance ;
                 
              // adjust the weights for all the estimated attributes and values
              for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
              {
                idx = iAttr - contAttrFrom ;
                Adiff = NumDistance(neighbourIdx, iAttr) ;
                contCorrection[idx] += Adiff * normDistance  ;
                PhitCont(i, idx) += contCorrection[idx]/distanceSum;
              }
              for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
              {
                idx = iAttr - discAttrFrom ;
                Adiff = DiscDistance(neighbourIdx, iAttr) ;
                discCorrection[idx] += Adiff * normDistance  ;
                PhitDisc(i, idx) += discCorrection[idx]/distanceSum;

              }
            }
            if (i > 0)
              while (i < maxK)
              {
                for (idx=0 ; idx < NoContEstimated ; idx ++)
                  PhitCont(i,idx) += contCorrection[idx]/distanceSum ;
                for (idx=0 ; idx < NoDiscEstimated ; idx ++)
                  PhitDisc(i,idx) += discCorrection[idx]/distanceSum ;
                i++ ;
              }

         }
         else
         {

            for (i=0 ; i < distanceArray[cl].filled() ; i++)
            {
              neighbourIdx = distanceArray[cl][i].value ;
              normDistance = distanceArray[cl][i].key ;
              distanceSum += normDistance ;
                 
              // adjust the weights for all the aestimated attributes and values
              for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
              {
                idx = iAttr - contAttrFrom ;
                Adiff = NumDistance(neighbourIdx, iAttr) ;
                contCorrection[idx] += clNorm(cl, currentClass) * Adiff * normDistance  ;
                PmissCont(i, idx) += contCorrection[idx]/distanceSum;
              }
              for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
              {
                idx = iAttr - discAttrFrom ;
                Adiff = DiscDistance(neighbourIdx, iAttr) ;
                discCorrection[idx] += clNorm(cl, currentClass) * Adiff * normDistance  ;
                PmissDisc(i, idx) += discCorrection[idx]/distanceSum;
              }
            }
            if (i >0)
               while (i < maxK)
               {
                 for (idx=0 ; idx < NoContEstimated ; idx ++)
                   PmissCont(i,idx) += contCorrection[idx]/distanceSum ;
                 for (idx=0 ; idx < NoDiscEstimated ; idx ++)
                   PmissDisc(i,idx) += discCorrection[idx]/distanceSum ;
                 i++ ;
               }

         }
      }
   }  
   
   double bestEst, est ;
   int k ;

   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      bestEst = (PmissCont(0,idx) - PhitCont(0,idx))/double(NoIterations) ;
      for (k=1 ; k < maxK ; k++)
      {
         est = (PmissCont(k,idx) - PhitCont(k,idx))/double(NoIterations) ;        
         if (est > bestEst)
             bestEst = est ;
      }
      NumEstimation[iAttr] = bestEst ;
      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed numeric weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      bestEst = (PmissDisc(0,idx) - PhitDisc(0,idx))/double(NoIterations) ;
      for (k=1 ; k < maxK ; k++)
      {
         est = (PmissDisc(k,idx) - PhitDisc(k,idx))/double(NoIterations) ;        
         if (est > bestEst)
             bestEst = est ;
      }
      DiscEstimation[iAttr] = bestEst ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed nominal weights are out of scope") ;
      #endif
   }

   
   #if defined(PRINT_EACH_K)
     char path[MaxPath] ;
     FILE *fileRelief ;
     snprintf(path, MaxPath, "%s%s.%02dek",fTree->resultsDirectory, fTree->domainName,fTree->currentSplitIdx) ; // estimation of weights for each k
     if ((fileRelief = fopen(path,"w"))==NULL)
     {
        merror("estimation::ReliefFbestK cannot open file for writting estimations for each k: ", path)  ;
     }
     fprintf(fileRelief, "\nReliefF weights changing with k nearest neighbours\n") ;
     fTree->printEstimationHead(fileRelief) ;

     int contCount,discCount; 
     for (k=0 ; k < maxK ; k++)
     {
       fprintf(fileRelief, "%18d,",k+1) ;  
       contCount = discCount = 0 ;
       for (i=1 ; i <= fTree->noAttr; i++)
       if (fTree->AttrDesc[i].continuous)
       {
          fprintf(fileRelief, "%10.5f, ", (PmissCont(k, contCount) - PhitCont(k, contCount))/double(NoIterations)) ;
          contCount++ ;
       }
       else {
         fprintf(fileRelief, "%10.5f, ", (PmissDisc(k, discCount) - PhitDisc(k, discCount))/double(NoIterations)) ;
         discCount++ ;
       }
       fprintf(fileRelief, "\n") ;
     }
     fclose(fileRelief) ;
   #endif


}


// ***************************************************************************
//
//                       Relief
//                       -------
//
//   original (Kira nad Rendell) Relief
// ***************************************************************************
void estimation::Relief(int contAttrFrom, int contAttrTo, int discAttrFrom, int discAttrTo)
{

   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimations arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
  
   // initialize weights for all the attributes and all the k
   marray<double> PhitDisc(NoDiscEstimated, 0.0) ;
   marray<double> PmissDisc(NoDiscEstimated, 0.0) ;
   marray<double> PhitCont(NoContEstimated, 0.0) ;
   marray<double> PmissCont(NoContEstimated, 0.0) ;
 
   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   int current, idx, iAttr, hit, miss ;
   
   // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;
  
   // main ReliefF loop
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
   
      current = sampleIdx[iterIdx] ;
  
	   // first we compute distances of  all other examples to current
      computeDistances(current) ;

      // compute distance factors
      findHitMiss(current, hit, miss) ;


      // adjust the weights for all the estimated attributes and values
      for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
      {
           idx = iAttr - contAttrFrom ;
           PhitCont[idx] += NumDistance(hit, iAttr) ;
           PmissCont[idx] += NumDistance(miss, iAttr) ; 
      }
      for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
      {
         idx = iAttr - discAttrFrom ;
         PhitDisc[idx] += DiscDistance(hit, iAttr) ;
         PmissDisc[idx] += DiscDistance(miss, iAttr) ; 
      
      }
   }  
   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      NumEstimation[iAttr] = (PmissCont[idx] - PhitCont[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimation::Relief", "computed numeric weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      DiscEstimation[iAttr] = (PmissDisc[idx] - PhitDisc[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed nominal weights are out of scope") ;
      #endif
   }
 
}


// ***************************************************************************
//
//                          findHitMiss
// find two nearest neighbors of current: hit and miss
//
// ***************************************************************************
void estimation::findHitMiss(int current, int &hit, int &miss)
{

   // we use only original attributes to obtain distance in attribute space

   double hitDistance = DBL_MAX, missDistance = DBL_MAX, distance ;
   
   for (int i=0 ; i < TrainSize; i++)
   {
      if (i==current)  // we skip current example
         continue ;
      
      distance = CaseDistance(i) ;
      if (DiscValues(current, 0) == DiscValues(i, 0)) // hit
      {
         if (distance < hitDistance)
         {
            hitDistance = distance ;
            hit = i ;
         }
      }
      else {  // miss
         if (distance < missDistance)
         {
            missDistance = distance ;
            miss = i ;
         }
      }
   }
}

  

// ***************************************************************************
//
//                       ReliefFmerit
//                       -------
//
//   contains two versions of ReliefF with merit:
//   1. with k nearest with equal influence
//   2. with k nearest with exponentially decreasing influence
//                   
//
// ***************************************************************************
void estimation::ReliefFmerit(int contAttrFrom, int contAttrTo,
                  int discAttrFrom, int discAttrTo, int distanceType)
{

   NumEstimation.init(contAttrFrom,contAttrTo,0.0) ;
   DiscEstimation.init(discAttrFrom,discAttrTo,0.0) ;

   // prepare estimations arrays
   int NoContEstimated = contAttrTo - contAttrFrom ;
   int NoDiscEstimated = discAttrTo - discAttrFrom ;
  

   // number of examples belonging to each of the classes
   marray<int> noExInClass(noClasses+1) ;
   marray<double> probClass(noClasses+1) ;
   noExInClass.init(0) ;
   probClass.init(0.0) ;
   int i, idx ;
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

   // compute estimations of class value probabilities with their 
   // relative frequencies
   for (idx=1 ; idx <= noClasses ; idx++)
      probClass[idx] = probClass[idx] / wAll ;

   // initialize weights for all the attributes and all the k
   marray<double> PhitDisc(NoDiscEstimated, 0.0) ;
   marray<double> PmissDisc(NoDiscEstimated, 0.0) ;
   marray<double> PhitCont(NoContEstimated, 0.0) ;
   marray<double> PmissCont(NoContEstimated, 0.0) ;
 
   // data structure to hold nearest hits/misses
   for (int iClss = 1 ; iClss <= noClasses; iClss++)
   {
      distanceArray[iClss].create(noExInClass[iClss]) ;
      diffSorted[iClss].create(noExInClass[iClss]) ;
   }

   // normalization of contribution of misses
   mmatrix<double> clNorm(noClasses+1,noClasses+1) ;
   for (int j=1 ; j<=noClasses ; j++)
     for (i=1 ; i<=noClasses ; i++)
        clNorm.Set(j,i, probClass[j]/(1.0-probClass[i]) ) ;

   // we have to compute distances up to the folowing attributes
   discUpper = Mmax(noDiscrete, discAttrTo) ;
   numUpper = Mmax(noNumeric, contAttrTo) ;

   double distanceSum, normDistance, Adiff, sumAdiff ;
   int current, neighbourIdx, cl, iAttr, currentClass ;
   
   marray<double> incContDiffA(NoContEstimated), incDiscDiffA(NoDiscEstimated) ;
       
    // prepare order of iterations
   marray<int> sampleIdx(NoIterations);
   randomizedSample(sampleIdx, NoIterations, TrainSize) ;
  
   for (int iterIdx=0 ; iterIdx < NoIterations ; iterIdx++)
   {
   
       current = sampleIdx[iterIdx] ;

       // initialize (optimization reasons)
       
       currentClass =  DiscValues(current, 0) ;
      
        
      // first we compute distances of  all other examples to current
      computeDistances(current) ;

      // compute distance factors
      prepareDistanceFactors(distanceType) ;

      for (cl=1 ; cl<=noClasses ; cl++)
      {
         // compute sum of diffs
         incContDiffA.init(0.0) ;
         incDiscDiffA.init(0.0) ;
         distanceSum = 0.0 ;
         for (i=0 ; i < distanceArray[cl].filled() ; i++)
         {
            neighbourIdx = distanceArray[cl][i].value ;
            normDistance = distanceArray[cl][i].key ;
            distanceSum += normDistance ;
                 
            sumAdiff = 0.0 ;
            for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
               sumAdiff += NumDistance(neighbourIdx, iAttr) ;
            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
               sumAdiff += DiscDistance(neighbourIdx, iAttr) ;

            // adjust the weights for all the estimated attributes and values
            for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
            {
               idx = iAttr - contAttrFrom ;
               Adiff = NumDistance(neighbourIdx, iAttr) ;
               incContDiffA[idx] += Adiff/sumAdiff * normDistance ;
            }
            for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
            {
               idx = iAttr - discAttrFrom ;
               Adiff = DiscDistance(neighbourIdx, iAttr) ;
               incDiscDiffA[idx] +=  Adiff/sumAdiff * normDistance  ;
            }

         }
         if (cl == currentClass) // hit or miss
         {
            // hit
            // normalization of increments
            for (idx=0 ; idx < NoContEstimated ; idx++)
              if (incContDiffA[idx] > epsilon)
                PhitCont[idx] += incContDiffA[idx]/distanceSum ;
            for (idx=0 ; idx < NoDiscEstimated ; idx++)
              if (incDiscDiffA[idx] > epsilon)
                PhitDisc[idx] += incDiscDiffA[idx]/distanceSum ;
          }
          else
          {
             // miss
             // normalization of increments
             for (idx=0 ; idx < NoContEstimated ; idx++)
               if (incContDiffA[idx] > epsilon)
                 PmissCont[idx] += clNorm(cl, currentClass) * incContDiffA[idx]/distanceSum ;
             for (idx=0 ; idx < NoDiscEstimated ; idx++)
               if (incDiscDiffA[idx] > epsilon)
                 PmissDisc[idx] += clNorm(cl, currentClass) * incDiscDiffA[idx]/distanceSum ;
          }
      }
     }  
   for (iAttr=contAttrFrom ; iAttr < contAttrTo ; iAttr ++)
   {
      idx = iAttr - contAttrFrom ;
      NumEstimation[iAttr] = (PmissCont[idx] - PhitCont[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (NumEstimation[iAttr] > 1.00001 || NumEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed numeric weights are out of scope") ;
      #endif
   }
   for (iAttr=discAttrFrom ; iAttr < discAttrTo ; iAttr ++)
   {
      idx = iAttr - discAttrFrom ;
      DiscEstimation[iAttr] = (PmissDisc[idx] - PhitDisc[idx])/double(NoIterations) ;
      #if defined(DEBUG)
      if (DiscEstimation[iAttr] > 1.00001 || DiscEstimation[iAttr] < -1.00001)
        merror("estimation::ReliefF", "computed nominal weights are out of scope") ;
      #endif
   }
 }

