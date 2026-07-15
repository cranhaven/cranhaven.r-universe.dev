/********************************************************************
*
*   Name:              modul regtree (regression  tree)
*
*   Description:  builds regression trees
*
*********************************************************************/


#include <cstdlib>
#include <cstring>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "regtree.h"
#include "utils.h"
#include "estimatorReg.h"
#include "mathutil.h"
#include "constrctReg.h"

using namespace std ;


//************************************************************
//
//                   constructRegTree
//
//       the main procedure for tree construction,
//              prepares data, calls tree builder
//
//************************************************************
int regressionTree::constructRegTree(void)
{

   noAttr = NoOriginalAttr ;
   // prepare training data
   marray<int> DTrain ;
   marray<double> pDTrain ;
   int TrainSize ;

   int i ;

   DTrain.create(NoTrainCases) ;
   pDTrain.create(NoTrainCases, 1.0) ;
   rootDTrain.create(NoTrainCases) ;
   for (i = 0 ; i < NoTrainCases ; i++)
   {
       rootDTrain[i] = DTrain[i] =  DTraining[i];
	   // DTrain[i] = DTraining[i];
   }
   rootTrainSize = TrainSize = NoTrainCases ;
   rootWeight = TrainSize ;
   rootAverage = 0.0 ;
   double Squares = 0.0 ;

   // find standard deviation of the root
   for (i=0 ; i < TrainSize ; i++)
   {
      rootAverage += NumData(DTrain[i], 0) ;
      Squares += sqr(NumData(DTrain[i], 0)) ;
   }
   rootAverage /= double(TrainSize) ;
   rootStdDev = Squares/double(TrainSize) - sqr(rootAverage) ;
   if (rootStdDev > 0.0)
      rootStdDev = double(sqrt(rootStdDev)) ;
   else
      rootStdDev = 0.0 ;

   // prepare the cache for the attributes
   int cachedNodes = int(pow(2.0, opt->constructionDepth)-0.5) ;
   if (cachedNodes > 1000 || cachedNodes < 0)
      cachedNodes = 1000 ; 
   CachedConstructs.create(opt->noCachedInNode * cachedNodes) ;

   // primary and secondary estimates
   primaryEstimate.create(noAttr+1, 0.0) ;
   secondaryEstimate.create(noAttr+1, 0.0) ;

   // call tree building algorithm
   destroy(root) ;
   root = 0 ;
   root = buildTree(DTrain,pDTrain,TrainSize, 1) ;
   if (root) {
	   switch (opt->selectedPrunerReg)   {
       case 0: break;
       case 1: mdlBottomUpPrune() ;
               break ;
       case 2: mPrune() ;
               break;
       case 3: M5prune() ;
               break;
       case 4: errorComplexityPrune() ;
               break ;
  	   default:merror("regressionTree::constructRegTree","invalid pruning method") ; 
		       break ;
       }
       return 1 ;
   }
   else
   {
      merror("Tree construction unsuccessful.","");
      return 0 ;
   }
}


//************************************************************
//
//                 buildTree
//                 ---------
//
//    builds featured tree with constructRegive induction ;
//             recursive TDIDT algorithm
//
//************************************************************
binnodeReg* regressionTree::buildTree(marray<int> &DTrain, marray<double> &pDTrain, 
                                   int TrainSize, int currentDepth)
{
   binnodeReg* Node = new binnodeReg ;
   CurrentNode = Node ;
   CurrentTrainSize = TrainSize ;
   CurrentExamples = &DTrain ;
   
   buildTreeNode(Node, DTrain, pDTrain, TrainSize) ;


   // stopping criterion
   if (time2stop(Node) )
   {
      createLeaf(Node) ;

      // free training data
      DTrain.destroy();
      pDTrain.destroy() ;
      return Node ;
   }
   else
   {
       // select/build splitting attribute/constructReg
      if (! buildConstruct(DTrain, pDTrain, TrainSize, Node, currentDepth) )
      {
            createLeaf(Node) ;
            DTrain.destroy() ;
            pDTrain.destroy() ;
            return Node ;
      }
  
      marray<int> LeftTrain, RightTrain ;
      marray<double> pLeftTrain, pRightTrain ;
      int LeftSize = 0, RightSize = 0;
      double wLeft=0.0, wRight = 0.0 ;

      // split the data according to attribute (call by reference)
      split(DTrain, pDTrain, TrainSize, Node, LeftTrain, pLeftTrain, LeftSize,
               RightTrain, pRightTrain, RightSize, wLeft, wRight) ;

      Node->weightLeft = wLeft ;
      // is the resulting split inappropriate
      if (LeftSize==0 || RightSize==0  || wLeft < opt->minNodeWeightTree || wRight < opt->minNodeWeightTree)
      {
         createLeaf(Node) ;
         DTrain.destroy() ;
         pDTrain.destroy() ;
         return Node ;
      }

      Node->Construct.splitEstimator = opt->selectionEstimatorReg ;
      //int oldEstimator = opt->selectionEstimatorReg ;
      //if (opt->selectionEstimatorReg != secondaryEstimator && correlationLimit < 1.0)
      //{
      //  // compute secondary estimationRegs
      //  estimationReg Estimator(this, DTrain, pDTrain, TrainSize) ;
      //  attributeCount bestType ;
      //  int bestIdx=Estimator.estimate(secondaryEstimator, 0, noNumeric, 1, noDiscrete, bestType) ;

      //  for (int attrIdx = 1 ; attrIdx <= noAttr; attrIdx++)
      //    if (AttrDesc[attrIdx].continuous)
      //      secondaryEstimate[attrIdx] = Estimator.NumEstimation[AttrDesc[attrIdx].tablePlace] ;
      //    else
      //      secondaryEstimate[attrIdx] =  Estimator.DiscEstimation[AttrDesc[attrIdx].tablePlace] ;
      //
      //  // if correlation is high enough switch estimators
      //  double corr = Correlation(primaryEstimate, secondaryEstimate, 1, noAttr+1) ; 
      //  if (corr > correlationLimit)
      //     opt->selectionEstimatorReg = secondaryEstimator ;
      //}
 
      // recursively call building on both partitions
      Node->left  = buildTree(LeftTrain, pLeftTrain, LeftSize, currentDepth+1) ;

      Node->right = buildTree(RightTrain, pRightTrain, RightSize, currentDepth+1) ;

      //opt->selectionEstimatorReg = oldEstimator ;

      return  Node;
   }
}


//************************************************************
//
//                        time2stop
//                        ---------
//
//            check the various stopping criteria
//
//************************************************************
booleanT regressionTree::time2stop(binnodeReg *Node)
{
   // absolute training weight (number of examples) is too small
   if (Node->weight <= opt->minNodeWeightTree)
      return mTRUE ;

   // proportion of training examples is too small
   if (Node->weight/rootWeight <= opt->relMinNodeWeight)
      return mTRUE ;

   if (sqrt(Node->MSE) <= opt->rootStdDevProportion * rootStdDev)
        return mTRUE ;

   return mFALSE ;
}


//**********************************************************************
//
//                         createLeaf
//                         ----------
//
//
//                transform node into a leaf
//
//**********************************************************************
void regressionTree::createLeaf(binnodeReg *Node)
{
   // create leaf, label it properly
   Node->Identification = leaf ;

   Node->left = Node->right = 0 ;
   Node->Construct.destroy() ;

}


//**********************************************************************
//
//                         buildTreeNode
//                         ------------
//
//
//                transform node into a leaf
//
//**********************************************************************
void regressionTree::buildTreeNode(binnodeReg *Node, marray<int> &DTrain, marray<double> &pDTrain, int TrainSize) const
{
   Node->DTrain.copy(DTrain) ;
   Node->DTrain.setFilled(TrainSize) ;
   Node->weight = 0.0 ;
   Node->left = Node->right = 0 ;

   int i, j ;

   // compute average, min and max value for class and weight of a node
   double classSum = 0.0 ;
   double fTemp, squares=0.0 ;
   Node->minClassValue = Node->maxClassValue = NumData(DTrain[0], 0) ;
   for (i=0 ; i < TrainSize ; i++)
   {
      Node->weight += pDTrain[i] ;
      fTemp =  pDTrain[i] * NumData(DTrain[i], 0) ;
      classSum += fTemp ;
      squares +=  fTemp * NumData(DTrain[i], 0) ;
      if (NumData(DTrain[i], 0) > Node->maxClassValue)
          Node->maxClassValue = NumData(DTrain[i], 0) ;
      else
         if (NumData(DTrain[i], 0) < Node->minClassValue)
             Node->minClassValue = NumData(DTrain[i], 0) ;
   }
   Node->averageClassValue = classSum / Node->weight ;
   // compute standard deviation of class values
   Node->stdDevClass =  squares / Node->weight - sqr(Node->averageClassValue) ;
   if (Node->stdDevClass > 0)
     Node->stdDevClass = sqrt(Node->stdDevClass) ;
   else
     Node->stdDevClass = 0.0 ;

   //-------------------------------------------------------------
   // compute most probable discrete values used instead of missing values
   //-------------------------------------------------------------
   Node->NAdiscValue.create(noDiscrete) ;
   marray<marray<double> > NAdiscCounter(noDiscrete) ;

   for (i=0 ; i < noDiscrete ; i++)
      NAdiscCounter[i].create(AttrDesc[DiscIdx[i]].NoValues +1, 0.0) ;

   for (i=0; i < noDiscrete ; i++)
     for (j=0 ; j < TrainSize ; j++)
        NAdiscCounter[i][DiscData(j,i)] += pDTrain[j] ;

   int max ;
   for (i=0 ; i < noDiscrete ; i++)
   {
      max = 1 ;
      for (j=2; j <= AttrDesc[DiscIdx[i]].NoValues ;  j++)
         if (NAdiscCounter[i][j] > NAdiscCounter[i][max])
            max = j ;
      Node->NAdiscValue[i] = max ;
    }

   //-------------------------------------------------------------
   //  continuous attribute missing values - use the average atribute value instead
   //   it would be better to use density estimationReg with kernel functions
   //-------------------------------------------------------------

   Node->NAnumValue.create(noNumeric) ;
   marray<double> NAcontWeight(noNumeric,0.0) ;
   marray<double> NAcontSum(noNumeric,0.0) ;

   for (i=0; i < noNumeric ; i++)
   {
     for (j=0 ; j < TrainSize ; j++)
       if (!isNAcont(NumData(j,i)))
       {
          NAcontWeight[i] += pDTrain[j] ;
          NAcontSum[i] += pDTrain[j] * NumData(j,i) ;
       }
     if (NAcontWeight[i] > 0)
       Node->NAnumValue[i] =  NAcontSum[i]/NAcontWeight[i] ;
     else
       Node->NAnumValue[i] = (maxValue[i] + minValue[i]) / 2.0 ;
   }
   
   // certain models need this variable because of NA values
   // this procedure can be called from estimation: nodes are not correctly set there
   //binnodeReg *globalCurrent = CurrentNode ;
   //CurrentNode = Node ;

   // build model for the data (used in case of a leaf and for estimationReg)
   buildModel(DTrain, pDTrain, TrainSize, Node) ;

   //CurrentNode = globalCurrent ;

   // compute mean squared and absolute error
   double residium ;
   Node->MSE = Node->MAE = 0.0 ;
   for (i=0 ; i < TrainSize ; i++)
   {
      residium = NumData(DTrain[i], 0) - Node->Model.predictSafe(Node, DTrain[i]) ;
      Node->MSE += sqr(residium) ;
      Node->MAE += fabs(residium) ;
   }
   Node->MSE /= double(TrainSize) ;
   Node->MAE /= double(TrainSize) ;

}



//**********************************************************************
//
//                         split
//                         -----
//
//
//    split the data acording to given feature into a left and
//                     a right branch
//
//**********************************************************************
void regressionTree::split(marray<int> &DTrain,
     marray<double> &pDTrain, int TrainSize, binnodeReg* Node,
     marray<int> &LeftTrain, marray<double> &pLeftTrain, int &LeftSize,
     marray<int> &RightTrain, marray<double> &pRightTrain, int &RightSize,
     double &wLeft, double &wRight)
{
   // data needed to compute probabilities of left and right branch
   // in the case of missing values
   double weightLeft = 0.0, weightOK = 0.0 ;

   double cVal ;
   char  dVal ;
   int i;
   // are there any unknown values in current node
   switch  (Node->Identification)
   {
       case continuousAttribute:
          for (i=0 ; i < TrainSize ; i++)
          {
             cVal = Node->Construct.continuousValue(DiscData,NumData,DTrain[i]) ;
             if (!isNAcont(cVal))
             {
                weightOK += pDTrain[i] ;
                if (cVal <= Node->Construct.splitValue) // || fabs(cVal - Node->Construct.splitValue) < epsilon)
                  weightLeft += pDTrain[i] ;
             }
          }
          break ;
       case discreteAttribute:
          for (i=0 ; i < TrainSize ; i++)
          {
             dVal = Node->Construct.discreteValue(DiscData,NumData,DTrain[i]) ;
             if (dVal != NAdisc)
             {
                weightOK += pDTrain[i] ;
                if (Node->Construct.leftValues[dVal])
                  weightLeft += pDTrain[i] ;
             }
          }
          break ;
       default: merror("regressionTree::split","Invalid identification of the node") ;
   }

   double probLeftNA ;
   if (weightOK > epsilon)
     probLeftNA= weightLeft / weightOK ;
   else  // invalid split: all the values are missing
     probLeftNA = 0.0 ;

   //  data for split
   marray<int> exLeft(TrainSize) ;
   marray<int> exRight(TrainSize) ;
   marray<double> probLeft(TrainSize) ;
   marray<double> probRight(TrainSize) ;
   LeftSize = RightSize = 0 ;
   wLeft = wRight = 0.0 ;
   int k ;
   // split the examples
   switch  (Node->Identification)
   {
      case continuousAttribute:
          for (k=0  ; k < TrainSize ; k++)
          {
             cVal = Node->Construct.continuousValue(DiscData,NumData,DTrain[k]) ; ;
             if (isNAcont(cVal))
             {
                // unknown value
                exLeft[LeftSize] = DTrain[k];
                probLeft[LeftSize] = pDTrain[k] * probLeftNA ;
                exRight[RightSize] = DTrain[k];
                probRight[RightSize] = pDTrain[k] - probLeft[LeftSize] ;
                if (probLeft[LeftSize] > opt->minInstanceWeight)
                {
                   wLeft += probLeft[LeftSize] ;
                   LeftSize++ ;
                }
                if (probRight[RightSize] > opt->minInstanceWeight)
                {
                   wRight += probRight[RightSize] ;
                   RightSize++ ;
                }
             }
             else
               if (cVal <= Node->Construct.splitValue) // || fabs(cVal - Node->Construct.splitValue) < epsilon)
               {
                  exLeft[LeftSize] = DTrain[k];
                  probLeft[LeftSize] = pDTrain[k] ;
                  wLeft += pDTrain[k] ;
                  LeftSize ++ ;
               }
               else
               {
                  exRight[RightSize] = DTrain[k];
                  probRight[RightSize] = pDTrain[k] ;
                  wRight += pDTrain[k] ;
                  RightSize ++ ;
               }
          }
          break ;
      case discreteAttribute:
          for (k=0  ; k < TrainSize ; k++)
          {
             dVal = Node->Construct.discreteValue(DiscData,NumData,DTrain[k]) ; ;
             if (dVal == NAdisc)
             {
                // unknown value
                exLeft[LeftSize] = DTrain[k];
                probLeft[LeftSize] = pDTrain[k] * probLeftNA ;
                exRight[RightSize] = DTrain[k];
                probRight[RightSize] = pDTrain[k] - probLeft[LeftSize] ;
                if (probLeft[LeftSize] > opt->minInstanceWeight)
                {
                   wLeft += probLeft[LeftSize] ;
                   LeftSize++ ;
                }
                if (probRight[RightSize] > opt->minInstanceWeight)
                {
                   wRight += probRight[RightSize] ;
                   RightSize++ ;
                }
             }
             else
               if (Node->Construct.leftValues[dVal])
               {
                  exLeft[LeftSize] = DTrain[k];
                  probLeft[LeftSize] = pDTrain[k] ;
                  wLeft += pDTrain[k] ;
                  LeftSize ++ ;
               }
               else
               {
                  exRight[RightSize] = DTrain[k];
                  probRight[RightSize] = pDTrain[k] ;
                  wRight += pDTrain[k] ;
                  RightSize ++ ;
               }
          }
          break ;
      case leaf:
          merror("regressionTree::split", "node type cannot be leaf") ;
          break ;
   }
   // try not to waste space ;
   LeftTrain.create(LeftSize) ;
   pLeftTrain.create(LeftSize) ;
   for (k = 0; k < LeftSize ; k++)
   {
      LeftTrain[k] = exLeft[k] ;
      pLeftTrain[k] = probLeft[k] ;
   }

   RightTrain.create(RightSize) ;
   pRightTrain.create(RightSize) ;
   for (k = 0; k < RightSize ; k++)
   {
      RightTrain[k] = exRight[k] ;
      pRightTrain[k] = probRight[k] ;
   }
}




// ************************************************************
//
//                 buildConstruct
//                 --------------
//
//    builds constructReg in a node
//
// ************************************************************
booleanT regressionTree::buildConstruct(marray<int> &DTrain, marray<double> &pDTrain, 
                                    int TrainSize, binnodeReg* Node, int currentDepth)
{
   //  will we constructReg 
   if  (currentDepth > opt->constructionDepth || opt->constructionMode == 1)
       // singleAttribute
       return singleAttributeModel(DTrain, pDTrain, TrainSize, Node) ;

      // do the construction 
      estimationReg Estimator(this, DTrain, pDTrain, TrainSize) ;
      attributeCount bestAttrType ;
   
      // estimate original attributes
      int bestAttrIdx = Estimator.estimate(opt->selectionEstimatorReg, 1,noNumeric,0,noDiscrete, bestAttrType) ;

      // copy primary estimationRegs
      for (int attrIdx = 1 ; attrIdx <= noAttr; attrIdx++)
        if (AttrDesc[attrIdx].continuous)
           primaryEstimate[attrIdx] = Estimator.NumEstimation[AttrDesc[attrIdx].tablePlace] ;
         else
           primaryEstimate[attrIdx] =  Estimator.DiscEstimation[AttrDesc[attrIdx].tablePlace] ;

      if (bestAttrIdx == -1)
        return mFALSE ;
      double bestEstimate  ;
      if (bestAttrType == aCONTINUOUS)
        bestEstimate = Estimator.NumEstimation[bestAttrIdx] ;
      else 
        bestEstimate = Estimator.DiscEstimation[bestAttrIdx] ;

      if ( (opt->selectionEstimatorReg == estRReliefFexpRank || opt->selectionEstimatorReg == estRReliefFkEqual || 
            opt->selectionEstimatorReg == estRReliefFbestK || opt->selectionEstimatorReg == estRReliefFdistance ||
            opt->selectionEstimatorReg == estRReliefFsqrDistance)  && bestEstimate < opt->minReliefEstimate) 
      {  
         return mFALSE ;  
      }
      
      // caches
      marray<constructReg> stepConjCache(opt->noCachedInNode) ;
      marray<double> stepConjCacheEst(opt->noCachedInNode) ;
      marray<constructReg> stepSumCache(opt->noCachedInNode) ;
      marray<double> stepSumCacheEst(opt->noCachedInNode) ;
      marray<constructReg> stepMplyCache(opt->noCachedInNode) ;
      marray<double> stepMplyCacheEst(opt->noCachedInNode) ;
      
      // best
      constructReg bestConjunct, bestSum, bestProduct ;
      double bestConjunctEst = -DBL_MAX, bestSumEst=-DBL_MAX, bestProductEst=-DBL_MAX ;

      if (opt->constructionMode & 0x08)
        bestProductEst = multiplicator(Estimator, bestProduct, stepMplyCache, stepMplyCacheEst) ; 
      if (opt->constructionMode & 0x04)
        bestSumEst = summand(Estimator, bestSum, stepSumCache, stepSumCacheEst) ; 
      if (opt->constructionMode & 0x02)
        bestConjunctEst = conjunct(Estimator, bestConjunct, stepConjCache, stepConjCacheEst) ;
      
      // copy this step caches into global cache

      // prepare space if neccessary
      if (CachedConstructs.filled() + opt->noCachedInNode > CachedConstructs.len())
        CachedConstructs.enlarge(Mmax(CachedConstructs.len() *2, CachedConstructs.len()+2*opt->noCachedInNode)) ;
      
      // select the best from the cached
      int i, conjIdx = 0, sumIdx = 0, mplyIdx = 0 ;
      double Est1, Est2, Est3 ;
      if (conjIdx < stepConjCache.filled())
        Est1 = stepConjCacheEst[conjIdx] ;
      else
        Est1 = -DBL_MAX ;
      if (sumIdx < stepSumCache.filled())
         Est2 = stepSumCacheEst[sumIdx] ;
      else
        Est2 = -DBL_MAX ;
      if (mplyIdx < stepMplyCache.filled())
        Est3 = stepMplyCacheEst[mplyIdx] ;
      else
        Est3 = -DBL_MAX ;

      // fill the cache
      for (i=0 ; i < opt->noCachedInNode ; i++)
      {
         
         if (Est1 >= Est2 && Est1 >= Est3 && Est1 != -DBL_MAX)
         {
           CachedConstructs.addEnd(stepConjCache[conjIdx]) ;
           conjIdx ++ ;
           if (conjIdx < stepConjCache.filled())
             Est1 = stepConjCacheEst[conjIdx] ;
           else
             Est1 = -DBL_MAX ;
         }
         else 
            if (Est2 >= Est1 && Est2 >= Est3 && Est2 != -DBL_MAX)
            {
              CachedConstructs.addEnd(stepSumCache[sumIdx]) ;
              sumIdx ++ ;
             if (sumIdx < stepSumCache.filled())
               Est2 = stepSumCacheEst[sumIdx] ;
             else
               Est2 = -DBL_MAX ;
            }
            else
               if (Est3 >= Est1 && Est3 >= Est2 && Est3 != -DBL_MAX)
            {
              CachedConstructs.addEnd(stepMplyCache[mplyIdx]) ;
              mplyIdx ++ ;
              if (mplyIdx < stepMplyCache.filled())
                Est3 = stepMplyCacheEst[mplyIdx] ;
              else
                Est3 = -DBL_MAX ;
            }
            else
               break ; // none is valid
      }

      // put the best constructReg into the node
      if ( (opt->constructionEstimatorReg == opt->selectionEstimatorReg && bestEstimate >= bestConjunctEst 
            && bestEstimate >= bestSumEst && bestEstimate >= bestProductEst) || 
            (bestConjunctEst == -DBL_MAX && bestSumEst == -DBL_MAX && bestProductEst == -DBL_MAX) )
      {
        // revert to single attribute
        makeSingleAttrNode(Node, Estimator, bestAttrIdx, bestAttrType) ;
      }
      else
      {
         if (bestConjunctEst >= bestSumEst && bestConjunctEst >= bestProductEst)
           makeConstructNode(Node, Estimator, bestConjunct) ;
         else
            if (bestSumEst >= bestConjunctEst && bestSumEst >= bestProductEst)
              makeConstructNode(Node, Estimator, bestSum) ;
            else
              if (bestProductEst >= bestConjunctEst && bestProductEst >= bestSumEst)
                makeConstructNode(Node, Estimator, bestProduct) ;
              else
              {
                 merror("regressionTree::buildConstruct", "cannot select the best constructReg") ;
                 return mFALSE ;
              }
      }
      return mTRUE ;
}


