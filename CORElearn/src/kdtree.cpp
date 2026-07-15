#include <climits>
#include <cfloat>

#include "general.h"
#include "error.h"
#include "dataStore.h"
#include "utils.h"
#include "kdtree.h"

using namespace std ;

void kdNode::copy(kdNode &Source)
{
    nodeId = Source.nodeId ;
    attrIdx = Source.attrIdx ;
    contPivot = Source.contPivot ;
    discPivot = Source.discPivot ;
    range = Source.range ;
    Lower = Source.Lower ;
    Upper = Source.Upper ;
    dataSize = Source.dataSize ;
    DataPoint = Source.DataPoint ;
    // pointers to left and right should be handled seperately

}

kdTree::kdTree() 
{ 
    root = 0 ; 
    DiscVal = 0 ;
    ContVal = 0 ;
    noDiscValues = 0 ;
    minValue = maxValue = valueInterval = step = 0 ;
    NAdiscValue = NAnumValue = 0 ;
    bucketSize = 10 ;
    contFrom = contTo = discFrom = discTo = 0 ;
    noNumeric = noDiscrete = kNear = qPoint = -1 ;
}

kdTree::~kdTree() 
{ 
    destroy(); 
    DiscVal = 0 ;
    ContVal = 0 ;
} 


kdTree::kdTree(kdTree &Copy)
{
   root = 0 ;
   DiscVal = 0 ;
   ContVal = 0 ;
   copy(Copy) ;
}

//************************************************************
//
//                       destroy
//                       -------
//
//          recursivly destroys entire  tree
//
//************************************************************
void kdTree::destroy(kdNode *node)
{
   if (node)
   {
      destroy(node->left);
      destroy(node->right);

      delete node ;
   }
}



// **********************************************************************
//
//                      copy
//                      --------
//
//      copies source tree
//
// **********************************************************************
void kdTree::copy(kdTree &Source)
{
   if (&Source != this)
   {
     destroy() ;
     ContVal = Source.ContVal ;  // !! only pointers are copied
     DiscVal = Source.DiscVal ;
     noDiscValues = Source.noDiscValues ;
     minValue = Source.minValue ;
     maxValue = Source.maxValue ;
     valueInterval = Source.valueInterval ;
     step = Source.step ;
     NAdiscValue = Source.NAdiscValue ;
     NAnumValue = Source.NAnumValue ;
     bucketSize = Source.bucketSize ;
     contFrom = Source.contFrom ;
     contTo = Source.contTo ;
     discFrom = Source.discFrom ;
     discTo = Source.discTo ;
     noNumeric = Source.noNumeric ;
     noDiscrete = Source.noDiscrete ;
     kNear = Source.kNear ;
     qPoint = Source.qPoint ;
     #if defined(RAMP_FUNCTION)
       DifferentDistance = Source.DifferentDistance ;
       EqualDistance = Source.EqualDistance ;
       CAslope = Source.CAslope ;
     #endif
     
     PQnear = Source.PQnear ;

     if (Source.root)
       dup(Source.root, root) ;
     else
       root = 0 ;
   }
}
   
   


// **********************************************************************
//
//                      operator =
//                      --------
//
//      copies source tree
//
// **********************************************************************
kdTree& kdTree::operator=(kdTree &Source)
{
   copy (Source) ;
   return *this ;
}


//**********************************************************************
//
//                      dup
//                      -------
//
//      duplicates source booleanT exprRegession into target
//
//**********************************************************************
void kdTree::dup(kdNode *Source, kdNode* &Target)
{
    Target = new kdNode ;
    Target->copy(*Source) ;

    if (Source->left)
      dup(Source->left, Target->left) ;
    else
      Target->left = 0 ;
    if (Source->right)
      dup(Source->right, Target->right ) ;
    else
      Target->right = 0 ;
}




//**********************************************************************
//
//                      insertAll
//                      ---------
//
//      inserts all the examples into k-d tree
//
//**********************************************************************
void kdTree::insertAll(marray<int> &DTrain, int TrainSize, mmatrix<double> *NumValues, mmatrix<int> *DiscValues,
                        marray<int> *noDscValues, 
                        marray<double> *minValues, marray<double> *maxValues,  marray<double> *valueIntervals, 
                        marray<double> *steps, 
                        marray<marray<double> > *NAdiscValues, marray<marray<double> > *NAcontValues,
                      #if defined(RAMP_FUNCTION)
                        marray<double> *DifferentDistances, marray<double> *EqualDistances, marray<double> *CAslopes,
                       #endif
                       int cntFrom, int cntTo, int dscFrom, int dscTo)  
{
    ContVal = NumValues ;
    DiscVal = DiscValues ;
    contFrom = cntFrom ;
    contTo = cntTo ;
    discFrom = dscFrom ;
    discTo = dscTo ;
    noDiscValues = noDscValues ;
    minValue = minValues ;
    maxValue = maxValues ;
    valueInterval = valueIntervals ;
    step = steps ;
    NAdiscValue = NAdiscValues ;
    NAnumValue = NAcontValues ;
    #if defined(RAMP_FUNCTION)
      DifferentDistance = DifferentDistances ;
      EqualDistance = EqualDistances ;
      CAslope = CAslopes ;
    #endif
    noNumeric = contTo - contFrom ;
    noDiscrete = discTo - discFrom ;

    marray<double> Low(noNumeric), Up(noNumeric) ;
    int i ;
    for (i=0 ; i < noNumeric ; i++)
    {
        Low[i] = (*minValue)[i+contFrom] ;
        Up[i] = (*maxValue)[i+contFrom] ;
    }
    marray<marray<booleanT> > range(noDiscrete) ;
    for (i=0 ; i < noDiscrete ; i++)
       range[i].create((*noDiscValues)[i+discFrom]+1, mTRUE) ;

    destroy(root) ;
    root = 0 ;
    root = build(DTrain, TrainSize, Low, Up, range) ;
}

//**********************************************************************
//
//                      build
//                      -------
//
//      builds the k-d treee from the data
//
//**********************************************************************
kdNode* kdTree::build(marray<int> &DataPts, int TrainSize, marray<double> &Low, marray<double> &Up, marray<marray<booleanT> > &discContain)
{
    kdNode* Node = new kdNode ;
    Node->dataSize = TrainSize ;
    Node->Lower = Low ;
    Node->Upper = Up ;
    Node->range = discContain ;

    if (TrainSize <= bucketSize)  // create leaf (bucket of data points)
    {
makeLeaf:
        Node->nodeId = leaf ;
        Node->DataPoint = DataPts ;
        Node->left = Node->right = 0 ;
        return Node ;
    }    
    // select the splitting dimension - the dimension with the greatest scattering of values is chosen
    // for continuous attributes scatter measure is variance
    int i, iAttr, maxContIdx = -1 ; ;
    double valueSum, squares, fTemp;
    double maxVariance = -1.0, attrVariance ;
    double avgVariance = 0.0 ;
    if (noNumeric > 0)
    {
        for (iAttr=contFrom ; iAttr < contTo; iAttr++)
        {
           valueSum = squares = attrVariance = 0.0 ;
           for (i=0 ; i < TrainSize ; i++)
           {
              // we normalize values to [0,1]
              fTemp =  (*ContVal)(DataPts[i], iAttr) ;
              if (!isNAcont(fTemp))
              {
                 fTemp = (fTemp - (*minValue)[iAttr] ) / (*valueInterval)[iAttr] ;
                 valueSum += fTemp ;
                 squares +=  sqr(fTemp) ;
               }
           }
 
           //   attrVariance = squares / double(valid) - sqr(valueSum/double(valid)) ;
           // upper line would be the correct variance, but since we want to decrease the 
           // possibility of choosing the attribute with lots of missing values we divide
           // by the whole number of examples
           attrVariance = squares / double(TrainSize) - sqr(valueSum/double(TrainSize)) ;
    
           if (attrVariance > maxVariance)
           {
               maxVariance  = attrVariance ;
               maxContIdx =iAttr ;
           }
           avgVariance += attrVariance ;
        }
        avgVariance /= double(noNumeric) ;
    }
    // for discrete attribute scatter measure is gini index
    marray<int> valueWeight ;
    int noValues, maxDiscIdx = -1 ;
    double gini, maxGini = -1.0, avgGini = 0.0, denominator ;
    if (noDiscrete >0)
    {
       for (iAttr=discFrom ; iAttr < discTo ; iAttr++)
       {
          noValues = (*noDiscValues)[iAttr] ;
          valueWeight.create(noValues+1, 0) ;
          for (i=0 ; i < TrainSize ; i++)
             valueWeight[(*DiscVal)(DataPts[i], iAttr)]++ ;
          // as for continuous attributes: to punish missing values we add them to the sum of squares
          gini = sqr(double(valueWeight[0])/double(TrainSize)) ; ;
          denominator = TrainSize - valueWeight[0] ;
          if (denominator >0)
            for (i=1 ; i <= noValues ; i++)
               gini += sqr(double(valueWeight[i])/double(denominator)) ;
          gini = 1.0 - gini ;
          if (gini > maxGini)
          {
             maxGini = gini  ;
             maxDiscIdx = iAttr ;
          }
          avgGini += gini ;
       }
       avgGini /= double(noDiscrete) ;
    }
    // now we have to somehow compare variance and gini index
    // we just select multiplicative factor
    // and assume that average variance and average gini have the same scattering
    if (avgVariance > 0.0 && avgGini > 0.0)
    {
        double factor = avgVariance / avgGini ;
        maxGini *= factor ;
    }
    if (maxGini > maxVariance)
    {
       Node->nodeId = discreteAttribute ;
       Node->attrIdx = maxDiscIdx ;
    }
    else {
       Node->nodeId = continuousAttribute ;
       Node->attrIdx = maxContIdx ;
    }

    int leftSize, rightSize ;
    marray<int> leftData, rightData ;
    marray<int> invData(TrainSize) ;
    marray<double> leftLower(Low), rightLower(Low), leftUpper(Up), rightUpper(Up) ;
    marray<marray<booleanT> > leftRange(discContain), rightRange(discContain) ;
    // now we select the splitting point
    if (Node->nodeId == continuousAttribute)
    {
       marray<sortRec> splitTable(TrainSize) ;
       for (i=0 ; i < TrainSize ; i++)
       {
           fTemp = (*ContVal)(DataPts[i], Node->attrIdx) ;
           if (isNAcont(fTemp))
             fTemp = Node->Lower[Node->attrIdx - contFrom] ;
           splitTable[i].key = fTemp ;
           splitTable[i].value = DataPts[i] ;
       }
       splitTable.setFilled(TrainSize) ;
       leftSize = TrainSize /2 ;
       splitTable.select(leftSize) ;
       Node->contPivot = splitTable[leftSize].key ;
       // if there are values equal to the pivot we have to put all of them in one side
       leftData.create(TrainSize) ;
       rightData.create(TrainSize);
       leftSize = rightSize = 0 ;
       for (i=0 ; i < TrainSize ; i++)
       {
           fTemp = (*ContVal)(DataPts[i], Node->attrIdx) ;
           if (isNAcont(fTemp) || fTemp < Node->contPivot)
             leftData[leftSize++] = DataPts[i] ;
           else
              rightData[rightSize++] = DataPts[i] ;
       }
       // invalid split
       if (leftSize == 0 || leftSize == TrainSize)
          goto makeLeaf ;

       // lower and upper values
       leftUpper[Node->attrIdx-contFrom] = splitTable[leftSize].key ;
       rightLower[Node->attrIdx-contFrom] = splitTable[leftSize].key ;
      
    }
    else {
        // we try to find as balanced partition of discrete attribute as possible
       noValues = (*noDiscValues)[Node->attrIdx] ;
       valueWeight.create(noValues+1,0) ;
       for (i=0 ; i < TrainSize ; i++)
          valueWeight[(*DiscVal)(DataPts[i], Node->attrIdx)]++ ;
       int desired = TrainSize/2  ;
       // try to find balanced partition
       // valueWeight will be destroyed
       leftSize =  balancedPartition(valueWeight, noValues, desired, Node->discPivot) ;

       // invalid split
       if (leftSize == 0 || leftSize == TrainSize)
          goto makeLeaf ;

       // now split the values
       
       rightSize = TrainSize - leftSize ;
       leftData.create(leftSize) ;
       rightData.create(rightSize) ;
       int iLeft=0, iRight=0 ;
       for (i=0 ; i < TrainSize; i++)
       {
          if (Node->discPivot[(*DiscVal)(DataPts[i], Node->attrIdx)] == mTRUE)
             leftData[iLeft++] = DataPts[i] ;
          else
             rightData[iRight++] = DataPts[i] ;
       }
       leftRange[Node->attrIdx- discFrom] = Node->discPivot ;
       for (i=0 ; i <= noValues ; i++)
          if (Node->discPivot[i])
             rightRange[Node->attrIdx- discFrom][i] = mFALSE ;
    }
    // now build the subtrees
    Node->left =  build(leftData, leftSize, leftLower, leftUpper, leftRange) ;
    Node->right = build(rightData, rightSize, rightLower, rightUpper, rightRange) ;
    return Node ;
 }
    


//**********************************************************************
//
//                      balancedPartition
//                      -----------------
//
//      greedily creates balanced partition of the integer list
//
//**********************************************************************
int kdTree::balancedPartition(marray<int> &values, int noValues, int desired, marray<booleanT> &splitIdx) 
{
   splitIdx.create(noValues+1, mFALSE) ;
   
   int i, diff, minIdx ;
   int iniDesired = desired ; 
   do {
      // greedy selection of the value which minimizes the difference to desired value
      diff = INT_MAX ;
      minIdx = -1 ;
      for (i=0 ; i <= noValues ; i++)
        if (abs(desired - values[i]) < diff)
        {
           diff = abs(desired - values[i]) ;
           minIdx = i ;
        }
      if (diff < desired)
      {
         desired = desired - values[minIdx] ;
         splitIdx[minIdx] = mTRUE ;
         values[minIdx] = INT_MAX ;
      }
      else 
         break ;
   } while (desired > 0) ;
   return iniDesired - desired ;
}


//**********************************************************************
//
//                      findK
//                      -----
//
//      finds k nearest neighbours to the querry point
//
//**********************************************************************
void kdTree::findK(int queryPoint, int k)
{

   kNear = k ;
   qPoint = queryPoint ;
   if (PQnear.len() < kNear)
      PQnear.create(kNear) ;
   PQnear.clear() ;
   

   findK(root) ;

   // PQnear.sort(ascSortComp) ;

}


//**********************************************************************
//
//                      findK
//                      -----
//
//      recursively finds k nearest neighbours to the querry point
//
//**********************************************************************
booleanT kdTree::findK(kdNode* Node)
{
   
   // in leaf we add cases in the bucket to the PQ
   if (Node->nodeId == leaf)
   {
      addPQ(Node) ;
      return BallWithinBounds(Node) ;
   }
   
   // initial case when we do not have full PQ yet
   if (PQnear.filled() <= kNear && Node->dataSize <= kNear)
   {
      fillPQ(Node) ;
      return BallWithinBounds(Node) ;
   }

   booleanT firstLeft ;
   // in interior node we have to decide which path to take
   // first we do recursive call on closer son
   if (Node->nodeId == continuousAttribute)
   {
      double fTemp = (*ContVal)(qPoint, Node->attrIdx) ;
      if (isNAcont(fTemp) || fTemp < Node->contPivot)
      {
         if (findK(Node->left))
            return mTRUE ;
         firstLeft = mTRUE ;
      }
      else {
         if (findK(Node->right))
            return mTRUE ;
         firstLeft = mFALSE ;
      }
   }
   else {
     #if defined(DEBUG)
        if (Node->nodeId != discreteAttribute)
            merror("kdTree::findK", "invalid node type") ;
     #endif
     if  ( Node->discPivot[(*DiscVal)(qPoint, Node->attrIdx)] )
     {
         if (findK(Node->left))
            return mTRUE ;
         firstLeft = mTRUE ;
     }
      else {
         if (findK(Node->right))
            return mTRUE ;
         firstLeft = mFALSE ;
      }
   }

   // if we are here it means that we have returned from lower levels
   //  and call recursively farther son, if neccessary
   if (firstLeft)
   {
      if (BoundsOverlapBall(Node->right))
         findK(Node->right) ;
   }
   else {
      if (BoundsOverlapBall(Node->left))
         findK(Node->left) ;
   }

   // see if we should return or terminate
   return BallWithinBounds(Node) ;
 }


//**********************************************************************
//
//                      BallWithinBounds
//                      ----------------
//
//      checks if the hyper-ball spanning around the nearest is within 
//       the bounds of the current space
//
//**********************************************************************
booleanT kdTree::BallWithinBounds(kdNode *Node)
{
   // first test if we already have enough examples otherwise we cannot stop anyway
   if (PQnear.filled() < kNear)
      return mFALSE ;

   // now test for all the dimensions
   // if the radius of the ball is smaller than coordinate distance
   int iAttr, iTemp ;
   // any single distance cannot be grater than 1.0, so if radius is larger than 1.0
   // we can be sure that ball is not within bounds
   if (PQnear[0].key >= 1.0)
      return mFALSE ;

   for (iAttr = discFrom ; iAttr < discTo ; iAttr++)
   {
      //  coordinate distance is compared to radius
      iTemp = (*DiscVal)(qPoint, iAttr) ;
      if (! Node->range[iAttr-discFrom][iTemp]) // coordinate distance is 1.0, and radius is less
         return mFALSE ;
   }

   double fTemp ;
   for (iAttr = contFrom ; iAttr < contTo ; iAttr++)
   {
      //  coordinate distance is compared to radius
      fTemp = (*ContVal)(qPoint, iAttr) ;
      if (CAdiffV(iAttr, fTemp,  Node->Lower[iAttr-contFrom]) <= PQnear[0].key || 
          CAdiffV(iAttr, fTemp, Node->Upper[iAttr-contFrom]) <= PQnear[0].key )
            return  mFALSE ;
   }
   return mTRUE ;
}
         

//**********************************************************************
//
//                      BoundsOverlapBall
//                      -----------------
//
//      checks if the bounds of the current space overlap
//         hyper-ball spanning around the nearest 
//
//**********************************************************************
booleanT kdTree::BoundsOverlapBall(kdNode *Node)
{
   if (PQnear.filled() < kNear)
      return mTRUE ;

   double sumDist =0.0 ;
   int iAttr, iTemp ;
   for (iAttr = discFrom ; iAttr < discTo ; iAttr++)
   {
      //  coordinate distance is compared to radius
      iTemp = (*DiscVal)(qPoint, iAttr) ;
      if (iTemp != NAdisc &&  !Node->range[iAttr-discFrom][iTemp])
      {
         sumDist += 1.0 ;
         if (sumDist > PQnear[0].key)
            return  mFALSE ;
      }
   }
   double fTemp ;
   for (iAttr = contFrom ; iAttr < contTo ; iAttr++)
   {
      //  coordinate distance is compared to radius
      fTemp = (*ContVal)(qPoint, iAttr) ;
      if (!isNAcont(fTemp))
      {
         if (fTemp < Node->Lower[iAttr-contFrom])  
         {
            // lower than low boundary
            sumDist += CAdiffV(iAttr, fTemp,  Node->Lower[iAttr-contFrom]) ;
            if (sumDist > PQnear[0].key)
               return  mFALSE ;
         }
         else
           if (fTemp > Node->Upper[iAttr-contFrom])  
           {
             // higher than high boundary
             sumDist += CAdiffV(iAttr, fTemp,  Node->Upper[iAttr-contFrom]) ;
             if (sumDist > PQnear[0].key)
               return  mFALSE ;
           }
      }
   }
   return mTRUE ;
}



//**********************************************************************
//
//                      addPQ
//                      -----
//
//      adds elements of the node to the priority queue of the best
//
//**********************************************************************
void kdTree::addPQ(kdNode *Node)
{
   #if defined(DEBUG)
     if (Node->nodeId != leaf)
       merror("kdTree:addPQ", "invalid node type") ;
   #endif

   sortRec newCase, temp ;

   for (int i=0 ; i < Node->dataSize ; i++)
   {
      newCase.value = Node->DataPoint[i] ;
      newCase.key = caseDist(qPoint, Node->DataPoint[i]) ;

      if (PQnear.filled() < kNear )
         PQnear.addPQmax(newCase) ;
      else {
         if (newCase.key < PQnear[0].key)
         { 
            PQnear.deleteMaxPQmax(temp) ;
            PQnear.addPQmax(newCase) ;
         }
      }
   }
}


//**********************************************************************
//
//                      fillPQ
//                      -------
//
//      adds elements of the node and its subtrees to the priority queue 
//
//**********************************************************************
void kdTree::fillPQ(kdNode *Node)
{
   if (Node->nodeId == leaf)
      addPQ(Node) ;
   else
   {
      fillPQ(Node->left) ;
      fillPQ(Node->right) ;
   }
}


#if defined(MANHATTAN) 
//************************************************************
//
//                           caseDist
//                           --------
//
//                     computes distance among two instances
//
//************************************************************
double kdTree::caseDist(int I1, int I2) 
{
   double Distance = 0.0;

   int i ;
   for (i=discFrom ; i < discTo ; i++)
      Distance += DAdiff(i,I1, I2) ;

   for (i=contFrom; i<contTo; i++)
      Distance += CAdiff(i, I1, I2) ;

   return  Distance ;
}
#endif

#if defined(EUCLID) || defined(MAHALANOBIS)
//************************************************************
//
//                           caseDist
//                           --------
//
//                     computes distance among two instances
//
//************************************************************
double kdTree::caseDist(int I1, int I2) 
{
   double Distance = 0.0;

   int i ;
   for (i=discFrom ; i < discTo ; i++)
      Distance += sqr(DAdiff(i,I1, I2)) ;

   for (i=contFrom; i<contTo; i++)
      Distance += sqr(CAdiff(i, I1, I2)) ;

   return  sqrt(Distance) ;
}
#endif


// ***************************************************************************
//
//                   CAdiff
//              diff function for continuous attribute (instances)
//
// ***************************************************************************
double kdTree::CAdiff(int AttrIdx, int I1, int I2)
{
   double cV1 = (*ContVal)(I1, AttrIdx) ;
   double cV2 = (*ContVal)(I2, AttrIdx) ;
   if (isNAcont(cV1))
      return NAnumDiff(AttrIdx,cV2) ;
    else
      if (isNAcont(cV2))
        return NAnumDiff(AttrIdx,cV1) ;
       else
         #if defined(RAMP_FUNCTION)
           return CARamp(AttrIdx, fabs(cV2 - cV1) ) ;
        #else
           return  fabs(cV2 - cV1) / (*valueInterval)[AttrIdx] ;
        #endif
}



// ***************************************************************************
//
//                   CAdiffV
//              diff function for continuous attribute (values)
//
// ***************************************************************************
double kdTree::CAdiffV(int AttrIdx, double cV1, double cV2)
{
    if (isNAcont(cV1))
      return NAnumDiff(AttrIdx,cV2) ;
    else
      if (isNAcont(cV2))
        return NAnumDiff(AttrIdx,cV1) ;
       else
         #if defined(RAMP_FUNCTION)
           return CARamp(AttrIdx, fabs(cV2 - cV1) ) ;
        #else
           return  fabs(cV2 - cV1) / (*valueInterval)[AttrIdx] ;
        #endif
}



// ***************************************************************************
//
//                   DAdiff
//              diff function of discrete attribute (instances)
//
// ***************************************************************************
double kdTree::DAdiff(int AttrIdx, int I1, int I2)
{

  // we assume that missing value has value 0
  int dV1 = (*DiscVal)(I1, AttrIdx) ;
  int dV2 = (*DiscVal)(I2, AttrIdx) ;
  if (dV1 == NAdisc)
     return (*NAdiscValue)[AttrIdx][int(dV2)] ;
  else
    if (dV2 == NAdisc)
      return (*NAdiscValue)[AttrIdx][int(dV1)] ;
     else
       if (dV1 == dV2)
         return  0.0 ;
       else
         return 1.0 ;
}

// ***************************************************************************
//
//                   DAdiffV
//              diff function of discrete attribute (values)
//
// ***************************************************************************
double kdTree::DAdiffV(int AttrIdx, int dV1, int dV2)
{

  // we assume that missing value has value 0
  if (dV1 == NAdisc)
     return (*NAdiscValue)[AttrIdx][int(dV2)] ;
  else
    if (dV2 == NAdisc)
      return (*NAdiscValue)[AttrIdx][int(dV1)] ;
     else
       if (dV1 == dV2)
         return  0.0 ;
       else
         return 1.0 ;
}

// ***************************************************************************
//
//                    CARamp
//          ramp function of continuous attribute (or class)
//
// ***************************************************************************
#if defined(RAMP_FUNCTION)
inline double kdTree::CARamp(int AttrIdx, double distance)
{
  if (distance >= (*DifferentDistance)[AttrIdx])
     return 1.0 ;
  if (distance <= (*EqualDistance)[AttrIdx])
     return 0.0 ;

  return  (distance - (*EqualDistance)[AttrIdx]) * (*CAslope)[AttrIdx] ;
}
#endif


// ***************************************************************************
//
//                   NAnumDiff
//         diff function for missing values at continuous attribute
//
// ***************************************************************************
double kdTree::NAnumDiff(int AttrIdx, double Value)
{
   if (isNAcont(Value))
      return (*NAnumValue)[AttrIdx][0] ;

   return (*NAnumValue)[AttrIdx][int((Value - (*minValue)[AttrIdx]) / (*step)[AttrIdx]) +1] ;
}


