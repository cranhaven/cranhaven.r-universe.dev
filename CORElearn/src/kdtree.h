#if !defined(KDTREE_H)
#define KDTREE_H


#include "binnodeReg.h"



class kdNode
{
public:
     nodeType nodeId ;
     int attrIdx ;
     double contPivot ;
     marray<booleanT> discPivot ;
     marray<marray<booleanT> > range ;
     marray<double> Lower, Upper ;
     int dataSize ;
     marray<int> DataPoint ;
     kdNode *left, *right; 
        
     kdNode() { nodeId = leaf ; attrIdx = dataSize = -1 ; contPivot = -DBL_MAX ; left = right = 0 ; }

     kdNode& operator= (kdNode &Source) { copy(Source); return *this ; } 
     void copy(kdNode &Source) ;
};



class kdTree
{
   kdNode *root ;
   mmatrix<double> *ContVal;
   mmatrix<int> *DiscVal ;
   marray<int> *noDiscValues ;
   marray<double> *minValue, *maxValue, *valueInterval;
   marray<double>  *step ;
   marray<marray<double> > *NAdiscValue, *NAnumValue ;
   int bucketSize ;
   int contFrom, contTo, discFrom, discTo ;
   int noNumeric, noDiscrete ;
   int kNear, qPoint ;


   #if defined(RAMP_FUNCTION)
    marray<double> *DifferentDistance, *EqualDistance, *CAslope ;
    inline double CARamp(int AttrIdx, double distance) ;
   #endif

   
   void dup(kdNode *Source, kdNode* &Target) ;
   int balancedPartition(marray<int> &values, int noValues, int desired, marray<booleanT> &splitIdx) ;
   kdNode* build(marray<int> &DataPts, int TrainSize, marray<double> &Low, marray<double> &Up, marray<marray<booleanT> > &discContain) ;
   booleanT findK(kdNode* Node) ;
   void addPQ(kdNode *Node) ;
   void fillPQ(kdNode *Node) ;
   double CAdiff(int AttrIdx, int I1, int I2)  ;
   double CAdiffV(int AttrIdx, double cV1, double cV2) ;
   double DAdiff(int AttrIdx, int I1, int I2) ;
   double DAdiffV(int AttrIdx, int dV1, int dV2) ;
   double NAnumDiff(int AttrIdx, double Value) ;
   booleanT BallWithinBounds(kdNode *Node) ;
   booleanT BoundsOverlapBall(kdNode *Node) ;

  
public:
   marray<sortRec> PQnear ;
   
   kdTree() ;
   kdTree(kdTree &Copy) ;
   ~kdTree() ;
   
   void destroy(void) { destroy(root) ; root = 0 ; }
   void destroy(kdNode *node);
   int operator== (kdTree &) { return 0 ; }
   int operator< (kdTree &) { return 0 ; }
   int operator> (kdTree &) { return 0 ; }
   kdTree& operator= (kdTree &Source) ;
   void copy(kdTree &Source) ;
   void insertAll(marray<int> &DTrain, int TrainSize, mmatrix<double> *NumValues, mmatrix<int> *DiscValues,
                        marray<int> *noDscValues, 
                        marray<double> *minValues, marray<double> *maxValues,  marray<double> *valueIntervals,
                        marray<double> *steps, 
                        marray<marray<double> > *NAdiscValues, marray<marray<double> > *NAcontValues ,
                       #if defined(RAMP_FUNCTION)
                        marray<double> *DifferentDistance, marray<double> *EqualDistance, marray<double> *CAslope,
                       #endif
                       int cntFrom, int cntTo, int dscFrom, int dscTo)  ;
   double caseDist(int I1, int I2)  ;
   void setBucketSize(int bcktSize) { bucketSize = bcktSize; }
   void findK(int queryPoint, int k) ;
} ;

#endif
