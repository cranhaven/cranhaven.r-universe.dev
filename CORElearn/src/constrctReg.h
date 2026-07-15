#if !defined(CONSTRUCTREG_H)
#define CONSTRUCTREG_H

#include <float.h>
#include "general.h"
#include "contain.h"

class estimationReg ; // forward

class constructRegNode
{
 public:
   constructNodeType nodeType ;
   int attrIdx, valueIdx ;
   double  lowerBoundary, upperBoundary ;
   constructRegNode *left, *right ;
   constructRegNode(){ nodeType = cnDISCattribute; attrIdx = valueIdx = -1 ;
                       lowerBoundary = upperBoundary = -DBL_MAX ;
                       left = right = 0 ; }
} ;

typedef constructRegNode* PconstructRegNode ;

// struct binnodeReg ; // forward definition

class constructReg
{
   friend class regressionTree ;
   constructRegNode *root ;
   //  binnodeReg* TreeNode ;  

   void destroy(constructRegNode *node) ;
   void dup(const constructRegNode *Source, PconstructRegNode &Target) ;
   char discreteValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx, constructRegNode* Node) ;
   double continuousValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx, constructRegNode* Node) ;
   char* description(constructRegNode *Node)  ;
   int degreesOfFreedom(constructRegNode *Node) ;
   booleanT containsAttribute(constructRegNode *Node, int attributeIdx) ;
   void flattenConjunct(constructRegNode *Node, marray<int> &discAttrIdxs, marray<int> &AttrVals, marray<int> &contAttrIdxs, marray<double> &lowerBndys, marray<double> &upperBndys) ;
   double mdlAux(constructRegNode *Node) ;
   void flattenContConstruct(constructRegNode *Node, marray<int> &contAttrIdxs)  ;


public:
   regressionTree *gRT ;
   attributeCount countType ;
   constructComposition compositionType ;
   marray<booleanT> leftValues ;
   double splitValue ;
   int noValues ;
   int splitEstimator ;
   
   constructReg() { root = 0 ; gRT = 0 ; countType = aDISCRETE; compositionType = cSINGLEattribute;
                       splitValue = -DBL_MAX; noValues = -1; splitEstimator = -1; }

   ~constructReg() { destroy() ; gRT = 0 ; }
   constructReg(constructReg &Copy) {    root = 0;  copy(Copy) ; }
   void destroy() { if (root) destroy(root) ; root = 0 ; }
   int operator== (constructReg &X) ;
   constructReg& operator= (constructReg &X) ;
   int operator< (constructReg &) { return 0; }
   int operator> (constructReg &) { return 0; }
   void copy(constructReg &Source) ;
   void descriptionString(char* const Str) ;
   int degreesOfFreedom(void) ;
   char discreteValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx) ;
   double continuousValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx)  ;
   void createSingle(int bestIdx, attributeCount count) ;
   void Conjoin(constructReg &First, constructReg &Second) ;
   booleanT containsAttribute(constructReg &AVconstructReg) ;
   void flattenConjunct(marray<int> &discAttrIdxs, marray<int> &AttrVals, marray<int> &contAttrIdxs, marray<double> &lowerBndys, marray<double> &upperBndys) ;
   double mdlAux(void) ;
   void add(constructReg &First, constructReg &Second) ;
   void multiply(constructReg &First, constructReg &Second) ;
   void flattenContConstruct(marray<int> &contAttrIdxs) ;
   double mdlConstructCode()  ;
} ;


#endif

