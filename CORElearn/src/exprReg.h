#if !defined(EXPRREG_H)
#define EXPRREG_H

#include <cfloat>
#include "general.h"
#include "contain.h"

enum exprRegType  {plus, minus, times, constTimesAttr, intConstant, floatConstant, kNNreg, kernelRegression, LWLR} ;

class exprRegNode
{
public:
   exprRegType nodeType ;
   int iMain, iAux ;
   double  dMain, dAux ;
   exprRegNode *left, *right ;
   exprRegNode() { nodeType = floatConstant; iMain = iAux = -1 ; dMain = dAux = -DBL_MAX ; left = right = 0 ;}

} ;

typedef exprRegNode* PexprRegNode ;

// forward definitions
class binnodeReg ; 
class regressionTree ;
class dataStore ;

class exprReg
{
//   friend class regressionTree ;
   exprRegNode *root ;

   marray<double> equalDistance, differentDistance, CAslope ;

   void destroy(exprRegNode *node) ;
   void dup(exprRegNode *Source, PexprRegNode &Target) ;
   int noCoefficients(exprRegNode* Node) ;
   double predict(binnodeReg *treeNode, int Case, exprRegNode* Node) ;
   char* descriptionString(exprRegNode* Node)  ;
   double examplesDistance(binnodeReg *treeNode, int I1, int I2) ;
   double CAdiff(binnodeReg *treeNode, int AttrIdx, int I1, int I2) ;
   double DAdiff(binnodeReg *treeNode, int AttrIdx, int I1, int I2) ;
   void createLocal(int TrainSize, int k) ;
   void svdLWLR(double *x, double *y, int TrainSize, double *a) ;


public:
   const regressionTree *gRT ;
   exprReg() { root = 0 ; gRT = 0 ; }
   exprReg(regressionTree *rt) { root = 0 ; gRT = rt; }
   ~exprReg() { destroy(); gRT=0; }
   exprReg(exprReg &Copy) {    root = 0 ;  copy(Copy) ; }
   void destroy(void) ;
   void copy(const exprReg &Source) ;
   exprReg& operator=(const exprReg &Source) ;
   int operator== (exprReg &) { return 0 ; }
   void createPoint(double Value) ;
   void createLinear(double *Parameters, int modelSize, marray<int> &Mask) ;
   void createKNN(int TrainSize, int k) ;
   void creatennKernelWidthReg(int TrainSize, int k, double nnKernelWidth) ;
   void createLWLR(int TrainSize, int k, double nnKernelWidth) ;
   int noCoefficients(void)
       { if (root) return noCoefficients(root);
              else return 0 ; }
   double predict(binnodeReg *treeNode, int Case) ;
   double predictSafe(binnodeReg *treeNode, int Case) ;
   char* descriptionString(void)  ;
   double mdlCost(int  noAttributes) ;
   double mdlExprCost(exprRegNode* Node) ;
   double mdlPointCost() { if (root) return mdlPointCost(root) ; else return 0 ; }
   double mdlPointCost(exprRegNode* Node) ;
   int degreesOfFreedom(void) { return noCoefficients(); }
#if defined(RAMP_FUNCTION)
      inline double CARamp(int AttrIdx, double distance) ;
#endif
} ;


#endif

