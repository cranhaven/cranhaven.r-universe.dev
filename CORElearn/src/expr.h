#if !defined(EXPR_H)
#define EXPR_H

#include "general.h"
#include "contain.h"

enum exprType  {majority=1, kNN=2, kNNkernel=3, simpleBayes=4} ;

class exprNode
{
 public:
   int iMain, iAux ;
   exprNode *left, *right ;  // usually 2: left = 0, right = 1
   exprNode(){ iMain = iAux = -1 ;  left = right = 0 ; }
} ;

typedef exprNode* PexprNode ;

class binnode ; // forward definition

class estimation ;
class featureTree ;

class expr
{
//   friend class regressionTree ;
   exprType modelType ;
   exprNode *root ; // in case of constructs
   int majorClass ; // in case of major class
   marray<marray< marray<double> > > SBclAttrVal ; // in case of simple Bayes
   marray<marray<double> > SBattrVal ; 
   marray<double> SBcl ; 
   marray< marray<double> > Boundary ; // in case of simple Bayes and numeric attributes
   marray<double> equalDistance, differentDistance, CAslope ; // in case of kNN
   

   void destroy(void);
   void destroy(exprNode *node) ;
   void dup(exprNode *Source, PexprNode &Target) ;
   // void predict(binnode *treeNode, int Case, exprNode* Node) ;
   // char* descriptionString(exprNode* Node)  ;
   double CARamp(int AttrIdx, double distance) ;
   double DAdiff(binnode *treeNode, int AttrIdx, int I1, int I2) ;
   double CAdiff(binnode *treeNode, int AttrIdx, int I1, int I2) ;
   double examplesDistance(binnode *treeNode, int I1, int I2) ;

public:
   const featureTree *gFT ;

   expr() { root = 0 ; gFT = 0 ; majorClass = -1 ; modelType = majority ; }
   expr(featureTree* ft) { root = 0 ; gFT = ft; majorClass = -1 ; modelType = majority ;}
   ~expr() ;
   expr(expr &Copy) ;
   void copy(const expr &Source) ;
   expr& operator=(const expr &Source) ;
   int operator== (expr &) { return 0 ; }
   void createMajority(int Value) ;
   void createKNN(void) ;
   void createKNNkernel(void) ;
   void createSimpleBayes(estimation &Estimator, binnode *treeNode) ;
   void predict(binnode *treeNode, int Case, marray<double> &probDist) ;
   char* descriptionString(void)  ;
   int degreesOfFreedom(void) { return 1; }
   double smoothingParameter(int smoothingType) ;
} ;

#endif

