#if !defined(FTREE_H)
#define FTREE_H


#include "general.h"
#include "dataStore.h"
#include "contain.h"
#include "expr.h"
#include "bintree.h"
#include "rndforest.h"


class estimation ; // forward
class construct ; // forward

// class dealing with trees and forests
class featureTree: public bintree, public dataStore {
  friend class construct ;
  friend class expr ;
  friend class estimation ;

protected:
   double rootWeight ;
   marray<int> rootDTrain ;
   int rootTrainSize ;
   marray<construct> CachedConstructs ;
   marray<forestTree> forest ;
   int rfNoSelAttr ;
   marray<double> rfA ; // forest coefficients
   double rfA0 ;

   binnode* buildTree(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize, int currentDepth) ;
   void split(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize,
              binnode *Node, marray<int> &LeftTrain, marray<double> &pLeftTrain, int &LeftSize, marray<int> &RightTrain,
              marray<double> &pRightTrain, int &RightSize, double &wLeft, double &wRight) ;
   booleanT time2stop(binnode *Node) ;
   void createLeaf(binnode *Node) ;
   void buildModel(estimation &Estimator, binnode* Node) ;
   void check(binnode *branch, int caseIdx, marray<double> &ClassTable) ;
   char* printFTree(int &featureNo, int &leavesNo, marray<binnode*> &featureNode, marray<binnode*> &modelNode, binnode *branch, int place) ;
   char* tree2Dot(binnode *branch, int &featureNo, int &leavesNo, marray<binnode*> &featureNode, marray<binnode*> &modelNode) ;

   void Feature2Str(binnode *Node, char* const Str) ;
   double mPrune(binnode *Node) ;
   double mdlCode(binnode *Node) ;

   booleanT buildConstruct(estimation &Estimator, binnode* Node, int currentDepth) ;
   booleanT singleAttributeModel(estimation &Estimator, binnode* Node) ;
   double conjunct(estimation &Estimator, construct &bestConjunct, marray<construct> &stepCache, marray<double> &stepCacheEst ) ;
   double summand(estimation &Estimator, construct &bestSummand, marray<construct> &stepCache, marray<double> &stepCacheEst ) ;
   double multiplicator(estimation &Estimator, construct &bestMultiplicator, marray<construct> &stepCache, marray<double> &stepCacheEst ) ;
   int prepareAttrValues(estimation &Estimator, marray<construct> &Candidates) ;
   int prepareContAttrs(estimation &Estimator, constructComposition composition, marray<construct> &Candidates, construct& bestCandidate) ;
   void makeConstructNode(binnode* Node, estimation &Estimator, construct &Construct) ;
   void makeSingleAttrNode(binnode* Node, estimation &Estimator, int bestIdx, attributeCount bestType) ;
   void selectBeam(marray<construct> &Beam, marray<construct> &stepCache, marray<double> &stepCacheEst, marray<construct> &Candidates, estimation &Estimator, attributeCount aCount) ;
   double oobInplaceEvaluate(binnode *root, marray<int> &dSet, marray<booleanT> &oobSet, mmatrix<int> &oob) ;
   binnode* buildForestTree(int TrainSize, marray<int> &DTrain, int attrEstimator,  const marray<double> &attrProb, int rndIdx) ;
   double rfBuildConstruct(estimation &Estimator, binnode* Node, const marray<double> &attrProb, int rndIdx) ;
   void rfCheck(int caseIdx, marray<double> &probDist) const ;
   int rfTreeCheck(binnode *branch, int caseIdx, marray<double> &probDist) const;
   void rfSplit(marray<int> &DTrain, int TrainSize, binnode* Node, marray<int> &LeftTrain, int &LeftSize, marray<int> &RightTrain, int &RightSize) ;
   void rfNearCheck(int caseIdx, marray<double> &probDist) ;
   void rfFindNearInTree(binnode *branch, int caseIdx, marray<IntSortRec> &near) const ;
   binnode* rfBuildLimitedTree(int noTerminal, int TrainSize, marray<int> &DTrain, int attrEstimator, const marray<double> &attrProb, int rndIdx) ;
   void rfRevertToLeaf(binnode *Node) ;
   binnode* rfPrepareLeaf(int TrainSize, marray<int> &DTrain) ;
   void rfCheckReg(int caseIdx, marray<double> &probDist) const ;
   double rfEvalA0(void);
   void rfWriteTree(FILE* fout, int indent, int treeIdx) const;
   void rfWriteSubTree(FILE* fout, int indent, binnode *branch) const;
   void rfConsolidateTree(binnode *branch) ;
   booleanT rfTime2stop(binnode *Node) ;

   double oobAccuracy(mmatrix<int> &oob) ;
   void oobEvaluate(mmatrix<int> &oob) const ;
   double oobMargin(mmatrix<int> &oob, marray<int> &maxOther, double &varMargin) ;
   double oobSTD(marray<int> &maxOther) ;
   void oobMarginAV(mmatrix<int> &oob, int noVal, marray<int> &origVal,
								marray<double> &avMargin) ;
   void oobEvaluateCluster(mmatrix<int> &oob, marray<booleanT> &cluster) ;

   void shuffleChange(int noValues, marray<int> &valArray) ;
   void rfRegularize() ;
   void rfRegFrprmn(double lambda, marray<double> &p, int &iter, double &fret) ;
   double rfRegEval(marray<double> &a, marray<double> &g) ;
   void rfLinmin(marray<double> &p, marray<double> &xi, int n, double &fret) ;
   double rfFunc(marray<double> &a);
   void rfmnbrak(double &ax, double &bx, double &cx, double &fa, double &fb, double &fc);
   double rfBrent(double ax, double bx, double cx, double tol, double &xmin);
   double f1dim(double x);

#if defined(R_PORT)
   void rfMarkCaseInTree(binnode *branch, int caseIdx) ;
   void rfClearDTrain(binnode *branch) ;
   void rfLeafCooccurence(binnode *branch,  int outDim, SEXP out) ;
#endif

public:
   booleanT learnRF ;
   double avgOobAccuracy, avgOobMargin, avgOobCorrelation ;
   PseudoRandomStreams rndStr ;

   featureTree() { rootWeight = rfA0 = avgOobAccuracy = avgOobMargin = avgOobCorrelation = -DBL_MAX ;
                   rootTrainSize = rfNoSelAttr = -1 ; learnRF = mFALSE ; }
   ~featureTree() {}
   int constructTree(void);
   void test(marray<int> &DSet, int SetSize, double &Accuracy, double &avgCost, double &Inf,
             double &Auc, mmatrix<int> &PredictionMatrix, double &kappa, double &sensitivity, double &specificity,
			 double &brier, double &precision, double &Gmean, double &KS, double &TPR, double &FPR, FILE *probabilityFile) ;
   void outDomainSummary(FILE *to) const ;
   void printResultsHead(FILE *to) const ;
   void printResultLine(FILE *to, int idx, int Leaves, int freedom,
        double Accuracy, double Cost, double Inf, double Auc, double Sens, double Spec, double Brier, double Kappa) const ;
   void printResultSummary(FILE *to, marray<int> &Leaves, marray<int> &freedom,
        marray<double> &Accuracy, marray<double> &Cost, marray<double> &Inf, marray<double> &Auc,
		marray<double> &Sens, marray<double> &Spec, marray<double> &Brier, marray<double> &Kappa) const ;
   void printFTreeFile(char *FileName, int idx,  int Leaves, int freedom,
        double Accuracy, double Cost, double Inf, double Auc,
        mmatrix<int> &PMx, double Sens, double Spec, double Brier, double Kappa) ;
   char* printFTreeDot(void) ;
   char* printFTreeStr(void) ;
   double mPrune(void) { return mPrune(root) ; }
   int buildForest(void) ;
   void rfResultHead(FILE *to) const ;
   void rfResultLine(FILE *to, int idx, double oobAccuracy, double oobMargin, double oobCorrelation,
        double TestAccuracy, double TestCost, double TestInf, double TestAuc, double TestSens, double TestSpec, double TestBrier, double TestKappa) const ;
   void varImportance(marray<double> &varEval) ;
   void varImportanceCluster(marray<double> &varEval, marray<booleanT> &cluster) ;

   void printAttrEval(FILE *to, marray<int> &idx, marray<marray<double> > &attrEval) ;
   void avImportance(marray<marray<double> > &avEval) ;
   int writeRF(const char* TreeFileName) const;
   //int tempSaveForest(char *fName) ;
   int predictR(marray<int> &predictedClass, marray<double> &predictedProb) ;
    booleanT readForest(char *fileName) ;
   binnode* readTree(FILE* fin, int treeIdx) ;
   binnode* readNode(FILE* fin) ;
   int getSize(binnode *branch) const;
   int getSumOverLeaves(binnode *branch, int depth) const;
#if defined(R_PORT)
   SEXP exportSizes(void);
   SEXP exportSumOverLeaves(void);
   SEXP RF2R(void) ;
   SEXP RFtree2R(binnode *branch);

   SEXP T2Rpart(void) ;
   SEXP proximity(bool distance) ;
   SEXP proximityM(bool distance) ;
   SEXP importance2R(void) ;
   SEXP importance2RCluster(marray<double> &varEval, marray<booleanT> &cluster) ;


#endif



} ;

#endif
