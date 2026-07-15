#if !defined(REGTREE_H)
#define REGTREE_H

#include <cfloat>

#include "dataStore.h"
#include "contain.h"
#include "bintreeReg.h"
#include "exprReg.h"


class estimationReg ; // forward
class constructReg ; // forward

// class dealing with regression trees,
class regressionTree: public bintreeReg, public dataStore
{
	friend class constructReg ;
	friend void ContDataRetriever(double Index, double Data[], marray<int> &Mask, int DataSize) ;
	friend void ContWDataRetriever(double Index, double Data[], marray<int> &Mask, int DataSize) ;
	friend double MdlCodeLen(double parameter[],  marray<int> &Mask) ;
	friend double CAdiffSign(int AttrIdx, int I1, int I2) ;

	friend class exprReg ;
	friend class estimationReg ;
	friend double CAdiffSign(binnodeReg *treeNode, int AttrIdx, int I1, int I2) ;

protected:

	binnodeReg *CurrentNode ;
	marray<int> *CurrentExamples ;
	int CurrentTrainSize ;
	marray<int> rootDTrain ;
	int rootTrainSize ;
	marray<constructReg> CachedConstructs ;
	marray<double> primaryEstimate, secondaryEstimate ;

	binnodeReg* buildTree(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize, int currentDepth) ;
	void split(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize,
			binnodeReg *Node, marray<int> &LeftTrain, marray<double> &pLeftTrain, int &LeftSize, marray<int> &RightTrain,
			marray<double> &pRightTrain, int &RightSize, double &wLeft, double &wRight) ;
	booleanT time2stop(binnodeReg *Node) ;
	void createLeaf(binnodeReg *Node) ;
	void buildTreeNode(binnodeReg *Node, marray<int> &DTrain, marray<double> &pDTrain, int TrainSize) const;
	void buildModel(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize, binnodeReg* Node) const;
	double check(binnodeReg *branch, int caseIdx) ;
	char* printRegTree(int &featureNo, int &leavesNo, marray<binnodeReg*> &featureNode, marray<binnodeReg*> &modelNode,  binnodeReg *branch, int place) ;
	char* tree2dot(binnodeReg *branch, int &featureNo, int &leavesNo, marray<binnodeReg*> &featureNode, marray<binnodeReg*> &modelNode) ;

	void binarize(constructReg &nodeConstruct, estimationReg &Estimator) ;
	double bestSplit(constructReg &nodeConstruct, estimationReg &Estimator) ;
	void Feature2Str(binnodeReg *Node, char* const Str) ;
	double mPrune(binnodeReg *Node) ;
	double mdlBottomUpPrune(binnodeReg *Node) ;
	double M5prune(binnodeReg *Node) ;
	double errorComplexityPrune(binnodeReg* Node, int &Size) ;
	double errorComplexityPruneVar(binnodeReg* Node, int &Size) ;
	double errorComplexityPruneCV(binnodeReg* Node, int &Size) ;
	void svdFitLinearModel(marray<int> &DTrain, int TrainSize, exprReg& Model)const ;
	void powellFitLinearModel(marray<int> &DTrain, int TrainSize, exprReg& Model)const ;
	void M5Simplify(marray<int> &DTrain, int TrainSize, binnodeReg *Node) const;
	double mdlCode(binnodeReg *Node) ;

	booleanT buildConstruct(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize, binnodeReg* Node, int currentDepth) ;
	booleanT singleAttributeModel(marray<int> &DTrain, marray<double> &pDTrain, int TrainSize, binnodeReg* Node) ;
	double conjunct(estimationReg &Estimator, constructReg &bestConjunct, marray<constructReg> &stepCache, marray<double> &stepCacheEst ) ;
	double summand(estimationReg &Estimator, constructReg &bestSummand, marray<constructReg> &stepCache, marray<double> &stepCacheEst ) ;
	double multiplicator(estimationReg &Estimator, constructReg &bestMultiplicator, marray<constructReg> &stepCache, marray<double> &stepCacheEst ) ;
	int prepareAttrValues(estimationReg &Estimator, marray<constructReg> &Candidates) ;
	int prepareContAttrs(estimationReg &Estimator, constructComposition composition, marray<constructReg> &Candidates, constructReg& bestCandidate) ;
	void makeConstructNode(binnodeReg* Node, estimationReg &Estimator, constructReg &Construct) ;
	void makeSingleAttrNode(binnodeReg* Node, estimationReg &Estimator, int bestIdx, attributeCount bestType) ;
	void selectBeam(marray<constructReg> &Beam, marray<constructReg> &stepCache, marray<double> &stepCacheEst, marray<constructReg> &Candidates, estimationReg &Estimator, attributeCount aCount) ;

public:
	double rootStdDev, rootAverage, rootWeight ;

	regressionTree() { rootStdDev = rootAverage = rootWeight = - DBL_MAX ;
	CurrentNode = 0 ; CurrentExamples = 0 ;
	CurrentTrainSize = rootTrainSize = -1 ; }
	~regressionTree() {}
	//   inline void Setopt->beamSize(int beam) { opt->beamSize=beam; }
	//   inline void SetLookahead(int l) { maxLookahead=l; }
	int constructRegTree(void);
	void test(marray<int> &DSet, int SetSize, double &SE, double &RSE, double &AE, double &RAE, FILE *residFile) ;
	void outDomainSummary(FILE *to) const ;
	void printResultsHead(FILE *to) const ;
	void printResultLine(FILE *to, int idx, int LeavesAfter, int freedomAfter,
			double TestSEafter, double TestRSEafter, double TestAEafter, double TestRAEafter) const ;
	void printResultSummary(FILE *to, marray<int> &LeavesAfter, marray<int> &freedomAfter,
			marray<double> &TestSEafter, marray<double> &TestRSEafter, marray<double> &TestAEafter, marray<double> &TestRAEafter) const ;
	void printFTreeFile(char *OutName, int idx, int LeavesAfter, int freedomAfter,
			double TestSEafter, double TestRSEafter, double TestAEafter, double TestRAEafter) ;
	char* printTreeStr(void) ;
	char* printTreeDot(void) ;
	double mPrune(void) { return mPrune(root) ; }
	double M5prune(void) { return M5prune(root) ; }
	double mdlBottomUpPrune(void) { return mdlBottomUpPrune(root) ; }
	double mdlSelectBestPrune(void) ;
	double errorComplexityPrune(void) { int Size = 0 ; return errorComplexityPrune(root, Size) ; }
	int predictRreg(marray<double> &predicted) ;
	double discretizeGreedy(int ContAttrIdx, estimationReg &Estimator, int maxBins, marray<double> &Bounds) ;

	int getSize(binnodeReg *branch);
#if defined(R_PORT)
		   SEXP T2Rpart(void) ;
#endif

} ;

#endif
