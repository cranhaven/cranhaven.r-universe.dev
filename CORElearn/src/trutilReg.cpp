/*********************************************************************
 *   Name:              modul trutil  (tree utillities)
 *
 *   Description:  utillities for  tree
 *
 *********************************************************************/

#include <cstring>     // dealing with names
#include <cstdlib>     // min, max
#include <cstdio>
#include <cfloat>

#include "general.h"
#include "utils.h"
#include "error.h"
#include "regtree.h"
#include "constrctReg.h"
#include "estimatorReg.h"
#include "frontend.h"

using namespace std ;


//************************************************************
//
//                      printRegTree
//                      ----------
//
//   recursively prints the entire feature tree
//
//
//************************************************************
char* regressionTree::printRegTree(int &featureNo,int &leavesNo,
		marray<binnodeReg*> &featureNode, marray<binnodeReg*> &modelNode,
		 binnodeReg *branch, int place)
{
	if (branch)
	{
		if (branch->left) // not the leaf yet
		{
			mstring outTree ;

			int fNo = featureNo++ ;   // reserve current index

			char *lTreeStr = printRegTree(featureNo, leavesNo, featureNode, modelNode, branch->left, place+5);

			outTree.append(lTreeStr) ;
			delete [] lTreeStr ;
			char *buf = new char[place + 20] ;
			snprintf(buf, place + 20, "%*sf%d\n",place," ",fNo) ;
			outTree.append(buf) ;
			delete [] buf ;
			featureNode[fNo] = branch ;

			char *rTreeStr = printRegTree(featureNo, leavesNo, featureNode, modelNode, branch->right, place+5);
			outTree.append(rTreeStr) ;
			delete [] rTreeStr ;

			char *retStr = outTree.unWrap() ;
			return retStr ;
		}
		else
		{
			char *buf = new char[place + 20] ;
			snprintf(buf, place + 20, "%*sl%d\n", place, " ", leavesNo) ;
			modelNode[leavesNo] = branch ;
			leavesNo++ ;
			return buf ;
		}
	}
	return 0 ;
}

char* regressionTree::printTreeStr(void) {

	int featureNo  = 0;
	int noLeaf = noLeaves() ;
	marray<binnodeReg*> featureNode(noLeaf) ;
	marray<binnodeReg*> modelNode(noLeaf) ;
	int leavesNo  = 0;

	char buf[MaxFeatureStrLen] ;

	char *tStr = printRegTree(featureNo, leavesNo, featureNode, modelNode, root, 0);
	mstring fTreeStr(tStr) ;
	delete [] tStr ;

	int i ;
	mstring fStr("\n") ;
	char *bufLine = new char[MaxFeatureStrLen+30] ;

	for (i=0; i < featureNo ; i++)
	{
		Feature2Str(featureNode[i], buf);
		snprintf(bufLine, MaxFeatureStrLen+30, "f%d: %s\n", i, buf) ;
		fStr.append(bufLine) ;
	}
	fTreeStr.append(fStr) ;
	char *modelDescription ;

	mstring lStr("\n\nLeaf     weight sqrt(MSE)       MAE avg.pred.   std.dev  model_description \n--------------------------------------------------------------------\n") ;

	for (i=0 ; i<leavesNo ; i++)
	{
		modelDescription = modelNode[i]->Model.descriptionString() ;
		snprintf(bufLine, MaxFeatureStrLen+30, "l%-3d: %9.2f %9.2f %9.2f %9.2f %9.2f  %s\n",
				i, modelNode[i]->weight, sqrt(modelNode[i]->MSE), modelNode[i]->MAE,
				modelNode[i]->averageClassValue, modelNode[i]->stdDevClass,
				modelDescription ) ;

		lStr.append(bufLine) ;
		delete [] modelDescription ;
	}
	fTreeStr.append(lStr) ;
	delete [] bufLine ;

	char *retStr = fTreeStr.unWrap() ;
	return retStr ;
}


char* regressionTree::printTreeDot(void) {
	int featureNo  = 0;
	int noLeaf = noLeaves() ;
	marray<binnodeReg*> featureNode(noLeaf) ;
	marray<binnodeReg*> modelNode(noLeaf) ;
	int leavesNo  = 0;


	char buf[MaxFeatureStrLen], dotBuf[MaxFeatureStrLen+30] ;

	snprintf(dotBuf, MaxFeatureStrLen+30, "digraph \"dotRegressionTree\" {\n") ;
	mstring dotTree(dotBuf) ;
	char *treeStr = tree2dot(root, featureNo, leavesNo, featureNode, modelNode) ;
	dotTree.append(treeStr) ;
	delete [] treeStr ;

	int i ;
	mstring fStr("\n") ;
	for (i=0; i<featureNo ; i++)
	{
		Feature2Str(featureNode[i], buf);
		snprintf(dotBuf, MaxFeatureStrLen+30, "\tf%d [label = \"%s\"]\n", i, buf) ;
		fStr.append(dotBuf) ;
	}
	dotTree.append(fStr) ;
	char *modelDescription ;
	mstring modelStr("\n") ;

	for (i=0 ; i<leavesNo ; i++)
	{
		modelDescription = modelNode[i]->Model.descriptionString() ;
		snprintf(dotBuf, MaxFeatureStrLen+30, "\tl%d [shape = box, label = \"%s\"]\n", i, modelDescription) ;
		modelStr.append(dotBuf) ;
		delete [] modelDescription ;
	}
	modelStr.append("}\n") ;
	dotTree.append(modelStr) ;

	char *retValue = dotTree.unWrap() ;
	return retValue ;
}



//************************************************************
//
//                   printFTreeFile
//                   --------------
//
//             prints the feature tree on a file
//
//************************************************************
void regressionTree::printFTreeFile(char *FileName, int idx,
		int LeavesAfter, int freedomAfter,
		double TestSEafter, double TestRSEafter,
		double TestAEafter, double TestRAEafter)
{
	FILE *to, *toDot=0 ;
	if ((to=fopen(FileName,"w"))==NULL)
	{
		merror("Cannot open tree output file",FileName) ;
		return ;
	}
	outVersion(to) ;
	opt->outConfig(to) ;
	fprintf(to,"\n");
	printLine(to,"-",46) ;
	printResultsHead(to) ;
	printResultLine(to, idx, LeavesAfter, freedomAfter,
			TestSEafter, TestRSEafter,
			TestAEafter, TestRAEafter) ;
	printLine(to,"-",46) ;

	char buf[MaxFeatureStrLen] ;

	char *fTreeStr = printTreeStr();
	fprintf(to, "%s\n", fTreeStr) ;
	delete [] fTreeStr ;

	printLine(to,"-",46) ;


	if (opt->printTreeInDot)
	{
		strcpy(buf, FileName) ;
		strcat(buf, ".dot") ;
		if ((toDot=fopen(buf,"w"))==NULL)
		{
			merror("Cannot open dot tree output file",buf) ;
		}
		else {
			char *dotTreeStr = printTreeDot() ;
			fprintf(toDot, "%s\n", dotTreeStr) ;
			fclose(toDot) ;
			delete [] dotTreeStr ;
		}
	}
	fclose(to) ;


}


//************************************************************
//
//                tree2dot
//                 -------------
//
//   recursively prints the entire feature tree in a dot format to string
//
//************************************************************
const int MaxLine = 128 ;
char* regressionTree::tree2dot(binnodeReg *branch, int &featureNo, int &leavesNo, marray<binnodeReg*> &featureNode, marray<binnodeReg*> &modelNode) {
	if (branch)
	{
		if (branch->left) // not the leaf yet
		{
			int fNo = featureNo ;
			featureNo++ ;   // reserve current index
			featureNode[fNo] = branch ;
			char *buf = new char[MaxLine] ;

			if (branch->left->left) // Is the left node a leaf?
				snprintf(buf, MaxLine, "\tf%d -> f%d [label = \"yes\"]\n", fNo, featureNo) ;
			else
				snprintf(buf, MaxLine, "\tf%d -> l%d [label = \"yes\"]\n", fNo, leavesNo) ;

			mstring treeStr(buf) ;
			char *leftStr = tree2dot(branch->left, featureNo, leavesNo, featureNode, modelNode);
			treeStr.append(leftStr) ;
			delete [] leftStr ;

			if (branch->right->left) // is right one the leaf
				snprintf(buf, MaxLine, "\tf%d -> f%d [label = \"no\"]\n", fNo, featureNo) ;
			else
				snprintf(buf, MaxLine, "\tf%d -> l%d [label = \"no\"]\n", fNo, leavesNo) ;

			treeStr.append(buf) ;
			delete [] buf ;

			char *rightStr = tree2dot(branch->right, featureNo, leavesNo, featureNode, modelNode);
			treeStr.append(rightStr) ;
			delete [] rightStr ;
			char *retStr = treeStr.unWrap() ;
			return retStr ;
		}
		else  {
			// fprintf(outDot, "\tl%d [shape = box]\n", LeavesNo) ;
			modelNode[leavesNo] = branch ;
			leavesNo++ ;
		}
	}
	return 0 ;

}



//************************************************************
//
//                      outDomainSummary
//                      ---------------
//
//     prints various parameters of the data
//
//************************************************************

void regressionTree::outDomainSummary(FILE *to) const
{
	fprintf(to,"\n\n DATA INFO") ;
	fprintf(to,"\n-----------------------------------------------------------") ;
	fprintf(to,"\nDomain name: %s", opt->domainName.getConstValue()) ;
	fprintf(to,"\nNumber of all examples: %d", NoCases) ;
	fprintf(to,"\nNumber of discrete attributes: %d", noDiscrete) ;
	fprintf(to,"\nNumber of continuous attributes: %d", noNumeric-1) ;
	fprintf(to,"\nNumber of all attributes: %d", noAttr) ;
	fprintf(to,"\n-----------------------------------------------------------\n") ;
}



//************************************************************
//
//                      test
//                      ----
//
//        performs testing on testing examples
//
//************************************************************
void regressionTree::test(marray<int> &DSet, int SetSize,
		double &SE, double &RSE,double &AE, double &RAE,
		FILE *residFile)
{

	double residium ;

	if (SetSize == 0)   {
		merror("regressionTree::test","There is no data set available.");
		return ;
	}
	// set where the prediction data is
	dData = &DiscData ;
	nData = &NumData ;

	marray<double> prediction(SetSize), truePrediction(SetSize) ;
	for (int i=0; i < SetSize ; i++)      {
		prediction[i] = check(root,DSet[i]) ;  // safe prediction
		truePrediction[i] = NumData(DSet[i],0) ;

		residium = prediction[i] - truePrediction[i] ;
		if (residFile != NULL)
			fprintf(residFile,"%6d, %f\n",DSet[i], residium) ;
	}

	modelEvalReg(SetSize, truePrediction, prediction, root->averageClassValue, SE, RSE, AE, RAE) ;
}



//************************************************************
//
//                      check
//                      -----
//
//        computes classification for single case
//
//************************************************************
double regressionTree::check(binnodeReg *branch, int caseIdx)
{
	double contValue = NAcont;
	char discValue = NAdisc;
	switch (branch->Identification)
	{
	case leaf:
		return branch->Model.predictSafe(branch, caseIdx) ;
	case continuousAttribute:
		contValue = branch->Construct.continuousValue(*dData,*nData,caseIdx) ;
		break ;
	case discreteAttribute:
		discValue = branch->Construct.discreteValue(*dData,*nData,caseIdx) ;
		break ;
	default:
		merror("regressionTree::check", "invalid branch identification") ;
	}
	double ret = 0 ;
	if ((branch->Identification == continuousAttribute && isNAcont(contValue)) ||
			(branch->Identification == discreteAttribute  && discValue == NAdisc) )
	{   // missing value
		ret = ( branch->weightLeft * check(branch->left, caseIdx) +
				(branch->weight - branch->weightLeft) * check(branch->right, caseIdx) +
				opt->smoothingValue* branch->Model.predictSafe(branch, caseIdx) )
                    		  / (branch->weight + opt->smoothingValue) ;
	}
	else {
		if ( (branch->Identification == continuousAttribute && (contValue <= branch->Construct.splitValue)) // || fabs(contValue - branch->Construct.splitValue)<epsilon) )
				||(branch->Identification == discreteAttribute &&  branch->Construct.leftValues[discValue]) )
			// going left
			ret =  (opt->smoothingValue * branch->Model.predictSafe(branch, caseIdx) +
					branch->weightLeft * check(branch->left, caseIdx))
					/ (branch->weightLeft + opt->smoothingValue) ;
		else // going right
			ret =  ( opt->smoothingValue * branch->Model.predictSafe(branch, caseIdx) +
					(branch->weight - branch->weightLeft) * check(branch->right, caseIdx))
					/ (branch->weight - branch->weightLeft + opt->smoothingValue);
	}
	return ret ;
}


//************************************************************
//
//                      printResultsHead
//                      ----------------
//
//              prints head of results table
//
//************************************************************
void regressionTree::printResultsHead(FILE *to) const
{
	fprintf(to,"\n%3s %5s %6s %8s %5s %8s %5s\n",  "idx","#leaf", "dgFree", "SqrErr", "RSE", "AbsErr", "RAE") ;
	printLine(to,"-",46) ;
}


//************************************************************
//
//                      printResultLine
//                      ---------------
//
//        prints results for one tree into a single line
//
//************************************************************
void regressionTree::printResultLine(FILE *to, int idx,
		int LeavesAfter, int freedomAfter,
		double TestSEafter, double TestRSEafter,
		double TestAEafter, double TestRAEafter) const
{
	fprintf(to,"%3d %5d %6d %8.3f %5.3f %8.3f %5.3f\n",
			idx,
			LeavesAfter, freedomAfter,
			TestSEafter, TestRSEafter,
			TestAEafter, TestRAEafter) ;
}

//************************************************************
//
//                    printReportFile
//                    ---------------
//
//           prints the report about domain testing
//              with current parameters on a file
//
//************************************************************
void regressionTree::printResultSummary(FILE *to,
		marray<int> &LeavesAfter, marray<int> &freedomAfter,
		marray<double> &TestSEafter, marray<double> &TestRSEafter,
		marray<double> &TestAEafter, marray<double> &TestRAEafter) const
{
	double avgLA, stdLA, avgFA, stdFA, avgSAtest, stdSAtest, avgRSAtest, stdRSAtest ;
	double avgAAtest, stdAAtest, avgRAAtest, stdRAAtest ;

	AvgStd(LeavesAfter, opt->numberOfSplits, avgLA, stdLA) ;
	AvgStd(freedomAfter, opt->numberOfSplits, avgFA, stdFA) ;
	AvgStd(TestSEafter, opt->numberOfSplits, avgSAtest, stdSAtest) ;
	AvgStd(TestRSEafter, opt->numberOfSplits, avgRSAtest, stdRSAtest) ;

	AvgStd(TestAEafter, opt->numberOfSplits, avgAAtest, stdAAtest) ;
	AvgStd(TestRAEafter, opt->numberOfSplits, avgRAAtest, stdRAAtest) ;

	printLine(to,"-",46) ;

	printResultLine(to, -1, int(avgLA+0.5), int(avgFA+0.5),
			avgSAtest,  avgRSAtest,  avgAAtest, avgRAAtest) ;

	// after pruning
	fprintf(to, "\n\nNumber of leaves after pruning : %.2f(%.2f)\n", avgLA, stdLA) ;
	fprintf(to, "Degrees of freedom after pruning : %.2f(%.2f)\n", avgFA, stdFA) ;
	fprintf(to, "Root of squared error for test sample after pruning : %.2f(%.2f)\n",avgSAtest, stdSAtest) ;
	fprintf(to, "Relative squared error for test sample after pruning : %.2f(%.2f)\n", avgRSAtest, stdRSAtest) ;
	fprintf(to, "Absolute error for test sample after pruning : %.2f(%.2f)\n",avgAAtest, stdAAtest) ;
	fprintf(to, "Relative absolute error for test sample after pruning : %.2f(%.2f)\n", avgRAAtest, stdRAAtest) ;
}




// ************************************************************
//
//                      Feature2Str
//                      -----------
//
//        converts feature (a node) to a description string
//
// ************************************************************
void regressionTree::Feature2Str(binnodeReg *Node, char* const Str)
{
	Node->Construct.descriptionString(Str) ;
}







