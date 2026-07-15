/*********************************************************************
*   Name:              modul trutil  (tree utillities)
*
*   Description:  utillities for  tree
*
*********************************************************************/

#include <cstring>     // dealing with names
#include <cstdlib>     // min, max
#include <cfloat>
#include <cstdio>

#include "general.h"
#include "utils.h"
#include "error.h"
#include "ftree.h"
#include "constrct.h"
#include "estimator.h"
#include "frontend.h"
#include "options.h"

using namespace std ;

//************************************************************
//
//                      printFTree
//                      ----------
//
//   recursively prints the entire feature tree
//   if features are too long, make an abbreviation and full
//                  description below
//
//************************************************************
char* featureTree::printFTree(int &featureNo, int &leavesNo, marray<binnode*> &featureNode, marray<binnode*> &modelNode,
                              binnode *branch, int place)
{
   if (branch)
   {
      if (branch->left) // not a leaf yet
      {
    	 mstring outTree ;

    	 int fNo = featureNo ;  // reserve current index
    	 featureNo++ ;

         char *lTreeStr = printFTree(featureNo, leavesNo, featureNode, modelNode,
                    branch->left, place+5);
         outTree.append(lTreeStr) ;
         delete [] lTreeStr ;
         char *buf = new char[place + 20] ;
         snprintf(buf,place + 20, "%*sf%d\n",place," ",fNo) ;
         outTree.append(buf) ;
         delete [] buf ;
         featureNode[fNo] = branch ;

         char *rTreeStr = printFTree(featureNo, leavesNo, featureNode, modelNode,
                    branch->right, place+5);
         outTree.append(rTreeStr) ;
         delete [] rTreeStr ;

         char *retStr = outTree.unWrap() ;
         return retStr ;
      }
      else
      {
         char *buf = new char[place + 20] ;
    	 snprintf(buf, place + 20, "%*sl%d\n",place," ",leavesNo) ;
    	 modelNode[leavesNo] = branch ;
         leavesNo++ ;
         return buf ;
      }
   }
   return 0 ;
}

char* featureTree::printFTreeDot(void) {
	int featureNo  = 0;
	int noLeaf = noLeaves() ;
	marray<binnode*> featureNode(noLeaf) ;
	marray<binnode*> modelNode(noLeaf) ;
	int leavesNo  = 0;

	char buf[MaxFeatureStrLen], dotBuf[MaxFeatureStrLen+30] ;

	snprintf(dotBuf, MaxFeatureStrLen+30, "digraph \"dotDecisionTree\" {\n") ;
	mstring dotTree(dotBuf) ;
	char *treeStr = tree2Dot(root, featureNo, leavesNo, featureNode, modelNode) ;
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

	for (i=0 ; i < leavesNo ; i++)
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
//                   printFTreeStr
//                   --------------
//
//             prints the feature tree
//
//************************************************************
char* featureTree::printFTreeStr(void) {

   int featureNo  = 0;
   int noLeaf = noLeaves() ;
   marray<binnode*> featureNode(noLeaf) ;
   marray<binnode*> modelNode(noLeaf) ;
   int leavesNo  = 0;

   char buf[MaxFeatureStrLen] ;

   char *tStr = printFTree(featureNo, leavesNo, featureNode, modelNode, root, 0);
   mstring fTreeStr(tStr) ;
   delete [] tStr ;

   int i,j ;
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

   // header
   mstring clStr("\nLeaf    node_weight") ;
   for (j=0 ; j<noClasses ; j++) {
	  snprintf(buf, MaxFeatureStrLen, "p(%s)", AttrDesc[0].ValueName[j]);
      snprintf(bufLine, MaxFeatureStrLen+30, " %*s", Mmax(6,int(4+strlen(AttrDesc[0].ValueName[j]))), buf);
      clStr.append(bufLine) ;
   }
   clStr.append(" prediction\n") ;
   for (i=0 ; i < leavesNo ; i++)
   {
      snprintf(bufLine, MaxFeatureStrLen+30, "l%-4d |%12.2f", i, modelNode[i]->weight) ;
      clStr.append(bufLine) ;
      for (j=0 ; j<noClasses ; j++) {
         snprintf(bufLine, MaxFeatureStrLen+30, " %*.4f", Mmax(6,4+int(strlen(AttrDesc[0].ValueName[j]))),
                            modelNode[i]->Classify[j+1] / modelNode[i]->weight );
         clStr.append(bufLine) ;
      }

      modelDescription = modelNode[i]->Model.descriptionString() ;
      snprintf(bufLine, MaxFeatureStrLen+30, " %s\n", modelDescription) ;
      clStr.append(bufLine) ;
      delete [] modelDescription ;
   }
   fTreeStr.append(clStr) ;
   delete [] bufLine ;

   char *retStr = fTreeStr.unWrap() ;
   return retStr ;
}



//************************************************************
//
//                   printFTreeFile
//                   --------------
//
//             prints the feature tree on a file
//
//************************************************************
void featureTree::printFTreeFile(char *FileName, int idx,
        int Leaves, int freedom,  double TestAccuracy, double TestCost, double TestInf, double TestAuc,
        mmatrix<int> &TestPMx, double TestSens, double TestSpec, double TestBrier, double TestKappa)
{
   FILE *to, *toDot=NULL ;
   if ((to=fopen(FileName,"w"))==NULL)
   {
       merror("Cannot open tree output file",FileName) ;
       return ;
   }
   outVersion(to) ;
   opt->outConfig(to) ;
   fprintf(to,"\n");
   printLine(to,"-",70)  ;
   printResultsHead(to) ;
   printResultLine(to, idx, Leaves, freedom,
                            TestAccuracy, TestCost, TestInf, TestAuc, TestSens, TestSpec, TestBrier, TestKappa) ;
   printLine(to,"-",70)  ;

   char buf[MaxFeatureStrLen] ;

   char *fTreeStr = printFTreeStr();
   fprintf(to, "%s\n", fTreeStr) ;
   delete [] fTreeStr ;

   printLine(to,"-",70)  ;

   if (opt->printTreeInDot)
   {
	  strcpy(buf, FileName) ;
	  strcat(buf, ".dot") ;
      if ((toDot=fopen(buf,"w"))==NULL)
	  {
         merror("Cannot open dot tree output file",buf) ;
	  }
	  else {
		  char *dotTreeStr = printFTreeDot() ;
		  fprintf(toDot, "%s\n", dotTreeStr) ;
		  fclose(toDot) ;
		  delete [] dotTreeStr ;
      }
   }

   int i, j  ;
   fprintf(to,"Prediction matrix for testing set (%d instances)\n",TestPMx(0,0)) ;
   printLine(to,"-",65)  ;
   for (i=0 ; i<noClasses; i++)
      fprintf(to," (%c)  ",'a'+i) ;
   fprintf(to,"    <- classified as\n") ;
   for (i=0 ; i<noClasses*6; i++)
      fprintf(to, "-") ;
   fprintf(to,"\n") ;
   for (j=1 ; j <= noClasses; j++)
   {
      for (i=1 ; i<=noClasses; i++)
         fprintf(to,"%4d  ",TestPMx(i,j)) ;
      fprintf(to,"    (%c): %s\n",'a'+j-1,AttrDesc[0].ValueName[j-1]) ;
   }
   fprintf(to, "\n") ;
   if (noClasses == 2) {
	  fprintf(to,"\nPositives: %s, negatives: %s", AttrDesc[0].ValueName[0], AttrDesc[0].ValueName[1]) ;
	  fprintf(to, "\nSensitivity: %.3f\nSpecificity: %.3f\n", TestSens, TestSpec) ;
   }
   fclose(to) ;
}

/* ***********************************************************

                    tree2dot
                      -------------

   recursively prints the entire feature tree in a dot format

*********************************************************** */
const int MaxLine = 128 ;
char* featureTree::tree2Dot(binnode *branch, int &featureNo, int &leavesNo, marray<binnode*> &featureNode, marray<binnode*> &modelNode)
{
   if (branch)
   {
      if (branch->left) // not the leaf yet
      {
         int fNo = featureNo ; // reserve current index
         featureNo++ ;
         featureNode[fNo] = branch ; // store
         char *buf = new char[MaxLine] ;

         if (branch->left->left) // is left one the leaf
		   snprintf(buf, MaxLine, "\tf%d -> f%d [label = \"yes\"]\n", fNo, featureNo) ;
		 else
		   snprintf(buf, MaxLine, "\tf%d -> l%d [label = \"yes\"]\n", fNo, leavesNo) ;

         mstring treeStr(buf) ;
         char *leftStr = tree2Dot(branch->left, featureNo, leavesNo, featureNode, modelNode);
         treeStr.append(leftStr) ;
         delete [] leftStr ;

         if (branch->right->left) // is the right one a leaf?
		   snprintf(buf, MaxLine, "\tf%d -> f%d [label = \"no\"]\n", fNo, featureNo) ;
		 else
		   snprintf(buf, MaxLine, "\tf%d -> l%d [label = \"no\"]\n", fNo, leavesNo) ;

         treeStr.append(buf) ;
         delete [] buf ;

         char *rightStr = tree2Dot(branch->right, featureNo, leavesNo, featureNode, modelNode);
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

void featureTree::outDomainSummary(FILE *to) const
{
    fprintf(to,"\n\n DATA INFO") ;
    fprintf(to,"\n-----------------------------------------------------------") ;
    fprintf(to,"\nDomain name: %s", opt->domainName.getConstValue()) ;
    fprintf(to,"\nNumber of examples: %d", NoCases) ;
    fprintf(to,"\nNumber of class values: %d", noClasses) ;
    fprintf(to,"\nNumber of attributes: %d", noAttr) ;
    fprintf(to,"\nNumber of nominal attributes: %d", noDiscrete-1) ;
    fprintf(to,"\nNumber of numeric attributes: %d", noNumeric) ;
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
void featureTree::test(marray<int> &DSet, int SetSize, double &Accuracy,
           double &avgCost, double &Inf, double &Auc,
		   mmatrix<int> &PredictionMatrix, double &kappa, double &sensitivity, double &specificity,
		   double &brier, double &precision, double &Gmean, double &KS, double &TPR, double &FPR, FILE *probabilityFile){

   Accuracy = avgCost = Inf = Auc = sensitivity = specificity = Gmean = KS = TPR = FPR =-1.0 ;
   if (SetSize == 0) {
      merror("featureTree::test","There is no data set available.");
      return ;
   }
   // set where the prediction data is
   dData = &DiscData ;
   nData = &NumData ;

   marray<int> trueClass(SetSize, NAdisc) ;
   marray<marray<double> > probDist(SetSize);

   for (int i=0; i < SetSize ; i++)
   {
      probDist[i].create(noClasses+1, 0.0) ;

	  if  (learnRF) {
          if (opt->rfkNearestEqual>0)
		     rfNearCheck(DSet[i], probDist[i]) ;
		  else if (noClasses==2 && opt->rfRegType==1)
			 rfCheckReg(DSet[i], probDist[i]) ;
		  else 	rfCheck(DSet[i], probDist[i]) ;
	  }
	  else check(root,DSet[i], probDist[i]) ;

	  trueClass[i] = DiscData(DSet[i],0) ;

      // probability distribution output
	  if (probabilityFile != NULL) {
		fprintf(probabilityFile,"%d", DSet[i]+1) ;
	    for (int j=1 ; j<=noClasses ; j++)
	        fprintf(probabilityFile,", %f", probDist[i][j]) ;
	    fprintf(probabilityFile,"\n") ;
	  }
   }
   // evaluate results
   marray<double> priorClProb(noClasses+1, 0) ;
   for (int c=1 ; c <=noClasses ; c++)
	   priorClProb[c] = AttrDesc[0].valueProbability[c];
   modelEval(SetSize, trueClass, probDist, noClasses, priorClProb, CostMatrix, Accuracy, avgCost, Inf, Auc,
		   PredictionMatrix, kappa, sensitivity, specificity, brier, precision, Gmean, KS, TPR, FPR) ;
}





//************************************************************
//
//                      check
//                      -----
//
//        computes classification for single case
//
//************************************************************
void featureTree::check(binnode *branch, int caseIdx, marray<double> &probDist)
{
   double contValue = NAcont;
   int discValue = NAdisc;
   int i ;
   switch (branch->Identification)
   {
           case leaf:
              branch->Model.predict(branch, caseIdx, probDist) ;
              return ;
           case continuousAttribute:
                contValue = branch->Construct.continuousValue(*dData, *nData, caseIdx) ;
                break ;
           case discreteAttribute:
                discValue = branch->Construct.discreteValue(*dData, *nData, caseIdx) ;
                break ;
           default:
                merror("featureTree::check", "invalid branch identification") ;
   }
   if ((branch->Identification == continuousAttribute && isNAcont(contValue)) ||
       (branch->Identification == discreteAttribute  && discValue == NAdisc) )
   {   // missing value

       marray<double> leftTable(probDist.len()) ;
       marray<double> rightTable(probDist.len()) ;

       check(branch->left, caseIdx, leftTable) ;
       check(branch->right, caseIdx, rightTable);

       for (i = 1; i < probDist.len() ; i++)
          probDist[i] = (leftTable[i] + rightTable[i])/2.0  ;
   }
   else
     if ((branch->Identification == continuousAttribute && (contValue <= branch->Construct.splitValue)) // || fabs(contValue - branch->Construct.splitValue) < epsilon))
    		 ||(branch->Identification == discreteAttribute &&  branch->Construct.leftValues[discValue]))
         // going left
        check(branch->left, caseIdx, probDist) ;
      else // going right
        check(branch->right, caseIdx,probDist) ;
}


//************************************************************
//
//                      printResultsHead
//                      ----------------
//
//              prints head of results table
//
//************************************************************
void featureTree::printResultsHead(FILE *to) const
{
   fprintf(to,"\n%3s %5s %5s %5s %8s %5s %5s %5s %5s ",
       "idx", "#leaf","dFree","accur","cost","infSc","AUC","Brier","kappa") ;
   if (noClasses==2)
      fprintf(to,"%5s %5s","SenTe","SpeTe") ;
   fprintf(to,  "\n") ;
   printLine(to,"-",70) ;
}


//************************************************************
//
//                      printResultLine
//                      ---------------
//
//        prints results for one tree into a single line
//
//************************************************************
void featureTree::printResultLine(FILE *to, int idx,
        int Leaves, int freedom,
        double TestAccuracy, double TestCost, double TestInf, double TestAuc, double TestSens, double TestSpec, double TestBrier, double TestKappa) const
{
    char idxStr[32] ;
    if (idx>=0) snprintf(idxStr, 32, "%3d",idx);
    else if (idx == -1) strcpy(idxStr,"avg") ;
    else if (idx == -2) strcpy(idxStr,"std") ;
    else strcpy(idxStr,"???") ;

   fprintf(to,"%3s %5d %5d %5.3f %8.3f %5.3f %5.3f %5.3f %5.3f ",
                           idxStr, Leaves,  freedom,
                                  TestAccuracy,  TestCost, TestInf, TestAuc, TestBrier, TestKappa) ;
   if (noClasses==2)
	  fprintf(to,"%5.3f %5.3f", TestSens, TestSpec) ;
   fprintf(to,"\n") ;

}



//************************************************************
//
//                    printResultSummary
//                    ---------------
//
//           prints the report about domain testing
//              with current parameters on a file
//
//************************************************************
void featureTree::printResultSummary(FILE *to,
        marray<int> &Leaves, marray<int> &freedom,
        marray<double> &TestAccuracy, marray<double> &TestCost, marray<double> &TestInf, marray<double> &TestAuc,
		marray<double> &TestSens, marray<double> &TestSpec, marray<double> &TestBrier, marray<double> &TestKappa) const
{
   double avgL, stdL, avgF, stdF,
       avgAtest, stdAtest, avgCtest, stdCtest, avgItest, stdItest,avgUtest, stdUtest,
	   avgSensTest, stdSensTest, avgSpecTest, stdSpecTest, avgBrierTest, stdBrierTest, avgKaTest, stdKaTest  ;

   AvgStd(Leaves, opt->numberOfSplits, avgL, stdL) ;
   AvgStd(freedom, opt->numberOfSplits, avgF, stdF) ;
   AvgStd(TestAccuracy, opt->numberOfSplits, avgAtest, stdAtest) ;
   AvgStd(TestCost, opt->numberOfSplits, avgCtest, stdCtest) ;
   AvgStd(TestInf, opt->numberOfSplits, avgItest, stdItest) ;
   AvgStd(TestAuc, opt->numberOfSplits, avgUtest, stdUtest) ;
   AvgStd(TestSens, opt->numberOfSplits, avgSensTest, stdSensTest) ;
   AvgStd(TestSpec, opt->numberOfSplits, avgSpecTest, stdSpecTest) ;
   AvgStd(TestBrier, opt->numberOfSplits, avgBrierTest, stdBrierTest) ;
   AvgStd(TestKappa, opt->numberOfSplits, avgKaTest, stdKaTest) ;


   printLine(to,"-", 70) ;
   printResultLine(to, -1, int(avgL+0.5), int(avgF+0.5),
              avgAtest, avgCtest, avgItest, avgUtest, avgSensTest, avgSpecTest,avgBrierTest, avgKaTest) ;
   printResultLine(to, -2, int(stdL+0.5), int(stdF+0.5),
              stdAtest, stdCtest, stdItest, stdUtest, stdSensTest, stdSpecTest, stdBrierTest, stdKaTest) ;
}




// ************************************************************
//
//                      Feature2Str
//                      -----------
//
//        converts feature (a node) to a description string
//
// ************************************************************
void featureTree::Feature2Str(binnode *Node, char* const Str)
{
   Node->Construct.descriptionString(Str) ;
}

