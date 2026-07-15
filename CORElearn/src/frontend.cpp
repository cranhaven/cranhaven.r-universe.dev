/********************************************************************
*
*      Project:  CORElearn - a system for machine learning and predictive analytics
*
*      Authors:    Marko Robnik-Sikonja, Petr Savicky
*      Date:    October 2007 -
*      Based on code form older systems CORE, DIRE, and CORElearn from 1993-
*
*
*********************************************************************/

/********************************************************************
*
*   Name:      Command window frontend
*
*   Description:    provides command-line as well as textual menu access to
*                   most of the functionality
*
*********************************************************************/

#include <cstring>
#include <ctime>

#include "general.h"  // general constants and data type definitions
                      // here you specify weather to compile for
                      // Windows or UNIX
#if defined(DEBUG)
#if defined(MICROSOFT)
#include <cmalloc>  // for heapcheck and coreleft
#endif
#endif

#if defined(DEBUG_NEW)
  extern int SetSize ;
#endif


#include "error.h"    // joint method of reporting errors
#include "menu.h"     // functions for menu handling
#include "dataStore.h"  // frame for decision trees
#include "ftree.h"    // decision tree with feature construction
#include "regtree.h"  //regression trees
#include "rndforest.h"  // random forests
#include "utils.h"    // various utilities e.g., computing of standard deviation
#include "frontend.h"      // header for this file
#include "estimator.h"    // attribute evaluation in classification
#include "estimatorReg.h"     // attribute evaluation in regression
#include "options.h"   // parameters and program options
#include "printUtil.h"  // utilities for textual display of results
#include "Rfront.h"     // front end for R interface

using namespace std ;

extern int NoEstimators ;
extern int NoEstimatorsReg ;

char VersionString[]="CORElearn, "
#if defined(MICROSOFT)
"Windows"
#else
"Linux"
#endif
#if defined(R_PORT)
" R"
#else
" standalone"
#endif
" version 1.57.3, built on " __DATE__ " at " __TIME__
#if defined(_OPENMP)
" with OpenMP support"
#endif
#if defined(DEBUG)
" (debug mode)"
#endif
;

//**********************************************************************
//
//                      outVersion
//                      ----------
//
//                prints version information
//
//**********************************************************************
void outVersion(FILE *fout)
{
    fprintf(fout,"%s\n",VersionString) ;
}


#if !defined(R_PORT)
#include <cstdio>

int main(int argc, char *argv[]) {
    outVersion(stdout) ;

    featureTree *gFT = new featureTree ;
	booleanT exitImmediately=mFALSE;

	if (argc == 1) {
		printf("Use option \"h\" or \"help\" to print a usage summary, including a batch mode.\n");
		mainMenu(gFT) ;
	}
	else {
		if ((strcmp(argv[1], "help")==0) || (strcmp(argv[1], "h") ==0) || (strcmp(argv[1], "HELP")==0) || (strcmp(argv[1], "H")==0)) {
			exitImmediately = mTRUE;
	    }
		if (exitImmediately) {
			fprintf(stderr, "\n");
            fprintf(stderr,"Usage:  %s # runs in an interactive mode\n", argv[0]) ;
			fprintf(stderr, "       %s o=optionFile a=menu # runs an in interactive mode, first reads the configuration file \n", argv[0]) ;
			fprintf(stderr, "       %s o=optionFile a=action [keyword=value ...] # the batch mode executing action,\n", argv[0]) ;
			fprintf(stderr, "          # where the action can be any of the\n") ;
			fprintf(stderr, "          #   {none, est, estAll, estReg, estRegAll, tree, treeAll, treeReg, treeRegAll, rf, rfAll, data, dataReg, ordEval, ordEvalNorm}\n") ;
			fprintf(stderr, "          # and keywords are the same as in the configuration file\n") ;
			fprintf(stderr, "          # options specified on a command line override previous ones, including options in the file\n") ;
			fprintf(stderr, "       %s h # prints this message \n", argv[0]) ;
            exit(1) ;
		}

		// now process command line options
		for (int i=1 ; i < argc ; i++)
 		    gFT->opt->assignOption(argv[i]) ;

		if (gFT->opt->action == "none" || gFT->opt->action == "menu")
			mainMenu(gFT) ;
		else if (gFT->opt->action == "est")
    	    singleEstimation(gFT) ;
	    else if (gFT->opt->action == "estAll")
            allSplitsEstimation(gFT) ;
		else if (gFT->opt->action == "estReg")
    	    singleEstimationReg(gFT) ;
	    else if (gFT->opt->action == "estRegAll")
            allSplitsEstimationReg(gFT) ;
		else if (gFT->opt->action == "tree")
            singleTree(gFT) ;
        else if (gFT->opt->action == "treeAll")
            allSplitsTree(gFT) ;
		else if (gFT->opt->action == "regTree")
            singleTreeReg(gFT) ;
        else if (gFT->opt->action == "regTreeAll")
            allTreeReg(gFT) ;
		else if (gFT->opt->action == "rf")
            singleRF(gFT) ;
		else if (gFT->opt->action == "rfAll")
            allSplitsRF(gFT) ;
		else if (gFT->opt->action == "data")
            domainCharacteristics(gFT) ;
		else if (gFT->opt->action == "dataReg")
            domainCharacteristicsReg(gFT) ;
		else if (gFT->opt->action == "ordEval")
			evalOrdAttrValNorm(gFT, ordEval) ;
		else if (gFT->opt->action == "ordEvalNorm")
			evalOrdAttrValNorm(gFT, ordEvalNorm) ;
		else if (gFT->opt->action == "ordEvalNormClDiff1")
			evalOrdAttrValNorm(gFT, ordEvalNormClDiff1) ;
		else if (gFT->opt->action == "ordEvalNormAttrDiff1")
			evalOrdAttrValNorm(gFT, ordEvalNormAttrDiff1) ;
		else if (gFT->opt->action == "ordEvalInst")
			runOrdEvalInst(gFT) ;
		//else if (gFT->opt->action == "ordClassEvalNorm")
		//	evalOrdClassNorm(gFT) ;
		else if (gFT->opt->action == "rfSave")
            saveSingleRF(gFT) ;
		//else if (gFT->opt->action == "rfSaveLarge")
        //    saveLargeRF(gFT) ;
		else if (gFT->opt->action == "rfLoad")
            loadSingleRF(gFT) ;
		else if (gFT->opt->action == "simRcall")
            simRcall() ;

		else
			merror("Unrecognized action:",gFT->opt->action.getConstValue()) ;
	}
    fflush(stdout) ;
    delete gFT ;

#if defined(DEBUG)
#if defined(MICROSOFT)
   /* Check heap status */
   int heapstatus = _heapchk();
   if (heapstatus!= _HEAPOK)
       printf("\nWARNING: Heap is not OK !!") ;
   // _HEAPOK, _HEAPEMPTY, _HEAPBADBEGIN, _HEAPBADNODE
#endif
#endif

#if defined(DEBUG_NEW)
   printf("Still allocated memory blocks: %ld\n",SetSize) ;
#endif
    return 0 ;
}


void mainMenu(featureTree* gFT) {
       char const* MainMenu[] = { "Estimate attributes on a single split in classification" ,
                            "Estimate attributes on all splits in classification" ,
                            "Train a decision trees on a single data split in classification",
                            "Train decision trees on all data splits in classification",
                            "Train a random forest on a single data split in classification",
                            "Train random forests on all data splits in classification",
                            "Summarize data characteristics in classification",
                            "Estimate attributes on a single split in regression" ,
                            "Estimate attributes on all splits in regression" ,
                            "Train a regression tree on a single data split in regression",
                            "Train regression trees on all data splits in regression",
                            "Summarize data characteristics in regression",
                            "Options" ,
                            "Load parameters",
                            "Save parameters",
                            "Exit"

	   } ;
       int choice ;
       do
       {
    	 printf("\n\n Current domain: ") ;
		 if (gFT->opt->domainName[0])
           printf("%s\n", gFT->opt->domainName.getConstValue()) ;
         else
           printf("<none>\n") ;
         fflush(stdout) ;
         char tempName[MaxPath] ;
         switch (choice=textMenu("Choose the number:", MainMenu,16))  {

             // attribute estimation on single data split
             case 1 : singleEstimation(gFT) ;
                      break ;

             // attribute estimation on all data splits
			 case 2 : allSplitsEstimation(gFT);
                      break ;

             // learning tree on single data split
             case 3 : singleTree(gFT) ;
                      break ;

             // learning tree on all data split
             case 4 : allSplitsTree(gFT);
                      break ;

             // learning random forst on single data split
             case 5 : singleRF(gFT) ;
                      break ;

             // learning random forests on all data split
             case 6 : allSplitsRF(gFT);
                      break ;

             // data characteristics
 			 case 7: domainCharacteristics(gFT) ;
				     break ;

             // attribute estimation on single data split
             case 8 : singleEstimationReg(gFT) ;
                      break ;

             // attribute estimation on all data splits
			 case 9 : allSplitsEstimationReg(gFT);
                      break ;

             // learning tree on single data split
             case 10 : singleTreeReg(gFT) ;
                      break ;

             // learning tree on all data split
             case 11 : allTreeReg(gFT);
                      break ;

             // data characteristics
 			 case 12: domainCharacteristicsReg(gFT) ;
				     break ;
             // Options menu
             case 13 : gFT->opt->processOptions() ;
                      break ;

             // Load parameters
             case 14: printf("\nConfiguration file name: ") ;
                      fflush(stdout) ;
                      scanf("%s",tempName) ;
                      printf("\nReading configuration file %s . . .", tempName ) ;
					  fflush(stdout) ;
                      gFT->opt->readConfig(tempName) ;
                      printf(" done.") ;
					  fflush(stdout) ;
                      break ;

             // Save parameters
             case 15: printf("\nConfiguration file name: ") ;
                      fflush(stdout) ;
                      scanf("%s", tempName) ;
                      printf("\nWritting configuration file %s . . .", tempName );
					  fflush(stdout) ;
                      gFT->opt->writeConfig(tempName) ;
                      printf(" done.") ;
					  fflush(stdout) ;
                      break ;

             // Exit
             case 16: break ;

             default: merror("Non existing menu option.","") ;
         }
       }  while (choice > 0 && choice != 16) ;
}

//**********************************************************************
//
//                      singleEstimation
//                      ----------
//
//      dealing with single split estimation
//
//**********************************************************************
void singleEstimation(featureTree* const Tree){
	if (!Tree->readProblem(mTRUE, mTRUE))
      return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   Tree->setDataSplit(Tree->opt->splitIdx) ;

   marray<double> weight(Tree->NoTrainCases,1.0) ;
   estimation Estimator(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;

   marray<marray<double> > Result(NoEstimators+1) ;
   int estIdx ;
   for (estIdx = 1 ; estIdx <= NoEstimators ; estIdx++)
      if (Tree->opt->estOn[estIdx])
         Result[estIdx].create(Tree->noAttr+1, 0.0) ;

   printEstimationHead(stdout, Tree) ;

   int i;
   attributeCount attrType ;
   double estStart = timeMeasure() ;

   for (estIdx = 1 ; estIdx <= NoEstimators ; estIdx++)
      if (Tree->opt->estOn[estIdx])
      {
         Estimator.estimate(estIdx, 0,Tree->noNumeric,1,Tree->noDiscrete, attrType) ;

         for (i=1 ; i <= Tree->noAttr; i++)
           if (Tree->AttrDesc[i].continuous)
             Result[estIdx][i] = Estimator.NumEstimation[Tree->AttrDesc[i].tablePlace] ;
           else
             Result[estIdx][i] =  Estimator.DiscEstimation[Tree->AttrDesc[i].tablePlace] ;
      }

   double estEnd = timeMeasure() ;

   FILE *fout ;
   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.%02dest", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue(), Tree->opt->splitIdx) ;
   if ((fout = fopen(path,"w"))==NULL)
   {
      merror("singleEstimation: cannot open results file: ", path)  ;
   }

   if (fout) {
	   outVersion(fout);
	   printEstimationHead(fout, Tree);
	   printEstimations(fout, Tree->opt->splitIdx, Result, Tree);
	   fprintf(fout, "\nCPU time used: %.2f seconds\n", timeMeasureDiff(estStart, estEnd));
	   printLine(fout, "-", 23 + 11 * Tree->noAttr);
	   fclose(fout);

   }

   printEstimations(stdout, Tree->opt->splitIdx, Result, Tree);
   fprintf(stdout, "\nCPU time used: %.2f seconds\n", timeMeasureDiff(estStart, estEnd));
   printLine(stdout, "-", 23 + 11 * Tree->noAttr);

}

//**********************************************************************
//
//                      singleEStimationReg
//                      ----------
//
//      dealing wih single split estimationReg
//
//**********************************************************************
void singleEstimationReg(featureTree* const FTree)
{
   regressionTree *Tree = new regressionTree ;
   Tree->opt = FTree->opt ;
   if (!Tree->readProblem(mTRUE, mTRUE)) {
	   Tree->opt = 0 ;
	   delete Tree ;
       return ;
   }
   Tree->setDataSplit(Tree->opt->splitIdx) ;
   if (!Tree->isRegression) {
	   merror("Provided data is not regressional", "") ;
	   Tree->opt = 0 ;
	   delete Tree ;
	   return ;
   }

   marray<double> weight(Tree->NoTrainCases,1.0) ;
   estimationReg Estimator(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;

   // print immediately to the screen, later to the file, so that existence of the file is an indication that process is (nearly) finished
   printEstimationHead(stdout, Tree) ;

   marray<marray<double> > Result(NoEstimatorsReg+1) ;
   int estIdx ;
   for (estIdx = 1 ; estIdx <= NoEstimatorsReg ; estIdx++)
      if (Tree->opt->estOnReg[estIdx])
         Result[estIdx].create(Tree->noAttr+1, 0.0) ;


   int i ;
   attributeCount attrType ;
   double estStart = timeMeasure() ;
   double estSign  ;

   for (estIdx = 1 ; estIdx <= NoEstimatorsReg ; estIdx++)
   {
      if (estIdx == estMSEofMean  || estIdx == estMSEofModel || estIdx ==  estMAEofModel)
         estSign = -1.0 ;
      else
         estSign = 1.0 ;

      if (Tree->opt->estOnReg[estIdx])
      {
         Estimator.estimate(estIdx, 1,Tree->noNumeric,0,Tree->noDiscrete, attrType) ;

         for (i=1 ; i <= Tree->noAttr; i++)
           if (Tree->AttrDesc[i].continuous)
             Result[estIdx][i] = estSign * Estimator.NumEstimation[Tree->AttrDesc[i].tablePlace] ;
           else
             Result[estIdx][i] =  estSign * Estimator.DiscEstimation[Tree->AttrDesc[i].tablePlace] ;
      }
   }
   double estEnd = timeMeasure() ;

   // write results to the file
   FILE *fout ;
   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.%02de",Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue(), Tree->opt->splitIdx) ;
   if ((fout = fopen(path,"w"))==NULL)
   {
      merror("singleEstimationReg: cannot open results file: ", path)  ;
   }
   
   if (fout) {
	   outVersion(fout);
	   printEstimationHead(fout, Tree);
	   printEstimations(fout, Tree->opt->splitIdx, Result, Tree);
	   fprintf(fout, "\nCPU time used: %f seconds\n", timeMeasureDiff(estStart, estEnd));
	   fclose(fout);
   }
	   
   printEstimations(stdout, Tree->opt->splitIdx, Result, Tree) ;
   fprintf(stdout,"\nCPU time used: %f seconds\n", timeMeasureDiff(estStart, estEnd)) ;

   Tree->opt = 0;
   delete Tree;

}



 //**********************************************************************
//
//                      allSplitsEstimation
//                      --------------------
//
//      dealing wih single split estimation
//
//**********************************************************************
void allSplitsEstimation(featureTree* const Tree)
{
  if (!Tree->readProblem(mTRUE, mTRUE))
	   return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   marray<double> weight ;
   estimation *pEstimator ;
   FILE *fout ;
   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.est", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue()) ;
   if ((fout = fopen(path,"w"))==NULL)   {
      stop("allSplitsEstimation: cannot open results file: ", path)  ;
   }

   outVersion(fout) ;
   printEstimationHead(fout, Tree) ;
   printEstimationHead(stdout, Tree) ;

   int estIdx ;
   marray<marray<double> > result(NoEstimators+1), sumResult(NoEstimators+1) ;
   for (estIdx=1 ; estIdx <= NoEstimators ; estIdx++)
     if (Tree->opt->estOn[estIdx])   {
        result[estIdx].create(Tree->noAttr+1, 0.0) ;
        sumResult[estIdx].create(Tree->noAttr+1, 0.0) ;
     }
   int i,  iter ;
   attributeCount attrType ;
   double estStart = timeMeasure() ;

   for (iter = 0 ; iter < Tree->opt->numberOfSplits ; iter++)   {
      Tree->setDataSplit(iter) ;
      weight.create(Tree->NoTrainCases,1.0) ;
      pEstimator = new estimation(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;

      for (estIdx = 1 ; estIdx <= NoEstimators ; estIdx++)
        if (Tree->opt->estOn[estIdx])  {
           pEstimator->estimate(estIdx, 0,Tree->noNumeric,1,Tree->noDiscrete, attrType) ;

           for (i=1 ; i <= Tree->noAttr; i++)  {
             if (Tree->AttrDesc[i].continuous)
               result[estIdx][i] = pEstimator->NumEstimation[Tree->AttrDesc[i].tablePlace] ;
             else
               result[estIdx][i] =  pEstimator->DiscEstimation[Tree->AttrDesc[i].tablePlace] ;

             sumResult[estIdx][i] += result[estIdx][i] ;
           }
        }

      printEstimations(fout,   iter, result, Tree) ;
      printEstimations(stdout, iter, result, Tree) ;

      fflush(fout) ;
      fflush(stdout) ;

      delete pEstimator ;
    }

    double estEnd = timeMeasure() ;

   for (i=1 ; i <= Tree->noAttr; i++)   {
      for (estIdx = 1 ; estIdx <= NoEstimators ; estIdx++)
        if (Tree->opt->estOn[estIdx])
           sumResult[estIdx][i] /= double(Tree->opt->numberOfSplits) ;
   }
   printLine(fout,"-",23+11 * Tree->noAttr)  ;
   printLine(stdout,"-",23+ 11 * Tree->noAttr)  ;
   printEstimations(fout,   -1, sumResult, Tree) ;
   printEstimations(stdout, -1, sumResult, Tree);

   printEstimationsInColumns(fout,   -1, sumResult, Tree) ;
   printEstimationsInColumns(stdout, -1, sumResult, Tree) ;

   fprintf(fout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(estStart, estEnd)) ;
   fprintf(stdout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(estStart, estEnd)) ;

   fflush(stdout) ;
   fclose(fout) ;
}

//**********************************************************************
//
//                      allSplitsEstimationReg
//                      --------------------
//
//      estimations on all splits in regression
//
//**********************************************************************
void allSplitsEstimationReg(const featureTree *FTree)
{
  regressionTree *Tree = new regressionTree ;
   Tree->opt = FTree->opt ;
   if (!Tree->readProblem(mTRUE, mTRUE)) {
	   Tree->opt = 0 ;
	   delete Tree ;
       return ;
   }
   if (!Tree->isRegression) {
	   merror("Provided data is not regressional", "") ;
	   Tree->opt = 0 ;
	   delete Tree ;
	   return ;
   }

   marray<double> weight ;
   estimationReg *pEstimator ;
   FILE *fout ;
   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.est",Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue()) ;
   if ((fout = fopen(path,"w"))==NULL)
   {
      stop("allSplitsEstimationReg: cannot open results file: ", path)  ;
   }

   Tree->opt->outConfig(fout) ;
   Tree->opt->outConfig(stdout) ;
   Tree->outDomainSummary(fout) ;
   Tree->outDomainSummary(stdout) ;

   printEstimationHead(fout, Tree) ;
   printEstimationHead(stdout, Tree) ;

   int estIdx ;
   marray<marray<double> > result(NoEstimatorsReg+1), sumResult(NoEstimatorsReg+1) ;
   for (estIdx=1 ; estIdx <= NoEstimatorsReg ; estIdx++)
     if (Tree->opt->estOnReg[estIdx])
     {
        result[estIdx].create(Tree->noAttr+1, 0.0) ;
        sumResult[estIdx].create(Tree->noAttr+1, 0.0) ;
     }

   int i,  iter ;
   attributeCount attrType ;
   double estSign ;
   double estStart = timeMeasure() ;
   for (iter = 0 ; iter < Tree->opt->numberOfSplits ; iter++)
   {
      Tree->setDataSplit(iter) ;
      weight.create(Tree->NoTrainCases,1.0) ;
      pEstimator = new estimationReg(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;

      for (estIdx = 1 ; estIdx <= NoEstimatorsReg ; estIdx++)
      {
         if (estIdx == estMSEofMean  || estIdx == estMSEofModel || estIdx ==  estMAEofModel)
           estSign = -1.0 ;
         else
           estSign = 1.0 ;
         if (Tree->opt->estOnReg[estIdx])
         {
           pEstimator->estimate(estIdx, 1,Tree->noNumeric,0,Tree->noDiscrete, attrType) ;

           for (i=1 ; i <= Tree->noAttr; i++)
           {
             if (Tree->AttrDesc[i].continuous)
               result[estIdx][i] = estSign * pEstimator->NumEstimation[Tree->AttrDesc[i].tablePlace] ;
             else
               result[estIdx][i] =  estSign * pEstimator->DiscEstimation[Tree->AttrDesc[i].tablePlace] ;

             sumResult[estIdx][i] += result[estIdx][i] ;
           }
        }
      }
      printEstimations(fout,   iter, result, Tree) ;
      printEstimations(stdout, iter, result, Tree) ;

      fflush(fout) ;
      fflush(stdout) ;

      delete pEstimator ;
    }

   double estEnd = timeMeasure() ;

   for (i=1 ; i <= Tree->noAttr; i++)
   {
      for (estIdx = 1 ; estIdx <= NoEstimatorsReg ; estIdx++)
        if (Tree->opt->estOnReg[estIdx])
           sumResult[estIdx][i] /= double(Tree->opt->numberOfSplits) ;
   }
   printLine(fout, "-", Tree->noAttr * 11 + 24) ;
   printLine(stdout, "-", Tree->noAttr * 11 + 24) ;

   printEstimations(fout,   -1, sumResult, Tree) ;
   printEstimations(stdout, -1, sumResult, Tree);

   printEstimationsInColumns(fout,  -1, sumResult, Tree) ;
   printEstimationsInColumns(stdout, -1, sumResult, Tree) ;

   fprintf(fout,"\nCPU time used: %f seconds\n", timeMeasureDiff(estStart, estEnd)) ;
   fprintf(stdout,"\nCPU time used: %f seconds\n", timeMeasureDiff(estStart, estEnd)) ;
   fflush(stdout) ;
   fclose(fout) ;

   Tree->opt = 0 ;
   delete Tree ;
}



//**********************************************************************
//
//                      singleTree
//                      ----------
//
//      dealing with single tree construction and testing
//
//**********************************************************************
void singleTree(featureTree* const Tree) {
   if (!Tree->readProblem(mTRUE, mTRUE))
      return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   double buildStart = timeMeasure() ;
   Tree->learnRF = mFALSE ;
   Tree->setDataSplit(Tree->opt->splitIdx) ;
   if (Tree->constructTree())   {
       Tree->printResultsHead(stdout) ;
       // only after pruning prediction matrix is needed
       int PMxSize = Tree->noClasses+1 ;
       mmatrix<int> PMx(PMxSize,PMxSize) ;
       int Leaves = Tree->noLeaves() ;
       int freedom = Tree->degreesOfFreedom() ;
       double Accuracy, Kappa, Inf, Cost, Auc, Brier, Sens, Spec, precision, Gmean, KS, TPR, FPR ;
	   FILE *distrFile = prepareDistrFile(Tree->opt->splitIdx, Tree->opt) ;
 	   if (distrFile != NULL) {
	  	 fprintf(distrFile, "# Training instances \n") ;
         Tree->test(Tree->DTraining, Tree->NoTrainCases, Accuracy, Cost, Inf, Auc, PMx, Kappa, Sens, Spec, Brier, precision, Gmean, KS, TPR, FPR, distrFile);
 	   }
 	   if (distrFile != NULL)
	 	 fprintf(distrFile, "# Testing instances \n") ;
       Tree->test(Tree->DTesting, Tree->NoTestCases, Accuracy, Cost, Inf, Auc, PMx, Kappa, Sens, Spec, Brier, precision, Gmean, KS, TPR, FPR, distrFile) ;
       if (distrFile != NULL)
          fclose(distrFile) ;

       double buildEnd = timeMeasure() ;
       Tree->printResultLine(stdout, Tree->opt->splitIdx, Leaves, freedom,
                            Accuracy, Cost, Inf, Auc, Sens, Spec, Brier, Kappa) ;
       fflush(stdout) ;

       char OutName[MaxFileNameLen] ;
       snprintf(OutName, MaxFileNameLen, "%s%s.%02dtree", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue(), Tree->opt->splitIdx) ;
       Tree->printFTreeFile(OutName, Tree->opt->splitIdx,Leaves, freedom, Accuracy, Cost, Inf, Auc,PMx, Sens, Spec, Brier, Kappa) ;
       fprintf(stdout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;
       fflush(stdout) ;
   }
}

//**********************************************************************
//
//                      singleTreeReg
//                      ----------
//
//      single tree construction and testing
//
//**********************************************************************
void singleTreeReg(featureTree* const FTree)
{
   regressionTree *Tree = new regressionTree ;
   Tree->opt = FTree->opt ;
   if (!Tree->readProblem(mTRUE, mTRUE)) {
	   Tree->opt = 0 ;
	   delete Tree ;
       return ;
   }
   if (!Tree->isRegression) {
	   merror("Provided data is not regressional", "") ;
	   Tree->opt = 0 ;
	   delete Tree ;
	   return ;
   }

   Tree->setDataSplit(Tree->opt->splitIdx) ;

   double buildStart = timeMeasure() ;
   Tree->printResultsHead(stdout) ;

   if (Tree->constructRegTree())
   {
       int Leaves = Tree->noLeaves() ;
       int freedom = Tree->degreesOfFreedom() ;
       double TestSE, TestRSE ;
       double TestAE, TestRAE ;
       Tree->test(Tree->DTesting, Tree->NoTestCases, TestSE, TestRSE,
                  TestAE, TestRAE, NULL ) ;

       Tree->printResultLine(stdout,Tree->opt->splitIdx,
                            Leaves, freedom, TestSE, TestRSE, TestAE, TestRAE) ;
       double buildEnd = timeMeasure() ;
       fprintf(stdout,"\nCPU time used: %f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;
       fflush(stdout) ;

       char OutName[MaxFileNameLen] ;
       snprintf(OutName, MaxFileNameLen, "%s%s.%02dt", Tree->opt->resultsDirectory.getConstValue(),
    		         Tree->opt->domainName.getConstValue(), Tree->opt->splitIdx ) ;
       Tree->printFTreeFile(OutName, 0, Leaves, freedom, TestSE, TestRSE, TestAE, TestRAE) ;
   }
   Tree->opt = 0 ;
   delete Tree ;
}

//**********************************************************************
//
//                      allSplitsTree
//                      -------------
//
//          constructs and tests the whole domain
//
//**********************************************************************
void allSplitsTree(featureTree* const Tree) {
   if (!Tree->readProblem(mTRUE, mTRUE))
	   return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   Tree->learnRF = mFALSE ;

   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.treeResults", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue());
   FILE *to, *distrFile ;
   if ((to=fopen(path,"w"))==NULL)
   {
       stop("Cannot open decision tree output file",path) ;
	   return;
   }

   outVersion(to) ;
   fprintf(to,"Parameters:\n" ) ;
   fprintf(to,"-----------\n" ) ;
   fprintf(stdout,"Parameters:\n" ) ;
   fprintf(stdout,"-----------\n" ) ;
   Tree->opt->outConfig(to) ;
   Tree->opt->outConfig(stdout) ;
   Tree->outDomainSummary(to) ;
   Tree->outDomainSummary(stdout) ;
   Tree->printResultsHead(to) ;
   Tree->printResultsHead(stdout) ;
   fflush(to) ;
   fflush(stdout) ;
   marray<double> Accuracy(Tree->opt->numberOfSplits) ;
   marray<double> Inf(Tree->opt->numberOfSplits) ;
   marray<double> Cost(Tree->opt->numberOfSplits) ;
   marray<double> Auc(Tree->opt->numberOfSplits) ;
   marray<double> Sens(Tree->opt->numberOfSplits) ;
   marray<double> Spec(Tree->opt->numberOfSplits) ;
   marray<double> Brier(Tree->opt->numberOfSplits) ;
   marray<double> Kappa(Tree->opt->numberOfSplits) ;
   marray<int> Leaves(Tree->opt->numberOfSplits) ;
   marray<int> freedom(Tree->opt->numberOfSplits) ;
   marray<double> precision(Tree->opt->numberOfSplits) ;
   marray<double> Gmean(Tree->opt->numberOfSplits) ;
   marray<double> KS(Tree->opt->numberOfSplits) ;
   marray<double> TPR(Tree->opt->numberOfSplits) ;
   marray<double> FPR(Tree->opt->numberOfSplits) ;

   int sizePMx = Tree->noClasses +1;
   mmatrix<int> PMx(sizePMx, sizePMx) ;

   double buildStart = timeMeasure() ;
   for (int i = 0 ; i < Tree->opt->numberOfSplits ; i++)
   {
      Tree->setDataSplit(i) ;

      if (Tree->constructTree())
      {
		 distrFile = prepareDistrFile(i, Tree->opt) ;

         Leaves[i] = Tree->noLeaves() ;
         freedom[i] = Tree->degreesOfFreedom() ;

		 if (distrFile != NULL) {
			fprintf(distrFile, "# Training instances \n") ;
            Tree->test(Tree->DTraining, Tree->NoTrainCases,  Accuracy[i], Cost[i], Inf[i], Auc[i], PMx, Kappa[i], Sens[i], Spec[i], Brier[i], precision[i], Gmean[i], KS[i], TPR[i], FPR[i],  distrFile) ;
		 }
		 if (distrFile != NULL)
			fprintf(distrFile, "# ing instances \n") ;
         Tree->test(Tree->DTesting, Tree->NoTestCases, Accuracy[i], Cost[i], Inf[i], Auc[i], PMx, Kappa[i], Sens[i], Spec[i], Brier[i], precision[i], Gmean[i], KS[i], TPR[i], FPR[i], distrFile );

         if (distrFile != NULL)
            fclose(distrFile) ;

         snprintf(path, MaxPath, "%s%s.%02dtree", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue(), i) ;
         Tree->printFTreeFile(path,i, Leaves[i], freedom[i], Accuracy[i], Cost[i], Inf[i],Auc[i],PMx, Sens[i], Spec[i], Brier[i], Kappa[i]) ;

         Tree->printResultLine(to, i,  Leaves[i], freedom[i],Accuracy[i], Cost[i], Inf[i], Auc[i], Sens[i], Spec[i], Brier[i], Kappa[i]) ;

         Tree->printResultLine(stdout, i, Leaves[i], freedom[i],Accuracy[i], Cost[i], Inf[i], Auc[i], Sens[i], Spec[i], Brier[i], Kappa[i]) ;
         fflush(to) ;
         fflush(stdout) ;
      }
   }
   double buildEnd = timeMeasure() ;

   Tree->printResultSummary(to, Leaves, freedom, Accuracy, Cost, Inf, Auc, Sens, Spec, Brier, Kappa) ;
   Tree->printResultSummary(stdout, Leaves, freedom, Accuracy, Cost, Inf, Auc, Sens, Spec, Brier, Kappa) ;
   fprintf(to,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;
   fprintf(stdout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;

   fclose(to) ;
   fflush(stdout) ;
}

// **********************************************************************
//
//                      allTreeReg
//                      -------------
//
//          constructs and tests the whole domain
//
// **********************************************************************
void allTreeReg(featureTree* const FTree, demandType demand)
{
   regressionTree *Tree = new regressionTree ;
   Tree->opt = FTree->opt ;
   if (!Tree->readProblem(mTRUE, mTRUE)) {
	   Tree->opt = 0 ;
	   delete Tree ;
       return ;
   }
   if (!Tree->isRegression) {
	   merror("Provided data is not regressional", "") ;
	   Tree->opt = 0 ;
	   delete Tree ;
	   return ;
   }

   char path[MaxPath], residPath[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.res", Tree->opt->resultsDirectory.getConstValue(),Tree->opt->domainName.getConstValue());
   FILE *to, *residFile ;
   if ((to=fopen(path,"w"))==NULL)
   {
       stop("Cannot open regression tree output file",path) ;
	   return;
   }

   fprintf(to,"Parameters:\n" ) ;
   fprintf(to,"-----------\n" ) ;
   fprintf(stdout,"Parameters:\n" ) ;
   fprintf(stdout,"-----------\n" ) ;
   Tree->opt->outConfig(to) ;
   Tree->opt->outConfig(stdout) ;
   Tree->outDomainSummary(to) ;
   Tree->outDomainSummary(stdout) ;
   Tree->printResultsHead(to) ;
   Tree->printResultsHead(stdout) ;
   fflush(to) ;
   fflush(stdout) ;
   marray<double> TestSEafter(Tree->opt->numberOfSplits) ;
   marray<double> TestRSEafter(Tree->opt->numberOfSplits) ;
   marray<double> TestAEafter(Tree->opt->numberOfSplits) ;
   marray<double> TestRAEafter(Tree->opt->numberOfSplits) ;
   marray<int> LeavesAfter(Tree->opt->numberOfSplits) ;
   marray<int> freedomAfter(Tree->opt->numberOfSplits) ;

   double buildStart = timeMeasure() ;

   for (int i = 0 ; i < Tree->opt->numberOfSplits ; i++)
   {

      Tree->setDataSplit(i) ;

      if (Tree->constructRegTree())
      {
         if (demand==residuals)
         {
            snprintf(residPath, MaxPath, "%s%s.%02dr", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue(), i) ;
            if ((residFile=fopen(residPath,"w"))==NULL)
               merror("Cannot write to residuals file", residPath) ;
         }
         else
            residFile = NULL ;

        LeavesAfter[i] = Tree->noLeaves() ;
        freedomAfter[i] = Tree->degreesOfFreedom() ;

        if (residFile != NULL)
           Tree->test(Tree->DTraining, Tree->NoTrainCases,  TestSEafter[i], TestRSEafter[i],
                     TestAEafter[i], TestRAEafter[i], residFile) ;

		Tree->test(Tree->DTesting, Tree->NoTestCases, TestSEafter[i], TestRSEafter[i],
                      TestAEafter[i], TestRAEafter[i], residFile );

        if (residFile != NULL)
            fclose(residFile) ;

        snprintf(path, MaxPath, "%s%s.%02dt", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue(),i) ;
        Tree->printFTreeFile(path,i, LeavesAfter[i], freedomAfter[i],
                               TestSEafter[i], TestRSEafter[i], TestAEafter[i], TestRAEafter[i]) ;

        Tree->printResultLine(to, i, LeavesAfter[i], freedomAfter[i],
                               TestSEafter[i], TestRSEafter[i], TestAEafter[i], TestRAEafter[i]) ;

        Tree->printResultLine(stdout, i, LeavesAfter[i], freedomAfter[i],
                               TestSEafter[i], TestRSEafter[i], TestAEafter[i], TestRAEafter[i]) ;
        fflush(to) ;
        fflush(stdout) ;
    }
   }
   double buildEnd = timeMeasure() ;

   Tree->printResultSummary(to, LeavesAfter, freedomAfter, TestSEafter, TestRSEafter, TestAEafter, TestRAEafter) ;
   Tree->printResultSummary(stdout, LeavesAfter, freedomAfter, TestSEafter, TestRSEafter, TestAEafter, TestRAEafter) ;

   fprintf(to,"\nCPU time used: %f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;
   fprintf(stdout,"\nCPU time used: %f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;

   fclose(to) ;
   fflush(stdout) ;

   Tree->opt = 0 ;
   delete Tree ;
}


//**********************************************************************
//
//                      singleRF
//                      ----------
//
//      dealing with single random forest construction and testing
//
//**********************************************************************
void singleRF(featureTree* const Tree) {
  if (!Tree->readProblem(mTRUE, mTRUE))
      return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   char path[MaxPath] ;
   FILE *to ;
   snprintf(path, MaxPath, "%s%s.rfResult", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue());
   if ((to=fopen(path,"w"))==NULL) {
       merror("Cannot open random forests report file", path) ;
   }
   if (to) {
	   outVersion(to);
	   fprintf(to, "Parameters:\n");
	   fprintf(to, "-----------\n");
	   Tree->opt->outConfig(to);
	   Tree->outDomainSummary(to);
	   Tree->rfResultHead(to);
	   fflush(to);
   }
   Tree->outDomainSummary(stdout);
   Tree->rfResultHead(stdout);
   Tree->rfResultHead(stdout);


   double buildStart = timeMeasure();
   Tree->learnRF = mTRUE ;
   Tree->setDataSplit(Tree->opt->splitIdx) ;
   randSeed(Tree->opt->rfRndSeed) ;

   if (Tree->buildForest()) {
       int PMxSize = Tree->noClasses+1 ;
       mmatrix<int> PMx(PMxSize,PMxSize) ;

	   double Accuracy, Inf, Cost, Auc, Sens, Spec, Brier, Kappa, precision, Gmean, KS, TPR, FPR ;

	   FILE *distrFile = prepareDistrFile(Tree->opt->splitIdx, Tree->opt) ;

	   //Tree->writeRF("tempRF.txt") ;
	   //Tree->readForest("tempRF.txt") ;

	   if (distrFile != NULL) {
	  	 fprintf(distrFile, "# Training instances \n") ;
	     Tree->test(Tree->DTraining, Tree->NoTrainCases, Accuracy, Cost, Inf, Auc, PMx, Kappa, Sens, Spec, Brier, precision, Gmean, KS, TPR, FPR, distrFile);
	   }
 	   if (distrFile != NULL)
	  	 fprintf(distrFile, "# testing instances \n") ;
       Tree->test(Tree->DTesting, Tree->NoTestCases, Accuracy, Cost, Inf, Auc, PMx, Kappa, Sens, Spec, Brier, precision, Gmean, KS, TPR, FPR, distrFile) ;
       if (distrFile != NULL)
          fclose(distrFile) ;


       double buildEnd = timeMeasure() ;
       Tree->rfResultLine(stdout, Tree->opt->splitIdx,
           Tree->avgOobAccuracy, Tree->avgOobMargin,Tree->avgOobCorrelation,
           Accuracy, Cost, Inf, Auc,Sens, Spec, Brier, Kappa) ;
       Tree->rfResultLine(to, Tree->opt->splitIdx,
           Tree->avgOobAccuracy, Tree->avgOobMargin,Tree->avgOobCorrelation,
           Accuracy, Cost, Inf, Auc,Sens, Spec, Brier, Kappa) ;
	   if (Tree->opt->rfAttrEvaluate) {
		  marray<marray<double> > attrEval(1) ;
		  attrEval[0].create(Tree->noAttr+1) ;
		  attrEval.setFilled(1) ;
          marray<int> idx(1,Tree->opt->splitIdx) ;
	      Tree->varImportance(attrEval[0]) ;
		  // for attribute values evaluation
		  // marray<marray<double> > avEval(Tree->noAttr+1) ;
		  // for (int iA=1 ; iA <= Tree->noAttr ; iA++)
		  //    if (Tree->AttrDesc[iA].continuous)
  		  //	     avEval[iA].create(1) ;
		  // else
		  //    avEval[iA].create(Tree->AttrDesc[iA].NoValues+1) ;
		  //Tree->avImportance(avEval) ;
		  Tree->printAttrEval(stdout,idx,attrEval) ;
		  Tree->printAttrEval(to,idx,attrEval) ;
	   }
	   if (to) {
		   fflush(to);
		   fclose(to);
	   }
	   fprintf(stdout, "\nCPU time used: %.2f seconds\n", timeMeasureDiff(buildStart, buildEnd));
	   fflush(stdout);

   }
}



//**********************************************************************
//
//                      allSplitsRF
//                      -------------
//
//          constructs and tests random forest on all splits
//
//**********************************************************************
void allSplitsRF(featureTree* const Tree) {
   if (!Tree->readProblem(mTRUE, mTRUE))
	   return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   Tree->learnRF = mTRUE ;

   char path[MaxPath] ;
   FILE *to, *distrFile ;
   snprintf(path, MaxPath, "%s%s.rfResult", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue());
   if ((to=fopen(path,"w"))==NULL) {
       stop("Cannot open random forests report file",path) ;
   }

   outVersion(to) ;
   fprintf(to,"Parameters:\n" ) ;
   fprintf(to,"-----------\n" ) ;
   fprintf(stdout,"Parameters:\n" ) ;
   fprintf(stdout,"-----------\n" ) ;
   Tree->opt->outConfig(to) ;
   Tree->opt->outConfig(stdout) ;
   Tree->outDomainSummary(to) ;
   Tree->outDomainSummary(stdout) ;
   Tree->rfResultHead(to) ;
   Tree->rfResultHead(stdout) ;
   fflush(to) ;
   fflush(stdout) ;
   marray<double> Accuracy(Tree->opt->numberOfSplits) ;
   marray<double> Inf(Tree->opt->numberOfSplits) ;
   marray<double> Cost(Tree->opt->numberOfSplits) ;
   marray<double> Auc(Tree->opt->numberOfSplits) ;
   marray<double> Sens(Tree->opt->numberOfSplits) ;
   marray<double> Spec(Tree->opt->numberOfSplits) ;
   marray<double> Brier(Tree->opt->numberOfSplits) ;
   marray<double> Kappa(Tree->opt->numberOfSplits) ;
   marray<double> oobAccuracy(Tree->opt->numberOfSplits) ;
   marray<double> oobMargin(Tree->opt->numberOfSplits) ;
   marray<double> oobCorrelation(Tree->opt->numberOfSplits) ;
   marray<double> precision(Tree->opt->numberOfSplits) ;
   marray<double> Gmean(Tree->opt->numberOfSplits) ;
   marray<double> KS(Tree->opt->numberOfSplits) ;
   marray<double> TPR(Tree->opt->numberOfSplits) ;
   marray<double> FPR(Tree->opt->numberOfSplits) ;

   marray<marray<double> > attrEval(Tree->opt->numberOfSplits+1) ;
   attrEval[0].create(Tree->noAttr+1, 0.0) ; // for averages
   attrEval.setFilled(Tree->opt->numberOfSplits+1) ;
   marray<int> idx(Tree->opt->numberOfSplits+1) ;
   idx[0] = -1 ;
   int sizePMx = Tree->noClasses +1;
   mmatrix<int> PMx(sizePMx, sizePMx) ;
   randSeed(Tree->opt->rfRndSeed) ;

   double buildStart = timeMeasure() ;
   int i ;
   for (i = 0 ; i < Tree->opt->numberOfSplits ; i++)  {
      Tree->setDataSplit(i) ;

      if (Tree->buildForest()) {
		 oobAccuracy[i] = Tree->avgOobAccuracy ;
		 oobMargin[i] = Tree->avgOobMargin ;
		 oobCorrelation[i] = Tree->avgOobCorrelation ;
 	     distrFile = prepareDistrFile(i, Tree->opt) ;
 	     if (distrFile != NULL) {
	  	   fprintf(distrFile, "# Training instances \n") ;
           Tree->test(Tree->DTraining, Tree->NoTrainCases,  Accuracy[i], Cost[i], Inf[i], Auc[i], PMx, Kappa[i], Sens[i], Spec[i], Brier[i], precision[i], Gmean[i], KS[i], TPR[i], FPR[i], distrFile) ;
 	     }
 	     if (distrFile != NULL)
	  	    fprintf(distrFile, "# Testing instances \n") ;
         Tree->test(Tree->DTesting, Tree->NoTestCases, Accuracy[i], Cost[i], Inf[i], Auc[i], PMx, Kappa[i], Sens[i], Spec[i], Brier[i], precision[i], Gmean[i], KS[i], TPR[i], FPR[i], distrFile);

 	     if (distrFile != NULL)
            fclose(distrFile) ;

         Tree->rfResultLine(to, i, oobAccuracy[i],oobMargin[i],oobCorrelation[i],
                               Accuracy[i], Cost[i], Inf[i], Auc[i], Sens[i], Spec[i], Brier[i], Kappa[i]) ;
         Tree->rfResultLine(stdout, i, oobAccuracy[i],oobMargin[i],oobCorrelation[i],
                               Accuracy[i], Cost[i], Inf[i],Auc[i], Sens[i], Spec[i], Brier[i],Kappa[i]) ;
		 if (Tree->opt->rfAttrEvaluate) {
		    attrEval[i+1].create(Tree->noAttr+1) ;
  	        Tree->varImportance(attrEval[i+1]) ;
			for (int iA=1 ; iA <= Tree->noAttr ; iA++) // averages
				attrEval[0][iA] += attrEval[i+1][iA] ;
			idx[i+1] = i ;
		 }
         fflush(to) ;
         fflush(stdout) ;
      }
   }
   double buildEnd = timeMeasure() ;

   double avgOobAcc, stdOobAcc, avgOobMg, stdOobMg, avgOobRo, stdOobRo,
          avgAcc, stdAcc, avgCost, stdCost, avgInf, stdInf,
          avgKappa, stdKappa, avgAuc, stdAuc, avgSens, stdSens, avgSpec, stdSpec,
		  avgBrier, stdBrier;
   AvgStd(oobAccuracy, Tree->opt->numberOfSplits, avgOobAcc, stdOobAcc) ;
   AvgStd(oobMargin, Tree->opt->numberOfSplits, avgOobMg, stdOobMg) ;
   AvgStd(oobCorrelation, Tree->opt->numberOfSplits, avgOobRo, stdOobRo) ;
   AvgStd(Accuracy, Tree->opt->numberOfSplits, avgAcc, stdAcc) ;
   AvgStd(Cost, Tree->opt->numberOfSplits, avgCost, stdCost) ;
   AvgStd(Inf, Tree->opt->numberOfSplits, avgInf, stdInf) ;
   AvgStd(Auc, Tree->opt->numberOfSplits, avgAuc, stdAuc) ;
   AvgStd(Sens, Tree->opt->numberOfSplits, avgSens, stdSens) ;
   AvgStd(Spec, Tree->opt->numberOfSplits, avgSpec, stdSpec) ;
   AvgStd(Brier, Tree->opt->numberOfSplits, avgBrier, stdBrier) ;
   AvgStd(Kappa, Tree->opt->numberOfSplits, avgKappa, stdKappa) ;

   printLine(to,"-",80)  ;

   Tree->rfResultLine(to, -1, avgOobAcc, avgOobMg, avgOobRo, avgAcc, avgCost, avgInf, avgAuc, avgSens, avgSpec, avgBrier, avgKappa) ;
   Tree->rfResultLine(to, -2, stdOobAcc, stdOobMg, stdOobRo, stdAcc, stdCost, stdInf, stdAuc, stdSens, stdSpec, stdBrier, stdKappa) ;
   Tree->rfResultLine(stdout, -1, avgOobAcc, avgOobMg, avgOobRo, avgAcc, avgCost, avgInf, avgAuc, avgSens, avgSpec, avgBrier, avgKappa) ;
   Tree->rfResultLine(stdout, -2, stdOobAcc, stdOobMg, stdOobRo, stdAcc, stdCost, stdInf, stdAuc, stdSens, stdSpec, stdBrier, stdKappa) ;

   if (Tree->opt->rfAttrEvaluate) {
	   for (int iA=1 ; iA <= Tree->noAttr ; iA++)
		   attrEval[0][iA] /= double(Tree->opt->numberOfSplits) ;

	   Tree->printAttrEval(to, idx, attrEval) ;
       Tree->printAttrEval(stdout, idx, attrEval) ;
   }

   fprintf(to,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;
   fprintf(stdout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(buildStart, buildEnd)) ;

   fclose(to) ;
   fflush(stdout) ;
}



//**********************************************************************
//
//                      domainCharacteristics
//                      ---------------------
//
//      computes concept variation of estimation data
//
//**********************************************************************
void domainCharacteristics(featureTree* const Tree)
{
   if (!Tree->readProblem(mTRUE, mTRUE))
      return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   char path[MaxPath] ;
   FILE *to ;
   snprintf(path, MaxPath, "%s%s.dataCharacteristics", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue());
   if ((to=fopen(path,"w"))==NULL) {
       stop("Cannot open data characteristics report file",path) ;
   }

   Tree->setDataSplit(Tree->opt->splitIdx) ;
   outVersion(stdout) ;
   Tree->outDomainSummary(stdout) ;
   outVersion(to) ;
   Tree->outDomainSummary(to) ;
   fprintf(to, "\nNumber of training examples for statistics below: %d\n",Tree->NoTrainCases) ;
   fprintf(to, "\nCharacteristics of attributes:\n") ;
   fprintf(stdout, "\nNumber of training examples for statistics below: %d\n",Tree->NoTrainCases) ;
   fprintf(stdout, "\nCharacteristics of attributes:\n") ;
   int i, j, k ;
   int missing, attrIdx, missingSum=0, valSum=0 ;
   for (i=0 ; i <= Tree->noAttr ; i++)
   {
       if (i==0) {
	    fprintf(stdout, "Class:") ;
	    fprintf(to, "Class:") ;
       }
       else {
		 fprintf(stdout, "%d. ",i) ;
		 fprintf(to, "%d. ",i) ;
       }
	  fprintf(to, "%s",Tree->AttrDesc[i].AttributeName) ;
	  fprintf(stdout, "%s",Tree->AttrDesc[i].AttributeName) ;
      if (Tree->AttrDesc[i].continuous)
	  {
          missing = 0 ;
		  attrIdx = Tree->AttrDesc[i].tablePlace ;
		  for (k=0 ; k < Tree->NoTrainCases; k++)
             if (isNAcont(Tree->NumData(Tree->DTraining[k], attrIdx)))
				 missing++ ;
		 fprintf(stdout, " (numeric, missing: %d values = %7.4f%%)\n", missing, double(missing)/Tree->NoTrainCases*100.0);
		 fprintf(to, " (numeric, missing: %d values = %7.4f%%)\n", missing, double(missing)/Tree->NoTrainCases*100.0);
	     missingSum += missing ;
      }
	  else {
		 attrIdx =  Tree->AttrDesc[i].tablePlace ;
		 marray<int> valCount(Tree->AttrDesc[i].NoValues+1, 0) ;
         if ( i>0 )
             valSum += Tree->AttrDesc[i].NoValues ;
         for (k=0 ; k < Tree->NoTrainCases ; k++)
           valCount[Tree->DiscData(Tree->DTraining[k], attrIdx)] ++ ;
	  	 fprintf(stdout, " (%d values, missing: %d = %7.4f%%)\n",Tree->AttrDesc[i].NoValues,valCount[0], double(valCount[0])/Tree->NoTrainCases*100.0) ;
         fprintf(to, " (%d values, missing: %d = %7.4f%%)\n",Tree->AttrDesc[i].NoValues,valCount[0], double(valCount[0])/Tree->NoTrainCases*100.0) ;
         missingSum += valCount[0]  ;
         for (j=0 ; j < Tree->AttrDesc[i].NoValues ; j++) {
			 fprintf(stdout, "\t%s (%d=%7.4f%%)\n",Tree->AttrDesc[i].ValueName[j], valCount[j+1], double(valCount[j+1])/Tree->NoTrainCases*100.0) ;
			 fprintf(to, "\t%s (%d=%7.4f%%)\n",Tree->AttrDesc[i].ValueName[j], valCount[j+1], double(valCount[j+1])/Tree->NoTrainCases*100.0) ;
         }
	  }
   }
   double avgVal = 0 ;
   if (Tree->noDiscrete > 1)
       avgVal = double(valSum)/double(Tree->noDiscrete-1) ;
   fprintf(to,"Average number of values per nominal attribute: %.2f\n",avgVal) ;
   fprintf(to,"Number of missing values: %d = %.2f%%\n", missingSum, double(missingSum)/Tree->NoTrainCases/Tree->noAttr*100.0) ;
   fprintf(stdout,"Average number of values per nominal attribute: %.2f\n",avgVal) ;
   fprintf(stdout,"Number of missing values: %d = %.2f%%)\n", missingSum, double(missingSum)/Tree->NoTrainCases/Tree->noAttr*100.0) ;
   printLine(to,"-",50)  ;

   // conceptVariation
   marray<double> weight(Tree->NoTrainCases,1.0) ;
   estimation Estimator(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;

   double ConVar ;
   //ConVar = Estimator.CVVilalta(0,Tree->noNumeric,1,Tree->noDiscrete) ;
   //fprintf(stdout,"\nConcept variation (Vilalta) for %d examples is %10.4f\n", Estimator.TrainSize, ConVar) ;
   //fprintf(to,"\nConcept variation (Vilalta) for %d examples is %10.4f\n", Estimator.TrainSize, ConVar) ;

   ConVar = Estimator.CVmodified(0,Tree->noNumeric,1,Tree->noDiscrete) ;
   fprintf(stdout,"\nConcept variation (Robnik Sikonja variant): %10.4f\n", ConVar) ;
   fprintf(to,"\nConcept variation (Robnik Sikonja variant): %10.4f\n", ConVar) ;
   fclose(to) ;
}


//**********************************************************************
//
//                      domainCharacteristicsReg
//                      ----------------
//
//      computes various charactristics including concept variation of data
//
//**********************************************************************
void domainCharacteristicsReg(featureTree* const FTree)
{
  regressionTree *Tree = new regressionTree ;
   Tree->opt = FTree->opt ;
   if (!Tree->readProblem(mTRUE, mTRUE)) {
	   Tree->opt = 0 ;
	   delete Tree ;
       return ;
   }
   if (!Tree->isRegression) {
	   merror("Provided data is not regressional", "") ;
	   Tree->opt = 0 ;
	   delete Tree ;
	   return ;
   }

   Tree->setDataSplit(Tree->opt->splitIdx) ;

   Tree->outDomainSummary(stdout) ;
   printf("\nNumber of training examples for statistics below: %d\n",Tree->NoTrainCases) ;
   printf("\nCharacteristics of attributes:\n") ;
   int i, j, k ;
   int missing, attrIdx ;
   for (i=0 ; i <= Tree->noAttr ; i++)
   {
      if (i==0)
	    printf("Prediction value:") ;
      else
		printf("%d. ",i) ;
	  printf("%s",Tree->AttrDesc[i].AttributeName) ;
      if (Tree->AttrDesc[i].continuous)
	  {
          missing = 0 ;
		  attrIdx = Tree->AttrDesc[i].tablePlace ;
		  for (k=0 ; k < Tree->NoTrainCases; k++)
             if (isNAcont(Tree->NumData(Tree->DTraining[k], attrIdx)))
				 missing++ ;
		 printf(" (numeric, missing: %d values = %5.2f%%)\n", missing, double(missing)/Tree->NoTrainCases*100.0);
	  }
	  else {
		 attrIdx =  Tree->AttrDesc[i].tablePlace ;
		 marray<int> valCount(Tree->AttrDesc[i].NoValues+1, 0) ;
         for (k=0 ; k < Tree->NoTrainCases ; k++)
           valCount[Tree->DiscData(Tree->DTraining[k], attrIdx)] ++ ;

	  	 printf(" (%d values, missing: %d = %5.2f%%)\n",Tree->AttrDesc[i].NoValues,valCount[0], double(valCount[0])/Tree->NoTrainCases*100.0) ;
		 for (j=0 ; j < Tree->AttrDesc[i].NoValues ; j++)
			 printf("\t%s (%d=%5.2f%%)\n",Tree->AttrDesc[i].ValueName[j], valCount[j+1], double(valCount[j+1])/Tree->NoTrainCases*100.0) ;
	  }
   }
   printf("\n-----------------------------------------------------------\n") ;

   marray<double> weight(Tree->NoTrainCases,1.0) ;
   estimationReg Estimator(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;

   double ConVar;

   // ConVar = Estimator.ConceptVariation(1,Tree->noNumeric,0,Tree->noDiscrete) ;
   // fprintf(stdout,"\nConcept variation for %d examples is %10.4f\n", Estimator.TrainSize, ConVar) ;

   ConVar = Estimator.CVmodified(1,Tree->noNumeric,0,Tree->noDiscrete) ;
   fprintf(stdout,"\nConcept variation (Robnik Sikonja variant) for %d examples is %10.4f\n", Estimator.TrainSize, ConVar) ;

   Tree->opt = 0 ;
   delete Tree ;
}


void evalOrdAttrValNorm(featureTree*  Tree, demandType demand)  {
  if (!Tree->readProblem(mTRUE, mTRUE))
	   return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   Tree->setDataSplit(Tree->opt->splitIdx) ;
   FILE *fout ;
   char path[MaxPath] ;

   marray<double> weight ;
   estimation *pEstimator=0 ;
   int attrIdx,iV, maxNoValues = 0 ;
   marray<marray<double> > reinfPos(Tree->noDiscrete), reinfNeg(Tree->noDiscrete), anchor(Tree->noDiscrete);

   for (attrIdx=1 ; attrIdx < Tree->noDiscrete ; attrIdx++) {
	   // for each attribute we need space for its values
	   reinfPos[attrIdx].create(Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues+1, 0.0) ;
	   reinfNeg[attrIdx].create(Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues+1, 0.0) ;
	   anchor[attrIdx].create(Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues+1, 0.0) ;
   	   if (Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues > maxNoValues)
		   maxNoValues = Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues ;
   }
   mmatrix<marray<double> > reinfPosRnd(Tree->noDiscrete, maxNoValues+1), reinfNegRnd(Tree->noDiscrete, maxNoValues+1), anchorRnd(Tree->noDiscrete, maxNoValues+1) ;
   for (attrIdx=1 ; attrIdx < Tree->noDiscrete ; attrIdx++) {
	   for (iV = 0 ; iV <= Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues ; ++iV) {
	      reinfPosRnd(attrIdx, iV).create(noOEstats, 0.0) ;
	      reinfNegRnd(attrIdx, iV).create(noOEstats, 0.0) ;
	      anchorRnd(attrIdx, iV).create(noOEstats, 0.0) ;
	   }
   }

   double estStart = timeMeasure() ;
   weight.create(Tree->NoTrainCases,1.0) ;
   pEstimator = new estimation(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;


   if (demand == ordEval) {
	   pEstimator->ordAVdAeq(1,Tree->noDiscrete, reinfPos,reinfNeg, anchor, kEqual);
   }
   else	if (demand == ordEvalNorm || demand == ordEvalNormClDiff1 || demand == ordEvalNormAttrDiff1) {
	   if (demand == ordEvalNorm)
		   pEstimator->ordAVdAeqNorm(1,Tree->noDiscrete, kEqual, reinfPos,reinfNeg,anchor, reinfPosRnd,reinfNegRnd,anchorRnd);
       else if (demand == ordEvalNormClDiff1)
		   pEstimator->ordAVdAeqNormClDiff1(1,Tree->noDiscrete, kEqual, reinfPos,reinfNeg,anchor, reinfPosRnd,reinfNegRnd,anchorRnd);
       else if (demand == ordEvalNormAttrDiff1)
		   pEstimator->ordAVdAeqNormAttrDiff1(1,Tree->noDiscrete, kEqual, reinfPos,reinfNeg,anchor, reinfPosRnd,reinfNegRnd,anchorRnd);
       if (Tree->opt->ordEvalNoRandomNormalizers > 0) {
		   snprintf(path, MaxPath, "%s%s.oer",Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue()) ;
		   if ((fout = fopen(path, "w")) == NULL) {
			   merror("evalOrdAttrValNorm: cannot open results file: ", path);
		   }
		   else {
			   printAVestRnd(fout, reinfPosRnd, reinfNegRnd, anchorRnd, Tree);
			   fclose(fout);
		   }
	   }
   }
    else merror("evalOrdAttrValNorm", "unrecognized demand") ;

   snprintf(path, MaxPath, "%s%s.oe", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue()) ;
   if ((fout = fopen(path,"w"))==NULL)   {
      merror("evalOrdAttrValNorm: cannot open results file: ", path)  ;
   }
   if (fout) {
	   printAVest(fout, reinfPos, reinfNeg, anchor, Tree);
	   fclose(fout);
   }

   printAVest(stdout, reinfPos, reinfNeg, anchor, Tree);

   delete pEstimator ;
   double estEnd = timeMeasure() ;

   fprintf(stdout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(estStart, estEnd)) ;
   fflush(stdout) ;
}



void runOrdEvalInst(featureTree*  Tree)  {
  if (!Tree->readProblem(mTRUE, mTRUE))
	   return ;
   if (Tree->isRegression) {
	   merror("Provided data is regressional", "") ;
	   return ;
   }

   Tree->setDataSplit(Tree->opt->splitIdx) ;
   FILE *foei=NULL, *foeiR=NULL ;
   char path1[MaxPath],path2[MaxPath] ;
   marray<double> weight ;
   estimation *pEstimator=0 ;
   int attrIdx, maxNoValues = 0 ;
   marray<double> reinfPos(Tree->noDiscrete), reinfNeg(Tree->noDiscrete), anchor(Tree->noDiscrete);

   for (attrIdx=1 ; attrIdx < Tree->noDiscrete ; attrIdx++) {
	  if (Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues > maxNoValues)
		   maxNoValues = Tree->AttrDesc[Tree->DiscIdx[attrIdx]].NoValues ;
   }
   marray<marray<double> > reinfPosRnd(Tree->noDiscrete), reinfNegRnd(Tree->noDiscrete), anchorRnd(Tree->noDiscrete) ;
   for (attrIdx=1 ; attrIdx < Tree->noDiscrete ; attrIdx++) {
	      reinfPosRnd[attrIdx].create(noOEstats, 0.0) ;
	      reinfNegRnd[attrIdx].create(noOEstats, 0.0) ;
	      anchorRnd[attrIdx].create(noOEstats, 0.0) ;
   }
   snprintf(path1, MaxPath, "%s%s.oei", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue()) ;
   if ((foei = fopen(path1,"w"))==NULL)   {
        merror("runOrdEvalInst: cannot open results file: ", path1)  ;
        return ;
     }
   if (Tree->opt->ordEvalNoRandomNormalizers > 0) {
   		   snprintf(path2, MaxPath, "%s%s.oeir", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue()) ;
   		   if ((foeiR = fopen(path2,"w"))==NULL) {
   	          merror("runOrdEvalInst: cannot open results file: ", path2)  ;
   	          return ;
   		   }
   }
   else strcpy(path2,"random normalization not selected");
   printf("Writing the output to file %s (%s)",path1,path2);
   fflush(stdout) ;

   double estStart = timeMeasure() ;
   weight.create(Tree->NoTrainCases,1.0) ;
   pEstimator = new estimation(Tree, Tree->DTraining,weight,Tree->NoTrainCases) ;
   for (int inst = 0 ; inst < Tree->NoTrainCases; inst++){
 	   pEstimator->ordEvalInst3(Tree->DTraining[inst], 1, Tree->noDiscrete, expRank, reinfPos,reinfNeg,anchor, reinfPosRnd,reinfNegRnd,anchorRnd);
 	   printOrdEvalInst(foei, Tree->DTraining[inst], reinfPos, reinfNeg, anchor, Tree) ;
 	   if (Tree->opt->ordEvalNoRandomNormalizers > 0)
	      printOrdEvalInstRnd(foeiR, Tree->DTraining[inst], reinfPosRnd, reinfNegRnd, anchorRnd, Tree) ;
   }


   delete pEstimator ;
   double estEnd = timeMeasure() ;

   fprintf(stdout,"\nCPU time used: %.2f seconds\n", timeMeasureDiff(estStart, estEnd)) ;

   fflush(stdout) ;
   fclose(foei) ;
   if (Tree->opt->ordEvalNoRandomNormalizers > 0)
      fclose(foeiR);
}




FILE* prepareDistrFile(int fileIdx, Options *opt) {
  FILE *distrFile = NULL ;
  if (opt->outProbDistr)     {
	 char distrPath[MaxPath] ;
     snprintf(distrPath, MaxPath, "%s%s.%03d.cpd", opt->resultsDirectory.getConstValue(), opt->domainName.getConstValue(), fileIdx) ;
	 if ((distrFile=fopen(distrPath,"w"))==NULL)
           merror("Cannot write to distribution file", distrPath) ;
	 else {
	   fprintf(distrFile, "# Class probability distribution file generated by %s\n", VersionString) ;
	   fprintf(distrFile, "# Format: instance prob_of_class_1 ... prob_of_class_n\n# \n") ;
      }
  }
  return distrFile ;
}

void saveSingleRF(featureTree* const Tree) {
   singleRF(Tree) ;
   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.rf", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue());
   if (Tree->writeRF(path))
	   printf("Random forest successfully saved to %s", path) ;
   else
	   printf("Saving random forest failed to file %s", path) ;
}

/*
void saveLargeRF(featureTree* const Tree) {
   char path[MaxPath] ;
   snprintf(path, MaxPath, "%s%s.rf", Tree->opt->resultsDirectory, Tree->opt->domainName);
   if (Tree->tempSaveForest(path))
	   printf("Random forest successfully saved to %s", path) ;
   else
	   printf("Saving random forest failed to file %s", path) ;
}
*/
void loadSingleRF(featureTree* const Tree) {
	char path[MaxPath] ;
	snprintf(path, MaxPath, "%s%s.rf", Tree->opt->resultsDirectory.getConstValue(), Tree->opt->domainName.getConstValue());
	if (!Tree->readProblem(mTRUE, mTRUE))
		return ;
    Tree->setDataSplit(Tree->opt->splitIdx) ;

	if (Tree->readForest(path))
		printf("Random forest successfully loaded from %s", path) ;
	else
		printf("Loading random forest failed from file %s", path) ;
	double Accuracy, Inf, Cost, Auc, Sens, Spec, Brier, Kappa, precision, Gmean, KS, TPR, FPR ;

	FILE *distrFile = NULL;
	int PMxSize = Tree->noClasses+1 ;
	mmatrix<int> PMx(PMxSize,PMxSize) ;

	Tree->test(Tree->DTraining, Tree->NoTrainCases, Accuracy, Cost, Inf, Auc, PMx, Kappa, Sens, Spec, Brier, precision, Gmean, KS, TPR, FPR, distrFile);
}
#endif // if !defined(R_PORT)

