#include <cstdlib>
#include <cfloat>
#include <cmath>

#include "general.h"
#include "error.h"
#include "regtree.h"
#include "mathutil.h"

using namespace std ;

//************************************************************
//
//                 buildModel
//                 ---------
//
//    builds model to explain the data in a node
//
//************************************************************
void regressionTree::buildModel(marray<int> &DTrain, marray<double> &pDTrain, 
								int TrainSize, binnodeReg *Node) const
{
   // what kind of a model do we use in a leaf
   Node->Model.gRT = this ;
   switch (opt->modelTypeReg)
   {

       case 1:
        point: //  use point model (mean of class values)
              {
                double weight=0.0, classSum=0.0 ;
                for (int i=0 ; i < TrainSize ; i++)
                {
                   weight += pDTrain[i] ;
                   classSum +=  pDTrain[i] * NumData(DTrain[i], 0) ;
                }
                Node->Model.createPoint(classSum/weight) ;
              }
              break;

       case 2:
              //  use point model (median of class values)
              {
                 marray<double> sortArray(TrainSize) ;
                 for (int i=0 ; i < TrainSize ; i++)
                    sortArray[i] = NumData(DTrain[i], 0) ;
                 sortArray.setFilled(TrainSize) ;
                 sortArray.select(TrainSize/2)  ;
                 Node->Model.createPoint(sortArray.select(TrainSize/2)) ;
              }
              break ;

       case 3:
              // maximum likehod (best MSE) linear model
              if (TrainSize <= noNumeric)
                 goto point ;
              // fit coeficients of linear (in coeficients) model with single value decomposition method
              svdFitLinearModel(DTrain, TrainSize, Node->Model ) ;
              break ;

       case 4:
              if (TrainSize <= noNumeric)
                goto point ;
              powellFitLinearModel(DTrain, TrainSize, Node->Model) ;
              break ;


	    case 5:
		      if (TrainSize <= noNumeric)
                goto point ;
              M5Simplify(DTrain, TrainSize, Node) ;
			  break ;

       case 6: 
              Node->Model.createKNN(TrainSize, opt->kInNN) ; 
              break ;

       case 7:
           // locally weighted (kernel) regression 
kernel:       Node->Model.creatennKernelWidthReg(TrainSize, opt->kInNN, opt->nnKernelWidth) ;
              break ;

       case 8: // locally weighted linear regression
              if (TrainSize <= noNumeric)
                 goto kernel ;
              Node->Model.createLWLR(TrainSize, opt->kInNN, opt->nnKernelWidth) ;
              break ;

       default: merror("regressionTree::buildModel","invalid opt->modelTypeRegReg detected") ;
   }
}

//************************************************************
//
//                 powellFitLinearModel
//                 --------------------
//
//    fits MDL based linear model with Powell's optimization
//
//************************************************************
void regressionTree::powellFitLinearModel(marray<int> &DTrain, int TrainSize, exprReg& Model) const
{
      int i, j ;

      // linear model with shortest description
         int modelSize = noNumeric  ;

         // fit coeficients of linear (in coeficients) model with
         // single value decomposition method
         // prepare data for computation
         double *x = new double[TrainSize+1] ;
         double *y = new double[TrainSize+1] ;
         double *sig = new double[TrainSize+1] ;
         for (i=0 ; i < TrainSize ; i++)
         {
            x[i+1] = DTrain[i] ;
            y[i+1] = NumData(DTrain[i], 0) ;
            sig[i+1] = 1.0 ;
         }
         double *a = new double[modelSize+1] ;
         double **u= new double*[TrainSize+1] ;
         for (i=1 ; i <= TrainSize ; i ++)
           u[i] = new double[modelSize+1] ;
         double **v= new double*[modelSize+1] ;
         for (i=1 ; i <= modelSize ; i ++)
           v[i] = new double[modelSize+1] ;
         double *w = new double[modelSize+1] ;
         double chiSquare ;
         marray<int> Mask(modelSize+1, 1) ;

		 svdfit(this, x,y,sig,TrainSize,a, Mask, modelSize,u,v,w,&chiSquare,ContDataRetriever) ;
         delete [] x ;
         delete [] y ;
         delete [] sig ;
         for (i=1 ; i <= TrainSize ; i ++)
           delete [] u[i] ;
         delete [] u ;
         for (i=1 ; i <= modelSize ; i ++)
           delete [] v[i] ;
         delete [] v ;
         delete [] w ;

         // Powels optimization method
         //int modelSize = noNumeric   ;
         //double *p = new double[modelSize+1] ;
         //for (i=0 ; i < modelSize ; i++)
         //   p[i] = 0.0 ;
         //p[modelSize] = 0.0 ; // Node->averageClassValue ;
         double **dirVector= new double*[modelSize+1] ;
         for (i=1 ; i <= modelSize ; i ++)
         {
            dirVector[i] = new double[modelSize+1] ;
            for (j=1 ; j <= modelSize ; j++)
               dirVector[i][j] = 0.0 ;
            dirVector[i][i] = 1.0 ;
         }
         int iterationsCounter ;
         double codeLen;

         powell(this, a, dirVector, Mask, modelSize, 0.0001 , &iterationsCounter, &codeLen, MdlCodeLen) ;
         Model.createLinear(a,modelSize, Mask) ;

         for (i=1 ; i <= modelSize ; i ++)
            delete [] dirVector[i] ;
         delete [] dirVector ;
   }


//************************************************************
//
//                 svdFitLinearModel
//                 -----------------
//
//    fits linear model with singular value decomposition
//
//************************************************************
void regressionTree::svdFitLinearModel(marray<int> &DTrain, int TrainSize, exprReg& Model) const
{
      int i ;
      double *x = new double[TrainSize+1] ;
      double *y = new double[TrainSize+1] ;
      double *sig = new double[TrainSize+1] ;
      for (i=0 ; i < TrainSize ; i++)
      {
         x[i+1] = DTrain[i] ;
         y[i+1] = NumData(DTrain[i], 0) ;
         sig[i+1] = 1.0 ;
      }
      int modelSize = noNumeric  ;
      double *a = new double[modelSize+1] ;
      double **u= new double*[TrainSize+1] ;
      for (i=1 ; i <= TrainSize ; i ++)
        u[i] = new double[modelSize+1] ;
      double **v= new double*[modelSize+1] ;
      for (i=1 ; i <= modelSize ; i ++)
        v[i] = new double[modelSize+1] ;
      double *w = new double[modelSize+1] ;
      double chiSquare ;
      marray<int> Mask(modelSize+1, 1) ;

	  svdfit(this,x,y,sig,TrainSize,a, Mask, modelSize,u,v,w,&chiSquare,ContDataRetriever) ;
      // use fitted coefficients
      Model.createLinear(a,modelSize, Mask) ;

      delete [] x ;
      delete [] y ;
      delete [] sig ;
      delete [] a ;
      for (i=1 ; i <= TrainSize ; i ++)
        delete [] u[i] ;
      delete [] u ;
      for (i=1 ; i <= modelSize ; i ++)
        delete [] v[i] ;
      delete [] v ;
      delete [] w ;
}


//************************************************************
//
//                 M5Simplify
//                 ----------
//
//    fit linear model with singular value decomposition
//     and then simplify  as Quinlan's M5 does
//
//************************************************************
void regressionTree::M5Simplify(marray<int> &DTrain, int TrainSize, binnodeReg* Node) const
{
      int i ;
      double *x = new double[TrainSize+1] ;
      double *y = new double[TrainSize+1] ;
      double *sig = new double[TrainSize+1] ;
      for (i=0 ; i < TrainSize ; i++)
      {
         x[i+1] = DTrain[i] ;
         y[i+1] = NumData(DTrain[i], 0) ;
         sig[i+1] = 1.0 ;
      }
      int modelSize = noNumeric  ;
      double *a = new double[modelSize+1] ;
      double **u= new double*[TrainSize+1] ;
      for (i=1 ; i <= TrainSize ; i ++)
        u[i] = new double[modelSize+1] ;
      double **v= new double*[modelSize+1] ;
      for (i=1 ; i <= modelSize ; i ++)
        v[i] = new double[modelSize+1] ;
      double *w = new double[modelSize+1] ;
      double chiSquare ;
      marray<int> Mask(modelSize+1, 1) ;
      marray<int> bestMask(Mask) ;       

	  svdfit(this,x,y,sig,TrainSize,a, Mask, modelSize,u,v,w,&chiSquare,ContDataRetriever) ;
      // use fitted coefficients
      Node->Model.createLinear(a,modelSize, Mask) ;
 
      double residium=0.0 ;
      double qError ;
      for (i=0 ; i < TrainSize ; i++)
         residium += fabs(NumData(DTrain[i], 0) - Node->Model.predict(Node, DTrain[i])) ;
      residium /= double(TrainSize) ;
      double bestError = residium * (TrainSize+modelSize)/(TrainSize-modelSize) ;
      double worstSizeError ;
      int position, worstSizePosition, bestModelSize=modelSize ;
      int currentModelSize = modelSize-1 ;
      while (currentModelSize >= 1)
      {
         worstSizeError =  DBL_MAX ;
         worstSizePosition = -1 ;
 
         for (position=1 ; position < Mask.len() ; position++)
         {
            if (Mask[position] == 1)
            {
               Mask[position] = 0 ;
               Node->Model.createLinear(a, modelSize, Mask) ;
               residium = 0.0 ;         
               for (i=0 ; i < TrainSize ; i++)
                 residium += fabs(NumData(DTrain[i], 0) - Node->Model.predict(Node, DTrain[i])) ;
               residium /= double(TrainSize) ;
               qError = residium * (TrainSize + currentModelSize) / double(TrainSize - currentModelSize) ;
               if (qError < worstSizeError)
               {
                 worstSizeError = qError ;
                 worstSizePosition = position ;
               }
               Mask[position] = 1 ;
            }
         }
         Mask[worstSizePosition] = 0 ;
         if (worstSizeError <= bestError)
         {
             bestError = worstSizeError ;
             bestMask.copy(Mask) ;
			 bestModelSize = currentModelSize ;
         }
 
		 svdfit(this,x, y, sig, TrainSize, a, Mask, currentModelSize, u, v, w, &chiSquare, ContDataRetriever) ;
 
         currentModelSize -- ;
      }
      // create final model
	  svdfit(this,x,y,sig,TrainSize,a, bestMask, bestModelSize,u,v,w,&chiSquare,ContDataRetriever) ;
      Node->Model.createLinear(a, modelSize, bestMask) ;

      delete [] x ;
      delete [] y ;
      delete [] sig ;
      delete [] a ;
      for (i=1 ; i <= TrainSize ; i ++)
        delete [] u[i] ;
      delete [] u ;
      for (i=1 ; i <= modelSize ; i ++)
        delete [] v[i] ;
      delete [] v ;
      delete [] w ;
}

	
