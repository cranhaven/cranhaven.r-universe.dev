#include <cstdlib>
#include <cfloat>
#include <cmath>

#include "general.h"
#include "error.h"
#include "ftree.h"
#include "mathutil.h"
#include "options.h"

using namespace std ;


//************************************************************
//
//                 buildModel
//                 ---------
//
//    builds model to explain the data in a node
//
//************************************************************
void featureTree::buildModel(estimation &Estimator, binnode* Node)
{
   Node->Model.gFT = this ;
   // what kind of a model do we use in a leaf
   switch (opt->modelType)   {
       case 1:
              //  majority class value
              Node->Model.createMajority(Node->majorClass) ;
              break;
       case 2:  // k-NN
              Node->Model.createKNN() ; 
              break ;
       case 3:  // k-NN
              Node->Model.createKNNkernel() ; 
              break ;
       case 4:  // simple Bayes
              Node->Model.createSimpleBayes(Estimator, Node) ; 
              break ;
      default: merror("featureTree::buildModel","invalid modelType detected") ;
   }
}

