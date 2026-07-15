/*********************************************************************
*   Name:              modul MDL
*
*   Description:  MDL based pruning
*
*********************************************************************/

#include <cstdlib>

#include "general.h"
#include "error.h"
#include "utils.h"
#include "regtree.h"

using namespace std ;

//************************************************************
//
//                      mdlBottomUpPrune
//                      -------------
//  prunes regression tree bottom up in recursive manner
//
//************************************************************
double regressionTree::mdlBottomUpPrune(binnodeReg *Node)
{

   // 1 bit is added as indicator of status of a node: leaf or interior node
   if  (Node->left == 0)  // && (Node->right == 0) ) // leaf
      return double(1.0) + mdlCode(Node) ; // return leaf coding

   double codeL = mdlBottomUpPrune(Node->left) ;
   double codeR = mdlBottomUpPrune(Node->right) ;

   double codeSubtrees = codeL + codeR + double(1.0) + Node->Construct.mdlConstructCode() ;
   double codeNode = mdlCode(Node) + double(1.0) ;

   if (codeNode <= codeSubtrees)
   {
       // prune subtrees
       destroy(Node->left) ;
       destroy(Node->right) ;

       createLeaf(Node) ;

       return codeNode  ;
   }
   else
     return codeSubtrees ;
}



double regressionTree::mdlCode(binnodeReg *Node)
{
    double codeLen = 0.0 ;
    double intValue ;
      // comute normalized standard deviation of the class
      // variant1: unnormalized standard deviation divided by a qutient
      //stdDevErrorDensity = (rootStdDev/stdDevErrorDensityQuotient ;
      // variant2: normalized standard deviation divided by a quotient
      //stdDevErrorDensity = (rootStdDev/valueInterval[0]) / stdDevErrorDensityQuotient ;
      // variant3: absolute value of  standard deviation (normalized values)
      //stdDevErrorDensity = stdDevErrorDensityQuotient ;

      //mdlPrecision = valueInterval[0] * mdlProportionEqual ;
      //coefficientCode = log2( valueInterval[0]/opt->mdlModelPrecision) ;   // for variant 4,5,6

    // code model
    switch (opt->modelTypeReg)
    {

        case 1: // point model
        case 2:
              //Node->code = Node->Model.mdlPointCost(opt->mdlModelPrecision) ; // we code the point model
              //codeLen = log2(valueInterval[0]/opt->mdlModelPrecision) ; // fixed code size ;
              //codeLen = log2(1.0/opt->mdlModelPrecision) ; // fixed code size ;
              // all have points have equal code
              intValue = valueInterval[0]/opt->mdlErrorPrecision ;
              if (intValue <= 1.0)
                 codeLen = 0.0 ;
              else 
                codeLen =  mlog2(intValue) ;
              break ;
        case 3: // linear by MSE (all attributes included)
        case 4: // linear by MDL (only selected attributes)
        case 5: // linear as in M5 (only selected attributes)
              codeLen = Node->Model.mdlCost(noNumeric-1) ; // we code the model
              break ;
        default: merror("regressionTree::mdlCode", "invalid model in the node") ;
    }

    // code error
    // marray<double> Multinom(3,0.0) ;  // variant 4
    double prediction ;
    for (int i=0 ; i < Node->DTrain.len() ; i++)
    {
          prediction = Node->Model.predictSafe(Node, Node->DTrain[i]) ;
          // we compute code for error from the model
          // variant1: we code the error by integers
          codeLen += double(1.0) + // sign
              mdlIntEncode((NumData(Node->DTrain[i], 0) - prediction)/opt->mdlErrorPrecision )  ;  // we round the integer

          //  variant 2, 3: error is normalized to [0,1], we code the probabilities of normal distribution
          //fTemp = erfcc(double(fabs((NumData(DTrain[i], 0) - Node->averageClassValue)/valueInterval[0]/stdDevErrorDensity/sqrt2))) ;
          //if (fTemp < epsilon)  // protect from log of zero
          //  fTemp = epsilon ;
          //Node->code += double(1.0) - log2(fTemp) ;

          // variant 4,5: we code the error by hit/upper/lower
          //               ( n+c-1 )         (       n         )
          // I(Leaf) = log (  c-1  )   + log ( hit upper lower )
          // here we count hits, upper, lower

          // if (NumData(Node->DTrain[i], 0) < prediction - mdlPrecision)
          //    Multinom[0] += 1.0 ;
          // else
          //    if (NumData(Node->DTrain[i], 0) > prediction + mdlPrecision)
          //       Multinom[1] += 1.0 ;
          //    else
          //      Multinom[2] += 1.0 ;

          //variant 6: we code the error by hit/miss
          //               ( n+c-1 )         (  n   )
          // I(Leaf) = log (  c-1  )   + log ( hit  )
          // here we count hits and misses
          // if (NumData(DTrain[i], 0) < Node->averageClassValue - mdlPrecision ||
          //     NumData(DTrain[i], 0) > Node->averageClassValue + mdlPrecision)
          //   Multinom[0] += 1.0 ;
       }
       //variant 4,5: we code the error by hit/upper/lower
       //Multinom.setFilled(3) ;
       //codeLen += multinomLog2(Multinom) ;
       //Multinom[0] = 2 ;
       //Multinom[1] = Node->DTrain.len() ;
       //Multinom.setFilled(2)  ;
       //codeLen += multinomLog2(Multinom) ;

       //variant 6: we code the error by hit/miss
       //Multinom[1] = TrainSize - Multinom[0] ;
       //Multinom.setFilled(2) ;
       //Node->code += multinomLog2(Multinom) ;
       //Multinom[0] = 1 ;
       //Multinom[1] = TrainSize ;
       //Multinom.setFilled(2)  ;
       //Node->code += multinomLog2(Multinom) ;

       // variant 7: same as 4, but use Rissanen's L formula
       // Multinom.setFilled(3) ;
       // codeLen += L2(Multinom) ;

     return codeLen ;
 }




//************************************************************
//
//                 mPrune
//                 -------
//
//     prune regression tree with modification of Nibbet-Bratko method using
//                       m-estimate
//
//************************************************************
double regressionTree::mPrune(binnodeReg* Node)
{

     double priorMSE = 0.0 ;
     for (int i=0; i < rootTrainSize ; i++)
        priorMSE += sqr(NumData(rootDTrain[i],0) - Node->Model.predictSafe(Node, rootDTrain[i]) ) ;
     priorMSE /= rootTrainSize ;

     double Es = (opt->mEstPruning * priorMSE + Node->weight * Node->MSE) / (opt->mEstPruning + Node->weight) ;

//   double Es = (Node->squaresClass + mEstPrune/rootWeight*rootSquares)/
//                              (Node->weight + mEstPrune) -
//        sqr((rootAverage*mEstPrune + Node->averageClassValue*Node->weight)/
//              (Node->weight + mEstPrune) ) ;

   if  (Node->left == 0)  // && (Node->right == 0) ) // leaf
       // return static error
     return Es ;

   double El = mPrune(Node->left) ;
   double Er = mPrune(Node->right) ;

   double pLeft = Node->weightLeft/Node->weight ;
   double Ed = pLeft * El + (double(1.0) - pLeft) * Er ;

   if (Es <= Ed)
   {
       // prune subtrees
       destroy(Node->left) ;
       destroy(Node->right) ;

       createLeaf(Node) ;

       return Es  ;
   }
   else return Ed ;
}


//**********************************************************************
//
//                      mdlSelectBestPrune
//                      ------------------
//
//          test the mdl pruning algorithm
//
//**********************************************************************
/*
double regressionTree::mdlSelectBestPrune()
{

   bintreeReg fullTree(*this) ;

    double codeLen, bestCode = 1e38, bestEq=0.1, bestPrec=0.1 ;
    double eq = 0.0, precision = 0.00000001 ;

    while (eq <= 1.000001)
    {
       mdlProportionEqual = eq ;
       precision = 0.00000001 ;
       while (precision <= 1.00001)
       {
            opt->mdlModelPrecision = precision ;
            copy(fullTree) ;
            codeLen = mdlBottomUpPrune() ;
            if (codeLen < bestCode)
            {
               bestCode = codeLen ;
               bestEq = eq ;
               bestPrec = precision ;
            }

            if (precision == 0.00000001)
               precision = 0.0 ;
            precision += 0.02 ;
        }
        eq += 0.02 ;
    }
    mdlProportionEqual = bestEq ;
    opt->mdlModelPrecision = bestPrec ;
    copy(fullTree) ;
    return mdlBottomUpPrune() ;
}
*/




//************************************************************
//
//                 M5prune
//                 -------
//
//     prune regression tree the same way Quinlan does in M5
//
//************************************************************
double regressionTree::M5prune(binnodeReg* Node)
{

     double modelSize = Node->Model.noCoefficients() ;
     double Es ;
     if (Node->weight <= modelSize)
       Es = Node->MAE ;
     else
       Es = Node->MAE * 
              double(Node->weight + modelSize) / double(Node->weight - modelSize) ;


   if  (Node->left == 0)  // && (Node->right == 0) ) // leaf
       // return static error
     return Es ;

   double El = M5prune(Node->left) ;
   double Er = M5prune(Node->right) ;

   double pLeft = Node->weightLeft/Node->weight ;
   double Ed = pLeft * El + (double(1.0) - pLeft) * Er ;

   if (Es <= Ed)
   {
       // prune subtrees
       destroy(Node->left) ;
       destroy(Node->right) ;

       createLeaf(Node) ;

       return Es  ;
   }
   else return Ed ;
}




//************************************************************
//
//                 errorComplexityPrune
//                 -------------------
//
//     prune regression tree with minimal error complexity pruning 
//        by Breiman et al.; version with preselected alpha
//
//************************************************************
double regressionTree::errorComplexityPrune(binnodeReg* Node, int &Size)
{


   if  (Node->left == 0)  // && (Node->right == 0) ) // leaf
   {
       
       Size = 1 ;
       return Node->MSE * Node->DTrain.len() ; // the squared error
   }


   int sizeLeft=0, sizeRight = 0 ;
   double El = errorComplexityPrune(Node->left, sizeLeft) ;
   double Er = errorComplexityPrune(Node->right, sizeRight) ;

   double Enode= Node->MSE * Node->DTrain.len() ;

   double aBound = (Enode - El - Er) / (sizeLeft + sizeRight - 1.0) ;

   
   if (aBound < opt->alphaErrorComplexity)
   {
       // prune subtrees
       destroy(Node->left) ;
       destroy(Node->right) ;

       createLeaf(Node) ;

       Size = 1 ;
       return Enode  ;
   }
   else 
   {
       Size = sizeLeft + sizeRight ; 
       return El + Er ;
   }
}




//************************************************************
//
//                 errorComplexityPruneVar
//                 -------------------
//
//     prune regression tree with minimal error complexity pruning 
//        by Breiman et al.; version with preselected alpha
//
//************************************************************
double regressionTree::errorComplexityPruneVar(binnodeReg* Node, int &Size)
{


   if  (Node->left == 0)  // && (Node->right == 0) ) // leaf
   {
       
       Size = 1 ;
       return Node->stdDevClass * Node->DTrain.len() ; // the squared error
   }


   int sizeLeft=0, sizeRight = 0 ;
   double El = errorComplexityPrune(Node->left, sizeLeft) ;
   double Er = errorComplexityPrune(Node->right, sizeRight) ;

   double Enode= Node->stdDevClass * Node->DTrain.len() ;

   double aBound = (Enode - El - Er) / (sizeLeft + sizeRight - 1.0) ;

   
   if (aBound < opt->alphaErrorComplexity)
   {
       // prune subtrees
       destroy(Node->left) ;
       destroy(Node->right) ;

       createLeaf(Node) ;

       Size = 1 ;
       return Enode  ;
   }
   else 
   {
       Size = sizeLeft + sizeRight ; 
       return El + Er ;
   }
}

