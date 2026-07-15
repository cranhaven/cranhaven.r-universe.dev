/********************************************************************
*
*   Name:          module EXPR
*
*   Description:      deals with representation of
*                          booleanT exprRegessions
*
*********************************************************************/

#include <cfloat>

#include "general.h"
#include "exprReg.h"
#include "bintreeReg.h"
#include "dataStore.h"
#include "regtree.h"
#include "error.h"
#include "utils.h"
#include "mathutil.h"

binnodeReg *currentNode ; // used for LWR
int currentCase ;
double *LWRweight ; // used for LWR



// **********************************************************************
//
//                      destroy
//                      -----------
//
//      deallocates space consumed by exprRegession
//
// **********************************************************************
void exprReg::destroy(void)
{ 
    if (root) 
    {
       destroy(root);  
       root = 0 ;
    }
    equalDistance.destroy() ;
    differentDistance.destroy() ;
    CAslope.destroy() ;
}


void exprReg::destroy(exprRegNode *node)
{
    if (node->left)
      destroy(node->left) ;
    if (node->right)
      destroy(node->right) ;

    delete node ;

}

//**********************************************************************
//
//                      copy
//                      --------
//
//      copies source booleanT exprRegession to target
//
//**********************************************************************
void exprReg::copy(const exprReg &Source)
{
   if (&Source == this)
     return ;

   gRT = Source.gRT ;
   destroy() ;
   if (Source.root)
     dup(Source.root,root) ;
   else
     root = 0 ;

    differentDistance = Source.differentDistance ;
    equalDistance = Source.equalDistance ;
    CAslope = Source.CAslope ;

}


//**********************************************************************
//
//                      operator =
//                      --------
//
//      copies source booleanT exprRegession to target
//
//**********************************************************************
exprReg& exprReg::operator= (const exprReg &Source)
{
    copy(Source) ;
    return *this ;
}

//**********************************************************************
//
//                      dup
//                      -------
//
//      duplicates source booleanT exprRegession into target
//
//**********************************************************************
void exprReg::dup(exprRegNode *Source, PexprRegNode &Target)
{
    Target = new exprRegNode ;
    Target->nodeType = Source->nodeType ;
    Target->iMain = Source->iMain ;
    Target->iAux = Source->iAux ;
    Target->dMain = Source->dMain ;
    Target->dAux = Source->dAux ;

    if (Source->left)
      dup(Source->left, Target->left) ;
    else
      Target->left = 0 ;
    if (Source->right)
      dup(Source->right, Target->right ) ;
    else
      Target->right = 0 ;
}


//**********************************************************************
//
//                      createPoint
//                      -----------
//
//                 creates point model
//
//
//**********************************************************************
void exprReg::createPoint(double Value)
{
    destroy() ;
    root = new exprRegNode ;
    root->nodeType = floatConstant ;
    root->dMain = Value ;
    root->left = root->right = 0 ;
}


//**********************************************************************
//
//                      createLinear
//                      ------------
//
//                 creates linear model
//
//
//**********************************************************************
void exprReg::createLinear(double *Parameters, int modelSize, marray<int> &Mask)
{
    destroy() ;
    root = new exprRegNode ;
    exprRegNode *workingNode = root ;
    int i = 1 ;
    exprRegNode *parentNode = 0 ;
    while (i < modelSize)
    {
       if (Parameters[i] == 0.0 || Mask[i] == 0)
       {
          i++ ;
          continue ;
       }
//       if (Parameters[i+1] < 0.0)  // select minus as operator
//       {
//          for (int j=i+1  ; j <= modelSize ; j++)  // change signs
//            Parameters[j] = - Parameters[j] ;
//          workingNode->nodeType = minus ;
//       }
//       else
          workingNode->nodeType = plus ;
       workingNode->left = new exprRegNode ;
       workingNode->left->nodeType = constTimesAttr ;
       workingNode->left->iMain = i ;
       workingNode->left->dMain = Parameters[i] ;
       workingNode->left->left = workingNode->left->right = 0 ;
       workingNode->right = new exprRegNode ;
       parentNode = workingNode ;
       workingNode = workingNode->right ;
       i++ ;
    }
    if (parentNode == 0) // all parameters (except possibly constant) were disabled or 0
    {
         workingNode->nodeType = floatConstant ;
         workingNode->left = workingNode->right = 0 ;        
         if (Mask[i]==1)
            workingNode->dMain = Parameters[modelSize] ; // constantValue
         else
            workingNode->dMain = 0.0 ;  
    }
    else 
      if (Mask[i]==1 )
      {  // we have expected last node to be constant
         workingNode->nodeType = floatConstant ;
         workingNode->dMain = Parameters[modelSize] ; // constantValue
         workingNode->left = workingNode->right = 0 ;
      }
      else
      {   // change parent node accordingly
	 	parentNode->nodeType = constTimesAttr ;
        parentNode-> iMain = parentNode->left->iMain ;
        parentNode-> dMain = parentNode->left->dMain ;
        delete parentNode->left ;
		delete parentNode->right ;
        parentNode->left = parentNode->right = 0 ;  
     }
}

//**********************************************************************
//
//                      createLocal
//                      -----------
//
//                 creates local (nearest-neighbour based) model
//
//
//**********************************************************************
void exprReg::createLocal(int TrainSize, int k)
{
    destroy() ;
    root = new exprRegNode ;
    if (k > 0 && k <= TrainSize) 
      root->iMain = k ;
    else
      root->iMain = TrainSize ;
    
    root->iAux = TrainSize ;
    
    root->left = root->right = 0 ;

    differentDistance.create(gRT->noNumeric) ;
    equalDistance.create(gRT->noNumeric) ;
    CAslope.create(gRT->noNumeric) ;

    int i ;
    for (i=0 ; i < gRT->noNumeric ; i++)
    {
        differentDistance[i] = gRT->AttrDesc[gRT->ContIdx[i]].DifferentDistance ;
        equalDistance[i] = gRT->AttrDesc[gRT->ContIdx[i]].EqualDistance ;
        if (differentDistance[i] != equalDistance[i])
            CAslope[i] = double(1.0)/(differentDistance[i] - equalDistance[i]) ;
         else
            CAslope[i] = DBL_MAX ;
    }
}


//**********************************************************************
//
//                      createKNN
//                      -----------
//
//                 creates k-nearest-neighbour model
//
//
//**********************************************************************
void exprReg::createKNN(int TrainSize, int k)
{
    createLocal(TrainSize,k) ;
    root->nodeType = kNNreg ;
}



//**********************************************************************
//
//                      createopt->nnKernelWidthReg
//                      -----------
//
//                 creates kernel regression model
//
//
//**********************************************************************
void exprReg::creatennKernelWidthReg(int TrainSize, int k, double nnKernelWidth)
{
    createLocal(TrainSize,k) ;
    root->nodeType = kernelRegression ;
    root->dMain = nnKernelWidth ;
}

//**********************************************************************
//
//                      createLWLRg
//                      -----------
//
//                 creates localy weighted linear regression model
//
//
//**********************************************************************
void exprReg::createLWLR(int TrainSize, int k, double nnKernelWidth)
{
    createLocal(TrainSize,k) ;
    root->nodeType = LWLR ;
    root->dMain = nnKernelWidth ;
}

//**********************************************************************
//
//                      noCoefficients
//                      -----------
//
//                 count number of coefficients in the model
//
//
//**********************************************************************

int exprReg::noCoefficients(exprRegNode *Node)
{
    if (Node->nodeType == floatConstant || Node->nodeType == constTimesAttr
        || Node->nodeType == kNNreg)
         return 1 ;

    int noLeft=0, noRight=0 ;
    if (Node->left)
      noLeft = noCoefficients(Node->left) ;
    if (Node->right)
      noRight = noCoefficients(Node->right) ;

    return noLeft + noRight ;
}




//**********************************************************************
//
//                      predictSafe
//                      -----------
//
//   prediction with the safety correction: it should stay in the [min,max] range
//
//
//**********************************************************************
double exprReg::predictSafe(binnodeReg *treeNode, int Case)
{
   double prediction = predict(treeNode, Case, root) ;

   if (prediction > treeNode->maxClassValue)
       return treeNode->maxClassValue ;
   if (prediction < treeNode->minClassValue)
      return treeNode->minClassValue ;
   
   return prediction ; 
}


//**********************************************************************
//
//                      predict
//                      -------
//
//           prediction of the class value for the given Case
//
//
//**********************************************************************
double exprReg::predict(binnodeReg *treeNode, int Case)
{
   if (root)
      return predict(treeNode, Case, root) ;
   else
      merror("exprReg::predict","Cannot evaluate nonexistent model") ;
   return 0.0 ;
}



double exprReg::predict(binnodeReg *treeNode, int Case, exprRegNode* Node)
{
    #if defined(DEBUG)
	if (!Node) {
		merror("exprReg::predict", "Invalid structure of model");
		return -DBL_MAX;
	}
    #endif
    switch(Node->nodeType)
    {
        case plus:
                    return predict(treeNode, Case, Node->left) + predict(treeNode, Case, Node->right) ;

        case minus:
                    return predict(treeNode, Case, Node->left) - predict(treeNode, Case, Node->right) ;

        case times:
                    return predict(treeNode, Case, Node->left) * predict(treeNode, Case, Node->right) ;

        case constTimesAttr:
                    if (isNAcont((*(gRT->nData))(Case, Node->iMain)))
					{
					   if (treeNode)
                         return Node->dMain * treeNode->NAnumValue[Node->iMain] ;
                       else
                         return Node->dMain * (gRT->maxValue[Node->iMain] - gRT->minValue[Node->iMain]) / 2.0 ;
					} 						   
                    else
                       return Node->dMain * (*(gRT->nData))(Case, Node->iMain) ;

        case floatConstant:
                    return Node->dMain ;

        case kNNreg:{
                    // find k nearest
                    marray<sortRec> NN(treeNode->DTrain.filled()) ;
                    int i ;
                    for (i=0 ; i < treeNode->DTrain.filled() ; i++)
                    {
                        NN[i].value = treeNode->DTrain[i] ;
                        NN[i].key = examplesDistance(treeNode, treeNode->DTrain[i], Case) ;
                    }
                    NN.setFilled(treeNode->DTrain.filled()) ;
                    NN.sortKsmallest(Node->iMain) ;

                    double sum = 0.0 ;
                    for (i=NN.filled()-1 ; i > NN.filled()-1-Node->iMain ; i--)
                        sum += gRT->NumData(NN[i].value, 0) ;
                    return sum / double(Node->iMain) ;

                  } 
        case kernelRegression:{

                     // find k nearest
                     marray<sortRec> NN(treeNode->DTrain.len()) ;
                     int i ;
                     for (i=0 ; i < treeNode->DTrain.len() ; i++)
                     {
                       NN[i].value = treeNode->DTrain[i] ;
                       NN[i].key = examplesDistance(treeNode, treeNode->DTrain[i], Case) ;
                     }
                     NN.setFilled(treeNode->DTrain.len()) ;
                     NN.sortKsmallest(Node->iMain) ;

                     double sum = 0.0, weightSum = 0, weight ;
                     for (i=NN.filled()-1 ; i > NN.filled()-1-Node->iMain ; i--)
                     {
                        weight = exp(-sqr(NN[i].key/Node->dMain)/2.0) ;
                        weightSum += weight ;
                        sum += weight * gRT->NumData(NN[i].value, 0) ;
                     }
                     return sum / weightSum ;
                  } 
 
        case LWLR:{
                     // find k nearest
                     marray<sortRec> NN(treeNode->DTrain.len()) ;
                     int i ;
                     for (i=0 ; i < treeNode->DTrain.len() ; i++)
                     {
                       NN[i].value = treeNode->DTrain[i] ;
                       NN[i].key = examplesDistance(treeNode, treeNode->DTrain[i], Case) ;
                     }
                     NN.setFilled(treeNode->DTrain.len()) ;
                     NN.sortKsmallest(Node->iMain) ;
                     LWRweight = new double[gRT->NoCases+1] ;
                     double *x = new double[Node->iMain+1] ;
                     double *y = new double[Node->iMain+1] ;
                     int ModelSize = gRT->noNumeric;
                     double *a = new double[ModelSize+1] ;
                     for (i=NN.filled()-1 ; i > NN.filled()-1-Node->iMain ; i--) 
                     {
                        LWRweight[NN[i].value] = sqrt(exp(-sqr(NN[i].key/Node->dMain)/2.0)) ;
                        x[NN.filled()-i] = NN[i].value ;
                        y[NN.filled()-i] = gRT->NumData(NN[i].value, 0) ;
                        //y[NN.filled()-i] = LWRweight[NN[i].value] * (gRT->NumData(NN[i].value, 0) - gRT->NumData(Case, 0)) ;
 
                     }
                     currentNode = treeNode ;
                     currentCase = Case ;
                     svdLWLR(x, y, Node->iMain, a);
                     // we have shifted the origin to (0,0,..0,1) - that's why we consider just constant
                     double predict = a[ModelSize] ;
                     // for (i=1 ; i < ModelSize ; i++)
                     //    predict += a[i] * gRT->NumData(Case, i) ;
                
                     delete [] x ;
                     delete [] a ;
                     delete [] LWRweight ;
                     delete [] y ;

                     return predict ;
                } 
 

        default:    merror("exprReg::predict","invalid operator") ;
                    return 0.0 ;

    }
}

void exprReg::svdLWLR(double *x, double *y, int TrainSize, double *a)
{
      int i ;
      // double *x = new double[TrainSize+1] ;
      // double *y = new double[TrainSize+1] ;
      double *sig = new double[TrainSize+1] ;
      for (i=1 ; i <= TrainSize ; i++)
      {
         // x[i+1] = DTrain[i] ;
         // y[i] = LWRweight[i]*gRT->NumData(round(x[i]), 0) ;
         sig[i] = 1.0 ;
      }
      int modelSize = gRT->noNumeric ;
      // double *a = new double[modelSize+1] ;
      double **u= new double*[TrainSize+1] ;
      for (i=1 ; i <= TrainSize ; i ++)
        u[i] = new double[modelSize+1] ;
      double **v= new double*[modelSize+1] ;
      for (i=1 ; i <= modelSize ; i ++)
        v[i] = new double[modelSize+1] ;
      double *w = new double[modelSize+1] ;
      double chiSquare ;
      marray<int> Mask(modelSize+1, 1) ;

	  svdfit(gRT, x,y,sig,TrainSize,a, Mask, modelSize,u,v,w,&chiSquare,ContWDataRetriever) ;
    
//      delete [] x ;
//      delete [] y ;
      delete [] sig ;
//      delete [] a ;
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
//                        descriptionString
//                        -----------------
//
//     returns the string describing the exprRegession
//
//************************************************************
char* exprReg::descriptionString(void)
{
   if (root)
      return descriptionString(root) ;
   else
      merror("exprReg::descriptionString","Cannot print  nonexistent model") ;
   return 0 ;
}


char* exprReg::descriptionString(exprRegNode* Node)
{
    #if defined(DEBUG)
       if (!Node)
          merror("exprReg::descriptionString", "Invalid structure of model") ;
    #endif
    char *leftString, *rightString, *result ;
    switch(Node->nodeType)
    {
        case plus:
                    leftString = descriptionString(Node->left) ;
                    rightString = descriptionString(Node->right) ;
                    result = new char[strlen(leftString)+strlen(rightString)+4] ;
                    strcpy(result,leftString) ;
                    strcat(result," + ") ;
                    strcat(result,rightString) ;
                    delete [] leftString ;
                    delete [] rightString ;
                    return result ;

        case minus:
                    leftString = descriptionString(Node->left) ;
                    rightString = descriptionString(Node->right) ;
                    result = new char[strlen(leftString)+strlen(rightString)+4] ;
                    strcpy(result,leftString) ;
                    strcat(result," - ") ;
                    strcat(result,rightString) ;
                    delete [] leftString ;
                    delete [] rightString ;
                    return result ;

        case times:
                    leftString = descriptionString(Node->left) ;
                    rightString = descriptionString(Node->right) ;
                    result = new char[strlen(leftString)+strlen(rightString)+8] ;
                    snprintf(result, strlen(leftString)+strlen(rightString)+8, "(%s) * (%s)",leftString,rightString) ;
                    delete [] leftString ;
                    delete [] rightString ;
                    return result ;

        case constTimesAttr:
                    result = new char[strlen(gRT->AttrDesc[gRT->ContIdx[Node->iMain]].AttributeName)+32] ;
                    snprintf(result, strlen(gRT->AttrDesc[gRT->ContIdx[Node->iMain]].AttributeName)+32, "%.5f*%s",Node->dMain,gRT->AttrDesc[gRT->ContIdx[Node->iMain]].AttributeName) ;
                    return result ;

        case floatConstant:
                    result = new char[32] ;
                    snprintf(result, 32, "%.5f",Node->dMain) ;
                    return result ;

        case kNNreg:
                    result = new char[32] ;
                    snprintf(result, 32, "%d-NN of %d",Node->iMain, Node->iAux) ;
                    return result ;

        case kernelRegression:
                    result = new char[64] ;
                    snprintf(result, 64, "Gaussopt->nnKernelWidthReg(%d,%.2f) of %d",Node->iMain, Node->dMain, Node->iAux) ;
                    return result ;

        case LWLR:
                    result = new char[64] ;
                    snprintf(result, 64, "LWLR(%d,%.2f) of %d",Node->iMain, Node->dMain, Node->iAux) ;
                    return result ;

        default:    merror("exprReg::descriptionString","invalid operator") ;
                    return 0 ;

    }
}


//************************************************************
//
//                        mdlCost
//                        ------------
//
//     computes the code lenghth of the model
//
//************************************************************
double exprReg::mdlCost(int noAttributes)
{
  // coding the length of the exprRegession
  double codeLen = mlog2((double)(noAttributes+1)) ; //  log2(noAttributes and constant term)
  // selection of attributes
  marray<double> Multinom(2,0.0) ;  
  Multinom[0] = noCoefficients() ;
  Multinom[1] = noAttributes+1 - Multinom[0] ;
  Multinom.setFilled(2)  ;
  codeLen += multinomLog2(Multinom) ;

  // codes for coefficients
  if (root) 
    codeLen +=  mdlExprCost(root) ; 
  return codeLen ; 
}
   

//************************************************************
//
//                        mdlExprCost
//                        ------------
//
//     computes the code lenghth of the model
//
//************************************************************
double exprReg::mdlExprCost(exprRegNode* Node)
{
    #if defined(DEBUG)
	if (!Node) {
		merror("exprReg::mdlExprCost", "Invalid structure of model");
		return -DBL_MAX;
	}
    #endif
    switch(Node->nodeType)
    {
        case plus:
        case minus:
        case times:
                  return mdlExprCost(Node->left) + mdlExprCost(Node->right) ;

        case constTimesAttr:
        case floatConstant:
          {
             return 1.0 + mdlIntEncode( Node->dMain/gRT->opt->mdlModelPrecision ) ;
          }
        default:    merror("exprReg::mdlExprCost","invalid operator") ;
                    return 0.0 ;

    }
}



//************************************************************
//
//                        mdlPointCost
//                        ------------
//
//     computes the code lenghth for point model
//
//************************************************************
double exprReg::mdlPointCost(exprRegNode* Node)
{
    #if defined(DEBUG)
	if (!Node) {
		merror("exprReg::predict", "Invalid structure of model");
		return -DBL_MAX;
	}
    #endif
    switch(Node->nodeType)
    {
        case floatConstant:
                    return 1.0 +   mdlIntEncode(Node->dMain/gRT->opt->mdlModelPrecision) ;
                                // log2( lround(labs(long(Node->dMain/Precision))) ) ;

        default:    merror("exprReg::mdlPointCost","invalid operator") ;
                    return 0.0 ;

    }
}



//************************************************************
//
//                        examplesDistance
//                        ----------------
//
//     finds the distance between two examples in attribute space
//
//************************************************************
double exprReg::examplesDistance(binnodeReg *treeNode, int I1, int I2) 
{
    int i ;
    double distance = 0.0;

    for (i=0 ; i < gRT->noDiscrete ; i++)
       distance += DAdiff(treeNode, i, I1, I2) ;

    for (i=1 ; i < gRT->noNumeric ; i++)
       distance += CAdiff(treeNode, i, I1, I2) ;

    return distance ;
}


// ***************************************************************************
//
//                   CAdiff
//              diff function for continuous attribute
//
// ***************************************************************************
double exprReg::CAdiff(binnodeReg *treeNode, int AttrIdx, int I1, int I2)
{
   double cV1 = gRT->NumData(I1, AttrIdx) ;
   double cV2 = (*(gRT->nData))(I2, AttrIdx) ;
   if (isNAcont(cV1))
      cV1 = treeNode->NAnumValue[AttrIdx] ;
   if (isNAcont(cV2))
      cV2 = treeNode->NAnumValue[AttrIdx] ;
   #if defined(RAMP_FUNCTION)
       return CARamp(AttrIdx, fabs(cV2 - cV1) ) ;
   #else
      return  fabs(cV2 - cV1) / gRT->valueInterval[AttrIdx] ;
   #endif
}



// ***************************************************************************
//
//                   DAdiff
//              diff function of discrete attribute
//
// ***************************************************************************
double exprReg::DAdiff(binnodeReg *treeNode, int AttrIdx, int I1, int I2)
{

  // we assume that missing value has value 0
  int dV1 = gRT->DiscData(I1, AttrIdx) ;
  int dV2 = (*(gRT->dData))(I2, AttrIdx) ;
  if (dV1 == NAdisc)
     dV1 = treeNode->NAdiscValue[AttrIdx] ;
  if (dV2 == NAdisc)
     dV2 = treeNode->NAdiscValue[AttrIdx] ;
   if (dV1 == dV2)
      return  0.0 ;
   else
      return 1.0 ;
}

// ***************************************************************************
//
//                    CARamp
//          ramp function of continuous attribute (or class)
//
// ***************************************************************************
#if defined(RAMP_FUNCTION)
inline double exprReg::CARamp(int AttrIdx, double distance)
{
  if (distance >= differentDistance[AttrIdx])
     return 1.0 ;
 
  if (distance <= equalDistance[AttrIdx])
     return 0.0 ;

  return  (distance - equalDistance[AttrIdx]) * CAslope[AttrIdx] ;
}
#endif



