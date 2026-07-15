/********************************************************************
*
*   Name:          module EXPR
*
*   Description:      deals with representation of
*                          booleanT expressions
*
*********************************************************************/

#include <cfloat>

#include "general.h"
#include "expr.h"
#include "bintree.h"
#include "dataStore.h"
#include "ftree.h"
#include "error.h"
#include "utils.h"
#include "estimator.h"
#include "options.h"

using namespace std ;

expr::expr(expr &Copy)
{
   root = 0 ;
   copy(Copy) ;
}

expr::~expr()
{
    destroy() ;
	gFT = 0 ;
}


//**********************************************************************
//
//                      destroy
//                      -----------
//
//      deallocates space consumed by booleanT expression
//
//**********************************************************************
void expr::destroy(void) 
{ 
	if (root) 
	  destroy(root);  
	root = 0 ;
}


void expr::destroy(exprNode *node)
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
//      copies source booleanT expression to target
//
//**********************************************************************
void expr::copy(const expr &Source)
{
   modelType = Source.modelType ;
   gFT = Source.gFT ;

   if (root)
     destroy(root) ;
   if (Source.root)
     dup(Source.root,root) ;
   else
     root = 0 ;
 
   majorClass = Source.majorClass ;
   SBclAttrVal = Source.SBclAttrVal ;
   SBattrVal = Source.SBattrVal ;
   SBcl = Source.SBcl ;
   equalDistance = Source.equalDistance ;
   differentDistance = Source.differentDistance ;
   CAslope = Source.CAslope ;
}


//**********************************************************************
//
//                      operator =
//                      --------
//
//      copies source booleanT expression to target
//
//**********************************************************************
expr& expr::operator= (const expr &Source)
{
    copy(Source) ;
	return *this ;
}

//**********************************************************************
//
//                      dup
//                      -------
//
//      duplicates source booleanT expression into target
//
//**********************************************************************
void expr::dup(exprNode *Source, PexprNode &Target)
{
    Target = new exprNode ;
	Target->iMain = Source->iMain ;
	Target->iAux = Source->iAux ;
//    Target->nodeType = Source->nodeType ;
//    Target->majorClass = Source->majorClass ;
 //   Target->instances = Source->instances ;

    if (Source->left)
      dup(Source->left, Target->left) ;
    else
      Target->left = 0 ;
    if (Source->right)
      dup(Source->right, Target->right ) ;
    else
      Target->right = 0 ;
}





void expr::createMajority(int Value)
{
    destroy() ;
    modelType = majority ;
    majorClass = Value ;
}



void expr::createKNN(void)
{
    destroy() ;
    modelType = kNN ;

    differentDistance.create(gFT->noNumeric) ;
    equalDistance.create(gFT->noNumeric) ;
    CAslope.create(gFT->noNumeric) ;

    int i ;
    for (i=0 ; i < gFT->noNumeric ; i++)
    {
        differentDistance[i] = gFT->AttrDesc[gFT->ContIdx[i]].DifferentDistance ;
        equalDistance[i] = gFT->AttrDesc[gFT->ContIdx[i]].EqualDistance ;
        if (differentDistance[i] != equalDistance[i])
            CAslope[i] = double(1.0)/(differentDistance[i] - equalDistance[i]) ;
         else
            CAslope[i] = DBL_MAX ;
    }
}


void expr::createKNNkernel(void)
{
    destroy() ;
    modelType = kNNkernel ;

    differentDistance.create(gFT->noNumeric) ;
    equalDistance.create(gFT->noNumeric) ;
    CAslope.create(gFT->noNumeric) ;

    int i ;
    for (i=0 ; i < gFT->noNumeric ; i++)
    {
        differentDistance[i] = gFT->AttrDesc[gFT->ContIdx[i]].DifferentDistance ;
        equalDistance[i] = gFT->AttrDesc[gFT->ContIdx[i]].EqualDistance ;
        if (differentDistance[i] != equalDistance[i])
            CAslope[i] = double(1.0)/(differentDistance[i] - equalDistance[i]) ;
         else
            CAslope[i] = DBL_MAX ;
    }
}


void expr::createSimpleBayes(estimation &Estimator, binnode *treeNode)
{
    destroy() ;
	modelType = simpleBayes ;

	int noAttr = gFT->noAttr ;
	int iCont, iDisc, iClass, iAttr, iEx, iVal ;
    double contValue ;

	// discretize numeric attribute
    Boundary.create(gFT->noNumeric) ;
	switch  (gFT->opt->bayesDiscretization)
	{
		case discrGreedy:
			 for (iCont = 0 ; iCont < Estimator.noNumeric ; iCont++)
				Estimator.discretizeGreedy(iCont, 0, Boundary[iCont], Estimator.noDiscrete) ;
			 break ;
		case discrEqFreq:
			 for (iCont = 0 ; iCont < Estimator.noNumeric ; iCont++)
				Estimator.discretizeEqualFrequency(iCont, gFT->opt->discretizationIntervals, Boundary[iCont]) ;
			 break ;

		case discrEqWidth:
			 for (iCont = 0 ; iCont < Estimator.noNumeric ; iCont++)
				Estimator.discretizeEqualWidth(iCont, gFT->opt->discretizationIntervals, Boundary[iCont]) ;
			 break ;

		default: merror("expr::createSimpleBayes", "invalid discretization type for simple bayes") ;
	}

	// create appropriate data structures
    SBclAttrVal.create(gFT->noClasses+1) ;
	for (iClass = 1 ; iClass <= gFT->noClasses ; iClass++)
	{
    	iCont = 0 ; 
		SBclAttrVal[iClass].create(noAttr+1) ;
		for (iAttr = 1 ; iAttr <= noAttr ; iAttr++)
			if (gFT->AttrDesc[iAttr].continuous)
			{
	 			SBclAttrVal[iClass][iAttr].create(Boundary[iCont].filled()+2,0.0) ;
                iCont++ ;
			}
			else {
				SBclAttrVal[iClass][iAttr].create(gFT->AttrDesc[iAttr].NoValues+1, 0.0) ;
            }
	}

	// fill the data structure with the weights
    for (iDisc = 1 ; iDisc < Estimator.noDiscrete ; iDisc++)
       for (iEx = 0 ; iEx < Estimator.TrainSize ; iEx++)
		    SBclAttrVal[Estimator.DiscValues(iEx,0)][gFT->DiscIdx[iDisc]][Estimator.DiscValues(iEx, iDisc)] ++ ;

    for (iCont = 0 ; iCont < Estimator.noNumeric ; iCont++)
       for (iEx = 0 ; iEx < Estimator.TrainSize ; iEx++)
	   {
		   contValue = Estimator.NumValues(iEx, iCont)  ;
		   if ( ! isNAcont(contValue))
		      SBclAttrVal[Estimator.DiscValues(iEx, 0)][gFT->ContIdx[iCont]][(Boundary[iCont]).lessEqPlace(contValue)+1] ++ ;
           else
              SBclAttrVal[Estimator.DiscValues(iEx, 0 )][gFT->ContIdx[iCont]][0]++ ;
       }
    SBcl.create(gFT->noClasses+1, 0.0) ;
	for (iClass = 1 ; iClass <=gFT->noClasses ; iClass++)
		SBcl[iClass] = (treeNode->Classify[iClass]+1)/(treeNode->weight+gFT->noClasses) ;

   
    SBattrVal.create(noAttr+1) ;
	iCont = 0 ;
	for (iAttr=1 ; iAttr <= noAttr ; iAttr++)
    {
	    if (gFT->AttrDesc[iAttr].continuous)
		{
	 		SBattrVal[iAttr].create(Boundary[iCont].filled()+2,0.0) ;
            iCont++ ;
		}
		else {
			SBattrVal[iAttr].create(gFT->AttrDesc[iAttr].NoValues+1, 0.0) ;
		}
			
		for (iVal = 0 ; iVal < SBattrVal[iAttr].len() ; iVal ++)
		   for (iClass = 1 ; iClass <= gFT->noClasses ; iClass++)
                SBattrVal[iAttr][iVal] += SBclAttrVal[iClass][iAttr][iVal] ;
	}
}

double expr::smoothingParameter(int smoothingType){
	double m = 0 ;
	switch (smoothingType){
				case 0: // no smoothing
					    m= 0; break;
				case 1: // additive smoothing
				case 3: // m-estimate smoothing
					    m = gFT->opt->smoothingValue; break ;
				case 2: // pure Laplace
					    m = 1 ; break ;
				case 4: // Zadrozny-Elkan form of m-estimate: m*p_min=smoothingValue
            		    m = gFT->opt->smoothingValue / gFT->AttrDesc[0].valueProbability[gFT->minClass]; break ;
                default: m = 0 ;
				}
	return m ;
}

void expr::predict(binnode *treeNode, int Case, marray<double> &probDist)
{
    switch(modelType)
    {
  
        case majority:
			{
				int i ;
				double m = smoothingParameter(gFT->opt->smoothingType) ;
                if (gFT->opt->smoothingType == 0 || gFT->opt->smoothingType == 1 || gFT->opt->smoothingType == 2) {
                	// use additive (Laplace) smoothing
                	for (i=1 ; i < probDist.len() ; i++){
                           probDist[i] = (treeNode->Classify[i] + m)
        					              /  (treeNode->weight + m * gFT->noClasses) ;
        			}
                }
                else if (gFT->opt->smoothingType == 3 || gFT->opt->smoothingType == 4){
                	//  m-estimate smoothing
                	for (i=1 ; i < probDist.len() ; i++)
                	{
                		probDist[i] = (treeNode->Classify[i] + m * gFT->AttrDesc[0].valueProbability[i] )
					            		  /  (treeNode->weight + m) ;
                	}
                }
				return ;
            }

        case kNN:
			{
				// find k nearest
									// find k nearest
				marray<sortRec> NN(treeNode->DTrain.filled()) ;
				int i ;
				for (i=0 ; i < treeNode->DTrain.filled() ; i++)
				{
					NN[i].value = treeNode->DTrain[i] ;
					NN[i].key = examplesDistance(treeNode, treeNode->DTrain[i], Case) ;
				}
				NN.setFilled(treeNode->DTrain.filled()) ;
				int k = Mmin(gFT->opt->kInNN, treeNode->DTrain.filled()) ;
				NN.sortKsmallest(k) ;
                  
				probDist.init(0.0) ;
				for (i=NN.filled()-1 ; i > NN.filled()-1-k ; i--)
					probDist[gFT->DiscData(NN[i].value, 0)] += 1.0 ;
            
				for (i=1 ; i <= gFT->noClasses ; i++)
				   probDist[i] /= double(k) ;
				
				return  ;
            } 
 
        case kNNkernel:
			{
                // for short description see e.g. Kukar et al(1999):Analysing and improving
                // the diagnosis of ishaemic heart disease with machine learning.
                // Artificial Intelligence in Medicine 16:25-50

                // find k nearest
               	marray<sortRec> NN(treeNode->DTrain.filled()) ;
				int i ;
				for (i=0 ; i < treeNode->DTrain.filled() ; i++)
				{
					NN[i].value = treeNode->DTrain[i] ;
					NN[i].key = examplesDistance(treeNode, treeNode->DTrain[i], Case) ;
				}
				NN.setFilled(treeNode->DTrain.filled()) ;
				int k = Mmin(gFT->opt->kInNN, treeNode->DTrain.filled()) ;
				NN.sortKsmallest(k) ;
                  
				int noClasses = gFT->AttrDesc[0].NoValues ;
				probDist.init(0.0) ;
                double kr2 = 2*sqr(gFT->opt->nnKernelWidth) ;
				for (i=NN.filled()-1 ; i > NN.filled()-1-k ; i--)
					probDist[gFT->DiscData(NN[i].value, 0)] += exp(-sqr(NN[i].key)/kr2) ;
            
                double sumW = 0 ;
                kr2 = sqrt(2.0*Phi) * gFT->opt->nnKernelWidth ;
				for (i=1 ; i <= noClasses ; i++)
                {
                   probDist[i] /=  kr2 ; 
				   sumW += probDist[i] ;
                }
                for (i=1 ; i <= noClasses ; i++)
				   probDist[i] /= sumW ;
                
				return  ;
            } 
        case simpleBayes:
			{
				int noClasses = gFT->noClasses ;
				int noAttr = gFT->noAttr ;
                int iClass, iAttr, valueIdx, iCont, iDisc ;
				double contValue, denominator, factor ;
                double m = smoothingParameter(gFT->opt->smoothingType) ;
                double pAll = 0.0 ;

				for (iClass = 1 ; iClass <= noClasses ; iClass++)
				{
					probDist[iClass] = SBcl[iClass] ;
					
					iCont = 0 ;
					iDisc = 1 ;
					for(iAttr = 1 ; iAttr <= noAttr ; iAttr++)
					{
            			if (gFT->AttrDesc[iAttr].continuous)
						{
                           contValue = (*(gFT->nData))(Case, iCont) ;
						   if (isNAcont(contValue)) 
							  valueIdx = 0 ;
						   else
						      valueIdx = Boundary[iCont].lessEqPlace(contValue)+1 ;
						   iCont++ ;
						}
						else {
							valueIdx = (*(gFT->dData))(Case, iDisc) ;
							iDisc ++ ;
						}
            			if (gFT->opt->smoothingType == 0 || gFT->opt->smoothingType == 1 || gFT->opt->smoothingType == 2) {
            				// use additive and Laplace smoothing
     						denominator =  (SBattrVal[iAttr][valueIdx] + m * gFT->noClasses) * SBcl[iClass] ;
    						if (denominator > 0)
    						   factor = (SBclAttrVal[iClass][iAttr][valueIdx] + m) / denominator ;
                            else factor = 0 ;
           			}
            			else {
            				// m-estimate smoothing
            				denominator =  (SBattrVal[iAttr][valueIdx] + m) * SBcl[iClass] ;
            				if (denominator > 0)
            					factor = (SBclAttrVal[iClass][iAttr][valueIdx] + m * SBcl[iClass]) / denominator ;
            				else factor = 0 ;
            			}
						if (factor > 0)
						  probDist[iClass] *= factor ;
					}
					pAll += probDist[iClass] ;
				}
				// normalization to probabilities
				for (iClass = 1 ; iClass <= noClasses ; iClass++)
     				probDist[iClass] /= pAll ;


				return ;
  			}

        default:  merror("expr::predict","Cannot evaluate nonexistent model") ;

    }
}



/*
void expr::predict(binnode *treeNode, int Case, exprNode* Node)
{
    #if defined(DEBUG)
       if (!Node)
          merror("expr::predict", "Invalid structure of model") ;
    #endif
}

*/


char* expr::descriptionString(void)
{
	char *result = 0 ;
    switch(modelType)
    {
        case majority:
                    strcpy(result=new char[strlen(gFT->AttrDesc[0].ValueName[majorClass-1])+1],
                           gFT->AttrDesc[0].ValueName[majorClass - 1] ) ;
                    break ;
        case kNN:
                    result = new char[5] ;
                    snprintf(result, 5, "k-NN") ;
                    break ; 
        case kNNkernel:
                    result = new char[20] ;
                    snprintf(result, 20, "k-NN with kernel") ;
                    break ;
        case simpleBayes:
                    result = new char[16] ;
                    snprintf(result, 16, "simple Bayes") ;
                    break ;
			
        default:    merror("expr::descriptionString","Cannot print nonexistent model") ;
					break ;
    }
   return result ;
}


/*
char* expr::descriptionString(exprNode* Node)
{
    #if defined(DEBUG)
       if (!Node)
          merror("expr::descriptionString", "Invalid structure of model") ;
    #endif
	return 0 ;
}
*/

//************************************************************
//
//                        examplesDistance
//                        ----------------
//
//     finds the distance between two examples in attribute space
//
//************************************************************
double expr::examplesDistance(binnode *treeNode, int I1, int I2) 
{
    int i ;
    double distance = 0.0;

    for (i=1 ; i < gFT->noDiscrete ; i++)
       distance += DAdiff(treeNode, i, I1, I2) ;

    for (i=0 ; i < gFT->noNumeric ; i++)
       distance += CAdiff(treeNode, i, I1, I2) ;

    return distance ;
}


// ***************************************************************************
//
//                   CAdiff
//              diff function for numeric attribute
//
// ***************************************************************************
double expr::CAdiff(binnode *treeNode, int AttrIdx, int I1, int I2)
{
   double cV1 = gFT->NumData(I1, AttrIdx) ;
   double cV2 = (*(gFT->nData))(I2, AttrIdx) ;
   if (isNAcont(cV1))
      cV1 = treeNode->NAnumValue[AttrIdx] ;
   if (isNAcont(cV2))
      cV2 = treeNode->NAnumValue[AttrIdx] ;
   #if defined(RAMP_FUNCTION)
       return CARamp(AttrIdx, fabs(cV2 - cV1) ) ;
   #else
      return  fabs(cV2 - cV1) / gFT->valueInterval[AttrIdx] ;
   #endif
}



// ***************************************************************************
//
//                   DAdiff
//              diff function of discrete attribute
//
// ***************************************************************************
double expr::DAdiff(binnode *treeNode, int AttrIdx, int I1, int I2)
{

  // we assume that missing value has value 0
  int dV1 = gFT->DiscData(I1, AttrIdx) ;
  int dV2 = (*(gFT->dData))(I2, AttrIdx) ;
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
//          ramp function of numeric attribute (or class)
//
// ***************************************************************************
#if defined(RAMP_FUNCTION)
double expr::CARamp(int AttrIdx, double distance)
{
  if (distance >= differentDistance[AttrIdx])
     return 1.0 ;
 
  if (distance <= equalDistance[AttrIdx])
     return 0.0 ;

  return  (distance - equalDistance[AttrIdx]) * CAslope[AttrIdx] ;
}
#endif



