// ********************************************************************
// *
// *   Name:          module constrct
// *
// *   Description:      deals with constructs 
// *
// *********************************************************************
#include <cfloat>
#include <cstring>

#include "general.h"
#include "estimatorReg.h"
#include "constrctReg.h"
#include "regtree.h"
#include "error.h"

using namespace std ;



// ********************************************************************
// *
// *             destroy
// *             -------
// *
// *     the eraser of class constructReg 
// *
// *********************************************************************
void constructReg::destroy(constructRegNode *node)
{
    if (node->left)
      destroy(node->left) ;
    if (node->right)
      destroy(node->right) ;

    delete node ;
}

// ********************************************************************
// *
// *             copy   and operator =
// *             ----------------------
// *
// *     the copier of class constructReg 
// *
// *********************************************************************
void constructReg::copy(constructReg &Source)
{
   if (&Source == this)
       return ;

   destroy() ;
   if (Source.root)
     dup(Source.root,root) ;
   else
     root = 0 ;
   gRT = Source.gRT ;
   countType = Source.countType ;
   compositionType = Source.compositionType ;
   leftValues = Source.leftValues ;
   splitValue = Source.splitValue ;
   noValues = Source.noValues ;
   splitEstimator = Source.splitEstimator ;
}

constructReg& constructReg::operator= (constructReg &X) 
{
   copy(X) ;
   return *this ;
}


// **********************************************************************
//
//                      dup
//                      -------
//
//      duplicates source constructReg node into target
//
// **********************************************************************
void constructReg::dup(const constructRegNode *Source, PconstructRegNode &Target)
{
    Target = new constructRegNode ;
    Target->nodeType = Source->nodeType ;
    Target->attrIdx = Source->attrIdx ;
    Target->valueIdx = Source->valueIdx ;
    Target->lowerBoundary = Source->lowerBoundary ;
    Target->upperBoundary = Source->upperBoundary ;

    if (Source->left)
      dup(Source->left, Target->left) ;
    else
      Target->left = 0 ;
    if (Source->right)
      dup(Source->right, Target->right ) ;
    else
      Target->right = 0 ;
}




// ********************************************************************
// *
// *             descriptionString
// *             -----------------
// *
// *    fills in the description of constructReg 
// *
// *********************************************************************
void constructReg::descriptionString(char* const Str)
{
   char *dscrStr = description(root) ;

   switch(countType)
   {
      case aCONTINUOUS:
            snprintf(Str, MaxFeatureStrLen, "%s <= %f", dscrStr, splitValue) ;
           break ;
      case aDISCRETE:
           snprintf(Str, MaxFeatureStrLen, "%s", dscrStr) ;
           if (compositionType == cSINGLEattribute)
           { 
             strcat(Str, "= (") ; 
             int pos = 1 ;
             // first value
             while (pos < leftValues.len() && (leftValues[pos] == mFALSE))
                pos++ ;
             if (pos < leftValues.len())
                strcat(Str, gRT->AttrDesc[gRT->DiscIdx[root->attrIdx]].ValueName[pos-1]) ;
             else
                merror("constructReg::descriptionString","invalid binarization detected") ;
             for (int i=pos+1 ; i < leftValues.len() ; i++)
                if (leftValues[i])
                {
                   strcat(Str, " | ") ;
                   strcat(Str, gRT->AttrDesc[gRT->DiscIdx[root->attrIdx]].ValueName[i-1]) ;
                }
             strcat(Str,")") ;
           }
           break ;
               
      default: merror("constructReg::descriptionString","invalid count type") ;
   } 
   delete [] dscrStr ;
}


// ********************************************************************
// *
// *             description
// *             ------------
// *
// *    recursively collects the description of constructReg 
// *
// *********************************************************************
char* constructReg::description(constructRegNode *Node) 
{
   char *Str = new char[MaxFeatureStrLen] ;
   switch(Node->nodeType)
   {
      case cnDISCattrValue:
           snprintf(Str, MaxFeatureStrLen, "(%s = %s)", gRT->AttrDesc[gRT->DiscIdx[Node->attrIdx]].AttributeName,
                                   gRT->AttrDesc[gRT->DiscIdx[Node->attrIdx]].ValueName[Node->valueIdx-1] ) ;
           break ;          
      case cnCONTattrValue:
           if (Node->lowerBoundary == -DBL_MAX)
              snprintf(Str, MaxFeatureStrLen, "(%s <= %.3f)", gRT->AttrDesc[gRT->ContIdx[Node->attrIdx]].AttributeName,
                                           Node->upperBoundary ) ;
           else
             if (Node->upperBoundary == DBL_MAX)
                snprintf(Str, MaxFeatureStrLen, "(%s > %.3f)", gRT->AttrDesc[gRT->ContIdx[Node->attrIdx]].AttributeName,
                                           Node->lowerBoundary ) ;
             else
                snprintf(Str, MaxFeatureStrLen, "(%.3f < %s <= %.3f)", Node->lowerBoundary,
                                                    gRT->AttrDesc[gRT->ContIdx[Node->attrIdx]].AttributeName,
                                                    Node->upperBoundary ) ;
           break ;
      
      case cnCONTattribute:
           snprintf(Str, MaxFeatureStrLen, "%s", gRT->AttrDesc[gRT->ContIdx[Node->attrIdx]].AttributeName) ;
           break ;
      
      case cnDISCattribute:
           snprintf(Str, MaxFeatureStrLen, "%s", gRT->AttrDesc[gRT->DiscIdx[Node->attrIdx]].AttributeName) ;
           break ;

      case cnAND:
         {
            char *leftStr = description(Node->left) ;
            char *rightStr = description(Node->right) ;
            snprintf(Str, MaxFeatureStrLen, "%s & %s", leftStr, rightStr) ;
            delete [] leftStr ;
            delete [] rightStr ;
         }
         break ;
      case cnTIMES:
         {
            char *leftStr = description(Node->left) ;
            char *rightStr = description(Node->right) ;
            snprintf(Str, MaxFeatureStrLen, "%s * %s", leftStr, rightStr) ;
            delete [] leftStr ;
            delete [] rightStr ;
         }
         break ;
      case cnPLUS:
         {
            char *leftStr = description(Node->left) ;
            char *rightStr = description(Node->right) ;
            snprintf(Str, MaxFeatureStrLen, "%s + %s", leftStr, rightStr) ;
            delete [] leftStr ;
            delete [] rightStr ;
         }
         break ;

      default: 
            merror("constructReg::description","invalid type of node") ;
            strcpy(Str, "ERROR(constructReg::description)") ;
   }
   return Str ;
}



// ********************************************************************
// *
// *             degreesOfFreedom
// *             ----------------
// *
// *     return the number of building attrubutes in constructReg 
// *
// *********************************************************************
int constructReg::degreesOfFreedom(void) 
{
   switch (compositionType)
   {
      case cSINGLEattribute:
           return 1 ;
      case cCONJUNCTION:
      case cSUM:
      case cPRODUCT:
           return degreesOfFreedom(root) ;
           
      default: 
           merror("constructReg::degreesOfFreedom","invalid composition") ;
           return 0 ;
   } 

}


// ********************************************************************
// *
// *             degreesOfFreedom
// *             ----------------
// *
// *     return the number of building attrubutes in constructReg 
// *
// *********************************************************************
int constructReg::degreesOfFreedom(constructRegNode *Node) 
{
   switch (Node->nodeType)
   {
      case cnDISCattrValue:
      case cnCONTattrValue:
      case cnDISCattribute:
      case cnCONTattribute:
                         return 1 ;

      case cnAND:
      case cnTIMES:
      case cnPLUS:
                return degreesOfFreedom(Node->left) + 
                       degreesOfFreedom(Node->right) ;
      default: 
           merror("constructReg::degreesOfFreedom","invalid node type") ;
           return 0 ;
   } 

}



// ********************************************************************
// *
// *             continuousValue/1
// *             ---------------
// *
// *     returns the value of continuous constructReg
// *
// *********************************************************************/
double constructReg::continuousValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx) 
{
#if defined(DEBUG)
  if (countType != aCONTINUOUS) 
    merror("constructReg::continuousValue", "invalid count of constructReg") ;
#endif

  switch (compositionType)
  {
     case cSINGLEattribute:
       return NumData(caseIdx, root->attrIdx) ;

     case cSUM:
     case cPRODUCT:
        return continuousValue(DiscData,NumData,caseIdx, root) ;
     
     default:
        merror("constructReg::continuousValue", "invalid composition type detected") ;    
        return -DBL_MAX ;
  }

}



// ********************************************************************
// *
// *             continuousValue/2
// *             ---------------
// *
// *     returns the value of continuous constructReg 
// *
// *********************************************************************/
double constructReg::continuousValue(mmatrix<int> &DiscData, mmatrix<double> &NumData, int caseIdx, constructRegNode* Node) 
{
   switch (Node->nodeType)
   {
     case cnCONTattribute:
        return NumData(caseIdx, Node->attrIdx) ;
     case cnTIMES:
       {
         double leftValue, rightValue ;
         leftValue = continuousValue(DiscData,NumData,caseIdx, Node->left) ;
         rightValue = continuousValue(DiscData,NumData,caseIdx, Node->right) ;
         if (isNAcont(leftValue) || isNAcont(rightValue))
           return NAcont ;
         return leftValue*rightValue ; 
       } 
     case cnPLUS:
       {
         double leftValue, rightValue ;
         leftValue = continuousValue(DiscData,NumData,caseIdx, Node->left) ;
         rightValue = continuousValue(DiscData,NumData,caseIdx, Node->right) ;
         if (isNAcont(leftValue) || isNAcont(rightValue))
           return NAcont ;
         return leftValue+rightValue ; 
       } 
     default: 
        merror("constructReg::continuousValue/2", "invalid node type") ;
        return NAcont ;
  }
}


// ********************************************************************
// *
// *             discreteValue/1
// *             -------------
// *
// *     returns the value of discrete constructReg
// *
// *********************************************************************/
char constructReg::discreteValue(mmatrix<int> &DiscData, mmatrix<double> &NumData,int caseIdx) 
{
#if defined(DEBUG)
  if (countType != aDISCRETE) 
    merror("constructReg::discreteValue", "invalid count of constructReg") ;
#endif

  
  switch (compositionType)
  {
     case cSINGLEattribute:
       return DiscData(caseIdx, root->attrIdx) ;
     case cCONJUNCTION:
       return discreteValue(DiscData,NumData, caseIdx, root) ;

     default:
       merror("constructReg::discreteValue/1", "invalid composition type of constructReg") ;
       return NAdisc ;
  }
}


// ********************************************************************
// *
// *             discreteValue/2
// *             -------------
// *
// *     returns the value of discrete constructReg 
// *
// *********************************************************************/
char constructReg::discreteValue(mmatrix<int> &DiscData, mmatrix<double> &NumData,int caseIdx, constructRegNode* Node) 
{
   char discValue, anotherDiscValue ;
   double contValue ;
   switch (Node->nodeType)
   {
     case cnDISCattribute:
        return DiscData(caseIdx, Node->attrIdx) ;
     case cnDISCattrValue:
        discValue = DiscData(caseIdx, Node->attrIdx) ;
        if (discValue == NAdisc)
           return NAdisc ;
        if (discValue == Node->valueIdx)
          return 1 ;
        else 
          return 2 ;
     case cnCONTattrValue:
        contValue = NumData(caseIdx, Node->attrIdx) ;
        if (isNAcont(contValue))
           return NAdisc ;
        if (contValue > Node->lowerBoundary &&
            contValue <= Node->upperBoundary)
          return 1 ;
        else 
          return 2 ;
     case cnAND:
        discValue = discreteValue(DiscData,NumData,caseIdx, Node->left) ;
        anotherDiscValue = discreteValue(DiscData,NumData,caseIdx, Node->right) ;
        if (discValue == NAdisc || anotherDiscValue == NAdisc)
           return NAdisc ;
        if (discValue == 1 && anotherDiscValue == 1)
           return 1 ;
        else
           return 2 ;

     default: 
        merror("constructReg::discreteValue/2", "invalid node type") ;
        return NAdisc ;
  }
}


// ********************************************************************
// *
// *             createSingle
// *             ------------
// *
// *     creates traditional one-attribute constructReg 
// *
// *********************************************************************
void constructReg::createSingle(int bestIdx, attributeCount count) 
{
   destroy() ;
   countType = count ;
   compositionType = cSINGLEattribute ;
   root = new constructRegNode ;
   root->left = root->right=0 ;
   root->attrIdx = bestIdx ;
   
   switch (countType)
   {
      case aCONTINUOUS:
        root->nodeType = cnCONTattribute ;
        break ;
      
      case aDISCRETE:
        root->nodeType = cnDISCattribute ;
        break ;

      default: merror("constructReg::singleAttribute","invalid count type") ;
   }

}


// ********************************************************************
// *
// *             conjoin
// *             ------------
// *
// *     makes conjunction of two constructs
// *
// *********************************************************************
void constructReg::Conjoin(constructReg &First, constructReg &Second)
{
   destroy() ;
   countType = aDISCRETE ;
   compositionType = cCONJUNCTION ;
   root = new constructRegNode ;
   root->nodeType = cnAND ;
   dup(First.root, root->left) ;
   dup(Second.root, root->right) ;
}


// ********************************************************************
// *
// *             add
// *             ----
// *
// *     makes sum of two constructs
// *
// *********************************************************************
void constructReg::add(constructReg &First, constructReg &Second)
{
   destroy() ;
   countType =  aCONTINUOUS;
   compositionType = cSUM ;
   root = new constructRegNode ;
   root->nodeType = cnPLUS ;
   dup(First.root, root->left) ;
   dup(Second.root, root->right) ;
}



// ********************************************************************
// *
// *             multiply
// *             --------
// *
// *     makes product of two constructs
// *
// *********************************************************************
void constructReg::multiply(constructReg &First, constructReg &Second)
{
   destroy() ;
   countType =  aCONTINUOUS;
   compositionType = cPRODUCT ;
   root = new constructRegNode ;
   root->nodeType = cnTIMES ;
   dup(First.root, root->left) ;
   dup(Second.root, root->right) ;
}


// ********************************************************************
// *
// *            containsAttribute 
// *            -----------------
// *
// *     checks if constructReg contains attribute of
// *         given attribute's value
// *
// *********************************************************************
booleanT constructReg::containsAttribute(constructReg &AttrConstruct)
{
#if defined(DEBUG)
   if (AttrConstruct.root->left != 0 || AttrConstruct.root->right != 0 ||
       (AttrConstruct.root->nodeType != cnCONTattrValue && 
        AttrConstruct.root->nodeType != cnDISCattrValue &&
        AttrConstruct.root->nodeType != cnCONTattribute &&
        AttrConstruct.root->nodeType != cnDISCattribute) )
      merror("constructReg::containsAttribute", "unexpected constructReg was given as input") ;
#endif

    if (root)
       return containsAttribute(root, AttrConstruct.root->attrIdx) ;
    else
       return mFALSE ;
}


// ********************************************************************
// *
// *            containsAttribute 
// *            -----------------
// *
// *     checks if constructReg node or its subnodes contain 
// *            given attribute
// *
// *********************************************************************
booleanT constructReg::containsAttribute(constructRegNode *Node, int attributeIdx)
{
   if (Node->attrIdx == attributeIdx)
      return mTRUE ;
   
   if (Node->left)
      if (containsAttribute(Node->left, attributeIdx))
         return mTRUE ;
   
   if (Node->right)
     return containsAttribute(Node->right, attributeIdx) ;
        
   return mFALSE ;
   
}



// ********************************************************************
// *
// *            operator== 
// *            -----------
// *
// *     checks if two constructs are the same
// *
// *********************************************************************
int constructReg::operator== (constructReg &X) 
{
   if (countType != X.countType || compositionType != X.compositionType)
      return 0 ;
   switch (compositionType)
   {
     case cSINGLEattribute:
       if (root->nodeType != X.root->nodeType || 
           root->attrIdx != X.root->attrIdx)
          return 0 ;
       else
          return 1;
     case  cCONJUNCTION:
     {
       
       int noConjs = degreesOfFreedom() ;
       int XnoConjs = X.degreesOfFreedom() ;
       if (noConjs != XnoConjs) 
         return 0 ;
       marray<int> discAttrIdxs(noConjs), AttrVals(noConjs), contAttrIdxs(noConjs) ;
       marray<int> XdiscAttrIdxs(XnoConjs), XAttrVals(XnoConjs), XcontAttrIdxs(XnoConjs) ;
       marray<double> lowerBndys(noConjs), upperBndys(noConjs) ;
       marray<double> XlowerBndys(XnoConjs), XupperBndys(XnoConjs) ;

       flattenConjunct(discAttrIdxs, AttrVals, contAttrIdxs, lowerBndys, upperBndys) ;
       X.flattenConjunct(XdiscAttrIdxs, XAttrVals, XcontAttrIdxs, XlowerBndys, XupperBndys) ;
       
       if (discAttrIdxs.filled() != XdiscAttrIdxs.filled() || 
           contAttrIdxs.filled() != XcontAttrIdxs.filled() )
         return 0 ;
       
       int i, j ;
       booleanT noSuchComponent ;
       // check attribute values of discrete attributes
       for (i=0 ; i < discAttrIdxs.filled() ; i++)
       {
         noSuchComponent = mTRUE ;
         for (j=0 ; j < XdiscAttrIdxs.filled() ; j++)
            if (discAttrIdxs[i] == XdiscAttrIdxs[j] && AttrVals[i] == XAttrVals[j])
            {
               noSuchComponent = mFALSE ;
               break ;
            }
         if (noSuchComponent)
            return 0 ;
       }

       // check intervals of continuous attributes
       for (i=0 ; i < contAttrIdxs.filled() ; i++)
       {
         noSuchComponent = mTRUE ;
         for (j=0 ; j < XcontAttrIdxs.filled() ; j++)
            if (contAttrIdxs[i] == XcontAttrIdxs[j] && lowerBndys[i] == XlowerBndys[j]
                && upperBndys[i] == XupperBndys[j])
            {
               noSuchComponent = mFALSE ;
               break ;
            }
         if (noSuchComponent)
            return 0 ;
       }
       // they are the same
       return 1; 
     }
     case cSUM:
     case cPRODUCT:
     {
       
       int noConts = degreesOfFreedom() ;
       int XnoConts = X.degreesOfFreedom() ;
       if (noConts != XnoConts) 
         return 0 ;
       marray<int> contAttrIdxs(noConts), XcontAttrIdxs(XnoConts) ;

       flattenContConstruct(contAttrIdxs) ;
       X.flattenContConstruct(XcontAttrIdxs) ;
       
       if (contAttrIdxs.filled() != XcontAttrIdxs.filled() )
         return 0 ;
       
       int i, j ;
       booleanT noSuchComponent ;
       // check continuous attributes
       for (i=0 ; i < contAttrIdxs.filled() ; i++)
       {
         noSuchComponent = mTRUE ;
         for (j=0 ; j < XcontAttrIdxs.filled() ; j++)
           if (contAttrIdxs[i] == XcontAttrIdxs[j]) 
           {
              noSuchComponent = mFALSE ;
              break ;
           }
         if (noSuchComponent)
            return 0 ;
       }
       // they are the same
       return 1; 
     }

     case cXofN:
     default:
       merror("constructReg::operator==", "invalid composition type") ;
       return 0 ;
   }
}


// ********************************************************************
// *
// *            flattenConjunct 
// *            --------------
// *
// *     gives flat representation of conjunctive constructReg 
// *
// *********************************************************************
void constructReg::flattenConjunct(marray<int> &discAttrIdxs, marray<int> &AttrVals, 
                     marray<int> &contAttrIdxs, marray<double> &lowerBndys, marray<double> &upperBndys) 
{
  // asumption: there is enough space in array to hold values
  discAttrIdxs.setFilled(0) ;
  AttrVals.setFilled(0) ;
  contAttrIdxs.setFilled(0) ;
  lowerBndys.setFilled(0) ;
  upperBndys.setFilled(0) ;

  if (root) 
     flattenConjunct(root, discAttrIdxs, AttrVals, contAttrIdxs, lowerBndys, upperBndys) ;
}


// ********************************************************************
// *
// *            flattenConjunct 
// *            --------------
// *
// *     gives flat representation of conjunctive constructReg 
// *
// *********************************************************************
void constructReg::flattenConjunct(constructRegNode *Node, marray<int> &discAttrIdxs, marray<int> &AttrVals, 
                     marray<int> &contAttrIdxs, marray<double> &lowerBndys, marray<double> &upperBndys) 
{
   switch (Node->nodeType)
   {
     case cnAND:
        if (Node->left)
           flattenConjunct(Node->left, discAttrIdxs, AttrVals, contAttrIdxs, lowerBndys, upperBndys) ;
        if (Node->right)
           flattenConjunct(Node->right, discAttrIdxs, AttrVals, contAttrIdxs, lowerBndys, upperBndys) ;
        break ; 
     case cnCONTattrValue:
        contAttrIdxs.addEnd(Node->attrIdx) ;
        lowerBndys.addEnd(Node->lowerBoundary) ;
        upperBndys.addEnd(Node->upperBoundary) ;
        break ;
     case cnDISCattrValue:
        discAttrIdxs.addEnd(Node->attrIdx) ;
        AttrVals.addEnd(Node->valueIdx) ;
        break ;
     default:
        merror("constructReg::flattenConjunct", "unexpected node type detected") ;
   }
}



// ********************************************************************
// *
// *            flattenContConstruct 
// *            --------------------
// *
// *     gives flat representation of summary or product
// *
// *********************************************************************
void constructReg::flattenContConstruct(marray<int> &contAttrIdxs) 
{
  // asumption: there is enough space in array to hold values
  contAttrIdxs.setFilled(0) ;

  if (root) 
     flattenContConstruct(root, contAttrIdxs) ;
}


// ********************************************************************
// *
// *            flattenContConstruct
// *            --------------------
// *
// *     gives flat representation of summary or product
// *
// *********************************************************************
void constructReg::flattenContConstruct(constructRegNode *Node, marray<int> &contAttrIdxs) 
{
   switch (Node->nodeType)
   {
     case cnTIMES:
     case cnPLUS: 
       if (Node->left)
           flattenContConstruct(Node->left, contAttrIdxs) ;
        if (Node->right)
           flattenContConstruct(Node->right, contAttrIdxs) ;
        break ; 
     case cnCONTattribute:
        contAttrIdxs.addEnd(Node->attrIdx) ;
        break ;
     default:
        merror("constructReg::flattenContConstruct", "unexpected node type detected") ;
   }
}


// ********************************************************************
// *
// *            mdlAux/0 
// *            --------
// *
// *     computes part of the mdl code len for  constructReg 
// *
// *********************************************************************
double constructReg::mdlAux() 
{
   switch (compositionType)
   {
      case cCONJUNCTION:
      case cSUM:
      case cPRODUCT:
         return mdlAux(root) ;
        
      case cSINGLEattribute:
      case cXofN:
      default:  
         merror("constructReg::mdlAux", "unexpected behaviour") ;
         return 0.0 ;
   }
}


// ********************************************************************
// *
// *            mdlAux/1
// *            --------
// *
// *     computes part of the mdl code len for  constructReg 
// *
// *********************************************************************
double constructReg::mdlAux(constructRegNode *Node) 
{
   switch(Node->nodeType)
   { 
      case cnAND:
      case cnPLUS:
      case cnTIMES:
         return mdlAux(Node->left) + mdlAux(Node->right) ;

      case cnCONTattribute: // summing and multiplying
         return mlog2((double)(gRT->noNumeric-1)) ;
      
      case cnCONTattrValue:
      {
        double intValue = gRT->valueInterval[Node->attrIdx]/gRT->opt->mdlErrorPrecision ;
        if (intValue < 1.0)
          intValue = 1.0 ;
        return  mlog2((double)gRT->noAttr) + 2.0*mlog2((double)intValue) ;
      }
      case  cnDISCattrValue:
        return mlog2((double)gRT->noAttr) +
               mlog2((double)gRT->AttrDesc[gRT->DiscIdx[Node->attrIdx]].NoValues) ;

      case cnDISCattribute:
      default: 
         merror("constructReg::mdlAux", "unexpected use") ;
         return 0.0 ;
   }
}


// ********************************************************************
// *
// *            mdlConstructCode
// *            ----------------
// *
// *     computes MDL code len for  constructReg 
// *
// *********************************************************************
double constructReg::mdlConstructCode() 
{
   double code = mlog2((double)no1bits(gRT->opt->constructionMode)) ;
   switch (compositionType)
   {
     case cSINGLEattribute:
          code += mlog2((double)gRT->noAttr) ;
          if (countType == aDISCRETE)
          {
 //           marray<double> Multinom(2, 0.0) ;  
 //           for (int i=1 ; i < leftValues.len() ; i++)
 //             if (leftValues[i]) 
 //               Multinom[0] += 1.0 ;
 //           Multinom[1] = leftValues.len() - 1.0 - Multinom[0];
 //           Multinom.setFilled(2)  ;
 //           code += multinomLog2(Multinom) ;
              code += noValues ;
          }
          else
          {
            double intValue = gRT->valueInterval[root->attrIdx]/gRT->opt->mdlModelPrecision ;
            if (intValue < 1.0)
              intValue = 1.0 ;
            code += mlog2(intValue) ;
          }
          break ;       
     case cCONJUNCTION:
         code += mlog2((double)gRT->opt->maxConstructSize) ;
         code += mdlAux() ;       
         break ;
    
     case cSUM:
     case cPRODUCT:
         {  code += mlog2((double)gRT->opt->maxConstructSize) ;
            // selection of the attributes       
            marray<double> Multinom(2, 0.0) ;  
            Multinom[0] = degreesOfFreedom() ;
            Multinom[1] = gRT->noNumeric-1 - Multinom[0] ;
            Multinom.setFilled(2) ;
            code += multinomLog2(Multinom) ;
            code += 1.0 + mdlIntEncode(splitValue/gRT->opt->mdlModelPrecision) ; 
         }
         break ;

     default:
         merror("constructReg::mdlConstructCode","constructReg has unexpected composition") ;
   }
   return code ;
}

