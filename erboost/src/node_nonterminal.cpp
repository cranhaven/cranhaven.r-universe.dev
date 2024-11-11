// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
#include "node_nonterminal.h"

CNodeNonterminal::CNodeNonterminal()
{
    pLeftNode = NULL;
    pRightNode = NULL;
    iSplitVar = 0;
    dImprovement = 0.0;
    pMissingNode = NULL;
}


CNodeNonterminal::~CNodeNonterminal()
{

}



erboostRESULT CNodeNonterminal::Adjust
(
    unsigned long cMinObsInNode
)
{
    erboostRESULT hr = erboost_OK;

    hr = pLeftNode->Adjust(cMinObsInNode);
    hr = pRightNode->Adjust(cMinObsInNode);
    
    if(pMissingNode->isTerminal && (pMissingNode->cN < cMinObsInNode))
    {
        dPrediction = ((pLeftNode->dTrainW)*(pLeftNode->dPrediction) +
                       (pRightNode->dTrainW)*(pRightNode->dPrediction))/
                      (pLeftNode->dTrainW + pRightNode->dTrainW);
        pMissingNode->dPrediction = dPrediction;
    }
    else
    {
        hr = pMissingNode->Adjust(cMinObsInNode);
        dPrediction = 
            ((pLeftNode->dTrainW)*   (pLeftNode->dPrediction) +
             (pRightNode->dTrainW)*  (pRightNode->dPrediction) +
             (pMissingNode->dTrainW)*(pMissingNode->dPrediction))/
            (pLeftNode->dTrainW + pRightNode->dTrainW + pMissingNode->dTrainW);
    }

    return hr;
}



erboostRESULT CNodeNonterminal::Predict
(
    CDataset *pData, 
    unsigned long iRow, 
    double &dFadj
)
{
    erboostRESULT hr = erboost_OK;

    signed char schWhichNode = WhichNode(pData,iRow);
    if(schWhichNode == -1)
    {
        hr = pLeftNode->Predict(pData, iRow, dFadj);
    }
    else if(schWhichNode == 1)
    {
        hr = pRightNode->Predict(pData, iRow, dFadj);
    }
    else
    {
        hr = pMissingNode->Predict(pData, iRow, dFadj);
    }

    return hr;
}


erboostRESULT CNodeNonterminal::Predict
(
    double *adX,
    unsigned long cRow,
    unsigned long cCol,
    unsigned long iRow,
    double &dFadj
)
{
    erboostRESULT hr = erboost_OK;

    signed char schWhichNode = WhichNode(adX,cRow,cCol,iRow);
    if(schWhichNode == -1)
    {
        hr = pLeftNode->Predict(adX,cRow,cCol,iRow,dFadj);
    }
    else if(schWhichNode == 1)
    {
        hr = pRightNode->Predict(adX,cRow,cCol,iRow,dFadj);
    }
    else
    {
        hr = pMissingNode->Predict(adX,cRow,cCol,iRow,dFadj);
    }

    return hr;
}


erboostRESULT CNodeNonterminal::GetVarRelativeInfluence
(
    double *adRelInf
)
{
    erboostRESULT hr = erboost_OK;

    adRelInf[iSplitVar] += dImprovement;
    pLeftNode->GetVarRelativeInfluence(adRelInf);
    pRightNode->GetVarRelativeInfluence(adRelInf);


    return hr;
}


