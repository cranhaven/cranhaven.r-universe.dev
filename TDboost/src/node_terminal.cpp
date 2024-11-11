// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
#include "node_terminal.h"
#include "node_factory.h"

CNodeTerminal::CNodeTerminal()
{
    isTerminal = true;
}


CNodeTerminal::~CNodeTerminal()
{
    #ifdef NOISY_DEBUG
    Rprintf("terminal destructor\n");
    #endif
}

TDboostRESULT CNodeTerminal::Adjust
(
    unsigned long cMinObsInNode
)
{
    return TDboost_OK;
}

TDboostRESULT CNodeTerminal::ApplyShrinkage
(
    double dLambda
)
{
    TDboostRESULT hr = TDboost_OK;
    dPrediction *= dLambda;

    return hr;
}



TDboostRESULT CNodeTerminal::Predict
(
    CDataset *pData, 
    unsigned long iRow, 
    double &dFadj
)
{
    dFadj = dPrediction;

    return TDboost_OK;
}




TDboostRESULT CNodeTerminal::Predict
(
    double *adX,
    unsigned long cRow,
    unsigned long cCol,
    unsigned long iRow,
    double &dFadj
)
{
    dFadj = dPrediction;

    return TDboost_OK;
}



TDboostRESULT CNodeTerminal::PrintSubtree
(
    unsigned long cIndent
)
{
    unsigned long i = 0;
    
    for(i=0; i< cIndent; i++) Rprintf("  ");
    Rprintf("N=%f, Prediction=%f *\n",
           dTrainW,
           dPrediction);

    return TDboost_OK;
}


TDboostRESULT CNodeTerminal::GetVarRelativeInfluence
(
    double *adRelInf
)
{
    return TDboost_OK;
}


TDboostRESULT CNodeTerminal::RecycleSelf
(
    CNodeFactory *pNodeFactory
)
{
    pNodeFactory->RecycleNode(this);
    return TDboost_OK;
}



TDboostRESULT CNodeTerminal::TransferTreeToRList
(
    int &iNodeID,
    CDataset *pData,
    int *aiSplitVar,
    double *adSplitPoint,
    int *aiLeftNode,
    int *aiRightNode,
    int *aiMissingNode,
    double *adErrorReduction,
    double *adWeight,
    double *adPred,
    VEC_VEC_CATEGORIES &vecSplitCodes,
    int cCatSplitsOld,
    double dShrinkage
)
{
    TDboostRESULT hr = TDboost_OK;

    aiSplitVar[iNodeID] = -1;
    adSplitPoint[iNodeID] = dShrinkage*dPrediction;
    aiLeftNode[iNodeID] = -1;
    aiRightNode[iNodeID] = -1;
    aiMissingNode[iNodeID] = -1;
    adErrorReduction[iNodeID] = 0.0;
    adWeight[iNodeID] = dTrainW;
    adPred[iNodeID] = dShrinkage*dPrediction;

    iNodeID++;

    return hr;
}


