// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
#include "node.h"

CNode::CNode()
{
    dPrediction = 0.0;
    dTrainW = 0.0;
    isTerminal = false;
}


CNode::~CNode()
{
    // the nodes get deleted by deleting the node factory
}


TDboostRESULT CNode::Adjust
(
    unsigned long cMinObsInNode
)
{
    TDboostRESULT hr = TDboost_NOTIMPL;
    return hr;
}


TDboostRESULT CNode::Predict
(
    CDataset *pData, 
    unsigned long iRow, 
    double &dFadj
)
{
    TDboostRESULT hr = TDboost_NOTIMPL;
    return hr;
}


double CNode::TotalError()
{
    TDboostRESULT hr = TDboost_NOTIMPL;
    return hr;
}


TDboostRESULT CNode::PrintSubtree
(
    unsigned long cIndent
)
{
    TDboostRESULT hr = TDboost_NOTIMPL;
    return hr;
}


TDboostRESULT CNode::GetVarRelativeInfluence
(
    double *adRelInf
)
{
    TDboostRESULT hr = TDboost_NOTIMPL;
    return hr;
}


TDboostRESULT CNode::TransferTreeToRList
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
    return TDboost_NOTIMPL;
}


