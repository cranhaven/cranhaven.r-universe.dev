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


erboostRESULT CNode::Adjust
(
    unsigned long cMinObsInNode
)
{
    erboostRESULT hr = erboost_NOTIMPL;
    return hr;
}


erboostRESULT CNode::Predict
(
    CDataset *pData, 
    unsigned long iRow, 
    double &dFadj
)
{
    erboostRESULT hr = erboost_NOTIMPL;
    return hr;
}


double CNode::TotalError()
{
    erboostRESULT hr = erboost_NOTIMPL;
    return hr;
}


erboostRESULT CNode::PrintSubtree
(
    unsigned long cIndent
)
{
    erboostRESULT hr = erboost_NOTIMPL;
    return hr;
}


erboostRESULT CNode::GetVarRelativeInfluence
(
    double *adRelInf
)
{
    erboostRESULT hr = erboost_NOTIMPL;
    return hr;
}


erboostRESULT CNode::TransferTreeToRList
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
    return erboost_NOTIMPL;
}


