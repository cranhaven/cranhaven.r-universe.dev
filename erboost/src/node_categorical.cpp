// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
#include "node_categorical.h"
#include "node_factory.h"

CNodeCategorical::CNodeCategorical()
{
    aiLeftCategory = NULL;
    cLeftCategory = 0;
}


CNodeCategorical::~CNodeCategorical()
{
    #ifdef NOISY_DEBUG
    Rprintf("categorical destructor\n");
    #endif
    if(aiLeftCategory != NULL)
    {
        delete [] aiLeftCategory;
        aiLeftCategory = NULL;
    }
}


erboostRESULT CNodeCategorical::PrintSubtree
(
    unsigned long cIndent
)
{
    erboostRESULT hr = erboost_OK;
    unsigned long i = 0;
    
    for(i=0; i< cIndent; i++) Rprintf("  ");
    Rprintf("N=%f, Improvement=%f, Prediction=%f, NA pred=%f\n",
           dTrainW,
           dImprovement,
           dPrediction,
           (pMissingNode == NULL ? 0.0 : pMissingNode->dPrediction));

    for(i=0; i< cIndent; i++) Rprintf("  ");
    Rprintf("V%ld in ",iSplitVar);
    for(i=0; i<cLeftCategory; i++)
    {
        Rprintf("%ld",aiLeftCategory[i]);
        if(i<cLeftCategory-1) Rprintf(",");
    }
    Rprintf("\n");
    hr = pLeftNode->PrintSubtree(cIndent+1);

    for(i=0; i< cIndent; i++) Rprintf("  ");
    Rprintf("V%ld not in ",iSplitVar);
    for(i=0; i<cLeftCategory; i++)
    {
        Rprintf("%ld",aiLeftCategory[i]);
        if(i<cLeftCategory-1) Rprintf(",");
    }
    Rprintf("\n");
    hr = pRightNode->PrintSubtree(cIndent+1);

    return hr;
}


signed char CNodeCategorical::WhichNode
(
    CDataset *pData,
    unsigned long iObs
)
{
    signed char ReturnValue = 0;
    double dX = pData->adX[iSplitVar*(pData->cRows) + iObs];

    if(!ISNA(dX))
    {
        if(std::find(aiLeftCategory,
                     aiLeftCategory+cLeftCategory,
                     (ULONG)dX) != aiLeftCategory+cLeftCategory)
        {
            ReturnValue = -1;
        }
        else
        {
            ReturnValue = 1;
        }
    }
    // if missing value returns 0

    return ReturnValue;
}


signed char CNodeCategorical::WhichNode
(
    double *adX,
    unsigned long cRow,
    unsigned long cCol,
    unsigned long iRow
)
{
    signed char ReturnValue = 0;
    double dX = adX[iSplitVar*cRow + iRow];

    if(!ISNA(dX))
    {
        if(std::find(aiLeftCategory,
                     aiLeftCategory+cLeftCategory,
                     (ULONG)dX) != aiLeftCategory+cLeftCategory)
        {
            ReturnValue = -1;
        }
        else
        {
            ReturnValue = 1;
        }
    }
    // if missing value returns 0

    return ReturnValue;
}




erboostRESULT CNodeCategorical::RecycleSelf
(
    CNodeFactory *pNodeFactory
)
{
    erboostRESULT hr = erboost_OK;
    hr = pNodeFactory->RecycleNode(this);
    return hr;
};



erboostRESULT CNodeCategorical::TransferTreeToRList
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
    erboostRESULT hr = erboost_OK;

    int iThisNodeID = iNodeID;
    unsigned long cCatSplits = vecSplitCodes.size();
    unsigned long i = 0;
    int cLevels = pData->acVarClasses[iSplitVar];

    aiSplitVar[iThisNodeID] = iSplitVar;
    adSplitPoint[iThisNodeID] = cCatSplits+cCatSplitsOld; // 0 based
    adErrorReduction[iThisNodeID] = dImprovement;
    adWeight[iThisNodeID] = dTrainW;
    adPred[iThisNodeID] = dShrinkage*dPrediction;

    vecSplitCodes.push_back(VEC_CATEGORIES());

    vecSplitCodes[cCatSplits].resize(cLevels,1);
    for(i=0; i<cLeftCategory; i++)
    {
        vecSplitCodes[cCatSplits][aiLeftCategory[i]] = -1;
    }

    iNodeID++;
    aiLeftNode[iThisNodeID] = iNodeID;
    hr = pLeftNode->TransferTreeToRList(iNodeID,
                                        pData,
                                        aiSplitVar,
                                        adSplitPoint,
                                        aiLeftNode,
                                        aiRightNode,
                                        aiMissingNode,
                                        adErrorReduction,
                                        adWeight,
                                        adPred,
                                        vecSplitCodes,
                                        cCatSplitsOld,
                                        dShrinkage);
    if(erboost_FAILED(hr)) goto Error;

    aiRightNode[iThisNodeID] = iNodeID;
    hr = pRightNode->TransferTreeToRList(iNodeID,
                                         pData,
                                         aiSplitVar,
                                         adSplitPoint,
                                         aiLeftNode,
                                         aiRightNode,
                                         aiMissingNode,
                                         adErrorReduction,
                                         adWeight,
                                         adPred,
                                         vecSplitCodes,
                                         cCatSplitsOld,
                                         dShrinkage);
    if(erboost_FAILED(hr)) goto Error;

    aiMissingNode[iThisNodeID] = iNodeID;
    hr = pMissingNode->TransferTreeToRList(iNodeID,
                                           pData,
                                           aiSplitVar,
                                           adSplitPoint,
                                           aiLeftNode,
                                           aiRightNode,
                                           aiMissingNode,
                                           adErrorReduction,
                                           adWeight,
                                           adPred,
                                           vecSplitCodes,
                                           cCatSplitsOld,
                                           dShrinkage);
    if(erboost_FAILED(hr)) goto Error;


Cleanup:
    return hr;
Error:
    goto Cleanup;
}


