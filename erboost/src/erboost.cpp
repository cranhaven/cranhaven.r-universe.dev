// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
#include<vector>
#include "erboost.h"

unsigned long erboost_setup
(
    double *adY,
    double *adOffset,
    double *adX,
    int *aiXOrder,
    double *adWeight,
    double *adMisc,
    int cRows,
    int cCols,
    int *acVarClasses,
    int *alMonotoneVar,
    const char *pszFamily,
    int cTrees,
    int cDepth,
    int cMinObsInNode,
    double dShrinkage,
    double dBagFraction,
    int cTrain,
    CDataset *pData,
    PCDistribution &pDist
)
{
    unsigned long hr = 0;

    hr = pData->SetData(adX,aiXOrder,adY,adOffset,adWeight,adMisc,
                        cRows,cCols,acVarClasses,alMonotoneVar);
    if(erboost_FAILED(hr))
    {
        goto Error;
    }

    // set the distribution

    if(strncmp(pszFamily,"expectile",2) == 0)
    {
        pDist = new CExpectile(adMisc[0]);
        if(pDist==NULL)
        {
            hr = erboost_OUTOFMEMORY;
            goto Error;
        }
    }




    if(pDist==NULL)
    {
        hr = erboost_INVALIDARG;
        goto Error;
    }

Cleanup:
    return hr;
Error:
    goto Cleanup;
}



erboostRESULT erboost_transfer_to_R
(
    Cerboost *perboost,
    VEC_VEC_CATEGORIES &vecSplitCodes,
    int *aiSplitVar,
    double *adSplitPoint,
    int *aiLeftNode,
    int *aiRightNode,
    int *aiMissingNode,
    double *adErrorReduction,
    double *adWeight,
    double *adPred,
    int cCatSplitsOld
)
{
    erboostRESULT hr = erboost_OK;

    hr = perboost->TransferTreeToRList(aiSplitVar,
                                   adSplitPoint,
                                   aiLeftNode,
                                   aiRightNode,
                                   aiMissingNode,
                                   adErrorReduction,
                                   adWeight,
                                   adPred,
                                   vecSplitCodes,
                                   cCatSplitsOld);
    if(erboost_FAILED(hr)) goto Error;

Cleanup:
    return hr;
Error:
    goto Cleanup;
}


erboostRESULT erboost_transfer_catsplits_to_R
(
    int iCatSplit,
    VEC_VEC_CATEGORIES &vecSplitCodes,
    int *aiSplitCodes
)
{
    unsigned long i=0;

    for(i=0; i<vecSplitCodes[iCatSplit].size(); i++)
    {
        aiSplitCodes[i] = vecSplitCodes[iCatSplit][i];
    }

    return erboost_OK;
}


int size_of_vector
(
    VEC_VEC_CATEGORIES &vec,
    int i
)
{
    return vec[i].size();
}



