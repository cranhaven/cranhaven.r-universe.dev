// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
//
//  File:       TDboost.h
//
//  License:    GNU GPL (version 2 or later)
//
//  Contents:   Entry point for TDboost.dll
//            
//  Owner:      gregr@rand.org
//
//  History:    2/14/2003   gregr created
//
//------------------------------------------------------------------------------

#include<vector>
#include "dataset.h"
#include "distribution.h"
#include "EDM.h"
#include "TDboost_engine.h"

typedef vector<char> VEC_CATEGORIES;
typedef vector<VEC_CATEGORIES> VEC_VEC_CATEGORIES;

TDboostRESULT TDboost_setup
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
    int cLeaves,
    int cMinObsInNode,
    double dShrinkage,
    double dBagFraction,
    int cTrain,
    CDataset *pData,
    PCDistribution &pDist
);


TDboostRESULT TDboost_transfer_to_R
(
    CTDboost *pTDboost,
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
);


TDboostRESULT TDboost_transfer_catsplits_to_R
(
    int iCatSplit,
    VEC_VEC_CATEGORIES &vecSplitCodes,
    int *aiSplitCodes
);


int size_of_vector
(
    VEC_VEC_CATEGORIES &vec,
    int i
);


