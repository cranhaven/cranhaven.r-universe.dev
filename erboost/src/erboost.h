// The code is a modified version of gbm library originally written by Greg Ridgeway. See
// 
// Ridgeway, G. (2007). Generalized boosted models: A guide to the gbm package. R pack-
// age vignette. http://cran.r-project.org/web/packages/gbm.
//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
//
//  File:       erboost.h
//
//  License:    GNU GPL (version 2 or later)
//
//  Contents:   Entry point for erboost.dll
//            
//  Owner:      gregr@rand.org
//
//  History:    2/14/2003   gregr created
//
//------------------------------------------------------------------------------

#include<vector>
#include "dataset.h"
#include "distribution.h"
#include "expectile.h"
#include "erboost_engine.h"

typedef vector<char> VEC_CATEGORIES;
typedef vector<VEC_CATEGORIES> VEC_VEC_CATEGORIES;

erboostRESULT erboost_setup
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
);


erboostRESULT erboost_transfer_catsplits_to_R
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


