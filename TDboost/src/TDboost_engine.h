//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
//
//  File:       TDboost_engine.h
//
//  License:    GNU GPL (version 2 or later)
//
//  Contents:   Generalized boosted model engine
//
//  Owner:      gregr@rand.org
//
//  History:    3/26/2001   gregr created
//              2/14/2003   gregr: adapted for R implementation
//
//------------------------------------------------------------------------------

#ifndef TDboost_ENGINTDboost_H
#define TDboost_ENGINTDboost_H

#include <vector>
#include "buildinfo.h"
#include "distribution.h"
#include "tree.h"
#include "dataset.h"
#include "node_factory.h"

using namespace std;

class CTDboost
{

public:

    CTDboost();
    ~CTDboost();
    TDboostRESULT Initialize(CDataset *pData,
                         CDistribution *pDist,
                         double dLambda,
                         unsigned long nTrain,
                         double dBagFraction,
                         unsigned long cLeaves,
                         unsigned long cMinObsInNode);

    TDboostRESULT iterate(double *adF,
                    double &dTrainError,
                    double &dValidError,
                    double &dOOBagImprove,
                    int &cNodes);
    TDboostRESULT TransferTreeToRList(int *aiSplitVar,
                                double *adSplitPoint,
                                int *aiLeftNode,
                                int *aiRightNode,
                                int *aiMissingNode,
                                double *adErrorReduction,
                                double *adWeight,
                                double *adPred,
                                VEC_VEC_CATEGORIES &vecSplitCodes,
                                int cCatSplitsOld);
    TDboostRESULT Predict(unsigned long iVar,
                    unsigned long cTrees,
                    double *adF,
                    double *adX,
                    unsigned long cLength);
    TDboostRESULT Predict(double *adX,
                    unsigned long cRow,
                    unsigned long cCol,
                    unsigned long cTrees,
                    double *adF);

    TDboostRESULT GetVarRelativeInfluence(double *adRelInf,
                                    unsigned long cTrees);
    TDboostRESULT PrintTree();

    CDataset *pData;            // the data
    CDistribution *pDist;       // the distribution
    bool fInitialized;          // indicates whether the TDboost has been initialized
    CNodeFactory *pNodeFactory;

    // these objects are for the tree growing
    // allocate them once here for all trees to use
    bool *afInBag;
    unsigned long *aiNodeAssign;
    CNodeSearch *aNodeSearch;
    PCCARTTree ptreeTemp;
    VEC_P_NODETERMINAL vecpTermNodes;
    double *adZ;
    double *adFadj;

private:
    double dLambda;
    unsigned long cTrain;
    unsigned long cValid;
    unsigned long cTotalInBag;
    double dBagFraction;
    unsigned long cDepth;
    unsigned long cMinObsInNode;
};

#endif // TDboost_ENGINTDboost_H



