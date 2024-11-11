//------------------------------------------------------------------------------
//  by Greg Ridgeway  Copyright (C) 2003
//
//  File:       erboost_engine.h
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

#ifndef erboost_ENGINerboost_H
#define erboost_ENGINerboost_H

#include <vector>
#include "buildinfo.h"
#include "distribution.h"
#include "tree.h"
#include "dataset.h"
#include "node_factory.h"

using namespace std;

class Cerboost
{

public:

    Cerboost();
    ~Cerboost();
    erboostRESULT Initialize(CDataset *pData,
                         CDistribution *pDist,
                         double dLambda,
                         unsigned long nTrain,
                         double dBagFraction,
                         unsigned long cLeaves,
                         unsigned long cMinObsInNode);

    erboostRESULT iterate(double *adF,
                    double &dTrainError,
                    double &dValidError,
                    double &dOOBagImprove,
                    int &cNodes);
    erboostRESULT TransferTreeToRList(int *aiSplitVar,
                                double *adSplitPoint,
                                int *aiLeftNode,
                                int *aiRightNode,
                                int *aiMissingNode,
                                double *adErrorReduction,
                                double *adWeight,
                                double *adPred,
                                VEC_VEC_CATEGORIES &vecSplitCodes,
                                int cCatSplitsOld);
    erboostRESULT Predict(unsigned long iVar,
                    unsigned long cTrees,
                    double *adF,
                    double *adX,
                    unsigned long cLength);
    erboostRESULT Predict(double *adX,
                    unsigned long cRow,
                    unsigned long cCol,
                    unsigned long cTrees,
                    double *adF);

    erboostRESULT GetVarRelativeInfluence(double *adRelInf,
                                    unsigned long cTrees);
    erboostRESULT PrintTree();

    CDataset *pData;            // the data
    CDistribution *pDist;       // the distribution
    bool fInitialized;          // indicates whether the erboost has been initialized
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

#endif // erboost_ENGINerboost_H



