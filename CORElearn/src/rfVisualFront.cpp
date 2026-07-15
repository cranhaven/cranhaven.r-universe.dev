// the frontend file for call from R
//#include <stdio.h>
#include <cstring>
#include <ctime>

// general constants and data type definitions
// here you specify whether to compile for  Windows or UNIX
#include "general.h"
// joint method of reporting errors
#include "error.h"

#if defined(DEBUG)
#if defined(MICROSOFT)
#include <cmalloc>  // for heapcheck
#endif
#endif

#include "dataStore.h"  // frame for data
#include "ftree.h"    // decision tree with feature construction
#include "regtree.h"
#include "rndforest.h"  // random forests
#include "utils.h"    // various utillities e.g., computing of std. dev.
#include "estimator.h"
#include "estimatorReg.h"
#include "utils.h"
#include "options.h"
#include "printUtil.h"
#include "calibrate.h"
#include "Rfront.h"

using namespace std ;

extern "C" {

// stores pointers to all the active models, one for each
extern marray<dataStore*> allModels;


#if defined(R_PORT)
SEXP exportModelRT(SEXP modelID){
        int mi;
        mi = INTEGER(modelID)[0];
        // is modelID valid
        if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
                return (NULL);
        regressionTree *dT = (regressionTree*) allModels[mi]; // working Model
        //dT->learnRF = mFALSE;
        return (dT->T2Rpart());
}
SEXP exportModelT(SEXP modelID){
        int mi;
        mi = INTEGER(modelID)[0];
        // is modelID valid
        if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
                return (NULL);
        featureTree *dT = (featureTree*) allModels[mi]; // working Model
        //dT->learnRF = mFALSE;
        return (dT->T2Rpart());
}
SEXP exportProximity(SEXP modelID, SEXP dis){
        bool dist = (bool)INTEGER(dis)[0];
        int mi;
        mi = INTEGER(modelID)[0];
    	// is modelID valid
        if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
                return (NULL);
        featureTree *dT = (featureTree*) allModels[mi]; // working Model
        dT->learnRF = mTRUE;
        return (dT->proximity(dist));
}
SEXP exportVarImportanceCluster(int *modelID, int *clusterData, double *var){
        int mi;
        mi = *modelID;
        // is modelID valid
        if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
                        return (NULL);
        featureTree *dT = (featureTree*) allModels[mi]; // working Model
        dT->learnRF = mTRUE;

        marray<double> varEval;
        varEval.wrap(dT->noAttr, var);
        varEval.init(0.0);

        marray<booleanT> boolClData(dT->NoTrainCases, mFALSE);
        boolClData.setFilled(dT->NoTrainCases);
        for (int i=0 ; i < dT->NoTrainCases ; i++){
                if(clusterData[i] == 1){
                        boolClData[i] = mTRUE;
                }
        }

        SEXP retValue = (dT->importance2RCluster(varEval, boolClData));
        int dummy ;
        varEval.unWrap(dummy);
        return retValue ;
}
#endif
} //extern "C"
