#if !defined(RFRONT_H)
#define RFRONT_H

extern "C" {
void initCore(int *maxModels) ;
void testNA(int *t, double *x, int *a) ;
void testRPORT(int *a) ;
void testCoreRand(int *n, double *x) ;
void testClassPseudoRandom(int *n, int *s, int *k, int *m, double *x) ;
void testTime(double *x) ;
void versionCore(char **version) ;
void buildCoreModel(int *noInst, int *noDiscrete, int *noDiscVal,
		int *discData, int *noNumeric, double *numData, double *costMx,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
		int *noOptions, char **optName, char **optValue,
		int *modelID, int *noClasses) ;
void destroyOneCoreModel(int* modelID) ;
void predictWithCoreModel(int *modelID, int *noInst, int *discData,
		double *numData, double *costMx, int *returnPred, double *returnProb,
		double *returnPredReg, int *noOptions, char **optName, char **optValue) ;
void destroyCore() ;
void simRcall() ;
void availableEstimatorsCore(char **estBrief) ;
void estimateCore(int *noInst, int *noDiscrete,
		int *noDiscVal, int *discData, int *noNumeric, double *numData, double *costMx,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
        int *noOptions, char **optName, char **optValue,
		int *selectedEstimator, double *discEst, double *numEst, double *numSplitPoint) ;
void estimateCoreReg(int *noInst, int *noDiscrete,
		int *noDiscVal, int *discData, int *noNumeric, double *numData,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
        int *noOptions, char **optName, char **optValue,
		int *selectedEstimator, double *discEst, double *numEst, double *numSplitPoint) ;
void ordEvalCore(int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		char **dscAttrNames, char **dscValNames,
		int *noOptions, char **optName, char **optValue,
		double *rePos, double *reNeg, double *anch, double *rndrePos, double *rndreNeg, double *rndAnch,
		int *noAttrVal, char **ordEvalFile, char **ordEvalRndFile, int *variant) ;
void modelEvaluate(int *noInst, int *correctCl, // int *predictedCl,
		double *predictedPr, double *costMx, int *noClasses, double *priorClProbability,
		double *accuracy, double *avgCost,
		double *infScore, double *auc, int *predictionMx, double *sensitivity,
		double *specificity, double *brier, double *kappa, double *precision, double *Gmean,
		double *KS, double *TPR, double *FPR) ;
void modelEvaluateReg(int *noInst, double *truePred,
		double *pred, double *avgPrediction, double *MSE, double *RMSE, double *MAE, double *RMAE) ;
void calibrate(int *calMethod, int *noInst, int *correctCl, double *predictedPr, double *wght, int *noBins,
		       int *noIntervals, double *interval, double *calProb);
void discretize(int *methodIdx, int *isRegression, int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		int *noNumeric, double *numData, char **dscAttrNames,
		char **dscValNames, char ** nmAttrNames, int *noOptions,
		char **optName, char **optValue, int *selectedEstimator,
		int *maxBns, int *noBnds, double *bnds) ;
void rfAttrEval(int *modelID, double *estOut) ;
void rfOOB(int *modelID, double *oobAccuracy, double *oobMargin, double *oobCorrelation) ;
void optionsInOut(int *modelID, char **fileName, char **io) ;
void saveRF(int *modelID, char **fileName) ;
void readRF(char **fileName, int *modelID) ;
#if defined(R_PORT)
SEXP exportSizesRF(SEXP modelID) ;
SEXP exportSumOverLeavesRF(SEXP modelID) ;
SEXP exportModel(SEXP modelID) ;
SEXP exportModelRT(SEXP modelID) ;
SEXP exportModelT(SEXP modelID) ;
SEXP exportProximity(SEXP modelID, SEXP dis) ;
SEXP exportVarImportanceCluster(int *modelID, int *clusterData, double *var) ;
SEXP printTreeDot2R(SEXP modelID) ;
SEXP printTree2R(SEXP modelID) ;
SEXP noEqualRows(SEXP data1, SEXP data2, SEXP nrowsd1, SEXP nrowsd2, SEXP ncol, SEXP tolerance, SEXP countOnce) ;

#endif
}

#endif /*RFRONT_H_*/
