// the frontend file for call from R

#include <cstring>
#include <ctime>
#include <cstdio>
#include <cstdlib>

#include "general.h"  // general constants and data type definitions
// here you specify weather to compile for  Windows or UNIX

#if defined(DEBUG)
#if defined(MICROSOFT)
#include <cmalloc>  // for heapcheck
#endif
#endif

#include "error.h"    // joint method of reporting errors
#include "dataStore.h"  // frame for data
#include "ftree.h"    // decision tree with feature construction
#include "regtree.h"
#include "rndforest.h"  // random forests
#include "utils.h"    // various utilities e.g., computing of std. dev.
#include "random.h"
#include "estimator.h"
#include "estimatorReg.h"
#include "utils.h"
#include "options.h"
#include "printUtil.h"
#include "calibrate.h"
#include "Rfront.h"

#if defined(R_PORT)
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#else
#define Rprintf printf
#endif

#if defined(DEBUG_NEW)
extern int SetSize;
#endif

extern int NoEstimators;
extern int NoEstimatorsReg;
extern estDsc estName[];
extern estDsc estNameReg[];
extern char VersionString[];

using namespace std ;

extern "C" {

marray<dataStore*> allModels; // stores pointers to all the active models, one for each

#if defined(R_PORT)
#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

const static R_CallMethodDef R_CallDef[] = {
   CALLDEF(exportModelRT,1),
   CALLDEF(exportModelT,1),
   CALLDEF(exportProximity,2),
   CALLDEF(printTree2R,1),
   CALLDEF(printTreeDot2R,1),
   CALLDEF(exportSizesRF,1),
   CALLDEF(exportSumOverLeavesRF,1),
   CALLDEF(exportModel,1),
   CALLDEF(noEqualRows,7),

   {NULL, NULL, 0}
};

const static R_CMethodDef R_CDef[] = {
   CALLDEF(exportVarImportanceCluster, 3),
   CALLDEF(initCore, 1),
   CALLDEF(destroyCore, 0),
   CALLDEF(versionCore, 1),
   CALLDEF(destroyOneCoreModel, 1),
   CALLDEF(buildCoreModel, 15),
   CALLDEF(predictWithCoreModel, 11),
   CALLDEF(estimateCoreReg, 16),
   CALLDEF(estimateCore, 17),
   CALLDEF(rfAttrEval, 2),
   CALLDEF(rfOOB, 4),
   CALLDEF(ordEvalCore, 19),
   CALLDEF(modelEvaluate, 20),
   CALLDEF(modelEvaluateReg, 8),
   CALLDEF(optionsInOut, 3),
   CALLDEF(saveRF, 2),
   CALLDEF(readRF, 2),
   CALLDEF(calibrate, 9),
   CALLDEF(discretize, 18),
   CALLDEF(testNA, 3),
   CALLDEF(testRPORT, 1),
   CALLDEF(testCoreRand, 2),
   CALLDEF(testClassPseudoRandom, 5),
   CALLDEF(testTime, 1),
   {NULL, NULL, 0}
};

void R_init_CORElearn(DllInfo *dll)
{
	R_registerRoutines(dll, R_CDef, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    //R_useDynamicSymbols(dll, TRUE); // for debugging to find entry points
    R_forceSymbols(dll, TRUE);
    //R_forceSymbols(dll, FALSE); // for debugging to find entry points
}

#endif

// on entry to library
void initCore(int *maxModels) {
	destroyCore(); // in case of multiple initializations
	allModels.create(*maxModels, 0);
	allModels.setFilled(*maxModels);
}

//for debugging
void testNA(int *t, double *x, int *a) {
	double y;
	//union ieee_double u;
	if (*t == 1) {
		*x = NAcont;
	} else if (*t == 2) {
		y = 0.0;
		*x = y / y;
	}
	//u.d = *x;
	//Rprintf("%08x  %08x", u.i.p1, u.i.p0); // OK for little endian
	a[0] = isNAcont(*x);
	a[1] = isNaN(*x);
}

void testRPORT(int *a) {
#if defined(R_PORT)
	*a = 1;
#else
	*a = 0;
#endif
}

void testCoreRand(int *n, double *x) {
	testRand(n, x);
}

void testClassPseudoRandom(int *n, int *s, int *k, int *m, double *x)
{
	PseudoRandomStreams *rnd;
	int i, j;
	rnd = new PseudoRandomStreams;
	rnd->initSeed(*m, *n, s);
	for (i=0; i<*k; i++)
		for (j=0; j<*m; j++)
			x[i + j * (*k)] = rnd->getDouble(j);
}

void testTime(double *x)
{
	x[0] = (double)time(NULL);
}

// library version
void versionCore(char **version) {
	strcpy(version[0], VersionString);
}

// build the model from the data
void buildCoreModel(int *noInst, int *noDiscrete, int *noDiscVal,
		int *discData, int *noNumeric, double *numData, double *costMx,
		char **dscAttrNames, char **dscValNames, char ** nmAttrNames,
		int *noOptions, char **optName, char **optValue, int *modelID,
		int *noClasses) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData, costMatrix, priorClProb;
	numericData.wrap(*noNumeric * (*noInst), numData);
	// priorClProb.wrap(1024, priorClassProb);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	int dummy;
	booleanT isRegression = mFALSE;
	Options *opt = new Options;
	dataStore *model = 0;
	featureTree *dT = 0;
	regressionTree *rT = 0;
	dataStore *data = 0;
	*modelID = allModels.memberPlace(model);
	// if there is no more space in the table
	if (*modelID < 0) {
		Rprintf("maximum number of models reached\n");
		delete opt;
		goto cleanUp;
	}
	// read options
	opt->optionsFromStrings(*noOptions, optionsName, optionsValue);

	if (opt->action == "tree" || opt->action == "bayes"
			|| opt->action == "knn" || opt->action == "knnKernel"
					|| opt->action == "rf" || opt->action == "rfNear" )
		isRegression = mFALSE;
	else if (opt->action == "regTree")
		isRegression = mTRUE;
	else {
		merror("unknown action: %s\n", opt->action.getConstValue());
		*modelID = -1;
		delete opt;
		goto cleanUp;
	}

	if (isRegression) {
		allModels[*modelID] = new regressionTree;
		rT = (regressionTree*) allModels[*modelID]; // working model
		data = (dataStore*) rT;
	} else {
		allModels[*modelID] = new featureTree;
		dT = (featureTree*) allModels[*modelID]; // working model
		data = (dataStore*) dT;
	}
	data->opt->copy(*opt) ;
	delete opt ;

	// data is passed from R
	data->isRegression = isRegression;
	// prepare data, first description than matrixes
	// the data structures are defined in dataStore class
	data->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric,
			discAttrNames, discValNames, numAttrNames);
	data->dataFromR(*noInst, discreteData, numericData, mTRUE);
	costMatrix.wrap(data->noClasses * data->noClasses, costMx);
	data->costsFromR(costMatrix);

	// prepare data for training/testing
	data->opt->splitSelection = ALL_TRAINING;
	data->prepareDataSplits();
	data->setDataSplit(data->opt->splitIdx);

	// copy some data to output
	*noClasses = data->noClasses;
	//if (!data->isRegression)
	//	for (int c = 1; c <= *noClasses; c++)
	//		priorClProb[c - 1] = data->AttrDesc[0].valueProbability[c];

	if (data->opt->action == "tree") {
		dT->learnRF = mFALSE;
		dT->constructTree();
	} else if (data->opt->action == "bayes") {
		dT->learnRF = mFALSE;
		dT->opt->minNodeWeightTree = dT->NoCases + 1;
		dT->opt->modelType = 4;
		dT->constructTree();
	} else if (data->opt->action == "knn") {
		dT->learnRF = mFALSE;
		dT->opt->minNodeWeightTree = dT->NoCases + 1;
		dT->opt->modelType = 2;
		dT->constructTree();
	} else if (data->opt->action == "knnKernel") {
		dT->learnRF = mFALSE;
		dT->opt->minNodeWeightTree = dT->NoCases + 1;
		dT->opt->modelType = 3;
		dT->constructTree();
	} else if (data->opt->action == "rf") {
		dT->learnRF = mTRUE;
		dT->opt->rfkNearestEqual = 0; // force to zero, so that memory can be released
#if !defined(R_PORT)
		randSeed(dT->opt->rfRndSeed);
#endif
		dT->buildForest();
	} else if (data->opt->action == "rfNear") {
		dT->learnRF = mTRUE;
#if !defined(R_PORT)
		randSeed(dT->opt->rfRndSeed);
#endif
		dT->buildForest();
	} else if (data->opt->action == "regTree") {
		rT->constructRegTree();
	} else {
		Rprintf("unknown action: %s\n", dT->opt->action.getConstValue());
		delete allModels[*modelID];
		allModels[*modelID] = 0;
		*modelID = -1;
	}

	//if (data->isRegression)
	//	*avgPrediction = rT->rootAverage;
	// for some models we can destroy the data here
	if ((data->opt->action == "tree" && (data->opt->modelType == 1
			|| data->opt->modelType == 4))
			|| data->opt->action == "bayes")
		data->clearData(mTRUE);

	// unwrap arrays
	cleanUp:
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	// priorClProb.unWrap(dummy);

	costMatrix.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);

#if defined(R_PORT)
	PutRNGstate();
#endif
}

// return class value and its probability for given data with selected model
void predictWithCoreModel(int *modelID, int *noInst, int *discData,
		double *numData, double *costMx, int *returnPred, double *returnProb,
		double *returnPredReg, int *noOptions, char **optName, char **optValue) {
	// is modelID valid
	if (modelID == 0 || *modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	dataStore *data = allModels[*modelID]; // working Model

	// wrap arrays for security reasons
	marray<int> discreteData, returnPredicted;
	discreteData.wrap(data->noDiscrete * (*noInst), discData);
	returnPredicted.wrap(*noInst, returnPred);
	marray<double> numericData, costMatrix, returnProbability,
	returnPredictedReg;
	numericData.wrap(data->noNumeric * (*noInst), numData);
	costMatrix.wrap(data->noClasses * data->noClasses, costMx);
	returnProbability.wrap(data->noClasses * (*noInst), returnProb);
	returnPredictedReg.wrap(*noInst, returnPredReg);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);

	// read options
	data->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);

	// prepare prediction data, which should have the same description as training one
	data->dataFromR(*noInst, discreteData, numericData, mFALSE);
	data->costsFromR(costMatrix);

	if (data->isRegression)
		((regressionTree*) data)->predictRreg(returnPredictedReg);
	else
		((featureTree*) data)->predictR(returnPredicted, returnProbability);

	// we can destroy the prediction data
	data->clearData(mFALSE);

	int dummy;
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	costMatrix.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	returnPredicted.unWrap(dummy);
	returnProbability.unWrap(dummy);
	returnPredictedReg.unWrap(dummy);
}

// destroy all the models
void destroyCore() {
	int i, modelID;
	for (i = 0; i < allModels.len(); i++) {
		modelID = i ;
		destroyOneCoreModel(&modelID);
	}
	allModels.destroy();

#if defined(DEBUG)
#if defined(MICROSOFT)
	/* Check heap status */
	int heapstatus = _heapchk();
	if (heapstatus!= _HEAPOK)
		merror("destroyCore final check","Heap is not OK !!");
	// _HEAPOK, _HEAPEMPTY, _HEAPBADBEGIN, _HEAPBADNODE
#endif
#endif

#if defined(DEBUG_NEW)
	Rprintf("Still allocated memory blocks: %ld\n",SetSize);
#endif
}

void destroyOneCoreModel(int* modelID) {
	// is modelID valid
	if (modelID != 0)
		if (allModels.defined())
			if (modelID != 0 && *modelID >= 0)
				if (*modelID < allModels.len())
					if (allModels[*modelID] != 0) {
						dataStore *data = allModels[*modelID];
						if (data->isRegression)
							delete (regressionTree*) allModels[*modelID];
						else
							delete (featureTree*) allModels[*modelID];
						allModels[*modelID] = 0;
						*modelID = -1;
					}
}

void availableEstimatorsCore(char **estBrief) {
	strcpy(estBrief[0], "");
	for (int i = 1; i <= NoEstimators; i++) {
		strcat(estBrief[0], estName[i].brief);
		if (i < NoEstimators)
			strcat(estBrief[0], ",");
	}
}

void estimateCore(int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		int *noNumeric, double *numData, double *costMx, char **dscAttrNames,
		char **dscValNames, char ** nmAttrNames, int *noOptions,
		char **optName, char **optValue, int *selectedEstimator,
		double *discEst, double *numEst, double *numSplitPoint) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData, costMatrix;
	numericData.wrap(*noNumeric * (*noInst), numData);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<double> discreteEst, numericEst, numericSplit;
	numericEst.wrap(*noNumeric, numEst);
	discreteEst.wrap(*noDiscrete, discEst);
	numericSplit.wrap(*noNumeric, numSplitPoint) ;
	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	int dummy, i;
	featureTree *dT = new featureTree;

	// read options
	dT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);
	// reset estimation options
	dT->opt->estOn.init(mFALSE);
	dT->opt->estOn[*selectedEstimator] = mTRUE;

	// prepare data, first description than matrixes
	//marray<char *> discAttrNames(*noDiscrete, 0), discValNames(*noDiscrete, 0), numAttrNames(*noNumeric, 0);
	dT->isRegression = mFALSE;
	dT->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, discAttrNames, discValNames, numAttrNames);
	dT->dataFromR(*noInst, discreteData, numericData, mTRUE);
	costMatrix.wrap(dT->noClasses * dT->noClasses, costMx);
	dT->costsFromR(costMatrix);
	// prepare data for training/testing
	dT->opt->splitSelection = ALL_TRAINING;
	dT->prepareDataSplits();
	dT->setDataSplit(dT->opt->splitIdx);

	marray<double> weight(dT->NoTrainCases, 1.0);
	attributeCount attrType;
	estimation Estimator(dT, dT->DTraining, weight, dT->NoTrainCases);
	Estimator.estimate(*selectedEstimator, 0, dT->noNumeric, 1, dT->noDiscrete,
			attrType);

	discreteEst[0] = NAcont;
	for (i = 1; i < dT->noDiscrete; i++)
		discreteEst[i] = Estimator.DiscEstimation[i];
	for (i = 0; i < dT->noNumeric; i++) {
		numericEst[i] = Estimator.NumEstimation[i];
		if (dT->opt->binaryEvaluateNumericAttributes && Estimator.isMyopic(*selectedEstimator))
			numericSplit[i] = Estimator.splitPoint[i] ;
		else numericSplit[i] = NAcont ;
	}

	// unwrap arrays
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	costMatrix.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	numericEst.unWrap(dummy);
	discreteEst.unWrap(dummy);
	numericSplit.unWrap(dummy) ;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);

	delete dT;

#if defined(R_PORT)
	PutRNGstate();
#endif
}

void estimateCoreReg(int *noInst, int *noDiscrete, int *noDiscVal,
		int *discData, int *noNumeric, double *numData, char **dscAttrNames,
		char **dscValNames, char **nmAttrNames, int *noOptions, char **optName,
		char **optValue, int *selectedEstimator, double *discEst,
		double *numEst, double *numSplitPoint) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData, costMatrix;
	numericData.wrap(*noNumeric * (*noInst), numData);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<double> discreteEst, numericEst, numericSplit;
	numericEst.wrap(*noNumeric, numEst);
	discreteEst.wrap(*noDiscrete, discEst);
	numericSplit.wrap(*noNumeric, numSplitPoint) ;
	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	int dummy, i;
	regressionTree *rT = new regressionTree;

	// read options
	rT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);
	// reset estimation options
	rT->opt->estOnReg.init(mFALSE);
	rT->opt->estOnReg[*selectedEstimator] = mTRUE;

	// prepare data, first description than matrixes
	//marray<char *> discAttrNames(*noDiscrete, 0), discValNames(*noDiscrete, 0), numAttrNames(*noNumeric, 0);
	rT->isRegression = mTRUE ;
	rT->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, discAttrNames, discValNames, numAttrNames);
	rT->dataFromR(*noInst, discreteData, numericData, mTRUE);
	// prepare data for training/testing
	rT->opt->splitSelection = ALL_TRAINING;
	rT->prepareDataSplits();
	rT->setDataSplit(rT->opt->splitIdx);

	marray<double> weight(rT->NoTrainCases, 1.0);
	attributeCount attrType;
	estimationReg Estimator(rT, rT->DTraining, weight, rT->NoTrainCases);
	Estimator.estimate(*selectedEstimator, 1, rT->noNumeric, 0, rT->noDiscrete,
			attrType);

	numericEst[0] = NAcont;
	for (i = 0; i < rT->noDiscrete; i++)
		discreteEst[i] = Estimator.DiscEstimation[i];
	for (i = 1; i < rT->noNumeric; i++) {
		numericEst[i] = Estimator.NumEstimation[i];
		if (rT->opt->binaryEvaluateNumericAttributes && Estimator.isMyopic(*selectedEstimator))
			numericSplit[i] = Estimator.splitPoint[i] ;
		else numericSplit[i] = NAcont ;
	}

	// unwrap arrays
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	numericEst.unWrap(dummy);
	discreteEst.unWrap(dummy);
	numericSplit.unWrap(dummy) ;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);

	delete rT;

#if defined(R_PORT)
	PutRNGstate();
#endif
}

void rfAttrEval(int *modelID, double *estOut) {
	// is modelID valid
	if (*modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	featureTree *dT = (featureTree*) allModels[*modelID]; // working Model
	dT->learnRF = mTRUE;

	marray<double> attrEst;
	attrEst.wrap(dT->noAttr + 1, estOut);

	dT->varImportance(attrEst);

	int dummy;
	attrEst.unWrap(dummy);

}

void rfOOB(int *modelID, double *oobAccuracy, double *oobMargin, double *oobCorrelation) {
	if (modelID == 0 || *modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	featureTree *dT = (featureTree*) allModels[*modelID]; // get the model
	if  (dT->learnRF) {
		*oobAccuracy = dT->avgOobAccuracy ;
		*oobMargin = dT->avgOobMargin ;
		*oobCorrelation = dT->avgOobCorrelation ;
	}
	else return ;
}

void ordEvalCore(int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		char **dscAttrNames, char **dscValNames, int *noOptions,
		char **optName, char **optValue, double *rePos, double *reNeg,
		double *anch, double *rndrePos, double *rndreNeg, double *rndAnch,
		int *noAttrVal, char **ordEvalFile, char **ordEvalRndFile, int *variant) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	int dummy, i, attrIdx, iA, iV, iS, iVout;
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData, noAttrValues;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	int maxAttrValues = noDiscreteValues[1];
	for (i = 2; i < *noDiscrete; i++)
		if (maxAttrValues < noDiscreteValues[i])
			maxAttrValues = noDiscreteValues[i];
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<double> reinfPosOut, reinfNegOut, anchorOut, rndReinfPosOut,
	rndReinfNegOut, rndAnchorOut;
	reinfPosOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1), rePos);
	reinfNegOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1), reNeg);
	anchorOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1), anch);
	rndReinfPosOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1) * noOEstats,
			rndrePos);
	rndReinfNegOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1) * noOEstats,
			rndreNeg);
	rndAnchorOut.wrap((*noDiscrete - 1) * (maxAttrValues + 1) * noOEstats,
			rndAnch);
	noAttrValues.wrap((*noDiscrete - 1) * (maxAttrValues + 1), noAttrVal);
	marray<char *> discAttrNames, discValNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}

	featureTree *dT = new featureTree;

	// read options
	dT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);

	// prepare data, first description than matrixes
	marray<double> numericData;
	marray<char *> numAttrNames(0, 0);
	dT->isRegression = mFALSE ;
	dT->dscFromR(*noDiscrete, noDiscreteValues, 0, discAttrNames, discValNames, numAttrNames);
	dT->dataFromR(*noInst, discreteData, numericData, mTRUE);
	// prepare data for training/testing
	dT->opt->splitSelection = ALL_TRAINING;
	dT->prepareDataSplits();
	dT->setDataSplit(dT->opt->splitIdx);

	// prepare data structures to hold results
	marray<marray<double> > reinfPos(dT->noDiscrete), reinfNeg(dT->noDiscrete),
			anchor(dT->noDiscrete);

	for (attrIdx = 1; attrIdx < dT->noDiscrete; attrIdx++) {
		// for each attribute we need space for its values
		reinfPos[attrIdx].create(dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues
				+ 1, 0.0);
		reinfNeg[attrIdx].create(dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues
				+ 1, 0.0);
		anchor[attrIdx].create(dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues + 1,
				0.0);
	}
	mmatrix<marray<double> > reinfPosRnd(dT->noDiscrete, maxAttrValues + 1),
			reinfNegRnd(dT->noDiscrete, maxAttrValues + 1), anchorRnd(
					dT->noDiscrete, maxAttrValues + 1);
	for (attrIdx = 1; attrIdx < dT->noDiscrete; attrIdx++) {
		for (iV = 0; iV <= dT->AttrDesc[dT->DiscIdx[attrIdx]].NoValues; ++iV) {
			reinfPosRnd(attrIdx, iV).create(noOEstats, 0.0);
			reinfNegRnd(attrIdx, iV).create(noOEstats, 0.0);
			anchorRnd(attrIdx, iV).create(noOEstats, 0.0);
		}
	}

	// call attribute evaluation
	marray<double> weight(dT->NoTrainCases, 1.0);
	estimation Estimator(dT, dT->DTraining, weight, dT->NoTrainCases);
	switch (*variant) {
	case 1:
		Estimator.ordAVdAeqNorm(1, dT->noDiscrete, kEqual, reinfPos, reinfNeg,
				anchor, reinfPosRnd, reinfNegRnd, anchorRnd);
		break;
	case 2:
		Estimator.ordAVdAeqNormAttrDiff1(1, dT->noDiscrete, expRank, reinfPos,
				reinfNeg, anchor, reinfPosRnd, reinfNegRnd, anchorRnd);
		break;
	case 3:
		Estimator.ordAVdAeqNormClDiff1(1, dT->noDiscrete, expRank, reinfPos,
				reinfNeg, anchor, reinfPosRnd, reinfNegRnd, anchorRnd);
		break;
	default:
		merror("ordEvalCore, invalid variant parameter ", "");
		break;
	}
	// initialize output structures
	reinfPosOut.init(NAcont);
	reinfNegOut.init(NAcont);
	anchorOut.init(NAcont);
	rndReinfPosOut.init(NAcont);
	rndReinfNegOut.init(NAcont);
	rndAnchorOut.init(NAcont);

	// count number of active (non missing) attribute values
	marray<marray<int> > noAV;
	dT->countAV(noAV);

	int noAttr = dT->noDiscrete - 1;
	// copy to output structures
	for (iA = 1; iA < dT->noDiscrete; iA++) {
		for (iV = 0; iV <= dT->AttrDesc[dT->DiscIdx[iA]].NoValues; ++iV) {
			if (iV == 0) {
				iVout = maxAttrValues;
				noAttrValues[iA - 1 + iVout * noAttr] = dT->NoTrainCases
						- noAV[iA][iV]; // all but missing
			} else {
				iVout = iV - 1;
				noAttrValues[iA - 1 + iVout * noAttr] = noAV[iA][iV];
			}
			reinfPosOut[iA - 1 + iVout * noAttr] = reinfPos[iA][iV];
			reinfNegOut[iA - 1 + iVout * noAttr] = reinfNeg[iA][iV];
			anchorOut[iA - 1 + iVout * noAttr] = anchor[iA][iV];
			for (iS = 0; iS < noOEstats; iS++) {
				rndReinfPosOut[iA - 1 + iVout * noAttr + iS * noAttr
							   * (maxAttrValues + 1)] = reinfPosRnd(iA, iV)[iS];
				rndReinfNegOut[iA - 1 + iVout * noAttr + iS * noAttr
							   * (maxAttrValues + 1)] = reinfNegRnd(iA, iV)[iS];
				rndAnchorOut[iA - 1 + iVout * noAttr + iS * noAttr
							 * (maxAttrValues + 1)] = anchorRnd(iA, iV)[iS];
			}
		}
	}
	// write to files
	FILE *fout;
	if (ordEvalFile && ordEvalFile[0] && strlen(ordEvalFile[0]) > 0) {
		if ((fout = fopen(ordEvalFile[0], "w")) == NULL) {
			merror("ordEvalCore, cannot open ordEvalFile: ", ordEvalFile[0]);
		} else {
			printAVest(fout, reinfPos, reinfNeg, anchor, dT);
			fclose(fout);
		}
	}
	if (ordEvalRndFile && ordEvalRndFile[0] && strlen(ordEvalRndFile[0]) > 0) {
		if ((fout = fopen(ordEvalRndFile[0], "w")) == NULL) {
			merror("ordEvalCore, cannot open ordEvalRndFile: ",
					ordEvalRndFile[0]);
		} else {
			printAVestRnd(fout, reinfPosRnd, reinfNegRnd, anchorRnd, dT);
			fclose(fout);
		}
	}
	// unwrap arrays
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	reinfPosOut.unWrap(dummy);
	reinfNegOut.unWrap(dummy);
	anchorOut.unWrap(dummy);
	rndReinfPosOut.unWrap(dummy);
	rndReinfNegOut.unWrap(dummy);
	rndAnchorOut.unWrap(dummy);
	noAttrValues.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}

	delete dT;
#if defined(R_PORT)
	PutRNGstate();
#endif
}

void calibrate(int *calMethod, int *noInst, int *correctCl,
		double *predictedPr, double *wght, int*noBins, int *noIntervals,
		double *interval, double *calProb) {
	// is modelID valid
	//if (*modelID < 0|| *modelID >= allModels.len() || allModels[*modelID] == 0 ||
	//		allModels[*modelID]->isRegression || *noInst <=0)
	//	return;
	//featureTree *dT = (featureTree*)allModels[*modelID]; // working Model

	// wrap arrays
	marray<int> correctClass;
	correctClass.wrap(*noInst, correctCl);
	marray<double> predictedProb, weight;
	predictedProb.wrap(*noInst, predictedPr);
	weight.wrap(*noInst, wght);
	int i, dummy;

	Calibrate cal;
	marray<sort3Rec> y(*noInst);
	for (i = 0; i < *noInst; i++) {
		y[i].value = correctClass[i];
		y[i].key = predictedProb[i];
		y[i].weight = weight[i];
	}
	y.setFilled(*noInst);
	switch (*calMethod) {
	case 1:
		cal.isoRegCal(y);
		break;
	case 2:
		cal.binIsoCal(y, *noBins);
		break;
	case 3:
		cal.binningCal(y, *noBins);
		break;
	case 4:
		cal.mergeCal(y, *noBins);
		break;
	default:
		merror("CORElearn C++:", "Invalid calibration method");
	}
	// copy results to output
	*noIntervals = cal.interval.len();
	for (i = 0; i < cal.interval.len(); i++) {
		interval[i] = cal.interval[i];
		calProb[i] = cal.calProb[i];
	}
	//unwrap arrays
	correctClass.unWrap(dummy);
	predictedProb.unWrap(dummy);
	weight.unWrap(dummy);
}

void modelEvaluate(int *noInst, int *correctCl, // int *predictedCl,
		double *predictedPr, double *costMx, int *noClasses,
		double *priorClProbability, double *accuracy, double *avgCost,
		double *infScore, double *auc, int *predictionMx, double *sensitivity,
		double *specificity, double *brier, double *kappa, double *precision, double *Gmean,
		double *KS, double *TPR, double *FPR) {
	// wrap arrays
	marray<int> correctClass, predictionMatrix;
	correctClass.wrap(*noInst, correctCl);
	// predictedClass.wrap(*noInst, predictedCl); // computed from predictedProb and costMatrix
	predictionMatrix.wrap(*noClasses * *noClasses, predictionMx);
	marray<double> predictedProb, costMatrix, priorClProb;
	predictedProb.wrap(*noInst * *noClasses, predictedPr);
	costMatrix.wrap(*noClasses * *noClasses, costMx);
	priorClProb.wrap(*noClasses, priorClProbability);
	mmatrix<double> CostMatrix;
	costMxFromR(*noClasses, costMatrix, CostMatrix);
	int i, j, dummy;

	marray<double> priorClassProb(*noClasses + 1, 0);
	for (i = 1; i <= *noClasses; i++)
		priorClassProb[i] = priorClProb[i - 1];

	marray<marray<double> > probDist(*noInst);
	for (i = 0; i < *noInst; i++) {
		probDist[i].create(*noClasses + 1, 0.0);
		for (j = 1; j <= *noClasses; j++)
			probDist[i][j] = predictedProb[i + (j - 1) * (*noInst)];
	}
	mmatrix<int> predMx(*noClasses + 1, *noClasses + 1, 0);

	modelEval(*noInst, correctClass, probDist, *noClasses, priorClassProb,
			CostMatrix, *accuracy, *avgCost, *infScore, *auc, predMx, *kappa,
			*sensitivity, *specificity, *brier, *precision, *Gmean, *KS, *TPR, *FPR);

	for (i = 1; i <= *noClasses; i++)
		for (j = 1; j <= *noClasses; j++)
			predictionMatrix[i - 1 + (j - 1) * *noClasses] = predMx(i, j);

	//unwrap arrays
	correctClass.unWrap(dummy);
	// predictedClass.unWrap(dummy);
	predictionMatrix.unWrap(dummy);
	predictedProb.unWrap(dummy);
	costMatrix.unWrap(dummy);
	priorClProb.unWrap(dummy);
}

void modelEvaluateReg(int *noInst, double *truePred, double *pred,
		double *avgPrediction, double *MSE, double *RMSE, double *MAE,
		double *RMAE) {

	// wrap arrays
	marray<double> truePrediction, prediction;
	prediction.wrap(*noInst, pred);
	truePrediction.wrap(*noInst, truePred);

	modelEvalReg(*noInst, truePrediction, prediction, *avgPrediction, *MSE,
			*RMSE, *MAE, *RMAE);

	//unwrap arrays
	int dummy;
	prediction.unWrap(dummy);
	truePrediction.unWrap(dummy);
}

// library version
void optionsInOut(int *modelID, char **fileName, char **io) {
	// is modelID valid
	if (modelID == 0 || *modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	dataStore *data = allModels[*modelID]; // working Model
	if (strcmp(io[0], "read") == 0) {
		data->opt->readConfig(fileName[0]);
	} else if (strcmp(io[0], "write") == 0) {
		data->opt->writeConfig(fileName[0]);
	} else {
		merror("Unrecognized directive for option processing: ", io[0]);
	}
}

void saveRF(int *modelID, char **fileName) {
	// is modelID valid
	if (modelID == 0 || !allModels.defined() || *modelID < 0 || *modelID >= allModels.len() || allModels[*modelID] == 0)
		return;
	featureTree *dT = (featureTree*) allModels[*modelID]; // working Model
	dT->learnRF = mTRUE;
	dT->writeRF(fileName[0]);
}

// read RF model from the file
void readRF(char **fileName, int *modelID) {
	featureTree *dT = 0;
	dataStore *model = 0;
	*modelID = allModels.memberPlace(model);
	// if there is no more space in the table
	if (*modelID < 0) {
		Rprintf("maximum number of models reached\n");
		return;;
	}
	allModels[*modelID] = new featureTree;
	dT = (featureTree*) allModels[*modelID]; // working model
	dT->learnRF = mTRUE;

	if (!dT->readForest(fileName[0])) {
		destroyOneCoreModel(modelID);
	}
}


void discretize(int *methodIdx, int *isRegression, int *noInst, int *noDiscrete, int *noDiscVal, int *discData,
		int *noNumeric, double *numData, char **dscAttrNames,
		char **dscValNames, char ** nmAttrNames, int *noOptions,
		char **optName, char **optValue, int *selectedEstimator,
		int *maxBns, int *noBnds, double *bnds) {
#if defined(R_PORT)
	GetRNGstate();
#endif
	// wrap arrays for security reasons
	marray<int> noDiscreteValues, discreteData;
	noDiscreteValues.wrap(*noDiscrete, noDiscVal);
	discreteData.wrap(*noDiscrete * (*noInst), discData);
	marray<double> numericData;
	numericData.wrap(*noNumeric * (*noInst), numData);
	marray<char*> optionsName, optionsValue;
	optionsName.wrap(*noOptions, optName);
	optionsValue.wrap(*noOptions, optValue);
	marray<int> noBounds, maxBins ;
	noBounds.wrap(*noNumeric, noBnds) ;
	marray<double> bounds;
	bounds.wrap(*noNumeric * (*noInst), bnds);

	marray<char *> discAttrNames, discValNames, numAttrNames;
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.wrap(*noDiscrete, dscAttrNames);
		discValNames.wrap(*noDiscrete, dscValNames);
	} else {
		discAttrNames.create(*noDiscrete, 0);
		discValNames.create(*noDiscrete, 0);
	}
	if (nmAttrNames && nmAttrNames[0]) {
		numAttrNames.wrap(*noNumeric, nmAttrNames);
	} else {
		numAttrNames.create(*noNumeric, 0);
	}

	if (*isRegression == 1){
		regressionTree *rT = new regressionTree;
		maxBins.wrap(*noNumeric-1, maxBns) ;
		// read options
		rT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);
		rT->opt->selectionEstimatorReg = *selectedEstimator ;

		// prepare data, first description than data matrices
		rT->isRegression = mTRUE ;
		rT->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, discAttrNames, discValNames, numAttrNames);
		rT->dataFromR(*noInst, discreteData, numericData, mTRUE);

		// prepare data for training/testing
		rT->opt->splitSelection = ALL_TRAINING;
		rT->prepareDataSplits();
		rT->setDataSplit(rT->opt->splitIdx);

		marray<double> weight(rT->NoTrainCases, 1.0);
		// double estDummy ;
		int j ;
		marray<double> attrBounds ;
		estimationReg Estimator(rT, rT->DTraining, weight, rT->NoTrainCases);
		noBounds[0] = 0 ;
		for (int idx=1 ; idx < Estimator.noNumeric ; idx++)	{
			switch (*methodIdx) {
			case discrGreedy:
				Estimator.discretizeGreedy(idx, maxBins[idx-1], attrBounds) ;
				break ;
			case discrEqFreq:
				Estimator.discretizeEqualFrequency(idx, maxBins[idx-1], attrBounds) ;
				break ;
			case discrEqWidth:
				Estimator.discretizeEqualWidth(idx, maxBins[idx-1], attrBounds) ;
				break ;
			default: merror("Invalid discretization method in function discretize for regression ","Rfront.cpp") ;
			break ;
			}
			noBounds[idx] = attrBounds.filled() ;
			for (j=0 ; j < attrBounds.filled(); j++)
				bounds[idx*(*noInst)+j] = attrBounds[j] ;
		}
		delete rT ;
	}
	else {
		featureTree *dT = new featureTree ;
		maxBins.wrap(*noNumeric, maxBns) ;

		// read options
		dT->opt->optionsFromStrings(*noOptions, optionsName, optionsValue);
		dT->opt->selectionEstimator = *selectedEstimator ;

		// prepare data, first description then data matrices
		dT->isRegression = mFALSE;
		dT->dscFromR(*noDiscrete, noDiscreteValues, *noNumeric, discAttrNames, discValNames, numAttrNames);
		dT->dataFromR(*noInst, discreteData, numericData, mTRUE);

		// prepare data for training/testing
		dT->opt->splitSelection = ALL_TRAINING;
		dT->prepareDataSplits();
		dT->setDataSplit(dT->opt->splitIdx);

		marray<double> weight(dT->NoTrainCases, 1.0);
		int j ;
		marray<double> attrBounds ;
		estimation Estimator(dT, dT->DTraining, weight, dT->NoTrainCases);
		for (int idx=0 ; idx < Estimator.noNumeric ; idx++)	{
			switch (*methodIdx) {
			case discrGreedy:
				Estimator.discretizeGreedy(idx, maxBins[idx], attrBounds, Estimator.noDiscrete) ;
				break ;
			case discrEqFreq:
				Estimator.discretizeEqualFrequency(idx, maxBins[idx], attrBounds) ;
				break ;
			case discrEqWidth:
				Estimator.discretizeEqualWidth(idx, maxBins[idx], attrBounds) ;
				break ;
			default: merror("Invalid discretization method in function discretize for classification","Rfront.cpp") ;
			break ;
			}
			noBounds[idx] = attrBounds.filled() ;
			for (j=0 ; j < attrBounds.filled(); j++)
				bounds[idx*(*noInst)+j] = attrBounds[j] ;
		}
		delete dT;
	}
	// unwrap arrays
	int dummy;
	noDiscreteValues.unWrap(dummy);
	discreteData.unWrap(dummy);
	numericData.unWrap(dummy);
	optionsName.unWrap(dummy);
	optionsValue.unWrap(dummy);
	if (dscAttrNames && dscAttrNames[0]) {
		discAttrNames.unWrap(dummy);
		discValNames.unWrap(dummy);
	}
	if (nmAttrNames && nmAttrNames[0])
		numAttrNames.unWrap(dummy);
	noBounds.unWrap(dummy);
	bounds.unWrap(dummy);
	maxBins.unWrap(dummy) ;

#if defined(R_PORT)
	PutRNGstate();
#endif
	return;
}




#if defined(R_PORT)

SEXP exportSizesRF(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid
	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (R_NilValue);
	featureTree *dT = (featureTree*) allModels[mi]; // working Model
	dT->learnRF = mTRUE;
	return (dT->exportSizes());
}

SEXP exportSumOverLeavesRF(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid
	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (R_NilValue);
	featureTree *dT = (featureTree*) allModels[mi]; // working Model
	dT->learnRF = mTRUE;
	return (dT->exportSumOverLeaves());
}

SEXP exportModel(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid
	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (R_NilValue);
	featureTree *dT = (featureTree*) allModels[mi]; // working Model
	dT->learnRF = mTRUE;
	return (dT->RF2R());
}

SEXP printTree2R(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid

	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (R_NilValue);
	dataStore *tree = allModels[mi]; // working Model
	char *treeStr ;
	if (tree->isRegression)
		treeStr = ((regressionTree*) tree)->printTreeStr();
	else
		treeStr = ((featureTree*) tree)->printFTreeStr();

	SEXP out;
	PROTECT(out = Rf_allocVector(STRSXP, 1));
	SET_STRING_ELT(out, 0, Rf_mkChar(treeStr));
	delete [] treeStr ;
	UNPROTECT(1);
	return(out);
}

SEXP printTreeDot2R(SEXP modelID) {
	int mi;
	mi = INTEGER(modelID)[0];
	// is modelID valid

	if (mi < 0 || mi >= allModels.len() || allModels[mi] == 0)
		return (R_NilValue);
	dataStore *tree = allModels[mi]; // working Model
	char *treeStr ;
	if (tree->isRegression)
		treeStr = ((regressionTree*) tree)->printTreeDot();
	else
		treeStr = ((featureTree*) tree)->printFTreeDot();

	SEXP out;
	PROTECT(out = Rf_allocVector(STRSXP, 1));
	SET_STRING_ELT(out, 0, Rf_mkChar(treeStr));
	delete [] treeStr ;
	UNPROTECT(1);
	return(out);
}

SEXP noEqualRows(SEXP data1, SEXP data2, SEXP nrowsd1, SEXP nrowsd2, SEXP ncols, SEXP tolerance, SEXP countOnce)
{
	int i, j, k, eq=0 ;
	int nrd1 = INTEGER(nrowsd1)[0], nrd2 = INTEGER(nrowsd2)[0], nc = INTEGER(ncols)[0] ;
	booleanT once = (booleanT)(INTEGER(countOnce)[0]) ;
	SEXP ans;
	double d, eps=REAL(tolerance)[0] ;

	PROTECT(ans = Rf_allocVector(INTSXP, 1));
	for (i=0; i < nrd1 ; i++) {
		for (j=0; j < nrd2 ; j++) {
			d = 0 ;
			for (k=0; k < nc; k++) {
				d += fabs(REAL(data1)[i + k*nrd1] - REAL(data2)[j+k*nrd2]) ;
				if (d > eps)
					break ;
			}
			if (d <= eps) {
				eq++ ;
				if (once)
					break ;
			}
		}
	}
	INTEGER(ans)[0] = eq ;
	UNPROTECT(1);
	return(ans);
}

#endif

#if !defined(R_PORT)
void simRcall() {
	int maxModel = 128;
	initCore(&maxModel);
	int noInst = 10, noNumeric = 2, noDisc = 3;
	int *discData = new int[noInst * noDisc];
	double *numData = new double[noInst * noNumeric];
	int noDiscreteValues[] = { 2, 3, 4 };
	int noClasses = noDiscreteValues[0];
	double *costMx = new double[noClasses * noClasses];
	int i, j;
	for (i = 0; i < noInst; i++) {
		discData[i] = 1 + i % 2;
		discData[noInst + i] = 1 + i % 3;
		discData[2 * noInst + i] = 1 + i % 4;
		numData[i] = 1.0 + 0.1 * i;
		numData[noInst + i] = 2.0 + 0.1 * i;
	}
	for (i = 0; i < noClasses; i++)
		for (j = 0; j < noClasses; j++)
			if (i == j)
				costMx[i + j * noClasses] = 0.0;
			else
				costMx[i + j * noClasses] = 1.0;
	int modelID;
	int noOptions = 4;
	char const* optionsName[] = { "action", "domainName", "rfNoTrees",
			"rfPredictClass" };
	char const* optionsVal[] = { "rf", "test", "100", "N" };
	buildCoreModel(&noInst, &noDisc, noDiscreteValues, discData, &noNumeric,
			numData, costMx, 0, 0, 0, &noOptions, (char**) optionsName,
			(char**) optionsVal, &modelID, &noClasses);
	int noPredict = noInst;
	int *pred = new int[noPredict];
	double *prob = new double[noPredict * noDiscreteValues[0]];
	double *regPred = new double[noPredict];
	predictWithCoreModel(&modelID, &noPredict, discData, numData, costMx, pred,
			prob, regPred, &noOptions, (char**) optionsName,
			(char**) optionsVal);

	for (i = 0; i < noPredict; i++) {
		Rprintf("%d  %d  ", i + 1, pred[i]);
		for (j = 0; j < noDiscreteValues[0]; j++)
			Rprintf("%5.3f ", prob[i + j * noPredict]);
		Rprintf("\n");
	}
	destroyOneCoreModel(&modelID);

	int correctCl[] = { 0, 0, 0, 0, 0, 1, 1, 1, 1, 1 };
	double predictedPr[] = { 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 };
	double weight[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	int calMethod = 3, noBins = 5, noIntervals = 0;
	noInst = 10;
	double *interval = new double[noInst];
	double *calProb = new double[noInst];
	calibrate(&calMethod, &noInst, correctCl, predictedPr, weight, &noBins,
			&noIntervals, interval, calProb);

	delete[] interval;
	delete[] calProb;
}
#endif

} //extern "C"

