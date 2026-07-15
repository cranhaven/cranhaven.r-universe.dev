/********************************************************************
 *
 *   Name:    MODULE dataStore
 *
 *   Description: basic operations on decision trees, not regarding any
 *                special additives and improvements;
 *                 - data input
 *
 *********************************************************************/

#include <cstdio>    // read and write functions
#include <cstring>    // dealing with attribute and value names
#include <cstdlib>
#include <cfloat>

#include "general.h"
#include "utils.h"
#include "error.h"
#include "dataStore.h"
#include "c45read.h"
#include "options.h"

extern int NoEstimators ;
extern int NoEstimatorsReg ;
extern estDsc estName[] ;
extern estDsc estNameReg[] ;

using namespace std ;

/* Implementation of testing NA double values */
double NAcont = genNAcont() ; // do not change without need

// char CommentChar = "%#" ;
char dataSeparators[] = " ,\t";


/********************************************************************
 *
 *   Name:   class attribute
 *   Base:   /
 *
 *   Description:   description of attribute
 *
 *********************************************************************/



void attribute::destroy()
{
	delete [] AttributeName ;
	AttributeName = 0 ;
	if ( ValueName.defined() && (! continuous) )
	{
		for(int j=0 ; j < NoValues  ; j++)
			delete [] ValueName[j] ;
		ValueName.destroy() ;
	}
	Boundaries.destroy() ;
	valueProbability.destroy() ;

	NoValues = 0  ;

}



/********************************************************************
 *
 *   Name:   class dataStore
 *   Base:   bintreeReg
 *
 *   Description:   base for decision trees: data input, pruning
 *
 *********************************************************************/

// constructor
dataStore::dataStore()
{
	noAttr =  NoOriginalAttr = NoCases = NoPredict = NoTrainCases = NoTestCases = noClasses = 0 ;
	noNumeric = noDiscrete = 0 ;
	minClass = -1 ;
	isRegression = mFALSE ;
	opt = new Options() ;
	dData = &DiscData ;
	nData = &NumData ;

}


// destructor: clears dinamicaly reserved free store
dataStore::~dataStore()
{
	clearData(mTRUE);
	clearData(mFALSE);
	clearDescription();
	if (opt) {
		delete opt ;
		opt = 0 ;
	}
}


// ************************************************************
//
//                    readProblem
//                    -----------
//
//     determine the data format and read in the description
//     and data for the problem
//
// ************************************************************
int dataStore::readProblem(booleanT isTrain, booleanT verbose)
{
#if !defined(R_PORT)

	if (! opt->domainName[0])
	{
		merror("Uninitialised domain name","") ;
		return 0 ;
	}

	if (isTrain) { // fill the first set of data
		dData = &DiscData ;
		nData = &NumData ;
	} else {
		dData = &DiscPredictData ;
		nData = &NumPredictData ;
	}

	// clear previous data
	clearData(isTrain) ;
	clearDescription() ;

	// check the existance of .names and determine C4.5 or native format
	char FileName[MaxPath] ;

	snprintf(FileName, MaxPath, "%s%s.dsc", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;
	FILE *from ;
	if ((from=fopen(FileName,"r"))!=NULL)
	{
		fclose(from) ;
		// native format

		if (verbose) {
			Rprintf("\nReading description, ") ;
			fflush(stdout) ;
		}
		if (readDescription())
		{
			if (!isRegression)
				readCosts() ;
			if (verbose) {
				Rprintf("data, ") ;
				fflush(stdout) ;
			}
			if (readData(isTrain)) {
				if (verbose) {
					Rprintf("done.\n") ;
					fflush(stdout) ;
				}
			}
			else {
				merror("Reading a problem failed in data file.","") ;
				return 0 ;
			}
		}
		else {
			merror("Reading a problem failed in description file.","") ;
			return 0 ;
		}
	}
	else {
		snprintf(FileName, MaxPath, "%s%s.names", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;
		if ((from=fopen(FileName,"r"))!=NULL) {
			// C4.5 format
			if (verbose) {
				Rprintf("\nReading C4.5 names,") ;
				fflush(stdout) ;
			}
			c45read c45r ;
			c45r.readC45names(from) ;
			// get data
			snprintf(FileName, MaxPath, "%s%s.data", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;
			if ((from=fopen(FileName,"r"))!=NULL)
			{
				if (verbose) {
					Rprintf(" data,") ;
					fflush(stdout) ;
				}
				c45r.readC45data(from, opt->NAstring.getConstValue());
			}
			else {
				merror("Nonexistent data file",FileName) ;
				return 0 ;
			}

			// convert to internal representation
			if (verbose) {
				Rprintf(" converting, ") ;
				fflush(stdout) ;
			}
			if (c45names2dsc(c45r)) {
				if (c45data2dat(c45r,isTrain)) {
					if (isTrain) {
						prepareDataSplits() ;
					}
					if (verbose) {
						Rprintf(" done.\n") ;
						fflush(stdout) ;
					}
				}
				else return 0;
			}
			else return 0 ;
			if (!c45r.isRegression) {
				// get cost matrix
				snprintf(FileName, MaxPath, "%s%s.costs", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;
				if ((from=fopen(FileName,"r"))!=NULL)
				{
					if (verbose) {
						Rprintf(" C4.5 costs,") ;
						fflush(stdout) ;
					}
					c45r.readC45costs(from, CostMatrix) ;
				}
				else {
					isRegression = mFALSE ;
					readCosts() ;
				}
			}
		}
		else {
			snprintf(FileName, MaxPath, "%s%s", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;
			merror("Description file (.dsc or .names) does not exist for problem",FileName) ;
			return 0 ;
		}
	}
#endif // if !defined(R_PORT)
	return 1 ;
}




//************************************************************
//
//                    readDescription
//                    ---------------
//
//          read description of attributes from file
//
//************************************************************
int dataStore::readDescription(void)
{
	clearDescription();
	char DescFileName[MaxPath] ;
	snprintf(DescFileName, MaxPath, "%s%s.dsc", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;
	FILE *from ;
	if ((from=fopen(DescFileName,"r"))==NULL)
	{
		merror("Cannot open description file", DescFileName);
		return 0;
	}

	char buf[MaxNameLen+1] ;      // buffers for strings and
	int temp;                     // numbers
	int DescLen ;
	double fTemp1, fTemp2 ;
	char *retVal ;  // return  value
	// start reading data
	do {
		retVal = fgets(buf,MaxNameLen,from);
	} while  (retVal != 0 && (buf[0] == '#' || buf[0] == '%')) ;
	if (retVal == 0)
	{
		merror("Faulty format of data description file", DescFileName) ;
		return 0 ;
	}
	sscanf(buf,"%d",&DescLen) ;

	NoOriginalAttr = noAttr = DescLen -1 ;
	if (noAttr <= 0)
	{
		merror("Data description contains no attributes in file", DescFileName) ;
		return 0 ;
	}

	noNumeric = 0 ;
	noDiscrete = 0 ;
	ContIdx.create(DescLen, -1) ;
	DiscIdx.create(DescLen, -1) ;

	AttrDesc.create(DescLen) ;
	int i, j ;
	for (i=0; i< DescLen ; i++)
	{

		do {
			retVal = fgets(buf,MaxNameLen,from);  // name of a attribute
		} while  (retVal != 0 && (buf[0] == '#' || buf[0] == '%')) ;
		if (retVal == 0)
		{
			merror("Faulty format of data description file", DescFileName) ;
			return 0 ;
		}
		buf[strlen(buf)-1] = '\0' ;
		strTrim(buf) ;
		strcpy(AttrDesc[i].AttributeName = new char[strlen(buf)+1], buf) ;
		do {
			retVal = fgets(buf,MaxNameLen,from);  // description line
		} while  (retVal != 0 && (buf[0] == '#' || buf[0] == '%')) ;
		if (retVal == 0)  {
			merror("Faulty format of data description file", DescFileName) ;
			return 0 ;
		}
		sscanf(buf, "%d",&temp) ; // how many values attribute has
		if (temp == 0)  // numeric variable
		{
			AttrDesc[i].continuous = mTRUE ;
			AttrDesc[i].NoValues = 0 ;
			AttrDesc[i].tablePlace = noNumeric ;
			AttrDesc[i].userDefinedDistance = mFALSE ;
			AttrDesc[i].EqualDistance = AttrDesc[i].DifferentDistance = -1.0 ;

			ContIdx[noNumeric] = i ;
			// read distances if defined
			sscanf(buf,"%d%lf%lf",&temp, &fTemp1, &fTemp2) ; // equal and different distance
			if (fTemp1<=0.0 && fTemp2 <= 0.0) // meaning: not defined, use defaults
			{
				AttrDesc[i].userDefinedDistance = mFALSE ;
				AttrDesc[i].EqualDistance = AttrDesc[i].DifferentDistance = -1.0 ;
			}
			else
			{
				AttrDesc[i].userDefinedDistance = mTRUE ;
				AttrDesc[i].EqualDistance = double(fabs(fTemp1)) ;
				AttrDesc[i].DifferentDistance = double(fabs(fTemp2)) ;
			}
			noNumeric ++ ;
		}
		else
		{
			AttrDesc[i].continuous = mFALSE ;
			DiscIdx[noDiscrete] = i ;
			AttrDesc[i].tablePlace = noDiscrete ;
			noDiscrete ++ ;

			if (temp < 0)
			{
				// Assistent format: numeric attribute with predefined boundaries
				AttrDesc[i].Boundaries.create(-temp, -DBL_MAX) ;
				AttrDesc[i].NoValues = -temp+1 ;
			}
			else
			{
				// normal discrete attribute
				AttrDesc[i].NoValues = temp ;
			}

			// read the values of the attribute
			AttrDesc[i].ValueName.create(AttrDesc[i].NoValues) ;
			AttrDesc[i].valueProbability.create(AttrDesc[i].NoValues+1) ;


			if (temp > 0)  // normal discrete attribute
			{
				for (j=0  ;  j < temp ; j++)
				{
					retVal = fgets(buf,MaxNameLen,from) ;
					if (retVal == 0) {
						merror("Faulty format of data description file", DescFileName) ;
						return 0 ;
					}
					buf[strlen(buf)-1] = '\0' ;
					strTrim(buf) ;
					strcpy(AttrDesc[i].ValueName[j]=new char[strlen(buf)+1], buf) ;
				}
			}
			else
			{
				// numeric attribute with predefined boundaries
				// first interval
				retVal = fgets(buf,MaxNameLen,from) ;
				if (retVal == 0)  {
					merror("Faulty format of data description file", DescFileName) ;
					return 0 ;
				}
				sscanf(buf,"%lf",&fTemp1) ; // first boundary
				AttrDesc[i].Boundaries[0] = fTemp1 ;
				snprintf(buf, MaxNameLen+1, "( <= %.3f )", fTemp1) ;
				strcpy(AttrDesc[i].ValueName[0] = new char[strlen(buf)+1], buf) ;
				fTemp2 = fTemp1 ;
				// intervals with two boundaries
				for (j=1 ;  j < -temp ; j++)
				{
					retVal = fgets(buf,MaxNameLen,from) ;
					if (retVal == 0)  {
						merror("Faulty format of data description file", DescFileName) ;
						return 0 ;
					}
					sscanf(buf,"%lf",&fTemp2) ; // how many values attribute has
					AttrDesc[i].Boundaries[j] = fTemp2 ;
					snprintf(buf, MaxNameLen+1, "( > %.3f  &  <= %.3f )", fTemp1, fTemp2) ;
					strcpy(AttrDesc[i].ValueName[j] = new char[strlen(buf)+1], buf) ;
					fTemp1 = fTemp2 ;
				}
				// last interval
				snprintf(buf, MaxNameLen+1, "( > %.3f )", fTemp2) ;
				strcpy(AttrDesc[i].ValueName[j] = new char[strlen(buf)+1], buf) ;
			}
		}
	}
	if (feof(from) || !ferror(from) )
	{
		if (AttrDesc[0].continuous)
		{
			isRegression = mTRUE ;
		}
		else  {
			isRegression = mFALSE ;
			noClasses = AttrDesc[0].NoValues ;
		}
		fclose(from) ;
		return 1;
	}
	else
	{
		clearDescription();
		merror("Cannot read file",DescFileName) ;
		fclose(from)  ;
		return 0 ;
	}
}

//************************************************************
//
//                       clearDescription
//                       ----------------
//
//    free the store for description of attributes and classes
//
//************************************************************
void dataStore::clearDescription(void)
{
	AttrDesc.destroy() ;
	ContIdx.destroy() ;
	DiscIdx.destroy() ;
	CostMatrix.destroy() ;
	noAttr = NoOriginalAttr = noDiscrete = noNumeric = 0 ;
}

// ************************************************************
//
//                           readData
//                           --------
//
//                     read the data from file
//
// ************************************************************
int dataStore::readData(booleanT isTrain)
{
	clearData(isTrain) ;

	char DataFileName[MaxPath] ;
	snprintf(DataFileName, MaxPath, "%s%s.dat", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;

	// check the existance of file
	FILE *dfrom ;
	if ((dfrom=fopen(DataFileName,"r"))==NULL)
	{
		merror("Cannot open data file",DataFileName);
		return 0;
	}

	char strBuf[MaxNameLen+1] ;
	char *retVal ;
	int NoInst = 0 ;
	// read the data, skip comments
	do {
		retVal = fgets(strBuf, MaxNameLen, dfrom) ;
	} while  (retVal != 0 && (strBuf[0] == '%' || strBuf[0] == '#')) ;
	if (retVal == 0)  {
		merror("Faulty format of data file", DataFileName) ;
		return 0 ;
	}
	sscanf(strBuf,"%d",&NoInst) ;

	int temp, i ;
	double number;
	char buf[MaxIntLen] ;
	char msg[MaxPath] ;
	char *token ;
	if (noDiscrete)
		dData->create(NoInst, noDiscrete) ;
	if (noNumeric)
		nData->create(NoInst, noNumeric) ;
	int contJ, discJ ;

	for (i = 0 ; i < NoInst; i++)
	{

		do {
			retVal = fgets(strBuf, MaxNameLen, dfrom) ;
		} while  (retVal != 0 && (strBuf[0] == '#' || strBuf[0] == '%')) ;
		if (retVal == 0)  {
			merror("Faulty format of data file", DataFileName) ;
			return 0 ;
		}
		if (strBuf[strlen(strBuf)-1] == '\n')
			strBuf[strlen(strBuf)-1] = '\0' ;

		token = strtok(strBuf, dataSeparators );

		contJ = 0, discJ = 0 ;
		for (int j=0 ; j<= noAttr; j++ )
		{
			if (token == 0)
			{
				snprintf(buf, MaxIntLen, "%d",i+1) ;
				merror("Not enough values at example",buf) ;
			}
			if (AttrDesc[j].continuous)
			{
				if (strcmp(token,opt->NAstring.getValue()) == 0) {
					nData->Set(i,contJ,NAcont) ;
					if (j==0) // missing classification
					{
						snprintf(buf, MaxIntLen, "%d",i+1) ;
						merror("Missing class value at example ",buf) ;
					}
				}
				else {
					sscanf(token,"%lf", &number) ;
					nData->Set(i,contJ,number) ;
				}
				contJ ++ ;
			}
			else   // discrete attribute
			{
				if (AttrDesc[j].Boundaries.defined())
				{
					if (strcmp(token,opt->NAstring.getValue()) == 0)
						dData->Set(i,discJ,NAdisc) ;
					else {
						sscanf(token,"%lf", &number) ;
						temp = 0 ;
						while (temp < AttrDesc[j].Boundaries.len() &&
								number < AttrDesc[j].Boundaries[temp])
							temp ++ ;
						dData->Set(i, discJ, temp + 1) ;
					}
				}
				else
				{
					// ordinary discrete attribute
					if (strcmp(token,opt->NAstring.getValue()) == 0){
						dData->Set(i,discJ,NAdisc) ;
						if (j==0) // missing classification
						{
							snprintf(buf, MaxIntLen, "%d",i+1) ;
							merror("Missing class value at example ",buf) ;
						}
					}
					else {
						sscanf(token,"%d", &temp) ;
						if ((temp<=0) || (temp>AttrDesc[j].NoValues))
						{
							dData->Set(i,discJ,NAdisc) ;
							strcpy(msg, "Data file corrupted; example ") ;
							snprintf(buf, MaxIntLen, "%d",i+1) ;
							strcat(msg,buf) ;
							strcat(msg, ", Attribute ") ;
							snprintf(buf, MaxIntLen, "%d",j) ;
							strcat(msg,buf) ;
							strcat(msg, ": unexisting value (") ;
							snprintf(buf, MaxIntLen, "%d",temp) ;
							strcat(msg,buf) ;
							strcat(msg, "). ") ;
							merror(msg,"") ;
						}
						else
							dData->Set(i,discJ,temp) ;
					}
				}
				discJ ++ ;
			}
			token = strtok(0, dataSeparators );

		}
	}
	if ( ferror(dfrom) )
	{
		clearData(isTrain);
		merror("Cannot read data from data file", DataFileName);
		fclose(dfrom) ;
		return 0;
	}
	fclose(dfrom) ;


	if (isTrain) {
		NoCases = NoInst ;
		prepareDataSplits() ;
	}
	else {
		NoPredict = NoInst ;
	}

	return 1 ;
}


//************************************************************
//
//                       clearData
//                       ---------
//
//                free the store reserved for training data
//
//************************************************************
void dataStore::clearData(booleanT isTrain)
{
	if (isTrain) {
		DiscData.destroy();
		NumData.destroy() ;
		DTraining.destroy() ;
		DTesting.destroy() ;
		NoCases = NoTrainCases = NoTestCases = 0 ;
	}
	else {
		DiscPredictData.destroy();
		NumPredictData.destroy() ;
		NoPredict = 0 ;
	}
}


int dataStore::c45names2dsc(c45read &c45r)
{
	int iT, iN ;
	mlistNode<c45Dsc> *iat ;
	mlistNode<mstring> *iv ;
	noAttr = c45r.noDiscreteAttr + c45r.noNumericAttr -1;
	isRegression = c45r.isRegression ;

	if (isRegression) {
		noNumeric = 1 ;
		noDiscrete = 0 ;
	}
	else {
		noNumeric = 0 ;
		noDiscrete = 1 ;
	}
	ContIdx.create(noAttr+1, -1) ;
	DiscIdx.create(noAttr+1, -1) ;
	AttrDesc.create(noAttr+1) ;
	for (iT=1, iat = c45r.attrs.first(); iat != 0 ; iat = c45r.attrs.next(iat)) {
		switch (iat->value.aType){
		case attrDiscClass:
			strcpy(AttrDesc[0].AttributeName=new char[1+strlen(iat->value.name.getValue())], iat->value.name.getValue()) ;
			AttrDesc[0].continuous = mFALSE ;  // should be discrete
			AttrDesc[0].NoValues = noClasses = iat->value.values.len();
			AttrDesc[0].ValueName.create(AttrDesc[0].NoValues) ;
			AttrDesc[0].valueProbability.create(AttrDesc[0].NoValues+1) ;
			for (iN = 0, iv = iat->value.values.first() ; iv != 0 ; iv = iat->value.values.next(iv), iN++) {
				strcpy(AttrDesc[0].ValueName[iN]=new char[1+strlen(iv->value.getValue())], iv->value.getValue()) ;
			}
			DiscIdx[0] = 0 ;
			AttrDesc[0].tablePlace = 0 ;
			break ;
		case attrNumClass:
			strcpy(AttrDesc[0].AttributeName=new char[1+strlen(iat->value.name.getValue())], iat->value.name.getValue()) ;
			AttrDesc[0].continuous = mTRUE ;
			AttrDesc[0].NoValues = noClasses = 0 ;// iat->value.values.len();
			AttrDesc[0].userDefinedDistance = mFALSE ;
			AttrDesc[0].EqualDistance = AttrDesc[0].DifferentDistance = -1.0 ;
			ContIdx[0] = 0 ;
			AttrDesc[0].tablePlace = 0 ;
			break ;
		case attrOrdinal: AttrDesc[iT].isOrdinal = mTRUE ;  // no break here
		case attrDisc:
			strcpy(AttrDesc[iT].AttributeName=new char[1+strlen(iat->value.name.getValue())], iat->value.name.getValue()) ;
			AttrDesc[iT].continuous = mFALSE ;  // should be discrete
			AttrDesc[iT].NoValues = iat->value.values.len();
			AttrDesc[iT].ValueName.create(AttrDesc[iT].NoValues) ;
			AttrDesc[iT].valueProbability.create(AttrDesc[iT].NoValues+1) ;
			for (iN = 0, iv = iat->value.values.first() ; iv != 0 ; iv = iat->value.values.next(iv), iN++) {
				strcpy(AttrDesc[iT].ValueName[iN]=new char[1+strlen(iv->value.getValue())], iv->value.getValue()) ;
			}
			DiscIdx[noDiscrete] = iT ;
			AttrDesc[iT].tablePlace = noDiscrete ;
			noDiscrete++ ;
			iT++ ;
			break ;
		case attrNum:
			strcpy(AttrDesc[iT].AttributeName=new char[1+strlen(iat->value.name.getValue())], iat->value.name.getValue()) ;
			AttrDesc[iT].continuous = mTRUE ;
			AttrDesc[iT].NoValues = 0 ;
			AttrDesc[iT].tablePlace = noNumeric ;
			AttrDesc[iT].userDefinedDistance = mFALSE ;
			AttrDesc[iT].EqualDistance = AttrDesc[iT].DifferentDistance = -1.0 ;
			ContIdx[noNumeric] = iT ;
			noNumeric++ ;
			iT++ ;
			break ;
		case attrIgnore:
			break ;
		}
	}
	NoOriginalAttr = noAttr = noNumeric+noDiscrete-1 ;
	return 1 ;
}


// ************************************************************
//
//                      c45data2dat
//                      ---------
//
//        converts C4.5 data to native format
//
// ************************************************************
int dataStore::c45data2dat(c45read &c45r, booleanT isTrain)
{
	int i, j, discIdx, numIdx ;
	mlistNode<c45Data> *id ;

	if (isTrain) { // fill the first set of data
		NoCases= c45r.noDataCases ;
		dData = &DiscData ;
		nData = &NumData ;
	} else {
		NoPredict = c45r.noDataCases ;
		dData = &DiscPredictData ;
		nData = &NumPredictData ;
	}

	if (noDiscrete)
		dData->create(c45r.noDataCases, noDiscrete) ;
	if (noNumeric)
		nData->create(c45r.noDataCases, noNumeric) ;

	for (i=0, id = c45r.dat.first() ; i<c45r.noDataCases ; i++, id = c45r.dat.next(id))
	{
		if (isRegression) {
			numIdx =1 ;
			discIdx = 0 ;
		}
		else {
			numIdx = 0 ;
			discIdx = 1 ;
		}
		for (j=0 ; j < noDiscrete ; j++) {
			if (!isRegression && j==c45r.classIdx)
				dData->Set(i, 0, id->value.discData[j]) ;
			else {
				dData->Set(i, discIdx, id->value.discData[j]) ;
				++discIdx ;
			}
		}
		for (j=0 ; j < noNumeric ; j++) {
			if (isRegression && j==c45r.classIdx)
				nData->Set(i, 0 , id->value.numData[j]) ;
			else
				nData->Set(i, numIdx, id->value.numData[j]) ;
			++numIdx ;
		}
	}
	return 1 ;
}


//************************************************************
//
//                       prepareDataSplits
//                       -----------------
//
//  prepares splits to train/test data
//
//************************************************************
int dataStore::prepareDataSplits(void) {

	randSeed(opt->rndSeedSplit) ;
	if (opt->splitSelection != FROM_FILES && opt->splitSelection != RANDOM_SPLIT) {
		// if FROM_FILES or RANDOM_SPLIT everything will be done at the time of use
		// generate  cross validation splits
		splitTable.create(NoCases) ;
		switch (opt->splitSelection) {
		case FROM_FILES:
		case RANDOM_SPLIT:
			break ;
		case CROSS_VALIDATION:
			cvTable(splitTable, NoCases, opt->numberOfSplits) ;
			break ;
		case STRATIFIED_CV:
		{
			marray<int> classTable(NoCases) ;
			for (int i=0 ; i < NoCases ; i++)
				classTable[i] = DiscData(i, 0) ;
			stratifiedCVtable(splitTable, classTable, NoCases, noClasses, opt->numberOfSplits) ;
		}
		break ;
		case LOOCV:
		{
			opt->numberOfSplits = NoCases ;
			for (int i=0 ; i < NoCases ; i++)
				splitTable[i] = i ;
		}
		break ;
		case ALL_TRAINING:
			splitTable.init(1) ;
			break ;
		}
	}
	return 1 ;
}


//************************************************************
//
//                       setDataSplit
//                       ------------
//
// sets the correct  data split in tables
//
//************************************************************
int dataStore::setDataSplit(int splitIdx)
{
	int NoTempTrain = 0, NoTempTest = 0, i ;
	marray<int> TempTrain(NoCases);
	marray<int> TempTest(NoCases) ;

	// use predefined splits on files
	if (opt->splitSelection == FROM_FILES) {
		// check the existance of file
		char ChoiceFileName[MaxPath] ;

		snprintf(ChoiceFileName, MaxPath, "%s.*%ds", opt->domainName.getConstValue(), splitIdx) ;
		char *FName = getWildcardFileName(opt->dataDirectory.getConstValue(), ChoiceFileName);
		if (FName == 0) {
			merror("Nonexistent choices file",ChoiceFileName);
			return 0;
		}
		strcpy(ChoiceFileName, FName) ;
		delete [] FName ;

		FILE *cfrom ;
		if ((cfrom=fopen(ChoiceFileName,"r"))==NULL) {
			merror("Cannot open choices file",ChoiceFileName);
			return 0;
		}

		int choice ;
		int retInt ;
		for (i=0; i<NoCases ; i++) {
			retInt = fscanf(cfrom,"%d", &choice) ;
			if (retInt != 1) {
				merror("Invalid structure of choices file",ChoiceFileName);
				fclose(cfrom) ;
				return 0;
			}
			if (choice==0)
				TempTrain[NoTempTrain++] = i;
			else
				if (choice==1)
					TempTest[NoTempTest++] = i;
		}

		if (ferror(cfrom)) {
			merror("\nCannot read data from choices file", ChoiceFileName) ;
			fclose(cfrom) ;
			return 0;
		}
		fclose(cfrom) ;
	}
	else if (opt->splitSelection == RANDOM_SPLIT) {
		for (i=0; i<NoCases ; i++)
			if (randBetween(0.0,1.0) <= opt->trainProportion)
				TempTrain[NoTempTrain++] = i;
			else
				TempTest[NoTempTest++] = i;
	}
	else {
		for (i=0; i<NoCases ; i++) {
			if (splitTable[i]!=splitIdx)
				TempTrain[NoTempTrain++] = i;
			else
				TempTest[NoTempTest++] = i;
		}
	}

	// set teach and test cases
	if (NoTempTrain == 0)     {
		merror("\nNo training instances", "") ;
		return 0 ;
	}
	NoTrainCases = NoTempTrain ;
	NoTestCases = NoTempTest ;
	DTraining.create(NoTrainCases);
	DTesting.create(NoTestCases);
	for (i=0; i<NoTrainCases ; i++)
		DTraining[i] = TempTrain[i] ;
	for (i=0; i<NoTestCases ; i++)
		DTesting[i] = TempTest[i] ;
	DTraining.setFilled(NoTrainCases) ;
	DTesting.setFilled(NoTestCases) ;

	SetValueProbabilities() ;

	SetDistances() ;

	return 1;
}


// ************************************************************
//
//                           readCosts
//                           --------
//
//                     read the costs from file
//
// ************************************************************
int dataStore::readCosts(void)
{
	if (isRegression)
		return 1 ;

	char CostFileName[MaxPath] ;
	snprintf(CostFileName, MaxPath, "%s%s.cm", opt->dataDirectory.getConstValue(), opt->domainName.getConstValue()) ;

	CostMatrix.create(noClasses+1,noClasses+1,0.0) ;

	// check the existance of file
	FILE *cfrom ;
	int i, j ;
	if ((cfrom=fopen(CostFileName,"r"))==NULL)
	{
		for (i=1; i <= noClasses; i++)
			for (j=1; j <= noClasses; j++)
				if (i==j) CostMatrix(i,j) = 0.0 ;
				else CostMatrix(i,j) = 1.0 ;
		return 1;
	}
	else
		Rprintf("costs, ") ;

	char strBuf[MaxNameLen+1], *token ;
	double costValue ;
	char buf[MaxIntLen] ;
	char *retVal ;
	for (i = 1 ; i <= noClasses; i++)
	{
		do {
			retVal = fgets(strBuf, MaxNameLen, cfrom) ;
		} while  (retVal != 0 && (strBuf[0] == '#' || strBuf[0] == '%')) ;
		if (retVal == 0)  {
			merror("Faulty format of cost file", CostFileName) ;
			return 0 ;
		}
		if (strBuf[strlen(strBuf)-1] == '\n')
			strBuf[strlen(strBuf)-1] = '\0' ;

		token = strtok(strBuf, dataSeparators );

		for (j=1 ; j<= noClasses; j++ )
		{
			if (token == 0)	 {
				snprintf(buf, MaxIntLen, "%d",j) ;
				merror("Not enough values for class value ",buf) ;
			}
			else {
				sscanf(token, "%lf", &costValue);
				CostMatrix(i, j) = costValue;
			}

			token = strtok(0, dataSeparators );
		}
	}
	fclose(cfrom) ;
	return 1 ;
}


//************************************************************
//
//                           writeDescription
//                           ----------------
//
//                     writes the description  to given file
//
//************************************************************
int dataStore::writeDescription(const char* DescriptionFileName) const
{
	FILE *descOut ;

	if ((descOut=fopen(DescriptionFileName,"w"))==NULL)
	{
		merror("Cannot create output description file", DescriptionFileName);
		return 0;
	}
	int i, j ;
	fprintf(descOut,"%d\n",noAttr+1) ;
	for (i=0 ; i<=noAttr ; i++)
	{
		fprintf(descOut,"%s\n",AttrDesc[i].AttributeName) ;
		if (AttrDesc[i].continuous)
		{
			fprintf(descOut,"0 \n") ;
		}
		else
		{
			fprintf(descOut,"%d\n",AttrDesc[i].NoValues) ;
			for (j=0 ; j < AttrDesc[i].NoValues ; j++)
				fprintf(descOut,"%s\n",AttrDesc[i].ValueName[j]) ;
		}
	}
	if (ferror(descOut))
	{
		merror("Error at writing description file to ",DescriptionFileName) ;
		fclose(descOut) ;
		return 0 ;
	}
	fclose(descOut) ;
	return 1 ;
}


//************************************************************
//
//                           writeData
//                           ---------
//
//                     writes data to given file
//
//************************************************************
int dataStore::writeData(const char* DataFileName) const
{
	FILE *dataOut ;
	if ((dataOut=fopen(DataFileName,"w"))==NULL)
	{
		merror("Cannot create output data file", DataFileName);
		return 0;
	}
	fprintf(dataOut,"%d\n",NoCases) ;
	int i,j ;
	for (i=0 ; i < NoCases ; i++)
	{
		for (j=0 ; j <= noAttr ; j++)
		{
			if (AttrDesc[j].continuous)
			{
				if (isNAcont(NumData(i, AttrDesc[j].tablePlace)))
					fprintf(dataOut," %10s", opt->NAstring.getConstValue()) ;
				else
					fprintf(dataOut," %10f",NumData(i, AttrDesc[j].tablePlace)) ;
			}
			else
			{
				if (DiscData(i, AttrDesc[j].tablePlace) == NAdisc)
					fprintf(dataOut," %4s",opt->NAstring.getConstValue()) ;
				else
					fprintf(dataOut," %4d",int(DiscData(i,AttrDesc[j].tablePlace))) ;
			}
		}
		fprintf(dataOut,"\n") ;
	}

	if (ferror(dataOut))
	{
		merror("Error at writing data file to ",DataFileName) ;
		fclose(dataOut) ;
		return 0 ;
	}

	fclose(dataOut) ;
	return 1 ;
}




//************************************************************
//
//                           SetValueProbabilities
//                           ---------------------
//
//            compute value probabilities for nominal attributes from training set
//
//************************************************************
void dataStore::SetValueProbabilities(void)
{
	int i, j ;
	marray<int> valueProb ;
	for (i=0 ; i < noDiscrete ; i++)
	{

		valueProb.create(AttrDesc[DiscIdx[i]].NoValues+1, 0) ;
		for (j=0 ; j < NoTrainCases ; j++)
			valueProb[DiscData(DTraining[j],i)] ++ ;

		// set prior probabilities with Laplace smoothing
		for (j=0 ; j <= AttrDesc[DiscIdx[i]].NoValues ; j++)
			AttrDesc[DiscIdx[i]].valueProbability[j] = double(valueProb[j]+1.0)/double(NoTrainCases + AttrDesc[DiscIdx[i]].NoValues ) ;

	}
	if (! isRegression) {
		minClass = 1 ;
		for (j=2; j <= AttrDesc[0].NoValues ; j++)
			if (AttrDesc[0].valueProbability[j] < AttrDesc[0].valueProbability[minClass])
				minClass = j ;
	}
}


//************************************************************
//
//                           SetDistances
//                           ---------
//
//            compute non user-defined distances from training set
//
//************************************************************
void dataStore::SetDistances(void)
{
	int i, j ;
	maxValue.create(noNumeric) ;
	minValue.create(noNumeric) ;
	valueInterval.create(noNumeric) ;
	for (i=0 ; i < noNumeric ; i++)
	{
		j=0 ;
		while (j < NoTrainCases && isNAcont(NumData(DTraining[j], i)))
			j++ ;
		if (j >= NoTrainCases) {
			merror("All values of the attribute in a data split are missing, please remove from learning:",AttrDesc[ContIdx[i]].AttributeName) ;
			minValue[i] = maxValue[i] = valueInterval[i] = NAcont ;
			continue ;
		}
		else
			minValue[i] = maxValue[i] = NumData(DTraining[j],i) ;

		for (j=j+1 ; j < NoTrainCases ; j++)
			if ( ! isNAcont(NumData(DTraining[j], i)))
			{
				if (NumData(DTraining[j], i) < minValue[i])
					minValue[i] = NumData(DTraining[j], i) ;
				else
					if (NumData(DTraining[j], i) > maxValue[i])
						maxValue[i] = NumData(DTraining[j], i) ;
			}
	}
	for (i=0 ; i < noNumeric ; i++)
	{
		valueInterval[i] = maxValue[i] - minValue[i] ;
		if (valueInterval[i] <= 0.0){
			merror("All values of the attribute in a data split are equal, please remove from learning:",AttrDesc[ContIdx[i]].AttributeName) ;
			valueInterval[i] = epsilon ;
		}
		if ( ! AttrDesc[ContIdx[i]].userDefinedDistance)
		{
			AttrDesc[ContIdx[i]].EqualDistance = valueInterval[i] * opt->numAttrProportionEqual ;
			AttrDesc[ContIdx[i]].DifferentDistance = valueInterval[i] * opt->numAttrProportionDifferent ;
		}
	}
}


//************************************************************
//
//                           countAV
//                           ---------
//
//            counts the values of discrete attributes
//
//************************************************************
void dataStore::countAV(marray<marray<int> > &noAV) const {
	noAV.create(noDiscrete+1) ;
	int i,k,attrIdx, iDisc = 0 ;
	for (i=1 ; i <= noAttr ; i++)   {
		if (AttrDesc[i].continuous)
			continue ;
		else ++iDisc ;
		attrIdx =  AttrDesc[i].tablePlace ;
		noAV[iDisc].create(AttrDesc[i].NoValues+1, 0) ;
		for (k=0 ; k < NoTrainCases ; k++)
			noAV[iDisc][DiscData(DTraining[k], attrIdx)] ++ ;
	}
}

//************************************************************
//
//                           countNA
//                           ---------
//
//            counts the missing values of numeric attributes
//
//************************************************************
void dataStore::countNA(marray<int> &noNA) const {
	noNA.create(noNumeric, 0) ;
	int i,k,attrIdx, iNum = 0 ;
	for (i=1 ; i <= noAttr ; i++)   {
		if (AttrDesc[i].continuous){
			attrIdx =  AttrDesc[i].tablePlace ;
			for (k=0 ; k < NoTrainCases ; k++)
				if (isNAcont(NumData(DTraining[k], attrIdx)))
					noNA[iNum] ++ ;
			++iNum ;
		}
	}
}


//************************************************************
//
//                           writeTree
//                           ---------
//
//                     writes Tree to given file
//
//************************************************************
/*int dataStore::writeSingleTree(const char* TreeFileName) const
{
   FILE *treeOut ;
   if ((treeOut=fopen(TreeFileName,"w"))==NULL)
   {
      merror("Cannot create output tree file", TreeFileName);
      return 0;
   }

   if (root)
     writeSubTree(treeOut,root,0) ;

   if (fmerror(treeOut))
   {
       merror("Error at writing tree to file ",TreeFileName) ;
       fclose(treeOut) ;
       return 0 ;
   }

   fclose(treeOut) ;
   return 1 ;
}
 */



// ************************************************************
//
//                           writeSubTree
//                           ---------
//
//                     writes subtree to given file
//
// ************************************************************
/*void dataStore::writeSubTree(FILE *treeOut, binnodeReg* Node, int tab) const
{
    int i ;
    switch (Node->Identification)
    {
         case leaf:
                   fprintf(treeOut, "%*s 1 %f %f %f %f %f \n",
                         tab, "",
                         Node->weight, Node->averageClassValue,
                         Node->stdDevClass, Node->squaresClass, Node->code) ;
                    break ;
         case continuousAttribute:
                   fprintf(treeOut, "%*s 2 %d %f %f %f %f %f %f %f \n",
                         tab, "", Node->AttrIdx,
                         Node->splitValue, Node->weight, Node->weightLeft,
                         Node->averageClassValue, Node->stdDevClass,
                         Node->squaresClass, Node->code) ;
                   writeSubTree(treeOut, Node->left, tab+4) ;
                   writeSubTree(treeOut, Node->right, tab+4) ;
                   break ;
          case discreteAttribute:
                   fprintf(treeOut, "%*s 3 %d %f %f %f %f %f %f ",
                         tab, "", Node->AttrIdx,
                         Node->weight, Node->weightLeft,
                         Node->averageClassValue, Node->stdDevClass,
                         Node->squaresClass, Node->code) ;
                   for (i=0 ; i < Node->leftValues.len() ; i++)
                       if (Node->leftValues[i])
                           fprintf(treeOut, "1 ") ;
                       else
                           fprintf(treeOut, "0 ") ;
                   fprintf(treeOut,"\n") ;
                   writeSubTree(treeOut, Node->left, tab+4) ;
                   writeSubTree(treeOut, Node->right, tab+4) ;
                   break;
          default:
                   merror("dataStore::wrteSubTree","invalid node identification") ;
     }
}
 */

// ************************************************************
//
//                           readTree
//                           ---------
//
//                     reads Tree from given file
//
// ************************************************************
/*
   int dataStore::readTree(const char* TreeFileName) const
{
   FILE *treeIn ;
   if ((treeIn=fopen(TreeFileName,"r"))==NULL)
   {
      merror("Cannot read tree from file", TreeFileName);
      return 0;
   }

   destroy(root) ;
   root = 0 ;
   readSubTree(treeIn,root) ;

   if (fmerror(treeIn))
   {
       merror("Error reading tree from file ",TreeFileName) ;
       fclose(treeIn) ;
       return 0 ;
   }

   fclose(treeIn) ;
   return 1 ;
}

// ************************************************************
//
//                           readSubTree
//                           ---------
//
//                     reads Tree from given file
//
// ************************************************************
void dataStore::readSubTree(FILE *treeIn, binnodeReg* &Node) const
{
    Node = new binnodeReg ;
    int dTemp, d1Temp ;
    char buf[MaxNameLen+1] ;
    char buf1[MaxNameLen] ;
    fgets(buf, MaxNameLen, treeIn);
    int i ;
    sscanf(buf, "%d", &dTemp)  ;

    switch (dTemp)
    {
       case 1:  // leaf
               Node->Identification = leaf ;
               sscanf(buf,"%d%lf%lf%lf%lf%lf", &d1Temp, &(Node->weight), &(Node->averageClassValue), &(Node->stdDevClass), &(Node->squaresClass), &(Node->code) ) ;
               Node->left = Node->right = 0 ;
               break ;
       case 2: // node with numeric attribute
               Node->Identification = continuousAttribute ;
               sscanf(buf, "%d%d%lf%lf%lf%lf%lf%lf%lf",
                            &d1Temp, &(Node->AttrIdx),
                            &(Node->splitValue), &(Node->weight),
                            &(Node->weightLeft), &(Node->averageClassValue),
                            &(Node->stdDevClass), &(Node->squaresClass),
                            &(Node->code) ) ;
               readSubTree(treeIn, Node->left) ;
               readSubTree(treeIn, Node->right) ;
               break ;
       case 3: // node with discrete attribute
               Node->Identification = discreteAttribute ;
               sscanf(buf, "%d%d%lf%lf%lf%lf%lf%lf%s",
                           &d1Temp, &(Node->AttrIdx),
                           &(Node->weight), &(Node->weightLeft),
                           &(Node->averageClassValue), &(Node->stdDevClass),
                           &(Node->squaresClass), &(Node->code), buf1 ) ;
               Node->leftValues.create(AttrDesc[DiscIdx[Node->AttrIdx]].NoValues+1) ;
               strTrim(buf1) ;
               if (strlen(buf1) != AttrDesc[DiscIdx[Node->AttrIdx]].NoValues )
                  merror("dataStore::readSubTree","number of discrete attribute values do not match") ;
               for (i=1; i < Node->leftValues.len(); i++)
               {
                  if (buf1[i-1] == '1')
                    Node->leftValues[i] = mTRUE ;
                  else
                    if (buf1[i-1] == '0')
                       Node->leftValues[i] = mFALSE ;
                    else
                      merror("dataStore::readSubTree","invalid indicator of nominal attribute value") ;
               }
               readSubTree(treeIn, Node->left) ;
               readSubTree(treeIn, Node->right) ;
               break ;
      default: merror("dataStore::readSubTree","invalid type of node") ;

    }
}
 */


