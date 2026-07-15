#if !defined(DATASTORE_H)
#define DATASTORE_H

#include <cstring>

#include "c45read.h"
#include "contain.h"
#include "options.h"



enum discretizationConst { discrGreedy=1, discrEqFreq = 2, discrEqWidth=3} ;

// data needed to describe attribute
class attribute
{
public:
   char *AttributeName ;
   booleanT continuous, isOrdinal ;
   int NoValues ;
   marray<Pchar > ValueName ;
   int tablePlace ; // a column index to the DiscData or NumData
   booleanT userDefinedDistance ;
   double DifferentDistance, EqualDistance ;
   marray<double> Boundaries ;
   marray<double> valueProbability ;
   attribute() { AttributeName = 0 ; continuous = mTRUE ; isOrdinal = mFALSE ; userDefinedDistance = mFALSE ;
                 NoValues = 0 ; tablePlace = -1 ; DifferentDistance = EqualDistance = 0.0 ; } ;
   ~attribute() { destroy(); };
   void destroy(void) ;
   int operator== (attribute &) { return 1; }
   int operator< (attribute &) { return 1; }
   int operator> (attribute &) { return 1; }

} ;



//  basic class data manipulations, provides data input and output
class dataStore
{

public:
   int noAttr, NoOriginalAttr, noNumeric, noDiscrete ;
   mmatrix<int> DiscData, DiscPredictData ;    // discrete data
   mmatrix<double> NumData, NumPredictData ;   // numeric data
   marray<int> ContIdx ; // index pointing to the place in description for continuous variable
   marray<int> DiscIdx;  // index pointing to the place in description for discrete variable
   marray<double> minValue, maxValue, valueInterval ; // valid for each training data
   marray<int> splitTable ;  // table of splits for cross-validation or random splits
   mmatrix<int> *dData ; // pointers to the discrete data to be used in learning/prediction
   mmatrix<double> *nData ; // pointers to the numerical data to be used in learning/prediction

   mmatrix<double> CostMatrix ;  // [predicted] [true]

   void SetValueProbabilities(void) ;
   void SetDistances(void) ;
   int names2dsc(void) ;
   int data2dat(void) ;
   void costsToCostMatrix(void) ;

   marray<attribute> AttrDesc;  // contains descriptions of all the attributes in the order as defined
                                // DiscIdx and ContIdx contain indexes to this structure

   int NoCases, NoTrainCases, NoTestCases, noClasses, NoPredict, minClass ;
   marray<int> DTraining, DTesting; // storage for indexes of instances of the training/testing data, set before training
                              // they are indexes of rows for *dData, *nData, DiscData, NumData, ...
   Options *opt ;  // parameters
   booleanT isRegression ;  // the type of problem: classification or regression

   dataStore() ;
   ~dataStore();
   int readProblem(booleanT isTrain, booleanT verbose) ;
   int readDescription(void);
   int readCosts(void);
   int readData(booleanT isTrain);
   int prepareDataSplits(void);
   int setDataSplit(int splitIdx);
   void clearData(booleanT isTrain);
   void clearPredictData(void);
   void clearDescription(void);
   int writeDescription(const char* DescriptionFileName) const ;
   int writeData(const char* DataFileName) const ;
   void countAV(marray<marray<int> > &noAV) const;
   void countNA(marray<int> &noNA) const ;
   int dscFromR(int noDiscreteAttr, marray<int> &noDiscreteValues, int noNumericAttr,
           marray<char* > &discAttrNames, marray<char* > &discValNames, marray<char* >  &numAttrNames) ;
   void dataFromR(int noInst, marray<int> &discreteData, marray<double> &numericData, booleanT isTrain) ;
   void costsFromR(marray<double> &costs) ;
   int c45names2dsc(c45read &c45r) ;
   int c45data2dat(c45read &c45r, booleanT isTrain);


} ;

#endif
