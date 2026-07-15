#if !defined(UTILS_H)
#define UTILS_H

#include <cmath>
#include <cstdlib>
#include <cstdio>

#include "general.h"
#include "contain.h"
#include "error.h"
#include "mlist.h"
#include "mstring.h"


struct sortRec
{
   int value ;
   double key;
   inline void operator= (sortRec& X) { value=X.value; key=X.key; }
   inline int operator== (sortRec &X) { if (key==X.key) return 1 ; else return 0 ; }
   inline int operator> (sortRec& X) { if (key > X.key)  return 1 ; else return 0 ; }
   inline int operator< (sortRec& X) { if ( key < X.key)  return 1 ; else return 0 ; }
};

struct sort3Rec
{
   double value, key, weight;
   inline void operator= (sort3Rec& X) { value=X.value; key=X.key; weight=X.weight; }
   inline int operator== (sort3Rec &X) { if (key==X.key) return 1 ; else return 0 ; }
   inline int operator> (sort3Rec& X) { if (key > X.key)  return 1 ; else return 0 ; }
   inline int operator< (sort3Rec& X) { if ( key < X.key)  return 1 ; else return 0 ; }
};

void quicksort(sortRec* const T,int left, int right);
extern "C" int ascSortComp(const void *a, const void *b) ;
extern "C" int descSortComp(const void *a, const void *b) ;
void quicksort(sort3Rec* const T,int left, int right);
extern "C" int ascSort3Comp(const void *a, const void *b) ;
extern "C" int descSort3Comp(const void *a, const void *b) ;

// char* int2str(int Number, char* const Str);

//   logarithm of basis 2: compatibility sake
inline double mlog2(double x) { return double( log(x) / 0.69314718055994528622) ; }

#define sqrt2 1.414213562373
#define epsilon  1e-7

#define Phi 3.14159265359

inline long int sqr(int x) { return long(x)*long(x) ; }
inline double sqr(double x) { return x*x ; }
//inline int abs(int x) { return (x>=0 ? x : -x) ; }
//#if !defined(MICROSOFT)
//inline double abs(double x) { return (x>=0 ? x : -x) ; }
//#endif
inline double sign(double x) { if (x>0) return 1.0; else if (x<0) return -1.0; else return 0.0; }
int posCharStr(const char Chr, const char* Str) ;
int posLastCharStr(const char Chr, const char* Str);
void strTrim(char* const Source) ;
void trimWhite(char* const Source) ;

double multinomLog2(marray<double> &selector) ;
double L2(marray<double> &selector) ;
double gammaLn(double xx) ;
double erfcc(double x) ;

double mdlIntEncode(long int number) ;
double mdlIntEncode(double number) ;

char* fgetStrIgnoreTill(FILE *from, char *Str, char Ignore, char *SkipChars) ;
char* sgetStrIgnoreTill(char *stringFrom, char *Str, char Ignore) ;

double binom(int N, int selector) ;

int intRound(double x) ;
int intRoundD(double x) ;
long int longRound(long int x) ;
int no1bits(unsigned long int number) ;

void cvTable(marray<int> &splitTable, int NoCases, int cvDegree)  ;
void stratifiedCVtable(marray<int> &splitTable, marray<int> &classTable, int NoCases, int noClasses, int cvDegree) ;

void randomizedSample(marray<int> &sampleIdx, int sampleSize, int domainSize) ;
double Correlation(marray<double> &X, marray<double> &Y, int From, int To) ;

double timeMeasure(void) ;
double timeMeasureDiff(double Start, double Finish) ;

char* getWildcardFileName(const char* Path, const char *WildcardFileName);

// pseudo-random number generators
double nrran1Between(double From, double To) ;
int nrran1Between(int From, int To) ;
void nrran1Seed(long seed) ;
double mrg32k5aBetween(double From, double To) ;
int mrg32k5aBetween(int from, int to) ;
void mrg32k5aSeed(long seed) ;
// wrapper for the selected generator
double randBetween(double From, double To) ;
int randBetween(int from, int to) ;
void randSeed(long seed) ;
void testRand(int *n, double *x);
double randNormal(double mean, double stddev) ;

void printLine(FILE *to, const char *what, int times) ;
char *myToken(char *inStr, int &idx, const char *delimiters) ;
char *myTokenMDskip(char *inStr, int &idx, const char *delimiters) ;
void tokenizedList(char *buf, mlist<mstring> &names, const char *tokenSeparators) ;
int fscanfUntil(FILE *fin, char *buf, const char delimiter, int maxSize);

void statOE(marray<double> &num, int n, marray<double> &stat, double percentile, double value) ;
void modelEval(int SetSize, marray<int> &trueClass,
		marray<marray<double> > &probDist, int noClasses, marray<double> &priorProbability,
		mmatrix<double> &CostMatrix, double &Accuracy, double &avgCost,
		double &Inf, double &Auc, mmatrix<int> &PredictionMatrix, double &kappa,
		double &sensitivity, double &specificity, double &brier, double &precision, double &Gmean,
		double &KS, double &TPR, double &FPR)  ;
void modelEvalReg(int SetSize, marray<double> &truePrediction,
      marray<double> &prediction, double avgPredicted, double &SE, double &RSE,double &AE, double &RAE) ;
void costMxFromR(int noClasses, marray<double> &costs, mmatrix<double> &CostMatrix) ;

union ieee_double
{
	double d;
	struct { unsigned int p0,p1; } i;
};

double genNAcont() ;
int isNaN(double x) ;


//template <class T> void AvgStd(marray<T> &Number, int NoNumbers, double &Avg, double &Std) ;
//void intAvgStd(marray<int> &Number, int NoNumbers, double* const Avg, double* const Std) ;
//************************************************************
//
//                        AvgStd
//                        -------
//
//     computes average and standard deviation for int table
//
//************************************************************
template <class T> void AvgStd(marray<T> &Number, int NoNumbers, double &Avg, double &Std)
{
    int i ;
    Avg = Std = 0.0 ;
    for (i=0; i<NoNumbers ; i++)
    {
       Avg += Number[i] ;
       Std += sqr(Number[i]) ;
    }
    Avg /= double(NoNumbers) ;
    // corrected sample standard deviation
    Std = (Std - NoNumbers*sqr(Avg))/double(NoNumbers-1) ;
    if (Std > 0)
    	Std = sqrt(Std);
    else
    	Std = 0 ;
    //Std = sqrt(Std/double(NoNumbers) - sqr(Avg)) ; // uncorrected

}



#endif

