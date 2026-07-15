/********************************************************************
 *
 *   Name:                 MODULE utils
 *
 *   Description: tools and utilities for other modules,
 *
 *
 *               - quicksort
 *               - integer to string
 *               - logarithm of basis 2
 *               - Rissanen's codes
 *               - some maths
 *
 *********************************************************************/
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <climits>
#include <cfloat>


#include "general.h"

#if defined(UNIX)
#include <sys/times.h>
#include <glob.h>
#include <stdint.h>
#define __int64 int64_t
#endif

#if defined(MICROSOFT)
#include <io.h>
#endif

#include "utils.h"
#include "error.h"
#include "contain.h"
#include "mathutil.h"

// used for quicksort
int (*fcmp) (sortRec &X, sortRec& Y) ;
int (*fcmp3) (sort3Rec &X, sort3Rec& Y) ;

marray<sortRec> *heap ;

using namespace std ;


//************************************************************
//
//                        int2str
//                        -------
//
//        converts integer to string: compatibility sake
//
//************************************************************
/*
char* int2str(int Number, char* const Str)
{
   int Len = 0;
   for (int i=Number; i != 0; i /= 10) Len++ ;
   if (Number<0)
   {
      Str[0]='-';
      Len++;
   }
   if (Number==0)
   {
     Str[0]='0';
     Str[1]='\0' ;
   }
   else
   {
      Str[Len--] = '\0' ;
      while (Number != 0)
      {
         Str[Len--] = char('0' + Number % 10);
          Number /= 10 ;
      }
   }
   return Str;
}
 */


//************************************************************
//
//                        quicksort
//                        ---------
//
//  standard algorithm for sorting; sorts only sort records
//                defined in header file
//
//************************************************************
void quicksort(sortRec* const T, int left, int right)
{

	qsort((void *)T, right-left+1, sizeof(sortRec), ascSortComp);

}


extern "C" int ascSortComp( const void *a, const void *b)
{
	return ( ( ((sortRec*)a)->key > ((sortRec*)b)->key ) ? 1 : (-1) ) ;
}


extern "C" int descSortComp( const void *a, const void *b)
{
	return ( ( ((sortRec*)a)->key < ((sortRec*)b)->key ) ? 1 : (-1) ) ;
}

void quicksort(sort3Rec* const T, int left, int right)
{

	qsort((void *)T, right-left+1, sizeof(sort3Rec), ascSort3Comp);

}


extern "C" int ascSort3Comp( const void *a, const void *b)
{
	return ( ( ((sort3Rec*)a)->key > ((sort3Rec*)b)->key ) ? 1 : (-1) ) ;
}


extern "C" int descSort3Comp( const void *a, const void *b)
{
	return ( ( ((sort3Rec*)a)->key < ((sort3Rec*)b)->key ) ? 1 : (-1) ) ;
}



//************************************************************
//
//                      posCharStr
//                      ----------
//
//       returns the index of char in a string,
//       -1 if there is none
//
//************************************************************
int posCharStr(const char Chr, const char* Str)
{
	int i = 0 ;
	while ( Str[i] )
		if (Str[i] == Chr)
		{
			return i ;
		}
		else ++i ;
	return -1 ;
}

int posLastCharStr(const char Chr, const char* Str)
{
	int i = strlen(Str)-1 ;
	while ( i >=0)
		if (Str[i] == Chr)
		{
			return i ;
		}
		else --i ;
	return -1 ;
}


//************************************************************
//
//                        strTrim
//                        -------
//
//     removes leading and trailing spaces from given string
//
//************************************************************
void strTrim(char* const Source)
{
	// trim from right
	int pos=0 ;
	while (Source[pos]) pos ++ ;
	pos-- ;
	while (pos >= 0 && Source[pos] == ' ')
		pos-- ;
	if (pos >=0)
		Source[pos+1] = '\0' ;
	else
	{
		Source[0] = '\0' ;
		return ;
	}

	// trim from left ;
	pos = 0 ;
	while (Source[pos]==' ')
		pos++ ;
	if (pos>0)
	{
		int i ;
		for (i = 0 ; Source[pos] != '\0' ; i++, pos++)
			Source[i] = Source[pos] ;
		Source[i] ='\0' ;
	}
}


//************************************************************
//
//                        trimWhite
//                        -------
//
//     removes leading and trailing white spaces from given string
//
//************************************************************
void trimWhite(char* const Source)
{
	// trim from right
	int pos=0 ;
	while (Source[pos]) pos ++ ;
	pos-- ;
	while (pos >= 0 && (Source[pos] == ' ' || Source[pos] == '\t' || Source[pos] == '\n'))
		pos-- ;
	if (pos >=0)
		Source[pos+1] = '\0' ;
	else
	{
		Source[0] = '\0' ;
		return ;
	}

	// trim from left ;
	pos = 0 ;
	while (Source[pos]==' ' || Source[pos] == '\t' || Source[pos] == '\n')
		pos++ ;
	if (pos>0)
	{
		int i ;
		for (i = 0 ; Source[pos] != '\0' ; i++, pos++)
			Source[i] = Source[pos] ;
		Source[i] ='\0' ;
	}
}

//************************************************************
//
//                         multinomLog2
//                         ------------
//
//            computes logarithm of base 2 of a kind of multinom:
//              positive real values are used instead of integers
//
//************************************************************
double multinomLog2(marray<double> &selector)
{
	const double ln2 = 0.69314718055994528622 ;

	int noSelectors = selector.filled() ;
	int i ;
	double noAll = 0.0 ;
	for (i = 0 ; i < noSelectors ; i ++ )
		noAll += selector[i] ;

	//   int selMax = 0 ;
	//   for (i = 1 ; i < noSelectors ; i ++ )
	//     if (selector[i] > selector[selMax])
	//        selMax = i ;

	// log2(N!)
	double lgNf = gammaLn(noAll+double(1.0))/ln2  ;

	// log2(n_i !)
	marray<double> lgnFac(noSelectors) ;

	for (i=0 ; i< noSelectors ; i++)
	{
		if ((selector[i] == 0) || (selector[i] == 1) )
			lgnFac[i] = 0.0 ;
		else
			if (selector[i] == 2)
				lgnFac[i] = 1.0 ;
			else
				if (selector[i] == noAll)
					lgnFac[i] = lgNf ;
				else
					lgnFac[i] = gammaLn(selector[i]+double(1.0))/ln2  ;
	}

	//   double temp = log2(noAll) ;
	//   if (lgNf - lgnFac[selMax] < temp )
	//   {
	//      double sumLg = 0.0 ;
	//      for (i=0 ; i < noSelectors ; i++)
	//        if (i != selMax)
	//           sumLg += lgnFac[i] ;
	//
	//      delete [] lgnFac ;
	//      return selector[selMax] * temp - sumLg ;
	//   }
	//   else
	//   {
	for (i=0 ; i < noSelectors ; i++)
		lgNf -= lgnFac[i] ;

	return lgNf ;
	//   }
}


//************************************************************
//
//                         L2 function
//                         ------------
//
//            computes L function in logarithm of base 2 (a modified multinom function)
//      as defined in M. Mehta, J. Rissanen, R. Agrawal: MDL-based Decision Tree Pruning
//         (Proceedings of KDD-95)
//              positive real values are used instead of integers
//
//************************************************************
double L2(marray<double> &selector)
{
	const double ln2 = 0.69314718055994528622 ;
	const double lnPi = 1.144729885849 ;

	int noSelectors =  selector.filled() ;
	double noAll = 0.0 ;
	int i ;
	for (i = 0 ; i < noSelectors ; i ++ )
		noAll += selector[i] ;

	double L = 0.0 ;
	for (i=0 ; i< noSelectors ; i++)
	{
		if (selector[i] != 0.0)
			L += selector[i] * log(noAll/selector[i])  ;
		// else L += 0.0 ;
	}

	L += double(noSelectors-1)/double(2.0) * log(noAll/double(2.0)) + (double(noSelectors)/double(2.0))*lnPi -
			gammaLn(noSelectors/double(2.0)) ;

	L /= ln2 ;
	return double(L) ;
}


//*********************************************************************
//
//                          gammaLn
//                          -------
//
//       computes natural logarithm of gamma function
//              for argument xx > 0;
//        taken from William H. Press, Saul A. Teukolsky,
//         William T. Vetterling, Brian P. Flannery:
//         NUMERICAL RECIPES IN C, The Art od Scientific Computing,
//         Second edition,  Cambridge University Press, 1992
//          approximation by Lanczos:
//
//                                  (z+1/2)    -(z + g +1/2)
//          GAMA(z+1) = (z + g + 1/2)^      * e^              *
//                        _____
//                    * \/ 2*PI * [ c0 + c1/(z+1) + c2/(z+2) + ....
//
//                                  cN/(z+N) + epsilon    ]   ;  Re(z)  > 0
//
//          function bellow works for g=6, N=6, error is smaller than
//           |epsilon| < 2e-10
//
//**********************************************************************
double gammaLn(double xx)
{

	// internal arithmetic will be done in double precision ;
	// single precision will be enough if 5 figure precision is OK

	double x, y, tmp, ser ;

	static double cof[6] = { 76.18009172947146, -86.50532032941677,
			24.01409824083091, -1.231739572450155,
			0.1208650973866179e-2, -0.5395239384953e-5
	} ;

	int j ;


	y = x = xx ;
	tmp = x + 5.5 ;
	tmp -=  (x + 0.5) * log(tmp) ;
	ser = 1.000000000190015 ;

	for (j=0 ; j <= 5 ; j++)
		ser += cof[j] / ++y ;

	return double(-tmp + log(2.5066282746310005 * ser / x)) ;

}


//************************************************************
//
//                         mdlIntEncode
//                         ------------
//
//        lengthh of Rissanen's coding of natural numbers:
//     code(0) = 1
//     code(n) = 1 + log2(n) + log2(log2(n)) + ... + log2(2.865064..)
//       where the sum includes only the term that are positive
//
//************************************************************
double mdlIntEncode(long int number)
{
	if (number==0)
		return 1.0 ;

	double code = double(1.0) + mlog2(double(2.865064)) ;

	double logarithm = mlog2((double)number) ;

	while (logarithm > 0)
	{
		code += logarithm ;
		logarithm = mlog2((double)logarithm) ;
	}

	return code ;
}


double mdlIntEncode(double number)
{

	number = fabs(number) ;

	if (number==0.0)
		return 1.0 ;

	double code = double(1.0) + mlog2(double(2.865064)) ;

	double logarithm = mlog2(number) ;

	while (logarithm > 0)
	{
		code += logarithm ;
		logarithm = mlog2(logarithm) ;
	}

	return code ;
}


//************************************************************
//
//                         fgetStrIgnoreTill
//                         -----------------
//
//       reads line from file, ignoring contens until given character, skiping some lines
//
//**********************************************************
char* fgetStrIgnoreTill(FILE *from, char *Str, char Ignore, char* SkipChars)
{
	char bufWhole[MaxNameLen] ;
	char *retChar ;
	do {
		retChar = fgets(bufWhole,MaxNameLen,from) ;
		if (retChar == 0)
			return 0 ;
		while (bufWhole[strlen(bufWhole)-1] == '\n' || bufWhole[strlen(bufWhole)-1] == '\r')
			bufWhole[strlen(bufWhole)-1] = '\0' ;
	}  while (strchr(SkipChars, bufWhole[0])) ;

	char *buf=bufWhole;
	while (buf[0] != '\0' && buf[0] != Ignore )
		buf++ ;
	if (buf[0] != '\0')
	{
		buf++ ;
		strTrim(buf) ;
	}
	strcpy(Str,buf) ;
	return Str ;
}


//************************************************************
//
//                         sgetStrIgnoreTill
//                         -----------------
//
//           reads line from string, ignoring contens until given character
//
//************************************************************
char* sgetStrIgnoreTill(char *stringFrom, char *Str, char Ignore)
{
	char *buf=stringFrom ;
	while (buf[0] != '\0' && buf[0] != Ignore )
		buf++ ;
	if (buf[0] != '\0')
	{
		buf++ ;
		strTrim(buf) ;
	}
	strcpy(Str,buf) ;
	return Str ;
}


//************************************************************
//
//                         erfcc
//                         ------
//
//      complement of error function errf, computed by
//   aproximation based on Chebyshev fitting
//      (Numerical Recipes in C)
//
//************************************************************
double erfcc(double x)
{
	double t,z,ans;

	z=double(fabs(x));
	t=1.0/(1.0 + 0.5*z);
	ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
			t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
					t*(-0.82215223+t*0.17087277)))))))));
	return x >= 0.0 ? ans : 2.0-ans;
}


//************************************************************
//
//                         round
//                         ------
//
//           rounds to nearest integer
//
//
//************************************************************
// round to the nearest integer, if half way between two integers round to larger (absolute) integer
int intRound(double x)
{
	return (x>=0 ? int(x+0.5) : int(x-0.5)) ;
}

// round to the nearest integer, if half way between two integers round to smaller (absolute) integer
int intRoundD(double x)
{
	return (x>=0 ? (x-0.5 > int(x) ? int(x)+1 : int(x)) : (x + 0.5 < int(x) ? int(x)-1 :  int(x))) ;
}

long int longRound(long int x)
{
	return (x>=0 ? (long int)(x+0.5) : (long int)(x-0.5)) ;
}



//************************************************************
//
//                         binom
//                         ------
//
//           simple implementation of binomial coefficient
//
//
//************************************************************
double binom(int n, int k) {
	if (k > n)
		return 0;

	if (k > n/2)
		k = n-k; // Take advantage of symmetry

	double b = 1.0;
	for (int i = 1; i <= k; i++)
		b *= (n-k+i) / i;

	return (b + 0.5);
}
/*
 long int binom(int N, int selector)
{

  long int value = 1 ;
  selector = Mmin(selector, N-selector) ;
  int j ;
  for (j=N ; j > N-selector ; j--)
    value *= j ;
  for (j=2 ; j <= selector ; j++)
     value /= j ;
  return value ;
}
 */


//************************************************************
//
//                         no1bits
//                         -------
//
//     number of set (1) bits in number
//
//
//************************************************************
int no1bits(unsigned long int number)
{
	int no1 = 0 ;
	while (number)
	{
		no1 += number % 2 ;
		number /= 2 ;
	}
	return no1 ;
}



// ************************************************************
//
//                         Correlation
//                         -----------
//
//     returns standard linear correlation coefficient between two arrays
//
//
// ************************************************************
double Correlation(marray<double> &X, marray<double> &Y, int From, int To)
{
	double sumX=0, sumY=0, sumXY=0, sumX2=0, sumY2=0 ;
	for (int i=From ; i < To ; i++)
	{
		sumX += X[i] ;
		sumY += Y[i] ;
		sumXY += X[i] * Y[i] ;
		sumX2 += sqr(X[i]) ;
		sumY2 += sqr(Y[i]) ;
	}
	int  N = To - From ;
	double divisor = 0, temp ;
	temp = N*sumX2 - sqr(sumX) ;
	if (temp > 0)
		divisor += sqrt(temp) ;
	temp = N*sumY2 - sqr(sumY) ;
	if (temp > 0)
		divisor *= sqrt(temp) ;
	else
		divisor = 0.0 ;

	if (divisor > 0 )
		return (N*sumXY - sumX*sumY)/divisor ;
	else
		return 0.0 ;
}

//************************************************************
//
//                         cvTable
//                         -------
//
//     returns the table filled with the indexes of the splits
//      according to the degree of cross validation
//
//
//************************************************************
void cvTable(marray<int> &splitTable, int NoCases, int cvDegree)
{
	marray<int> scrambledTable(NoCases) ;
	int selected, upper, noElem, alreadyUsed, i, j ;

	for (i=0 ; i < NoCases; i++)
		splitTable[i] = i ;

	// scramble the examples
	upper = NoCases ;
	for (i=0 ; i < NoCases ; i++)
	{
		selected = randBetween(0, upper) ;
		scrambledTable[i] = splitTable[selected] ;
		splitTable[selected] = splitTable[--upper] ;
	}

	// determine how many files define split with one element more than NoCases/NumberOfFiles
	upper = NoCases % cvDegree ;
	noElem = NoCases / cvDegree ;
	for (i=0; i<upper ; i++)
	{
		for (j=0 ; j < NoCases ; j++)
			if (scrambledTable[j] >= i*(noElem+1) && scrambledTable[j] < (i+1)*(noElem+1) )
				splitTable[j] = i ;
	}

	alreadyUsed = upper * (noElem+1) ;
	// splits with NoCases/NumberOfFiles
	for (i=upper; i<cvDegree ; i++)
	{
		for (j=0 ; j < NoCases ; j++)
			if (scrambledTable[j] >= alreadyUsed + (i-upper)*noElem && scrambledTable[j] < alreadyUsed +(i+1-upper)*noElem )
				splitTable[j] = i ;
	}
}


void stratifiedCVtable(marray<int> &splitTable, marray<int> &classTable, int NoCases, int noClasses, int cvDegree) {
	marray<marray<int> > clCase(noClasses+1) ;
	int i, cl ;
	for (cl=1 ; cl <= noClasses ; cl++)
		clCase[cl].create(NoCases) ;
	for (i=0 ; i < NoCases ; i++)
		clCase[classTable[i]].addEnd(i) ;
	int fold = 0, upper, pos ;
	for (cl=1 ; cl <= noClasses ; cl++) {
		upper =  clCase[cl].filled() ;
		for (i=0 ; i < upper ; i++) {
			pos = randBetween(0, clCase[cl].filled()) ;
			splitTable[clCase[cl][pos]] = fold++ ;
			clCase[cl][pos] = clCase[cl][clCase[cl].filled()-1] ;
			clCase[cl].setFilled(clCase[cl].filled()-1) ;
			if (fold >= cvDegree)
				fold = 0 ;
		}
	}
}



//************************************************************
//
//                       timeMeasure
//                       -----------
//
//     measures the current time as precisely as possible, independend on
//   the operating system
//
//
//************************************************************
double timeMeasure(void)
{
#if defined(MICROSOFT)
	return (double)clock() ;
#endif
#if defined(UNIX)
	struct tms timeMes ;
	times(&timeMes) ;
	return (double) (timeMes.tms_utime + timeMes.tms_stime +
			timeMes.tms_cutime + timeMes.tms_cstime) ;
#endif
}


double timeMeasureDiff(double Start, double Finish)
{
	return (Finish-Start)/double(CLOCKS_PER_SEC) ;
}


// ************************************************************
//
//                  getWildcardFileName
//                  -------------------
//
//     checks if file exists and returns first matching filename
//
//
//
// ************************************************************
char* getWildcardFileName(const char *Path, const char *WildcardFileName)
{
	char fullName[MaxPath] ;
	snprintf(fullName, MaxPath, "%s%s" ,Path,WildcardFileName) ;

#if defined(MICROSOFT)
	struct _finddata_t choiceF ;
	intptr_t hFile ;
	if( (hFile = _findfirst(fullName, &choiceF)) == -1L )
		return 0;

	char *FName = new char[strlen(Path)+strlen(choiceF.name)+1] ;
	snprintf(FName, strlen(Path)+strlen(choiceF.name)+1, "%s%s",Path,choiceF.name) ;
	_findclose(hFile) ;
	return FName ;
#endif
#if defined(UNIX)
	glob_t vecP;
	glob(fullName,GLOB_NOSORT,0, &vecP) ;
	char *FName = 0 ;
	if (vecP.gl_pathc >0)
	{
		FName = new char[strlen(vecP.gl_pathv[0])+1] ;
		strcpy (FName,vecP.gl_pathv[0]) ;
	}
	globfree(&vecP) ;
	return FName ;
#endif

}
// get indexes of sampleSize samples from 0 to domainSize-1, used in ReliefF
void randomizedSample(marray<int> &sampleIdx, int sampleSize, int domainSize) {
	int i ;
	if (sampleSize >= domainSize)  {
		for (i=0 ; i < sampleSize ; i++)
			sampleIdx[i] = i % domainSize ;
	} else {
		marray<int> samplePrep(domainSize) ;
		for (i=0 ; i < domainSize ; i++)
			samplePrep[i] = i ;
		int idx, size = domainSize ;
		for (i=0 ; i < sampleSize ; i++) {
			idx =  randBetween(0, size) ;
			sampleIdx[i] = samplePrep[idx] ;
			size -- ;
			samplePrep[idx] = samplePrep[size] ;
		}
	}
}


// ran1 from Numerical Recipes book (linear congruential with bit shuffling)
static long rseed = -1 ;

// exclusive of the endpoint values
double nrran1Between(double From, double To) {
	return From + ran1(&rseed) * (To-From) ;
}

// exclusive of the To value
int nrran1Between(int From, int To) {
	return From + (int)(ran1(&rseed) * (To-From)) ;
}


// seed equal to 0 is replaced by -1 in ran1
void nrran1Seed(long seed) {
	if (seed > 0)
		rseed = -seed ;
	else
		rseed  = seed ;
}


// ******** mrg32k5a from L'Ecuyer99  **********
//######################

double  s10, s11, s12, s13, s14, s20, s21, s22, s23, s24;

#define norm 2.3283163396834613e-10
#define m1   4294949027.0
#define m2   4294934327.0
#define a12     1154721.0
#define a14     1739991.0
#define a15n    1108499.0
#define a21     1776413.0
#define a23      865203.0
#define a25n    1641052.0

double MRG32k5a()
{
	long   k;
	double p1, p2;
	/* Component 1 */
	p1 = a12 * s13 - a15n * s10;
	if (p1 > 0.0) p1 -= a14 * m1;
	p1 += a14 * s11;   k = (long)(p1 / m1);    p1 -= k * m1;
	if (p1 < 0.0) p1 += m1;
	s10 = s11;   s11 = s12;   s12 = s13;  s13 = s14;  s14 = p1;
	/* Component 2 */
	p2 = a21 * s24 - a25n * s20;
	if (p2 > 0.0) p2 -= a23 * m2;
	p2 += a23 * s22;    k  = (long)(p2 / m2);   p2 -= k * m2;
	if (p2 < 0.0) p2 += m2;
	s20 = s21;   s21 = s22;   s22 = s23;  s23 = s24;  s24 = p2;
	/* Combination */
	if (p1 <= p2)  return ((p1 - p2 + m1) * norm);
	else  return ((p1 - p2) * norm);
}
#undef m1
#undef m2

void mrg32k5aSeed(long seed) {
	if (seed < 0)
		seed = - seed ;
	if (seed == 0)
		seed = 2 ;
	long m1 = 4294949027U, m2 = 4294934327U ;
	long sg = seed ;
	s10 = (double)sg ;
	sg = (sg * seed + 1) % m1 ;
	s11 = (double)sg ;
	sg = (sg * seed + 1) % m1 ;
	s12 = (double)sg ;
	sg = (sg * seed + 1) % m1 ;
	s13 = (double)sg ;
	sg = (sg * seed + 1) % m1 ;
	s14 = (double)sg ;

	sg = (sg * seed + 1) % m2 ;
	s20 = (double)sg ;
	sg = (sg * seed + 1) % m2 ;
	s21 = (double)sg ;
	sg = (sg * seed + 1) % m2 ;
	s22 = (double)sg ;
	sg = (sg * seed + 1) % m2 ;
	s23 = (double)sg ;
	sg = (sg * seed + 1) % m2 ;
	s24 = (double)sg ;
}

double mrg32k5aBetween(double From, double To) {
	return From + MRG32k5a() * (To-From) ;
}
int mrg32k5aBetween(int from, int to) {
	return from + int(MRG32k5a()*(to-from)) ;
}

#if defined(R_PORT)
#define unifrand unif_rand
#else
#define unifrand MRG32k5a
#endif

// wrapper around the selected method
double randBetween(double From, double To) {
	return From + unifrand() * (To-From) ;
}
int randBetween(int from, int to) {
	return from + int(unifrand()*(to-from)) ;
}
void randSeed(long seed) {
	mrg32k5aSeed(seed) ;
}

void testRand(int *n, double *x)
{
	int i;
#if defined(R_PORT)
	GetRNGstate();
#endif
	for (i=0; i<*n; i++)
		x[i] = unifrand();
#if defined(R_PORT)
	PutRNGstate();
#endif
}

booleanT haveCachedNormal = mFALSE ;
double cachedNormal ;

double randNormal(double mean, double stddev) {

	if (haveCachedNormal)
	{
		haveCachedNormal = mFALSE;
		return (cachedNormal * stddev + mean );
	}
	else
	{
		for (;;) {
			// Box-Muller transformation returns two N(0,1) distributed numbers
			double u1 = randBetween(0.0, 1.0);
			double u2 = randBetween(0.0, 1.0);
			double v1 = 2 * u1 - 1;
			double v2 = 2 * u2 - 1;
			double w = (v1 * v1) + (v2 * v2);

			if (w <= 1)  {
				double y = sqrt( (-2 * log(w)) / w);
				double x1 = v1 * y;
				double x2 = v2 * y;

				haveCachedNormal = mTRUE;
				cachedNormal = x2;
				return (x1 * stddev + mean);
			}
		}
	}
}


void printLine(FILE *to, const char *what, int times) {
	for (int i=0 ; i < times ; i++)
		fprintf(to, "%s", what) ;
	fprintf(to, "\n") ;
}


// tokenizer which works similarly to strtoken, but does not skips multiple delimiters
// and uses variable idx as an indicator for original string therefore enabling multiple
// simultaneous instances to be executed
char *myToken(char *inStr, int &idx, const char *delimiters) {
	if (idx == -1)
		return 0 ;
	char *token = &(inStr[idx]) ;
	size_t delimIdx = strcspn(token, delimiters) ;
	if (delimIdx >= strlen(token))
		idx = -1 ;
	else {
		token[delimIdx]='\0' ;
		idx += int(delimIdx+1) ;
	}

	return token ;
}
// variant of tokenizer which skips multiple delimiters
char *myTokenMDskip(char *inStr, int &idx, const char *delimiters) {
	char *token ;
	do {
		token = myToken(inStr, idx, delimiters) ;
	} while (token != 0 && strcmp(token,"")==0) ;
	return token ;
}

void tokenizedList(char *buf, mlist<mstring> &names, const char *tokenSeparators) {
	names.destroy() ;
	mstring item ;
	int strIdx = 0 ;
	char *token = myToken(buf, strIdx, tokenSeparators );
	while (token) {
		item.copy(token);
		names.addEnd(item) ;
		token = myToken(buf, strIdx, tokenSeparators );
	}
}

int fscanfUntil(FILE *fin, char *buf, const char delimiter, int maxSize){
	char ch ;
	int ichr, idx = 0;
	do {
		ichr = fgetc(fin) ;
		ch = (char) ichr ;
		if (feof(fin) || ch == delimiter){
			buf[idx]='\0';
			return idx ; // normal exit
		}
		else {
			buf[idx++] = ch ;
		}
	} while (idx < maxSize) ;
	// unexpectedly large number of chars without delimiter
	buf[idx]='\0';
	merror("fscanfUntil","unexpectedly large number of chars without delimiter") ;
	return idx ;
}

// statistics about array: median, 1st quartile, 3rd quartile, lower and upper percentile (e.g., 0.05 and 0.95),
//                         mean, sample deviation, p-value
void statOE(marray<double> &num, int n, marray<double> &stat, double percentile, double value) {
	if (n <= 0) {
		stat.init(0.0) ;
		return ;
	}
	// we assume lower percentile
	if (percentile > 0.5)
		percentile = 1 - percentile ;
	num.setFilled(n) ;
	num.qsortAsc() ;
	// median
	if (n % 2 == 0) // even
		stat[0] = (num[n/2] + num[n/2-1]) / 2.0 ;
	else stat[0] = num[n/2] ;
	// quartiles are computed by the method of Mendenhall, W. and Sincich, T. L. Statistics for Engineering and the Sciences, 4th ed. Prentice-Hall, 1995.
	// 1st quartile
	int L = intRound((n+1)/4.0) ; // rounds up if half way between to integers
	stat[1] = num[L-1]  ;
	// 3rd quartile
	int U = intRoundD(3*(n+1)/4.0) ; // rounds down if half way between to integers
	stat[2] = num[U-1]  ;
	// lower percentile
	stat[3] = num[intRound(percentile*n)]  ;
	// upper percentile
	stat[4] = num[intRoundD((1-percentile)*n)-1]  ;
	// mean and standard deviation
	double avg=0.0, s2=0.0 ;
	for (int i=0 ; i < n ; i++) {
		avg += num[i] ;
		s2 += sqr(num[i]) ;
	}
	avg /= n ;
	stat[5] = avg ; // mean
	stat[6] = sqrt(s2/n - sqr(avg)) ; // standard deviation
	stat[7] = 1.0 - (num.lessEqPlace(value)+1) / double(n+1) ; // p-value
}

// Handling double NA, NaN values

double genNAcont()
{
	union ieee_double u;
#define NA_BIG_END 0x7ff00000     // copied from R's NA
#define NA_LITTLE_END 0x000007a2  // copied from R's NA
	// try LITTLE_ENDIAN first
	u.i.p0 = NA_LITTLE_END ;
	u.i.p1 = NA_BIG_END ;
	if (u.d != u.d) return(u.d);
	// try BIG_ENDIAN
	u.i.p0 = NA_BIG_END ;
	u.i.p1 = NA_LITTLE_END ;
	if (u.d != u.d) return(u.d);
	merror("genNAcont","did not succeed to generate NAcont, NA value processing may fail!!!");
	return(-1e308);
}

#if defined(R_PORT)

int isNaN(double x)
{
	return(R_IsNaN(x));
}

int isNAcont(double x)
{
	return(R_IsNA(x));
}

#else

int isNaN(double x)
{
	return((x != x) && memcmp(&x, &NAcont, sizeof(double)) != 0);
}

int isNAcont(double x)
{
	return(memcmp(&x, &NAcont, sizeof(double)) == 0);
}

#endif

void modelEval(int SetSize, marray<int> &trueClass,
		marray<marray<double> > &probDist, int noClasses, marray<double> &priorProbability,
		mmatrix<double> &CostMatrix, double &Accuracy, double &avgCost,
		double &Inf, double &Auc, mmatrix<int> &PredictionMatrix, double &kappa,
		double &sensitivity, double &specificity, double &brier,
		double &precision, double &Gmean, double &KS, double &TPR, double &FPR) {

	int correct = 0;
	int i, j, c, cMin, cPredicted, cTrue;
	double infi=0.0, Cost = 0.0;
	brier = 0.0;
	double pClPrior, pClObtain, minRisk, cRisk;
	PredictionMatrix.init(0) ;
	PredictionMatrix(0, 0) = SetSize ;
	mmatrix<marray<double> > mm(noClasses+1, noClasses+1); // for auc
	for (i=1; i <= noClasses ; i++)
		for (j=1; j <= noClasses ; j++)
			mm(i,j).create(SetSize) ;

	for (i=0; i<SetSize; i++) {

		// prediction with costs
		// compute conditional risk
		minRisk = DBL_MAX;
		cMin = 0;
		for (cPredicted=1; cPredicted <= noClasses; cPredicted++) {
			cRisk = 0.0;
			for (cTrue=1; cTrue <= noClasses ; cTrue++)
				cRisk += probDist[i][cTrue] * CostMatrix(cTrue, cPredicted) ;
			if (cRisk < minRisk) {
				minRisk = cRisk ;
				cMin = cPredicted ;
			}
		}
		if (trueClass[i] == cMin)
			correct++;
		Cost += CostMatrix(trueClass[i], cMin) ;
		PredictionMatrix(trueClass[i], cMin) ++;

		// compute information score
		pClPrior = priorProbability[trueClass[i]];
		pClPrior = Mmax(epsilon, Mmin(pClPrior, 1.0 - epsilon)) ; // computational correction, if neccessary
		pClObtain = probDist[i][trueClass[i]];
		pClObtain = Mmax(epsilon, Mmin(pClObtain, 1-epsilon)) ; // computational correction, if neccessary
		if (pClObtain >= pClPrior) {
			infi += ( -mlog2(pClPrior) + mlog2(pClObtain) );
		} else {
			infi -= ( -mlog2(1.0 - pClPrior) + mlog2(1.0 - pClObtain) );
		}

		// AUC, M measure, brier
		for (c=1; c<= noClasses; c++) {
			mm(c, trueClass[i]).addEnd(probDist[i][c]) ;
			if (trueClass[i]==c)
				brier += sqr(1.0-probDist[i][c]) ;
			else
				brier += sqr(probDist[i][c]) ;
		}

	} // for all predictions

	// compute rows and columns totals of PredictionMatrix
	for (i=1; i<= noClasses; i++) {
		for (j=1 ; j <= noClasses; j++) {
			PredictionMatrix(i,0) += PredictionMatrix(i,j) ;
			PredictionMatrix(0,j) += PredictionMatrix(i,j) ;
		}
	}
	Accuracy = double(correct)/double(SetSize);
	Inf = infi/double(SetSize);
	avgCost = Cost/double(SetSize);
	sensitivity = specificity = precision = Gmean = KS = FPR = TPR = 0.0 ;
	sortRec tRec;

	if (noClasses == 2) {
		if (PredictionMatrix(1,0) > 0)
			sensitivity = double(PredictionMatrix(1,1))/double(PredictionMatrix(1,0)); // the same as recall
		if (PredictionMatrix(2,0) > 0)
			specificity = double(PredictionMatrix(2,2))/double(PredictionMatrix(2,0));
		if (PredictionMatrix(0,1) > 0)
			precision = double(PredictionMatrix(1,1))/double(PredictionMatrix(0,1));
		if (PredictionMatrix(1,0) > 0 && PredictionMatrix(2,0) > 0)
			Gmean = sqrt(double(PredictionMatrix(1,1))/double(PredictionMatrix(1,0)) * double(PredictionMatrix(2,2))/double(PredictionMatrix(2,0)));

		// Kolmogorov-Smirnov statistics (D.J.Hand, 2005, Journal of Operational Research Society)
		// let class 1 be the positive class, and class 2 the negative class
		double P = PredictionMatrix(1,0) ;
		double N = PredictionMatrix(2,0) ;
		marray<sortRec> prob(SetSize) ;
		for (i=0; i<SetSize; i++) {
			tRec.key = probDist[i][1]; // probability of positive class
			tRec.value = i ;
			prob.addEnd(tRec) ;
		}
		prob.qsortDsc() ;

		double tKS  ;
		int	FP = 0, TP = 0 ;
		double probPrev = -1.0 ;
		for (int k=0; k < prob.filled() ; k++) {
			if (prob[k].key != probPrev) {
				tKS = fabs(TP/P - FP/N) ;
				if (tKS > KS){
					FPR = FP/N ;
					TPR = TP/P ;
					KS = tKS ;
				}
				probPrev = prob[k].key ;
			}
			if (trueClass[prob[k].value] == 1) // positive
				++ TP ;
			else // k is a negative example
				++ FP ;
		}
		// for last position
		tKS = fabs(TP/P - FP/N) ;
		if (tKS > KS){
			FPR = FP/N ;
			TPR = TP/P ;
			KS = tKS ;
		}
	}
	brier /= double(SetSize);

	// AUC, AUC-Multiclass measure (Hand & Till, 2001)
	Auc = 0.0;
	marray<sortRec> sa(SetSize*2);
	int k, k_lo=0, k_hi, n0, n1, noPairs=0;
	double s0;
	for (i=1; i<= noClasses; i++) {
		n0 = mm(i,i).filled() ;
		if (n0==0)
			continue;
		for (j=1; j<=noClasses ; j++) {
			if (i==j)
				continue;
			n1 = mm(i,j).filled() ;
			if (n1==0)
				continue;
			sa.clear() ;
			for (k=0; k < n0 ; k++) {
				tRec.key = mm(i,i)[k];
				tRec.value = i ;
				sa.addEnd(tRec) ;
			}
			for (k=0; k < n1 ; k++) {
				tRec.key = mm(i,j)[k];
				tRec.value = j ;
				sa.addEnd(tRec) ;
			}
			sa.qsortAsc() ;
			s0 = 0;
			k_hi = -1;
			for (k=0; k < sa.filled() ; k++) {
				if (k > k_hi) {
					k_lo = k;
					k_hi = k;
					while (k_hi+1 < sa.filled() && sa[k_hi+1].key == sa[k_lo].key) {
						k_hi++;
					}
				}
				if (sa[k].value == i)
					s0 += (k_lo + k_hi)/2.0;
			}
			Auc += double(s0 - double(n0)*(n0-1)/2.0)/double(n0)/double(n1);
			noPairs++;
		}
	}
	if (noPairs >0) {
		Auc /= double(noPairs); // double(noClasses*(noClasses-1)) ;
	}

	// Cohen's kappa k = (accuracy - p_c) / (1 - p_c)
	kappa = 0.0 ;
	// store in kappa first the probability of being correct by chance p_c
	for (i=1 ; i <= noClasses ; i++)
		kappa += PredictionMatrix(i,0)*PredictionMatrix(0,i);
	kappa /= sqr((double)SetSize) ; // probability
	if (kappa > 1.0 - epsilon)
		kappa = 1.0 ;
	else
		kappa = (Accuracy - kappa) / (1.0 - kappa) ;
}

void modelEvalReg(int SetSize, marray<double> &truePrediction,
		marray<double> &prediction, double avgPredicted, double &SE, double &RSE,double &AE, double &RAE)
{
	int i ;
	double residium, Rresidium ;

	SE = RSE = AE = RAE = 0.0 ;


	for (i=0; i < SetSize ; i++) {
		residium = prediction[i] - truePrediction[i] ;
		SE += sqr( residium ) ;
		Rresidium = truePrediction[i] - avgPredicted;
		RSE += sqr( Rresidium ) ;
		AE += fabs( residium ) ;
		RAE += fabs( Rresidium ) ;
	}
	if (RSE > 0.0 && RAE > 0.0) {
		RSE = SE/RSE ;
		SE = sqrt(SE/double(SetSize)) ;
		RAE = AE/RAE ;
		AE = AE/double(SetSize) ;
	}
	else
	{
		//merror("regressionTree::test", "all values are the same, learning makes no sense.") ;
		RSE = RAE = 0.0 ;
		SE = sqrt(SE/double(SetSize)) ;
		AE = AE/double(SetSize) ;
	}
}

void costMxFromR(int noClasses, marray<double> &costs, mmatrix<double> &CostMatrix) {
	int i, j;

	CostMatrix.create(noClasses+1, noClasses+1, 0.0) ;
	for (i=1; i <= noClasses; i++)
		for (j=1; j <= noClasses; j++)
			CostMatrix(i, j) = costs[i-1 +(j-1)*noClasses];
}

