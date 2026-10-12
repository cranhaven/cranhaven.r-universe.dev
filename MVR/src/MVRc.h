#include <R.h>
#include <Rmath.h>
#include <stdlib.h>
#include <time.h>
#include <cmath>

extern "C" {

//=======//
// MVR.h //
//=======//
#ifndef _MVR_H_
#define _MVR_H_

#define __MVR_C_R__
#ifdef __MVR_C_R__
#else
typedef bool Rboolean;
#define TRUE true
#define FALSE false
#define R_PosInf 1.79769e308
#endif

#endif

//============//
// MVR_stat.h //
//============//
#ifndef _MVR_STAT_H_
#define _MVR_STAT_H_

double sum(double* a, int n);
double sd(double* a, double mean, int n);
#endif

//============//
// MVR_rand.h //
//============//
#ifndef _MVR_RAND_H_
#define _MVR_RAND_H_

//call MVR_rand_init() for once before calling any functions/macros in this library
void MVR_rand_init();
void MVR_rand_end();

/*
RANDI() generates random int number in [0, RANDI_MAX], 30 bits
For 1,000,000 generated numbers, 30 bits has calculated
propensity (probability of 1) from 0.498916 to 0.500959,
and average of 0.5000047333. These values agrees very well
with theoretical values.  Further tests for RANDF()
and RANDD() based on RANDI() confirmed the uniformity test for RANDI()

random float numbers from uniform[0,1.0] distribution
RANDF() has precision of 1e-9
RANDD() has 1e-16 (supposedly 1e-18, but limited by double precision)

uniformity test on generated random numbers by RANDD()
for 1000 sets of data with size of 10,000
Kolmogorov-Smirnov test rejects null hypothesis
for 50 of them at alpha = 0.05
the actual type 1 error of 0.0500 agrees with theoretical well
visually check the histograms for randomly selected 10 sets
all of them seem to follow normality ver well
therefore, the functions passed the normality test :)

RANDF() has lower precision, but still passed the same test 
with type 1 error of 0.0580, slightly bigger than theoretical of 0.05

RANDD() is preferred

Hua XU, PhD
June, 2011
*/

#define RANDI_MAX 0x3fffffff //or 1073741823, 30 bits
//#define RANDI() ((rand()&0x7fff) | ((rand()&0x7fff) << 15))
int randi();

#define RANDF_CONST 1.073741823e9
//#define RANDF() (double(RANDI())/RANDF_CONST)
//double randf();

#define RANDD_CONST1 1.152921504606846975e18
#define RANDD_CONST2 1.073741823999999999e9
//#define RANDD() (double(RANDI())/RANDD_CONST1 + double(RANDI())/RANDD_CONST2)
double randd();

void rand_unif(double* data, int n);

/*
rand_norm_std(data, n)
random numbers from a normal(0,1) distr
From http://www.taygeta.com/random/gaussian.html
Algorithm by Dr. Everett (Skip) Carter, Jr.

Benchmark:
To generate 200 million rand norm numbers
for precision of 1e-9, it takes 27 sec
for precision of 1e-16, it takes 37 sec
R built-in function takes 30 sec
on a computer with Win 7 64bit, Intel(R) Xeon(R) 2.8GHz CPU and 4GB RAM

Normality test on generated random numbers
for 1000 sets of data with size of 10,000
Kolmogorov-Smirnov test rejects null hypothesis
for 48 of them at alpha = 0.05
the actual type 1 error of 0.0480 agrees with theoretical well
visually check the histograms, qqplots for randomly selected 10 sets
all of them seem to follow normality ver well
therefore, the functions passed the normality test :)

rand_norm(data, n, mean, s) passed the same test with type 1 error of 0.0410

Hua XU, PhD
June, 2011
*/

//n must be even for rand_norm and rand_norm_std, OTHERWISE MEMORY OVERFLOWS
void rand_norm_std(double* data, int n);
void rand_norm(double* data, int n, double mean, double s);

/*
Random sampling rows from a matrix
the matrix is mimicked by an array data of size nr x nc as follows:
[0,0] [1,0] [2,0] ... [nr-1,0]
[0,1] ... [nr-1,1]
...
[0,nc-1] ... [nr-1,nc-1]
*/

void rand_spl_row(double* data, int nr, int nc, double* spl, int s, int* pool);
void rand_spl_row2(double* data, int nr, int nc, double* spl, int s);
#endif

//==============//
// MVR_kmeans.h //
//==============//
#ifndef _MVR_KMEANS_H_
#define _MVR_KMEANS_H_

/*
More algorithms can be added
*/

Rboolean MVR_kmeans_MacQueen(double *x, double *cen,
                             int *cl, int *nc, double *wss,
                             int n, int p, int k,
                             int maxiter);

#endif

//===========//
// MVR_sub.h //
//===========//
#ifndef _MVR_SUB_H_
#define _MVR_SUB_H_

void MVR_km_clustering(double* x,
                       double* x_unq,
                       
                       double* centers,
                       int* cl,//vec of m
                       int* nc,//vec of k
                       double* wss,//vec of k
                       double* tot_wss,
                       int* perror,
                       
                       int* pm,
                       int* pmm,
                       int* pp,
                       int* pk,
                       int* pnstart,
                       int* pmaxiter);

void MVR_withinsumsq(int* pn,
                     int* pp,
                     int* pk,
                     int* pB,
                     double* lWk_bo,
                     int* pnstart,
                     int* pmaxiter,
                     int* perror
                     );
#endif
}
