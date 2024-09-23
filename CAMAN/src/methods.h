#ifndef _DLL_H_
#define _DLL_H_
//#include <fstream.h>
//#include <math.h>
#include <errno.h>
//#include <stdlib.h>
#include <ctype.h>
#include <vector>

//#include <iomanip.h>

# define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>




#define one 1.00
#define rtwopi 2.506628275
//#define PI 3.141592654
#define half 0.5

extern "C" {
const int  nn = 10000;       // Max number of observations
const int kk = 100;          // Max. number of subpopulations
const int  nnlong = 250000;       // Max number of observations if LONGMODE

typedef int (*fcmp) (const void *, const void *);
int cmpgle( double *arg1, double *arg2 );
double dfchi  (double chi,int f);
double phi (double x);
double normal(double,double,double);
double binomial (double  k,double  n, double p);
double glngam (double x);
double poisson (double x,double lambda);

void caman_boot(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx, int * STARTK, int * DENS, int * NUMK, double * LL, double * P, double * T, double * LIMIT, double * ACC, int * NUMSTEP, double * COMP_VAR, int * NUMBOOT, int * KBOOT, double * LL_k1, int * IS_META);
void caman_C(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx, int * STARTK, int * DENS, int * NUMK, double * LL, double * P, double * T, double * LIMIT, double * ACC, int * NUMSTEP, double * COMP_VAR, double * VEM_DETAILS, double * EM_DETAILS, int * IS_META);
void mixalg_sub(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx, int * STARTK, int * DENS, int * NUMK, double * LL, double * P, double * T, double * LIMIT, double * ACC, int * NUMSTEP, double * COMP_VAR, int * ADD_INFO, int * IS_META, double * GRADIENT);

class MixMod
{
public:
double *p,*t,**x,*w,**xf,*grad,*s1,*ht, *y_grid,ll,maxder, compvar, *vem_details, *em_details, limit, acc;
int n,k,dens,type,datatype,startk, numstep, VEMStepsDone, maxstep;
bool add, longmode, ismeta;
MixMod(int * STARTK, int * DENS, int * NUMSTEP, int * NROWx);
~MixMod();
void Grid();
void CalcMat();
void Gradient();
void Compute(int * NUMK, double * LL, double * P, double * T, double * COMP_VAR);
void Init(double * DATa, double * DATb, double * DATc, double * DATd, int * NROWx);
int maxderiv(double& maxder);
int minderiv();
void vem();
double stepsize();
int Update();
double g(double s1);
double likelihood ();
double loglike1(double al,double *dd);
void EM(int nstep);
void EMCG();
void gradcg(std::vector<double> gradq,double *p1, double *t);
void gradcg(std::vector<double> gradq,std::vector<double> p1, std::vector<double>t1);
double stepcg(std::vector<double>  gradq,double *d);
double stepjj(std::vector<double> gradq,std::vector<double> d);
void getchange(std::vector<double> cx);
void initchange (std::vector<double> cx);
int Combine();
short klasse();
void Classify();
double LRS();
};

}

#endif /* _DLL_H_ */
