
//qtlmt.h

#include "R_ext/Error.h"
#include "R_ext/Memory.h"
#include "R_ext/Print.h"
#ifdef ENABLE_NLS
#include "libintl.h"
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif
#include <R_ext/Rdynload.h> //R_CMethodDef
//#include <Rdefines.h>

#include "iostream"
#include "fstream"
#include "iomanip"
#include "string.h"
#include "stdlib.h"
#include "time.h"
#include "math.h"
#include "limits"
#include "cmath"
#include "stdio.h"
using namespace std;

//double abs(double x);
void arr_t(double** arr,int m, int n,double** result);
void chol(double **A, int n, double *p);
void cholsl(double **A, int n, double b[], double x[]);
double dnorm(double x, double mean, double sd);
void fchar(char *path,char *file,int num,char *type,char *buff);
double det(double** A, int n);
void ginv(double** A, int m, int n, double** ginvA);
void inv(double** A, int n, double** invA);
double inv_det(double** A, int n, double** invA);
void i_to_a(int i,char buff[],int base=10);
void lubksb(double **a, int n, int *indx, double b[]);
void lud(double **a, int n, int *indx, double *d);
void runif(double* x,int n,long *seed=0);
void solve(double **A, int n, double b[], double x[]);
void solve(double **A, int m, int n, double b[], double x[]);
void svd(double **a, int m, int n, double* w, double **v);

template <class T> 
	void arr_print(T* arr,int m,int n,int width=12);
template <class T> 
	void arr_print(T** arr,int m,int n,int width=12);
template <class T>
	void arr_copy(T* arr1,int n,T* arr);
template <class T>
	void arr_copy(T** arr1,int m,int n,T** arr);
template <class T>
	void arr_prod(T** arr1,T** arr2, int m,int k,int n,T** arr);
template <class T>
	void arr_prod(T** arr1,T* arr2, int m,int n,T* arr);
template <class T>
	void arr_prod(T** arr1,T** arr2, int n,T** arr);
template <class T>
	void cumsum(T* x,int n,T* arr);
template <class T>
	void largest(T *x,int n,int k,T *arr,int *which=NULL);
template <class T>
	T max(T* x,int n,int* which=NULL);
template <class T>
	double mean(T* x,int n);
template <class T>
	T min(T* x,int n,int* which=NULL);
template <class T>
	void order(T* x, int n, int* arr, bool increasing=true);
template <class T>
	double quantile(T* x, int n, double p);
template <class T>
	void rep(T x,int n,T* arr);
template <class T>
	double round(T x,int n=0);
template <class T>
	double sd(T* x,int n);
template <class T>
	void sample(T* x,int n,T* arr,int k,long int *seed=0,bool replace=false);
template <class T>
	void smallest(T *x,int n,int k,T *arr,int *choice=NULL);
template <class T>
	void sort(T* x,int n,T* arr, bool increasing=true);
template <class T>
	void subset(T* x,bool* choice,int n,T* arr);
template <class T>
	T sum(T* x,int n);
template <class T>
	double var(T* x,int n);
template <class T>
	void read_T(T* x,char infile[]);
template <class T>
	void write_T(int n,int ncol,T* x,char outfile[]);

///////////////////////////////////////////////////////////////////////////////////////////
double haldane(double c);
double haldane_inv(double d);
double rate_ac(double r_ab, double r_bc);
double rate_bc(double r_ab,double r_ac);
double getprob(int m1,int m2,double r1,double r);
double getp_1(int mL,int mR,double d,double d1);
void getp(int** mdat,int nrow,int ncol,int mid,double d1,double d,double* p);
void itom(int *i,int i_len,int *nmark,int nm_len,int* m);
void mtoi(int* m,int m_len,int* nmark,int nm_len,int* i);
template <class T> void read_pos(T& pos,char infile[]); //T: mpos or qtlpos
template <class T> void write_pos(T& pos,char outfile[]); //T: mpos or qtlpos

///////////////////////////////////////////////////////////////////////////////////////////
double sureEst(double** y,int n,int p,double** x,int m,int* nqs,int* qs,
	double* b,double** sigma,int ini_sigma=0,int iter=100,double tol=1e-8);
void sureStep(double** y,int n,int p,double** x, int m,int* nlower,int* lower,
	int* nupper,int* upper,double k,int direction,int** vin,double* record,
	int max_terms=100,int steps=1000,int iter=100, double tol=1e-8);

///////////////////////////////////////////////////////////////////////////////////////////
double mimEst(double* y,double** P,double** G,double** W,int n,int m,int k,int l,
  double* a,double* b,double& sigma,int init=1,int iter=2500,double tol=1e-8);
double mtcmimEst(double** y,int n,int p,double** P,int np,
	double** G,int* ngs,int* gs,double** W,int* nws,int* ws,
	double* a,double* b,double** S,int init=1,int iter=2500,double tol=1e-8);
void fP(int** A,int nP,int nQ,int** mdat,int n,int nm,
	double** mpos,int* dists_ch,int* dists_mid,double* dists_d,int* mid,int nmid,
	double**P,int pp=1);

