#include <time.h>
#include <stdlib.h>
#include <limits.h>
#include <R_ext/PrtUtil.h>

double Pi(void);
void progress(int,int*, int);
/*double alea ();*/
void freeintvec (int *);
void freetab (double **);
void freevec (double *);
void taballoc (double ***,int,int);
void tabintalloc (int ***,int,int);
void freeinttab (int **);
void vecalloc (double **vec, int n);
void vecintalloc (int **vec, int n);
double bacos(double a);
void decalVal(double *,int,double);
void decalRect(int,double *,double *,double *,double *,double *,double *);
void decalCirc(int,double *,double *,double *,double *,double);
void decalRectTri(int,double *,double *,double *,double *,double *,double *,
	int,double *,double *,double *,double *,double *,double *);
void decalCircTri(int,double *,double *,double *,double *,double,
	int,double *,double *,double *,double *,double *,double *);
void decalRect2(int,double *,double *,int,double *,double *,double *,double *,double *,double *);
void decalCirc2(int,double *,double *,int,double *,double *,double *,double *,double);
void decalRectTri2(int,double *,double *,int,double *,double *,double *,double *,double *,double *,
	int,double *,double *,double *,double *,double *,double *);
void decalCircTri2(int,double *,double *,int,double *,double *,double *,double *,double,int,
	double *,double *,double *,double *,double *,double *);
void decalSample(int,double *,double *,double,double);
//void decalPoly(int ,double *, double *,double *, double *, double *, double *,int ,double *, double *);
double** taballoca(int,int *);
void complete_tab(int,double **,double **,int *,int *,int *,double *,double *);
extern void pnpoly(double *, double *, double *, double *, int *, int *, double *, double *, double *, double *, double *);
