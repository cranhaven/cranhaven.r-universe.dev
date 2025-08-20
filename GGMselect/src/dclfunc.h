/* ---------------------------------------------------------------
  GGMselect R package
  Copyright INRA 2017
  INRA, UR1404, Research Unit MaIAGE
  F78352 Jouy-en-Josas, France.
 
  URL: http://genome.jouy.inra.fr/logiciels/GGMselect
-------------------------------------------------------------- */

#ifndef DCLFUNC_H
#define DCLFUNC_H
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

/* loop.c */

void GGMModC01(int *n, int *p, int *nrowphi, double *rho,
	       double *phi);

void GGMloopAND(int *n, int *p, int *lK, int *nrowGrGlob, int *ncolGrGlob,
	       int *GrGlob, int *Dmax,
	       double *minvp, double *X,  double *sumX2, double *pen,
	       int *ncolGr, int *ncolGraph,
	       int *NVoisGraph, int *NVoisGr,
	       int *Graph, int *Gr, int *Dmaxmax,
	       double *scr,int *iwork, double *work, 
	       double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
	       double *Pr,
		double *critmin, int *Neighb);

void GGMloopEWOR(int *n, int *p, int *lK, int *nrowGrGlob, int *ncolGrGlob,
	       int *GrGlob, int *Dmax,
	       double *minvp, double *X,  double *sumX2, double *pen,
	       int *ncolGr, int *ncolGraph,
	       int *NVoisGraph, int *NVoisGr,
	       int *Graph, int *Gr, int *Dmaxmax,
	       double *scr,int *iwork, double *work, 
	       double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
	       double *Pr,
		 double *critmin, int *Neighb);

void GGMloopC01(int *n, int *p, int *lK, int *nrowGrGlob, int *ncolGrGlob,
	       int *GrGlob, int *Dmax,
	       double *minvp, double *X,  double *sumX2, double *pen,
	       int *ncolGraph, int *NVoisGraph,
	       int *Graph,  int *Dmaxmax,
	       double *scr, int *iwork, double *work, 
	       double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
	       double *Pr,
		double *critmin, int *Neighb) ;

void GGMSCRa(int *ia, int *n, int *p,
	         double *X, double *minvp,
		 int *NVois, double *sumX2,
	     int *Graph,double *scr,
	     int *iwork, double *work, 
	     double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
	     double *Pr);

void GGMGrMin( int *n, int *p, int *lK, int *ncolGraph,
	       int *Dmaxmax, double *scr, double *pen, int *Graph,
	       int *NVoisGraph, 
	       double *critmin, int *Neighb, int *err);


/* critQE.c */
void GGMloopGrSymQE(int *Mod, int *d, int *nrowMod, int *ncolMod, 
		    int *nrowModOut, int *ncolModOut,
		    int *lgind1, int *ind1, int *ModOut);
SEXP GGMscrgcritQE(SEXP list);
SEXP GGMcritminQE(SEXP listarg);


/* scr.c */
SEXP GGMcalcSCRQE( SEXP gNormX, SEXP gpen, SEXP gligpen, SEXP glK,
		   SEXP gSCRminSCR, SEXP gSCRminmod,
		   SEXP list );

/* QESW.c */

SEXP GGMbcSW(SEXP xMatMax, SEXP xMatMin, 
	     SEXP xp,  SEXP xImax,
	     SEXP xListOut,
	     SEXP z, SEXP xindF, SEXP xindB);

/* boucle.f */
void F77_SUB(bouclet)(int *p, int *k, int *veutlw,
		      double *a, double *b,  double *c, double *d, double *h, 
		      double *Xy, double *XX, double *alea,
		      double *L, double *LEW);


#endif
