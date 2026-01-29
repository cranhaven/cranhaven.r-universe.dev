
/* graphpcor_utils.h
 *
 * Copyright (C) 2023 Elias T Krainski
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * The author's contact information:
 *
 *        Elias T Krainski
 *        CEMSE Division
 *        King Abdullah University of Science and Technology
 *        Thuwal 23955-6900, Saudi Arabia
 */


#include <stdio.h>
#include "graphpcor.h"

// element location in a matrix
#define ij2k(_n, _i, _j) ((_n)-1)*(_j) + (_i)

// element location of a lower triangle matrix (with diagonal)
#define nij2Lk(_n, _i, _j)				       \
	if(1) {						       \
		k = ( _i) + (_n)*(_j);			       \
		if((_j)==1) k -= 1;			       \
		if((_j)>1)				       \
			k -= ((_j)*(_j+1))/2;		       \
	}						       \

// print elements of a matrix
#define printMat(_M, _nr, _nc, _msg)					\
	if(1) {								\
		int _i, _j;						\
		printf("%s (%d x %d)\n", _msg, _nr, _nc);		\
		for(_i=0; _i<_nr; _i++) {				\
			for(_j=0; _j<_nc; _j++) {			\
				printf("%5.4f ", (_M)[(_nc) * _i + _j]); \
			}						\
			printf("\n");					\
		}							\
		printf("\n");						\
	}								\

// print elemnts of a (ONLY) upper triangle matrix (with diagonal)
#define printLU(_M, _nr, _nc, _msg)                            \
	if(1) {							     \
		int _i, _j, _k=0;				     \
		printf("%s (%d x %d)\n", _msg, _nr, _nc);	     \
		for(_i=0; _i<_nr; _i++) {			     \
			for(_j=_i; _j<_nc; _j++) {		     \
				printf("%5.4f ", (_M)[_k++]);	     \
			}					     \
			printf("\n");				     \
		}						     \
		printf("\n");					     \
	}							     \

// print elemnts of (ONLY) a lower triangle matrix (with diagonal)
#define printL(_M, _nr, _nc, _msg)                             \
	if(1) {						       \
		int _i, _j, _k=0;			       \
		printf("%s (%d x %d)\n", _msg, _nr, _nc);      \
		for(_i=0; _i<_nr; _i++) {		       \
			_k = _i;				      \
			for(_j=0; _j<=_i; _j++) {			\
				printf("%5.4f ", (_M)[_k]);		\
				_k += (_nr-_j-1);			\
			}						\
			printf("\n");					\
		}							\
		printf("\n");						\
	}								\


double cov2kld(int n, double *C0, double *C1);
double pclogsigma(double lsigma, double lam);
double pcmultivar(int m, double param, double *theta0,
                  double *halfI, double *ldhI, double *theta);
void L2Cupper(int n, double *ll, double *cc);
void correlation_parent_children(int np, int N, int niiv, int *iiv, int *jjv, int *ipar, int *itop, double *sch, double *v2, double *CC);
void cov2cor(int n, double *cc);
void covariance_parent_children(int np, int N, int niiv, int *iiv, int *jjv, int *ipar, int *itop, double *sch, double *v2, double *CC);
void dl2Qu(int n, double *d, double *l, double *qu);
void dl2fullQ(int n, double *d, double *l, double *q);
void exchangeableU(int n, double r, double *cc);
void fillL(int *d, int *m, int *ii, int *jj, double *x);
void l2L(int n, double *l, double *L);
void cpcCholesky(int *N, double *theta,
                 double *L, double *ldR, double *ldJ);
void theta2Qcorrel(int n, int std, double *theta, double *Qu);
void theta2gamma2Lcorr(int n, double *hldet, double *theta, double *L);
void theta2precision(int n, double *l, double *Q);
void theta_parent_children_kldh(int np, int N, int niiv, int *iiv, int *jjv, int *ipar, int *itop, double *sch, double *theta, double hs,
				double *kld, double *kldd);
