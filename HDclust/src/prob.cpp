/*==========================================================================================*/
/*                                                                                          */
/* Copyright (C) [Dec 2015]-[April 2017] Jia Li, Department of Statistics,                  */
/* The Pennsylvania State University, USA - All Rights Reserved                             */
/*                                                                                          */
/* Unauthorized copying of this file, via any medium is strictly prohibited                 */
/*                                                                                          */
/* Proprietary and CONFIDENTIAL                                                             */
/*                                                                                          */
/* NOTICE: All information contained herein is, and remains the property of The             */
/* Pennsylvania State University. The intellectual and technical concepts                   */
/* contained herein are proprietary to The Pennsylvania State University and may            */
/* be covered by U.S. and Foreign Patents, patents in process, and are protected            */
/* by trade secret or copyright law. Dissemination of this information or                   */
/* reproduction of this material is strictly forbidden unless prior written                 */
/* permission is obtained from Jia Li at The Pennsylvania State University. If              */
/* you obtained this code from other sources, please write to Jia Li.                       */
/*                                                                                          */
/*                                                                                          */
/* The software is a part of the package for                                                */
/* Clustering with Hidden Markov Models on Variable Blocks                                  */
/*                                                                                          */
/* Written by Jia Li <jiali@stat.psu.edu>, April 7, 2017                                    */ 
/*                                                                                          */
/*==========================================================================================*/

#include <math.h>
#include <stdio.h>
#include "hmm.h"
#include <Rcpp.h>
#define Pi 3.141592653589793
#define LOG_2_PI 1.83787706640935

int newgauss(GaussModel *md, int dim, int exist)
{
  md->dim=dim;
  md->exist=exist;

  md->mean=(double *)R_Calloc((size_t)dim,double);
  matrix_2d_double(&(md->sigma),dim,dim);
  matrix_2d_double(&(md->sigma_inv), dim, dim);

  return 0;
}

int cpgauss(GaussModel *md1, GaussModel *md2)
{
  int i,j;

  md2->dim=md1->dim;
  md2->exist=md1->exist;
  md2->cls=md1->cls;
  md2->sigma_det_log=md1->sigma_det_log;

  for (i=0;i<md1->dim;i++)
    md2->mean[i]=md1->mean[i];
  
  for (i=0;i<md1->dim;i++)
    for (j=0;j<md1->dim;j++) {
      md2->sigma[i][j]=md1->sigma[i][j];
      md2->sigma_inv[i][j]=md1->sigma_inv[i][j];
    }
    
  return 0;
}


void newgmm(GmmModel *md, int dim, int numst)
{
  int i;
  int exist=1;

  md->dim=dim;
  md->numst=numst;

  md->stpdf=(GaussModel **)R_Calloc((size_t)numst, GaussModel *);
  for (i=0; i<numst; i++) {
    md->stpdf[i]=(GaussModel *)R_Calloc(1,GaussModel);
    newgauss(md->stpdf[i], dim, exist);
  }
  
  md->p=(double *)R_Calloc((size_t)numst, double);
}


void freegmm(GmmModel **md_pt)
{
  int i;
  int numst;
  GmmModel *md;

  md= *md_pt;
  numst=md->numst;

  for (i=0; i<numst; i++) {
    R_Free(md->stpdf[i]->mean);
    free_matrix_2d_double(&(md->stpdf[i]->sigma), md->dim);
    free_matrix_2d_double(&(md->stpdf[i]->sigma_inv), md->dim);
    R_Free(md->stpdf[i]);
  }
  R_Free(md->stpdf);

  R_Free(md->p);

  R_Free(md);
  *md_pt=NULL;
}


void hmm2gmm(HmmModel *md, GmmModel *md2)
{
  int i;

  md2->dim=md->dim;
  md2->numst=md->numst;

  for (i=0; i<md->numst; i++) {
    cpgauss(md->stpdf[i], md2->stpdf[i]);
    md2->p[i]=md->a00[i];
  }
}

void newhmm(HmmModel *md, int dim, int numst, int prenumst)
{
  int i;
  int exist=1;
  

  md->dim=dim;
  md->numst=numst;
  md->prenumst=prenumst;

  md->stpdf=(GaussModel **)R_Calloc((size_t)numst, GaussModel *);
  for (i=0; i<numst; i++) {
    md->stpdf[i]=(GaussModel *)R_Calloc(1,GaussModel);
    newgauss(md->stpdf[i], dim, exist);
  }
  
  matrix_2d_double(&(md->a), prenumst,numst);
  md->a00=(double *)R_Calloc((size_t)numst, double);
}


void freehmm(HmmModel **md_pt)
{
  int i;
  int numst,prenumst;
  HmmModel *md;

  md= *md_pt;
  numst=md->numst;
  prenumst=md->prenumst;

  for (i=0; i<numst; i++) {
    R_Free(md->stpdf[i]->mean);
    free_matrix_2d_double(&(md->stpdf[i]->sigma), md->dim);
    free_matrix_2d_double(&(md->stpdf[i]->sigma_inv), md->dim);
    R_Free(md->stpdf[i]);
  }
  R_Free(md->stpdf);

  R_Free(md->a00);
  free_matrix_2d_double(&md->a, prenumst);

  R_Free(md);
  *md_pt=NULL;
}


void newccm(CondChain *md, int nb, int *bdim, int **var, int *numst)
{
  int i,j,m;
  
  for (i=0,m=0;i<nb;i++) m+=bdim[i];
  md->nb=nb; md->dim=m;

  md->bdim=(int *)R_Calloc((size_t)nb,int);
  md->cbdim=(int *)R_Calloc((size_t)nb,int);
  md->numst=(int *)R_Calloc((size_t)nb,int);
  md->cnumst=(int *)R_Calloc((size_t)nb,int);
  md->var=(int **)R_Calloc((size_t)nb,int *);
  for (i=0;i<nb;i++)
    md->var[i]=(int *)R_Calloc((size_t)bdim[i],int);
  md->mds=(HmmModel **)R_Calloc((size_t)nb,HmmModel *);
  for (i=0;i<nb;i++) 
    md->mds[i]=(HmmModel *)R_Calloc(1,HmmModel);

  md->cbdim[0]=md->cnumst[0]=0;
  for (i=0,md->maxnumst=0;i<nb;i++) {
    md->bdim[i]=bdim[i];
    md->numst[i]=numst[i];
    if (numst[i]>md->maxnumst) md->maxnumst=numst[i];
    if (i<nb-1) {
      md->cbdim[i+1]=md->cbdim[i]+bdim[i];
      md->cnumst[i+1]=md->cnumst[i]+numst[i];
    }
    for (j=0;j<bdim[i];j++) md->var[i][j]=var[i][j];

    if (i>0)
      newhmm(md->mds[i],bdim[i],numst[i],numst[i-1]);
    else 
      newhmm(md->mds[i],bdim[i],numst[i],1);
  }
}

void freeccm(CondChain **md_pt)
{
  int i,nb;
  CondChain *md;

  md=*md_pt;
  nb=md->nb;

  R_Free(md->bdim); R_Free(md->cbdim); 
  R_Free(md->numst); R_Free(md->cnumst);
  for (i=0;i<nb;i++)
    R_Free(md->var[i]);
  R_Free(md->var);
  for (i=0;i<nb;i++) 
    freehmm(md->mds+i);
  R_Free(md->mds);
  R_Free(md);
  *md_pt=NULL;
}

void cphmm(HmmModel *md1, HmmModel *md2)
{
  int i,j;
  int numst, prenumst, dim;

  md2->dim=dim=md1->dim;
  md2->numst=numst=md1->numst;
  md2->prenumst=prenumst=md1->prenumst;

  for (i=0; i<numst; i++) {
    cpgauss(md1->stpdf[i], md2->stpdf[i]);
  }

  for (i=0;i<numst;i++) md2->a00[i]=md1->a00[i];
  for (i=0;i<prenumst;i++)
    for (j=0;j<numst;j++)
      md2->a[i][j]=md1->a[i][j];
}



double gauss_pdf_log(double *ft, GaussModel *gm)
{
  double res, tpdb, tpdb2, *db_array, *dif, *ptrdb1, *ptrdb2, *ptrdb3;
  int i,j,m;
  double *ptrft;

  if (!vector_double(&db_array, gm->dim))
	Rcpp::stop("Couldn't allocate memory in vector_double!\n");
    
    //exit(1);

  if (!vector_double(&dif, gm->dim))
	Rcpp::stop("Couldn't allocate memory in vector_double!\n");
    
    //exit(1);

  m=gm->dim;
  ptrdb1 = dif;
  ptrft = ft;
  ptrdb3 = gm->mean;
  for (i=0; i<m; i++) {
    *(ptrdb1++) = (*ptrft)-(*ptrdb3);
    ptrft++;
    ptrdb3++;
  }

  if (DIAGCOV==1) {
    tpdb2=0.0;
    for (i=0;i<m;i++) tpdb2+= dif[i]*dif[i]*gm->sigma_inv[i][i];
  }
  else {
    ptrdb1 = db_array;
    for (i=0; i<m; i++) {
      *ptrdb1 = 0.0;
      ptrdb2 = gm->sigma_inv[i];
      ptrdb3 = dif;
      for (j=0; j<m; j++) {
	(*ptrdb1) += (*ptrdb2)*(*ptrdb3);
	ptrdb2++;
	ptrdb3++;
      }
      ptrdb1++;
    }
    
    tpdb2 = 0.0;
    ptrdb1 = db_array;
    ptrdb2 = dif;
    for (i=0; i<m; i++)
      {
	tpdb2 += (*ptrdb1)*(*ptrdb2);
	ptrdb1++;
	ptrdb2++;
      }
  }
  
  tpdb = -((double)(gm->dim))/2.0*LOG_2_PI-0.5*gm->sigma_det_log;
  res = tpdb +(-0.5)*tpdb2;

  R_Free(db_array);
  R_Free(dif);

  return(res);
}

double gauss_pdf(double *ft, GaussModel *gm)
{
  return(exp(gauss_pdf_log(ft, gm)));
}

double mix_gauss_pdf_log(double *ft, GaussModel **gmlist, double *prior, 
			 int ncmp)
{
  double res, *h, v1,v2;
  int i;

  h=(double *)R_Calloc((size_t)ncmp,double);
  for (i=0;i<ncmp;i++)
    h[i]=gauss_pdf_log(ft, gmlist[i]);

  v1=h[0];
  for (i=1;i<ncmp;i++) if (h[i]>v1) v1=h[i];

  for (i=0,v2=0.0;i<ncmp;i++) {
    v2 += prior[i]*exp(h[i]-v1);
  }

  if (v2>0.0)  res=v1+log(v2); else res=-HUGE_VAL;

  R_Free(h);
  return(res);
}

