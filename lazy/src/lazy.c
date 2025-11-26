/* ---------------------------------------- -*- mode: c; mode: font-lock -*- */
/* lazy.c                                 Lazy learning for local regression */
/* ------------------------------------------------------------------------- */

/* ========================================================================= */
/* Lazy learning for local regression                                        */
/* ------------------------------------------------------------------------- */
/* Copyright (C) 1999, 2003 Mauro Birattari and Gianluca Bontempi            */
/* ========================================================================= */
/* This program is free software; you can redistribute it and/or modify it   */
/* under the terms of the GNU General Public License as published by the     */
/* Free Software Foundation; either version 2 of the License, or (at your    */
/* option) any later version.                                                */
/*                                                                           */
/* This program is distributed in the hope that it will be useful, but       */
/* WITHOUT ANY WARRANTY; without even the implied warranty of                */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU         */
/* General Public License for more details.                                  */
/*                                                                           */
/* You should have received a copy of the GNU General Public License along   */
/* with this program; if not, write to the Free Software Foundation, Inc.,   */
/* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.                  */
/* ========================================================================= */

/* ========================================================================= */
/*             Mauro Birattari                     Gianluca Bontempi         */
/*                IRIDIA                     Departement d'Informatique      */
/*    Universite' Libre de Bruxelles       Universite' Libre de Bruxelles    */
/*             mbiro@ulb.ac.be                     gbonte@ulb.ac.be          */
/* ========================================================================= */

static char *id="$Id: lazy.c,v 1.4 2003/09/30 15:02:28 mbiro Exp $";

#include <math.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>

/* Definition of some types */
typedef struct {double **X; double *Y; double **Q; double **C;} inPtr_t;
typedef struct {int m; int M; int v;} idPar_t;
typedef struct {int A; int o[3];} cbPar_t;
typedef struct {int n; double* y; double* s; double *t; int *k;} bM_t;
typedef struct {bM_t A; bM_t o[3];} bModel_t;
typedef struct {double **Z; double *W; double **v; double *t; double *a;} aM_t;
typedef struct {int n; int *index; double *dist;} nn_t;
typedef struct {aM_t mat[3]; nn_t nn; int nz[3]; int mz[3]; int Go[3];
  int n; int m; int q; int mzA; int nzA;
  double *Wei; double LAMBDA; int DISTANCE;} auxInfo_t;
typedef struct {double *c; SEXP R;} doubleOut_t;
typedef struct {int *c; SEXP R;} intOut_t;
typedef struct {int noRptd; int lAns; doubleOut_t *y;
  doubleOut_t *t; intOut_t *k; doubleOut_t *S;
  doubleOut_t *T; intOut_t *I;} out_t;

/* Headers of some functions */
void getInput(SEXP,SEXP,SEXP,inPtr_t*,auxInfo_t*);
void getParAux(SEXP,auxInfo_t*,idPar_t*,int);
void getPar(SEXP,SEXP,SEXP,SEXP,auxInfo_t*,idPar_t*,cbPar_t*);
void getWeights(SEXP,auxInfo_t*);
void getLambda(SEXP,auxInfo_t*);
void getDistance(SEXP,auxInfo_t*);
void prepareMatrices(bModel_t*,auxInfo_t*,idPar_t*,cbPar_t*);
void allocateOutput(out_t*,bModel_t*,idPar_t*,cbPar_t*,auxInfo_t*,
                    SEXP,SEXP,SEXP,SEXP,SEXP);
void findNeighbors(int,auxInfo_t*,out_t*,inPtr_t*);
void reinit(auxInfo_t*,bModel_t*,cbPar_t*,inPtr_t*,out_t*);
void storeResults(double,int,bModel_t*,auxInfo_t*,cbPar_t*,out_t*,int);
void idVal(idPar_t*,cbPar_t*,auxInfo_t*,bModel_t*,out_t*,int);
void combineModels(int,out_t*,auxInfo_t*,cbPar_t*,bModel_t*);
SEXP packOutput(out_t*);


/* The lazy learning function */
SEXP lazy(SEXP XR, SEXP YR, SEXP QR,
          SEXP idConR, SEXP idLinR, SEXP idQuaR,
          SEXP dR, SEXP WR, SEXP cbR, SEXP lR,
          SEXP t_out, SEXP k_out, SEXP S_out, SEXP T_out, SEXP I_out){

  inPtr_t inPtr;
  idPar_t idPar[3];
  cbPar_t cbPar;
  bModel_t bModel;
  auxInfo_t auxInfo;
  out_t out;
  int q, i;

  getInput(XR, YR, QR, &inPtr, &auxInfo);
  getPar(idConR, idLinR, idQuaR, cbR, &auxInfo, idPar, &cbPar);
  getLambda(lR,&auxInfo);
  getDistance(dR,&auxInfo);
  getWeights(WR,&auxInfo);

  prepareMatrices(&bModel,&auxInfo,idPar,&cbPar);
  allocateOutput(&out,&bModel,idPar,&cbPar,&auxInfo,
                 t_out,k_out,S_out,T_out,I_out);

  /* main loop over the queries */
  for(q=0; q<auxInfo.q; q++){
    findNeighbors(q,&auxInfo,&out,&inPtr);
    reinit(&auxInfo,&bModel,&cbPar,&inPtr,&out);
    for (i=0; i<=2; i++)
      if (auxInfo.Go[i])
        idVal(idPar,&cbPar,&auxInfo,&bModel,&out,i);
    combineModels(q,&out,&auxInfo,&cbPar,&bModel);
  }
  return packOutput(&out);
}




/* Implementation of all the functions */

void getInput(SEXP XR, SEXP YR, SEXP QR, inPtr_t *ip, auxInfo_t *aI){
  int i;
  double *Xvec, *Qvec, *Cvec;
  SEXP dim;

  /* Examples: input */
  PROTECT(dim=GET_DIM(XR));
  aI->m=INTEGER_POINTER(dim)[0];   /* number of examples */
  aI->n=INTEGER_POINTER(dim)[1];   /* number of dimensions */
  XR=AS_NUMERIC(XR);
  Xvec=NUMERIC_POINTER(XR);
  UNPROTECT(1);

  /* Examples: output */
  if (GET_LENGTH(YR)!=aI->m)
    error("Y must be a vector of mx components");
  YR=AS_NUMERIC(YR);
  ip->Y=NUMERIC_POINTER(YR);

  /* Queries */
  PROTECT(dim=GET_DIM(QR));
  aI->q=INTEGER_POINTER(dim)[0];  /* number of queries */
  if (INTEGER_POINTER(dim)[1]!=aI->n)
    error("Matrix dimensions do not agree: nq!=nx");
  QR=AS_NUMERIC(QR);
  Qvec=NUMERIC_POINTER(QR);
  UNPROTECT(1);

  /* Allocate column-major matrices from R vectors */
  ip->X=(double**)R_alloc(aI->n,sizeof(double*));
  ip->Q=(double**)R_alloc(aI->n,sizeof(double*));
  ip->C=(double**)R_alloc(aI->n,sizeof(double*));
  Cvec=(double*)R_alloc(aI->n*aI->m,sizeof(double));

  for (i=0; i<aI->n; i++,Xvec+=aI->m,Qvec+=aI->q,Cvec+=aI->m)
    ip->X[i]=Xvec, ip->Q[i]=Qvec, ip->C[i]=Cvec;
}


void getParAux(SEXP iR, auxInfo_t *aI, idPar_t* d, int DEG){

  double c;
  char *name[] = {"conIdPar", "linIdPar", "quaIdPar"};
  int i;

  d[DEG].m=0, d[DEG].M=0, d[DEG].v=0;

  if (isNull(iR))
    aI->Go[DEG]=0;
  else
    switch (GET_LENGTH(iR)){
      case 1:
        iR=AS_NUMERIC(iR);
        c=NUMERIC_POINTER(iR)[0];
        if (c==0)
          aI->Go[DEG]=0,
            warning("%s=0: proceed assuming %s=NULL", name[DEG], name[DEG]);
        else{
          if (c<0)
            error("%s<0", name[DEG]);
          switch (DEG){
            case 0:
              d[DEG].m=3, d[DEG].M=(int)ceil(5*c), d[DEG].v=0;
              break;
            case 1:
              i=aI->n+1, d[DEG].m=(int)floor(3*i),
                d[DEG].M=(int)ceil(5*i*c), d[DEG].v=0;
              break;
            case 2:
              i=(aI->n+1)*(aI->n+2)/2,
                d[DEG].m=(int)floor(3*i),d[DEG].M=(int)ceil(5*i*c),d[DEG].v=0;
              break;
              /* I trust the calling function: DEG is 0, 1, or 2. */
          }
          if (d[DEG].M>aI->m)
            d[DEG].M=aI->m,
              warning("%s is too large: proceed with idM%d=mx", name[DEG],DEG);
          if (d[DEG].M<d[DEG].m)
            error("%s is too small: idM%d<idm%d", name[DEG],DEG,DEG);
          aI->Go[DEG]=1;
        }
        break;
      case 2:
        iR=AS_INTEGER(iR);
        d[DEG].m=INTEGER_POINTER(iR)[0];
        d[DEG].M=INTEGER_POINTER(iR)[1];
        d[DEG].v=0;
        if (d[DEG].m<2)
          error("%s: idm%d<2", name[DEG],DEG);
        if (d[DEG].M>aI->m)
          d[DEG].M=aI->m,
            warning("%s: idM%d is too large, proceed with idM%d=mx",
                    name[DEG],DEG,DEG);
        if (d[DEG].M<d[DEG].m)
          error("%s: idM%d<idm%d", name[DEG],DEG,DEG);
        aI->Go[DEG]=1;
        break;
      case 3:
        iR=AS_INTEGER(iR);
        d[DEG].m=INTEGER_POINTER(iR)[0];
        d[DEG].M=INTEGER_POINTER(iR)[1];
        d[DEG].v=INTEGER_POINTER(iR)[2];
        if (d[DEG].m<2)
          error("%s: idm%d<2", name[DEG],DEG);
        if (d[DEG].M>aI->m)
          d[DEG].M=aI->m,
            warning("%s: idM%d is too large, proceed with idM%d=mx",
                    name[DEG],DEG,DEG);
        if (d[DEG].v<0)
          error("%s: val%d<0", name[DEG],DEG);
        aI->Go[DEG]=1;
        break;
      default:
        error("Illegal %s", name[DEG]);
    }
}



void getPar(SEXP iCR, SEXP iLR, SEXP iQR, SEXP cbR,
            auxInfo_t *aI, idPar_t* id, cbPar_t *cb){
  int i, idTot;

  /* Identification Parameters */
  getParAux(iCR,aI,id,0);
  getParAux(iLR,aI,id,1);
  getParAux(iQR,aI,id,2);

  if (aI->Go[0]+aI->Go[1]+aI->Go[2]==0)
    error("Identification is empty.");

  /* Combination Parameters */
  cbR=PROTECT(AS_INTEGER(cbR));
  switch (GET_LENGTH(cbR)){
    case 3:
      cb->A=0;
      for (i=0;i<3;i++){
        if (INTEGER_POINTER(cbR)[i]<0)
          error("Negative element in cmbPar");
        cb->o[i]=INTEGER_POINTER(cbR)[i];
        if (((cb->o[i])&&(!aI->Go[i]))||((!cb->o[i])&&(aI->Go[i])))
          error("Conflict between (con|lin|qua)IdPar and cmbPar");
        if ((aI->Go[i])&&(cb->o[i]>id[i].M-id[i].m+1))
          cb->o[i]=id[i].M-id[i].m+1,
            warning("cmb%d is too large: proceed with idM%d-idm%d+1",i,i,i);
      }
      break;
    case 1:
      cb->A=INTEGER_POINTER(cbR)[0];
      if (cb->A<=0)
        error("cmbPar<=0");
      for (i=0, idTot=0; i<=2; i++)
        cb->o[i]=0, idTot+=aI->Go[i]*(id[i].M-id[i].m+1);
      if (cb->A>idTot)
        cb->A=idTot,
         warning("CmbPar is too large: proceed with cmbPar=#modelsIdentified");
      break;
    default:
      error("Combination parameter no good.");
  }
  UNPROTECT(1);
}



void getWeights(SEXP WR, auxInfo_t *aI){
  int i;

  if (isNull(WR))
    aI->Wei = NULL;
  else{
    if (GET_LENGTH(WR)!=aI->n)
      error("Weights vector no good.");
    WR=AS_NUMERIC(WR);
    aI->Wei=NUMERIC_POINTER(WR);
    for (i=0;i<aI->n;i++)
      if (aI->Wei[i]<0)
        error("Weights must be non-negative.");
  }
}

void getLambda(SEXP lR, auxInfo_t *aI) {
  aI->LAMBDA=NUMERIC_POINTER(AS_NUMERIC(lR))[0];
}

void getDistance(SEXP dR, auxInfo_t *aI){
  if (strcmp(STRING_VALUE(dR),"manhattan")==0)
    aI->DISTANCE=1;
  else if (strcmp(STRING_VALUE(dR),"euclidean")==0)
   aI->DISTANCE=2;
  else
    error("Distance parameter no good.");
}


void prepareMatricesAux(bModel_t *bM, auxInfo_t *aI, cbPar_t *cb,
                        idPar_t* d, int DEG){
  int i;
  double *tmp;

  if (DEG < 0 || DEG > 2)
    error("Invalid DEG value");

  aI->mz[DEG] = d[DEG].M;
  aI->mzA = (d[DEG].M > aI->mzA)? d[DEG].M : aI->mzA;


  switch (DEG) {
    case 0: aI->nz[DEG] = 1; break;
    case 1: aI->nz[DEG] = aI->n+1; break;
    case 2: aI->nz[DEG] = (aI->n+1)*(aI->n+2)/2; break;
  }
  aI->nzA = aI->nz[DEG];

  aI->mat[DEG].t=(double*)R_alloc(aI->nz[DEG],sizeof(double));

  if (DEG>0){
    tmp=(double*)R_alloc(aI->mz[DEG]*aI->nz[DEG],sizeof(double));
    aI->mat[DEG].Z=(double**)R_alloc(aI->mz[DEG],sizeof(double*));
    for (i=0; i<aI->mz[DEG]; i++,tmp+=aI->nz[DEG])
      aI->mat[DEG].Z[i]=tmp;
    tmp=(double*)R_alloc(aI->nz[DEG]*aI->nz[DEG],sizeof(double));
    aI->mat[DEG].v=(double**)R_alloc(aI->nz[DEG],sizeof(double*));
    aI->mat[DEG].a=(double*)R_alloc(aI->nz[DEG],sizeof(double));
    for (i=0; i<aI->nz[DEG]; i++)
      aI->mat[DEG].v[i]=tmp+i*aI->nz[DEG];
  }
  if (!cb->A){
    bM->o[DEG].n=cb->o[DEG];
    bM->o[DEG].y=(double*)R_alloc(bM->o[DEG].n+1,sizeof(double));
    bM->o[DEG].s=(double*)R_alloc(bM->o[DEG].n+2,sizeof(double));
    bM->o[DEG].s[0]=0;
  }
}

void prepareMatrices(bModel_t *bM, auxInfo_t *aI,
                     idPar_t *id, cbPar_t *cb){
  int i;

  aI->mzA=0;
  for (i=0; i<=2; i++)
    if (aI->Go[i])
      prepareMatricesAux(bM,aI,cb,id,i);

  /* aI->mat[0].W, aI->mat[1].W, and aI->mat[2].W */
  /* all point to the same vector.                */
  aI->mat[0].W=(double*)R_alloc(aI->mzA,sizeof(double));
  aI->mat[2].W = aI->mat[1].W = aI->mat[0].W;

  if (cb->A){
    bM->A.n=cb->A;
    bM->A.y=(double*)R_alloc(bM->A.n+1,sizeof(double));
    bM->A.s=(double*)R_alloc(bM->A.n+2,sizeof(double));
    bM->A.s[0]=0;
  }
  /* Nearest neighbors */
  aI->nn.n=aI->mzA;
  aI->nn.index=(int*)R_alloc(aI->nn.n+1,sizeof(int));
  aI->nn.dist=(double*)R_alloc(aI->nn.n+2,sizeof(double));
  aI->nn.dist[0]=0;
}

void allocateOutput(out_t *out, bModel_t *bM, idPar_t *id,
                    cbPar_t *cb, auxInfo_t *aI,
                    SEXP t_out, SEXP k_out, SEXP S_out,
                    SEXP T_out, SEXP I_out){
  int i;
  SEXP dim;

  out->noRptd=0;
  out->lAns=0;

  out->y=(doubleOut_t*)R_alloc(1,sizeof(doubleOut_t));
  PROTECT(out->y->R=NEW_NUMERIC(aI->q));
  out->y->c=NUMERIC_POINTER(out->y->R);
  out->noRptd++;
  out->lAns++;

  if (LOGICAL_DATA(t_out)[0]){
    out->lAns++;
    if (cb->A)
      bM->A.t=(double*)R_alloc(aI->nzA*(bM->A.n+1),sizeof(double));
    else
      for (i=0; i<=2; i++)
        if (aI->Go[i])
          bM->o[i].t=(double*)R_alloc(aI->nz[i]*(bM->o[i].n+1),sizeof(double));
    out->t=(doubleOut_t*)R_alloc(1,sizeof(doubleOut_t));
    PROTECT(out->t->R=NEW_NUMERIC(aI->nzA*aI->q));
    PROTECT(dim=NEW_INTEGER(2));
    INTEGER_POINTER(dim)[0]=aI->nzA; INTEGER_POINTER(dim)[1]=aI->q;
    SET_DIM(out->t->R,dim);
    UNPROTECT(1);
    out->t->c=NUMERIC_POINTER(out->t->R);
    out->noRptd++;
  }else
    out->t=0;

  if (LOGICAL_DATA(k_out)[0]){
    out->lAns++;
    if (cb->A)
      bM->A.k=(int*)R_alloc(bM->A.n+1,sizeof(int));
    else
      for (i=0; i<=2; i++)
        if (aI->Go[i])
          bM->o[i].k=(int*)R_alloc(bM->o[i].n+1,sizeof(int));
    out->k=(intOut_t*)R_alloc(1,sizeof(intOut_t));
    PROTECT(out->k->R=NEW_INTEGER(aI->q));
    out->k->c=INTEGER_POINTER(out->k->R);
    out->noRptd++;
  }else
    out->k=0;

  if (LOGICAL_DATA(S_out)[0]){
    out->lAns++;
    out->S=(doubleOut_t*)R_alloc(3,sizeof(doubleOut_t));
    for (i=0; i<=2; i++)
      if (aI->Go[i]){
        PROTECT(out->S[i].R=NEW_NUMERIC(id[i].M*aI->q));
        PROTECT(dim=NEW_INTEGER(2));
        INTEGER_POINTER(dim)[0]=id[i].M; INTEGER_POINTER(dim)[1]=aI->q;
        SET_DIM(out->S[i].R,dim);
        UNPROTECT(1);
        out->S[i].c=NUMERIC_POINTER(out->S[i].R);
        out->noRptd++;
      }else{
        PROTECT(out->S[i].R=NULL_USER_OBJECT);
        out->noRptd++;
      }
  }else
    out->S=0;

  if (LOGICAL_DATA(T_out)[0]){
    out->lAns++;
    out->T=(doubleOut_t*)R_alloc(3,sizeof(doubleOut_t));
    for (i=0; i<=2; i++)
      if (aI->Go[i]){
        PROTECT(out->T[i].R=NEW_NUMERIC(aI->nz[i]*id[i].M*aI->q));
        PROTECT(dim=NEW_INTEGER(3));
        INTEGER_POINTER(dim)[0]=aI->nz[i];
        INTEGER_POINTER(dim)[1]=id[i].M;
        INTEGER_POINTER(dim)[2]=aI->q;
        SET_DIM(out->T[i].R,dim);
        UNPROTECT(1);
        out->T[i].c=NUMERIC_POINTER(out->T[i].R);
        out->noRptd++;
      }else{
        PROTECT(out->T[i].R=NULL_USER_OBJECT);
        out->noRptd++;
      }
  }else
    out->T=0;

  if (LOGICAL_DATA(I_out)[0]){
    out->lAns++;
    out->I=(intOut_t*)R_alloc(1,sizeof(intOut_t));
    PROTECT(out->I->R=NEW_INTEGER(aI->mzA*aI->q));
    PROTECT(dim=NEW_INTEGER(2));
    INTEGER_POINTER(dim)[0]=aI->mzA; INTEGER_POINTER(dim)[1]=aI->q;
    SET_DIM(out->I->R,dim);
    UNPROTECT(1);
    out->I->c=INTEGER_POINTER(out->I->R);
    out->noRptd++;
  }else
    out->I=0;
}


void weightedManhattan(int q, double **C, double **X, double **Q,
                       int mx, int nx,
                       int P, double *BestDist, int* BestIndx, double *W){
  int i, j, p;
  double dist;

  for (i=0; i<mx; i++){
    dist=0.0;
    for (j=0; j<nx; j++)
      C[j][i]=X[j][i]-Q[j][q], dist+=W[j]*fabs(C[j][i]);
    for(p=P; dist<BestDist[p]; p--)
      BestDist[p+1]=BestDist[p], BestIndx[p]=BestIndx[p-1];
    BestDist[p+1]=dist, BestIndx[p]=i;
  }
}



void weightedEuclidean(int q, double **C, double **X, double **Q,
                       int mx, int nx,
                       int P, double *BestDist, int* BestIndx, double *W){
  int i, j, p;
  double dist, tmp;
  for (i=0; i<mx; i++){
    dist=0.0;
    for (j=0; j<nx; j++)
      C[j][i]=X[j][i]-Q[j][q], tmp=C[j][i], tmp*=tmp, dist+=W[j]*tmp;
    for(p=P; dist<BestDist[p]; p--)
      BestDist[p+1]=BestDist[p], BestIndx[p]=BestIndx[p-1];
    BestDist[p+1]=dist, BestIndx[p]=i;
  }
}

void unweightedManhattan(int q, double **C, double **X, double **Q,
                         int mx, int nx,
                         int P, double *BestDist, int* BestIndx){
  int i, j, p;
  double dist;

  for (i=0; i<mx; i++){
    dist=0.0;
    for (j=0; j<nx; j++)
      C[j][i]=X[j][i]-Q[j][q], dist+=fabs(C[j][i]);
    for(p=P; dist<BestDist[p]; p--)
      BestDist[p+1]=BestDist[p], BestIndx[p]=BestIndx[p-1];
    BestDist[p+1]=dist, BestIndx[p]=i;
  }
}



void unweightedEuclidean(int q, double **C, double **X, double **Q,
                         int mx, int nx,
                         int P, double *BestDist, int* BestIndx){
  int i, j, p;
  double dist, tmp;
  for (i=0; i<mx; i++){
    dist=0.0;
    for (j=0; j<nx; j++)
      C[j][i]=X[j][i]-Q[j][q], tmp=C[j][i], tmp*=tmp, dist+=tmp;
    for(p=P; dist<BestDist[p]; p--)
      BestDist[p+1]=BestDist[p], BestIndx[p]=BestIndx[p-1];
    BestDist[p+1]=dist, BestIndx[p]=i;
  }
}

void findNeighbors(int q, auxInfo_t *aI, out_t *out, inPtr_t *in){

  int i;
  double initDist=R_PosInf;

  for (i=1; i<=aI->nn.n; i++)
     aI->nn.dist[i]=initDist;
   if (aI->Wei)
     if (aI->DISTANCE==1)
       weightedManhattan(q, in->C,in->X,in->Q,aI->m,aI->n,
                         aI->nn.n,aI->nn.dist,aI->nn.index,aI->Wei);
     else
       weightedEuclidean(q, in->C,in->X,in->Q,aI->m,aI->n,
                         aI->nn.n,aI->nn.dist,aI->nn.index,aI->Wei);
   else
     if (aI->DISTANCE==1)
       unweightedManhattan(q, in->C,in->X,in->Q,aI->m,aI->n,
                           aI->nn.n,aI->nn.dist,aI->nn.index);
     else
       unweightedEuclidean(q, in->C,in->X,in->Q,aI->m,aI->n,
                           aI->nn.n,aI->nn.dist,aI->nn.index);

   if (out->I)
     for(i=0; i<aI->mzA; i++)
       *(out->I->c++)=aI->nn.index[i]+1;
}

void reinit(auxInfo_t *aI, bModel_t *bM, cbPar_t *cb,
            inPtr_t *in, out_t *out){
  int i,j, p, m, tmp, indx;
  double *tmpPtr, initDist=R_PosInf;

  if (cb->A){
    for (p=1; p<=bM->A.n; p++)
      bM->A.s[p]=initDist;
    if (out->t)
      for(p=0; p<aI->nzA*(bM->A.n+1); p++)
        bM->A.t[p]=0;
  }else{
    for (i=0;i<=2;i++)
      if (aI->Go[i])
        for (p=1; p<=bM->o[i].n; p++)
          bM->o[i].s[p]=initDist;;
  }

  /* notice that aI->mat[0].W, aI->mat[1].W, */
  /* and aI->mat[2].W all point to the same  */
  /*vector. See function prepareMatrices()   */
  for(j=0; j<aI->mzA; j++)
    aI->mat[0].W[j]=in->Y[aI->nn.index[j]];

  for (i=0;i<=2;i++)
    if (aI->Go[i])
      for(j=0; j<aI->nz[i]; j++)
        aI->mat[i].t[j]=0.0;

  for (i=1;i<=2;i++){
    if (aI->Go[i]){
      tmpPtr = *(aI->mat[i].v);
      tmp = aI->nz[i];
      for (j=0; j<tmp*tmp; j++)
        *(tmpPtr++)=0.0;
      for (j=0; j<tmp; j++)
        aI->mat[i].v[j][j]=aI->LAMBDA;
      tmpPtr = *(aI->mat[i].Z);
      for(j=0; j<aI->mz[i]; j++){
        indx=aI->nn.index[j];
        *(tmpPtr++)=1.0;
        for(p=0; p<aI->n; p++)
          *(tmpPtr++)=in->C[p][indx];
        if (i==2)
          for(p=0; p<aI->n; p++)
            for(m=p; m<aI->n; m++)
              *(tmpPtr++)=in->C[p][indx]*in->C[m][indx];
      }
    }
  }
}

void storeResults(double eC, int k, bModel_t *bM, auxInfo_t *aI,
                  cbPar_t *cb, out_t *out, int DEG){
  int p, r, j;

  if (cb->A){
    for(p=bM->A.n; eC<bM->A.s[p]; p--){
      bM->A.s[p+1]=bM->A.s[p], bM->A.y[p]=bM->A.y[p-1];
      if(out->k)
        bM->A.k[p]=bM->A.k[p-1];
      if (out->t)
        for(r=0, j=p*aI->nzA; r<aI->nzA; r++,j++)
          bM->A.t[j]=bM->A.t[j-aI->nzA];
    }
    bM->A.s[p+1]=eC, bM->A.y[p]=aI->mat[DEG].t[0];
    if (out->k)
      bM->A.k[p]=k+1;
    if (out->t){
      for(r=0,j=p*aI->nzA; r<aI->nz[DEG]; r++,j++)
        bM->A.t[j]=aI->mat[DEG].t[r];
      for( ; r<aI->nzA; r++,j++)
        bM->A.t[j]=0;
    }
  }else{
    for(p=bM->o[DEG].n; eC<bM->o[DEG].s[p]; p--){
      bM->o[DEG].s[p+1]=bM->o[DEG].s[p], bM->o[DEG].y[p]=bM->o[DEG].y[p-1];
      if(out->k)
        bM->o[DEG].k[p]=bM->o[DEG].k[p-1];
      if (out->t)
        for(r=0, j=p*aI->nz[DEG]; r<aI->nz[DEG]; r++,j++)
          bM->o[DEG].t[j]=bM->o[DEG].t[j-aI->nz[DEG]];
    }
    bM->o[DEG].s[p+1]=eC, bM->o[DEG].y[p]=aI->mat[DEG].t[0];
    if(out->k)
      bM->o[DEG].k[p]=k+1;
    if (out->t)
      for(r=0, j=p*aI->nz[DEG]; r<aI->nz[DEG]; r++,j++)
        bM->o[DEG].t[j]=aI->mat[DEG].t[r];
  }
}


void idValStd(idPar_t *id, cbPar_t *cb, auxInfo_t *aI, bModel_t *bM,
           out_t *out, int DEG){
  int k, i, j, l, vl;
  double e, b, tmp, sse, eC;
  double *W=aI->mat[DEG].W, **v=aI->mat[DEG].v, **Z=aI->mat[DEG].Z;
  double *a=aI->mat[DEG].a, *t=aI->mat[DEG].t;
  int m=id[DEG].m, M=id[DEG].M, val=id[DEG].v, nz=aI->nz[DEG];

  for (k=0; k<M; k++){
    /* Identification */
    e=W[k];
    b=1;
    for (i=0; i<nz; i++){
      tmp=0;
      for(j=0; j<nz; j++)
        tmp += v[j][i]*Z[k][j];
      a[i]=tmp;
      b += Z[k][i]*tmp;
      e -= Z[k][i]*t[i];
    }
    for (i=0; i<nz; i++)
      for(j=0; j<nz; j++)
        v[j][i] -= a[i] * a[j] / b;
    for (i=0; i<nz; i++){
      tmp=0;
      for(j=0; j<nz; j++)
        tmp += v[j][i] * Z[k][j];
      t[i] += e * tmp;
    }
    if (out->T)
      memcpy(out->T[DEG].c,t,nz*sizeof(double)), out->T[DEG].c+=nz;
    /* Validation */
    if ((out->S)||(k>=m-1)){
      if (k==0){
        if (out->S)
          *(out->S[DEG].c++)=R_NaReal;
      }else{
        if (val != 0 && val < k + 1)
            vl = val;
        else
            vl = k + 1;
        sse=0;
        for(l=0; l<vl; l++){
          e=W[l];
          b=1;
          for (i=0; i<nz; i++){
            tmp=0;
            for(j=0; j<nz; j++)
              tmp += v[j][i] * Z[l][j];
            b -= Z[l][i] * tmp;
            e -= Z[l][i] * t[i];
          }
          tmp=e/b, tmp*=tmp;
          sse += tmp;
        }
        eC=sse/(k+1);
        if (out->S)
          *(out->S[DEG].c++)=sse/(k+1);
        if (k>=m-1)
          storeResults(eC,k,bM,aI,cb,out,DEG);
      }
    }
  }
}

void idValSpeedy(idPar_t *id, cbPar_t *cb, auxInfo_t *aI, bModel_t *bM,
                 out_t *out, int DEG){
  int k, i, vl;
  double y, e, tmp, eC;
  double *W=aI->mat[DEG].W, *t=aI->mat[DEG].t;
  int m=id[DEG].m, M=id[DEG].M, val=id[DEG].v;

      y=W[0];
      eC=1;
      if (out->T)
        *(out->T[DEG].c++)=y;
      if (out->S)
        *(out->S[DEG].c++)=R_NaReal;

      for (k=1; k<M; k++){
        if (val){
          y=(k*y+W[k])/(k+1);
          e=0;
          vl=(val<k+1)? val : k+1;
          for (i=0;i<vl;i++)
            tmp=y-W[i], tmp*=tmp, e+=tmp;
          eC=e*vl/(vl-1);
        }else{
          tmp=W[k]-y, tmp*=tmp;
          eC=eC*(k+1)*(k-1)*(k-1)/(k*k*k)+tmp/k;
          y=(k*y+W[k])/(k+1);
        }
        t[0]=y;
        if (out->T)
          *(out->T[DEG].c++)=y;
        if (out->S)
          *(out->S[DEG].c++)=eC;
        if (k>=m-1)
          storeResults(eC,k,bM,aI,cb,out,DEG);
      }
}


void idVal(idPar_t *id, cbPar_t *cb, auxInfo_t *aI, bModel_t *bM,
           out_t *out, int DEG){
  typedef void(*idVal_t)(idPar_t*,cbPar_t*,auxInfo_t*,bModel_t*,out_t*,int);
  idVal_t idValAux[]={&idValSpeedy,&idValStd,&idValStd};

  idValAux[DEG](id,cb,aI,bM,out,DEG);
}

void combineModels(int q, out_t *out, auxInfo_t *aI,
                   cbPar_t *cb, bModel_t *bM){

  double *t, e, y=0, w=0;
  int DEG, r, p, j, i, k_max=0;

  t=(double*)R_alloc(aI->nzA,sizeof(double));

  if (cb->A){
    if (out->k)
      for (p=0; p<bM->A.n; p++)
        k_max=(bM->A.k[p]>k_max)? bM->A.k[p] : k_max;
    if (out->t){
      for (j=0; j<aI->nzA; j++)
        t[j]=0;
      for(i=0; i<cb->A; i++){
        e = bM->A.s[i+1];
        e = (e==0)? 1E-20 : e;
        for (j=0,r=i*aI->nzA; j<aI->nzA; j++,r++)
          t[j] += bM->A.t[r]/e;
        y += bM->A.y[i]/e;
        w += 1/e;
      }
    }else{
      for(i=0; i<cb->A; i++){
        e = bM->A.s[i+1];
        e = (e==0)? 1E-20 : e;
        y += bM->A.y[i]/e;
        w += 1/e;
      }
    }
  }else{
    if(out->k){
      for (DEG=0; DEG<=2; DEG++)
        if (aI->Go[DEG])
          for (p=0; p<bM->o[DEG].n; p++)
            k_max=(bM->o[DEG].k[p]>k_max)? bM->o[DEG].k[p] : k_max;
    }
    if (out->t){
      for (j=0; j<aI->nzA; j++)
        t[j]=0;
      for (DEG=0; DEG<=2; DEG++)
        for(i=0; i<cb->o[DEG]; i++){
          e = bM->o[DEG].s[i+1];
          e = (e==0)? 1E-20 : e;
          for (p=0,r=i*aI->nz[DEG]; p<aI->nz[DEG]; p++,r++)
            t[p] += bM->o[DEG].t[r]/e;
          y += bM->o[DEG].y[i]/e;
          w += 1/e;
        }
    }else{
      for (DEG=0; DEG<=2; DEG++)
        for(i=0; i<cb->o[DEG]; i++){
          e = bM->o[DEG].s[i+1];
          e = (e==0)? 1E-20 : e;
          y += bM->o[DEG].y[i]/e;
          w += 1/e;
        }
    }
  }

  if (out->k)
    *(out->k->c++)=k_max;
  if (out->t)
    for (i=0; i<aI->nzA; i++)
      *(out->t->c++)=t[i]/w;
  out->y->c[q]=y/w;

}

SEXP packOutput(out_t *out){
  SEXP ans, names, S_list, S_names, T_list, T_names;
  int i=0, j;
  char *name[] = {"con", "lin", "qua"};

  /* Allocate output list */
  PROTECT(ans=NEW_LIST(out->lAns));
  PROTECT(names=NEW_CHARACTER(out->lAns));
  out->noRptd+=2;
  SET_ELEMENT(ans,i=0,out->y->R);
  CHARACTER_POINTER(names)[i++]=CREATE_STRING_VECTOR("h");
  if (out->t)
    SET_ELEMENT(ans,i,out->t->R),
        CHARACTER_POINTER(names)[i++]=CREATE_STRING_VECTOR("t");
  if (out->k)
    SET_ELEMENT(ans,i,out->k->R),
        CHARACTER_POINTER(names)[i++]=CREATE_STRING_VECTOR("k");
  if (out->S){
    PROTECT(S_list=NEW_LIST(3));
    PROTECT(S_names=NEW_CHARACTER(3));
    out->noRptd+=2;
    for (j=0; j<=2; j++)
        SET_ELEMENT(S_list,j,out->S[j].R),
          CHARACTER_POINTER(S_names)[j]=CREATE_STRING_VECTOR(name[j]);
    SET_NAMES(S_list, S_names);
    SET_ELEMENT(ans,i,S_list),
      CHARACTER_POINTER(names)[i++]=CREATE_STRING_VECTOR("S");
  }
  if (out->T){
    PROTECT(T_list=NEW_LIST(3));
    PROTECT(T_names=NEW_CHARACTER(3));
    out->noRptd+=2;
    for (j=0; j<=2; j++)
      SET_ELEMENT(T_list,j,out->T[j].R),
        CHARACTER_POINTER(T_names)[j]=CREATE_STRING_VECTOR(name[j]);
    SET_NAMES(T_list,T_names);
    SET_ELEMENT(ans,i,T_list),
      CHARACTER_POINTER(names)[i++]=CREATE_STRING_VECTOR("T");
  }
  if (out->I)
    SET_ELEMENT(ans,i,out->I->R),
      CHARACTER_POINTER(names)[i++]=CREATE_STRING_VECTOR("I");
  SET_NAMES(ans, names);
  UNPROTECT(out->noRptd);
  return ans;
}
