/* ---------------------------------------------------------------
  GGMselect R package
  Copyright INRA 2017
  INRA, UR1404, Research Unit MaIAGE
  F78352 Jouy-en-Josas, France.
 
  URL: http://genome.jouy.inra.fr/logiciels/GGMselect
-------------------------------------------------------------- */

#define USE_FC_LEN_T

/* ++++++++++++++ scr.c  +++++++++++++++++++ */
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Lapack.h>
#include <R_ext/Utils.h> // pour permettre l'interruption de l'exec
#include <float.h>

#ifndef FCONE
# define FCONE
#endif

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Multiplication of 2 matrix
INPUT
 Z: matrix (n, p)
 V: matrix (p, q)
 n: number of rows of Z
 p: number of columns of Z =  number of rows of V
 q: number of columns of V
OUTPUT
 res: matrix (n, q)  = Z %*% V
 Should be allocated before the call 
NOTE
 The dimensions are not checked
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

void GGMmultmm(double *Z, double *V, int n, int p,  int q,
	     double *res)
{
  int i, j, l;


for (i=0; i< n; i++) {
  for (j=0; j< q; j++) {
    res[n * j +i] =0.0;
    for (l=0; l< p; l++) {
      //      res[i,j] += (Z[i,l] * V[l,j]);
	res[n * j +i] += (Z[n * l + i] * V[p * j+l]);
    }
  }
 }

} // fin fonction

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Multiplication of a matrix by the transposed of another
INPUT
 Z: matrix (n, p)
 V: matrix (p, q)
 n: number of rows of Z
 p: number of columns of Z =  number of columns of V
 q: number of rows of V
 OUTPUT
 res: matrix (n, q) = Z %*% t(V)
 Should be allocated before the call 
NOTE
 The dimensions are not checked
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

void GGMmultmtm(double *Z, double *V, int n, int p,  int q,
	     double *res)
{
  int i, j, l;

for (i=0; i< n; i++) {
  for (j=0; j< q; j++) {
    for (l=0; l< p; l++) {
      //      res[i,j] += (Z[i,l] * V[j, l]);
	res[n * j +i] += (Z[n * l + i] * V[q * l+j]);
    }
  }
 }

}

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Multiplication of  matrices
 Compute: Z %*% V %*% t(Z)
INPUT
 Z: matrix (n, p)
 V: matrix (p, q)
 n: number of rows of Z
 p: number of columns of Z =  number of column of V
 q: number of rows of V
WORKING
 r1:  matrix (n, p)
 Should be allocated before the call
OUTPUT
 res: matrix (n, p) = Z %*% V %*% t(Z)
 Should be allocated before the call 
NOTE
 The dimensions are not checked
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

void GGMmultmmtm(double *Z, double *V, int n, int p,  int q,
		 double *r1,
	     double *res)
{

// calcul de Z %*%  V
  GGMmultmm(Z, V, n, p, q, r1);
// calcul de r1 %*% t(Z)
  GGMmultmtm(r1, Z, n, q, p, res);
} // fin fonction


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Multiplication of  matrices
 Compute: t(Z)%*% V
INPUT
 Z: matrix (n, p)
 V: matrix (p, q)
 n: number of rows of Z
 p: number of columns of Z =  number of column of Z
 q: number of rows of Z
WORKING
 W: matrix (n, p)
 Should be allocated before the call
OUTPUT
 res: matrix (n, q)= t(Z) %*% V
 Should be allocated before the call 
NOTE
 The dimensions are not checked
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMmulttmm(double *Z, double * V, int n, int p,
	      int q,
		double *W,
	     double * res)
{
  int i,j;

  // Construction de W=t(Z)
for (i=0; i< n; i++) {
  for (j=0; j< p; j++) {
      //      W[j,i]=Z[i,j]
    W[p * i +j] = 	Z[n * j +i];
  }
 }

// Multiplication matricielle
 GGMmultmm(W, V, p, n, q, res);


} // fin GGMmulttmm


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Compute:
 res = M %*% diag(y) %*% t(M)
INPUT
 M: matrix (n, p)
 y: vector (p)
 n: integer
 p: integer
OUTPUT
 res: matrix  (n, n)
 Should be allocated before the call
+++++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMmdiagtm(double *M, double *y,
		int n, int p,
		double *res)
{
  /* res[i,j]= y[1]*M[i,1]*M[j,1] +
               y[2]*M[i,2]*M[j,2] + ... +
               y[p]*M[i,p]*M[j,p]
  */
  int i,j, k;
  for (i=0; i< n; i++) {
    for (j=0; j< n; j++) {
      //      res[i,j]=0.0;
      res[n * j + i]=0.0;
      for (k=0; k< p; k++) {
	// res[i,j] += (y[k]*M[i,k]*M[j,k])
	res[n * j + i] += (y[k]*M[n * k + i]*M[n * k + j]);
      }
    }
  }
} // fin fonction


/* ++++++++++++++++++++++++++++++++++++++++++++++++ 
FUNCTION
 Replace the R function "svd" when x is not complex
 Call the Lapack program: DGESDD
INPUT
 x: matrix (n, p)
 n: number of rows of x
 p: number of columns of x
 nu, nv: see arguments of same name of the R function "svd"
 By convention, when nv=-1, the u and v vectors are not calculated
WORKING
 iwork: integer vector (8*(n<p ? n : p)
 xvals: double vector (n*p)
OUTPUT
 res: vector (n)
 See the d component of the output of the R function "svd"
 u, v: vectors (nu) and (nv)
 See the components of same name in the output
       of the R function "svd"
 The output should be allocated before the call
NOTE  
 This program is copied from modLa_svd 
 (see ~/R-2.8.0/src/modules/lapack/Lapack.c)
 DGESDD:
 (see ~/R-2.8.0/src/modules/lapack/dlapack1.f)
++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMsvd(double *x, int n, int p, int nu, int nv,
	    int *iwork, double *xvals,
	    double *res, double *u, double *v )
{
  int lwork =-1;
  int ldu = nu;
  int ldvt = nv;
  int info =0;
  double  tmp, *work;

  char jobu='N'; 
  char jobv='S'; 
  char job;
  // S= calculer les 1iers singular vectors
  // N= calculer aucun singular vectors


  // int *iwork= (int *) R_alloc(8*(n<p ? n : p), sizeof(int));

 int dimxvals = n * p;

//   xvals = (double *) R_alloc(dimxvals, sizeof(double));
/* work on a copy of x */
 Memcpy(xvals, x, dimxvals);

    if (ldvt == -1) {
      // Quand on ne veut pas les v, par convention, nv=-1
      ldu = 1;
      ldvt = 1;
      job = jobu;
    }    else {job= jobv;}

	/* ask for optimal size of work array */
	lwork = -1;

	F77_CALL(dgesdd)(&job,
			 &n, &p, xvals, &n, res,
			 u, &ldu,
			 v, &ldvt,
			 &tmp, &lwork, iwork, &info FCONE);

	if (info != 0)
	    error(("error code %d from first call to Lapack routine '%s'"), info, "dgesdd");
	lwork = (int) tmp;
	//	
 	// On alloue par R_Calloc pour pouvoir desallouer
	work  = (double *) R_Calloc(lwork, double);
	//	 work = (double *) R_alloc(lwork, sizeof(double));
	F77_CALL(dgesdd)(&job,
			 &n, &p, xvals, &n,  res,
			 u, &ldu,
			 v, &ldvt,
			work, &lwork, iwork, &info FCONE);

	R_Free(work);

	if (info != 0)
	    error(("error code %d from Lapack routine '%s'"), info, "dgesdd");
}


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Compute the following:
    M <- t(V)%*%V
    svdM <- svd(M,nu,nv)
INPUT
 V: matrix (n, p)
 n: number of rows of x
 p: number of columns of x
 nu, nv: see arguments of same name of the R function "svd"
 By convention, when nv=-1, the u and v vectors are not calculated
WORKING
 iwork: integer vector (p)
 xvals:  matrix (p, p)
 M: matrix (p, p)
 W: matrix (n, p)
 Should be allocated before the call
OUTPUT
 See GGMsvd
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMsvdM(double *V, int n, int p,
	     int nu, int nv,
	     int *iwork, double *xvals,
             double *M, double *W,
	     double * res,
	     double *u, double *v)
{

  GGMmulttmm(V,  V, n, p, p, W, M);
  GGMsvd(M, p, p, nu, nv,iwork, xvals, res, u, v);

} // fin GGMsvdM


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Compute
    res <- V%*%solve(M,t(V)%*%y)
 solve is replaced by a call to the LAPACK
 subroutine DGESV
INPUT
 M:  matrix (ncolv, ncolv)
 V: matrix (nrowv, ncolv)
 y: vector  (ncolv)
WORKING
 B: vector  (nrowv)
 W: matrix (nrowv, ncolv)
 ipiv: integer vector (ncolv)
 Should be allocated before the call
OUTPUT
 res: vector (nrowv)
 Should be allocated before the call
++++++++++++++++++++++++++++++++++++++++++++++++ */

void GGMsolveproj(  double *M, double *V, double *y, int nrowv, 
		    int ncolv,
		    double *B, double *W,
		    int *ipiv,
		    int *info,
		    double *res)
{
  int nrhs=1;
  int i;


  //   B <- matrix(t(V)%*%y, ncol=1)
  GGMmulttmm(V,  y, nrowv, ncolv, nrhs, W, B);
//  int *ipiv= (int *) R_alloc( ncolv, sizeof(int));
 for (i=0; i< ncolv; i++)
   ipiv[i] = i;

 // Appel du fortran, resultat dans B

 dgesv_(&ncolv, &nrhs, M, &ncolv, ipiv, B, &nrowv, info);


 if (*info >0)
   {
     error("Error in solveProj, call to DGESV:\nMatrix singular.  The factorization has been completed, but the factor U is exactly singular, so the solution could not be computed.");
   }
 if (*info <0)
   {
     error("Error in solveProj, call to DGESV:\n the %d-th argument had an illegal value", -(*info));
   }

 GGMmultmm(V, B, nrowv, ncolv,nrhs, res);


} // fin GGMsolveproj


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Compute the projection of y over the space spanned by V.
 Translation of the following:
------------------------------------------------------  
  M <- t(V)%*%V
  svdM <- svd(M,nu=0,nv=0)
  rgV <- sum(svdM$d > min.vp)

  if (rgV == d) {
    Proj <- V%*%solve(M,t(V)%*%y)
  } else {
    svdM <- svd(M)
    if (rgV > 1) {
      invM <-
        svdM$v[,1:rgV]%*%diag(1/svdM$d[1:rgV])%*%t(svdM$v[,1:rgV])
    }
    if (rgV==1) {
 { invM <-matrix(svdM$v[,1],ncol=1)%*%svdM$v[,1]/svdM$d[1] }
    Proj <- V%*%invM%*%t(V)%*%y
  }
  return(Proj)
  }
NOTE:
 To avoid possible double computation of svd, it is calculated svd(M)
 and never svd(M,nu=0,nv=0) which produced the same main result with
 reduced output
--------------------------------------------------
INPUT:
 V: n x d matrix
 y: n dim. vector
 n: integer
 d: integer
 minvp: (small) positive real number
WORKING:
 iwork: vector (d) integer
 svdMd: vector (d)
 r1:  matrix (n, d)
 W1: matrix (n, d)
 M: matrix (d, d)
 W2: matrix (d, d)
 W3: matrix (d, d)
 W4: vector (n)
 vu: matrix (d, d)
 svdMv: matrix (d, d)
 xvals: matrix (d, d)
 Should be allocated before the call
OUTPUT
 Proj: n dim. vector
 Should be allocated before the call
++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMcalcProjInd(double *V, double *y, 
		    int *n, int *d, double *minvp,
		    int *iwork, double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
		    double *Proj)
{
  int i, info;
  int un =1;
  int *ipiv;
  int  rgV= 0;
  int nn=*n;
  int dd=*d;
R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre

  /* Alloues avant une fois pour toutes 
  // S_alloc initialise a 0
     M = (double *) S_alloc(*d * *d, sizeof(double));
   W1 = (double *) S_alloc(*n * *d, sizeof(double));
   W2 = (double *) S_alloc(*d * *d, sizeof(double));
   W3 = (double *) S_alloc(*d * *d, sizeof(double));
 */

  /* Compute:    M <- t(V)%*%V; */
  GGMmulttmm(V, V, nn, dd, dd, W1, M);

  /* Alloues avant une fois pour toutes
   svdMd = (double *) S_alloc(*d, sizeof(double));
   vu = (double *) S_alloc(*d * *d, sizeof(double));
   svdMv = (double *) S_alloc(*d * *d, sizeof(double));
   iwork= (int *) R_alloc( *d, sizeof(int));
   xvals = (double *) R_alloc( *d * *d, sizeof(double));
*/

  /* Compute:         svdM <- svd(M)  */



  GGMsvdM(M, dd, dd, dd, dd, iwork, xvals, W2, W3, svdMd, vu, svdMv);

  /* Compute: rgV <- sum(svdM$d > min.vp) */
  rgV= 0;
  for (i=0; i<dd; i++) {
    if (svdMd[i] > *minvp)
      rgV++;
  }

  if (rgV ==0) {

    error("No eigenvalue for matrix inversion greater than min.ev: decrease the argument 'min.ev' \n");
  }

  if (rgV == dd ) {
//   Reuse the working array iwork
    ipiv = iwork;
    /* Compute:    Proj <- V%*%solve(M,t(V)%*%y) */
    GGMsolveproj(M, V, y, nn, dd, W4, W1, ipiv, &info, Proj);
    if (info != 0) {
      error("Error in calcProjInd (solve), code %d\n", info);
    }
  } // fin (rgV == *d )
  else {
    // Les cas rgV>1 et rgV=1 donnent les memes
    // resultats: ils peuvent etre confondus
    // bien qu'ils ne se programment pas
    // de la meme facon en R
    /* Compute:
 invM <- svdM$v[,1:rgV]%*%diag(1/svdM$d[1:rgV])%*%t(svdM$v[,1:rgV])
 que l'on decompose en:
 1/ Compute: svdMd= 1/svdMd
    */

      for (i=0; i<rgV; i++) {
	svdMd[i] = 1.0/svdMd[i];
      }
      // On peut reutiliser M pour mettre le resultat
      // de GGMmdiagtm, puisque M ne sert plus
    //  M: matrice a autant de lignes que svdMv, soit d
    // et autant de colonnes que de lignes, soit d
      /* 2/ Compute: M=svdMv %*%  diag(svdMd) %*% t(svdMv) */
      GGMmdiagtm(svdMv, svdMd, dd, rgV, M);

//     r1 = (double *) S_alloc(*n * *d, sizeof(double));
    /* Compute:
   Proj <- V%*%invM%*%t(V)%*%y
que l'on decompose en:
 1/ Compute: W1 = V%*%M  %*% t(V)
    */
    GGMmultmmtm(V, M, nn, dd, dd, r1, W1);

    /* 2/ Compute:  Proj= W1 %*% y */
    GGMmultmm(W1, y, nn, dd, un, Proj);

  } // fin else

} // fin fonction

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Put the content of the column ifrom from
 the matrix  mfrom (n rows) into the column
 ito of the matrix  mto (n rows)
INPUT
 mfrom : matrix (n, ignored)
 n: integer scalar= number of rows de mfrom and mto
 ifrom: integer scalar= index from 0 of the source column
 ito:   integer scalar= index from 0 of the target column
OUTPUT
 mto: matrix (n, ignored)
 Should be allocated before the call 
++++++++++++++++++++++++++++++++++++++++++++++++ */

void	    setCol(double *mfrom, int n,
		   int ifrom, int ito, 
		   double *mto)
{
  int i;
  for (i=0; i<n;i++) {
    // mto[i, ito] = mfrom[i, ifrom];
    mto[ito*n+i] = mfrom[ifrom*n+i];
  }
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Access to a component by name from a R-list
INPUT
 list: a R-list
 str: name of a component of list
RETURN VALUE
 the component "str" of list
NOTE
 This function is used by GGMcalcSCR
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
SEXP getListElement(SEXP list, const char *str)
{
  SEXP elmt = R_NilValue, names=getAttrib(list, R_NamesSymbol);
  int i;
  for (i=0; i< length(list); i++)
    if (strcmp(CHAR(STRING_ELT(names, i)), str) ==0) {
      elmt=VECTOR_ELT(list, i);
      break;
    }
  return(elmt);
}

  
/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Programmation of the loop on p to calculate SCR,
 Mat.Chap and mod.chap
 (QE method) 
 (complexite = p**(Dmax+1))
INPUT
 list: a list with all the input and output, i.e
 the following:
   X: double  matrix (n, p) ; observations
   Dmax: integer vector (p) ; Maximum degrees
   nMod: integer vector (max(Dmax)) ; 
   lesMod: list of Dmaxmax components, each one (d)
           is a integer matrix (d, nMod[d])
   n: integer scalar
   p: integer scalar
   minvp: double scalar
WORKING
 With setting Dmaxmax=(max(Dmax)):
 Proj:  double vector (n)
 workj: double vector (n)
 work: matrix (n, max(Dmax))
 M: matrix (Dmaxmax, Dmaxmax)
 W1: matrix (n, Dmaxmax)
 W2: matrix (Dmaxmax, Dmaxmax)
 W3: matrix (Dmaxmax, Dmaxmax)
 svdMd: vector (Dmaxmax)
 vu: matrix (Dmaxmax, Dmaxmax)
 svdMv: matrix (Dmaxmax, Dmaxmax)
 iwork: vector (Dmaxmax) integer
 xvals: matrix (Dmaxmax, Dmaxmax)
 Should be allocated before the call
OUTPUT
  list with components:
  SCR: list of p components.
       Each component j is a list of Dmax[j] components.
         Each component d is a vector of length nMod[d]
  Mat.Chap: array of dimension lK,p,p
  mod.Chap: list of lK components: each component iK is a list
      of p components; each component j is a vector of length Dmax[j]
NOTE
 This function is a SEXP, called by .Call (and not .C)
 because lists arguments.
+++++++++++++++++++++++++++++++++++++++++++++++++ */


SEXP GGMcalcSCRQE( SEXP gNormX, SEXP gpen, SEXP gligpen, SEXP glK,
		   SEXP gSCRminSCR, SEXP gSCRminmod,
		  SEXP list )
{

  // Les arguments de type liste
  SEXP scrj,  listlesMod ;
  SEXP gmodChapK, gmodChapKj;
  double  * modChapKj, *NormX, *pen;

  double *scrminjscr;// pointeur sur SCRminSCR
  double *scrminjmodd, *scrminjmodchap; //ATT: doivent etre double
  double *scrjd;// pointeur sur un composant de SCR[[d]]
  int *matlesMod;
  // Travail
  double trav, minscrj, sum, crmin, cr;
  int *lK, *ligpen, b, i, j, d, l, ii, ika, dcrmin;
  int ja, da, la; //indices a partir de 0

  /* Access aux composants de la liste en argu */
   SEXP glesMod = getListElement(list,"lesMod");
   SEXP gSCRout = getListElement(list,"SCRout");
   SEXP gSCR = getListElement(gSCRout,"SCR");
   SEXP gmodChap = getListElement(gSCRout,"mod.Chap");
   SEXP gmatChap = getListElement(gSCRout,"Mat.Chap");


  // Acces aux valeurs des arguments
  int *rp = INTEGER(getListElement(list,"p"));
  int *rn = INTEGER(getListElement(list,"n"));
  double *rminvp = REAL(getListElement(list,"minvp"));
  double *rX = NUMERIC_POINTER(getListElement(list,"X"));
  int *rnMod = INTEGER_POINTER(getListElement(list,"nMod"));
  int *rDmax = INTEGER_POINTER(getListElement(list,"Dmax"));
  double *rProj = NUMERIC_POINTER(getListElement(list,"Proj"));
  double *rwork = NUMERIC_POINTER(getListElement(list,"work"));
  double *rworkj = NUMERIC_POINTER(getListElement(list,"workj"));
  double *rM = NUMERIC_POINTER(getListElement(list,"M"));
  double *rW1 = NUMERIC_POINTER(getListElement(list,"W1"));
  double *rW2 = NUMERIC_POINTER(getListElement(list,"W2"));
  double *rW3 = NUMERIC_POINTER(getListElement(list,"W3"));
  double *rsvdMd = NUMERIC_POINTER(getListElement(list,"svdMd"));
  double *rvu = NUMERIC_POINTER(getListElement(list,"vu"));
  double *rsvdMv = NUMERIC_POINTER(getListElement(list,"svdMv"));
  int *riwork = INTEGER_POINTER(getListElement(list,"iwork"));
  double *rxvals = NUMERIC_POINTER(getListElement(list,"xvals"));
  double *rW4 = NUMERIC_POINTER(getListElement(list,"W4"));
  double *rr1 = NUMERIC_POINTER(getListElement(list,"r1"));
  double *matChap = NUMERIC_POINTER(gmatChap);

  NormX = NUMERIC_POINTER(gNormX);
  pen = NUMERIC_POINTER(gpen);
  lK= INTEGER_POINTER(glK);
  ligpen = INTEGER_POINTER(gligpen);

  matChap  = NUMERIC_POINTER(gmatChap);


  scrminjscr = NUMERIC_POINTER(gSCRminSCR);

  for (j=1; j<= *rp; j++) {
    R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre
    ja=j-1;
    // Le composant j de SCR
    scrj = VECTOR_ELT(gSCR, ja);

// mettre la colonne d'indice j
// de la matrice rX (rn, rp) dans la colonne
// d'indice 0 de workj (qui n'a qu'une colonne)
    setCol(rX, *rn, ja,  0, rworkj);

    // La boucle sur lK permet le calcul de MatChap et modChap
    // dans la foulée

    for (ika=0; ika < *lK; ika++) {
      dcrmin =0;
      //      crmin = NormX[ja] * (1+pen[0, ika])/n;
      crmin = NormX[ja] * 1+(pen[ ika * (*ligpen)])/ *rn;

	// pointeur sur le vecteur modChap[[ik]][[j]]
      gmodChapK = VECTOR_ELT(gmodChap, ika);
      gmodChapKj = VECTOR_ELT(gmodChapK, ja);
      modChapKj = NUMERIC_POINTER(gmodChapKj);

    for (d=1; d<=rDmax[ja]; d++) {

      da = d - 1;
      // Le d-ieme composant de scrj (un vecteur)
      scrjd = 	REAL(VECTOR_ELT(scrj, da));

      // Le d-ieme composant de scrminjmod (un vecteur)
      scrminjmodd = REAL(VECTOR_ELT(gSCRminmod, da));

      // prendre le d ieme composant de la liste
      // c'est une matrice
      listlesMod = VECTOR_ELT(glesMod, da);
      matlesMod = INTEGER_POINTER(listlesMod);

      if (ika==0) {
      // Pour le calcul du scr minimum
      ii =-1;
      minscrj = DBL_MAX;

      for (l=1; l<=rnMod[da]; l++) {
	la=l-1;
	for (i=0; i < d ; i++) {
	  //	  if (matlesMod[i,la] >=j) {
	  if (matlesMod[la * d + i] >= j) {
	    // mettre la colonne d'indice (matlesMod[i,l]+1)
	    // de la matrice rX (rn, rp) dans la colonne
	    // d'indice i de work
	    // mais ici les indices commencent a 0
	    //on met la colonne d'indice (matlesMod[i,l])
	    //	    setCol(rX, rn, (matlesMod[i,l]+1), i, rwork);
	    setCol(rX, *rn, matlesMod[la * d + i], i, rwork);
	  } else {
	    //	    setCol(rX, rn, matlesMod[i,l], i, rwork);
	    setCol(rX, *rn, (matlesMod[la * d + i]-1), i, rwork);
	  }
	} // fin i


	// Proj est alloue avant l'appel
	//	Proj = (double *) S_alloc(*rn, sizeof(double));
	// d=nb col de rwork

	GGMcalcProjInd(rwork, rworkj, rn, &d, rminvp,
		       riwork, rsvdMd, rr1,
		     rW1, rM,
		     rW2,  rW3,  rW4,
		    rvu, rsvdMv,
		    rxvals, rProj);

	sum=0.0;
	for (i=0; i < *rn; i++) {
	  //	  sum += (( X[i,j] - Proj[i]) **2);
	  trav = rX[ja * (*rn) +i] - rProj[i]; 
	  sum += ( trav * trav);

	}
	// SCR[[j]][[d]][l]
	scrjd[la] = sum;

	if (scrjd[la] <minscrj) {
	  minscrj = scrjd[la];
	  ii=la;
	}
      } //fin l
      scrminjscr[da] = scrjd[ii];

      for (i=0; i < d ; i++) {
	//	if (matlesMod[i,ii] >=j) {
	if (matlesMod[ii * d + i] >= j) {
	  scrminjmodd[i] = matlesMod[ii * d + i]  + 1;
	} else {
	  scrminjmodd[i] = matlesMod[ii * d + i];
	}
      } // fin i

      } // fin ika=0

      // Pour le calcul de modChap et matChap
      //      cr = scrminjscr[da] * 1+(pen[d, ika] / ( *rn-d));
      cr = scrminjscr[da] * (1+(pen[ ika * *ligpen + d] / ( *rn-d)));
	if (cr < crmin) {
	  crmin =cr;
	  dcrmin = d;
	} // fin (cr < crmin)
      
      } // fin d
// Calcul de modChap et matChap
      if (dcrmin >= 1) {
	dcrmin = dcrmin -1;
	scrminjmodchap = REAL(VECTOR_ELT(gSCRminmod, dcrmin));

      for (i=0; i <= dcrmin; i++) {
	//	b= scrminj$mod[[dcrmin]][i]
	b= scrminjmodchap[i];
	// modChap[[ika]][[ja]][i]=b
        modChapKj[i] = b;

	//	Matchap[ika, b-1, ja]=1
	matChap[ja* (*rp)* (*lK) + (b-1)* *lK + ika]=1;
      } // fin i

      } // fin (dcrmin > 1)
    } // fin iK
 
  } // fin j


    // Retour : les sorties sont SRC, matChap, modChap
return(gSCRout);
}

