/* ---------------------------------------------------------------
  GGMselect R package
  Copyright INRA 2017
  INRA, UR1404, Research Unit MaIAGE
  F78352 Jouy-en-Josas, France.
 
  URL: http://genome.jouy.inra.fr/logiciels/GGMselect
-------------------------------------------------------------- */

/* ++++++++++++++ critQE  +++++++++++++++++++ */
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Utils.h> // pour permettre d'interrompre

// Pour accéder à un composant nommé d'une liste passée en
// argument
extern SEXP getListElement(SEXP list, const char *str);



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
NOTE
  Same as function setCol, but, here, the matrix
  are integer and the output matrix has not the same number
  rows as the input matrix
CALLED BY
  GGMloopGrSymQE
++++++++++++++++++++++++++++++++++++++++++++++++ */

void	intsetCol(int *mfrom, int nrowfrom,
		   int ifrom, int ito, 
		      int nrowto, int *mto)
{
  int i;
  for (i=0; i<nrowfrom;i++) {
    // mto[i, ito] = mfrom[i, ifrom];
    mto[ito*nrowto+i] = mfrom[ifrom*nrowfrom+i];
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Remplacer une grosse boucle dans calcGrChapSymQE, c.a.d les
 instructions R suivantes:
            A <- NULL
            for (a in 1:dim(Mod)[2]) {
              i1 <- ind1>Mod[d-1,a]
              mat <- matrix(rep(Mod[,a],length(ind1[i1])),nrow=d-1)
              mat1 <- rbind(mat,t(ind1[i1]))
              A <- cbind(A,mat1)
            }
            Mod <- A
INPUT
 Mod: integer matrix (nrowMod, ncolMod)
 d,nrowMod, ncolMod, ncolModOut, lgind1: integer scalars
 ind1: integer vector of length lgind1
OUTPUT
 ModOut: integer matrix (nrowMod+1, ncolModOut)
Should be allocated before the call
CALLED BY
  The R function calcGrChapSymQE
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMloopGrSymQE(int *Mod, int *d, int *nrowMod, int *ncolMod, 
		    int *nrowModOut, int *ncolModOut,
		    int *lgind1, int *ind1, int *ModOut)
{
  int a,b, iModOut=0;
R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre


  for (a=0; a<*ncolMod; a++) {
    for (b=0; b<*lgind1; b++) {
      //      if (ind1[b] <= Mod[d-1, a]) continue; // de boucle b
      if (ind1[b] <= Mod[(*nrowMod)* a+(*d-2)]) continue; // de boucle b

      intsetCol(Mod, (*nrowMod), a, iModOut, (*nrowModOut), ModOut);
      //      ModOut[nrowMod+1, iModOut] = ind1[b];
      ModOut[iModOut*(*nrowModOut)+  *nrowMod] = ind1[b];
      iModOut++;
    } // fin b
  } //fin a
} //fin GGMloopGrSymQE



/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Translation of the R choose function when all the arguments >1
INPUT
 n: integer scalar
 k: integer scalar
OUTPUT
 res: double  scalar
   res= n*(n-1)*(n-2)* ... *(n-k+1)/k!
CALLED BY
  GGMscrgQE
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

double GGMchoose( int n, int k)
{
  int i;
  double num=1.0, den=1.0;
  for (i=0; i< k; i++) {
    num *= (double) (n-i);
    den *= (double) (i+1);
  }
  return(num/den);
}


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Remplacer la fonction calcSCRGQE de QE.R:
  p <- dim(G)[2]
  scrG <- list(scr=rep(0,p),d=rep(0,p))
  for (j in 1:p) {
    ind <- (1:p)[G[,j]==1]
    d <- length(ind)
    if (d==0) { 
      scrG$scr[j] <- NormX[j]
    }
    else {
      ind[ind>j] <- ind[ind>j]-1
      if (d==1) l <- ind[1]
      else {
        l<-choose(p-1,d)-(p-1-ind[d])
        l <- l-sum(choose(p-1-ind[1:(d-1)],d-(1:(d-1))+1))
        # Equivalent de:  for (k in 1:(d-1)) l <- l-choose(p-1-ind[k],d-k+1)
      }
      scrG$scr[j] <-SCR[[j]][[d]][l]
      scrG$d[j] <- d
    }
  }
INPUT
   p: integer
   G: adjacency matrix with dimension pxp (integer)
   NormX: vector with dimension p
   SCR: output from calcSCRQE
WORKING
   ind: integer vector  with dimension p
OUTPUT : scrG list with components :
   scr : vector with dimension p.
      scr[j]= Residual sum of squares associated to
              variable j of G
   d: vector with dimension p.
     d[j] Degree of  variable j of G
 Should be allocated before the call
CALLED BY
  GGMscrgcritQE
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

SEXP GGMscrgQE(SEXP list)
{
/* Pointeurs sur les composants de la liste en argu */
  SEXP  gSCR, gscrG, SCRj,SCRjd, gscrGscr, gscrGd;
  double *SCRjdv;// pointeur sur un composant de SCRj[[d]]

// Pointeurs sur les valeurs des arguments
  int *p, *G, *ind, *scrGd;
  double * NormX, *scrGscr;
  // Travail
  int j, k, d, pm1, ia, ib;
  double l, ll;

/* Access aux composants de la liste en argu */
  gSCR = getListElement(list,"SCR");
  gscrG = getListElement(list,"scrG");
  gscrGscr = getListElement(gscrG,"scr");
  gscrGd = getListElement(gscrG,"d");

// Acces aux valeurs des arguments
  p = INTEGER(getListElement(list,"p"));
  G = INTEGER_POINTER(getListElement(list,"G"));
  ind = INTEGER_POINTER(getListElement(list,"ind"));
  NormX  = NUMERIC_POINTER(getListElement(list,"NormX"));
  scrGscr = NUMERIC_POINTER(gscrGscr);
  scrGd = INTEGER_POINTER(gscrGd);

 pm1= *p-1;
 for (j=0; j< *p; j++) {
   scrGd[j] = 0;
   for (k=0; k< *p; k++) 
    ind[k]=0;
   // ind contiendra les indices des lignes de G
   // telles que G[,j] =1
   SCRj = VECTOR_ELT(gSCR, j); //SCR[[j]]
   d=0; // nbre de G[,j]=1

   // Calcul de ind
   /* Instructions R equivalentes
    ind <- (1:p)[G[,j]==1]
     ind[ind>j] <- ind[ind>j]-1
   */
   for (k=0; k<*p; k++) {
     //     if (G[k, j] ==1) {
     if (G[j* *p + k] ==1) {
       ind[d]=k;
       //  ind[ind>j] <- ind[ind>j]-1
      if (ind[d]>j)
	ind[d] --;
       d++;
     } // fin G=1
   } // fin k
   if (d==0) {
     // scrG$scr[j]= NormX[j]
     scrGscr[j] =  NormX[j];
   } else {
     if (d == 1) 
       l = ind[0]+1;
     else {
       //        l<-choose(p-1,d)-(p-1-ind[d])
       l = GGMchoose( pm1, d);
       l = l - (pm1-ind[d-1]-1);
       //  for (k in 1:(d-1)) l <- l-choose(p-1-ind[k],d-k+1)
       for (k=1; k< d; k++) {
	 ia = pm1 - ind[k-1] -1;
	 ib = d - k +1;
	 ll = GGMchoose( ia, ib);
	 l = l - ll;
       } // fin k
     } //fin else
     //           scrG$scr[j] <-SCR[[j]][[d]][l]
     SCRjd = VECTOR_ELT(SCRj, d-1); //SCR[[j]][[d]]
     SCRjdv = NUMERIC_POINTER(SCRjd);
     scrGscr[j] = SCRjdv[(int)l-1];
     //      scrG$d[j] <- d
     scrGd[j] = d;
   } // fin else

 } //fin for j


return(gscrG);
}
/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Remplacer la fonction calcCritQE de QE.R
  p <- length(scrG$d)
  crit <- rep(0,p)
  for (j in 1:p) {
    dmax <- dim(pen)[1]-1
    crit[j] <- scrG$scr[j]*(1+pen[scrG$d[j]+1,iK]/(n-scrG$d[j]))
  }	
  sumcrit <- sum(crit)
  return(sumcrit)
INPUT
   p: integer
   n: integer
   penrows: integer
   iK: integer
   pen: matrix with penrow rows= output from penalite 
   scr: vector with dimension p= output from calcSCRGQE
   d: integer vector with dimension p= output from calcSCRGQE
OUTPUT : 
   sumcrit: real scalar
CALLED BY
  GGMscrgcritQE
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

void GGMcritQE(int p, int n, int penrows, int iK,
	       double *pen, double *scr, int *d,
	       double *sumcrit)
{
  int j, GiK;

  GiK = iK-1; // indice from 0
  *sumcrit=0.0;
  for (j=0; j< p; j++) {

    //    *sumcrit += (scr[j]* (1.0 + pen[d[j]+1,iK]/(n-d[j])));
    *sumcrit += (scr[j]* (1.0 + 
			  pen[(GiK)*penrows + d[j]]/
			  (n-d[j])));

  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
Internal function
Calculer l'indice de ligne et de colonne qui correspond
a un indice de lower.tri
INPUT
   x : integer
   p : matrix dimension
OUTPUT
   icol, irow : indices a partir de 0
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMindice(int p, int x, int *icol, int *irow) {
  int borne, saut;
  borne = p-1;
  saut= borne-1;
  *icol=0;
  while (x>borne) {
    borne += saut;
    saut--;
    (*icol)++;
  }
  *irow= p - (borne-x) -1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
Remplacer l'enchainement des appels aux fonctions
 calcSCRGQE et calcCritQE de QE.R
INPUT
   n: integer
   p: integer
   iK: integer
   penrows: integer
   pen: matrix with penrows rows. Output from penalite
   G: adjacency matrix with dimension pxp (integer)
WORKING
   scrG: list with components :
      scr : vector with dimension p.
        scr[j]= Residual sum of squares associated to
              variable j of G
      d:  integer vector with dimension p.
        d[j] Degree of  variable j of G
 Should be allocated before the call
OUTPUT
   sumcrit: scalar
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

SEXP GGMscrgcritQE(SEXP list)
{
  int *n, *p, *iK, *penrows, *scrGd;
  double *sumcrit, *pen, *scrGscr;
  SEXP gscrG;
// unused SEXP scrG;
R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre

// Acces aux valeurs des arguments
  p = INTEGER(getListElement(list,"p"));
  n = INTEGER(getListElement(list,"n"));
penrows = INTEGER(getListElement(list,"penrows"));
iK = INTEGER(getListElement(list,"iK"));
  pen = NUMERIC_POINTER(getListElement(list,"pen"));
gscrG = getListElement(list,"scrG");
  scrGscr = NUMERIC_POINTER(getListElement(gscrG,"scr"));
  scrGd =INTEGER_POINTER(getListElement(gscrG,"d"));
  sumcrit = REAL(getListElement(list,"sumcrit"));

  // unused    scrG = GGMscrgQE( list);
    GGMcritQE((*p), (*n), (*penrows), (*iK),
	       pen, scrGscr, scrGd,
	      sumcrit);

    return(list);
} // fin GGMscrgcritQE


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
Compute the criterion and update minimum if needed
INPUT
   List with the required input
WORKING
   ind: integer vector  with dimension p
   G: adjacency matrix with dimension pxp (integer)
   scrG: list with components :
      scr : vector with dimension p.
        scr[j]= Residual sum of squares associated to
              variable j of G
      d:  integer vector with dimension p.
        d[j] Degree of  variable j of G
 Should be allocated before the call
OUTPUT
   critmin: scalar
   critargmin: integer vector with dimension dd
   ModTG: integer vector with dimension ll
   nModTG: integer scalar. Effective length of ModTG
CALLED BY
   The R function calcCritminQE
 ++++++++++++++++++++++++++++++++++++++++++++++++ */

SEXP GGMcritminQE(SEXP listarg)
{
  int i, j,l,  *p, *ll, *dd,  *nModTG;
  int *Dmax, *G, *Mod, *ModTG, *MatGetiK, *critargmin;
  int Nv, nvd, maxnvd, icol, irow;
  double *sumcrit, *critmin;

  // Duplicate the argument because it will be modified
  SEXP list = PROTECT(duplicate(listarg));

R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre

// Acces aux valeurs des arguments
  p = INTEGER(getListElement(list,"p"));
 // printf(" p=%d ", *p);

  //   ll: integer (number of models)
  ll = INTEGER(getListElement(list,"ll"));
// printf(" ll=%d ", *ll);

  dd = INTEGER(getListElement(list,"dd"));
  //   Dmax:  integer vector with dimension p. Maximum degrees
  Dmax = INTEGER_POINTER(getListElement(list,"Dmax"));
  G = INTEGER_POINTER(getListElement(list,"G"));
  //   Mod : integer matrix with dd rows 
  //         containing models with dimension dd
  Mod = INTEGER_POINTER(getListElement(list,"Mod"));
  //   MatGetiK: matrix with d rows containing models 
  // with dimension d for the value iK of K
  MatGetiK = INTEGER_POINTER(getListElement(list,"MatGetiK"));
  nModTG = INTEGER(getListElement(list,"nModTG"));
  ModTG = INTEGER_POINTER(getListElement(list,"ModTG"));
  critmin = REAL(getListElement(list,"critmin"));
  sumcrit = REAL(getListElement(list,"sumcrit"));
  critargmin = INTEGER_POINTER(getListElement(list,"critargmin"));

  *critmin= R_PosInf;
  *sumcrit= R_PosInf;
  *nModTG=0;

  for (l=0; l<(*ll); l++) {

    // Calcul de G
    /* Instructions R equivalentes:
    vect <- rep(0,lv)
    vect[Mod[,l]] <- 1
    G1 <- array(0,c(p,p))
    G1[lower.tri(G1)] <- vect
    G <- MatG.et[iK,,]+G1+t(G1)
    */

  for (j=0; j< (*p) ; j++) {
      for (i=0; i<(*p); i++) {
	//	G[i,j] = MatGetiK[i,j];
	G[(*p)*j+i] = MatGetiK[(*p)*j+i];
      } // fin i
    } // fin j
    for (i=0; i< (*dd); i++) {
      //      GGMindice((*p), Mod [i,l], &icol, &irow);
      GGMindice((*p), Mod [l* (*dd)+i], &icol, &irow);
      //      G[Mod [irow,icol] ]++;
      G[(*p)*icol+irow]++;
      //  G[Mod [icol, irow] ]++;
      G[(*p)*irow+icol]++;
    } // fin i
    //  Rque: G est mis a jour dans list puisque c'est un pointeur


    // Calcul de Nv
    /* Instructions R equivalentes:
    Nv <- apply(G, 2 , sum)
    */
    maxnvd = -1000; // Valeur negative quelconque pour initialisation
    for (j=0; j< (*p) ; j++) {
      Nv=0;
      for (i=0; i<(*p); i++) {
	//	Nv += 	G[i,j];
	Nv += 	G[j*(*p)+i];
      }
      //          printf(" Nv %d ", Nv);

      nvd = Nv - Dmax[j];
      if (nvd >= maxnvd) maxnvd=nvd;
    } // fin j
    /* maxnvd = (max(Nv-Dmax) */

    /*    # If at least one variable has more than Dmax neighbours,
	  # then don't consider  the new graph */
    if (maxnvd>0) {
      ModTG[*nModTG]= (l+1);
      (*nModTG)++;
      continue; // boucle suivante sur l
    }
  

    GGMscrgcritQE( list);
    // En sortie, list$sumcrit est modifie

    /* Instructions R equivalentes:
  critmin <- min(crit, na.rm=TRUE)
  critargmin <- Mod[,which.min(crit)]
    */

    if ((*sumcrit) <= (*critmin)) {
      (*critmin)=(*sumcrit);
      for (j=0; j< (*dd); j++) {
	critargmin[j]= Mod[l*(*dd)+j];
      }
    }

  } //fin l
  // Cas ou on est tjrs passé par le continue
  // ci-dessus, c.a.d  maxnvd>0 pour tout l
  if ( !R_FINITE(*critmin)) {
    for (j=0; j< (*dd); j++) {
      // 	critargmin[j]=Mod[j, 0]
	critargmin[j]= Mod[j];
    }
  }
UNPROTECT(1);
  return(list);
} // fin fonction
