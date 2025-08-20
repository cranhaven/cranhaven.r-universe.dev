 /* ---------------------------------------------------------------
  GGMselect R package
  Copyright INRA 2017
  INRA, UR1404, Research Unit MaIAGE
  F78352 Jouy-en-Josas, France.
 
  URL: http://genome.jouy.inra.fr/logiciels/GGMselect
-------------------------------------------------------------- */


/* ++++++++++++++ QESW +++++++++++++++++++ */
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Utils.h> // pour permettre l'interruption de l'exec

// Copyright@INRA-2009
extern SEXP getListElement(SEXP list, const char *str);
extern SEXP GGMscrgcritQE(SEXP list);

/* ............ Fonctions MIN  ........................*/
#define MIN(a,b) ( (a)<(b) ? (a) : (b))

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Return the indices i,j of a R  matrix (p,p) which
 correspond to the index V (from 0) in C
INPUT
 V: integer scalar= index from 0
 p: integer scalar= number of rows and columns
OUTPUT
 i: integer scalar= index from 1 
 j: integer scalar= index from 1 
++++++++++++++++++++++++++++++++++++++++++++++++ */

void quelind(int V, int p, int *i, int *j) {
  div_t d;
 d = div((V), p);
 *j= d.quot+1;
 *i = d.rem+1;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Return the index of the transposed element of the element i
 in a matrix (p,p)
INPUT
 p: integer scalar= number of rows and columns
 i: integer scalar= index from 0 
OUTPUT
    integer scalar= index from 0 
++++++++++++++++++++++++++++++++++++++++++++++++ */
int transposeIndex (int p, int i) {
  int ii,jj;
 quelind(i, p, &ii, &jj);
 return((ii-1)*p + (jj-1));
 // return(jj*p+ii);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Return the index of the element which corresponds to the
 element V in the lower triangle (diagonal not included)
 of a matrix (p,p)
INPUT
 p: integer scalar= number of rows and columns
 V: integer scalar= index from 1
OUTPUT
    integer scalar= index from 0 
++++++++++++++++++++++++++++++++++++++++++++++++ */
int convMod (int p, int V) {
  int i=1, ind=1, K=p, saut=1;
  while (V > ind) {
    ind++;
    i++;
    if (ind >=K) {
      saut++;
      i += saut;
      K += (p-saut);
    }
  } // fin while

  return(i);
} // fin convMod


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Remplace calcCritFB:
  ll <- length(Mod)   # nombre de modeles
  crit <- c(Inf)
  G1 <- array(0,c(z$p, z$p))
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  for (l in 1:ll) {
    G1[] <- 0
    G1[lower.tri(G1)][Mod[l]] <- 1
    G <- Mat.Chap+G1+t(G1)
    if (dir=="F") {
      Nv <- apply(G, 2 , sum)
      
      
    # If at least one variable has more than Dmax neighbours,
    # then don't consider  the new graph
      if (max(Nv-z$Dmax) >0 ) {
        cat("Function passer au l",l,"max=",max(Nv),
            "Dmax", z$Dmax,"\n")
        next
      }
    }
      z$G <- as.integer(G)
      crit[l] <- .Call("GGMscrgcritQE", z)$sumcrit
    
  } # fin l
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  critmin <- min(crit, na.rm=TRUE)
  
  critargmin <- Mod[which.min(crit)]

INPUT
 dir: integer scalar (if 1 forward) else backward
 ll: integer scalar= length(Mod)
 Mod: integer vector(ll)
 MatChap:  adjacency pxp matrix of the current graph
 list: list with all other required quantities
OUTPUT
 critmin: scalar= minimum value of the criteria over 
        all models in Mod
 argmin: integer scalar=  model for which the minimum is attained
++++++++++++++++++++++++++++++++++++++++++++++++ */

SEXP critFB(int dir, int ll, int *Mod, int *MatChap,
	       SEXP list,
	       double *critmin, int *argmin) {
  int i,l, j, lsuiv, sumlig, lmin;
  int *p, *Dmax, *G;
  double *sumcrit;

  /* Acces aux arguments */
  p = INTEGER(getListElement(list,"p"));
  G = INTEGER_POINTER(getListElement(list,"G"));
  Dmax = INTEGER_POINTER(getListElement(list,"Dmax"));
  

  lmin=-1;
  *critmin=DBL_MAX; // dans float.h


  for (l=0; l< ll; l++) {
    for (i=0;i<(*p) * (*p);i++)
      G[i] = MatChap[i];

    i= convMod ( *p,  Mod[l]);

    G[i]++;


    G[transposeIndex(*p, i)]++;
    if (dir == 1) { //F
      lsuiv =0;
      for (j=0; j<*p; j++) {
	sumlig=0;

	for (i=0;i<*p;i++) {
	  //	  sumlig += G[i,j];
	  sumlig += G[j * (*p) +i];
	  // Dmax est un vecteur de longueur p
	  /*
    # If at least one variable has more than Dmax neighbours,
    # then don't consider  the new graph
	  */
	  if (sumlig > Dmax[i]) {
	    // passer au l suivant
	    lsuiv =1;
	    break;
	  }
	} // fin i
	if (lsuiv ==1) break;
      } // fin j

      if (lsuiv ==1) continue; // passer au l suivant
    } // fin dir

    //G est mis a jour dans z
    list=GGMscrgcritQE( list);
    sumcrit = REAL(getListElement(list,"sumcrit"));

    if (*sumcrit <= *critmin) {
      *critmin= *sumcrit;
      lmin = l;
    }
  } // fin l

  *argmin = Mod[lmin];
  return(list);

}



/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Function for GGMbcSW
Replace the R commandes:
#      Gdiff <- mat1-mat2
#      v <- GdiffF[lower.tri(GdiffF)]
#      vect <- (1:length(v))[v==1]
#      lgvect <- length(vect)
INPUT
 mat1:  integer matrix (p,p)
 mat2:  integer matrix (p,p)
 p: integer
OUTPUT
 vect: integer vector of length (p*(p+1)/2) - p
   Allocated before the call
 lgvect: effective length of vect
++++++++++++++++++++++++++++++++++++++++++++++++ */
void calcVect(int *mat1, int * mat2, int *p,
	     int *vect, int *lgvect)
{
  int k=0, j=1, i=1, icol;
  div_t d;


  for (icol=1; icol <*p; icol++) {
    d = div(i, *p);
    while (d.rem !=0) {

      if ( (mat1[i] - mat2[i])==1) {
	vect[k++] = j;
      }
      j++;
      i++;
      d = div(i, *p);
    } // fin while
    i += (icol+1);
  } // fin icol
  *lgvect = k; // nbre d'elts utiles de vect
} //fin calcVect

/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Stepwise procedure
INPUT
 MatMax:  integer matrix (p,p)
 MatMin:  integer matrix (p,p)
 p: integer
 Imax:  integer
INPUT-OUTPUT
 ListOut: list which contains:
 MatChap:  integer matrix (p,p)
 critmin: scalar= minimum criterion
WORKING
 z : list with the other required quantities
 indF, indB : integer working arrays, of length >= (p*(p+1))/2 -p
CALLED BY
  The R function calcSW
++++++++++++++++++++++++++++++++++++++++++++++++ */
SEXP GGMbcSW(SEXP xMatMax, SEXP xMatMin, 
	     SEXP xp,  SEXP xImax,
	     SEXP xListOut,
	     SEXP z, SEXP xindF, SEXP xindB)
{
  int mm=0, i,  un=1, zero=0, dF, dB, *p, *Imax;
  int *MatChap, *MatMax, *MatMin, *indF, *indB;
  int  argminF, argminB;
  double *critmin;
  double critminF, critminB;
R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre

  MatChap = INTEGER_POINTER(getListElement(xListOut,"MatChap"));
  critmin = NUMERIC_POINTER(getListElement(xListOut,"critmin"));
  MatMax  = INTEGER_POINTER(xMatMax);
  MatMin  = INTEGER_POINTER(xMatMin);
  p  = INTEGER_POINTER(xp);
  Imax  = INTEGER_POINTER(xImax);
  indF  = INTEGER_POINTER(xindF);
  indB  = INTEGER_POINTER(xindB);

  while (mm <=*Imax) {
    mm++;
    critminF = DBL_MAX;
    critminB = DBL_MAX;

    calcVect(MatMax, MatChap, p, indF, &dF);
    if (dF !=0) {
	critFB(un, dF, indF, MatChap, z, &critminF, &argminF);
    } // fin dF!=0
    calcVect(MatChap, MatMin, p, indB, &dB);

    if (dB !=0) {
	critFB(zero, dB, indB, MatChap, z, &critminB, &argminB);
    } // fin dB!=0

    if (*critmin <= MIN(critminF, critminB)) {
      break; // sortie du while
    }
    if (critminF <= critminB) {
	i= convMod(*p, argminF);

	MatChap[i]++;
	MatChap[transposeIndex (*p, i)]++;
	*critmin = critminF;
      } // fin (critminF < critminB)
      else {
	i= convMod(*p, argminB);

	MatChap[i]--;
	MatChap[transposeIndex (*p, i)]--;
	*critmin = critminB;
      } // fin else (critminF < critminB)
	  } // fin while
	  
	  if (mm > *Imax) 
      warning(
" *** calcSW (QE method): Maximum number of iterations reached: %d\n",
mm);
	  return(xListOut);
} // fin GGMbcSW
	  




