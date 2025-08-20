/* ---------------------------------------------------------------
  GGMselect R package
  Copyright INRA 2017
  INRA, UR1404, Research Unit MaIAGE
  F78352 Jouy-en-Josas, France.
 
  URL: http://genome.jouy.inra.fr/logiciels/GGMselect
-------------------------------------------------------------- */

#include <math.h>
#include <Rmath.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Utils.h> // pour permettre d'interrompre

/* +++++++++++++++++++++++++++++++++++++++++++++ */
extern void GGMcalcProjInd(double *V, double *y, 
		    int *n, int *d, double *minvp,
		    int *iwork, double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
			   double *Proj);


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
  Write a warning message when some penalities are Inf
INPUT
  family: name of the family
CALLED BY
  GGMloop*
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMmesserr(char *family) {
    warning("Some values of penalty are greater than 1e+08: the criterion have been set to Inf at least once during the process (family %s).", family);
}



/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
  Remplacer la fonction is.element de R
INPUT
  n: integer
  a: integer
  vecteur: vector n
OUTPUT
  1 if a in vecteur, 0 else
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
int GGMiselement(int n, int a, int *vecteur) {
  int i;
  for (i=0; i<n; i++) {
    if (a == vecteur[i]) 
      return(1);
	} //fin i
  return(0);
} // fin fonction


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
  Remplacer dans les GGMNvGraph*, les commandes R suivantes:
    NVoisGr[i1]=NVoisGr[i1]-1
    Gr[i1,Gr[i1,]==-b] <- 0
    if (NVoisGr[i1]>0) {
      Gr[i1,] <- c(Gr[i1,Gr[i1,]!=0],
                  rep(0,length(Gr[i1,])-NVoisGr[i1]))
    }
INPUT
  nrowGr: integer
  ncolGr: integer
  i1:  integer (index from zero)
  b: integer
  NVoisGr: vector integer
INPUT/OUTPUT
  Gr: matrix nrowGr, ncolGr
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMupdateGr0(int nrowGr, int ncolGr, 
		  int i1, int b, int *Gr,
		  int *NVoisGr) {

  int l, il=0;


  NVoisGr[i1]--;
  // VOIR SH
  NVoisGr[i1]= imax2(0,  NVoisGr[i1]);


  for (l=0; l <ncolGr; l++) {
    //    if (Gr[i1,l]  == -b) {
    if (Gr[nrowGr*l+i1]  == -b) {
      //      Gr[i1,l]  = 0;
      Gr[nrowGr*l+i1]  = 0;
    }
    if (NVoisGr[i1] >0) {
      //      if (Gr[i1,l] !=0) {
      if (Gr[nrowGr*l+i1] !=0) {
	//Gr[i1,il] = Gr[i1, l];
	Gr[il * nrowGr+ i1 ] = Gr[l * nrowGr+ i1 ];
	il++;
      } // fin (Gr[i1,l] !=0)
    } // fin (NvoisGr[i1] >0)
  } // fin l
  if (NVoisGr[i1] >0) {
    for (l=il; l <ncolGr; l++) {
      //  Graph[i1,l] =0
    Gr[l * nrowGr +i1] =0;
    }
  }
} // fin GGMupdateGr0



/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
   Update Gr and Graph (method LA)
INPUT
  a,b: integers not larger than p
  p: integer
  ncolGr: integer
  ncolGraph: integer
WORKING
  iwork: vector (ncolGr)
INPUT/OUTPUT
  NVoisGr: p dim. vector
  NVoisGraph: p dim. vector
  Gr: p x min(n,p-1) array
  NVoisGraph: p dim. vector
  Graph: p x max(Dmax) matrix
OUTPUT
   NvGraph: 0 or 1 (a new graph created)
 Should be allocated before the call
CALLED BY
  GGMloopAND
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMNvGraphAND(  int a, int b, int p, 
		  int ncolGr, int ncolGraph,
		  int *iwork, int *NVoisGr, int *NVoisGraph,
		  int *Gr, int *Graph, int *NvGraph) {
  int iia, iib, k;
  *NvGraph =0;
    iia = a-1; // indice a partir de 0
  // if b>0, add an edge when needed
  if (b>0) {
    iib=b-1;// indice a partir de 0
    // update Gr
    //    Gr[a,NVoisGr[a]] <- b
    // Incrementation de NVoisGr apres la modif de Gr=
    // indicage a partir de zero
    Gr[NVoisGr[iia]* p + iia] = b;
    NVoisGr[iia]++;
    // update Graph if a in Gr[b,]
    for (k=0; k<ncolGr; k++)
      iwork[k] = Gr[ k* p + b -1];

    if (GGMiselement(ncolGr, a, iwork)==1) {
      *NvGraph = 1;
      //      Graph[a,NVoisGraph[a]] <- b
      // Incrementation de NVoisGr apres la modif de Gr=
    // indicage a partir de zero

      Graph[NVoisGraph[iia] * p + iia] = b;
      //      Graph[b,NVoisGraph[b]] <- a
      Graph[NVoisGraph[iib] * p + iib] = a;
      NVoisGraph[iia]++;
      NVoisGraph[iib]++;

    } // fin iselement
  } else {
    // b <=0
    // if b<0 remove the edge (if exists)
    // update Gr
    /* Les instructions R suivantes:
     NVoisGr[a] = NVoisGr[a]-1
    Gr[a,Gr[a,]==-b] <- 0
    if (NVoisGr[a]>0) {
      Gr[a,] <- c(Gr[a,Gr[a,]!=0],
                  rep(0,length(Gr[a,])-NVoisGr[a]))
    se traduisent par les 2 phrases suivantes:
*/
   iib=(-b)-1;// indice a partir de 0
    GGMupdateGr0 (p, ncolGr, iia, b, Gr, NVoisGr);

    // update Graph if a-b in Graph
   /* Les instructions R suivantes:
    if (is.element(a,Gr[-b,])) {
      Nouv.Gr <- 1
      DMAX <- max(Dmax)
      NVoisGraph[a] = NVoisGraph[a]-1
      NVoisGraph[-b] = NVoisGraph[-b]-1
      # update Graph[a,]
      Graph[a,Graph[a,]==-b] <- 0
      if (NVoisGraph[a]>0) {
        Graph[a,] <- c(Graph[a,Graph[a,]!=0],
                       rep(0,DMAX-NVoisGraph[a]))
      }
      # update Graph[-b,]
      Graph[-b,Graph[-b,]==a] <- 0
      if (NVoisGraph[-b]>0) {                                          
        Graph[-b,] <- c(Graph[-b,Graph[-b,]!=0],
                       rep(0,DMAX-NVoisGraph[-b]))
      }
    }
   se traduisent par les  phrases suivantes:
*/
    for (k=0; k<ncolGr; k++)
      iwork[k] = Gr[ k* p + iib];
    if (GGMiselement(ncolGr, a, iwork)==1) {
      *NvGraph = 1;
      // update Graph[a,]
      GGMupdateGr0 (p, ncolGraph, iia, b, Graph, NVoisGraph);
      // update Graph[-b,]
      GGMupdateGr0 (p, ncolGraph, iib, (-a), Graph, NVoisGraph);
    } // fin (GGMiselement(ncolGr, a, iwork))

  } // fin b <=0

} // fin GGMNvGraphAND


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
   Update Gr and Graph (method EW)
INPUT
  a,b: integers not larger than p
  p: integer
  ncolGr: integer
  ncolGraph: integer
INPUT/OUTPUT
  NVoisGr: p dim. vector
  NVoisGraph: p dim. vector
  Gr: p x min(n,p-1) array
  NVoisGraph: p dim. vector
  Graph: p x max(Dmax) matrix
 Should be allocated before the call
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMNvGraphEWOR(int a, int b, int p, 
		  int ncolGr, int ncolGraph,
		  int *NVoisGr, int *NVoisGraph,
		  int *Gr, int *Graph) {
  int iia, iib;
    iia = a-1; // indice a partir de 0
  // if b>0, add an edge when needed
  if (b>0) {
    iib=b-1;// indice a partir de 0
    // update Gr
    //    Gr[a,NVoisGr[a]] <- b
      // Incrementation de NVoisGr apres la modif de Gr=
    // indicage a partir de zero
    Gr[NVoisGr[iia]* p + iia] = b;
    // Graph[a,NVoisGraph[a]] <- b
    Graph[NVoisGraph[iia]* p + iia] = b;
    // Graph[b,NVoisGraph[b]] <- a
    Graph[NVoisGraph[iib]* p + iib] = a;
    // NVoisGr[a] = NVoisGr[a]+1
    NVoisGr[iia]++;
    // NVoisGraph[a] = NVoisGraph[a]+1
    NVoisGraph[iia]++;
    // NVoisGraph[b] = NVoisGraph[b]+1
    NVoisGraph[iib]++;
  } // fin (b>0)
  else {
    // if b<0 we remove the edge a-b
    //    NVoisGr[a] = NVoisGr[a]-1
    //Gr[a,Gr[a,]==-b] <- 0
    //  if (NVoisGr[a]>0){
    //      Gr[a,]<-c(Gr[a,Gr[a,]!=0],rep(0,length(Gr[a,])-NVoisGr[a]))
    //    }
    iib=(-b)-1;// indice a partir de 0
    GGMupdateGr0 (p, ncolGr, iia, b, Gr, NVoisGr);
    // NVoisGraph[a] = NVoisGraph[a]-1
    // Graph[a,Graph[a,]==-b] <- 0
    // if (NVoisGraph[a]>0){
    //      Graph[a,]<-c(Graph[a,Graph[a,]!=0],rep(0,Dmaxmax-NVoisGraph[a]))
    //    }
    GGMupdateGr0 (p, ncolGraph, iia, b, Graph, NVoisGraph);
	// NVoisGraph[-b] = NVoisGraph[-b]-1
	// Graph[-b,Graph[-b,]==a] <- 0
	//  if (NVoisGraph[-b]>0){
	//       Graph[-b,]<-c(Graph[-b,Graph[-b,]!=0],rep(0,Dmaxmax-NVoisGraph[-b]))
	//     }
    GGMupdateGr0 (p, ncolGraph, iib, (-a), Graph, NVoisGraph);
  } // fin b <=0

} // fin GGMNvGraphEWOR



 
/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
  Compute the criterion and update minimum if needed
 (methods LA and EW)
 INPUT
   n: integer
   p: integer
   lK: integer
   ncolGraph: integer
   Dmaxmax: integer
   scr: p dim. vector
   pen: max(Dmax+1) x lK array
   Graph: p x max(Dmax) matrix
   NVoisGraph: p dim. vector
INPUT/OUTPUT
   critmin: lk dim. vector
   Neighb: array  p x  max(Dmax) x  lK
RETURN
   an integer non null if error
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMGrMin( int *n, int *p, int *lK, int *ncolGraph,
	       int *Dmaxmax, double *scr, double *pen, int *Graph,
	       int *NVoisGraph, 
	      double *critmin, int *Neighb, int *err) {
  int i,iK,l, indice;
  double crit;
  *err=0; // code d'erreur

  for (iK=0; iK< *lK; iK++) {
    crit=0.0;
    for (i=0; i< *p; i++) {
      //      crit += (scr[i] * (1+pen[NVoisGraph[i]+1, iK]/
      //			 (n - NVoisGraph[i])));
      indice = iK* ((*Dmaxmax) +1) + NVoisGraph[i];
      crit += (scr[i] * (1+pen[indice]/
			 ((*n) - NVoisGraph[i])));

    } // fin i

    // The values of pen greater than 1e+08 are set to Inf by the
    // R function calcEDkhi. The calculated criterion is then inf

    if (!R_FINITE(crit)) {
      *err=2;
    }

    // Ne mettre a jour les sorties, que si le critere est inf
    // a critmin
    if (crit < critmin[iK]) {
      critmin[iK] = crit;
      for (i=0; i< *p; i++) {

	for (l=0; l < *ncolGraph; l++) {
	  //	  Neighb[i,l,iK] = Graph[i,l];
	  Neighb[(*p)*(*Dmaxmax)*iK + (*p)*l +i] = Graph[l*(*p)+i];
	}
      }
    } // fin (crit < critmin[iK])
  } // fin iK
} // fin GGMGrMin


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
  Gerer l'appel a GGMcalcProjInd pour une valeur de ia
INPUT/OUTPUT
  scr[ia]

 ++++++++++++++++++++++++++++++++++++++++++++++++ */

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
		    double *Pr) {
  int i, l, ii, iia, ind;


R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre
  iia = (*ia)-1; // indice a partir de zero

  if (NVois[iia] ==0) {
    scr[iia] = sumX2[iia];
  }    else {



  // Mettre les colonnes dont les indices sont dans ind dans work
 ii=0;
  for (l=0; l <NVois[iia]; l++) {
    // Mettre dans ind Graph[ia, 1:NVois[ia]]
      //      ind = Graph[ia,l];

      ind = Graph[l*(*p)+iia]-1;
      
      for (i=0; i< *n; i++) {
      //      work[ii++] = X[i, (ind-1)];
	work[ii] = X[(*n) * ind +i]; 
     ii++;
      }
  } // fin l


  GGMcalcProjInd(work,
		 &(X[iia * (*n)]),
		   n,  &NVois[iia],  minvp, iwork,
		   svdMd, r1, W1, M, W2, W3, W4,
		   vu, svdMv, xvals, Pr);


    scr[iia] =0.0;
    for (i=0; i< *n; i++) {
      //      scr[iia] += ((X[i, iia] - Pr[i])*(X[i, iia] - Pr[i]));
      scr[iia] += ((X[ iia * (*n)+i] - Pr[i])*(X[ iia * (*n)+i] - Pr[i]));
    }
 
  } // fin else

} // fin GGMSCRa


/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
  Compute the residual squares associated to the input graph
 (methods LA et EW)
INPUT
   a,b: integer scalars not larger than p
   n: integer
   p: integer
   X: n x p matrix
   minvp: positive real number
   NVois: p dim. vector
   sumX2: vector (p) 
   Graph: p x max(Dmax) matrix
   scr
WORKING
 With d=max(NVois),
   iwork: vector (d) integer
   work:  matrix (n, d)
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
   Pr: n dim. vector
INPUT/OUTPUT
   scr: p dim. vector (residual squares for each node)
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMSCR(int a, int b, int n, int p,
	    double *X, double minvp,
		 int *NVois, double *sumX2,
	  int *Graph,double *scr,
	    int *iwork, double *work,  
	    double *svdMd, double *r1,
		    double  *W1, double *M,
		    double  *W2, double  *W3, double  *W4,
		    double *vu, double *svdMv,
		    double *xvals,
		    double *Pr) {


  GGMSCRa(&a,  &n, &p, X, &minvp,
		      NVois,sumX2, Graph, scr,
	     iwork,work,  svdMd, r1, W1, M, W2, W3,W4,
	    vu, svdMv,  xvals, Pr);


     GGMSCRa(&b,  &n, &p, X, &minvp,
		      NVois,sumX2, Graph, scr,
		      iwork,work, svdMd, r1, W1, M, W2, W3,W4,
	    vu, svdMv,  xvals, Pr);



}





/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Loop on the rows of GrGlob (method LA)
INPUT
   n: integer
   p: integer
   lK: integer
   nrowGrGlob: integer
   ncolGrGlob: integer
   GrGlob: matrix with dimension nrowGrGlob x ncolGrGlob (integer)
           output from calcModLasso
   Dmax:  vector
   minvp: (small) positive real number
   X: matrix n x p 
   sumX2: vector (p) 
   pen: matrix max(Dmax) x lK  (output from penalte)
   ncolGr: positive integer number
   ncolGraph: positive integer number
WORKING
   NVoisGraph: vector p
   NVoisGr: vector p
   Graph: matrix p  x ncolGraph
   Gr: matrix p  x ncolGr
   scr: vector p (residual squares for each node)
   iwork: vector p 
   other: see GGMSCR
OUTPUT : 
   critmin: vector lK
   Neighb:  array with dimension p, Dmaxmax, lK
 Should be allocated before the call
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
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
	       double *critmin, int *Neighb) {

  int i,a,b, absb, NvGraph;

R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre

 int err =0; //Retour de GGMGrMin

  for (i=0; i< *nrowGrGlob; i++) {
    //    a=GrGlob[i,1]; b=GrGlob[i,2];
    NvGraph=0;
    a = GrGlob[i];
    b = GrGlob[*nrowGrGlob+i];

    if (b >0) {
      if ((NVoisGraph[a-1]>=Dmax[a-1]) ||
	  (NVoisGraph[b-1]>=Dmax[b-1]))
      break; // sortie de la boucle i
    }
    
    GGMNvGraphAND( a, b, (*p), (*ncolGr), (*ncolGraph),
		  iwork, NVoisGr, NVoisGraph,
		  Gr, Graph, &NvGraph);

    if (NvGraph == 1)  {
      absb = abs(b);

      GGMSCR(a, absb, (*n), (*p),
	     X, (*minvp), NVoisGraph, sumX2,
	     Graph,scr, iwork, work,  svdMd, r1,
	     W1, M, W2, W3, W4, vu, svdMv,
	     xvals,  Pr);
      GGMGrMin( n, p, lK, ncolGraph,  Dmaxmax, scr,
		pen, Graph, NVoisGraph, critmin, Neighb, &err);

    } // fin (NvGraph == 1) 
  } // fin i
  if (err !=0) {
    GGMmesserr("LA");
  }

  } // fin GGMloopAND




/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Loop on the rows of GrGlob (method EW)
INPUT
   n: integer
   p: integer
   lK: integer
   nrowGrGlob: integer
   ncolGrGlob: integer
   GrGlob: matrix with dimension nrowGrGlob x ncolGrGlob (integer)
           output from calcModLasso
   Dmax:  vector
   minvp: (small) positive real number
   X: matrix n x p 
   sumX2: vector (p) 
   pen: matrix max(Dmax) x lK  (output from penalte)
   ncolGr: positive integer number
   ncolGraph: positive integer number
WORKING
   NVoisGraph: vector p
   NVoisGr: vector p
   Graph: matrix p  x ncolGraph
   Gr: matrix p  x ncolGr
   scr: vector p (residual squares for each node)
   iwork: vector p 
   ind to Pr: see GGMSCR
OUTPUT : 
   critmin: vector lK
   Neighb:  array with dimension p, Dmaxmax, lK
 Should be allocated before the call
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
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
	       double *critmin, int *Neighb) {

  int i,k, a,b, absb;
R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre
 int err =0; //Retour de GGMGrMin

  for (i=0; i< *nrowGrGlob; i++) {
    //    a=GrGlob[i,1]; b=GrGlob[i,2];
    a = GrGlob[i];
    b = GrGlob[(*nrowGrGlob)+i];
    //   if ((b>0)&(is.element(a,Gr[abs(b),])))
    for (k=0; k< (*ncolGr); k++)
      iwork[k] = Gr[ k* (*p) + abs(b) -1];
    if (GGMiselement((*ncolGr), a, iwork)==1) {
      if (b>0) {
	// if b>0 and a-b already exists, update Gr but not Graph
	// NVoisGr[a] <- NVoisGr[a]+1
	// Gr[a,NVoisGr[a]] <- b
	// On incremente NVoisGr apres la modif de Gr:
	// l'indice est bien a partir de zero
	Gr[(*p)*NVoisGr[a-1] + (a-1)] = b;
	NVoisGr[a-1]++;
	continue;
      } // fin b>0
      else {
	// if b<0 and also a in Gr[-b,] update Gr but not Graph

	//  ((b<0)&(is.element(a,Gr[abs(b),])))
	//  NVoisGr[a]<-NVoisGr[a]-1
	// Gr[a,Gr[a,]==-b] <- 0
	//                   if (NVoisGr[a]>0){
	//                     Gr[a,]<-c(Gr[a,Gr[a,]!=0],rep(0,length(Gr[a,])-NVoisGr[a]))
	//                   }
	GGMupdateGr0 ((*p), (*ncolGr), (a-1), b, Gr, NVoisGr);
	continue;
      } //fin else
    } // fin 	GGMiselement
    if (b>0) {
      // stopping rule: 
      //it stops when it tries to add an edge to a node with degree Dmax

      //  if ((NVoisGraph[a]>=Dmax[a])|(NVoisGraph[b]>=Dmax[b])) break
      if ((NVoisGraph[a-1]>=Dmax[a-1]) ||
	  (NVoisGraph[b-1]>=Dmax[b-1])) 
	break; // sortie de la boucle i
    } 
    GGMNvGraphEWOR(a, b, (*p), (*ncolGr), (*ncolGraph),
		  NVoisGr, NVoisGraph,
		  Gr, Graph);

      absb = abs(b);
      GGMSCR(a, absb, (*n), (*p),
	     X, (*minvp), NVoisGraph, sumX2,
	     Graph,scr, iwork, work,  svdMd, r1,
	     W1, M, W2, W3, W4, vu, svdMv,
	     xvals,  Pr);
      GGMGrMin(n, p, lK, ncolGraph, Dmaxmax, scr,
	       pen, Graph, NVoisGraph, critmin, Neighb, &err);
  
    } // fin i
  if (err !=0)
    GGMmesserr("EW");

  } // fin GGMloopEWOR
/* ++++++++++++++++++++++++++++++++++++++++++++++++
FUNCTION
 Replace in calcC01 the following R commands:
 for (i in 1:dim(GrGlob)[1]) {
    # K:  boucle de taille jusqu'a  p*(p-1)/2
    a <- GrGlob[i,1]
    b <- GrGlob[i,2]
    # Stopping rule: stops if it tries to add an edge to a node with degree more than Dmax
    if  ((NVoisGraph[a]>=Dmax[a])|(NVoisGraph[b]>=Dmax[b]))  break
    # add the edge a-b
    NVoisGraph[a] <- NVoisGraph[a]+1
    Graph[a,NVoisGraph[a]] <- b
    NVoisGraph[b] <- NVoisGraph[b]+1
    Graph[b,NVoisGraph[b]] <- a
    scr <- calcSCR(X,a,abs(b),scr,Graph,NVoisGraph,min.vp)
    res <- calcGrMin(scr,n,Graph,NVoisGraph,pen,lK,crit.min,Neighb)
    # update if necessary
    if (res$NvNeighb == 1){
      crit.min <- res$crit.min
      Neighb <- res$Neighb
    }
  }

INPUT
   n: integer
   p: integer
   lK: integer
   nrowGrGlob: integer
   ncolGrGlob: integer
   GrGlob: matrix with dimension nrowGrGlob x ncolGrGlob (integer)
           output from calcModLasso
   Dmax:  vector
   minvp: (small) positive real number
   X: matrix n x p 
   sumX2: vector (p) 
   pen: matrix max(Dmax) x lK  (output from penalte)
   ncolGraph: positive integer number
   NVoisGraph: vector p
   Graph: matrix p  x ncolGraph
WORKING
   scr: vector p (residual squares for each node)
   iwork: vector p 
   other: see GGMSCR
OUTPUT : 
   critmin: vector lK
   Neighb:  array with dimension p, Dmaxmax, lK
 Should be allocated before the call
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
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
	       double *critmin, int *Neighb) {

  int i,a,b, absb, iia, iib;

R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre
 int err =0; //Retour de GGMGrMin

  for (i=0; i <*nrowGrGlob; i++) {
    //   a <- GrGlob[i,1]
    a= GrGlob[i]; 
    // b <- GrGlob[i,2]
    b = GrGlob[*nrowGrGlob+i];
    iia=a-1;
    iib=b-1;


    // Stopping rule: stops if it tries to add an edge to a node with degree more than Dmax
    // if  ((NVoisGraph[a]>=Dmax[a])|(NVoisGraph[b]>=Dmax[b]))  break
    if  ((NVoisGraph[iia]>=Dmax[iia])||(NVoisGraph[iib]>=Dmax[iib])) 
      break; // sortir de la boucle sur i
    // add the edge a-b
    // Graph[a,NVoisGraph[a]] <- b
	// On incremente NVoisGr apres la modif de Gr:
	// l'indice est bien a partir de zero
    Graph[NVoisGraph[iia] * (*p) + iia]=b;
    NVoisGraph[iia]++;
    //Graph[b,NVoisGraph[b]] <- a
    Graph[NVoisGraph[iib] * (*p) + iib]=a;
    NVoisGraph[iib]++;
    absb = abs(b);


    GGMSCR(a, absb, (*n), (*p),
	     X, (*minvp), NVoisGraph, sumX2,
	     Graph,scr, iwork, work,  svdMd, r1,
	     W1, M, W2, W3, W4, vu, svdMv,
	     xvals,  Pr);

    GGMGrMin(n, p, lK, ncolGraph, Dmaxmax, scr,
	     pen, Graph, NVoisGraph, critmin, Neighb, &err);
      //  GGMGrMin modifie critmin et Neighb que si necessaire
  } // fin i

  if (err !=0)
    GGMmesserr("C01");

} // fin loopC01
/* ++++++++++++++++++++++++++++++++++++++++++++++++
 FUNCTION
  Remplacer dans calcModC01,  les commandes R suivantes:
for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      l <- l+1
      ind.k <- (1:p)[-c(i,j)]
      num <- rho[i,j] - rho[i,ind.k]*rho[j,ind.k]
      den <- sqrt((1-rho[i,ind.k]**2)*(1-rho[j,ind.k]**2))
      Z1 <- sqrt(n-3)*abs(log((1+rho[i,j])/(1-rho[i,j])))/2 # C0
      Z2 <- sqrt(n-4)*abs(log((1+num/den)/(1-num/den)))/2 #C1
      phi[l,] <- c(i,j,min(abs(c(Z1,Z2))))
    }
  }

INPUT
   n: integer
   p: integer
   nrowphi:  integer
   rho: matrix p x p 
OUTPUT
   phi: p*(p-1)/2 x 3 array
 Should be allocated before the call
 ++++++++++++++++++++++++++++++++++++++++++++++++ */
void GGMModC01(int *n, int *p, int *nrowphi, double *rho,
	       double *phi) {
  int i,j,l,k;
  double minZ, Z1, Z2, den, num;
R_CheckUserInterrupt(); // permettre a l'utilisateur d'interrompre

  l=0;
  for (i=0; i<(*p-1) ; i++) {
    for (j= (i+1); j < *p; j++) {
      minZ=DBL_MAX ; // dans float.h
      for (k=0; k<*p;k++) {
	if ((k==i) || (k==j))
	  continue; // boucle suivante sur k
	//	num  <- rho[i,j] - rho[i,ind.k]*rho[j,ind.k]
	num = rho[j* (*p)+i] - rho[k*(*p)+i]*rho[k*(*p)+j];
	// den <- sqrt((1-rho[i,ind.k]**2)*(1-rho[j,ind.k]**2))
	den = sqrt((1-pow(rho[k*(*p)+i],2))*(1-pow(rho[k*(*p)+j],2)));

	// Z1 <- sqrt(n-3)*abs(log((1+rho[i,j])/(1-rho[i,j])))/2 # C0
	Z1 = sqrt(*n-3)*fabs(log((1+rho[j* (*p)+i])/(1-rho[j* (*p)+i])))/2;
	//  Z2 <- sqrt(n-4)*abs(log((1+num/den)/(1-num/den)))/2 #C1
	Z2 =  sqrt(*n-4)*fabs(log((1+num/den)/(1-num/den)))/2;
	Z1 = fabs(Z1);
	Z2 = fabs(Z2);
	if (Z1 <= minZ)  minZ=Z1;
	if (Z2 <= minZ)  minZ=Z2;
      } // fin k
      // phi[l,] <- c(i,j,min(abs(c(Z1,Z2))))
      phi[l] = i+1;
      phi[*nrowphi +l ] =j+1;
      phi[*nrowphi*2 +l ] =minZ;
      l++;
    } //fin j
  } //fin i

} //fin ModC01









