#include <R.h>
#include <Rmath.h>

/* EntropyMCMC package 
   Entropy estimate using Nearest Neighbor (NN) 
   from Kozachenko et al
   each Euclidean distance is computed twice since we do not store the
   upper diagonal of the (huge) distance matrix as in R code, 
   so this is a trade-off between time and RAM usage
 */
void entropyNNC(
    int *mc, 		/* nb chains = sample size = row size */
    int *dd, 		/* space dimension = column size */
    double *x,  	/* nmc by d matrix of simulated values */
    double *lc1,	/* log(c1(d)) using gamma function in R */
    double *y		/* returned value (entropy) computed here */ 
     ) {
  int nmc=*mc, d=*dd, i, j, k;
  double logc1=*lc1;
  double xik, xjk, rho, dist; 
  double dnmc;
  double CEuler = 0.57721566490153286; /* Euler constant */
  double Ent = 0.0; /* stores final result */
  
  dnmc = 1.0 * (double)(nmc);  
  for (i=0; i<nmc; i++) { 	/* find rho(i) for each i*/
  	  rho=1e+100; /* start with infinity */  	  
  	  for (j=0; j<nmc; j++) { /* compute dist(xi,xj) */
  	  	  if (j != i) {   /* avoid dist(xi,xi) = 0 case */
  	  	  	 dist = 0.0;
  	  	  	 for (k=0; k<d; k++) { /* for each coordinate */
  	  	  	 	 xik = x[i + nmc*k]; xjk = x[j + nmc*k];
  	  	  	 	 dist += (xik - xjk)*(xik - xjk);
  	  	  	 	}
  	  	  	 dist = sqrt(dist);
  	  	  	 if (dist < rho) rho = dist; /* min_j d(xi,xj) */
  	  	  	}
  	  }
  	  Ent += log(rho);	  
  } /* for each i */
  Ent = (double)(d)*Ent/dnmc + log(dnmc-1) + logc1 + CEuler;
  *y = - Ent; /* convention : we estimate E_p(log p) */ 
 }
 
 
