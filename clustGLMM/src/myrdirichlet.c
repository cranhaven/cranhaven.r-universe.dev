/*
 * Generating from Dirichlet distribution
 */


#include <R.h>
#include <Rmath.h>


void my_rdirichlet(double* gen,  // OUT [n*K] generated values
                int* K,       // IN  [1] dimension of the distribution
                double* alpha // IN  [K] parameter of Dirichlet distribution
                )    
{ 
  /** Declarations **/
  int j;     // looping indeces
  double sumx;  // sum of all gamma distributed variables
  
  sumx = 0;
  for(j = 0; j < *K; j++){
    gen[j] = rgamma(alpha[j], 1.0);
    sumx += gen[j];
  }
  
  for(j = 0; j < *K; j++){
    gen[j] /= sumx;
  }
  
}
