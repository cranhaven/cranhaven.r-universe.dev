/*
 * Functions for logs of full conditional pdfs and their derivatives
 */

#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "structures.h"

/*
 * e0 
 */

void logple0(struct str_state* last,    // IN last known values of generated parameters
             struct str_param* param,   // IN hyperparameters
             int* G,                    // IN [1] number of all clusters
             int* Gplus,                // IN [1] number of non-empty clusters sum(nUg>0)
             int* n,                    // IN [1] total number of subjects
             int* nUg,                  // IN [G] number of subjects currently within g-th cluster 
             double* le,                // IN [1] value of e (mu) at which it should be evaluated
             double* value              // OUT [1] value of the pdf
){
  // Auxiliary variables
  int g;
  double e = exp(*le);
  
  // Start with prior distribution contribution
  *value = (*((*param).ae)) * (*le) - *((*param).be) * e;
  
  // Now add log Gamma functions
  *value += lgamma((*G) * e);
  *value -= lgamma(*n + (*G) * e);
  *value -= (*Gplus) * lgamma(e);
  for(g = 0; g < *G; g++){
    if(nUg[g] > 0){
      *value += lgamma(nUg[g] + e);
    }
  }
  // factorials are not needed - just unimportant constants
  
}

void d_d2_logple0(struct str_state* last,   // IN last known values of generated parameters
                  struct str_param* param,  // IN hyperparameters
                  int* G,                   // IN [1] number of all clusters
                  int* Gplus,               // IN [1] number of non-empty clusters sum(nUg>0)
                  int* n,                   // IN [1] total number of subjects
                  int* nUg,                 // IN [G] number of subjects currently within g-th cluster 
                  double* le,               // IN [1] value of e (mu) at which it should be evaluated
                  double* grad,             // OUT [1] first derivative
                  double* negd2             // OUT [1] negative second derivative
){
  // Auxiliary variables
  int g;
  double e = exp(*le);
  double aux;
  
  // Start with gradient 
  *grad = -(*((*param).be));
  *grad += *G * digamma((*G) * e);
  *grad -= *G * digamma(*n + (*G) * e);
  *grad -= (*Gplus) * digamma(e);
  for(g = 0; g < *G; g++){
    if(nUg[g] > 0){
      *grad += digamma(nUg[g] + e);
    }
  }
  *grad *= e;
  // this part is the same for negd2
  *negd2 = -(*grad);
  // grad needs to add a_e
  *grad += *((*param).ae);
  
  aux = *G * *G * trigamma((*G) * e);
  aux -= *G * *G * trigamma(*n + (*G) * e);
  aux -= (*Gplus) * trigamma(e);
  for(g = 0; g < *G; g++){
    if(nUg[g] > 0){
      aux += trigamma(nUg[g] + e);
    }
  }
  *negd2 -= e * e * aux;
  
}

