#ifndef PDFS_DERIVATIVES_LE0_H
#define PDFS_DERIVATIVES_LE0_H

void logple0(struct str_state* last,    // IN last known values of generated parameters
             struct str_param* param,   // IN hyperparameters
             int* G,                    // IN [1] number of all clusters
             int* Gplus,                // IN [1] number of non-empty clusters sum(nUg>0)
             int* n,                    // IN [1] total number of subjects
             int* nUg,                  // IN [G] number of subjects currently within g-th cluster 
             double* le,                // IN [1] value of e (mu) at which it should be evaluated
             double* value              // OUT [1] value of the pdf
);

void d_d2_logple0(struct str_state* last,   // IN last known values of generated parameters
                  struct str_param* param,  // IN hyperparameters
                  int* G,                   // IN [1] number of all clusters
                  int* Gplus,               // IN [1] number of non-empty clusters sum(nUg>0)
                  int* n,                   // IN [1] total number of subjects
                  int* nUg,                 // IN [G] number of subjects currently within g-th cluster 
                  double* le,               // IN [1] value of e (mu) at which it should be evaluated
                  double* grad,             // OUT [1] first derivative
                  double* negd2             // OUT [1] negative second derivative
);

#endif
