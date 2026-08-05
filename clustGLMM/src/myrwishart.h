#ifndef WISHART_H
#define WISHART_H

void my_rwishart(double* gen,    // OUT [p*(p+1)/2] Wishart distributed matrix
              double* cholV,  // IN  [p*(p+1)/2] Choleski decomposition (upper triangle) of scale matrix
              double* df,     // degrees of freedom
              int* p          // dimension of the matrix
);

#endif
