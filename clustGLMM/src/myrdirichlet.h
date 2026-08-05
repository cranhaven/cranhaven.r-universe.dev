#ifndef DIRICHLET_H
#define DIRICHLET_H

void my_rdirichlet(double* gen,  // OUT [n*K] generated values
                int* K,       // IN  [1] dimension of the distribution
                double* alpha // IN  [K] parameter of Dirichlet distribution
);

#endif
