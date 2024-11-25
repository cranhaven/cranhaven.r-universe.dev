#define SQR(x) ((x)*(x))

void bwsolve
  (
    double *x,
    const double *u,
    const int n
  );

void fwsolve
  (
    double *y,
    const double *u,
    const double *x,
    const int n
  );

void subscalarmul
  (
    double *x, 
    const double alpha, 
    const double *y, 
    const int n
  );

double realdot
  (
      const double *x, 
      const double *y, 
      const int n
  );
