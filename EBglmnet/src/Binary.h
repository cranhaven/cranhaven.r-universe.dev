#ifndef BINARY_H
#define BINARY_H

//elastic net binary (alpha, lambda)
void ElasticNetBinaryNEmainEff(double *BASIS, double * Targets, double *a_Lambda,double *b_Alpha,
                               double * logLIKELIHOOD, double * Beta, double *wald,double *intercept, int *n, int *kdim);


//NEG binary (a_gamma, b_gamma)
void LinearSolver(double * a, double *logout, int N,int M,double *output);
void fEBInitialization(double *Alpha, double * PHI2, int *Used, int *Unused, double *Mu2,
                            double *BASIS, double *Targets, double *Scales, int * initial, int n, int *m, int kdim);


void fEBSigmoid(double * y, double * PHI_Mu,int N);
double fEBDataError(double dataError,double *y,double *PHI_Mu,double *Targets,int N);
void MatrixInverse(double * a,int N);
void fEBCatPostMode(double * Mu2, double *beta,double *SIGMA2, double * H2, double *PHI2,
                         double *Targets, double *Alpha,int N, int M);
void fEBCatFullStat(double * beta, double * SIGMA2, double * H2, double *S_in, double * Q_in, 
                         double * S_out, double * Q_out,  double *BASIS,double * Scales, double *PHI2, 
                         double * Targets, int * Used, double *Alpha, double * Mu2, double * BasisCache,
                         int *n, int *m, int *kdim);

#endif

