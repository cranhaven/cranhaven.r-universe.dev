#ifndef LINEAR_H
#define LINEAR_H

#include "Binary.h"

//same: void LinearSolver(double * a, double *logout, int N,int M,double *output);
void fEBInitialization_Gauss(double *Alpha, double * PHI, int *Used, int *Unused, 
                            double *BASIS, double *Targets, double *Scales, int * initial, 
                            int n, int *m, int kdim, double *beta);

//same void MatrixInverse(double * a,int N);
void CacheBP(double **BASIS_PHI, double *BASIS_Targets, double *BASIS, double *PHI,
                  double *Targets,double *scales,int N,int K,int M, int M_full);
void CacheBPGmNeg(double *BASIS_PHI, double *BASIS_Targets, double *BASIS, double *PHI,
                  double *Targets, double *scales,int N,int K,int M,int M_full);



void fEBLinearFullStat(double *beta,double * SIGMA,double *H, double *S_in, double * Q_in, double * S_out, 
                             double * Q_out,  double *BASIS, double * Scales, double *PHI, double **BASIS_PHI,
                             double *BASIS_Targets, double * Targets, int * Used, double *Alpha, double * Mu, 
                             double *Gamma,int *n, int *m, int* kdim, int *iteration,int *i_iter); //Same func shared in NE/NEG linear
void fEBLinearFullStatGmNeg(double *beta, double * SIGMA, double *H, double *S_in, double * Q_in, double * S_out, 
                            double * Q_out,   double *BASIS, double * Scales, double *PHI, double *BASIS_PHI,
                            double *BASIS_Targets, double * Targets, int * Used, double *Alpha, double * Mu, 
                            double *gamma,int *n, int *m, int* kdim, int *iteration,int *i_iter);



void fEBDeltaML_NEG(double *DeltaML, int *Action, double *AlphaRoot, int *anyToDelete,
                     int *Used, int * Unused, double * S_out, double * Q_out, double *Alpha,
                     double *a_gamma, double *b_gamma, int m, int mBar);


void LinearFastEmpBayes_NEG(int *Used, double *Mu, double *SIGMA, double *H, double *Alpha, double *PHI,
                             double *BASIS, double * Targets, double *Scales, double *a_gamma, double *b_gamma,
                             int *iteration, int *n, int *kdim, int *m, int basisMax, double *b, double *beta,double * C_inv,int verbose);



int ActionAdd(double **BASIS_PHI, double* BASIS, double*scales, double*PHI, double*Phi,
                   double *beta, double* Alpha, double newAlpha, double*SIGMA, double*Mu, double*S_in,
                   double*Q_in, int nu, double*SIGMANEW, int M_full, int N, int K, int M);
int ActionAddGmNeg(double *BASIS_PHI, double* BASIS, double*scales, double*PHI, double*Phi,
                   double *beta, double* Alpha, double newAlpha, double*SIGMA, double*Mu, double*S_in,
                   double*Q_in, int nu, double*SIGMANEW, int M_full,int N, int K, int M);

int ActionDel(double*PHI, double*Alpha, double*SIGMA, double*SIGMANEW, double**BASIS_PHI,
                   double*Mu, double*S_in, double*Q_in, double *beta, int jj, int N, int M, int M_full);
int ActionDelGmNeg(double*PHI, double*Alpha, double*SIGMA, double*SIGMANEW, double*BASIS_PHI,
                   double*Mu, double*S_in, double*Q_in, double *beta, int jj, int N, int M, int M_full);

double varTargets(double* Target,int N);

void FinalUpdate(double *PHI,double * H,double*SIGMA,double *Targets,double *Mu,double *Alpha,
                      double *beta,int N, int M);


#endif
