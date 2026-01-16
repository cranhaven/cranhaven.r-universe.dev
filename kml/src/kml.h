# include <R.h>
# include <Rdefines.h>
# include <Rmath.h>
# include <stdio.h>
# include <stdlib.h>
# include <float.h>

// all distance 'Traj' are optimized to work with trajectories

void printMatrix(double *mTraj,int *nbCol, int *nbLigne);

void printMatrixInt(int *mTraj,int *nbCol, int *nbLigne);

static double euclideanTraj(double *x,double *y,int *taille);

void calculMean(double *traj, int *nbInd, int *nbTime, int *clusterAffectation, int *nbClusters, double *trajMean);

void affecteIndiv(double *traj, int *nbInd, int *nbTime, double *trajMean, int *nbClusters, int *clusterAffectation);

void kml1(double *traj, int *nbInd, int *nbTime, int *nbClusters, int *maxIt,int *clusterAffectation1, int *convergenceTime);

