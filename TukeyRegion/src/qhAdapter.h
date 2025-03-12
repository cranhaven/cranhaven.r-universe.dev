// qhAdapter.h
// By Pavlo Mozharovskyi
// Last changed 12.08.2016
// Header for the adapter for the QHULL routines

int convhull(double *points, int n, int d, int *vertexIndices);
double convvol(double *points, int n, int d);
