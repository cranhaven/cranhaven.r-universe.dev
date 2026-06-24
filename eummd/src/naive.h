#ifndef GUARD_naive_h
#define GUARD_naive_h

#include<vector>

double cpp_mmd_lap(double* X, double* Y, 
                   int nX, int dX,
                   int nY, int dY,
                   double beta);

double cpp_mmd_gau(double* X, double* Y, 
                   int nX, int dX,
                   int nY, int dY,
                   double beta);

std::vector<double> cpp_mmd_lap_pval(double* X, double* Y, 
                                     int nX, int dX,
                                     int nY, int dY,
                                     int numperm, int seednum,
                                     double beta, 
                                     int twosided, int boundedminpval);

std::vector<double> cpp_mmd_gau_pval(double* X, double* Y, 
                                     int nX, int dX,
                                     int nY, int dY,
                                     int numperm, int seednum,
                                     double beta, 
                                     int twosided, int boundedminpval);


double cpp_energydist(double* X, double* Y, 
                      int nX, int dX,
                      int nY, int dY);


std::vector<double> cpp_energydist_pval(double* X, double* Y, 
                                        int nX, int dX,
                                        int nY, int dY,
                                        int numperm, int seednum,
                                        int twosided, int boundedminpval);

#endif
