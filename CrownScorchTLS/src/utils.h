#ifndef UTILS_HPP
#define UTILS_HPP

#include "classes.h"
using namespace Rcpp;

vector<vector<vector<double>>> getChunks(vector<vector<double>>& cloud, vector<unsigned int>& identifier);
vector<double> getMinMax(vector<vector<double>>& xyz);
vector<vector<vector<double>>> getSlices(vector<vector<double>>& cloud, double zmin = 1, double zmax = 3, double zstep = 0.5);
vector<vector<vector<double>>> getSlices(NumericMatrix& cloud, double zmin = 1, double zmax = 3, double zstep = 0.5);
vector<vector<double>> rmatrix2cpp(NumericMatrix& cloud);

#endif // UTILS_HPP
