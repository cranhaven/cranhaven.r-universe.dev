/*
  This file is part of SNPknock.

    Copyright (C) 2017-2019 Matteo Sesia

    SNPknock is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SNPknock is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with SNPknock.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef INTERFACE_CPP
#define INTERFACE_CPP

#include "interface.h"

using namespace Rcpp;

vector numToVec(const Rcpp::NumericVector & v) {
  return(Rcpp::as<vector>(v));
}

ivector numToIntVec(const Rcpp::IntegerVector & v) {
  ivector X(v.size(), 0);
  for(int i=0; i<v.size(); i++) {
      X[i] = v[i];
  }
  return(X);
}

vector2 numToVec2(const Rcpp::NumericVector & v, int dim1) {
  int dim2 = int(v.size()/dim1);
  vector2 X(dim1, vector(dim2, 0));
  for(int i=0; i<dim1; i++) {
    for(int j=0; j<dim2; j++) {
      X[i][j] = v[j*dim1+i];
    }
  }
  return(X);
}

ivector2 numToIntVec2(const Rcpp::IntegerVector & v, int dim1) {
  int dim2 = int(v.size()/dim1);
  ivector2 X(dim1, ivector(dim2, 0));
  for(int i=0; i<dim1; i++) {
    for(int j=0; j<dim2; j++) {
      X[i][j] = v[j*dim1+i];
    }
  }
  return(X);
}

vector3 numToVec3(const Rcpp::NumericVector & v, int dim1, int dim2) {
  int dim3 = int(v.size()/(dim1*dim2));
  vector3 X(dim1, std::vector<vector>(dim2, vector(dim3,0)));

  for(int i=0; i<dim1; i++) {
    for(int j=0; j<dim2; j++) {
      for(int k=0; k<dim3; k++) {
        X[i][j][k] = v[k*dim1*dim2+j*dim1+i];
      }
    }
  }
  return(X);
}

#endif
