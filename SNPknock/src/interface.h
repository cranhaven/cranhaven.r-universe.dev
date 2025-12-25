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

#ifndef INTERFACE_H
#define INTERFACE_H

#include <RcppArmadillo.h>
#include <vector>
#include <iostream>
#include "dmc.h"
#include "hmm.h"
#include "haplotypes.h"
#include "genotypes.h"

typedef std::vector< double > vector;
typedef std::vector< std::vector<double> > vector2;
typedef std::vector< vector2 > vector3 ;
typedef std::vector< int > ivector;
typedef std::vector< std::vector<int> > ivector2;

vector  numToVec(const Rcpp::NumericVector &);
ivector numToIntVec(const Rcpp::IntegerVector & v);
vector2 numToVec2(const Rcpp::NumericVector &, int);
vector3 numToVec3(const Rcpp::NumericVector &, int, int);
ivector2 numToIntVec2(const Rcpp::IntegerVector &, int);

#endif
