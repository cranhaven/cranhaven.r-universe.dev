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

#ifndef GENOTYPES_H
#define GENOTYPES_H

/*
Knockoffs for phased genotypes
*/

#include <vector>
#include <random>
#include "utils.h"
#include <iostream>

typedef std::vector< std::vector<double> > matrix;
typedef std::vector< std::vector<int> > imatrix;

class GroupGenotypes {
 public:
  GroupGenotypes(const std::vector<double> & r, const matrix & alpha, const matrix & _theta,
                 const std::vector<int> _groups, int seed);
  imatrix sample(const imatrix & X);
  std::vector<int> sample(const std::vector<int> & X);
 private:
  int pair_to_index(int i, int j);
  std::vector<int> single_to_indices(int j);
  double emission_prob(int j, int x, int k, int l);
  double emission_prob(int j, int x, int m);
  imatrix table;
  void sampleViterbi(const std::vector<int> & X);
  void knockoffMC(const std::vector<int> & H);
  void emission(const std::vector<int> & Hk);
  double Q_bar(int j, int k, int l);
  double V_bar(int k, int l, const std::vector<double>& v, const std::vector<double>& w, double u);
  matrix theta, a;
  std::vector<double> b;
  matrix beta;
  double beta_const;
  std::vector<double> weightsEmit, weights;
  int nStates, p, K, nGroups;
  std::vector<int> H, Hk, Xk, groups;
  imatrix elements;
  // Partition function for Markov chain knockoffs
  std::vector<double> Z, Z_old, C, D;
  std::vector<int> indices;
  // Random number generation
  std::random_device rd;
  std::mt19937 gen;
  std::uniform_real_distribution<> dis;
  std::mt19937 gen2;
};

#endif
