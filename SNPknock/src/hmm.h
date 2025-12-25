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

#ifndef KNOCKOFF_HMM_H
#define KNOCKOFF_HMM_H

#include "dmc.h"
#include "utils.h"
#include <random>
#include <vector>
#include <algorithm>

typedef std::vector< std::vector<double> > matrix;

class KnockoffHMM {
 public:
  KnockoffHMM(const std::vector<double> & _initP, const std::vector< matrix > & _Q, \
               const std::vector< matrix > & _emissionP, const std::vector<int>& G, int seed);
  ~KnockoffHMM();
  std::vector<int> sample(const std::vector<int> & X);
  std::vector< std::vector<int> > sample(const std::vector<std::vector<int> > & X);
 private:
  std::random_device rd;
  std::mt19937 gen;
  std::uniform_real_distribution<> dis;

  std::vector<double> initP;
  std::vector< matrix > Q;
  std::vector< matrix > emissionP;
  void sampleHMMConditional(const std::vector<int> & X);
  void backwardHMM(const std::vector<int> & X);
  KnockoffDMC* H_knockoffs;
  std::vector<int> H, Ht, Xt;
  std::vector<double> weightsEmit, weights, fBeta;
  matrix beta;
  int nStates, nEmitStates, p;
  double betaSum;
};

#endif
