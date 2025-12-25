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

#ifndef KNOCKOFF_DMC_H
#define KNOCKOFF_DMC_H

/*
Group knockoffs for a Discrete Markov Chain model
*/

#include <vector>
#include <random>
#include <iostream>
#include <RcppArmadillo.h>
#include <stdio.h>

typedef std::vector< std::vector<double> > matrix;

class KnockoffDMC {
 public:
  KnockoffDMC(const std::vector<int>& _G, int K, int seed);
  KnockoffDMC(const std::vector<double>& _pInit, const std::vector< matrix >& _Q,
               const std::vector<int>& _G, int seed);
  ~KnockoffDMC();
  std::vector< std::vector<int> > sample(const std::vector< std::vector<int> >& X);
  std::vector< std::vector<int> > sample(const std::vector< std::vector<int> >& X, const arma::cube& Q);
  std::vector<int> sample(const std::vector<int>& X, const arma::cube& Q);
  std::vector<int> sample(const std::vector<int>& X);

 private:
  arma::cube Q_stored;
  arma::uvec G;
  std::vector<arma::uvec> gInd;
  arma::mat Z0;
  arma::vec Z, Q1, w;
  std::mt19937 gen;
  std::random_device rd;
  std::uniform_real_distribution<> dis;
};

unsigned int weighted_choice(const arma::vec & w, double R);

#endif
