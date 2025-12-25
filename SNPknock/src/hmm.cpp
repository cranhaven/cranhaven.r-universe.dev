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

#ifndef KNOCKOFF_HMM_CPP
#define KNOCKOFF_HMM_CPP

#include "hmm.h"

KnockoffHMM::KnockoffHMM(const std::vector<double> & _initP, const std::vector< matrix > & _Q, \
                         const std::vector< matrix > & _emissionP, const std::vector<int>& G, int seed) {
  initP = _initP;
  Q = _Q;
  emissionP = _emissionP;
  H_knockoffs = new KnockoffDMC(initP, Q, G, seed+100000);
  nStates = initP.size();
  nEmitStates = emissionP[0].size();
  p = emissionP.size();
  H = std::vector<int> (p);
  Ht = std::vector<int> (p);
  Xt = std::vector<int> (p);
  weights = std::vector<double> (nStates);
  weightsEmit = std::vector<double> (nEmitStates);
  fBeta = std::vector<double> (nStates);
  beta = matrix (p, std::vector<double> (nStates));
  std::fill(beta[p-1].begin(), beta[p-1].end(), 1.0);
  gen = std::mt19937();
  dis = std::uniform_real_distribution<double>(0.0,1.0);
  gen.seed(seed);
}

KnockoffHMM::~KnockoffHMM(){
  delete H_knockoffs;
}

std::vector<int> KnockoffHMM::sample(const std::vector<int> & X) {
  // Compute the beta values using the backward algorithm
  backwardHMM(X);
  // Sample the hidden states from their conditional distribution given the observations
  sampleHMMConditional(X);
  // Create knockoff hidden states
  Ht = H_knockoffs->sample(H);

  // Sample from the conditional distribution of the observations given the knockoff hidden states
  for(int j=0; j<p; j++) {
    for(int s=0; s<nEmitStates; s++) {
      weightsEmit[s] = emissionP[j][s][Ht[j]];
    }
    Xt[j] = weighted_choice(dis(gen),weightsEmit);
  }
  return(Xt);
}

std::vector< std::vector<int> > KnockoffHMM::sample(const std::vector<std::vector<int> > & X) {
  int n = X.size();
  std::vector< std::vector<int> > XtMatrix(n, std::vector<int>(p));
  for(unsigned int i=0; i<X.size(); i++) {
    XtMatrix[i] = sample(X[i]);
  }
  return(XtMatrix);
}

void KnockoffHMM::sampleHMMConditional(const std::vector<int> & X) {
  double weights_sum = 0.0;
  for(int k=0; k<nStates; k++) {
    weights[k] = initP[k] * emissionP[0][X[0]][k] * beta[0][k];
    weights_sum += weights[k];
  }

  for(int k=0; k<nStates; k++) {
    weights[k] /= weights_sum;
  }
  H[0] = weighted_choice(dis(gen),weights);

  for(int j=1; j<p; j++) {
    weights_sum = 0.0;
    for(int k=0; k<nStates; k++) {
      weights[k] = Q[j-1][H[j-1]][k] * emissionP[j][X[j]][k] * beta[j][k];
      weights_sum += weights[k];
    }

    for(int k=0; k<nStates; k++) {
      weights[k] /= weights_sum;
    }
    H[j] = weighted_choice(dis(gen),weights);
  }
}

void KnockoffHMM::backwardHMM(const std::vector<int> & X) {
  std::fill(beta[p-1].begin(), beta[p-1].end(), 1.0);

  for(int j=p-2; j>=0; j--) {
    for(int l=0; l<nStates; l++) {
      fBeta[l] = emissionP[j+1][X[j+1]][l] * beta[j+1][l];
    }
    betaSum = 0.0;
    for(int k=0; k<nStates; k++) {
      beta[j][k] = std::inner_product(Q[j][k].begin(), Q[j][k].end(), fBeta.begin(), 0.0);
      betaSum += beta[j][k];
    }

    for(int k=0; k<nStates; k++) {
      beta[j][k] /= betaSum;
    }

  }
}

#endif
