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

#ifndef KNOCKOFF_DMC_CPP
#define KNOCKOFF_DMC_CPP

#include "dmc.h"

KnockoffDMC::KnockoffDMC(const std::vector<int>& _G, int K, int seed){
  unsigned int p = _G.size();

  // Initialize temporary variables
  arma::mat Z0(K,K);
  arma::vec Z(K);
  arma::vec Q1(K);
  arma::vec w(K);

  // Initialize the random number generator
  gen = std::mt19937();
  gen.seed(seed);
  dis = std::uniform_real_distribution<> (0.0,1.0);

  // Convert G and split non-contigous groups
  G = arma::conv_to< arma::uvec >::from(_G);
  unsigned int group = 0;
  G(0) = group;
  for(unsigned int j=1; j<p; j++) {
    if(_G[j]!=_G[j-1]) {
      group++;
    }
    G(j) = group;
  }
  gInd.resize(group+1);
  for(unsigned int g=0; g<gInd.size(); g++) {
    gInd[g] = arma::find(G==g);
  }
}

KnockoffDMC::KnockoffDMC(const std::vector<double>& _pInit, const std::vector< matrix >& _Q,
                           const std::vector<int>& _G, int seed) :
  KnockoffDMC(_G, _pInit.size(), seed) {

  unsigned int K = _pInit.size();
  unsigned int p = _Q.size()+1;

  // Store parameters as matrices
  Q_stored = arma::cube(K,K,p);
  for (unsigned int k1 = 0; k1 < K; k1++) {
      Q_stored.slice(0).row(k1) = arma::mat(&_pInit[0], 1, K);
  }
  for (unsigned int j = 1; j < p; j++) {
    for (unsigned int k1 = 0; k1 < K; k1++) {
      Q_stored.slice(j).row(k1) = arma::mat(&_Q[j-1][k1][0], 1, K);
    }
  }
}

KnockoffDMC::~KnockoffDMC(){}

std::vector< std::vector<int> > KnockoffDMC::sample(const std::vector< std::vector<int> >& X) {
  return sample(X, Q_stored);
}

std::vector<int> KnockoffDMC::sample(const std::vector<int>& X) {
  return sample(X, Q_stored);
}

std::vector< std::vector<int> > KnockoffDMC::sample(const std::vector< std::vector<int> >& X, const arma::cube& Q) {
  unsigned int n = X.size();
  unsigned int p = X[0].size();
  std::vector< std::vector<int> > Xt(n, std::vector<int>(p));           // Store the results here
  for(unsigned int i=0; i<n; i++) {
    Xt.at(i) = sample(X.at(i), Q);       // Sample knockoffs individual-by-individual
  }
  return(Xt);
}

std::vector<int> KnockoffDMC::sample(const std::vector<int>& X, const arma::cube& Q) {

  // Read dimensions of problem
  unsigned int p = X.size();
  unsigned int K = Q.n_rows;

  std::vector<int> Xt(p);                                     // Store the results here

  Z.fill(1);                                                  // Reset normalization function
  for(unsigned int g=0; g<gInd.size(); g++) {                 // Sample the knockoffs for this group
    arma::uvec ind = gInd[g];                                 // Find indices of variables within this group
    unsigned int L = ind.n_elem;                              // Size of this group

    // Compute initial probabilities based on partition function
    if( ind[0]>0 )
      Q1 = Q.slice(ind[0]).row(X.at(ind[0]-1)).t() % Q.slice(ind[0]).row(Xt.at(ind[0]-1)).t() / Z;
    else
      Q1 = Q.slice(0).row(0).t();

    // Compute the normalization functions
    Z0 = arma::eye(K,K);
    for(unsigned int j=1; j<L; j++) {                         // Middle L-2 elements + last (inside)
      Z0 = Z0 * Q.slice(ind[j]);
    }
    Z = Z0.t() * Q1;                                          // First element of cluster
    if( ind[L-1]<(p-1) )                                      // Last element of cluster (outside)
      Z = Q.slice(ind[L-1]+1).t() * Z;

    // Backpropagate normalization for conditional HMM
    arma::mat QN(K,L);
    if( ind[L-1]<(p-1) )
      QN.col(L-1) = Q.slice(ind[L-1]+1).col(X.at(ind[L-1]+1));
    else
      QN.col(L-1).fill(1.0/K);
    if(L>1) {
      for(int j=L-2; j>=0; j--) {
        QN.col(j) = Q.slice(ind[j+1]) * QN.col(j+1);
      }
    }

    // Sample the first knockoff
    w = Q1 % QN.col(0);
    Xt.at(ind[0]) = weighted_choice(w, dis(gen));

    // Sample the other knockoffs (if size>1)
    for(unsigned int j=1; j<L; j++) {
      w = Q.slice(ind[j]).row(Xt.at(ind[j]-1)).t() % QN.col(j);
      Xt.at(ind[j]) = weighted_choice(w, dis(gen));
    }
  }

  return( Xt );
}

unsigned int weighted_choice(const arma::vec & w, double R) {
  unsigned int K = w.n_elem;
  arma::vec wCumulative = arma::cumsum(w);
  wCumulative /= wCumulative(K-1);
  for(unsigned int k=0; k<K; k++) {
    if( wCumulative(k) > R ) {
      return k;
    }
  }
  return(K-1);
}


#endif
