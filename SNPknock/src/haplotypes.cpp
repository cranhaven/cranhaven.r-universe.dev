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

#ifndef HAPLOTYPES_CPP
#define HAPLOTYPES_CPP

#include "haplotypes.h"

GroupHaplotypes::GroupHaplotypes(const std::vector<double> & r, const matrix & alpha, const matrix & _theta,
                               const std::vector<int> groups, int seed){
  // Store input parameters
  theta = _theta;
  p = alpha.size();
  nStates = alpha[0].size();

  // Process MC parameters
  a = matrix(p, std::vector<double> (nStates));
  b = std::vector<double>(p, 0);
  b[0] = 0;
  for(int l=0; l<nStates; l++) {
    a[0][l] = alpha[0][l];
  }
  for(int j=1; j<p; j++) {
    b[j] = std::exp(-r[j]);
    for(int l=0; l<nStates; l++) {
      a[j][l] = (1.0-b[j])*alpha[j][l];
    }
  }
  // Initialize random number generator
  gen = std::mt19937();
  gen2 = std::mt19937();
  dis = std::uniform_real_distribution<double>(0.0,1.0);
  gen.seed(seed);
  gen2.seed(seed+100000);

  // Initialize variables
  weights = std::vector<double> (nStates);
  beta = matrix(p, std::vector<double> (nStates));
  H = std::vector<int> (p,0);
  Hk = std::vector<int> (p,0);
  Xk = std::vector<int> (p,0);
  // Knockoffs for Markov chains
  Z = std::vector<double> (nStates,1.0);
  Z_old = std::vector<double> (nStates,1.0);

  // Process group membership
  nGroups = groups.back()+1;
  elements.resize(nGroups);
  for(int i=0; i<p; i++) {
    elements[groups[i]].push_back(i);
  }

}

void GroupHaplotypes::sampleViterbi(const std::vector<int> & X) {
  // Compute backward weights
  std::fill(beta[p-1].begin(), beta[p-1].end(), 1.0);
  for(int j=p-2; j>=0; j--) {
    beta_const = 0.0;
    for(int l=0; l<nStates; l++) {
      if ( X[j+1] == 1) {
        beta_const += a[j+1][l] * theta[j+1][l] * beta[j+1][l];
      }
      else {
        beta_const += a[j+1][l] * (1.0-theta[j+1][l]) * beta[j+1][l];
      }
    }
    double betaSum = 0.0;
    for(int k=0; k<nStates; k++) {
      if ( X[j+1] == 1) {
        beta[j][k] = beta_const + b[j+1] * theta[j+1][k] * beta[j+1][k];
      }
      else {
        beta[j][k] = beta_const + b[j+1] * (1.0-theta[j+1][k]) * beta[j+1][k];
      }
      betaSum += beta[j][k];
    }
    for(int k=0; k<nStates; k++) {
      beta[j][k] /= betaSum;
    }
  }

  // Forward sampling
  double weights_sum = 0.0;
  for(int k=0; k<nStates; k++) {
    if ( X[0] == 1) {
      weights[k] = a[0][k] * theta[0][k] * beta[0][k];
    }
    else {
      weights[k] = a[0][k] * (1.0-theta[0][k]) * beta[0][k];
    }
    weights_sum += weights[k];
  }
  for(int k=0; k<nStates; k++) {
    weights[k] /= weights_sum;
  }
  H[0] = weighted_choice(dis(gen),weights);

  for(int j=1; j<p; j++) {
    weights_sum = 0.0;
    for(int k=0; k<nStates; k++) {
      if ( X[j] == 1) {
        weights[k] = (a[j][k] + b[j]*(double)(k==H[j-1])) * theta[j][k] * beta[j][k];
      }
      else {
        weights[k] = (a[j][k] + b[j]*(double)(k==H[j-1])) * (1.0-theta[j][k]) * beta[j][k];
      }
      weights_sum += weights[k];
    }
    for(int k=0; k<nStates; k++) {
      weights[k] /= weights_sum;
    }
    H[j] = weighted_choice(dis(gen),weights);
  }
}

void GroupHaplotypes::knockoffMC(const std::vector<int> & H) {
  std::fill(Z_old.begin(), Z_old.end(), 1.0);
  double Z_min = 1.0e-10;

  for(int g=0; g<nGroups; g++) {
    // Compute vstar
    int groupSize = elements[g].size();
    std::vector<double> vstar(groupSize,0.0);
    for(int j=groupSize-1; j>=0; j--) {
      if(j < groupSize-1) {
        vstar[j] = vstar[j+1] * b[elements[g][j+1]];
      }
      else {
        if(g < nGroups-1) {
          vstar[j] = b[elements[g+1][0]];
        }
        else {
          vstar[j] = 0.0;
        }
      }
    }

    // Compute vbar matrix
    matrix vbar;
    vbar = matrix(groupSize, std::vector<double> (nStates,0));
    for(int j=groupSize-1; j>=0; j--) {
      double sum_a = 0;
      if(j < groupSize-1) {
        for(int l=0; l<nStates; l++) {
          sum_a += a[elements[g][j+1]][l];
        }
      }
      for(int z=0; z<nStates; z++) {
        if(j < groupSize-1) {
          vbar[j][z] = vbar[j+1][z] * (sum_a + b[elements[g][j+1]]);
          vbar[j][z] += vstar[j+1] * a[elements[g][j+1]][z];
        }
        else {
          if(g < nGroups-1) {
            vbar[j][z] = a[elements[g+1][0]][z];
          }
          else {
            vbar[j][z] = 1.0;
          }
        }
      }
    }

    // Precompute sum for partition function
    double Z_sum = 0;
    for(int k=0; k<nStates; k++) {
      if( g==0 ) {
        Z_sum += a[elements[g][0]][k];
      }
      else {
        int H0 = H[elements[g-1].back()];
        int H0k = Hk[elements[g-1].back()];
        double tmp = (a[elements[g][0]][k] + b[elements[g][0]]*(double)(k==H0));
        tmp  = tmp * (a[elements[g][0]][k] + b[elements[g][0]]*(double)(k==H0k));
        Z_sum += tmp / Z_old[k];
      }
    }

    // Compute partition function
    std::fill(Z.begin(), Z.end(), 0.0);
    for(int k=0; k<nStates; k++) {
      if(g < nGroups-1) {
        Z[k] += vbar[0][k] * Z_sum;
        if(g==0) {
          Z[k] += vstar[0] * a[elements[g][0]][k];
        }
        else {
          int H0 = H[elements[g-1].back()];
          int H0k = Hk[elements[g-1].back()];
          double tmp = (a[elements[g][0]][k] + b[elements[g][0]]*(double)(k==H0));
          tmp  = tmp * (a[elements[g][0]][k] + b[elements[g][0]]*(double)(k==H0k));
          Z[k] += vstar[0] * tmp / Z_old[k];
        }
      }
    }

    // Normalize partition function and make sure we avoid division by zero
    double Z_norm = 0;
    for(int k=0; k<nStates; k++) {
      if(Z[k] < Z_min) {
        Z[k] = Z_min;
      }
      Z_norm += Z[k];
    }
    for(int k=0; k<nStates; k++) {
      Z[k] /= Z_norm;
      if(Z[k] < Z_min) {
        Z[k] = Z_min;
      }
    }

    // Compute sampling weights
    for(int j=0; j<groupSize; j++) {
      std::fill(weights.begin(), weights.end(), 1.0);
      double weights_sum = 0;
      for(int k=0; k<nStates; k++) {
        if(j>0) {
          int H0k = Hk[elements[g][j-1]];
          weights[k] *= (a[elements[g][j]][k]+b[elements[g][j]]*(double)(k==H0k));
        }
        else {
          if( g==0 ) {
            weights[k] *= a[elements[g][0]][k];
          }
          else {
            int H0 = H[elements[g-1].back()];
            int H0k = Hk[elements[g-1].back()];
            weights[k] *= (a[elements[g][0]][k]+b[elements[g][0]]*(double)(k==H0));
            weights[k] *= (a[elements[g][0]][k]+b[elements[g][0]]*(double)(k==H0k));
          }
          weights[k] /= Z_old[k];
        }
        if(g == nGroups-1) {
          weights[k] *= (vbar[j][0]);
        }
        else {
          int H1 = H[elements[g+1][0]];
          weights[k] *= (vbar[j][H1] + vstar[j] * (double)(k==H1));
        }
        weights_sum += weights[k];
      }

      // Normalize weights
      for(int k=0; k<nStates; k++) {
        weights[k] /= weights_sum;
      }
      Hk[elements[g][j]] = weighted_choice(dis(gen2),weights);
    }

    for(int k=0; k<nStates; k++) {
      Z_old[k] = Z[k];
    }
  }
}

void GroupHaplotypes::emission(const std::vector<int> & Hk) {
  std::vector<double> weights2(2,1.0);
  // A little slower, but random seeds are compatible with older code
  for(int j=0; j<p; j++) {
    weights2[0] = 1.0-theta[j][Hk[j]];
    weights2[1] = theta[j][Hk[j]];
    Xk[j] = weighted_choice(dis(gen),weights2);
  }
}

std::vector<int> GroupHaplotypes::sample(const std::vector<int> & X) {
  sampleViterbi(X);
  knockoffMC(H);
  emission(Hk);
  return(Xk);
}

imatrix GroupHaplotypes::sample(const std::vector<std::vector<int> > & X) {
  unsigned int n = X.size();
  imatrix XkMatrix(n, std::vector<int>(p));
  for(unsigned int i=0; i<X.size(); i++) {
    XkMatrix[i] = sample(X[i]);
  }
  return(XkMatrix);

}

#endif
