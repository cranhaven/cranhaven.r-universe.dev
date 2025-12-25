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

#ifndef GENOTYPES_CPP
#define GENOTYPES_CPP

#include "genotypes.h"

GroupGenotypes::GroupGenotypes(const std::vector<double> & r, const matrix & alpha, const matrix & _theta,
                               const std::vector<int> groups, int seed){
  // Store input parameters
  theta = _theta;
  p = alpha.size();
  K = alpha[0].size();
  nStates = (K*(K+1))/2;

  // Process MC parameters
  a = matrix(p, std::vector<double> (K));
  b = std::vector<double>(p, 0);
  b[0] = 0;
  for(int l=0; l<K; l++) {
    a[0][l] = alpha[0][l];
  }
  for(int j=1; j<p; j++) {
    b[j] = std::exp(-r[j]);
    for(int l=0; l<K; l++) {
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
  H = std::vector<int> (p);
  table = imatrix(nStates, std::vector<int>(2));
  for(int i=1; i<K; i++) {
    for(int j=0; j<=i; j++) {
      int m = pair_to_index(i,j);
      table[m][0] = i;
      table[m][1] = j;
    }
  }
  Xk = std::vector<int> (p,0);
  indices = std::vector<int>(K);
  // Knockoffs for Markov chains
  C = std::vector<double>(nStates,0.0);
  D = std::vector<double>(K,0.0);
  Hk = std::vector<int> (p,0);
  Z = std::vector<double> (nStates);
  Z_old = std::vector<double> (nStates,1);

  // Process group membership
  nGroups = groups.back()+1;
  elements.resize(nGroups);
  for(int i=0; i<p; i++) {
    elements[groups[i]].push_back(i);
  }
}

int GroupGenotypes::pair_to_index(int k, int l) {
  // Make sure that k >= l
  if(k<l) {
    int tmp = l;
    l = k;
    k = tmp;
  }
  return((k*(k+1))/2+l);
}

std::vector<int> GroupGenotypes::single_to_indices(int j) {
  std::vector<int> indices(K);
  for(int i=0; i<K; i++) {
    indices[i] = pair_to_index(i,j);
  }
  return(indices);
}

double GroupGenotypes::emission_prob(int j, int x, int m) {
  return emission_prob(j, x, table[m][0], table[m][1]);
}

double GroupGenotypes::emission_prob(int j, int x, int k, int l) {
  double prob = 0.0;
  if(x==0) {
    prob = (1.0-theta[j][k])*(1.0-theta[j][l]);
  }
  else if(x==1) {
    prob = theta[j][k]*(1.0-theta[j][l]) + theta[j][l]*(1.0-theta[j][k]);
  }
  else if(x==2) {
    prob = theta[j][k]*theta[j][l];
  }
  return(prob);
}

double GroupGenotypes::Q_bar(int j, int k, int l) {
  int k1 = table[k][0];
  int k2 = table[k][1];
  int l1 = table[l][0];
  int l2 = table[l][1];
  double q;

  if(j==0) {
    q = (2.0-(k1==k2)) * a[j][k1] * a[j][k2];
  }
  else {
    q  = (a[j][k1] + b[j]*(k1==l1)) * (a[j][k2] + b[j]*(k2==l2));
    if(k1!=k2){
      q += (a[j][k2] + b[j]*(k2==l1)) * (a[j][k1] + b[j]*(k1==l2));
    }
  }
  return(q);
}

double GroupGenotypes::V_bar(int k, int l, const std::vector<double>& v, const std::vector<double>& w, double u){
  // FIX ME: update this with correct recursion formulae
  int k1 = table[k][0];
  int k2 = table[k][1];
  int l1 = table[l][0];
  int l2 = table[l][1];
  double V;

  V  = v[k] * (2.0-(k1==k2));
  V += ( w[k1]*((k2==l1)+(k2==l2)) + w[k2]*((k1==l1)+(k1==l2)) ) / (1.0+(k1==k2));
  V += u * (k==l);

  return(V);
}

void GroupGenotypes::sampleViterbi(const std::vector<int> & X) {
  // Compute backward weights
  std::fill(beta[p-1].begin(), beta[p-1].end(), 1.0);

  for(int j=p-2; j>=0; j--) {
    // Precompute scalar sum
    double beta_pre_scalar = 0;
    for(int k=0; k<K; k++) {
      for(int l=0; l<K; l++) {
        int m = pair_to_index(k,l);
        beta_pre_scalar += a[j+1][k] * a[j+1][l] * emission_prob(j+1, X[j+1], m) * beta[j+1][m];
      }
    }
    // Precompute vector sum
    std::vector<double> beta_pre_vec(K,0);
    for(int k=0; k<K; k++) {
      std::vector<int> indices = single_to_indices(k);
      for(int l=0; l<K; l++) {
        int k1 = table[indices[l]][0];
        int k2 = table[indices[l]][1];
        double factor = 0;
        if(k1==k2) {
          factor = a[j+1][k1];
        }
        else {
          factor = a[j+1][k1] * (k2==k) + a[j+1][k2] * (k1==k);
        }
        beta_pre_vec[k] += factor * emission_prob(j+1, X[j+1], indices[l]) * beta[j+1][indices[l]];
      }
    }
    // Assemble backward weights
    double betaSum = 0.0;
    for(int k=0; k<K; k++) {
      for(int l=0; l<=k; l++) {
        int m = pair_to_index(k,l);
        double pEmit = emission_prob(j+1, X[j+1], m);
        beta[j][m]  = beta_pre_scalar;
        beta[j][m] += b[j+1] * (beta_pre_vec[k] + beta_pre_vec[l]);
        beta[j][m] += b[j+1] * b[j+1] * pEmit * beta[j+1][m];
        betaSum += beta[j][m];
      }
    }

    // Normalize the backward weights
    for(int m=0; m<nStates; m++) {
      beta[j][m] /= betaSum;
    }

  }

  // Forward sampling
  double weights_sum = 0.0;
  for(int k=0; k<K; k++) {
    for(int l=0; l<=k; l++) {
      int m = pair_to_index(k,l);
      if(k==l) {
        weights[m] = a[0][k] * a[0][l];
      }
      else {
        weights[m] = 2.0 * a[0][k] * a[0][l];
      }
      weights[m] *= emission_prob(0, X[0], m) * beta[0][m];
      weights_sum += weights[m];
    }
  }

  for(int k=0; k<nStates; k++) {
    weights[k] /= weights_sum;
  }
  H[0] = weighted_choice(dis(gen),weights);

  for(int j=1; j<p; j++) {
    weights_sum = 0.0;
    for(int k=0; k<K; k++) {
      for(int l=0; l<=k; l++) {
        int m = pair_to_index(k,l);     // New state
        int k0  = table[H[j-1]][0];
        int l0  = table[H[j-1]][1];
        weights[m]  = (a[j][k]+b[j] * (double)(k==k0)) * (a[j][l]+b[j] * (double)(l==l0));
        if(k!=l) {
          weights[m] += (a[j][k]+b[j] * (double)(k==l0)) * (a[j][l]+b[j] * (double)(l==k0));
        }
        weights[m] *= emission_prob(j, X[j], m) * beta[j][m];
        weights_sum += weights[m];
      }
    }

    for(int k=0; k<nStates; k++) {
      weights[k] /= weights_sum;
    }
    H[j] = weighted_choice(dis(gen),weights);
  }
}

void GroupGenotypes::knockoffMC(const std::vector<int> & H) {
  std::fill(Z_old.begin(), Z_old.end(), 1.0);
  double Z_min = 1.0e-10;

  for(int g=0; g<nGroups; g++) {
    int groupSize = elements[g].size();

    // Compute v,w,u

    // FIXME: update this with correct recursion formulae
    std::vector<double> u;
    matrix v,w;
    u = std::vector<double>(groupSize, 0);
    v = matrix(groupSize, std::vector<double> (nStates,0.0));
    w = matrix(groupSize, std::vector<double> (K,0.0));

    for(int j=groupSize-1; j>=0; j--) {
      // Update u
      if(j == groupSize-1) {
        if(g < nGroups-1) {
          u[j] = b[elements[g+1][0]] * b[elements[g+1][0]];
        }
        else {
          u[j] = 0.0;
        }
      }
      else {
        u[j] = u[j+1] * b[elements[g][j+1]] * b[elements[g][j+1]];
      }
      for(int k1=0; k1<K; k1++) {
        for(int k2=0; k2<K; k2++) {
          int k12 = pair_to_index(k1,k2);
          if(k2<=k1) {
            // Update v
            if(j == groupSize-1) {
              if(g < nGroups-1) {
                v[j][k12] = a[elements[g+1][0]][k1] * a[elements[g+1][0]][k2];
              }
              else {
                v[j][k12] = 1.0;
              }
            }
            else {
              v[j][k12]  = v[j+1][k12];
              v[j][k12] += u[j+1]*a[elements[g][j+1]][k1]*a[elements[g][j+1]][k2];
              v[j][k12] += w[j+1][k1]*a[elements[g][j+1]][k2];
              v[j][k12] += w[j+1][k2]*a[elements[g][j+1]][k1];
            }
          }
        }
        // Update w
        if(j == groupSize-1) {
          if(g < nGroups-1) {
            w[j][k1] = b[elements[g+1][0]] * a[elements[g+1][0]][k1];
          }
          else {
            w[j][k1] = 0.0;
          }
        }
        else {
          w[j][k1]  = w[j+1][k1] * b[elements[g][j+1]];
          w[j][k1] += u[j+1] * a[elements[g][j+1]][k1] * b[elements[g][j+1]];
        }
      }
    }

    std::fill(C.begin(), C.end(), 0.0);
    std::fill(D.begin(), D.end(), 0.0);
    std::fill(Z.begin(), Z.end(), 0.0);
    if(g < nGroups-1) {
      // Precompute C and D for partition function
      int j0 = elements[g][0];
      double C_sum = 0.0;
      for(int k1=0; k1<K; k1++) {
        for(int k2=0; k2<=k1; k2++) {
          int k12 = pair_to_index(k1,k2);
          if(j0==0) {
            C[k12] = Q_bar(j0, k12, k12);
          }
          else {
            C[k12] = Q_bar(j0, k12, H[j0-1]) * Q_bar(j0, k12, Hk[j0-1]) / Z_old[k12];
          }
          D[k1] += C[k12];
          D[k2] += C[k12];
          C_sum += C[k12];
        }
      }

      // Compute partition function (FIXME: update this)
      double Z_norm = 0;
      for(int k1=0; k1<K; k1++) {
        for(int k2=0; k2<=k1; k2++) {
          int k12 = pair_to_index(k1,k2);
          Z[k12]  = (2.0-(k1==k2)) * v[0][k12] * C_sum;
          Z[k12] += (w[0][k1]*D[k2] + w[0][k2]*D[k1]) / (1.0 + (k1==k2));
          Z[k12] += u[0] * C[k12];
          Z_norm += Z[k12];
        }
      }

      // Normalize partition function and make sure we avoid division by zero
      for(int k1=0; k1<K; k1++) {
        for(int k2=0; k2<=k1; k2++) {
          int k12 = pair_to_index(k1,k2);
          Z[k12] /= Z_norm;
          if(Z[k12] < Z_min) {
            Z[k12] = Z_min;
          }
        }
      }

    }

    // Compute sampling weights
    for(int j=0; j<groupSize; j++) {
      std::fill(weights.begin(), weights.end(), 1.0);
      double weights_sum = 0;
      for(int k=0; k<nStates; k++) {
        // Compute first part of transition matrix
        if(j==0) {
          if( g==0 ) {
            weights[k] = Q_bar(elements[g][0], k, 0);
          }
          else {
            int H0  = H[elements[g-1].back()];
            int H0k = Hk[elements[g-1].back()];
            weights[k] = Q_bar(elements[g][0], k, H0) * Q_bar(elements[g][0], k, H0k) / Z_old[k];
          }
        }
        else {
          weights[k] = Q_bar(elements[g][j], k, Hk[elements[g][j-1]]);
        }

        // Compute second part of transition matrix
        if(g < nGroups-1) {
          weights[k] *= V_bar(H[elements[g+1][0]], k, v[j], w[j], u[j]);
        }
        else {
          weights[k] *= V_bar(0, k, v[j], w[j], u[j]);
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

void GroupGenotypes::emission(const std::vector<int> & Hk) {
  std::vector<double> weights3(3,1.0);
  for(int j=0; j<p; j++) {
    weights3[0] = emission_prob(j, 0, Hk[j]);
    weights3[1] = emission_prob(j, 1, Hk[j]);
    weights3[2] = emission_prob(j, 2, Hk[j]);
    Xk[j] = weighted_choice(dis(gen),weights3);
  }
}

std::vector<int> GroupGenotypes::sample(const std::vector<int> & X) {
  sampleViterbi(X);
  knockoffMC(H);
  emission(Hk);
  return(Xk);
}

imatrix GroupGenotypes::sample(const std::vector<std::vector<int> > & X) {
  unsigned int n = X.size();
  imatrix XkMatrix(n, std::vector<int>(p));
  for(unsigned int i=0; i<X.size(); i++) {
    XkMatrix[i] = sample(X[i]);
  }
  return(XkMatrix);

}

#endif
