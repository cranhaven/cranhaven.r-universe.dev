#include "trans_shortsimplex.h"

void trans_shortsimplex(vectorI & mass_a, vectorI & mass_b, refMat cost_matrix, 
                        matrixI & assign_mat, matrixI &  basis_mat) {
  
  int N = mass_a.size();
  // int * NN;
  // NN = &N;
  
  int M = mass_b.size();
  // int * MM;
  // MM = &M;
  
  int * a = mass_a.data();
  int * b = mass_b.data();
  
  double * costm = cost_matrix.data();
  
  int slength = std::min(M, 
                         15 + std::max(0, 
                                       int(std::floor(15.0 * 
                                           std::log(double(M)/400.0)/std::log(2.0)))));
  if (slength > M) {
    slength = M;
    Rcpp::warning("Shortlist parameter 'slength' too large...  decreased to maximal value.");
  }
  
  int kfound = slength; 
  double psearched = 0.05;
  int * assignment = assign_mat.data();
  int * basis = basis_mat.data();
  
  shortsimplex( &slength, &kfound, &psearched,
                &N, &M, a, b, costm, assignment, basis);
  
}
