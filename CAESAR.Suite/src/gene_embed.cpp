#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace arma;
using namespace std;
// using boost::math::tools::brent_find_minima;

// [[Rcpp::export]]
arma::mat gene_embed_weight_cpp(
    const arma::mat &X, const arma::mat &ce_cell, const arma::sp_mat &adj, const double c = 1.0)
{
  // X is a gene by cell expression matrix
  // adj is a sparse matrix, each row corresponding to a cell

  arma::mat indicatorX = X;
  indicatorX.for_each([](sp_mat::elem_type &val)
                      { val = (val != 0 ? 1 : 0); });
  arma::mat n_express = adj * indicatorX.t() + c;
  arma::vec n_neighbor = arma::sum(adj, 1) + c;
  n_express.each_col() /= n_neighbor;

  arma::mat weight = X % n_express.t();
  colvec sumW = sum(weight, 1);
  weight.each_col() /= sumW;

  return weight * ce_cell;
}

// [[Rcpp::export]]
Rcpp::List wpcaCpp(const arma::mat &X, const int &nPCs, const bool &weighted = true)
{
  arma::mat U, V;
  arma::vec s;
  arma::mat PCs, loadings;
  svd_econ(U, s, V, X);
  PCs = U.cols(0, nPCs - 1) * diagmat(s.subvec(0, nPCs - 1));
  loadings = V.cols(0, nPCs - 1);
  arma::mat dX = PCs * loadings.t() - X;
  arma::rowvec Lam_vec = mean(dX % dX);
  if (weighted)
  {
    svd_econ(U, s, V, X * diagmat(1.0 / sqrt(Lam_vec)));
    // vec s2 =  s % s; // s; //
    arma::mat loadings_unscale = diagmat(sqrt(Lam_vec)) * V.cols(0, nPCs - 1);
    arma::mat V1;
    arma::vec s1;
    svd_econ(loadings, s1, V1, loadings_unscale);
    PCs = U.cols(0, nPCs - 1) * diagmat(s.subvec(0, nPCs - 1)) * V1 * diagmat(s1);
    dX = PCs * loadings.t() - X;
    Lam_vec = mean(dX % dX);
  }
  List output = List::create(
      Rcpp::Named("PCs") = PCs,
      Rcpp::Named("loadings") = loadings,
      Rcpp::Named("Lam_vec") = Lam_vec);

  return output;
}

//' @title
//' getneighborhood_fast
//' @description
//' an efficient function to find the neighborhood based on the matrix of position and a pre-defined cutoff
//'
//' @param x is a n-by-2 matrix of position.
//' @param radius is a threashold of Euclidean distance to decide whether a spot is an neighborhood of another spot. For example, if the Euclidean distance between spot A and B is less than cutoff, then A is taken as the neighbourhood of B.
//' @return A sparse matrix containing the neighbourhood
//'
//' @export
// [[Rcpp::export]]
arma::sp_umat getneighborhood_fastcpp(const arma::mat x, double radius)
{
  int N = x.n_rows;
  arma::sp_umat D(N, N);
  double dis;
  uvec idx, idx2;
  for (int j = 0; j < N - 1; ++j)
  {
    idx = find(abs(x(j, 0) - x.col(0)) < radius);
    idx2 = find(idx > j);
    int p = idx2.n_elem;
    for (int i = 0; i < p; ++i)
    {
      dis = norm(x.row(idx(idx2(i))) - x.row(j), 2);
      if (dis < radius)
      {
        D(idx(idx2(i)), j) = 1;
        D(j, idx(idx2(i))) = 1;
      }
    }
  }
  return D;
}

// [[Rcpp::export]]
arma::mat pdistance_cpp(const arma::mat &Ar, const arma::mat &Br, const float &eta = 1e-10)
{

  vec An = sum(Ar % Ar, 1);
  vec Bn = sum(Br % Br, 1);
  mat C = -2 * Ar * Br.t();
  C.each_col() += An;
  C.each_row() += Bn.t();
  return sqrt(C + eta);
}

// [[Rcpp::export]]
arma::mat gene_embed_cpp(const arma::mat &X, const arma::mat &ce_cell)
{
  // X is a gene by cell expression matrix
  mat weight = X;
  colvec sumW = sum(weight, 1);
  weight.each_col() /= sumW;

  return weight * ce_cell;
}

// Define global variables
// double X_count_ij, a_i, invLambda_j,Mu_x_ij;
/*
 * Auxiliary
 */
//' @keywords internal
//' @noRd
//'
// Calculate the weighted matrix based on distance.
// [[Rcpp::export]]
arma::sp_mat weightAdj(const arma::mat &pos, const arma::mat &img_embed,
                       const double &radius, const double &width)
{
  int N = pos.n_rows;
  arma::sp_mat D(N, N);
  double dis, dis_img;
  uvec idx, idx2;
  for (int j = 0; j < N - 1; ++j)
  {
    idx = find(abs(pos(j, 0) - pos.col(0)) < radius);
    idx2 = find(idx > j);
    int p = idx2.n_elem;
    for (int i = 0; i < p; ++i)
    {
      dis = norm(pos.row(idx(idx2(i))) - pos.row(j), 2);
      if (dis < radius)
      {
        dis_img = norm(img_embed.row(idx(idx2(i))) - img_embed.row(j), 2);
        D(idx(idx2(i)), j) = exp(-dis_img * dis_img / width);
        D(j, idx(idx2(i))) = exp(-dis_img * dis_img / width);
      }
    }
  }
  return D;
}

// [[Rcpp::export]]
arma::sp_mat getneighbor_weightmat(const arma::mat x, const double &radius, const double &width)
{
  int N = x.n_rows;
  arma::sp_mat D(N, N);
  double dis;
  uvec idx, idx2;
  for (int j = 0; j < N - 1; ++j)
  {
    idx = find(abs(x(j, 0) - x.col(0)) < radius);
    idx2 = find(idx > j);
    int p = idx2.n_elem;
    for (int i = 0; i < p; ++i)
    {
      dis = norm(x.row(idx(idx2(i))) - x.row(j), 2);
      if (dis < radius)
      {
        D(idx(idx2(i)), j) = exp(-dis / width);
        D(j, idx(idx2(i))) = exp(-dis / width);
      }
    }
  }
  return D;
}
