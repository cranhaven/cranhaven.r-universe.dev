#ifndef __SKM__
#define __SKM__


#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;
// using namespace RcppParallel;


//' stratified_sampling
//' @description
//'  select k elements from vector v w.r.t stratify structure group g.
//'  TODO - implementing via template so v is flexible as vec or uvec.
//' @param v <vector>
//'  v: a numeric candidate v from which draw sample.
//' @param k <integer>
//'  k: selection sample size.
//' @param g <vector>
//'  g: stratify structure g - info on grouping of v so that the selected sample is stratified across groups.
//' @return s <vector>
//'  s: a vector select from v length k stratified by g.
//' @note
//'  v is required as an integer vector for using in skm
//' @export
// [[Rcpp::export]]
arma::uvec stratified_sampling(const arma::uvec& v, const arma::uword k, const arma::uvec& g);


// <http://www.cplusplus.com/doc/tutorial/classes/>
// expression	can be read as
// *x	    object pointed to by pointer x
// &x	    address of object x
// x.y	  member y of object x
// x->y	  member y of object pointed to by pointer x
// (*x).y	member y of object pointed to by pointer x (equivalent to the previous one)
// x[0]   first object pointed to by pointer array x
// x[1]   second object pointed to by pointer array x
// x[n]   (n+1)th object pointed to by pointer array x


class skmSolution {

public:

  double o; arma::uvec s;

  // .constructor
  skmSolution(double o, arma::uvec s) : o(o), s(s) {}

};

// skm_minmax_cpp: skm via min-max on in cpp - subroutine of skm_sgl_cpp calls
skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must);

// skm_sgl_cpp: solve skm with single and a fixed given s_init
skmSolution skm_sgl_cpp(const arma::mat& x, const arma::uvec s_init, const arma::uvec& s_must, const arma::uword max_it);

// skm_rgi_cpp: solve skm with single and random size k s_init
skmSolution skm_rgi_cpp(const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it);

// skm_rgs_cpp: solve skm with single and random size k s_init stratified sampled w.r.t g
skmSolution skm_rgs_cpp(const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it);

// skm_mlp_cpp: solve skm with multiple runs in serial and return all w. optim
// [[Rcpp::export]]
Rcpp::List skm_mlp_cpp(
    const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it, const arma::uword max_at
);

// skm_mls_cpp: solve skm with multiple runs in serial and return all w. optim and s_init stratified sampled w.r.t g
// [[Rcpp::export]]
Rcpp::List skm_mls_cpp(
    const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it, const arma::uword max_at
);

// skmRpl_rgi_cpp: subroutine for skmRpl struct with output fill in_situ
void skmRpl_rgi_cpp(
    RcppParallel::RMatrix<double>::iterator it_x_begin,
    RcppParallel::RVector<int>::const_iterator it_arg_begin,
    RcppParallel::RVector<double>::iterator it_o_ith,
    RcppParallel::RMatrix<int>::Row::iterator it_s_ith_begin
);

// skmRpl extend RcppParallel::Worker for paralleFor calls
// struct skmRpl : public RcppParallel::Worker

// skmRpl_mlp_cpp: solve skm with multiple runs in parallel
// skmRpl_mlp_cpp is exported from skm.h due to set default value on skmRpl_GS
// [[Rcpp::export]]
Rcpp::List skmRpl_mlp_cpp(
    const NumericMatrix x,
    const unsigned int  k,
    const IntegerVector s_must,
    const unsigned int  max_it,
    const unsigned int  max_at,
    const unsigned int  skmRpl_GS = 100
);

// rcpp class and module -> r

RCPP_EXPOSED_CLASS(skmSolution);

// RCPP_MODULE(skm_module) {
//
//   using namespace Rcpp;
//
//   class_<skmSolution>( "skmSolution" )
//
//     .constructor<double, arma::uvec>()
//
//     .field( "o", &skmSolution::o )
//     .field( "s", &skmSolution::s )
//     ;
//
//   function("skm_minmax_cpp", &skm_minmax_cpp, "skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must)");
//
//   function("skm_sgl_cpp", &skm_sgl_cpp, "skmSolution skm_sgl_cpp(const arma::mat& x, const arma::uvec s_init, const arma::uvec& s_must, const arma::uword max_it)");
//
//   function("skm_rgi_cpp", &skm_rgi_cpp, "skmSolution skm_rgi_cpp(const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it)");
//
//   function("skm_rgs_cpp", &skm_rgs_cpp, "skmSolution skm_rgs_cpp(const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it)");
//
// }

RCPP_MODULE(skm_module) {

  using namespace Rcpp;

  Rcpp::class_<skmSolution>( "skmSolution" )

    .constructor<double, arma::uvec>()

    .field( "o", &skmSolution::o )
    .field( "s", &skmSolution::s )
    ;

  function(
    "skm_minmax_cpp", &skm_minmax_cpp, Rcpp::List::create(_["x"], _["s_must"]),
    "skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must)"
  );

  function(
    "skm_sgl_cpp", &skm_sgl_cpp, Rcpp::List::create(_["x"], _["s_init"], _["s_must"], _["max_it"]),
    "skmSolution skm_sgl_cpp(const arma::mat& x, const arma::uvec s_init, const arma::uvec& s_must, const arma::uword max_it)"
  );

  function(
    "skm_rgi_cpp", &skm_rgi_cpp, Rcpp::List::create(_["x"], _["k"], _["s_must"], _["max_it"]),
    "skmSolution skm_rgi_cpp(const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it)"
  );

  function(
    "skm_rgs_cpp", &skm_rgs_cpp, Rcpp::List::create(_["x"], _["k"], _["g"], _["s_must"], _["max_it"]),
    "skmSolution skm_rgs_cpp(const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it)"
  );

}

#endif // __SKM__
