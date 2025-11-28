#------------------------------------------------------------------------------#
#--------------------------------- skm::skm.r ---------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ main ------------------------------------#
#------------------------------------------------------------------------------#

# skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must)

#' skm_minmax_cpp
#' @description
#'  skm via min-max on in cpp - subroutine of skm_sgl_cpp calls
#' @details
#'  skm_minmax_cpp init an input m x n matrix x, and a priority vector s_must
#'   would select n indicies from m such that:
#'
#'  minimize sum(min(x(i, j) where i <1..n> and j <1..n> each use <1..n> once))
#'
#' so in case m <= n it simply select all m - should always be apply on matrix
#' with m > n - it is designed as a expectation step in skm_cpp on updating s.
#'
#' it select i in <1..m> such that i has the colwise_min_idx on column j where
#' j has max difference of (colwise_max_val - colwise_min_val), it then remove
#' row i col j from matrix and repeat.
#'
#' s_must presents the indices with priority so that the selection must select
#' first indicies within s_must and then select other indicies outside s_must.
#'
#' an example skm_minmax_cpp is superior in bound worst case compare to greedy:
#' x = [1 100; 4 200; 2 400; 9 900]: greedy 1 then 200, min-max 100 then 2, and
#' greedy give [1 100; 4 200] with 201 and minmax give [1 100; 2 400] with 102.
#' @param x
#'  an m x n matrix often m > n
#' @param s_must
#'  matrix x row index start from 0 that must be selected with priority
"skm_minmax_cpp"


# class skmSolution {
#
#   public:
#
#     double o; arma::uvec s;
#
#   // .constructor
#   skmSolution(double o, arma::uvec s) : o(o), s(s) {}
#
# };

# #' skmSolution
# #' @description
# #'  skm solution often returned via skm solver implemented in cpp
# #' @param o
# #'  objective sum(min(x.subview(i in s, all j), min over all i), sum over all j)
# #' @param s
# #'  selected index set of row index start from 0
# #' @param o_list
# #'  skm often run with random start point, o_list store objective of each run.
# #' @param s_list
# #'  skm often run with random start point, s_list store index set of each run.
# "skmSolution"

#' skmSolution
#'
#' @description
#'
#'  class skmSolution, which often returned via skm solver implemented in cpp
#'
#' @details
#'
#'  an skmSolution instance has two member variable:
#'
#'   o: objective sum(min(x.subview(i in s, all j), min over all i), sum over all j)
#'
#'   s: selected index set of row index start from 0
#'
"skmSolution"

# #' Rcpp_skmSolution
# #' @description
# #'  skm solution often returned via skm solver implemented in cpp
# #' @param o
# #'  objective sum(min(x.subview(i in s, all j), min over all i), sum over all j)
# #' @param s
# #'  selected index set of row index start from 0


# skmSolution skm_sgl_cpp(
#   const arma::mat& x, const arma::uvec s_init, const arma::uvec& s_must, const arma::uword max_it
# )

#' skm_sgl_cpp
#' @description
#'  solve skm with single and a fixed given s_init
#' @details
#'  a numeric m x n matrix x often m << n and want to select a subset of k from
#'  m such that it minimize the sum(min(x(i, j) - minimum w.r.t each j over all
#'  i within selected index set), over all i)
#'
#'  if m == n and x(i, j) as euclidean distance then it is equivalent to kmeans
#'
#'  skm can select a combined set for deploying resource, for example, where to
#'  build 5 warehouses on united states, which often different than build these
#'  warehouses via select the current best one by one.
#' @param x
#'  an m x n matrix often m < n, as a convention index rows of x with s, and
#'   cols of x with t so x(i, j) can be expressed as (s_i, t_j) equally.
#' @param s_init
#'  an init vector of k index to start the search of optimal index set of k,
#'   length of s_init also defined the number of index want to be select.
#' @param s_must
#'  an index vector set should be selected before selecting other index.
#' @param max_it
#'  max number of iterations can run for optimizing result.
#'   max number of iterations within a single initial run on optimal path.
#' @return skmSolution
#' @family skm
#' @export
"skm_sgl_cpp"


# skmSolution skm_rgi_cpp(
#   const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it
# )

#' skm_rgi_cpp
#' @description
#'  solve skm with single and random size k s_init
#' @details
#'  refer skm_sgl_cpp
#' @inheritParams skm_sgl_cpp
#' @param k
#'  number of index to be selected from x row index start from 0.
#' @return skmSolution
#' @family skm
#' @export
"skm_rgi_cpp"


# Rcpp::List skm_mlp_cpp(
#   const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it, const arma::uword max_at
# )

#' skm_mlp_cpp
#' @description
#'  solve skm with multiple runs in serial and return all w. optim
#' @details
#'  refer skm_sgl_cpp
#' @inheritParams skm_sgl_cpp
#' @param k
#'  number of index to be selected from x row index start from 0.
#' @param max_at
#'  max number of attempts or repeats on running for optimial results,
#'   max number of random initialization for finding optimial results.
#' @return skmSolution
#'  skmSolution present in r list
#' @family skm
#' @export
"skm_mlp_cpp"


# skmSolution skm_rgs_cpp(
#   const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it
# )

#' skm_rgs_cpp
#' @description
#'  solve skm with single and random size k s_init stratified sampled w.r.t g
#' @details
#'  refer skm_sgl_cpp
#' @inheritParams skm_sgl_cpp
#' @param k
#'  number of index to be selected from x row index start from 0.
#' @param g
#'  stratify structure, often info on grouping of v so that algorithm should
#'   make random initialization from stratified sample across groups.
#' @return skmSolution
#' @family skm
#' @export
"skm_rgs_cpp"


# Rcpp::List skm_mls_cpp(
#   const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it, const arma::uword max_at
# )

#' skm_mls_cpp
#' @description
#'  solve skm with multiple runs in serial and return all w. optim
#'   and s_init stratified sampled w.r.t g
#' @details
#'  refer skm_sgl_cpp
#' @inheritParams skm_sgl_cpp
#' @param k
#'  number of index to be selected from x row index start from 0.
#' @param g
#'  stratify structure, often info on grouping of v so that algorithm should
#'   make random initialization from stratified sample across groups.
#' @param max_at
#'  max number of attempts or repeats on running for optimial results,
#'   max number of random initialization for finding optimial results.
#' @return skmSolution
#'  skmSolution present in r list
#' @family skm
#' @export
"skm_mls_cpp"


# Rcpp::List skmRpl_mlp_cpp(
#   const NumericMatrix x,
#   const unsigned int  k,
#   const IntegerVector s_must,
#   const unsigned int  max_it,
#   const unsigned int  max_at,
#   const unsigned int  skmRpl_GS = 100
# )

#' skmRpl_mlp_cpp
#' @description
#'  solve skm with multiple runs in parallel
#' @details
#'  refer skm_sgl_cpp
#' @inheritParams skm_sgl_cpp
#' @param k
#'  number of index to be selected from x row index start from 0.
#' @param max_at
#'  max number of attempts or repeats on running for optimial results,
#'   max number of random initialization for finding optimial results.
#' @param skmRpl_GS
#'  skmRpl_GS: RcppParallel grain size when run skmRpl_mlp_cpp
#' @return skmSolution
#'  skmSolution present in r list
#' @family skmRpl
#' @export
"skmRpl_mlp_cpp"


# void skmRpl_rgi_cpp(
#   RcppParallel::RMatrix<double>::iterator it_x_begin,
#   RcppParallel::RVector<int>::const_iterator it_arg_begin,
#   RcppParallel::RVector<double>::iterator it_o_ith,
#   RcppParallel::RMatrix<int>::Row::iterator it_s_ith_begin
# )

# #' skmRpl_rgi_cpp
# #' @description
# #'  subroutine for skmRpl struct with output fill in_situ
# #' @family skmRpl
# #' @export
# "skmRpl_rgi_cpp"


#------------------------------------------------------------------------------#
