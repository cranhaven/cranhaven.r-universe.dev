#include "skm.h"
#include "matrix_minmax.h"


// //' stratified_sampling
// //' @description
// //'  select k elements from vector v w.r.t stratify structure group g.
// //'  TODO - implementing via template so v is flexible as vec or uvec.
// //' @param v <vector>
// //'  v: a numeric candidate v from which draw sample.
// //' @param k <integer>
// //'  k: selection sample size.
// //' @param g <vector>
// //'  g: stratify structure g - info on grouping of v so that the selected sample is stratified across groups.
// //' @return s <vector>
// //'  s: a vector select from v length k stratified by g.
// //' @note
// //'  v is required as an integer vector for using in skm
// //' @export
// //[[Rcpp::export]]
arma::uvec stratified_sampling(const arma::uvec& v, const arma::uword k, const arma::uvec& g) {

  // Rcout << "stratified_sampling: check input - v: " << v.t() << std::endl;
  // Rcout << "stratified_sampling: check input - k: " << k << std::endl;
  // Rcout << "stratified_sampling: check input - g: " << g.t() << std::endl;

  // check k <= v.size()
  if ( k > v.size() ) { stop("stratified_sampling: k must <= v.size().\n"); }

  if ( g.size() != v.size() ) { stop("stratified_sampling: must have g.size() == v.size().\n"); }

  // special case k == 1 or k == v.size()
  if ( k == 1 ) { return RcppArmadillo::sample(v, 1, false); }

  if ( k == v.size() ) { return v; }

  // g_g: index of each stratification
  arma::uvec g_g = arma::unique(g);

  // Rcout << "g_g: " << g_g.t() << std::endl;

  // n: number of stratification in total
  arma::uword n = g_g.size();

  // Rcout << "n: " << n << std::endl;

  // special case g_g.size() == 1 no stratification
  if ( n == 1 ) { return RcppArmadillo::sample(v, k, false); }

  // quotient + remainder stratified sampling method
  // TODO: more complicate percentage of total style

  // v_g: v value belong to each group
  arma::field<arma::uvec> v_g(n);

  // k_g: select k_g from each group g with a total k
  arma::uvec k_g = arma::zeros<arma::uvec>(n);

  // r_g: number of remainder r w.r.t on each group g
  arma::uvec r_g(n);

  for ( arma::uword i = 0; i < n; i++) {

    v_g(i) = find(g == g_g(i));

    r_g(i) = v_g(i).size();

  }

  // Rcout << "v_g: " << v_g << std::endl;
  // Rcout << "k_g: " << k_g.t() << std::endl;
  // Rcout << "r_g: " << r_g.t() << std::endl;

  // init q and r - q as a variable version of k
  arma::uword q(k);

  // Rcout << "q: " << q << std::endl;

  arma::uvec q_g = arma::zeros<arma::uvec>(n);

  arma::uvec r_g_p = find(r_g > 0);

  // Rcout << "r_g_p: " << r_g_p.t() << std::endl;

  // q / n - quotient
  while ( q / r_g_p.size() > 0 ) {

    q_g(r_g_p).fill(q / r_g_p.size());

    // Rcout << "q_g: " << q_g.t() << std::endl;

    q = q % r_g_p.size();

    // Rcout << "q: " << q << std::endl;

    for ( arma::uword i = 0; i < r_g_p.size(); i++ ) {

      if ( q_g(r_g_p(i)) > r_g(r_g_p(i)) ) {

        q += q_g(r_g_p(i)) - r_g(r_g_p(i));

        q_g(r_g_p(i)) = r_g(r_g_p(i));

        r_g(r_g_p(i)) = 0;

      } else {

        r_g(r_g_p(i)) = r_g(r_g_p(i)) - q_g(r_g_p(i));

      }

      k_g(r_g_p(i)) += q_g(r_g_p(i));

    }

    // Rcout << "q_g: " << q_g.t() << std::endl;
    // Rcout << "k_g: " << k_g.t() << std::endl;
    // Rcout << "r_g: " << r_g.t() << std::endl;

    // Rcout << "q: " << q << std::endl;

    r_g_p = find(r_g > 0);

    // Rcout << "r_g_p: " << r_g_p.t() << std::endl;

    q_g.fill(0); // immaculatism and protection!

  }

  // q % n - remainder

  arma::uvec r_g_idx = RcppArmadillo::sample(r_g_p, q % r_g_p.size(), false);

  // Rcout << "r_g_idx: " << r_g_idx.t() << std::endl;

  k_g(r_g_idx) += 1;

  // Rcout << "k_g: " << k_g.t() << std::endl;

  // select k_g from each v_g into s
  arma::uvec s;

  for (arma::uword i = 0; i < n; i++) {

    s = arma::join_cols(s, RcppArmadillo::sample(v_g(i), k_g(i), false));

  }

  // Rcout << "stratified_sampling: construct output - s: " << s.t() << std::endl;

  return s;

}


// skm_minmax_cpp: skm via min-max on in cpp - subroutine of skm_sgl_cpp calls
// skm_minmax_cpp with an input m x n matrix: objective is to select n of m st
// minimize sum(min(<i, j> where i <1..n> and j <1..n> each use <1..n> once)).
// so in case m <= n it simply select all m - should always be apply on matrix
// with m > n - it is designed as a expectation step in skm_cpp on updating s.
// it select i in <1..m> such that i has the colwise_min_idx on column j where
// j has max difference of (colwise_max_val - colwise_min_val), it then remove
// row i col j from matrix and repeat.
// example skm_minmax_cpp is superior in bouding worst case compare to greedy:
// x = [1 100; 4 200; 2 400; 9 900]: greedy 1 then 200, min-max 100 then 2, so
// greedy give [1 100; 4 200] with 201 and minmax give [1 100; 2 400] with 102
skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must) {

  // Rcout << "skm_minmax_cpp: check input x: " << std::endl << x << std::endl;
  // Rcout << "skm_minmax_cpp: check input s_must: " << s_must.t() << std::endl;

  if ( x.n_rows < x.n_cols ) { stop("x must a matrix m x n with m >= n.\n"); }

  arma::uvec s = arma::zeros<arma::uvec>(x.n_cols);

  arma::uvec t = arma::zeros<arma::uvec>(x.n_cols);

  double o = 0;

  arma::uvec ulmt = arma::linspace<arma::uvec>(0, x.n_rows - 1, x.n_rows);

  arma::uvec vlmt = arma::linspace<arma::uvec>(0, x.n_cols - 1, x.n_cols);

  // push s_must w priority

  arma::uvec u_must(s_must.begin(), s_must.size());

  for (arma::uword i = 0; i < x.n_cols; i++) {

    arma::vec v = arma::zeros<arma::vec>(x.n_cols);

    if ( u_must.size() > 0 ) {

      // v - cost of enforcing s_must into s list
      for (arma::uvec::iterator jt = vlmt.begin(); jt != vlmt.end(); jt++) {

        v(*jt) = col_min_val(x.col(*jt), u_must) - col_min_val(x.col(*jt), ulmt);

      }

      t(i) = col_min_idx(v, vlmt);

      vlmt = vlmt(arma::find(vlmt != t(i)));

      s(i) = col_min_idx(x.col(t(i)), u_must);

      u_must = u_must(arma::find(u_must != s(i)));

      ulmt = ulmt(arma::find(ulmt != s(i)));

    } else {

      // v - potential benefit of chosen s w. min
      for (arma::uvec::iterator jt = vlmt.begin(); jt != vlmt.end(); jt++) {

        v(*jt) = col_rgn_val(x.col(*jt), ulmt);

      }

      t(i) = col_max_idx(v, vlmt);

      vlmt = vlmt(arma::find(vlmt != t(i)));

      s(i) = col_min_idx(x.col(t(i)), ulmt);

      ulmt = ulmt(arma::find(ulmt != s(i)));

    }

  }

  for (arma::uword i = 0; i < x.n_cols; i++) { o += x(s(i), t(i)); }

  return skmSolution(o, s);
}


// skm_sgl_cpp: solve skm with single and a fixed given s_init
skmSolution skm_sgl_cpp(
  const arma::mat& x, const arma::uvec s_init, const arma::uvec& s_must, const arma::uword max_it
) {

  // check input
  // Rcout << "skm_sgl_cpp: check input x: " << std::endl << x << std::endl;
  // Rcout << "skm_sgl_cpp: check input s_init: " << s_init.t() << std::endl;
  // Rcout << "skm_sgl_cpp: check input s_must: " << s_must.t() << std::endl;
  // Rcout << "skm_sgl_cpp: check input max_it: " << max_it << std::endl;

  // init output
  arma::uvec s(s_init.begin(), s_init.size());

  arma::uvec t(x.n_cols);

  double o = std::numeric_limits<double>::max();

  skmSolution a_skmSolution(o, s);

  // init while loop
  arma::uword num_it = 0;

  bool archive_optim = false;

  arma::mat gx(x.n_rows, s.size());

  while ( !archive_optim && (num_it < max_it) ) {

    // Rcout << "skm_sgl_cpp: while loop num_it: " << num_it << std::endl;

    // minimization step - assign t into s
    for (arma::uword j = 0; j < x.n_cols; j++) {

      t(j) = col_min_idx(x.col(j), s);

    }

    // Rcout << "skm_sgl_cpp: while loop min-step t: " << t.t() << std::endl;

    // aexpectation step - update s with t
    for (arma::uword i = 0; i < s.size(); i++) {

      arma::uvec g = arma::find(t == s(i));

      if ( g.size() > 0 ) {

        gx.col(i) = sum(x.cols(g), 1);

      } else {

        gx.col(i) = sum(x, 1) / (s.size() * 10.0);

      }

    }

    // Rcout << "gx :" << std::endl << gx << std::endl;

    a_skmSolution = skm_minmax_cpp(gx, s_must);

    archive_optim = ! (a_skmSolution.o < o);

    o = a_skmSolution.o;

    s = a_skmSolution.s;

    // Rcout << "skm_sgl_cpp: while loop exe-step o: " << o << std::endl;
    // Rcout << "skm_sgl_cpp: while loop exe-step s: " << s.t() << std::endl;

    num_it++;

  }

  // Rcout << "skm_sgl_cpp: check output o: " << a_skmSolution.o << std::endl;
  // Rcout << "skm_sgl_cpp: check output s: " << a_skmSolution.s.t() << std::endl;

  return a_skmSolution;
}


// skm_rgi_cpp: solve skm with single and random size k s_init
skmSolution skm_rgi_cpp(
  const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it
) {

  // skm_rgi_cpp is a sepcialize version of skm_rgs_cpp

  // arma::uvec g = arma::ones<arma::uvec>(x.n_rows);

  // return skm_rgs_cpp(x, k, g, s_must, max_it);

  // create s_init w k and s_must
  arma::uvec ulmt = arma::linspace<arma::uvec>(0, x.n_rows - 1, x.n_rows);

  arma::uvec s_init;

  if ( s_must.size() > k ) {

    s_init = RcppArmadillo::sample(s_must, k, false);

  } else {

    for (arma::uword i = 0; i < s_must.size(); i++) {

      ulmt = ulmt(arma::find(ulmt != s_must(i)));

    }

    s_init = join_cols(s_must, RcppArmadillo::sample(ulmt, k - s_must.size(), false));

  }

  // Rcout << "skm_rgi_cpp - construct s_init: " << s_init.t() << std::endl;

  return skm_sgl_cpp(x, s_init, s_must, max_it);

}


// skm_mlp_cpp: solve skm with multiple runs in serial and return all w. optim
// // [[Rcpp::export]]
Rcpp::List skm_mlp_cpp(
  const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it, const arma::uword max_at
) {

  // check input
  // Rcout << "skm_mlp_cpp: check input list x: " << std::endl << x << std::endl;
  // Rcout << "skm_mlp_cpp: check input list k: " << k << std::endl;
  // Rcout << "skm_mlp_cpp: check input list s_must: " << s_must.t() << std::endl;
  // Rcout << "skm_mlp_cpp: check input list max_it: " << max_it << std::endl;
  // Rcout << "skm_mlp_cpp: check input list max_at: " << max_at << std::endl;

  // init output
  arma::vec o_list(max_at);

  arma::umat s_list(max_at, k);

  double o;

  arma::uvec s;

  // init while loop
  arma::uword num_at = 0;

  while ( num_at < max_at ) {

    skmSolution a_skmSolution = skm_rgi_cpp(x, k, s_must, max_it);

    o_list(num_at) = a_skmSolution.o;

    s_list.row(num_at) = a_skmSolution.s.t();

    num_at++;
  }

  arma::uword optim_at;

  o = o_list.min(optim_at);

  s = s_list.row(optim_at).t();

  return Rcpp::List::create(
    Rcpp::Named("o") = o,
    Rcpp::Named("s") = s,
    Rcpp::Named("o_list") = o_list,
    Rcpp::Named("s_list") = s_list
  );

}


// skm_rgs_cpp: solve skm with single and random size k s_init stratified sampled w.r.t g
skmSolution skm_rgs_cpp(
  const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it
) {

  // create s_init w k, g and s_must
  arma::uvec ulmt = arma::linspace<arma::uvec>(0, x.n_rows - 1, x.n_rows);

  arma::uvec s_init;

  if ( s_must.size() > k ) {

    arma::uvec g_must = g(ulmt(s_must));

    s_init = stratified_sampling(s_must, k, g_must);

  } else {

    for (arma::uword i = 0; i < s_must.size(); i++) {

      ulmt = ulmt(arma::find(ulmt != s_must(i)));

    }

    s_init = join_cols(s_must, stratified_sampling(ulmt, k - s_must.size(), g(ulmt)));

  }

  // Rcout << "skm_rgs_cpp - construct s_init: " << s_init.t() << std::endl;

  return skm_sgl_cpp(x, s_init, s_must, max_it);
}


// skm_mls_cpp: solve skm with multiple runs in serial and return all w. optim and s_init stratified sampled w.r.t g
// // [[Rcpp::export]]
Rcpp::List skm_mls_cpp(
  const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it, const arma::uword max_at
) {

  // check input
  // Rcout << "skm_mls_cpp: check input list x: " << std::endl << x << std::endl;
  // Rcout << "skm_mls_cpp: check input list k: " << k << std::endl;
  // Rcout << "skm_mls_cpp: check input list g: " << g.t() << std::endl;
  // Rcout << "skm_mls_cpp: check input list s_must: " << s_must.t() << std::endl;
  // Rcout << "skm_mls_cpp: check input list max_it: " << max_it << std::endl;
  // Rcout << "skm_mls_cpp: check input list max_at: " << max_at << std::endl;

  // init output
  arma::vec o_list(max_at);

  arma::umat s_list(max_at, k);

  double o;

  arma::uvec s;

  // init while loop
  arma::uword num_at = 0;

  while ( num_at < max_at ) {

    skmSolution a_skmSolution = skm_rgs_cpp(x, k, g, s_must, max_it);

    o_list(num_at) = a_skmSolution.o;

    s_list.row(num_at) = a_skmSolution.s.t();

    num_at++;
  }

  arma::uword optim_at;

  o = o_list.min(optim_at);

  s = s_list.row(optim_at).t();

  return Rcpp::List::create(
    Rcpp::Named("o") = o,
    Rcpp::Named("s") = s,
    Rcpp::Named("o_list") = o_list,
    Rcpp::Named("s_list") = s_list
  );

}


// skmRpl - solve skm in parallel style

// skmRpl_rgi_cpp: subroutine for skmRpl struct with output fill in_situ
void skmRpl_rgi_cpp(
  RcppParallel::RMatrix<double>::iterator it_x_begin,
  RcppParallel::RVector<int>::const_iterator it_arg_begin,
  RcppParallel::RVector<double>::iterator it_o_ith,
  RcppParallel::RMatrix<int>::Row::iterator it_s_ith_begin
) {

  // init
  RcppParallel::RVector<int>::const_iterator it1 = it_arg_begin;

  RcppParallel::RVector<double>::iterator it2 = it_o_ith;

  RcppParallel::RMatrix<int>::Row::iterator it3 = it_s_ith_begin;

  // construct argument list for calling skm_rgi_cpp
  arma::uword k(*it1); it1++;

  arma::uword max_it(*it1); it1++;

  int x_nrow(*it1); it1++;

  int x_ncol(*it1); it1++;

  arma::uvec s_must(*it1);

  if ( s_must.size() > 0 ) {

    for (arma::uword i = 0; i < s_must.size(); i++) {

      it1++; s_must(i) = *it1;

    }

  }

  arma::mat a_x(it_x_begin, x_nrow, x_ncol, false);

  // Rcout << "skmRpl_rgi_cpp: construct argument list x: " << std::endl << a_x << std::endl;
  // Rcout << "skmRpl_rgi_cpp: construct argument list k: " << k << std::endl;
  // Rcout << "skmRpl_rgi_cpp: construct argument list max_it: " << max_it << std::endl;
  // Rcout << "skmRpl_rgi_cpp: construct argument list s_must: " << s_must.t() << std::endl;

  skmSolution a_skmSolution = skm_rgi_cpp(a_x, k, s_must, max_it);

  *it2 = a_skmSolution.o;

  for (arma::uword i = 0; i < a_skmSolution.s.size() ; i++) {

    *it3 = a_skmSolution.s(i); it3++;

  }

  // Rcout << "skmRpl_rgi_cpp: check skm_rgi_cpp solution o: " << a_skmSolution.o << std::endl;
  // Rcout << "skmRpl_rgi_cpp: check skm_rgi_cpp solution s: " << a_skmSolution.s.t() << std::endl;

}


// skmRpl extend RcppParallel::Worker for paralleFor calls
struct skmRpl : public RcppParallel::Worker {

  // x - input matrix with s <source> in row, t <target> in col and d <distance> cell
  RcppParallel::RMatrix<double> x; // remove specifier const so construct arma::mat is allowed

  // o - output vector o - objective w.r.t optim s - source on each run
  RcppParallel::RVector<double> o;

  // s - output matrix s - optim s - source found on each run
  RcppParallel::RMatrix<int> s;

  // arg - argument list with k, max_it, x.nrow(), x.ncol(), s_must.size and s_must vector
  const RcppParallel::RVector<int> arg;

  // .constructor convert input/output into RMatrix/RVector type for RcppParallel
  skmRpl(const NumericMatrix& x, NumericVector& o, IntegerMatrix& s, const IntegerVector& arg)
    : x(x), o(o), s(s), arg(arg) {}

  // parallel calls to skm_rgi_rpl
  void operator()(std::size_t begin, std::size_t end) {

    // -> going to be a ParallelFor!
    for (std::size_t i = begin; i < end; i++) {

      // check for user interrupts
      // Rcpp::checkUserInterrupt();

      // Rcout << "skmRpl: schedule i: " << i << std::endl;

      skmRpl_rgi_cpp(x.begin(), arg.begin(), o.begin() + i, s.row(i).begin());

    }

  }

};


// skmRpl_mlp_cpp: solve skm with multiple runs in parallel
// skmRpl_mlp_cpp is exported from skm.h due to set default value on skmRpl_GS
Rcpp::List skmRpl_mlp_cpp(
  const NumericMatrix x,
  const unsigned int  k,
  const IntegerVector s_must,
  const unsigned int  max_it,
  const unsigned int  max_at,
  const unsigned int  skmRpl_GS
) {

  // Rcout << "skmRpl_mlp_cpp: check input x: " << std::endl << x << std::endl;
  // Rcout << "skmRpl_mlp_cpp: check input k: " << k << std::endl;
  // Rcout << "skmRpl_mlp_cpp: check input s_must: " << s_must << std::endl;
  // Rcout << "skmRpl_mlp_cpp: check input max_it: " << max_it << std::endl;
  // Rcout << "skmRpl_mlp_cpp: check input max_at: " << max_at << std::endl;

  // construct output matrix
  NumericVector o_list(max_at);

  IntegerMatrix s_list(max_at, k);

  // construct argument list
  IntegerVector arg(5 + s_must.size());

  arg(0) = k;

  arg(1) = max_it;

  arg(2) = x.nrow();

  arg(3) = x.ncol();

  arg(4) = s_must.size();

  if ( s_must.size() > 0 ) {

    for (unsigned int i = 0; i < s_must.size(); i++) {

      arg(5 + i) = s_must(i);

    }

  }

  // Rcout << "skm_mlp_cpp - construct argument list arg: " << arg << std::endl;

  // create skmRpl object on parallel running
  skmRpl a_skmRpl(x, o_list, s_list, arg);

  // set up grain size to protect from crashes
  RcppParallel::parallelFor(0, max_at, a_skmRpl, skmRpl_GS);

  // Rcout << "skmRpl_mlp_cpp: check output matrix o_list: " << o_list << std::endl;
  // Rcout << "skmRpl_mlp_cpp: check output matrix s_list: " << s_list << std::endl;

  NumericVector::iterator it = std::min_element(o_list.begin(), o_list.end());

  double o(*it);

  IntegerVector s = s_list(it - o_list.begin(), _);

  return Rcpp::List::create(
    Rcpp::Named("o") = o,
    Rcpp::Named("s") = s,
    Rcpp::Named("o_list") = o_list,
    Rcpp::Named("s_list") = s_list
  );

}


