#include <Rcpp.h>
#include <vector>

std::vector<size_t> get_commutation_unequal_vec
  (unsigned const n, unsigned const m, bool const transpose){
  unsigned const nm = n * m,
            nnm_p1 = n * nm + 1L,
             nm_pm = nm + m;
  std::vector<size_t> out(nm);
  size_t * const o_begin = out.data();
  size_t idx = 0L;
  for(unsigned i = 0; i < n; ++i, idx += nm_pm){
    size_t idx1 = idx;
    for(unsigned j = 0; j < m; ++j, idx1 += nnm_p1)
      if(transpose)
        *(o_begin + idx1 / nm) = (idx1 % nm);
      else
        *(o_begin + idx1 % nm) = (idx1 / nm);
  }

  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector commutation_dot
  (unsigned const n, unsigned const m, Rcpp::NumericVector x,
   bool const transpose){
  size_t const nm = n * m;
  Rcpp::NumericVector out(nm);
  auto const indices = get_commutation_unequal_vec(n, m, transpose);

  for(size_t i = 0; i < nm; ++i)
    out[i] = x[indices[i]];

  return out;
}

Rcpp::NumericMatrix get_commutation_unequal
  (unsigned const n, unsigned const m){

  unsigned const nm = n * m,
             nnm_p1 = n * nm + 1L,
              nm_pm = nm + m;
  Rcpp::NumericMatrix out(nm, nm);
  double * o = &out[0];
  for(unsigned i = 0; i < n; ++i, o += nm_pm){
    double *o1 = o;
    for(unsigned j = 0; j < m; ++j, o1 += nnm_p1)
      *o1 = 1.;
  }

  return out;
}

Rcpp::NumericMatrix get_commutation_equal(unsigned const m){
  unsigned const mm = m * m,
                mmm = mm * m,
             mmm_p1 = mmm + 1L,
              mm_pm = mm + m;
  Rcpp::NumericMatrix out(mm, mm);
  double * const o = &out[0];
  unsigned inc_i(0L);
  for(unsigned i = 0; i < m; ++i, inc_i += m){
    double *o1 = o + inc_i + i * mm,
           *o2 = o + i     + inc_i * mm;
    for(unsigned j = 0; j < i; ++j, o1 += mmm_p1, o2 += mm_pm){
      *o1 = 1.;
      *o2 = 1.;
    }
    *o1 += 1.;
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix get_commutation(unsigned const n, unsigned const m){
  if(n == m)
    return get_commutation_equal(n);

  return get_commutation_unequal(n, m);
}

/*** R
options(digits = 3)

library(matrixcalc)
for(i in 1:5)
  for(j in 1:5){
    x <- rnorm(i * j)
    K <- get_commutation(i, j)
    stopifnot(all.equal(drop(K %*% x),
                        commutation_dot(i, j, x, FALSE)))
    stopifnot(all.equal(drop(crossprod(K, x)),
                        commutation_dot(i, j, x, TRUE)))
  }

library(microbenchmark)
i <- 20L
j <- 10L
x <- rnorm(i * j)
microbenchmark(
  `Using dense`  = get_commutation(i, j) %*% x,
  `Using sparse` = commutation_dot(i, j, x, FALSE))

get_commutation_R <- function(m){
  out <- matrix(0., nrow = m * m, ncol = m * m)
  for(i in 1:m)
    for(j in 1:m)
      out[(i - 1L) * m + j, (j - 1L) * m + i] <- 1.

  return(out)
}

# equal
for(i in 2:10){
  stopifnot(all.equal(commutation.matrix(i), get_commutation_R(i)))
  stopifnot(all.equal(commutation.matrix(i), get_commutation  (i, i)))
}

# unequal
for(i in 3:10)
  for(j in 2:(i - 1L)){
    stopifnot(all.equal(commutation.matrix(i, j), get_commutation  (i, j)))
    stopifnot(all.equal(commutation.matrix(j, i), get_commutation  (j, i)))
  }

# benchmark: equal
library(microbenchmark)
microbenchmark(
  matrixcalc = commutation.matrix(4L),
  R          = get_commutation_R (4L),
  cpp        = get_commutation   (4L, 4L),
  times = 1000)

microbenchmark(
  matrixcalc = commutation.matrix(20L),
  R          = get_commutation_R (20L),
  cpp        = get_commutation   (20L, 20L),
  times = 25)

# benchmark: unequal
microbenchmark(
  matrixcalc = commutation.matrix(17L, 20L),
  cpp        = get_commutation   (17L, 20L),
  times = 25)
*/
