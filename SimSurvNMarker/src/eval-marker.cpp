#include <Rcpp.h>

inline void eval_marker_inner
  (double * __restrict__ o, double const * const __restrict__ m,
   double const * __restrict__ b, R_len_t const lm,
   R_len_t const lo) noexcept {
  for(R_len_t i = 0; i < lo; ++i, ++o){
    auto mj = m;
    for(R_len_t j = 0; j < lm; ++j, ++b, ++mj)
      *o += *mj * *b;
  }
}

// [[Rcpp::export(name = "eval_marker_cpp", rng = false)]]
void eval_marker(SEXP B, SEXP m, SEXP Sout){
  bool const out_is_mat = Rf_isMatrix(Sout);
  if(Rf_isMatrix(B) and out_is_mat){
    if(Rf_isMatrix(m)){
      R_len_t const nr = Rf_nrows(B),
                    nc = Rf_ncols(B),
             n_col_out = Rf_nrows(m),
                    nm = Rf_ncols(m);

      bool const B_m_ok = nr == nm,
                 out_ok = Rf_ncols(Sout) == n_col_out;
      if(B_m_ok and out_ok){
        double * o = REAL(Sout);
        double const * const m_start = REAL(m),
                     * const b_start = REAL(B);

        for(R_len_t i = 0; i < n_col_out; ++i){
          double const * b = b_start;
          for(R_len_t j = 0; j < nc; ++j, ++o){
            double const * mi = m_start + i;
            for(R_len_t k = 0; k < nm; ++k, mi += n_col_out, ++b)
              *o += *mi * *b;
          }
        }

        return;

      } else
        throw std::invalid_argument("eval_marker: dims do not match");

    } else if(Rf_isVector(m)){
      R_len_t const nr = Rf_nrows(B),
                    nc = Rf_ncols(B),
                    nm = XLENGTH(m);

      bool const B_m_ok = nr == nm,
                 out_ok = Rf_ncols(Sout) == 1L;
      if(B_m_ok and out_ok){
        double const *b = REAL(B),
               *m_start = REAL(m);

        eval_marker_inner(REAL(Sout), m_start, b, nm, nc);

        return;

      } else
        throw std::invalid_argument("eval_marker: dims do not match");

    }
  }

  throw std::invalid_argument("eval_marker: B and Sout must be a matrix. m must be a vector or a Matrix");
}
