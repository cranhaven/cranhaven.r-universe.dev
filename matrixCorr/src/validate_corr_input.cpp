// Thiago de Paula Oliveira
// validate_corr_input.cpp
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <atomic>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// ---- helpers (single-thread safe) ----
inline bool any_na_int_vec(const int* pi, int n){
   for (int i = 0; i < n; ++i) if (pi[i] == NA_INTEGER) return true;
   return false;
}
inline bool any_na_logi_vec(const int* pl, int n){ // LOGICALSXP uses int storage
   for (int i = 0; i < n; ++i) if (pl[i] == NA_LOGICAL) return true;
   return false;
}
inline bool has_class(SEXP x, const char* cls){
   return Rf_inherits(x, cls);
}

// Parallel NA scan for a REALSXP matrix (no copies)
// Returns true if any NA/NaN is present.
inline bool any_na_real_matrix_parallel(SEXP x, int nr, int nc){
   const double* pr = REAL(x);
   std::atomic<bool> has_na(false);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) if(nc > 1)
#endif
   for (int j = 0; j < nc; ++j) {
      if (has_na.load(std::memory_order_relaxed)) continue;
      const double* col = pr + static_cast<R_xlen_t>(j) * nr;
      for (int i = 0; i < nr; ++i) {
         if (ISNAN(col[i])) { has_na.store(true, std::memory_order_relaxed); break; }
      }
   }
   return has_na.load(std::memory_order_relaxed);
}

// [[Rcpp::export]]
 Rcpp::NumericMatrix validate_corr_input_cpp(SEXP data, bool check_na = true){
    const bool is_mat = Rf_isMatrix(data);
    const bool is_df  = Rf_inherits(data, "data.frame");
    if (!is_mat && !is_df) Rcpp::stop("Input must be a matrix or a data frame.");

    // ---------------- matrix path ----------------
    if (is_mat){
       const int nr = Rf_nrows(data);
       const int nc = Rf_ncols(data);
       if (nc < 2) Rcpp::stop("At least two numeric columns are required.");
       if (nr < 2) Rcpp::stop("Each column must contain at least two values.");

       const SEXPTYPE t = TYPEOF(data);

       // double matrix: shallow wrap; optional parallel NA scan
       if (t == REALSXP){
          if (check_na && any_na_real_matrix_parallel(data, nr, nc))
             Rcpp::stop("Missing values are not allowed.");
          return Rcpp::NumericMatrix(data); // shallow, no copy
       }

       // integer matrix: convert to double (parallel fill into a temporary buffer)
       if (t == INTSXP){
          Rcpp::IntegerMatrix Mi(data);
          if (check_na){
             for (int j = 0; j < nc; ++j)
                if (any_na_int_vec(&(Mi(0, j)), nr)) Rcpp::stop("Missing values are not allowed.");
          }

          std::vector<double> buf(static_cast<size_t>(nr) * static_cast<size_t>(nc));
#ifdef _OPENMP
#pragma omp parallel for schedule(static) if(nc > 1)
#endif
          for (int j = 0; j < nc; ++j){
             const int* src = &(Mi(0, j));
             double*    dst = buf.data() + static_cast<size_t>(j) * static_cast<size_t>(nr);
             for (int i = 0; i < nr; ++i) dst[i] = static_cast<double>(src[i]);
          }

          // materialise NumericMatrix and copy buffer (serial, single touch)
          Rcpp::NumericMatrix M(nr, nc);
          for (int j = 0; j < nc; ++j){
             std::copy(buf.data() + static_cast<size_t>(j) * static_cast<size_t>(nr),
                       buf.data() + static_cast<size_t>(j + 1) * static_cast<size_t>(nr),
                       &(M(0, j)));
          }

          // preserve colnames
          Rcpp::List dn = Mi.attr("dimnames");
          if (dn.size() == 2){
             SEXP cn = dn[1];
             if (!Rf_isNull(cn)) M.attr("dimnames") = Rcpp::List::create(R_NilValue, cn);
          }
          return M;
       }

       // reject logical matrices: cannot drop columns in matrix path
       if (t == LGLSXP){
          Rcpp::stop("Logical matrices are not supported; use a data.frame so non-numeric columns can be dropped.");
       }

       Rcpp::stop("Matrix must be numeric (integer or double).");
    }

    // ---------------- data.frame path ----------------
    Rcpp::List df(data);
    const int p_all = df.size();
    if (p_all < 1) Rcpp::stop("No columns found.");

    // select true numeric columns (drop factors, logicals, and common time classes)
    std::vector<int> idx; idx.reserve(p_all);
    int n = -1;
    for (int j = 0; j < p_all; ++j){
       SEXP col = df[j];
       const SEXPTYPE t = TYPEOF(col);

       if (Rf_isFactor(col)) continue;           // drop factor/ordered
       if (t == LGLSXP) continue;                // drop logical
       if (has_class(col, "Date") || has_class(col, "POSIXct") || has_class(col, "difftime")) continue;

       if (t != REALSXP && t != INTSXP) continue;

       const int len = Rf_length(col);
       if (len < 2) Rcpp::stop("All numeric columns must have at least two values.");
       if (n == -1) n = len; else if (len != n) Rcpp::stop("All numeric columns must have the same length.");

       // quick NA screen (serial; detailed checks happen during parallel copy)
       if (check_na){
          if (t == REALSXP){
             const double* pr = REAL(col);
             for (int i = 0; i < len; ++i) if (ISNAN(pr[i])) Rcpp::stop("Missing values are not allowed.");
          } else { // INTSXP
             if (any_na_int_vec(INTEGER(col), len)) Rcpp::stop("Missing values are not allowed.");
          }
       }

       idx.push_back(j);
    }

    if (idx.empty())  Rcpp::stop("No numeric columns found in the input.");
    if (static_cast<int>(idx.size()) < 2) Rcpp::stop("At least two numeric columns are required.");

    const int k = static_cast<int>(idx.size());

    // parallel copy into a plain buffer; do NOT touch R objects inside OpenMP
    std::vector<double> buf(static_cast<size_t>(n) * static_cast<size_t>(k));
    std::atomic<bool> bad(false);

#ifdef _OPENMP
#pragma omp parallel for schedule(static) if(k > 1)
#endif
    for (int jj = 0; jj < k; ++jj){
       if (bad.load(std::memory_order_relaxed)) continue;

       const int j = idx[jj];
       SEXP col = df[j];
       const SEXPTYPE t = TYPEOF(col);

       double* dst = buf.data() + static_cast<size_t>(jj) * static_cast<size_t>(n);

       if (t == REALSXP){
          const double* pr = REAL(col);
          if (check_na){
             for (int i = 0; i < n; ++i){
                const double v = pr[i];
                if (ISNAN(v)) { bad.store(true, std::memory_order_relaxed); break; }
                dst[i] = v;
             }
          } else {
             std::copy(pr, pr + n, dst);
          }
       } else { // INTSXP
          const int* pi = INTEGER(col);
          if (check_na){
             for (int i = 0; i < n; ++i){
                const int v = pi[i];
                if (v == NA_INTEGER) { bad.store(true, std::memory_order_relaxed); break; }
                dst[i] = static_cast<double>(v);
             }
          } else {
             for (int i = 0; i < n; ++i) dst[i] = static_cast<double>(pi[i]);
          }
       }
    }

    if (bad.load(std::memory_order_relaxed)) Rcpp::stop("Missing values are not allowed.");

    // materialise NumericMatrix and copy buffer (serial)
    Rcpp::NumericMatrix M(n, k);
    for (int jj = 0; jj < k; ++jj){
       std::copy(buf.data() + static_cast<size_t>(jj) * static_cast<size_t>(n),
                 buf.data() + static_cast<size_t>(jj + 1) * static_cast<size_t>(n),
                 &(M(0, jj)));
    }

    // set column names (serial; safe)
    Rcpp::CharacterVector src_names = df.attr("names");
    Rcpp::CharacterVector cn(k);
    for (int jj = 0; jj < k; ++jj){
       const int j = idx[jj];
       if (src_names.size() > j) cn[jj] = src_names[j];
    }
    M.attr("dimnames") = Rcpp::List::create(R_NilValue, cn);
    return M;
 }
