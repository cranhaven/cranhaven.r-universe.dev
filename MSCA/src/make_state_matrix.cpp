#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix make_state_matrix_rcpp(DataFrame data,
                                     IntegerVector id,
                                     IntegerVector ltc,
                                     NumericVector aos,
                                     int l = 111,
                                     int fail_code = -1,
                                     int cens_code = -2) {
  IntegerVector ltc_levels = sort_unique(ltc);
  IntegerVector id_levels = sort_unique(id);
  int n_ltc = ltc_levels.size();
  int n_id = id_levels.size();
  int n_time = l + 1;

  NumericMatrix out(n_ltc * n_time, n_id);

  for (int p = 0; p < n_id; ++p) {
    int current_id = id_levels[p];
    std::vector<int> rows;
    for (int i = 0; i < id.size(); ++i)
      if (id[i] == current_id)
        rows.push_back(i);

      NumericMatrix mat(n_time, n_ltc);

      for (int i : rows) {
        int ltc_val = ltc[i];
        int aos_i = static_cast<int>(std::floor(aos[i]));
        if (aos_i >= n_time) continue;

        // find ltc column index
        int ltc_idx = std::find(ltc_levels.begin(), ltc_levels.end(), ltc_val) - ltc_levels.begin();

        if (ltc_val == cens_code || ltc_val == fail_code) {
          // set NA from aos onward for all ltc
          for (int j = 0; j < n_ltc; ++j)
            for (int t = aos_i; t < n_time; ++t)
              mat(t, j) = NA_REAL;
        } else {
          // set 1 from aos onward for specific ltc
          for (int t = aos_i; t < n_time; ++t)
            if (!NumericVector::is_na(mat(t, ltc_idx)))
              mat(t, ltc_idx) = 1;
        }
      }

      // flatten
      for (int j = 0; j < n_ltc; ++j)
        for (int t = 0; t < n_time; ++t)
          out(j * n_time + t, p) = mat(t, j);
  }

  return out;
}
