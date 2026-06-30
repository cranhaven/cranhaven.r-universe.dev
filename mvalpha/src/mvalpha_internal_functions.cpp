// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <omp.h>
#include <vector>
#include <numeric>
#include <cmath>
#include <string>
#include <chrono>
#include <locale>
using namespace Rcpp;

// ============================================================
// --- Type enum and parameters ---
// ============================================================

enum MetricType { NOMINAL, ORDINAL, INTERVAL, RATIO };

MetricType parse_type(const std::string& type) {
  if (type == "nominal")  return NOMINAL;
  if (type == "ordinal")  return ORDINAL;
  if (type == "interval") return INTERVAL;
  if (type == "ratio")    return RATIO;
  Rcpp::stop("Unknown type: " + type);
}

struct MetricParams {
  MetricType type;
  double     n_dotdot;
  double     n_c_first;
  double     n_c_last;
  double     c_min;
  double     c_max;
};

// ============================================================
// --- Combination generator ---
// ============================================================

bool next_combination(std::vector<int>& comb, int n) {
  int k = comb.size();
  int i = k - 1;
  while (i >= 0 && comb[i] == n - k + i + 1) i--;
  if (i < 0) return false;
  comb[i]++;
  for (int j = i + 1; j < k; j++) comb[j] = comb[j-1] + 1;
  return true;
}

std::vector<int> first_combination(int k) {
  std::vector<int> comb(k);
  std::iota(comb.begin(), comb.end(), 1);
  return comb;
}

// count combinations C(n, k) for chunking
long long n_combinations(int n, int k) {
  if (k == 0) return 1;
  if (k > n)  return 0;
  if (k > n - k) k = n - k;
  long long result = 1;
  for (int i = 0; i < k; i++) {
    result *= (n - i);
    result /= (i + 1);
  }
  return result;
}

// advance combination by step positions
void advance_combination(std::vector<int>& comb, int n, long long steps) {
  for (long long s = 0; s < steps; s++) next_combination(comb, n);
}

// ============================================================
// --- set_ops ---
// ============================================================

struct SetOpsResult {
  std::vector<int> A_intersect_B;
  std::vector<int> A_diff_B;
  std::vector<int> B_diff_A;
};

void set_ops(
    const std::vector<int>& A,
    const std::vector<int>& B,
    int n_labels,
    MetricType type,
    SetOpsResult& result
) {
  result.A_intersect_B.clear();
  result.A_diff_B.clear();
  result.B_diff_A.clear();

  std::vector<bool> in_B(n_labels + 1, false);
  for (int b : B) in_B[b] = true;

  for (int a : A) {
    if (in_B[a]) result.A_intersect_B.push_back(a);
    else         result.A_diff_B.push_back(a);
  }

  if (type != NOMINAL) {
    std::vector<bool> in_A(n_labels + 1, false);
    for (int a : A) in_A[a] = true;
    for (int b : B) {
      if (!in_A[b]) result.B_diff_A.push_back(b);
    }
  }
}

// [[Rcpp::export]]
List set_ops_r(
    const IntegerVector& A,
    const IntegerVector& B,
    int n_labels,
    const std::string& type
) {
  MetricType mt = parse_type(type);
  SetOpsResult result;
  set_ops(
    std::vector<int>(A.begin(), A.end()),
    std::vector<int>(B.begin(), B.end()),
    n_labels, mt, result
  );
  return List::create(
    Named("A_intersect_B") = result.A_intersect_B,
    Named("A_diff_B")      = result.A_diff_B,
    Named("B_diff_A")      = (mt == NOMINAL) ?
  R_NilValue :
    (SEXP)IntegerVector(result.B_diff_A.begin(), result.B_diff_A.end())
  );
}

// ============================================================
// --- metric_delta_CK ---
// ============================================================

template<typename DelsqFn>
double metric_delta_CK(
    const std::vector<int>& C,
    const std::vector<int>& K,
    const SetOpsResult& ops,
    MetricType type,
    const DelsqFn& delsq_fn
) {
  if (type == NOMINAL) {
    if (C.empty() && K.empty()) return 0.0;
    return 1.0 - (2.0 * (int)ops.A_intersect_B.size() /
                  ((int)C.size() + (int)K.size()));
  }

  double lhs = 0.0;
  if (!C.empty() && !ops.A_diff_B.empty()) {
    for (int c : C)
      for (int a : ops.A_diff_B)
        lhs += delsq_fn(c, a);
    lhs /= C.size();
  }

  double rhs = 0.0;
  if (!K.empty() && !ops.B_diff_A.empty()) {
    for (int k : K)
      for (int b : ops.B_diff_A)
        rhs += delsq_fn(k, b);
    rhs /= K.size();
  }

  double denom = (double)(C.size() + K.size());
  if (lhs == 0.0 && rhs == 0.0 && denom == 0.0) return 0.0;
  if (C.empty() || K.empty()) return 1.0;
  return (lhs + rhs) / denom;
}

// ============================================================
// --- prod calculation ---
// ============================================================

double calc_prod(
    const std::vector<int>& C,
    const SetOpsResult& ops,
    const std::vector<double>& n_c_w_vec,
    int null_set_observed
) {
  double prod_all = 1.0;
  for (int c : C) {
    double val = n_c_w_vec[c - 1 + null_set_observed];
    if (std::isnan(val) || val == 0.0) return 0.0;
    prod_all *= val;
  }
  for (int a : ops.A_diff_B) {
    double val = n_c_w_vec[a - 1 + null_set_observed];
    if (std::isnan(val) || val == 0.0) return 0.0;
    prod_all *= val;
  }
  double prod_int = 1.0;
  for (int a : ops.A_intersect_B) {
    double val = n_c_w_vec[a - 1 + null_set_observed] - 1.0;
    if (std::isnan(val)) continue;
    prod_int *= val;
  }
  return prod_all * prod_int;
}

// ============================================================
// --- process_CK ---
// ============================================================

template<typename DelsqFn>
inline void process_CK(
    const std::vector<int>& C,
    const std::vector<int>& K,
    int n_labels,
    const std::vector<double>& n_c_w_vec,
    int null_set_observed,
    const MetricParams& params,
    const DelsqFn& delsq_fn,
    SetOpsResult& ops,
    double& sum_prod,
    double& sum_weighted
) {
  set_ops(K, C, n_labels, params.type, ops);
  double prod = calc_prod(C, ops, n_c_w_vec, null_set_observed);
  if (prod != 0.0) {
    sum_prod     += prod;
    sum_weighted += prod * metric_delta_CK(C, K, ops, params.type, delsq_fn);
  }
}

// ============================================================
// --- finalize pair ---
// ============================================================

double finalise_pair(
    double sum_prod,
    double sum_weighted,
    double P_C, double P_K,
    int cardC, int cardK
) {
  if (sum_prod == 0.0) return 0.0;
  return P_C * P_K *
    (sum_weighted / sum_prod) *
    (1.0 + (cardC != cardK ? 1.0 : 0.0));
}

// ============================================================
// --- Parallel pair processing (on-the-fly combination generation) ---
// ============================================================

template<typename DelsqFn>
double calc_pair_parallel(
    int n_labels, int cardC, int cardK,
    const std::vector<double>& n_c_w_vec,
    int null_set_observed,
    double P_C, double P_K,
    const MetricParams& params,
    const DelsqFn& delsq_fn,
    int n_threads
) {
  long long n_C = (cardC == 0) ? 1 : n_combinations(n_labels, cardC);

  double sum_prod = 0.0, sum_weighted = 0.0;

#pragma omp parallel reduction(+:sum_prod, sum_weighted) num_threads(n_threads)
{
  int thread_id = omp_get_thread_num();
  int n_actual  = omp_get_num_threads();

  // each thread handles a contiguous chunk of C combinations
  long long chunk = (n_C + n_actual - 1) / n_actual;
  long long start = thread_id * chunk;
  long long end   = std::min(start + chunk, n_C);

  if (start < n_C) {
    SetOpsResult ops;
    double local_prod = 0.0, local_weighted = 0.0;
    std::vector<int> empty;

    if (cardC == 0) {
      // single empty C — only thread 0 does work
      if (thread_id == 0) {
        if (cardK == 0) {
          process_CK(empty, empty, n_labels, n_c_w_vec, null_set_observed, params, delsq_fn, ops, local_prod, local_weighted);
        } else {
          std::vector<int> K = first_combination(cardK);
          do {
            process_CK(empty, K, n_labels, n_c_w_vec, null_set_observed, params, delsq_fn, ops, local_prod, local_weighted);
          } while (next_combination(K, n_labels));
        }
      }
    } else {
      // advance to this thread's starting C combination
      std::vector<int> C = first_combination(cardC);
      advance_combination(C, n_labels, start);

      for (long long ci = start; ci < end; ci++) {
        if (cardK == 0) {
          process_CK(C, empty, n_labels, n_c_w_vec, null_set_observed, params, delsq_fn, ops, local_prod, local_weighted);
        } else {
          std::vector<int> K = first_combination(cardK);
          do {
            process_CK(C, K, n_labels, n_c_w_vec, null_set_observed, params, delsq_fn, ops, local_prod, local_weighted);
          } while (next_combination(K, n_labels));
        }
        if (ci + 1 < end) next_combination(C, n_labels);
      }
    }

    sum_prod     += local_prod;
    sum_weighted += local_weighted;
  }
}

return finalise_pair(sum_prod, sum_weighted, P_C, P_K, cardC, cardK);
}

// ============================================================
// --- Main export ---
// ============================================================

struct comma_numpunct : std::numpunct<char> {
  char do_thousands_sep()   const override { return ','; }
  std::string do_grouping() const override { return "\3"; } // groups of 3
};

// [[Rcpp::export]]
double calc_mvDe_cpp(
    int n_labels,
    const IntegerVector& cardC_vec,
    const IntegerVector& cardK_vec,
    const NumericVector& n_c_w,
    int null_set_observed,
    const NumericVector& P_C_vec,
    const NumericVector& P_K_vec,
    const std::string& type,
    double n_dotdot                     = 0.0,
    double n_c_first                    = 0.0,
    double n_c_last                     = 0.0,
    double c_min                        = 0.0,
    double c_max                        = 0.0,
    const NumericVector& labels         = NumericVector::create(),
    int n_threads                       = 1
) {

  int n_pairs = cardC_vec.size();

  std::vector<double> n_c_w_vec(n_c_w.begin(), n_c_w.end());
  std::vector<double> labels_vec(labels.begin(), labels.end());

  MetricParams params;
  params.type      = parse_type(type);
  params.n_dotdot  = n_dotdot;
  params.n_c_first = n_c_first;
  params.n_c_last  = n_c_last;
  params.c_min     = c_min;
  params.c_max     = c_max;

  double mvDe = 0.0;

  auto run_pairs = [&](auto delsq_fn) {
    long long total_combinations = 0;
    double    total_seconds      = 0.0;
    //auto      run_start          = std::chrono::steady_clock::now();

    for (int p = 0; p < n_pairs; p++) {

      long long n_C = (cardC_vec[p] == 0) ? 1 : n_combinations(n_labels, cardC_vec[p]);
      long long n_K = (cardK_vec[p] == 0) ? 1 : n_combinations(n_labels, cardK_vec[p]);
      long long pair_combinations = n_C * n_K;
      long long rate = (long long)(total_combinations / total_seconds);
      Rcpp::Rcout.imbue(std::locale(std::locale(), new comma_numpunct()));
      Rcpp::Rcout << "|C| = "       << cardC_vec[p]
                  << ", |K| = "     << cardK_vec[p]
                  << ", C-K pairs to evaluate = "   << pair_combinations;
      if (total_seconds > 0.0) {
        Rcpp::Rcout << ", avg. rate so far = "
                    << rate
                    << " pairs/sec";
      }
      Rcpp::Rcout << "\n";
      Rcpp::Rcout.flush();

      auto pair_start = std::chrono::steady_clock::now();

      mvDe += calc_pair_parallel(
        n_labels, cardC_vec[p], cardK_vec[p],
        n_c_w_vec, null_set_observed,
        P_C_vec[p], P_K_vec[p],
        params, delsq_fn, n_threads
      );

      auto pair_end = std::chrono::steady_clock::now();
      total_seconds      += std::chrono::duration<double>(pair_end - pair_start).count();
      total_combinations += pair_combinations;
    }

    // final summary
    Rcpp::Rcout << "\nTotal combinations: " << total_combinations
                << ", Overall rate: "
                << (long long)(total_combinations / total_seconds)
                << " pairs/sec\n";
    Rcpp::Rcout.flush();
  };

  switch (params.type) {
  case NOMINAL:
    run_pairs([](int c, int k) -> double {
      return (c != k) ? 1.0 : 0.0;
    });
    break;
  case ORDINAL:
    run_pairs([&n_c_w_vec, &params](int c, int k) -> double {
      int lo = std::min(c, k) - 1;
      int hi = std::max(c, k) - 1;
      double sum = 0.0;
      for (int i = lo; i <= hi; i++) sum += n_c_w_vec[i];
      double numerator   = sum - (n_c_w_vec[lo] + n_c_w_vec[hi]) / 2.0;
      double denominator = params.n_dotdot - (params.n_c_last + params.n_c_first) / 2.0;
      return std::pow(numerator / denominator, 2.0);
    });
    break;
  case INTERVAL:
    run_pairs([&labels_vec, &params](int c, int k) -> double {
      double cv = labels_vec[c - 1];
      double kv = labels_vec[k - 1];
      return std::pow((cv - kv) / (params.c_max - params.c_min), 2.0);
    });
    break;
  case RATIO:
    run_pairs([&labels_vec](int c, int k) -> double {
      double cv = labels_vec[c - 1];
      double kv = labels_vec[k - 1];
      return std::pow((cv - kv) / (cv + kv), 2.0);
    });
    break;
  }

  return mvDe;
}
