#include <cpp11.hpp>

#include <cmath>
#include <string>
#include <vector>

#include "ts_core.h"
#include "ts_penalised.h"

namespace {

std::vector<timesift::seconds> as_seconds(const cpp11::doubles& x) {
  std::vector<timesift::seconds> out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[static_cast<std::size_t>(i)] =
        static_cast<timesift::seconds>(std::floor(x[i]));
  }
  return out;
}

// The storage a Request points into, built once for the reduction and for the coverage so the
// two read the same readings the same way.
struct Held {
  std::vector<std::int32_t> unit_index;
  std::vector<double> reading;
  std::vector<timesift::seconds> instant;
  std::vector<timesift::seconds> naive;
  std::vector<timesift::seconds> supplied;
  std::vector<std::string> names;
  std::vector<const char*> name_ptr;
  timesift::Request req;
};

Held hold(cpp11::integers unit, cpp11::sexp value, cpp11::doubles when, cpp11::doubles local,
          cpp11::sexp custom, cpp11::strings unit_names, const std::string& grain,
          int year_month, int year_day) {
  Held h;
  const std::size_t n = static_cast<std::size_t>(local.size());

  h.unit_index.resize(n);
  for (std::size_t i = 0; i < n; ++i) h.unit_index[i] = unit[static_cast<R_xlen_t>(i)] - 1;

  h.reading.assign(n, 0.0);
  if (value != R_NilValue) {
    cpp11::doubles v(value);
    for (std::size_t i = 0; i < n; ++i) h.reading[i] = v[static_cast<R_xlen_t>(i)];
  }

  h.instant = as_seconds(when);
  h.naive = as_seconds(local);
  if (custom != R_NilValue) h.supplied = as_seconds(cpp11::doubles(custom));

  h.names.reserve(static_cast<std::size_t>(unit_names.size()));
  for (R_xlen_t i = 0; i < unit_names.size(); ++i) {
    h.names.push_back(std::string(unit_names[i]));
  }
  h.name_ptr.reserve(h.names.size());
  for (const std::string& s : h.names) h.name_ptr.push_back(s.c_str());

  h.req.unit = h.unit_index.data();
  h.req.value = h.reading.data();
  h.req.when = h.instant.data();
  h.req.local = h.naive.data();
  h.req.custom = h.supplied.empty() ? nullptr : h.supplied.data();
  h.req.unit_name = h.name_ptr.empty() ? nullptr : h.name_ptr.data();
  h.req.n = n;
  h.req.n_unit = h.names.size();
  h.req.grain = timesift::grain_from_name(grain);
  h.req.year_start = timesift::YearStart{year_month, year_day};
  return h;
}

}  // namespace

[[cpp11::register]]
cpp11::list ts_reduce_(cpp11::integers unit, cpp11::doubles value, cpp11::doubles when,
                       cpp11::doubles local, cpp11::sexp custom, cpp11::strings unit_names,
                       std::string grain, int year_month, int year_day, cpp11::strings stats,
                       double sampling_step) {
  Held h = hold(unit, value, when, local, custom, unit_names, grain, year_month, year_day);
  timesift::Request& req = h.req;
  req.sampling_step = static_cast<timesift::seconds>(sampling_step);
  for (R_xlen_t i = 0; i < stats.size(); ++i) {
    req.stats.push_back(timesift::stat_from_name(std::string(stats[i])));
  }

  const timesift::Result result = timesift::reduce(req);
  const std::size_t n_bin = result.bin_start.size();

  cpp11::writable::doubles values(static_cast<R_xlen_t>(result.values.size()));
  for (std::size_t i = 0; i < result.values.size(); ++i) {
    values[static_cast<R_xlen_t>(i)] = result.values[i];
  }
  cpp11::writable::doubles bin_start(static_cast<R_xlen_t>(n_bin));
  cpp11::writable::doubles bin_end(static_cast<R_xlen_t>(n_bin));
  cpp11::writable::logicals bin_partial(static_cast<R_xlen_t>(n_bin));
  for (std::size_t k = 0; k < n_bin; ++k) {
    bin_start[static_cast<R_xlen_t>(k)] = static_cast<double>(result.bin_start[k]);
    bin_end[static_cast<R_xlen_t>(k)] = static_cast<double>(result.bin_end[k]);
    bin_partial[static_cast<R_xlen_t>(k)] =
        result.bin_partial[k] ? TRUE : FALSE;
  }
  cpp11::writable::integers bin_n(static_cast<R_xlen_t>(result.bin_n.size()));
  for (std::size_t i = 0; i < result.bin_n.size(); ++i) {
    bin_n[static_cast<R_xlen_t>(i)] = result.bin_n[i];
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
    "values"_nm = values,
    "bin_start"_nm = bin_start,
    "bin_end"_nm = bin_end,
    "bin_n"_nm = bin_n,
    "bin_partial"_nm = bin_partial
  });
}

[[cpp11::register]]
cpp11::list ts_coverage_(cpp11::integers unit, cpp11::doubles when, cpp11::doubles local,
                         cpp11::sexp custom, cpp11::strings unit_names, std::string grain,
                         int year_month, int year_day) {
  Held h = hold(unit, R_NilValue, when, local, custom, unit_names, grain, year_month, year_day);
  const timesift::Coverage result = timesift::coverage(h.req);

  cpp11::writable::doubles bin_start(static_cast<R_xlen_t>(result.bin_start.size()));
  for (std::size_t k = 0; k < result.bin_start.size(); ++k) {
    bin_start[static_cast<R_xlen_t>(k)] = static_cast<double>(result.bin_start[k]);
  }
  cpp11::writable::integers count(static_cast<R_xlen_t>(result.count.size()));
  for (std::size_t i = 0; i < result.count.size(); ++i) {
    count[static_cast<R_xlen_t>(i)] = result.count[i];
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({"bin_start"_nm = bin_start, "count"_nm = count});
}

[[cpp11::register]]
cpp11::list ts_reduce_lookbacks_(cpp11::integers unit, cpp11::doubles value, cpp11::doubles when,
                               cpp11::doubles local, cpp11::strings unit_names,
                               cpp11::integers target_unit,
                               cpp11::doubles target_at, cpp11::strings target_names,
                               double span, double lag, int bins, cpp11::strings stats) {
  const std::size_t n = static_cast<std::size_t>(value.size());
  const std::size_t n_target = static_cast<std::size_t>(target_at.size());

  std::vector<std::int32_t> unit_index(n);
  for (std::size_t i = 0; i < n; ++i) unit_index[i] = unit[static_cast<R_xlen_t>(i)] - 1;

  std::vector<double> reading(n);
  for (std::size_t i = 0; i < n; ++i) reading[i] = value[static_cast<R_xlen_t>(i)];

  const std::vector<timesift::seconds> instant = as_seconds(when);
  const std::vector<timesift::seconds> naive = as_seconds(local);
  const std::vector<timesift::seconds> anchor = as_seconds(target_at);

  std::vector<std::int32_t> holder(n_target);
  for (std::size_t i = 0; i < n_target; ++i) {
    holder[i] = target_unit[static_cast<R_xlen_t>(i)] - 1;
  }

  std::vector<std::string> units, targets;
  for (R_xlen_t i = 0; i < unit_names.size(); ++i) units.push_back(std::string(unit_names[i]));
  for (R_xlen_t i = 0; i < target_names.size(); ++i) {
    targets.push_back(std::string(target_names[i]));
  }
  std::vector<const char*> unit_ptr, target_ptr;
  for (const std::string& s : units) unit_ptr.push_back(s.c_str());
  for (const std::string& s : targets) target_ptr.push_back(s.c_str());

  timesift::LookbackRequest req;
  req.unit = unit_index.data();
  req.value = reading.data();
  req.when = instant.data();
  req.local = naive.data();
  req.unit_name = unit_ptr.empty() ? nullptr : unit_ptr.data();
  req.n = n;
  req.n_unit = units.size();
  req.target_unit = holder.data();
  req.target_at = anchor.data();
  req.target_name = target_ptr.empty() ? nullptr : target_ptr.data();
  req.n_target = n_target;
  req.span = static_cast<timesift::seconds>(span);
  req.lag = static_cast<timesift::seconds>(lag);
  req.n_bin = bins;
  for (R_xlen_t i = 0; i < stats.size(); ++i) {
    req.stats.push_back(timesift::stat_from_name(std::string(stats[i])));
  }

  const timesift::LookbackResult result = timesift::reduce_lookbacks(req);

  cpp11::writable::doubles values(static_cast<R_xlen_t>(result.values.size()));
  for (std::size_t i = 0; i < result.values.size(); ++i) {
    values[static_cast<R_xlen_t>(i)] = result.values[i];
  }
  cpp11::writable::integers bin_n(static_cast<R_xlen_t>(result.bin_n.size()));
  for (std::size_t i = 0; i < result.bin_n.size(); ++i) {
    bin_n[static_cast<R_xlen_t>(i)] = result.bin_n[i];
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({"values"_nm = values, "bin_n"_nm = bin_n});
}

// The seven grains and the civil arithmetic under them, reachable from the suites so the oracle
// can be checked against the core rather than only through a whole reduction.
[[cpp11::register]]
cpp11::doubles ts_bin_starts_(cpp11::doubles local, std::string grain, int year_month,
                              int year_day) {
  const std::vector<timesift::seconds> naive = as_seconds(local);
  std::vector<timesift::seconds> out(naive.size());
  timesift::bin_starts(naive.data(), naive.size(), timesift::grain_from_name(grain),
                        timesift::YearStart{year_month, year_day}, out.data());
  cpp11::writable::doubles result(static_cast<R_xlen_t>(out.size()));
  for (std::size_t i = 0; i < out.size(); ++i) {
    result[static_cast<R_xlen_t>(i)] = static_cast<double>(out[i]);
  }
  return result;
}

[[cpp11::register]]
cpp11::doubles ts_bin_nexts_(cpp11::doubles bins, std::string grain, int year_month,
                             int year_day) {
  const std::vector<timesift::seconds> start = as_seconds(bins);
  std::vector<timesift::seconds> out(start.size());
  timesift::bin_nexts(start.data(), start.size(), timesift::grain_from_name(grain),
                       timesift::YearStart{year_month, year_day}, out.data());
  cpp11::writable::doubles result(static_cast<R_xlen_t>(out.size()));
  for (std::size_t i = 0; i < out.size(); ++i) {
    result[static_cast<R_xlen_t>(i)] = static_cast<double>(out[i]);
  }
  return result;
}

// The position of each bin in a cycle, from the instants the representation carries. The fraction
// is reachable on its own because it is the part of the position the contract pins exactly.
[[cpp11::register]]
cpp11::doubles ts_cycle_fraction_(cpp11::doubles bin_start, cpp11::doubles bin_end,
                                  std::string cycle) {
  const std::vector<timesift::seconds> opens = as_seconds(bin_start);
  const std::vector<timesift::seconds> closes = as_seconds(bin_end);
  std::vector<double> frac(opens.size());
  timesift::cycle_fraction(cycle, opens.data(), closes.data(), opens.size(), frac.data());
  cpp11::writable::doubles out(static_cast<R_xlen_t>(frac.size()));
  for (std::size_t i = 0; i < frac.size(); ++i) out[static_cast<R_xlen_t>(i)] = frac[i];
  return out;
}

[[cpp11::register]]
cpp11::doubles ts_cycle_phase_(cpp11::doubles bin_start, cpp11::doubles bin_end,
                               std::string cycle) {
  const std::vector<timesift::seconds> opens = as_seconds(bin_start);
  const std::vector<timesift::seconds> closes = as_seconds(bin_end);
  const std::size_t n = opens.size();
  std::vector<double> out_sin(n), out_cos(n);
  timesift::cycle_phase(cycle, opens.data(), closes.data(), n, out_sin.data(), out_cos.data());
  cpp11::writable::doubles out(static_cast<R_xlen_t>(2 * n));
  for (std::size_t i = 0; i < n; ++i) {
    out[static_cast<R_xlen_t>(i)] = out_sin[i];
    out[static_cast<R_xlen_t>(n + i)] = out_cos[i];
  }
  return out;
}

// The penalised fit, from the same core the Python side calls. The design arrives as the
// column-major buffer an R matrix already is, so nothing is copied to reach the descent.
namespace {

timesift::PenaltySpec penalty_spec(double alpha, int n_lambda, double lambda_min_ratio,
                                   cpp11::sexp lambda, double thresh, bool standardize,
                                   bool intercept, double max_pass, int threads) {
  timesift::PenaltySpec spec;
  spec.alpha = alpha;
  spec.n_lambda = n_lambda;
  spec.lambda_min_ratio = lambda_min_ratio;
  spec.thresh = thresh;
  spec.max_pass = static_cast<int>(max_pass);
  spec.threads = threads;
  spec.standardize = standardize;
  spec.intercept = intercept;
  if (lambda != R_NilValue) {
    cpp11::doubles given(lambda);
    for (R_xlen_t i = 0; i < given.size(); ++i) spec.lambda.push_back(given[i]);
  }
  return spec;
}

cpp11::writable::doubles give(const std::vector<double>& from) {
  cpp11::writable::doubles out(static_cast<R_xlen_t>(from.size()));
  for (std::size_t i = 0; i < from.size(); ++i) out[static_cast<R_xlen_t>(i)] = from[i];
  return out;
}

cpp11::writable::integers give(const std::vector<std::int32_t>& from) {
  cpp11::writable::integers out(static_cast<R_xlen_t>(from.size()));
  for (std::size_t i = 0; i < from.size(); ++i) out[static_cast<R_xlen_t>(i)] = from[i];
  return out;
}

cpp11::writable::list give(const timesift::PenaltyPath& path) {
  using namespace cpp11::literals;
  return cpp11::writable::list({
    "lambda"_nm = give(path.lambda),
    "a0"_nm = give(path.a0),
    "beta"_nm = give(path.beta),
    "df"_nm = give(path.df),
    "dev_ratio"_nm = give(path.dev_ratio),
    "null_deviance"_nm = cpp11::as_sexp(path.null_deviance),
    "passes"_nm = cpp11::as_sexp(path.passes),
    "stalled"_nm = cpp11::as_sexp(path.stalled),
    "n_column"_nm = cpp11::as_sexp(static_cast<int>(path.n_column)),
    "family"_nm = cpp11::as_sexp(std::string(timesift::family_name(path.family)))
  });
}

timesift::PenaltyPath take(cpp11::doubles lambda, cpp11::doubles a0, cpp11::doubles beta,
                           const std::string& family) {
  timesift::PenaltyPath path;
  path.family = timesift::family_from_name(family);
  for (R_xlen_t i = 0; i < lambda.size(); ++i) path.lambda.push_back(lambda[i]);
  for (R_xlen_t i = 0; i < a0.size(); ++i) path.a0.push_back(a0[i]);
  for (R_xlen_t i = 0; i < beta.size(); ++i) path.beta.push_back(beta[i]);
  path.n_column = path.lambda.empty() ? 0 : path.beta.size() / path.lambda.size();
  return path;
}

}  // namespace

[[cpp11::register]]
cpp11::list ts_penalised_path_(cpp11::doubles x, cpp11::doubles y, cpp11::doubles w, int n, int p,
                               std::string family, double alpha, int n_lambda,
                               double lambda_min_ratio, cpp11::sexp lambda, double thresh,
                               bool standardize, bool intercept, double max_pass) {
  const timesift::PenaltySpec spec = penalty_spec(alpha, n_lambda, lambda_min_ratio, lambda,
                                                  thresh, standardize, intercept, max_pass, 1);
  const timesift::PenaltyPath path =
      timesift::penalised_path(REAL_RO(x.data()), REAL_RO(y.data()), REAL_RO(w.data()), static_cast<std::size_t>(n),
                               static_cast<std::size_t>(p), timesift::family_from_name(family),
                               spec);
  return give(path);
}

[[cpp11::register]]
cpp11::list ts_penalised_cv_(cpp11::doubles x, cpp11::doubles y, cpp11::doubles w, int n, int p,
                             std::string family, double alpha, int n_lambda,
                             double lambda_min_ratio, cpp11::sexp lambda, double thresh,
                             bool standardize, bool intercept, cpp11::integers fold, int n_fold,
                             double max_pass, int threads) {
  const timesift::PenaltySpec spec = penalty_spec(alpha, n_lambda, lambda_min_ratio, lambda,
                                                  thresh, standardize, intercept, max_pass,
                                                  threads);
  std::vector<std::int32_t> which(static_cast<std::size_t>(n));
  for (int i = 0; i < n; ++i) which[static_cast<std::size_t>(i)] = fold[i];
  const timesift::PenaltyCV cv =
      timesift::penalised_cv(REAL_RO(x.data()), REAL_RO(y.data()), REAL_RO(w.data()), static_cast<std::size_t>(n),
                             static_cast<std::size_t>(p), timesift::family_from_name(family),
                             spec, which.data(), n_fold);
  using namespace cpp11::literals;
  cpp11::writable::list out = give(cv.path);
  out.push_back("cv_mean"_nm = give(cv.cv_mean));
  out.push_back("cv_sd"_nm = give(cv.cv_sd));
  out.push_back("index_min"_nm = cpp11::as_sexp(static_cast<int>(cv.index_min) + 1));
  out.push_back("index_1se"_nm = cpp11::as_sexp(static_cast<int>(cv.index_1se) + 1));
  out.push_back("fold_stalled"_nm = give(cv.fold_stalled));
  return out;
}

[[cpp11::register]]
cpp11::doubles ts_penalised_predict_(cpp11::doubles lambda, cpp11::doubles a0,
                                     cpp11::doubles beta, std::string family, double at,
                                     cpp11::doubles newx, int n) {
  const timesift::PenaltyPath path = take(lambda, a0, beta, family);
  std::vector<double> out(static_cast<std::size_t>(n));
  timesift::penalised_predict(path, at, REAL_RO(newx.data()), static_cast<std::size_t>(n), out.data());
  return give(out);
}

[[cpp11::register]]
cpp11::doubles ts_penalised_coef_(cpp11::doubles lambda, cpp11::doubles a0, cpp11::doubles beta,
                                  std::string family, double at) {
  const timesift::PenaltyPath path = take(lambda, a0, beta, family);
  std::vector<double> coef(path.n_column + 1, 0.0);
  timesift::penalised_coef(path, at, coef.data(), coef.data() + 1);
  return give(coef);
}
