#include "arma-wrap.h"
#include "simple-mat.h"
#include <vector>
#include "param-indexer.h"
#include "mmcif-logLik.h"
#include "ghq.h"
#include "wmem.h"
#include "bases.h"
#include <unordered_map>
#include "richardson-extrapolation.h"
#include "log-cholesky.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using Rcpp::IntegerVector;
using Rcpp::NumericVector;
using Rcpp::NumericMatrix;

namespace {

#ifdef _OPENMP
constexpr int openMP_chunk_size{25};
#endif

template<class T>
double nan_if_fail_and_parallel(T x){
#ifdef _OPENMP
  bool const is_in_parallel = omp_in_parallel();
#else
  constexpr bool const is_in_parallel{false};
#endif

  if(is_in_parallel)
    try {
      return x();
    } catch (...) {
      return std::numeric_limits<double>::quiet_NaN();
    }
  return x();
}

simple_mat<double> NumericMatrix_to_simple_mat(NumericMatrix const from){
  simple_mat<double> out(from.nrow(), from.ncol());
  std::copy(from.begin(), from.end(), out.begin());
  return out;
}

/// holds the data to compute the composite likelihood
struct mmcif_data_holder {
  // all matrices are stored as [dimension per obs] x [# Observations]

  /// covariates for the trajectory
  simple_mat<double> covs_trajectory;
  /// derivatives of covs_trajectory w.r.t. time
  simple_mat<double> d_covs_trajectory;
  /// covariates for the risks
  simple_mat<double> covs_risk;
  /// is the probability of the trajectory in (0, 1) (finite) or is it always 1
  std::vector<char> has_finite_trajectory_prob;
  /**
   * the flag for the cause (zero-based). Censoring is indicated by letting this
   * be the number of causes.
   */
  std::vector<unsigned> cause;
  /// 2 x [# pairs] matrix with zero-based indices for the pairs
  simple_mat<size_t> pair_indices;
  /// indices for singletons
  std::vector<size_t> singletons;
  /// the indexer for the parameters
  param_indexer indexer;
  /// covariates for the trajectory with delayed entry
  simple_mat<double> covs_trajectory_delayed;
  /// indices to pairs of each cluster
  std::vector<std::vector<size_t> > clusters_to_pair;

  mmcif_data_holder
    (NumericMatrix const covs_trajectory_in,
     NumericMatrix const d_covs_trajectory_in, NumericMatrix const covs_risk_in,
     IntegerVector const has_finite_trajectory_prob_in,
     IntegerVector const cause_in, size_t const n_causes,
     Rcpp::IntegerMatrix pair_indices_in, IntegerVector const singletons_in,
     NumericMatrix const covs_trajectory_delayed_in,
     IntegerVector const pair_cluster_id):
    covs_trajectory{NumericMatrix_to_simple_mat(covs_trajectory_in)},
    d_covs_trajectory{NumericMatrix_to_simple_mat(d_covs_trajectory_in)},
    covs_risk{NumericMatrix_to_simple_mat(covs_risk_in)},
    has_finite_trajectory_prob
    {
      ([&]{
        std::vector<char> out;
        out.reserve(has_finite_trajectory_prob_in.size());
        for(int val : has_finite_trajectory_prob_in)
          out.emplace_back(val == 1);
        return out;
      })()
    },
    cause
    {
      ([&]{
        std::vector<unsigned> out;
        out.reserve(cause_in.size());
        for(int i : cause_in)
          if(i < 0)
            throw std::invalid_argument("cause has values less than zero");
          else
            out.emplace_back(i);
        return out;
      })()
    },
    pair_indices
    {
      ([&]{
        if(pair_indices_in.nrow() != 2)
          throw std::invalid_argument("pair_indices.nrow() != 2");
        simple_mat<size_t> out(2, pair_indices_in.ncol());

        auto to = out.begin();
        for(auto from = pair_indices_in.begin(); from != pair_indices_in.end();
            ++from, ++to){
          if(*from < 0)
            throw std::invalid_argument("pair_indices has indices less than zero");
          else
            *to = *from;
        }

        return out;
      })()
    },
    singletons
    {
      ([&]{
        std::vector<size_t> out;
        out.reserve(singletons_in.size());
        for(int idx : singletons_in)
          if(idx < 0)
            throw std::invalid_argument("singletons has indices less than zero");
          else
            out.emplace_back(idx);
        return out;
      })()
    },
    indexer{covs_risk.n_rows(), covs_trajectory.n_rows() / n_causes, n_causes},
    covs_trajectory_delayed
      {NumericMatrix_to_simple_mat(covs_trajectory_delayed_in)},
    clusters_to_pair{
      ([&](){
        std::unordered_map<int, std::vector<size_t> > res_map;
        for(R_len_t i = 0; i < pair_cluster_id.size(); ++i)
          res_map[pair_cluster_id[i]].emplace_back(i);

        std::vector<std::vector<size_t> > out;
        out.reserve(res_map.size());
        for(auto &cluster : res_map)
          out.emplace_back(cluster.second);
        return out;
      })()
    }
    {
      if(static_cast<size_t>(pair_cluster_id.length()) != pair_indices.n_cols())
        throw std::invalid_argument("pair_cluster_id.length() != pair_indices.n_cols()");
      throw_if_invalidt();
    }

  bool has_delayed_entry(size_t const idx) const {
    return !std::isnan(*covs_trajectory_delayed.col(idx));
  }

private:
  void throw_if_invalidt(){
    if(covs_risk.n_rows() != indexer.n_cov_risk())
      throw std::invalid_argument("covs_risk.n_rows() != indexer.n_cov_risk()");

    if(covs_trajectory.n_rows() != indexer.n_cov_traject() * indexer.n_causes())
      throw std::invalid_argument("covs_trajectory.n_rows() != indexer.n_cov_traject() * indexer.n_causes()");
    if(d_covs_trajectory.n_rows() != indexer.n_cov_traject() * indexer.n_causes())
      throw std::invalid_argument("d_covs_trajectory.n_rows() != indexer.n_cov_traject() * indexer.n_causes()");
    if(covs_trajectory_delayed.n_rows() != indexer.n_cov_traject() * indexer.n_causes())
      throw std::invalid_argument("covs_trajectory_delayed.n_rows() != indexer.n_cov_traject() * indexer.n_causes()");

    size_t const n_obs{covs_risk.n_cols()};
    if(covs_trajectory.n_cols() != n_obs)
      throw std::invalid_argument("covs_trajectory.n_cols() != n_obs");
    if(d_covs_trajectory.n_cols() != n_obs)
      throw std::invalid_argument("d_covs_trajectory.n_cols() != n_obs");
    if(covs_trajectory_delayed.n_cols() != n_obs)
      throw std::invalid_argument("covs_trajectory_delayed.n_cols() != n_obs");
    if(has_finite_trajectory_prob.size() != n_obs)
      throw std::invalid_argument("has_finite_trajectory_prob.size() != n_obs");
    if(cause.size() != n_obs)
      throw std::invalid_argument("cause.size() != n_obs");

    for(size_t idx : pair_indices)
      if(idx >= n_obs)
        throw std::invalid_argument("pair_indices has an index that is out of bounds");
    for(size_t idx : singletons)
      if(idx >= n_obs)
        throw std::invalid_argument("singletons has an index that is out of bounds");

    std::vector<int> any_with_cause(indexer.n_causes(), 0);
    auto finite_prob = has_finite_trajectory_prob.begin();
    for(unsigned cause_i : cause){
      if(cause_i > indexer.n_causes())
        throw std::invalid_argument("there is cause with a value greater than the number of causes");
      else if(cause_i < indexer.n_causes())
        any_with_cause[cause_i] = 1;

      if(*finite_prob++ == 0 && cause_i < indexer.n_causes())
        throw std::runtime_error("there is an observed outcome with a probability of the trajectory which is one");
    }

    for(int any_with_cause_i : any_with_cause)
      if(any_with_cause_i < 1)
        throw std::invalid_argument("there is cause with no observations");
  }
};

ghqCpp::ghq_data ghq_data_from_list(Rcpp::List dat){
  NumericVector nodes = dat["node"],
              weigths = dat["weight"];
  if(nodes.size() != weigths.size())
    throw std::runtime_error("nodes.size() != weigths.size()");

  return { &nodes[0], &weigths[0], static_cast<size_t>(nodes.size()) };
}

void throw_if_invalid_par
  (mmcif_data_holder const &data, NumericVector const par){
  if(static_cast<size_t>(par.size()) != data.indexer.n_par<false>())
    throw std::invalid_argument(
        "invalid length of parameter vector (" +
          std::to_string(par.size()) + " vs " +
          std::to_string(data.indexer.n_par<false>()) + ')');
}

void throw_if_invalid_par_upper_tri
  (mmcif_data_holder const &data, NumericVector const par){
  if(static_cast<size_t>(par.size()) != data.indexer.n_par<true>())
    throw std::invalid_argument(
        "invalid length of parameter vector (" +
          std::to_string(par.size()) + " vs " +
          std::to_string(data.indexer.n_par<true>()) + ')');
}

void throw_if_invalid_par_wo_vcov
  (mmcif_data_holder const &data, NumericVector const par){
  if(static_cast<size_t>(par.size()) != data.indexer.n_par_wo_vcov())
    throw std::invalid_argument(
        "invalid length of parameter vector (" +
          std::to_string(par.size()) + " vs " +
          std::to_string(data.indexer.n_par_wo_vcov()) + ')');
}

mmcif_data mmcif_data_from_idx
  (mmcif_data_holder const &data, size_t const idx){
  return {
    data.covs_trajectory.col(idx), data.d_covs_trajectory.col(idx),
    data.covs_risk.col(idx), data.has_finite_trajectory_prob[idx] == 1,
    data.cause[idx],
    data.has_delayed_entry(idx) ? data.covs_trajectory_delayed.col(idx)
                                : nullptr
  };
}

} // namespace


// [[Rcpp::export("mmcif_data_holder", rng = false)]]
SEXP mmcif_data_holder_to_R
  (NumericMatrix const covs_trajectory,
   NumericMatrix const d_covs_trajectory,
   NumericMatrix const covs_risk,
   IntegerVector const has_finite_trajectory_prob,
   IntegerVector const cause, size_t const n_causes,
   Rcpp::IntegerMatrix pair_indices, IntegerVector const singletons,
   NumericMatrix const covs_trajectory_delayed,
   IntegerVector const pair_cluster_id){
  return Rcpp::XPtr<mmcif_data_holder const>
    (new mmcif_data_holder
       (covs_trajectory, d_covs_trajectory, covs_risk,
        has_finite_trajectory_prob, cause, n_causes, pair_indices,
        singletons, covs_trajectory_delayed, pair_cluster_id));
}

// [[Rcpp::export(rng = false)]]
int mmcif_n_terms(SEXP data_ptr){
  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  return data->singletons.size() + data->clusters_to_pair.size();
}

// [[Rcpp::export("mmcif_logLik_cpp", rng = false)]]
double mmcif_logLik_to_R
  (SEXP data_ptr, NumericVector const par, Rcpp::List ghq_data,
   unsigned n_threads){
  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par(*data, par);
  n_threads = std::max<unsigned>(1, n_threads);
  wmem::setup_working_memory(n_threads);
  auto ghq_data_pass = ghq_data_from_list(ghq_data);

  double out{};
  size_t const n_pairs{data->pair_indices.n_cols()},
          n_singletons{data->singletons.size()};
  double const * const par_ptr{&par[0]};

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
#endif
  {
#ifdef _OPENMP
#pragma omp for reduction(+:out) schedule(static, openMP_chunk_size)
#endif
    for(size_t i = 0; i < n_pairs; ++i)
      out += nan_if_fail_and_parallel([&]{
        auto mmcif_dat1 =
          mmcif_data_from_idx(*data, data->pair_indices.col(i)[0]);
        auto mmcif_dat2 =
          mmcif_data_from_idx(*data, data->pair_indices.col(i)[1]);

        wmem::mem_stack().reset();
        return mmcif_logLik(par_ptr, data->indexer, mmcif_dat1, mmcif_dat2,
                            wmem::mem_stack(), ghq_data_pass);
      });

#ifdef _OPENMP
#pragma omp for reduction(+:out) schedule(static, openMP_chunk_size)
#endif
    for(size_t i = 0; i < n_singletons; ++i)
      out += nan_if_fail_and_parallel([&]{
        auto mmcif_dat = mmcif_data_from_idx(*data, data->singletons[i]);

        wmem::mem_stack().reset();
        return mmcif_logLik
          (par_ptr,  data->indexer, mmcif_dat, wmem::mem_stack(), ghq_data_pass);
      });
  }

  return out;
}

/// computes the log composite likelihood and the gradient
double mmcif_logLik_grad
  (mmcif_data_holder const &data, double * const res, double const * const par,
   ghqCpp::ghq_data const &ghq_data_pass, unsigned n_threads){
  n_threads = std::max<unsigned>(1, n_threads);
  wmem::setup_working_memory(n_threads);

  double log_lik{};
  size_t const n_pairs{data.pair_indices.n_cols()},
          n_singletons{data.singletons.size()},
                 n_par{data.indexer.n_par<false>()};

  std::vector<std::vector<double> > grs
    (n_threads, std::vector<double>(n_par, 0.));

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
#endif
  {
   std::vector<double> &gr = grs[wmem::thread_num()];

#ifdef _OPENMP
#pragma omp for reduction(+:log_lik) schedule(static, openMP_chunk_size)
#endif
    for(size_t i = 0; i < n_pairs; ++i)
      log_lik += nan_if_fail_and_parallel([&]{
        auto mmcif_dat1 =
          mmcif_data_from_idx(data, data.pair_indices.col(i)[0]);
        auto mmcif_dat2 =
          mmcif_data_from_idx(data, data.pair_indices.col(i)[1]);

        wmem::mem_stack().reset_to_mark();
        return mmcif_logLik_grad(
          par, gr.data(), data.indexer, mmcif_dat1, mmcif_dat2,
          wmem::mem_stack(), ghq_data_pass);
      });

#ifdef _OPENMP
#pragma omp for reduction(+:log_lik) schedule(static, openMP_chunk_size)
#endif
    for(size_t i = 0; i < n_singletons; ++i)
      log_lik += nan_if_fail_and_parallel([&]{
        auto mmcif_dat = mmcif_data_from_idx(data, data.singletons[i]);

        wmem::mem_stack().reset_to_mark();
        return mmcif_logLik_grad
          (par, gr.data(), data.indexer, mmcif_dat, wmem::mem_stack(),
           ghq_data_pass);
      });
  }

  for(size_t thread = 0; thread < n_threads; ++thread)
    for(size_t i = 0; i < n_par; ++i)
      res[i] += grs[thread][i];

  return log_lik;
}

// [[Rcpp::export("mmcif_logLik_grad_cpp", rng = false)]]
Rcpp::NumericVector mmcif_logLik_grad_to_R
  (SEXP data_ptr, NumericVector const par, Rcpp::List ghq_data,
   unsigned const n_threads){
  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par(*data, par);
  auto ghq_data_pass = ghq_data_from_list(ghq_data);

  size_t const n_par{data->indexer.n_par<false>()};
  Rcpp::NumericVector res(n_par);

  res.attr("logLik") = mmcif_logLik_grad
    (*data, &res[0], &par[0], ghq_data_pass, n_threads);

  return res;
}

// [[Rcpp::export("mmcif_sandwich_cpp", rng = false)]]
Rcpp::List mmcif_sandwich
  (SEXP data_ptr, NumericVector const par, Rcpp::List ghq_data,
   unsigned n_threads, double const eps = 0.0001, double const scale = 2,
   double const tol = 0.00000001, unsigned const order = 6){
  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par_upper_tri(*data, par);
  n_threads = std::max<unsigned>(1, n_threads);
  wmem::setup_working_memory(n_threads);
  auto ghq_data_pass = ghq_data_from_list(ghq_data);

  auto map_tri_to_full = [&](double const *upper, double *full){
    auto &indexer = data->indexer;
    std::copy(upper, upper + indexer.vcov(), full);
    log_chol::pd_mat::get
      (upper + indexer.vcov(), 2 * indexer.n_causes(), full + indexer.vcov(),
       wmem::mem_stack());
  };

  auto d_map_tri_to_full =
    [&](double const *upper, double *d_upper, double const *d_full){
    auto &indexer = data->indexer;
    for(size_t i = 0; i < indexer.vcov(); ++i)
      d_upper[i] += d_full[i];

    log_chol::dpd_mat::get
      (upper + indexer.vcov(), 2 * indexer.n_causes(),
       d_upper + indexer.vcov(), d_full + indexer.vcov(), wmem::mem_stack());
  };

  // compute the hessian
  size_t const n_pars{data->indexer.n_par<true>()},
          n_pars_full{data->indexer.n_par<false>()};
  std::vector<double> const par_cp(par.begin(), par.end());

  size_t const n_cluster_pairs{data->clusters_to_pair.size()},
               n_singletons{data->singletons.size()};

  Rcpp::NumericMatrix hessian(n_pars, n_pars);
  {
    std::vector<double> my_par_cp = par_cp,
                        my_par_full(n_pars_full);

    struct {
      decltype(map_tri_to_full) const &map_tri;
      decltype(d_map_tri_to_full) const &d_map_tri;
      ghqCpp::ghq_data const &ghq_data_pass;
      mmcif_data_holder const &data;
      size_t which_var;
      std::vector<double> &par, &par_full;
      size_t const n_pars, n_pars_full;
      unsigned const n_threads;

      void operator()(double const x, double *gr) const {
        double const old_val{par[which_var]};
        par[which_var] = x;

        map_tri(par.data(), par_full.data());

        double * const gr_inner{wmem::mem_stack().get(n_pars)},
               * const gr_inner_full{wmem::mem_stack().get(n_pars_full)};
        auto gr_inner_mark = wmem::mem_stack().set_mark_raii();

        std::fill(gr_inner, gr_inner + n_pars, 0);
        std::fill(gr_inner_full, gr_inner_full + n_pars_full, 0);

        mmcif_logLik_grad
          (data, gr_inner_full, par_full.data(), ghq_data_pass, n_threads);
        d_map_tri(par.data(), gr_inner, gr_inner_full);

        std::copy(gr_inner, gr_inner + which_var + 1, gr);
        par[which_var] = old_val;
      }
    } functor{map_tri_to_full, d_map_tri_to_full, ghq_data_pass, *data, 0,
              my_par_cp, my_par_full, n_pars, n_pars_full, n_threads};

    for(size_t var = 0; var < n_pars; ++var){
      functor.which_var = var;

      using comp_obj = ghqCpp::richardson_extrapolation<decltype(functor)>;
      double * const wk_mem
        {wmem::mem_stack()
          .get(ghqCpp::n_wk_mem_extrapolation(var + 1, order))};
      auto wk_mem_mark = wmem::mem_stack().set_mark_raii();

      comp_obj
        (functor, order, wk_mem, eps, scale, tol, var + 1)
        (my_par_cp[var], &hessian[hessian.nrow() * var]);
    }
  }

  // compute the outer products of the scores
  std::vector<simple_mat<double> > meats
  {
    ([&]{
      simple_mat<double> blank_mat(n_pars, n_pars);
      std::fill(blank_mat.begin(), blank_mat.end(), 0);
      return std::vector<simple_mat<double> >(n_threads, blank_mat);
    })()
  };

  std::vector<double> const par_cp_full
  {
    ([&]{
      std::vector<double> out(n_pars_full);
      map_tri_to_full(par_cp.data(), out.data());
      return out;
    })()
  };

#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
#endif
  {
    simple_mat<double> &my_meat = meats[wmem::thread_num()];
    std::vector<double> gr_inner(n_pars),
                   gr_inner_full(n_pars_full);

#ifdef _OPENMP
#pragma omp for schedule(static, openMP_chunk_size)
#endif
    for(size_t cluster = 0; cluster < n_cluster_pairs; ++cluster){
      auto &indices = data->clusters_to_pair[cluster];
      std::fill(gr_inner.begin(), gr_inner.end(), 0);

      for(size_t idx : indices){
        std::fill(gr_inner_full.begin(), gr_inner_full.end(), 0);

        nan_if_fail_and_parallel([&]{
          auto mmcif_dat1 =
            mmcif_data_from_idx(*data, data->pair_indices.col(idx)[0]);
          auto mmcif_dat2 =
            mmcif_data_from_idx(*data, data->pair_indices.col(idx)[1]);

          return mmcif_logLik_grad(
            par_cp_full.data(), gr_inner_full.data(), data->indexer, mmcif_dat1,
            mmcif_dat2, wmem::mem_stack(), ghq_data_pass);
        });

        d_map_tri_to_full(par_cp.data(), gr_inner.data(), gr_inner_full.data());
      }

      for(size_t j = 0; j < n_pars; ++j){
        for(size_t i = 0; i < j; ++i){
          my_meat.col(j)[i] += gr_inner[i] * gr_inner[j];
          my_meat.col(i)[j] += gr_inner[i] * gr_inner[j];
        }
        my_meat.col(j)[j] += gr_inner[j] * gr_inner[j];
      }
    }

#ifdef _OPENMP
#pragma omp for schedule(static, openMP_chunk_size)
#endif
    for(size_t single_idx = 0; single_idx < n_singletons; ++single_idx){
      auto mmcif_dat = mmcif_data_from_idx(*data, data->singletons[single_idx]);
      std::fill(gr_inner.begin(), gr_inner.end(), 0);
      std::fill(gr_inner_full.begin(), gr_inner_full.end(), 0);

      nan_if_fail_and_parallel([&]{
        return mmcif_logLik_grad(
          par_cp_full.data(), gr_inner_full.data(), data->indexer, mmcif_dat,
          wmem::mem_stack(), ghq_data_pass);
      });

      d_map_tri_to_full(par_cp.data(), gr_inner.data(), gr_inner_full.data());

      for(size_t j = 0; j < n_pars; ++j){
        for(size_t i = 0; i < j; ++i){
          my_meat.col(j)[i] += gr_inner[i] * gr_inner[j];
          my_meat.col(i)[j] += gr_inner[i] * gr_inner[j];
        }
        my_meat.col(j)[j] += gr_inner[j] * gr_inner[j];
      }
    }
  }

  Rcpp::NumericMatrix meat(n_pars, n_pars);
  for(unsigned k = 0; k < n_threads; ++k)
    for(size_t j = 0; j < n_pars; ++j)
      for(size_t i = 0; i < n_pars; ++i)
        meat(i, j) += meats[k].col(j)[i];

  return Rcpp::List::create(Rcpp::_("hessian") = hessian, Rcpp::_("meat") = meat);
}

// [[Rcpp::export("mcif_logLik", rng = false)]]
double mcif_logLik_to_R
  (SEXP data_ptr, NumericVector const par, unsigned n_threads,
   bool const with_risk){

  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par_wo_vcov(*data, par);
  n_threads = std::max<unsigned>(1, n_threads);
  wmem::setup_working_memory(n_threads);

  double out{};
  double const * const par_ptr{&par[0]};

  size_t const n_terms{data->cause.size()};
#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
#endif
  {
    auto &mem = wmem::mem_stack();

#ifdef _OPENMP
#pragma omp for reduction(+:out) schedule(static, openMP_chunk_size)
#endif
    for(size_t i = 0; i < n_terms; ++i){
      out += nan_if_fail_and_parallel([&]{
        auto mmcif_dat = mmcif_data_from_idx(*data, i);

        return with_risk
          ? mcif_logLik<true>(par_ptr, data->indexer, mmcif_dat, mem)
          : mcif_logLik<false>(par_ptr, data->indexer, mmcif_dat, mem);
      });

      if(i % 100 == 0)
        mem.reset_to_mark();
    }
  }

  return out;
}

// [[Rcpp::export("mcif_logLik_grad", rng = false)]]
Rcpp::NumericVector mcif_logLik_grad_to_R
  (SEXP data_ptr, NumericVector const par, unsigned n_threads,
   bool const with_risk){

  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par_wo_vcov(*data, par);
  n_threads = std::max<unsigned>(1, n_threads);
  wmem::setup_working_memory(n_threads);

  double log_likelihood{};
  double const * const par_ptr{&par[0]};

  auto const n_grads = data->indexer.n_par_wo_vcov();
  std::vector<std::vector<double> > grads
    (n_threads, std::vector<double>(n_grads, 0));

  size_t const n_terms{data->cause.size()};
#ifdef _OPENMP
#pragma omp parallel num_threads(n_threads)
#endif
  {
    auto &mem = wmem::mem_stack();
    auto &my_grad = grads[wmem::thread_num()];

#ifdef _OPENMP
#pragma omp for reduction(+:log_likelihood) schedule(static, openMP_chunk_size)
#endif
    for(size_t i = 0; i < n_terms; ++i){
      log_likelihood += nan_if_fail_and_parallel([&]{
        auto mmcif_dat = mmcif_data_from_idx(*data, i);

        return with_risk
          ? mcif_logLik_grad<true>
              (par_ptr, my_grad.data(), data->indexer, mmcif_dat, mem)
          : mcif_logLik_grad<false>
              (par_ptr, my_grad.data(), data->indexer, mmcif_dat, mem);
      });

      if(i % 100 == 0)
        mem.reset_to_mark();
    }
  }

  Rcpp::NumericVector out(n_grads);
  for(auto &grad_res : grads)
    for(size_t i = 0; i < n_grads; ++i)
      out[i] += grad_res[i];

  out.attr("log_likelihood") = log_likelihood;

  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP ns_ptr(const arma::vec &boundary_knots, const arma::vec &interior_knots){
  return Rcpp::XPtr<bases::ns>(new bases::ns(boundary_knots, interior_knots));
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix ns_eval
  (SEXP ptr, Rcpp::NumericVector const points, int const ders){
  Rcpp::XPtr<bases::ns> basis(ptr);
  auto const n_wmem = basis->n_wmem();
  std::vector<double> mem(n_wmem);

  size_t const n_out = points.length();
  auto const dim = basis->n_basis();
  std::vector<double> res_i(dim);

  Rcpp::NumericMatrix out(n_out, dim);

  for(size_t j = 0; j < n_out; ++j){
    if(!std::isfinite(points[j])){
      for(size_t i = 0; i < dim; ++i)
        out(j, i) = std::numeric_limits<double>::quiet_NaN();
      continue;
    }

    (*basis)(res_i.data(), mem.data(), points[j], ders);
    for(size_t i = 0; i < dim; ++i)
      out(j, i) = res_i[i];
  }

  return out;
}

// [[Rcpp::export(rng = false)]]
double mmcif_pd_univariate_cpp
  (SEXP data_ptr, NumericVector const par, Rcpp::List ghq_data,
   NumericVector const cov_trajectory, NumericVector const d_cov_trajectory,
   NumericVector const cov_risk, bool const has_finite_trajectory_prob,
   unsigned const cause, Rcpp::NumericVector const cov_trajectory_delayed,
   bool const deriv){

  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par(*data, par);
  wmem::setup_working_memory(1);
  auto ghq_data_pass = ghq_data_from_list(ghq_data);

  bool const has_delayed_entry{!std::isnan(cov_trajectory_delayed[0])};
  return mmcif_log_mcif
    (&par[0], data->indexer,
     { &cov_trajectory[0], &d_cov_trajectory[0], &cov_risk[0],
       has_finite_trajectory_prob, cause,
       has_delayed_entry ? &cov_trajectory_delayed[0] : nullptr},
     wmem::mem_stack(), ghq_data_pass, deriv);
}

// [[Rcpp::export(rng = false)]]
double mmcif_pd_bivariate_cpp
  (SEXP data_ptr, NumericVector const par, Rcpp::List ghq_data,
   arma::mat const &cov_trajectory, arma::mat const &d_cov_trajectory,
   arma::mat const &cov_risk,
   Rcpp::IntegerVector const has_finite_trajectory_prob,
   Rcpp::IntegerVector const cause, arma::mat const &cov_trajectory_delayed,
   Rcpp::IntegerVector const derivs){

  Rcpp::XPtr<mmcif_data_holder const> data(data_ptr);
  throw_if_invalid_par(*data, par);
  wmem::setup_working_memory(1);
  auto ghq_data_pass = ghq_data_from_list(ghq_data);

  bool const has_delayed_entry1{!std::isnan(cov_trajectory_delayed.col(0)[0])},
             has_delayed_entry2{!std::isnan(cov_trajectory_delayed.col(1)[0])};

  mmcif_data const obs1
    {cov_trajectory.colptr(0), d_cov_trajectory.colptr(0), cov_risk.colptr(0),
     static_cast<bool>(has_finite_trajectory_prob[0]),
     static_cast<unsigned>(cause[0]),
     has_delayed_entry1 ? cov_trajectory_delayed.colptr(0) : nullptr};
  mmcif_data const obs2
    {cov_trajectory.colptr(1), d_cov_trajectory.colptr(1), cov_risk.colptr(1),
     static_cast<bool>(has_finite_trajectory_prob[1]),
     static_cast<unsigned>(cause[1]),
     has_delayed_entry2 ? cov_trajectory_delayed.colptr(1) : nullptr};

  std::array<bool, 2> const derivs_pass
    {static_cast<bool>(derivs[0]), static_cast<bool>(derivs[1])};

  return mmcif_log_mcif
    (&par[0], data->indexer, obs1, obs2, wmem::mem_stack(), ghq_data_pass,
     derivs_pass);
}
