#include "solver.hpp"

// [[Rcpp::export]]
Rcpp::List wrapper_optimizer(
    vd &init_state, vd &par_times, vi &param_idx_cuts, vd &lb_, vd &ub_,
    vd &state_measured, vi &state_idx_cuts, vd &integration_times,
    double reltol, vd &absolute_tolerances, Rcpp::XPtr<OS> fct, int nswarm,
    int ngen, double error, int solvertype, Rcpp::XPtr<error_calc_fct> ecf,
    Rcpp::XPtr<spline_fct> sf, Rcpp::XPtr<JAC> jf, int number_threads) {

  // add parameter to struct
  time_state_information tsi;

  tsi.init_state = init_state;
  tsi.par_times = par_times;
  tsi.param_idx_cuts = param_idx_cuts;
  tsi.state_measured = state_measured;
  tsi.state_idx_cut = state_idx_cuts;
  tsi.integration_times = integration_times;
  tsi.reltol = reltol;
  tsi.absolute_tolerances = absolute_tolerances;
  tsi.ecf = *ecf;
  tsi.sf = *sf;
  tsi.jf = *jf;

  OS ode = *fct;
  av lb = lb_;
  av ub = ub_;
  av best_particle_param_values;

  // define parameters
  int nparms = lb.size();
  double w = 0.5;
  const double w_max = 0.9;
  const double w_min = 0.4;
  double cog = 2.05;
  const double initial_c_cog = 2.5;
  const double final_c_cog = 0.5;
  double soc = 2.05;
  const double initial_c_soc = 0.5;
  const double final_c_soc = 2.5;
  double prop_objfn_val;
  double global_best = std::numeric_limits<double>::max();
  av global_best_vec(nparms);
  size_t iter = 0;
  int convergence_check = 0;
  int convergence_check_stop = 0;
  int idx1, idx2;
  double best_vals_idx1, best_vals_idx2;
  double objfn_vals_idx1, objfn_vals_idx2;
  int n_informants;
  const int K = 3;
  int best_neighberhood_particel;
  arma::vec current_nbhood;
  int temp_fittness_index;
  arma::vec local_best_vec;
  const double v = 0.9;
  double chi;
  double omega;
  std::vector<std::vector<double>> parameter(nswarm);
  for (int i = 0; i < nswarm; i++) {
    parameter[i].resize(nparms);
  }
  int min_objfn_val_index;
  double min_objfn_val;

  // define solver
  solver_ptr objfct = nullptr;
  solver_ptr_save save_fct = nullptr;
  if (solvertype == 1) {
    objfct = solver_bdf;
    save_fct = solver_bdf_save;
  } else if (solvertype == 2) {
    objfct = solver_adams;
    save_fct = solver_adams_save;
  } else if (solvertype == 3) {
    objfct = solver_bdf_with_jac;
    save_fct = solver_bdf_save_with_jac;
  }
  Rcpp::NumericMatrix df(integration_times.size(), init_state.size());

  if (save_fct == nullptr) Rcpp::stop("Undefined solvertype found!");
  if (objfct == nullptr) Rcpp::stop("Undefined solvertype found!");

  // thread pool
  RcppThread::ThreadPool pool(number_threads);
  std::vector<std::future<double>> futures(nswarm);

  // init
  av swarm_errors(nswarm);
  av swarm_bests(nswarm);
  am swarm_bests_params(nswarm, nparms);
  GetRNGstate();
  am swarm(nswarm, nparms, arma::fill::randu);
  PutRNGstate();
  am velo = arma::zeros(nswarm, nparms);
  std::vector<double> param_temp;
  param_temp.resize(nparms);

  for (int i = 0; i < nswarm; i++) {
    GetRNGstate();
    swarm.row(i) = lb.t() + (ub.t() - lb.t()) % arma::randu(1, nparms);
    PutRNGstate();
    for (int j = 0; j < nparms; j++) {
      if (swarm(i, j) > ub(j)) {
        swarm(i, j) = ub(j);
      } else if (swarm(i, j) < lb(j)) {
        swarm(i, j) = lb(j);
      }
      param_temp[j] = swarm(i, j);
    }
    swarm_errors(i) = objfct(param_temp, ode, tsi);
  }

  swarm_bests = swarm_errors;
  swarm_bests_params = swarm;
  global_best = swarm_bests.min();
  global_best_vec = swarm.row(swarm_bests.index_min()).t();

  // neighberhood
  arma::imat nbhood_precursor(
      nswarm, nswarm, arma::fill::eye); // Matrix with elements of main diagonal
                                        // set to 1; remeaining entries = 0
  std::vector<vi> neighberhood(
      nswarm); // neighberhood containts for each particle informants

  // begin generation loop
  while (iter < ngen) {
    // Calculate neighberhood
    if (iter == 0 || convergence_check >= 1) {
      // Randomize indizes of
      for (size_t z = 0; z < 1000; z++) {
        GetRNGstate();
        idx1 = arma::randi<int>(arma::distr_param(0, nswarm - 1));
        PutRNGstate();
        GetRNGstate();
        idx2 = arma::randi<int>(arma::distr_param(0, nswarm - 1));
        PutRNGstate();
        // swap
        swarm.swap_rows(idx1, idx2);
        swarm_bests_params.swap_rows(idx1, idx2);
        velo.swap_rows(idx1, idx2);
        // swap
        best_vals_idx1 = swarm_bests(idx1);
        best_vals_idx2 = swarm_bests(idx2);
        swarm_bests(idx1) = best_vals_idx2;
        swarm_bests(idx2) = best_vals_idx1;
        // swap
        objfn_vals_idx1 = swarm_errors(idx1);
        objfn_vals_idx2 = swarm_errors(idx2);
        swarm_errors(idx1) = objfn_vals_idx2;
        swarm_errors(idx2) = objfn_vals_idx1;
      }

      for (int i = 0; i < nswarm; i++) {
        GetRNGstate();
        n_informants = arma::randi<int>(arma::distr_param(0, K));
        PutRNGstate();
        GetRNGstate();
        arma::ivec informants =
            arma::randi(n_informants, arma::distr_param(0, nswarm - 1));
        PutRNGstate();
        GetRNGstate();
        arma::ivec temp_row = arma::zeros<arma::ivec>(nswarm);
        PutRNGstate();
        temp_row(i) = 1;
        for (int m = 0; m < n_informants; m++) {
          temp_row(informants(m)) = 1;
        }
        arma::irowvec test = temp_row.t();
        nbhood_precursor.row(i) = temp_row.t();
      }
      std::vector<int> size_counter(nswarm);
      for (int i = 0; i < nswarm; i++) {
        int temp_counter = 0;
        for (int j = 0; j < nswarm; j++) {
          if (nbhood_precursor(j, i) == 1) {
            temp_counter = temp_counter + 1;
          }
        }
        size_counter[i] = temp_counter;
        neighberhood[i].resize(size_counter[i]);
        int m_counter = 0;
        for (int m = 0; m < nswarm; m++) {
          if (nbhood_precursor(m, i) == 1) {
            neighberhood[i][m_counter] = m;
            m_counter = m_counter + 1;
          }
        }
      }
      convergence_check = 0;
    }
    iter++;

    // updating par_w_min, par_w_max, c_sog and par_c_cog
    w = w_max - iter * (w_max - w_min) / ngen;
    cog = initial_c_cog - (initial_c_cog - final_c_cog) * (iter + 1) / ngen;
    soc = initial_c_soc - (initial_c_soc - final_c_soc) * (iter + 1) / ngen;

    // first swarm loop
    for (int i = 0; i < nswarm; i++) {

      current_nbhood.resize(neighberhood[i].size());
      for (size_t j = 0; j < neighberhood[i].size(); j++) {
        current_nbhood(j) = swarm_bests(neighberhood[i][j]);
      }
      temp_fittness_index = current_nbhood.index_min();
      best_neighberhood_particel = neighberhood[i][temp_fittness_index];
      local_best_vec = swarm_bests_params.row(best_neighberhood_particel).t();

      chi = 2 * v;
      GetRNGstate();
      omega = cog * arma::randu() + soc * arma::randu();
      if (omega < 4.) {
        omega = 4.;
      }
      PutRNGstate();
      chi = chi / std::abs(2. - omega - std::sqrt(omega * (omega - 4.)));
      GetRNGstate();
      velo.row(i) =
          w * velo.row(i) +
          cog * arma::randu(1, nparms) %
              (swarm_bests_params.row(i) - swarm.row(i)) +
          soc * arma::randu(1, nparms) % (local_best_vec.t() - swarm.row(i));
      PutRNGstate();
      velo.row(i) = velo.row(i) * chi;
      swarm.row(i) += velo.row(i);

      // check if boundaries are violated
      for (int k = 0; k < nparms; k++) {
        if (swarm(i, k) > ub(k)) {
          swarm(i, k) = ub(k);
        } else if (swarm(i, k) < lb(k)) {
          swarm(i, k) = lb(k);
        }
      }
    }

    // eval
    for (int i = 0; i < nswarm; i++) {
      for (int l = 0; l < nparms; l++) {
        parameter[i][l] = swarm(i, l);
      }
    }
    for (int o = 0; o < nswarm; o++) {
      futures[o] =
          pool.pushReturn(objfct, std::ref(parameter[o]), ode, std::ref(tsi));
    }
    for (int o = 0; o < nswarm; o++) {
      swarm_errors(o) = futures[o].get();
    }

    // population loop Nr.3
    for (int i = 0; i < nswarm; i++) {
      if (swarm_errors(i) < swarm_bests(i)) {
        swarm_bests(i) = swarm_errors(i);
        swarm_bests_params.row(i) = swarm.row(i);
      }
    }

    min_objfn_val_index = swarm_bests.index_min();
    min_objfn_val = swarm_bests(min_objfn_val_index);

    if (min_objfn_val < global_best) {
      global_best = min_objfn_val;
      best_particle_param_values =
          swarm_bests_params.row(min_objfn_val_index).t();
      convergence_check_stop = 0;
    } else {
      convergence_check = convergence_check + 1;
      convergence_check_stop = convergence_check_stop + 1;
    }

    if (convergence_check_stop > ((ngen / nswarm) * 10)) {
      int convergence_threshold = (ngen / nswarm) * 10;
      Rcpp::Rcerr << "No convergence for: " << convergence_threshold
                  << " generations. Optimizing stoped" << std::endl;
      break;
    }
    if (iter % 50 == 0) {
      Rcpp::Rcerr << "global best val"
                  << "\t" << global_best << std::endl;
      Rcpp::Rcerr << "Generation number:"
                  << "\t" << iter << std::endl;
    }
    if (global_best <= error) {
      Rcpp::Rcout << "found sufficient solution" << std::endl;
      break;
    }
    Rcpp::checkUserInterrupt();
  } // end generation loop

  global_best_vec = swarm.row(swarm_bests.index_min()).t();

  // generate in silico states
  vd gbv(global_best_vec.size());
  for (size_t i = 0; i < gbv.size(); i++) {
    gbv[i] = global_best_vec(i);
  }
  save_fct(gbv, ode, tsi, df);

  Rcpp::List L = Rcpp::List::create(global_best, global_best_vec, df);
  return L;
}
