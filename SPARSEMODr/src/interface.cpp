#include <list>
#include "interface.h"



// ********************************************************************
// ********************************************************************
// COVID19
// [[Rcpp::export]]
Rcpp::List
covid19_model_interface(
    Rcpp::NumericMatrix input_dist_mat,
    Rcpp::IntegerVector input_N_pops,
    Rcpp::IntegerVector input_S_pops,
    Rcpp::IntegerVector input_E_pops,
    Rcpp::IntegerVector input_I_asym_pops,
    Rcpp::IntegerVector input_I_presym_pops,
    Rcpp::IntegerVector input_I_sym_pops,
    Rcpp::IntegerVector input_I_home_pops,
    Rcpp::IntegerVector input_I_hosp_pops,
    Rcpp::IntegerVector input_I_icu1_pops,
    Rcpp::IntegerVector input_I_icu2_pops,
    Rcpp::IntegerVector input_R_pops,
    Rcpp::IntegerVector input_D_pops,
    Rcpp::List input_tw,
    Rcpp::IntegerVector input_realz_seeds,
    Rcpp::NumericVector input_census_area = Rcpp::NumericVector::create(),
    double stoch_sd = 0.05,
    int trans_type = 1,
    double dd_trans_monod_k = 500,
    double frac_beta_asym = 0.55,
    double frac_beta_hosp = 0.05,
    double delta = 1/3.0, //1/2.0
    double recov_a = 1/6.0,
    double recov_p = 1/2.0,
    double recov_s = 1/6.0,
    double recov_home = 1/3.0,
    double recov_icu1 = 1/8.0,
    double recov_icu2 = 1/4.0,//1/6.0
    double asym_rate = 0.40,
    double sym_to_icu_rate = 0.015)
{
    int n_pop = input_N_pops.size();
    if (n_pop != input_dist_mat.nrow())
    {
        Rcpp::stop("nrow(dist_mat) should be same as number of populations=%d", n_pop);
    }

    // Require input_census_area to be provided for trans_type 2
    if ((trans_type == 2) && (input_census_area.length() == 0))
    {
        Rcpp::stop("Parameter input_census_area is required for trans_type 2.");
    }

    // Calculate t_max
    int t_max = input_tw["t_max"];

    // nrow = n_pop * n_realz * t_max
    int n_realz = input_realz_seeds.size();
    int nrow = n_pop * n_realz * t_max;
    Rcpp::IntegerVector out_pops_seed(nrow);
    Rcpp::IntegerVector out_pops_pop(nrow);
    Rcpp::IntegerVector out_pops_time(nrow);
    Rcpp::IntegerVector out_pops_S_pop(nrow);
    Rcpp::IntegerVector out_pops_E_pop(nrow);
    Rcpp::IntegerVector out_pops_I_asym_pop(nrow);
    Rcpp::IntegerVector out_pops_I_presym_pop(nrow);
    Rcpp::IntegerVector out_pops_I_sym_pop(nrow);
    Rcpp::IntegerVector out_pops_I_home_pop(nrow);
    Rcpp::IntegerVector out_pops_I_hosp_pop(nrow);
    Rcpp::IntegerVector out_pops_I_icu1_pop(nrow);
    Rcpp::IntegerVector out_pops_I_icu2_pop(nrow);
    Rcpp::IntegerVector out_pops_R_pop(nrow);
    Rcpp::IntegerVector out_pops_D_pop(nrow);
    Rcpp::IntegerVector out_events_pos(nrow);
    Rcpp::IntegerVector out_events_sym(nrow);
    Rcpp::IntegerVector out_events_total_hosp(nrow);
    Rcpp::IntegerVector out_events_total_icu(nrow);
    Rcpp::IntegerVector out_events_n_death(nrow);
    Rcpp::List input_beta = input_tw["beta"];
    Rcpp::NumericVector input_dist_phi = input_tw["dist_phi"];
    Rcpp::NumericVector input_m = input_tw["m"];
    Rcpp::NumericVector input_imm_frac = input_tw["imm_frac"];
    Rcpp::NumericVector input_hosp_rate = input_tw["hosp_rate"];
    Rcpp::NumericVector input_icu_rate = input_tw["icu_rate"];
    Rcpp::NumericVector input_death_rate = input_tw["death_rate"];
    Rcpp::NumericVector input_recov_hosp = input_tw["recov_hosp"];
    Rcpp::IntegerVector input_window_length = input_tw["window_length"];

    int total_windows = input_tw["total_windows"];
    double *beta = (double *)malloc(n_pop * total_windows * sizeof(double));
    for (int outerIndex = 0; outerIndex < n_pop; outerIndex++) {
        Rcpp::NumericVector inner_beta = input_beta[outerIndex];
        for (int innerIndex = 0; innerIndex < total_windows; innerIndex++) {
            beta[innerIndex + (outerIndex * total_windows)] = inner_beta[innerIndex];
        }
    }

    COVID19ParamStruct params;
    // General parameters
    params.input_realz_seeds = &input_realz_seeds[0];
    params.input_census_area = &input_census_area[0];
    params.input_dist_vec = &input_dist_mat[0];
    params.input_beta = beta;
    params.input_dist_phi = &input_dist_phi[0];
    params.input_m = &input_m[0];
    params.input_imm_frac = &input_imm_frac[0];
    params.input_hosp_rate = &input_hosp_rate[0];
    params.input_icu_rate = &input_icu_rate[0];
    params.input_death_rate = &input_death_rate[0];
    params.input_recov_hosp = &input_recov_hosp[0];
    params.input_window_length = &input_window_length[0];
    params.total_windows = total_windows;
    params.n_realz = n_realz;
    params.n_pop = n_pop;
    params.t_max = t_max;
    params.tau = 1;
    params.n_equations = 11;
    params.trans_type = trans_type;
    params.stoch_sd = stoch_sd;
    params.dd_trans_monod_k = dd_trans_monod_k;
    // COVID19 parameters
    params.input_N_pops = &input_N_pops[0];
    params.input_S_pops = &input_S_pops[0];
    params.input_E_pops = &input_E_pops[0];
    params.input_I_asym_pops = &input_I_asym_pops[0];
    params.input_I_presym_pops = &input_I_presym_pops[0];
    params.input_I_sym_pops = &input_I_sym_pops[0];
    params.input_I_home_pops = &input_I_home_pops[0];
    params.input_I_hosp_pops = &input_I_hosp_pops[0];
    params.input_I_icu1_pops = &input_I_icu1_pops[0];
    params.input_I_icu2_pops = &input_I_icu2_pops[0];
    params.input_R_pops = &input_R_pops[0];
    params.input_D_pops = &input_D_pops[0];
    params.delta = delta;
    params.recov_a = recov_a;
    params.recov_p = recov_p;
    params.recov_s = recov_s;
    params.recov_home = recov_home;
    params.recov_icu1 = recov_icu1;
    params.recov_icu2 = recov_icu2;
    params.asym_rate = asym_rate;
    params.sym_to_icu_rate = sym_to_icu_rate;
    params.frac_beta_asym = frac_beta_asym;
    params.frac_beta_hosp = frac_beta_hosp;

    int status = covid19_model( &params,
                                &out_pops_seed[0],
                                &out_pops_pop[0],
                                &out_pops_time[0],
                                &out_pops_S_pop[0],
                                &out_pops_E_pop[0],
                                &out_pops_I_asym_pop[0],
                                &out_pops_I_presym_pop[0],
                                &out_pops_I_sym_pop[0],
                                &out_pops_I_home_pop[0],
                                &out_pops_I_hosp_pop[0],
                                &out_pops_I_icu1_pop[0],
                                &out_pops_I_icu2_pop[0],
                                &out_pops_R_pop[0],
                                &out_pops_D_pop[0],
                                &out_events_pos[0],
                                &out_events_sym[0],
                                &out_events_total_hosp[0],
                                &out_events_total_icu[0],
                                &out_events_n_death[0]);

    free(beta);

    if(status == ERROR_POP_FILE_NOT_FOUND)
    {
       Rcpp::stop("pop file not found");
    }

    // https://stackoverflow.com/questions/46285682/rcppdataframecreate-is-limited-by-20-arguments
    // this List of two DataFrames is to overcome to Rcpp limit of 20
    // columns per DataFrame.
    return Rcpp::List::create(
       Rcpp::Named("pops", Rcpp::DataFrame::create(
               Rcpp::Named("seed", out_pops_seed),
               Rcpp::Named("pop", out_pops_pop),
               Rcpp::Named("time", out_pops_time),
               Rcpp::Named("S_pop", out_pops_S_pop),
               Rcpp::Named("E_pop", out_pops_E_pop),
               Rcpp::Named("I_asym_pop", out_pops_I_asym_pop),
               Rcpp::Named("I_presym_pop", out_pops_I_presym_pop),
               Rcpp::Named("I_sym_pop", out_pops_I_sym_pop),
               Rcpp::Named("I_home_pop", out_pops_I_home_pop),
               Rcpp::Named("I_hosp_pop", out_pops_I_hosp_pop),
               Rcpp::Named("I_icu1_pop", out_pops_I_icu1_pop),
               Rcpp::Named("I_icu2_pop", out_pops_I_icu2_pop),
               Rcpp::Named("R_pop", out_pops_R_pop),
               Rcpp::Named("D_pop", out_pops_D_pop)
       )),
       Rcpp::Named("events", Rcpp::DataFrame::create(
               Rcpp::Named("pos", out_events_pos),
               Rcpp::Named("sym", out_events_sym),
               Rcpp::Named("total_hosp", out_events_total_hosp),
               Rcpp::Named("total_icu", out_events_total_icu),
               Rcpp::Named("n_death", out_events_n_death)
       ))
    );
}

// ********************************************************************
// ********************************************************************
// SEIR
// [[Rcpp::export]]
Rcpp::List
seir_model_interface(
    Rcpp::NumericMatrix input_dist_mat,
    Rcpp::IntegerVector input_N_pops,
    Rcpp::IntegerVector input_S_pops,
    Rcpp::IntegerVector input_E_pops,
    Rcpp::IntegerVector input_I_pops,
    Rcpp::IntegerVector input_R_pops,
    Rcpp::List input_tw,
    Rcpp::IntegerVector input_realz_seeds,
    Rcpp::NumericVector input_census_area = Rcpp::NumericVector::create(),
    double stoch_sd = 0.05,
    int trans_type = 1,
    double dd_trans_monod_k = 500,
    double birth = 1/(75*365),
    double incubate = 1/8.0,
    double recov = 1/3.0)
{
    int n_pop = input_N_pops.size();
    if (n_pop != input_dist_mat.nrow())
    {
        Rcpp::stop("nrow(dist_mat) should be same as number of populations=%d", n_pop);
    }

    // Require input_census_area to be provided for trans_type 2
    if ((trans_type == 2) && (input_census_area.length() == 0))
    {
        Rcpp::stop("Parameter input_census_area is required for trans_type 2.");
    }

    // Calculate t_max
    int t_max = input_tw["t_max"];

    // nrow = n_pop * n_realz * t_max
    int n_realz = input_realz_seeds.size();
    int nrow = n_pop * n_realz * t_max;
    Rcpp::IntegerVector out_pops_seed(nrow);
    Rcpp::IntegerVector out_pops_pop(nrow);
    Rcpp::IntegerVector out_pops_time(nrow);
    Rcpp::IntegerVector out_pops_S_pop(nrow);
    Rcpp::IntegerVector out_pops_E_pop(nrow);
    Rcpp::IntegerVector out_pops_I_pop(nrow);
    Rcpp::IntegerVector out_pops_R_pop(nrow);
    Rcpp::IntegerVector out_events_birth(nrow);
    Rcpp::IntegerVector out_events_exposed(nrow);
    Rcpp::IntegerVector out_events_infectious(nrow);
    Rcpp::IntegerVector out_events_recov(nrow);
    Rcpp::IntegerVector out_events_death(nrow);
    Rcpp::List input_beta = input_tw["beta"];
    Rcpp::NumericVector input_dist_phi = input_tw["dist_phi"];
    Rcpp::NumericVector input_m = input_tw["m"];
    Rcpp::NumericVector input_imm_frac = input_tw["imm_frac"];
    Rcpp::IntegerVector input_window_length = input_tw["window_length"];

    int total_windows = input_tw["total_windows"];
    double *beta = (double *)malloc(n_pop * total_windows * sizeof(double));
    for (int outerIndex = 0; outerIndex < n_pop; outerIndex++) {
        Rcpp::NumericVector inner_beta = input_beta[outerIndex];
        for (int innerIndex = 0; innerIndex < total_windows; innerIndex++) {
            beta[innerIndex + (outerIndex * total_windows)] = inner_beta[innerIndex];
        }
    }

    SEIRParamStruct params;
    // General parameters
    params.input_realz_seeds = &input_realz_seeds[0];
    params.input_census_area = &input_census_area[0];
    params.input_dist_vec = &input_dist_mat[0];
    params.input_beta = beta;
    params.input_dist_phi = &input_dist_phi[0];
    params.input_m = &input_m[0];
    params.input_imm_frac = &input_imm_frac[0];
    params.input_window_length = &input_window_length[0];
    params.total_windows = total_windows;
    params.n_realz = n_realz;
    params.n_pop = n_pop;
    params.t_max = t_max;
    params.tau = 1;
    params.n_equations = 4;
    params.trans_type = trans_type;
    params.stoch_sd = stoch_sd;
    params.dd_trans_monod_k = dd_trans_monod_k;
    // SEIR parameters
    params.input_N_pops = &input_N_pops[0];
    params.input_S_pops = &input_S_pops[0];
    params.input_E_pops = &input_E_pops[0];
    params.input_I_pops = &input_I_pops[0];
    params.input_R_pops = &input_R_pops[0];
    params.birth = birth;
    params.incubate = incubate;
    params.recov = recov;

    int status = seir_model(&params,
                            &out_pops_seed[0],
                            &out_pops_pop[0],
                            &out_pops_time[0],
                            &out_pops_S_pop[0],
                            &out_pops_E_pop[0],
                            &out_pops_I_pop[0],
                            &out_pops_R_pop[0],
                            &out_events_birth[0],
                            &out_events_exposed[0],
                            &out_events_infectious[0],
                            &out_events_recov[0],
                            &out_events_death[0]);

                           if(status == ERROR_POP_FILE_NOT_FOUND)
                           {
                               Rcpp::stop("pop file not found");
                           }

                           // https://stackoverflow.com/questions/46285682/rcppdataframecreate-is-limited-by-20-arguments
                           // this List of two DataFrames is to overcome to Rcpp limit of 20
                           // columns per DataFrame.
                           return Rcpp::List::create(
                               Rcpp::Named("pops", Rcpp::DataFrame::create(
                                       Rcpp::Named("seed", out_pops_seed),
                                       Rcpp::Named("pop", out_pops_pop),
                                       Rcpp::Named("time", out_pops_time),
                                       Rcpp::Named("S_pop", out_pops_S_pop),
                                       Rcpp::Named("E_pop", out_pops_E_pop),
                                       Rcpp::Named("I_pop", out_pops_I_pop),
                                       Rcpp::Named("R_pop", out_pops_R_pop)
                               )),
                               Rcpp::Named("events", Rcpp::DataFrame::create(
                                       Rcpp::Named("birth", out_events_birth),
                                       Rcpp::Named("exposed", out_events_exposed),
                                       Rcpp::Named("infectious", out_events_infectious),
                                       Rcpp::Named("recov", out_events_recov),
                                       Rcpp::Named("death", out_events_death)
                               ))
                           );
}



// ********************************************************************
// ********************************************************************
