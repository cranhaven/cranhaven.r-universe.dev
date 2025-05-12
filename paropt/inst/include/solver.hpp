#ifndef SOLVER
#define SOLVER

#include "header.hpp"

double solver_bdf(std::vector<double> &param_combi_start, OS ode_system,
                      time_state_information &solv_param_struc);
double solver_bdf_with_jac(std::vector<double> &param_combi_start, OS ode_system,
                      time_state_information &solv_param_struc);
double solver_bdf_save(std::vector<double> &param_combi_start,
                           OS ode_system, time_state_information solv_param_struc,
                           Rcpp::NumericMatrix &DF);
double solver_bdf_save_with_jac(std::vector<double> &param_combi_start,
                           OS ode_system, time_state_information solv_param_struc,
                           Rcpp::NumericMatrix &DF);
double solver_adams(std::vector<double> &param_combi_start,
                          OS ode_system, time_state_information &solv_param_struc);
double solver_adams_save(std::vector<double> &param_combi_start,
                             OS ode_system,
                             time_state_information solv_param_struc,
                             Rcpp::NumericMatrix &DF);
#endif // SOLVER
