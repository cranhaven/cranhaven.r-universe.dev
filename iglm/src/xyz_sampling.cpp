#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <set>
#include <unordered_map>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <limits>
#include "iglm/xyz_class.h"
#include "iglm/extension_api.hpp"

using xyz_ValidateFunction = double(*)(const XYZ_class &object,
                                    const int &unit_i,
                                    const int &unit_j,
                                    const arma::mat &data,
                                    const double &type,
                                    const std::string &mode, const bool &is_full_neighborhood);

//[[Rcpp::depends(RcppProgress)]]

void show_info() {
  auto& reg = iglm::Registry::instance();
  for (auto& name : reg.names()) {
    auto m = reg.info(name);
    Rcpp::Rcout << "Function: " << name
                << " | short: " << m.short_name
                << " | value: " << m.value << "\n";
  }
}

// [[Rcpp::export]]
void iglm_print_registered_functions() {
  auto meta = iglm::Registry::instance().all_meta();
  Rcpp::Rcout << "Registered functions:\n";
  if (meta.empty()) {
    Rcpp::Rcout << "  <none>\n";
    return;
  } 
  
  for (auto& kv : meta) {
    const std::string& name = kv.short_name;
    const iglm::FUN& m = kv;
    Rcpp::Rcout << "  " << name
                << "  (" << m.short_name << ", " << m.value << ")\n";
  }
}


std::vector<xyz_ValidateFunction> xyz_change_statistics_generate_new(std::vector<std::string> terms) {
  // Res
  std::vector<xyz_ValidateFunction> fns;
  fns.reserve(terms.size());
  // Get all the registered functions 
  auto& reg = iglm::Registry::instance();
  
  for (size_t i = 0; i < terms.size(); ++i) {
    const std::string& name = terms[i];
    if (!reg.has(name)) {
      throw std::invalid_argument("The statistic " + name + " does not exist");
    }    fns.push_back(reg.get(name));
  }
  return fns;
}


arma::vec xyz_eval_at_empty_network_new(std::vector<std::string> terms, const XYZ_class& object) {
  arma::vec res(terms.size());
  
  auto& reg = iglm::Registry::instance();
  for (size_t i = 0; i < terms.size(); ++i) {
    const std::string& name = terms[i];
    if (!reg.has(name)) {
      throw std::invalid_argument("The statistic " + name + " does not exist");
    }
    const auto meta = reg.info(name);
    if(meta.value == 1.0){
      res.at(i) = object.n_units;
    } else {
      res.at(i) = meta.value;
    }
    
  }
  return res;
}

// Function to call all functions in the vector functions
inline void xyz_calculate_change_stats(arma::vec & change_stat,
                                       const int unit_i,
                                       const int unit_j,
                                       const XYZ_class &object,
                                       const std::vector<arma::mat> &data_list,
                                       const std::vector<double> &type_list,
                                       const std::string &mode,
                                       const bool &is_full_neighborhood,
                                       const std::vector<xyz_ValidateFunction> functions){
  
  // Rcout << functions.size() << std::endl;
  for (size_t a = 0; a < functions.size(); ++a) {
    change_stat[a] = functions[a](object, unit_i, unit_j, data_list[a], type_list[a], mode, is_full_neighborhood);
  }
}



arma::vec xyz_count_global_statistic( const XYZ_class &object,
                                      std::vector<arma::mat> &data_list,
                                      std::vector<double> &type_list,
                                      std::vector<xyz_ValidateFunction> functions, 
                                      std::string family_x, 
                                      std::string family_y, 
                                      double attr_x_scale, 
                                      double attr_y_scale) {
  // Start with empty network and zero attributes, filling them incrementally
  XYZ_class alt_object(object.n_units, object.z_network.directed, 
                       object.neighborhood, 
                       object.overlap,
                       object.overlap_mat,
                       family_x, family_y, attr_x_scale, attr_y_scale);
  bool is_full_neighborhood = object.check_if_full_neighborhood();
  arma::vec res(functions.size());
  res.fill(0);
  arma::vec change_stat(functions.size());
  std::vector<int> tmp_js;
  std::string z = "z", x = "x", y = "y";

  // 1. Attribute loop for X (on empty network and zero Y)
  for(int i = 1; i <= (int)object.x_attribute.attribute.size(); i++){
    xyz_calculate_change_stats(change_stat, i,
                               i,
                               alt_object,
                               data_list,
                               type_list,
                               x,
                               is_full_neighborhood,
                               functions);
    
    alt_object.x_attribute.set_attr_value(i, object.x_attribute.attribute.at(i-1));
    res += change_stat * object.x_attribute.get_val(i);
  }
  
  // 2. Attribute loop for Y (on empty network with observed X)
  for(int i = 1; i <= (int)object.y_attribute.attribute.size(); i++){
    xyz_calculate_change_stats(change_stat, i,
                               i,
                               alt_object,
                               data_list,
                               type_list,
                               y,
                               is_full_neighborhood,
                               functions);
    alt_object.y_attribute.set_attr_value(i, object.y_attribute.attribute.at(i-1));
    res += change_stat * object.y_attribute.get_val(i);
  }

  // 3. Network edge loop (with full observed X and Y attributes)
  for (int i = 1; i <= object.n_units; i++){
    tmp_js = object.z_network.adj_list.at(i);
    if(tmp_js.size() > 0){
      auto it = tmp_js.begin();
      while (it != tmp_js.end()) {
        if(*it == i){ 
        } else if(!object.z_network.directed){
          if(*it > i){
            xyz_calculate_change_stats(change_stat, i,
                                       *it,
                                       alt_object,
                                       data_list,
                                       type_list,
                                       z,
                                       is_full_neighborhood,
                                       functions);
            alt_object.add_edge(i, *it);
            res += change_stat;
          }
        } else {
          xyz_calculate_change_stats(change_stat, i,
                                     *it,
                                     alt_object,
                                     data_list,
                                     type_list,
                                     z,
                                     is_full_neighborhood,
                                     functions);
          alt_object.add_edge(i, *it);
          res += change_stat;
        }
        it++;
      }
    }
  }
  
  return(res);
}



// [[Rcpp::export]]
arma::vec xyz_count_global(const arma::mat& z_network,
                           const arma::vec& x_attribute,
                           const arma::vec& y_attribute,
                           const arma::mat& neighborhood,
                           const arma::mat& overlap,
                           bool directed,
                           std::vector<std::string> terms,
                           int n_units,
                           std::vector<arma::mat> &data_list,
                           std::vector<double> &type_list, 
                           std::string family_x, 
                           std::string family_y, 
                           double attr_x_scale, 
                           double attr_y_scale) {
  // std::unordered_map< int, std::unordered_set<int>> edges;
  // // Convert the matrix to two unordered_map objects
  // edges = mat_to_map(network,1, n_units);
  XYZ_class object(n_units,directed, x_attribute,y_attribute,z_network, neighborhood, overlap, family_x, family_y,attr_x_scale, attr_y_scale);
  // object.initialize(z_network, x_attribute,y_attribute,neighborhood );
  // object.print();
  std::vector<xyz_ValidateFunction> functions, functions_new;
  // functions = xyz_change_statistics_generate(terms);
  functions = xyz_change_statistics_generate_new(terms);
  arma::vec at_zero;
  at_zero = xyz_eval_at_empty_network_new(terms, object);
  // Rcout << xyz_change_statistics_generate_new(terms) << std::endl;
  // Rcout << functions << std::endl;
  // Rcout << at_zero << std::endl;
  
  arma::vec global_stats(xyz_count_global_statistic(object,
                                                    data_list,
                                                    type_list,
                                                    functions, 
                                                    family_x, 
                                                    family_y, 
                                                    attr_x_scale, 
                                                    attr_y_scale));
  global_stats = global_stats + at_zero;
  
  // arma::vec global_stats(xyz_count_global_statistic(object,
  //                                                   data_list,
  //                                                   type_list,
  //                                                   functions, 
  //                                                   family_x, 
  //                                                   family_y, 
  //                                                   attr_x_scale, 
  //                                                   attr_y_scale));
  // global_stats = global_stats + at_zero;
  // arma::vec global_stats;
  // Rcout << global_stats_new << std::endl;
  // Rcout <<global_stats << std::endl;
  
  return(global_stats);
}

arma::vec xyz_count_global_internal(const XYZ_class& object,
                                    std::vector<std::string> terms,
                                    int n_units,
                                    std::vector<arma::mat> &data_list,
                                    std::vector<double> &type_list, 
                                    std::string family_x, 
                                    std::string family_y, 
                                    double attr_x_scale, 
                                    double attr_y_scale) {
  std::vector<xyz_ValidateFunction> functions;
  functions = xyz_change_statistics_generate_new(terms);
  arma::vec at_zero;
  // Rcout << "at_zero" << std::endl;
  at_zero = xyz_eval_at_empty_network_new(terms, object);
  // Rcout << "at_zero" << std::endl;
  arma::vec global_stats(xyz_count_global_statistic(object,
                                                    data_list,
                                                    type_list,
                                                    functions, family_x, 
                                                    family_y, 
                                                    attr_x_scale, 
                                                    attr_y_scale));
  global_stats = global_stats + at_zero;
  // Rcout << "at_zero" << std::endl;
  // arma::vec global_stats;
  return(global_stats);
}



void xyz_simulate_network_consecutive_mh( const arma::vec &coef,
                                          XYZ_class &object,
                                          const std::vector<arma::mat> &data_list,
                                          const std::vector<double> &type_list,
                                          const bool &is_full_neighborhood,
                                          const std::vector<xyz_ValidateFunction> &functions,
                                          arma::vec &global_stats, 
                                          const double offset_nonoverlap) {
  std::string z = "z";
  arma::vec change_stat(functions.size());
  arma::vec tmp_vec, tmp_stat;
  
  // Go through a loop for all unit changes
  if(object.z_network.directed){
    for(int i = 1; i <=(object.n_units); ++i) {
      for(int j = 1; j <=(object.n_units); ++j) {
        if(object.get_val_overlap(i, j)){
          continue;
        } 
        if(i == j){
          continue;
        } 
        // Calculate the change stat for unit_i, unit_j from 0 to 1
        xyz_calculate_change_stats(change_stat, i,
                                   j,
                                   object,
                                   data_list,
                                   type_list,
                                   z,
                                   is_full_neighborhood,
                                   functions);
        // 3. Calculate the Hastings Ratios by exp(delta(tmp_entry)*coef)
        tmp_stat=change_stat;
        
        double HR_val = 1.0 / (1.0 + std::exp(-arma::dot(coef, tmp_stat) - offset_nonoverlap));
        // 4. Step: Sample a random number between 0 and 1, accept if it is > HR
        if(R::unif_rand() < HR_val){
          if(object.z_network.get_val(i,j) == 0){
            object.add_edge(i,j);
            global_stats += tmp_stat;
          }
        } else {
          if(object.z_network.get_val(i,j)){
            global_stats -= tmp_stat;  
            object.delete_edge(i,j);
          }
        }
      }
    }  
  } else {
    for(int i = 1; i <=(object.n_units-1); ++i) {
      for(int j = i+1; j <=(object.n_units); ++j) {
        if(object.get_val_overlap(i, j)){
          continue;
        } 
        // Calculate the change stat for unit_i, unit_j from 0 to 1
        xyz_calculate_change_stats(change_stat, i,
                                   j,
                                   object,
                                   data_list,
                                   type_list,
                                   z,
                                   is_full_neighborhood,
                                   functions);
        // 3. Calculate the Hastings Ratios by exp(delta(tmp_entry)*coef)
        tmp_stat=change_stat;
        double HR_val = 1.0 / (1.0 + std::exp(-arma::dot(coef, tmp_stat) - offset_nonoverlap));
        // 4. Step: Sample a random number between 0 and 1, accept if it is > HR
        if(R::unif_rand() < HR_val){
          if(object.z_network.get_val(i,j) == 0){
            object.add_edge(i,j);
            global_stats += tmp_stat;
          } 
        } else {
          if(object.z_network.get_val(i,j)){
            global_stats -= tmp_stat;  
            object.delete_edge(i,j);
          } 
        }
      }
    }
  }
  
}


void xyz_simulate_network_consecutive_degrees_mh( const arma::vec &coef_nondegrees,
                                                  const arma::vec &coef_degrees,
                                                  XYZ_class &object,
                                                  const std::vector<arma::mat> &data_list,
                                                  const std::vector<double> &type_list,
                                                  const bool &is_full_neighborhood,
                                                  const std::vector<xyz_ValidateFunction> &functions,
                                                  arma::vec &global_stats, 
                                                  const double offset_nonoverlap) {
  std::string z = "z";
  arma::vec change_stat(functions.size());
  
  // Go through a loop for all unit changes
  if(object.z_network.directed){
    for(int i = 1; i <=(object.n_units); ++i) {
      double coef_degrees_i = coef_degrees(i-1); 
      for(int j = 1; j <=(object.n_units); ++j) {
        
        if(object.get_val_overlap(i, j)){
          continue;
        }  
        if(i==j){
          continue;
        }  
        // Calculate the change stat for unit_i, unit_j from 0 to 1
        xyz_calculate_change_stats(change_stat, i,
                                   j,
                                   object,
                                   data_list,
                                   type_list,
                                   z,
                                   is_full_neighborhood,
                                   functions);
        // Rcout << "Got CS" << std::endl;
        
        // 3. Calculate the Hastings Ratios by exp(delta(tmp_entry)*coef)
        double HR_val = 1.0 / (1.0 + std::exp(-arma::dot(coef_nondegrees, change_stat) - offset_nonoverlap - (coef_degrees_i + coef_degrees(j-1+object.n_units))));
        // 4. Step: Sample a random number between 0 and 1, accept if it is > HR
        if(R::unif_rand() < HR_val){
          if(object.z_network.get_val(i,j) == 0){
            object.add_edge(i,j);
            global_stats += change_stat;
          }
        } else {
          if(object.z_network.get_val(i,j)){
            global_stats -= change_stat;  
            object.delete_edge(i,j);
          }
        }
      }
    }
  } else {
    for(int i = 1; i <=(object.n_units-1); ++i) {
      double coef_degrees_i = coef_degrees(i-1); 
      for(int j = i+1; j <=(object.n_units); ++j) {
        if(object.get_val_overlap(i, j)){
          continue;
        }  
        // Calculate the change stat for unit_i, unit_j from 0 to 1
        xyz_calculate_change_stats(change_stat, i,
                                   j,
                                   object,
                                   data_list,
                                   type_list,
                                   z,
                                   is_full_neighborhood,
                                   functions);
        // 3. Calculate the Hastings Ratios by exp(delta(tmp_entry)*coef)
        double HR_val = 1.0 / (1.0 + std::exp(-arma::dot(coef_nondegrees, change_stat) - offset_nonoverlap - (coef_degrees_i + coef_degrees(j-1))));
        // 4. Step: Sample a random number between 0 and 1, accept if it is > HR
        if(R::unif_rand() < HR_val){
          if(object.z_network.get_val(i,j) == 0){
            object.add_edge(i,j);
            global_stats += change_stat;
          }
        } else {
          if(object.z_network.get_val(i,j)){
            global_stats -= change_stat;  
            object.delete_edge(i,j);
          }
        }
      }
    }
  }
  
}

void xyz_simulate_network_consecutive_mh_directed(const arma::vec &coef,
                                                  XYZ_class &object,
                                                  const std::vector<arma::mat> &data_list,
                                                  const std::vector<double> &type_list,
                                                  const bool &is_full_neighborhood,
                                                  const std::vector<xyz_ValidateFunction> &functions,
                                                  arma::vec &global_stats, 
                                                  const double offset_nonoverlap) {
  std::string z = "z";
  
  arma::vec change_stat_10(functions.size());
  arma::vec change_stat_01(functions.size());
  arma::vec change_stat_11_given_10(functions.size());
  
  
  for(int i = 1; i <= (object.n_units - 1); ++i) {
    for(int j = i + 1; j <= object.n_units; ++j) {
      if(object.get_val_overlap(i, j)){
        continue;
      } 
      
      // Temporarily remove both edges to reach the (0,0) state
      if (object.z_network.get_val(i, j)) {
        xyz_calculate_change_stats(change_stat_10, i, j, object, data_list, type_list, z, is_full_neighborhood, functions);
        global_stats -= change_stat_10;
        object.delete_edge(i, j);
      }
      if (object.z_network.get_val(j, i)) {
        xyz_calculate_change_stats(change_stat_01, j, i, object, data_list, type_list, z, is_full_neighborhood, functions);
        global_stats -= change_stat_01;
        object.delete_edge(j, i);
      }
      
      // Now network is at (0,0) for the dyad.
      xyz_calculate_change_stats(change_stat_10, i, j, object, data_list, type_list, z, is_full_neighborhood, functions);
      xyz_calculate_change_stats(change_stat_01, j, i, object, data_list, type_list, z, is_full_neighborhood, functions);
      
      object.add_edge(i, j);
      xyz_calculate_change_stats(change_stat_11_given_10, j, i, object, data_list, type_list, z, is_full_neighborhood, functions);
      object.delete_edge(i, j); 
      
      double log_P_00 = 0.0;
      double log_P_10 = arma::dot(coef, change_stat_10) + offset_nonoverlap;
      double log_P_01 = arma::dot(coef, change_stat_01) + offset_nonoverlap;
      double log_P_11 = arma::dot(coef, change_stat_10 + change_stat_11_given_10) + 2.0 * offset_nonoverlap;
      
      double max_log_P = std::max({log_P_00, log_P_10, log_P_01, log_P_11});
      double P_00 = std::exp(log_P_00 - max_log_P);
      double P_10 = std::exp(log_P_10 - max_log_P);
      double P_01 = std::exp(log_P_01 - max_log_P);
      double P_11 = std::exp(log_P_11 - max_log_P);
      double sum_P = P_00 + P_10 + P_01 + P_11;
      
      P_00 /= sum_P;
      P_10 /= sum_P;
      P_01 /= sum_P;
      
      double r = R::unif_rand();
      
      if (r < P_00) {
        // stay (0,0)
      } else if (r < P_00 + P_10) { 
        object.add_edge(i, j);
        global_stats += change_stat_10;
      } else if (r < P_00 + P_10 + P_01) { 
        object.add_edge(j, i);
        global_stats += change_stat_01;
      } else { 
        object.add_edge(i, j);
        object.add_edge(j, i);
        global_stats += change_stat_10 + change_stat_11_given_10;
      }
    }
  }
}

void xyz_simulate_network_consecutive_degrees_mh_directed(const arma::vec &coef_nondegrees,
                                                          const arma::vec &coef_degrees,
                                                          XYZ_class &object,
                                                          const std::vector<arma::mat> &data_list,
                                                          const std::vector<double> &type_list,
                                                          const bool &is_full_neighborhood,
                                                          const std::vector<xyz_ValidateFunction> &functions,
                                                          arma::vec &global_stats, 
                                                          const double offset_nonoverlap) {
  std::string z = "z";
  
  arma::vec change_stat_10(functions.size());
  arma::vec change_stat_01(functions.size());
  arma::vec change_stat_11_given_10(functions.size());
  
  
  for(int i = 1; i <= (object.n_units - 1); ++i) {
    for(int j = i + 1; j <= object.n_units; ++j) {
      if(object.get_val_overlap(i, j)){
        continue;
      } 
      
      if (object.z_network.get_val(i, j)) {
        xyz_calculate_change_stats(change_stat_10, i, j, object, data_list, type_list, z, is_full_neighborhood, functions);
        global_stats -= change_stat_10;
        object.delete_edge(i, j);
      }
      if (object.z_network.get_val(j, i)) {
        xyz_calculate_change_stats(change_stat_01, j, i, object, data_list, type_list, z, is_full_neighborhood, functions);
        global_stats -= change_stat_01;
        object.delete_edge(j, i);
      }
      
      xyz_calculate_change_stats(change_stat_10, i, j, object, data_list, type_list, z, is_full_neighborhood, functions);
      xyz_calculate_change_stats(change_stat_01, j, i, object, data_list, type_list, z, is_full_neighborhood, functions);
      
      object.add_edge(i, j);
      xyz_calculate_change_stats(change_stat_11_given_10, j, i, object, data_list, type_list, z, is_full_neighborhood, functions);
      object.delete_edge(i, j); 
      
      double deg_ij = coef_degrees(i - 1) + coef_degrees(j - 1 + object.n_units);
      double deg_ji = coef_degrees(j - 1) + coef_degrees(i - 1 + object.n_units);
      
      double log_P_00 = 0.0;
      double log_P_10 = arma::dot(coef_nondegrees, change_stat_10) + offset_nonoverlap + deg_ij;
      double log_P_01 = arma::dot(coef_nondegrees, change_stat_01) + offset_nonoverlap + deg_ji;
      double log_P_11 = arma::dot(coef_nondegrees, change_stat_10 + change_stat_11_given_10) + 2.0 * offset_nonoverlap + deg_ij + deg_ji;
      
      double max_log_P = std::max({log_P_00, log_P_10, log_P_01, log_P_11});
      double P_00 = std::exp(log_P_00 - max_log_P);
      double P_10 = std::exp(log_P_10 - max_log_P);
      double P_01 = std::exp(log_P_01 - max_log_P);
      double P_11 = std::exp(log_P_11 - max_log_P);
      double sum_P = P_00 + P_10 + P_01 + P_11;
      
      P_00 /= sum_P;
      P_10 /= sum_P;
      P_01 /= sum_P;
      
      double r = R::unif_rand();
      
      if (r < P_00) {
        // stay (0,0)
      } else if (r < P_00 + P_10) { 
        object.add_edge(i, j);
        global_stats += change_stat_10;
      } else if (r < P_00 + P_10 + P_01) { 
        object.add_edge(j, i);
        global_stats += change_stat_01;
      } else { 
        object.add_edge(i, j);
        object.add_edge(j, i);
        global_stats += change_stat_10 + change_stat_11_given_10;
      }
    }
  }
}

void xyz_simulate_network_mh(const arma::vec coef,
                             XYZ_class &object,
                             const int &n_proposals,
                             const std::vector<arma::mat> &data_list,
                             const std::vector<double> &type_list,
                             const bool &is_full_neighborhood,
                             const std::vector<xyz_ValidateFunction> &functions,
                             arma::vec &global_stats, 
                             const bool tnt = true) {
  if (n_proposals == 0 || object.overlap_mat.n_rows == 0) return;
  
  int proposed_change;
  std::string z = "z";
  arma::mat HR;
  arma::vec change_stat(functions.size());
  
  arma::vec tmp_stat;
  int multiplier = 1;
  int tmp_i, tmp_j, proposal_idx, tmp_switch; 
  
  // // Track strictly overlap dyads
  // int K = object.z_network.directed ? 1 : 2;
  // int N_total_overlap = object.overlap_mat.n_rows / K;
  // int N_1_overlap = 0;
  // 
  // // Initialize N_1_overlap strictly from the overlap matrix
  // for (int idx = 0; idx < object.overlap_mat.n_rows; ++idx) {
  //   if (object.z_network.get_val(object.overlap_mat(idx, 0), object.overlap_mat(idx, 1))) {
  //     N_1_overlap++;
  //   }
  // }
  // if (!object.z_network.directed) N_1_overlap /= 2;
  
  // int accepted_proposals = 0;
  
  for (int a = 0; a < n_proposals; a++) {
    double hr_adj = 0.0;
    int N_0_overlap = object.N_total_overlap - object.N_1_overlap;
    
    if (tnt) {
      double p_drop_forward = (object.N_1_overlap == 0) ? 0.0 : ((N_0_overlap == 0) ? 1.0 : 0.5);
      bool propose_drop = R::unif_rand() < p_drop_forward;
      
      if (propose_drop) {
        int target_edge_idx = (int)(R::unif_rand() * object.active_edges_nb.size());
        auto edge = object.active_edges_nb[target_edge_idx];
        tmp_i = edge.first;
        tmp_j = edge.second;
        
        double p_add_reverse = (object.N_1_overlap - 1 == 0) ? 1.0 : ((N_0_overlap + 1 == 0) ? 0.0 : 0.5);
        hr_adj = std::log(p_add_reverse / p_drop_forward) + std::log((double)object.N_1_overlap / (double)(N_0_overlap + 1));
        
      } else {
        // Rejection sample strictly a non-edge within the overlap
        do {
          proposal_idx = (int)(R::unif_rand() * object.overlap_mat.n_rows);
          tmp_i = object.overlap_mat(proposal_idx, 0);
          tmp_j = object.overlap_mat(proposal_idx, 1);
        } while (object.z_network.get_val(tmp_i, tmp_j));
        
        double p_drop_reverse = (object.N_1_overlap + 1 == 0) ? 0.0 : ((N_0_overlap - 1 == 0) ? 1.0 : 0.5);
        double p_add_forward = 1.0 - p_drop_forward;
        hr_adj = std::log(p_drop_reverse / p_add_forward) + std::log((double)N_0_overlap / (double)(object.N_1_overlap + 1));
      }
    } else {
      proposal_idx = (int)(R::unif_rand() * object.overlap_mat.n_rows);
      tmp_i = object.overlap_mat(proposal_idx, 0);
      tmp_j = object.overlap_mat(proposal_idx, 1);
      hr_adj = 0.0;
    }
    
    if (!object.z_network.directed) {
      if (tmp_i > tmp_j) {
        tmp_switch = tmp_j; 
        tmp_j = tmp_i; 
        tmp_i = tmp_switch;
      }
    }
    
    if (object.z_network.get_val(tmp_i, tmp_j)) {
      proposed_change = 0;
      multiplier = -1;
    } else {
      proposed_change = 1;
      multiplier = 1;
    }
    
    xyz_calculate_change_stats(change_stat, tmp_i, tmp_j, object, data_list, type_list, 
                               z, is_full_neighborhood, functions);
    
    tmp_stat = change_stat * multiplier;
    
    // Non-overlap offset removed; mathematically impossible to propose outside overlap
    double HR_val = std::exp(arma::dot(coef, tmp_stat) + hr_adj);
    
    if (R::unif_rand() < HR_val) {
      // accepted_proposals++;
      global_stats += tmp_stat;
      if (proposed_change == 0) {
        object.delete_edge(tmp_i, tmp_j);
      } 
      if (proposed_change == 1) {
        object.add_edge(tmp_i, tmp_j);
      }
    }
  }
  
  // Rcpp::Rcout << "MH Sampler Acceptance Rate: " << (double)accepted_proposals / n_proposals << "\n";
}

void xyz_simulate_network_mh_degrees(const arma::vec coef_nondegrees,
                                     const arma::vec coef_degrees,
                                     XYZ_class &object,
                                     const int &n_proposals,
                                     const std::vector<arma::mat> &data_list,
                                     const std::vector<double> &type_list,
                                     const bool &is_full_neighborhood,
                                     const std::vector<xyz_ValidateFunction> &functions,
                                     arma::vec &global_stats, 
                                     const bool tnt = true) {
  if (n_proposals == 0 || object.overlap_mat.n_rows == 0) return;
  
  int proposed_change;
  std::string z = "z";
  arma::mat HR;
  arma::vec change_stat(functions.size());
  
  arma::vec tmp_stat;
  int multiplier = 1;
  int tmp_i, tmp_j, proposal_idx, tmp_switch; 
  
  // int K = object.z_network.directed ? 1 : 2;
  // int N_total_overlap = object.overlap_mat.n_rows / K;
  // int N_1_overlap = 0;
  // 
  // for (int idx = 0; idx < object.overlap_mat.n_rows; ++idx) {
  //   if (object.z_network.get_val(object.overlap_mat(idx, 0), object.overlap_mat(idx, 1))) {
  //     N_1_overlap++;
  //   }
  // }
  // if (!object.z_network.directed) N_1_overlap /= 2;
  
  // int accepted_proposals = 0;
  
  for (int a = 0; a < n_proposals; a++) {
    double hr_adj = 0.0;
    int N_0_overlap = object.N_total_overlap - object.N_1_overlap;
    
    if (tnt) {
      double p_drop_forward = (object.N_1_overlap == 0) ? 0.0 : ((N_0_overlap == 0) ? 1.0 : 0.5);
      bool propose_drop = R::unif_rand() < p_drop_forward;
      
      if (propose_drop) {
        int target_edge_idx = (int)(R::unif_rand() * object.active_edges_nb.size());
        auto edge = object.active_edges_nb[target_edge_idx];
        tmp_i = edge.first;
        tmp_j = edge.second;
        
        double p_add_reverse = (object.N_1_overlap - 1 == 0) ? 1.0 : ((N_0_overlap + 1 == 0) ? 0.0 : 0.5);
        hr_adj = std::log(p_add_reverse / p_drop_forward) + std::log((double)object.N_1_overlap / (double)(N_0_overlap + 1));
        
      } else {
        do {
          proposal_idx = (int)(R::unif_rand() * object.overlap_mat.n_rows);
          tmp_i = object.overlap_mat(proposal_idx, 0);
          tmp_j = object.overlap_mat(proposal_idx, 1);
        } while (object.z_network.get_val(tmp_i, tmp_j));
        
        double p_drop_reverse = (object.N_1_overlap + 1 == 0) ? 0.0 : ((N_0_overlap - 1 == 0) ? 1.0 : 0.5);
        double p_add_forward = 1.0 - p_drop_forward;
        hr_adj = std::log(p_drop_reverse / p_add_forward) + std::log((double)N_0_overlap / (double)(object.N_1_overlap + 1));
      }
    } else {
      proposal_idx = (int)(R::unif_rand() * object.overlap_mat.n_rows);
      tmp_i = object.overlap_mat(proposal_idx, 0);
      tmp_j = object.overlap_mat(proposal_idx, 1);
      hr_adj = 0.0;
    } 
    
    if (!object.z_network.directed) {
      if (tmp_i > tmp_j) {
        tmp_switch = tmp_j; 
        tmp_j = tmp_i; 
        tmp_i = tmp_switch;
      }
    }
    
    if (object.z_network.get_val(tmp_i, tmp_j)) {
      proposed_change = 0;
      multiplier = -1;
    }  else { 
      proposed_change = 1;
      multiplier = 1;
    } 
    
    xyz_calculate_change_stats(change_stat, tmp_i, tmp_j, object, data_list, type_list, 
                               z, is_full_neighborhood, functions);
    
    tmp_stat = change_stat * multiplier;
    
    if (object.z_network.directed) {
      double HR_val = std::exp(arma::dot(coef_nondegrees, tmp_stat) + hr_adj + 
        multiplier * (coef_degrees(tmp_i - 1) + coef_degrees(tmp_j - 1 + object.n_units)));
      if (R::unif_rand() < HR_val) {
        global_stats += tmp_stat;
        if (proposed_change == 0) object.delete_edge(tmp_i, tmp_j);
        if (proposed_change == 1) object.add_edge(tmp_i, tmp_j);
      }
    } else {
      double HR_val = std::exp(arma::dot(coef_nondegrees, tmp_stat) + hr_adj + 
        multiplier * (coef_degrees(tmp_i - 1) + coef_degrees(tmp_j - 1)));  
      if (R::unif_rand() < HR_val) {
        global_stats += tmp_stat;
        if (proposed_change == 0) object.delete_edge(tmp_i, tmp_j);
        if (proposed_change == 1) object.add_edge(tmp_i, tmp_j);
      }
    }
  }
  
  // Rcpp::Rcout << "MH Degrees Sampler Acceptance Rate: " << (double)accepted_proposals / n_proposals << "\n";
}

void xyz_simulate_attribute_mh( const arma::vec coef,
                                XYZ_class &object,
                                const int &n_proposals,
                                const  std::vector<arma::mat> &data_list,
                                const std::vector<double> &type_list,
                                const bool &is_full_neighborhood,
                                const std::vector<xyz_ValidateFunction> &functions,
                                arma::vec &global_stats, 
                                const std::string type) {
  if(n_proposals == 0){
    return;
  }
  int proposed_change;
  arma::vec change_stat(functions.size());
  arma::vec tmp_stat(functions.size());
  int multiplier;
  int tmp_i; 
  const double MAX_LOG_RATE = 100.0;
  arma::vec tmp;
  arma::vec tmp_row;
  // Go through a loop for each proposed change
  for(int a = 0; a <=(n_proposals-1); a ++ ) {
    // Here we pick the random entry
    tmp_i = (int)(R::unif_rand() * object.n_units) + 1;
    // Here we calculate the change stat from turning y_i from 0 to 1
    xyz_calculate_change_stats(change_stat, tmp_i,
                               tmp_i,
                               object,
                               data_list,
                               type_list,
                               type,
                               is_full_neighborhood,
                               functions);
    if(type == "x"){
      if(object.x_attribute.family == "binomial"){
        if(object.x_attribute.get_val(tmp_i)){
          proposed_change = 0;
          multiplier = -1;
        } else {
          proposed_change = 1;
          multiplier = 1;
        }  
        // 3. Step: Calculate the Hastings Ratios
        tmp_stat=change_stat*multiplier;
        double HR_val = std::exp(arma::dot(coef, tmp_stat));
        
        // 4. Step: Sample a random number between 0 and 1, accept if it is > HR
        if(R::unif_rand() < HR_val){
          global_stats += (multiplier * 1.0) * change_stat;
          // Here we modify the network
          if(proposed_change == 0){
            object.x_attribute.set_attr_0(tmp_i);  
          } else {
            object.x_attribute.set_attr_1(tmp_i);
          }
        }
      }
      if(object.x_attribute.family == "poisson"){
        double safe_eta = std::min(arma::dot(coef, change_stat), MAX_LOG_RATE);
        double tmp_val = R::rpois(exp(safe_eta)); 
        global_stats += (tmp_val - object.x_attribute.get_val_no_scale(tmp_i)) * change_stat;
        object.x_attribute.set_attr_value(tmp_i, tmp_val);  
      }
      if(object.x_attribute.family == "normal"){
        double HR_val = arma::dot(coef, change_stat);
        double tmp_val = R::rnorm(HR_val, sqrt(object.x_attribute.scale)); 
        global_stats += (tmp_val- object.x_attribute.get_val_no_scale(tmp_i))/object.x_attribute.scale * change_stat;
        object.x_attribute.set_attr_value(tmp_i, tmp_val);  
      }
    }
    if(type == "y"){
      if(object.y_attribute.family == "binomial"){
        if(object.y_attribute.get_val(tmp_i)){
          proposed_change = 0;
          multiplier = -1;
        } else {
          proposed_change = 1;
          multiplier = 1;
        }
        // 3. Step: Calculate the Hastings Ratios
        tmp_stat=change_stat*multiplier;
        double HR_val = std::exp(arma::dot(coef, tmp_stat));
        // 4. Step: Sample a random number between 0 and 1, accept if it is > HR
        if(R::unif_rand() < HR_val){
          global_stats += (multiplier * 1.0 / object.y_attribute.scale) * change_stat;
          // Here we modify the network
          if(proposed_change == 0){
            object.y_attribute.set_attr_0(tmp_i);  
          } else {
            object.y_attribute.set_attr_1(tmp_i);
          }
        }
      }
      if(object.y_attribute.family == "poisson"){
        double safe_eta = std::min(arma::dot(coef, change_stat), MAX_LOG_RATE);
        double tmp_val = R::rpois(exp(safe_eta)); 
        global_stats +=  (tmp_val - object.y_attribute.get_val_no_scale(tmp_i)) * change_stat;
        object.y_attribute.set_attr_value(tmp_i, tmp_val);  
      }
      if(object.y_attribute.family == "normal"){
        double HR_val = arma::dot(coef, change_stat);
        double tmp_val = R::rnorm(HR_val, sqrt(object.y_attribute.scale)); 
        global_stats += (tmp_val - object.y_attribute.get_val_no_scale(tmp_i))/object.y_attribute.scale * change_stat;
        object.y_attribute.set_attr_value(tmp_i, tmp_val);  
      }
      
    }
    
    
    
    
  }
}

arma::mat xyz_simulate_internal(XYZ_class & object,
                                const arma::vec& coef,
                                const  arma::vec& coef_degrees,
                                const std::vector<arma::mat>& data_list,
                                const std::vector<double>& type_list,
                                arma::vec & global_stats,
                                const int n_proposals_x,
                                const int n_proposals_y,
                                const  int n_proposals_z,
                                const int seed,
                                const int n_burn_in,
                                const  int n_simulation,
                                std::vector<arma::vec>& res_x,
                                std::vector<arma::vec>& res_y,
                                std::vector<std::vector<std::vector<int>>>& res_z,
                                const bool only_stats,
                                const bool is_full_neighborhood,
                                const std::vector<xyz_ValidateFunction> functions,
                                const bool display_progress, 
                                const bool degrees, 
                                const double offset_nonoverlap, 
                                 const bool fix_x = false, 
                                 const bool fix_z = false, 
                                 const bool nonoverlap_random = true,
                                 const bool tnt = true){
  arma::mat stats(n_simulation,functions.size());
  stats.fill(0);
  std::string x, y; 
  x = "x";
  y = "y";
  if (seed != NA_INTEGER) {
    Rcpp::Function set_seed_r("set.seed");
    set_seed_r(seed);
  }
  Progress p(n_simulation + n_burn_in, display_progress);
  // Start for a burn in period with the normal number of proposals
  // Intialize global statistics and then adapt them peu a peu
  for(int i = 1; i <=(n_simulation + n_burn_in);i ++) {
    // Simulate the network
    // Rcout << global_stats << std::endl;
    // Rcout << "Updated Global Statistics: " << global_stats.t() << std::endl;
    p.increment(); // update progress
    if(!fix_x){
      // Rcout << "Sampling X| Y,Z" << std::endl;
      // Sample X| Y,Z
      xyz_simulate_attribute_mh(coef,object,
                                n_proposals_x,
                                data_list, 
                                type_list,
                                is_full_neighborhood, 
                                functions,
                                global_stats, x);  
    }
    // Sample Y| X,Z
    xyz_simulate_attribute_mh(coef,object,
                              n_proposals_y,
                              data_list, type_list,
                              is_full_neighborhood, functions,
                              global_stats, y);
    
    if(!fix_z){
      // Sample Z_overlapping|X,Y
      if(degrees){
        xyz_simulate_network_mh_degrees(coef,
                                        coef_degrees,
                                        object,
                                        n_proposals_z,
                                        data_list, type_list,
                                        is_full_neighborhood, functions,
                                        global_stats, tnt); 
      } else {
        xyz_simulate_network_mh(coef,object,
                                n_proposals_z,
                                data_list, type_list,
                                is_full_neighborhood, functions,
                                global_stats, tnt);  
      }
      if(nonoverlap_random){
        // Sample Z_nonoverlapping|X,Y
        if(degrees){
          if(object.z_network.directed) {
            xyz_simulate_network_consecutive_degrees_mh_directed(coef,
                                                        coef_degrees,object,
                                                        data_list, type_list,
                                                        is_full_neighborhood, functions,
                                                        global_stats, offset_nonoverlap);
          } else {
            xyz_simulate_network_consecutive_degrees_mh(coef,
                                                        coef_degrees,object,
                                                        data_list, type_list,
                                                        is_full_neighborhood, functions,
                                                        global_stats, offset_nonoverlap);
          }
        } else {
          if(object.z_network.directed) {
            xyz_simulate_network_consecutive_mh_directed(coef,object,
                                                data_list, type_list,
                                                is_full_neighborhood, functions,
                                                global_stats, offset_nonoverlap);
          } else {
            xyz_simulate_network_consecutive_mh(coef,object,
                                                data_list, type_list,
                                                is_full_neighborhood, functions,
                                                global_stats, offset_nonoverlap);
          }
        }
      }
    }
    // We throw the first n_burn_in samples away
    if(i>n_burn_in){
      if(only_stats){
        // Count global statistics
        stats.row(i - n_burn_in-1) = global_stats.as_row();
      } else{
        // Save the object
        // if(!fix_x){
        //   res_x.at(i - n_burn_in-1) =object.x_attribute.attribute; 
        // }
        res_x.at(i - n_burn_in-1) =object.x_attribute.attribute; 
        res_y.at(i - n_burn_in-1) =object.y_attribute.attribute;
        res_z.at(i - n_burn_in-1) =object.z_network.adj_list;
        // Count global statistics
        stats.row(i - n_burn_in-1) = global_stats.as_row();
      }
    }
    
  }
  return(stats);
}


// [[Rcpp::export]]
List xyz_simulate_cpp(arma::vec& coef,
                      arma::vec& coef_degrees,
                      std::vector<std::string>& terms,
                      int& n_units,
                      arma::mat z_network,
                      arma::mat neighborhood,
                      arma::mat overlap,
                      arma::vec x_attribute,
                      arma::vec y_attribute,
                      bool init_empty,
                      bool directed,
                      bool degrees,
                      std::vector<arma::mat>& data_list,
                      std::vector<double>& type_list,
                      double offset_nonoverlap,
                      std::string family_x, 
                      std::string family_y, 
                      double attr_x_scale, 
                      double attr_y_scale,
                      bool nonoverlap_random = false, 
                      int n_proposals_x = 100,
                      int n_proposals_y = 100,
                      int n_proposals_z = 100,
                      int seed = 123,
                      int n_burn_in = 100,
                      int n_simulation = 1,
                      bool only_stats = false,
                      bool display_progress = false, 
                      bool fix_x = false, 
                      bool fix_z = false,
                      bool tnt = true){
  // res(n_simulation);
  // stats2.fill(0);
  XYZ_class object(n_units,directed, neighborhood, overlap, family_x, family_y,attr_x_scale, attr_y_scale);
  if(!init_empty){
    object.set_info_arma(x_attribute,y_attribute, z_network);
  } else {
    if(fix_x){
      object.x_attribute.set_attr_from_armavec(x_attribute);
    }
    if(fix_z){
      object.set_network_from_mat(n_units, directed, z_network);
    } else if(!nonoverlap_random){
      for(size_t r = 0; r < z_network.n_rows; ++r){
        int u = static_cast<int>(z_network(r, 0));
        int v = static_cast<int>(z_network(r, 1));
        if(!object.get_val_overlap(u, v)){
          object.add_edge(u, v);
        }
      }
    }
  }
  bool is_full_neighborhood = object.check_if_full_neighborhood();
  // Rcout <<object.overlap.at(1)<< std::endl;
  
  // object.set_neighborhood_from_mat(neighborhood);
  // Generate change statistic function from the terms
  std::vector<xyz_ValidateFunction> functions;
  
  functions = xyz_change_statistics_generate_new(terms);
  // arma::vec at_zero;
  // at_zero = xyz_eval_at_empty_network_new(terms);
  // // Start for a burn in period with the normal number of proposals
  // // Intialize global statistics and then adapt them peu a peu
  // arma::vec global_stats(functions.size());
  // global_stats.fill(0);
  // global_stats = global_stats + at_zero;
  // Rcout << "Here"<< std::endl;
  arma::vec global_stats = xyz_count_global_internal( object,
                                                      terms,
                                                      n_units,
                                                      data_list,
                                                      type_list,
                                                      family_x, family_y, 
                                                      attr_x_scale, 
                                                      attr_y_scale);
  // Rcout << global_stats << std::endl;
  std::vector<arma::vec> res_x(n_simulation);
  std::vector<arma::vec> res_y(n_simulation);
  std::vector<std::vector<std::vector<int>>> res_z(n_simulation);
  // Rcout << "B"<< std::endl;
  arma::mat stats = xyz_simulate_internal(object, coef,coef_degrees, data_list, type_list, global_stats,
                                          n_proposals_x,
                                          n_proposals_y,
                                          n_proposals_z, seed,
                                          n_burn_in, n_simulation,
                                          res_x,res_y,res_z,
                                          only_stats, 
                                          is_full_neighborhood, 
                                          functions, 
                                          display_progress, 
                                          degrees,
                                          offset_nonoverlap, 
                                          fix_x, 
                                          fix_z, 
                                          nonoverlap_random,
                                          tnt);
  if(only_stats){
    return(List::create(_["stats"] = stats));
  } else {
    return(List::create(_["simulation_attributes_x"] =res_x,_["simulation_attributes_y"] =res_y,
                        _["simulation_networks_z"] =res_z, _["stats"] = stats));  
  }
}
std::tuple<arma::mat, arma::vec> xyz_get_info_pl(const XYZ_class& object,
                                                 std::vector<std::string> terms,
                                                 std::vector<arma::mat> &data_list,
                                                 std::vector<double> &type_list, 
                                                 bool display_progress, 
                                                 arma::uvec &i_vec,
                                                 arma::uvec &j_vec, 
                                                 arma::uvec &overlap_vec, 
                                                 int n_units, 
                                                 bool fix_x, 
                                                 bool fix_z) {
  bool is_full_neighborhood = object.check_if_full_neighborhood();
  // Generate vector of functions that calculate the sufficient statistics 
  std::vector<xyz_ValidateFunction> functions;
  functions = xyz_change_statistics_generate_new(terms);
  // strings z, x, and y later needed to tell the sufficient statistics 
  // what type of change statistic is wanted
  std::string z = "z", x = "x", y = "y";
  // Just a temporary vector of the change statistics for one dyad of the network
  arma::vec change_stat_network(functions.size()),
  // The same thing but for the attributes i and j
  change_stat_attribute_i(functions.size()), change_stat_attribute_j(functions.size());
  double x_i, y_i, z_ij;
  // int ncores = 5;
  arma::mat res_covs((!fix_z) * (n_units* (n_units - 1) * (object.z_network.directed + 1) / 2) + 
    n_units * (!fix_x + 1), terms.size());
  arma::vec res_target((!fix_z) * (n_units* (n_units - 1) * (object.z_network.directed + 1) / 2) + 
    n_units * (!fix_x + 1));
  
  Progress p(res_target.size(), display_progress);
  int now = 0;
  if(!fix_z){
    if(object.z_network.directed){
      for(int i: seq(1,n_units)){
        Rcpp::checkUserInterrupt();
        
        for(int j: seq(1,n_units)){
          // Get present values of x_ij, y_i, y_j
          p.increment(); // update progress
          if(i == j){
            continue;
          }
          z_ij = object.z_network.get_val(i,j);
          xyz_calculate_change_stats(change_stat_network, i,
                                     j,
                                     object,
                                     data_list,
                                     type_list,
                                     z,
                                     is_full_neighborhood,
                                     functions);
          // change_stat_network(change_stat_network.size()+1) =x_ij;
          res_covs.row(now)= (change_stat_network).as_row();
          res_target.at(now) = z_ij;
          i_vec.at(now) = i;
          j_vec.at(now) = j;
          overlap_vec.at(now) = object.get_val_overlap(i, j);
          now += 1;
        } 
      }
    } else {
      for(int i: seq(1,n_units-1)){
        for(int j: seq(i+1,n_units)){
          // Get present values of x_ij, y_i, y_j
          p.increment(); // update progress
          
          z_ij = object.z_network.get_val(i,j);
          xyz_calculate_change_stats(change_stat_network, i,
                                     j,
                                     object,
                                     data_list,
                                     type_list,
                                     z,
                                     is_full_neighborhood,
                                     functions);
          // change_stat_network(change_stat_network.size()+1) =x_ij;
          res_covs.row(now)= (change_stat_network).as_row();
          res_target.at(now) = z_ij;
          i_vec.at(now) = i;
          j_vec.at(now) = j;
          overlap_vec.at(now) = object.get_val_overlap(i, j);
          now += 1;
        } 
      }
    }
  } 
  for(int i: seq(1,n_units)){
    if(!fix_x){
      p.increment(); 
      x_i = object.x_attribute.get_val_no_scale(i);
      xyz_calculate_change_stats(change_stat_attribute_i, i,
                                 i,
                                 object,
                                 data_list,
                                 type_list,
                                 x,
                                 is_full_neighborhood,
                                 functions);
      res_covs.row(now)= (change_stat_attribute_i).as_row();
      res_target.at(now) = x_i;
      now += 1;  
    }
    p.increment(); 
    y_i = object.y_attribute.get_val_no_scale(i);
    xyz_calculate_change_stats(change_stat_attribute_i, i,
                               i,
                               object,
                               data_list,
                               type_list,
                               y,
                               is_full_neighborhood,
                               functions);
    res_covs.row(now)= (change_stat_attribute_i).as_row();
    res_target.at(now) = y_i;
    now += 1;
  } 
  
  return(std::tuple<arma::mat, arma::vec> {res_covs, res_target});
} 

// Version where the argument of a XYZ_class object 
// and not the separate attributes and network as in the function xyz_prepare_composite_estimation_internal
// std::vector<arma::mat> xyz_get_info(XYZ_class object,
//                                     std::vector<std::string> terms,
//                                     std::vector<arma::mat> &data_list,
//                                     std::vector<double> &type_list, 
//                                     bool add_info, 
//                                     bool display_progress, 
//                                     arma::vec overlap_vec) {
//   // Set up objects
//   int n_units = object.n_units;
//   std::vector<xyz_ValidateFunction> functions;
//   functions = xyz_change_statistics_generate(terms);
//   std::string z = "z", x = "x", y = "y";
//   // arma::vec change_stat_x_i(functions.size()),
//   // change_stat_x_j(functions.size()), change_stat_y_i(functions.size()),
//   // change_stat_y_j(functions.size()), change_stat_z_ij(functions.size());
//   bool is_full_neighborhood = object.check_if_full_neighborhood();
//   
//   bool x_i, x_j, z_ij, y_i,y_j;
//   int now = -1;
//   
//   if(object.z_network.directed){
//     std::vector<arma::mat> res(n_units*(n_units-1));
//     Progress p_3(n_units*(n_units-1), display_progress);
//     
//     
//     for(int i: seq(1,n_units)){
//       for(int j: seq(1,n_units)){
//         if(i == j){
//           continue;
//         } else {
//           now += 1;
//           p_3.increment();
//         } 
//         // Get present values of x_ij, y_i, y_j
//         x_i = object.x_attribute.get_val(i);
//         x_j = object.x_attribute.get_val(j);
//         y_i = object.y_attribute.get_val(i);
//         y_j = object.y_attribute.get_val(j);
//         z_ij = object.z_network.get_val(i,j);
//         overlap_vec.at(now) = object.get_val_overlap(i, j);
//         if(add_info) {
//           res.at(now) = get_all_combinations_xyz(i,j,x_i, x_j,y_i,y_j,z_ij,now, 
//                  object,functions, data_list, type_list, is_full_neighborhood);
//         } else { 
//           res.at(now) = get_all_combinations_xyz(i,j,x_i, x_j,y_i,y_j,z_ij,now, 
//                  object,functions, data_list, type_list, is_full_neighborhood).submat(0,8,31,7+functions.size());
//         } 
//       } 
//     }
//     
//     return(res);
//     
//   } else {
//     std::vector<arma::mat> res(n_units*(n_units-1)/2);
//     Progress p_3(n_units*(n_units-1)/2, display_progress);
//     
//     for(int i: seq(1,n_units-1)){
//       for(int j: seq(i+1,n_units)){
//         
//         // 
//         // Get present values of x_ij, y_i, y_j
//         x_i = object.x_attribute.get_val(i);
//         x_j = object.x_attribute.get_val(j);
//         y_i = object.y_attribute.get_val(i);
//         y_j = object.y_attribute.get_val(j);
//         z_ij = object.z_network.get_val(i,j);
//         overlap_vec.at(now) = object.get_val_overlap(i, j);
//         
//         now += 1;
//         p_3.increment();
//         if(add_info) {
//           res.at(now) = get_all_combinations_xyz(i,j,x_i, x_j,y_i,y_j,z_ij,now, 
//                  object,functions, data_list, type_list, is_full_neighborhood);
//           
//         } else { 
//           res.at(now) = get_all_combinations_xyz(i,j,x_i, x_j,y_i,y_j,z_ij,now, 
//                  object,functions, data_list, type_list, is_full_neighborhood).submat(0,8,31,7+functions.size());
//         } 
//         
//       } 
//     }
//     p_3.cleanup();
//     return(res);
//   }
//   
// }

arma::mat get_A_exact(arma::uvec i_vec, 
                      arma::uvec  j_vec,
                      arma::uvec  overlap_vec,
                      std::tuple<arma::mat,arma::vec> &pseudo_lh, 
                      arma::vec &coef_degrees, 
                      arma::vec &coef_nondegrees, 
                      double &offset_nonoverlap, 
                      bool directed, 
                      int n_units){
  arma::vec res;
  arma::vec exp_tmp;
  arma::mat A(coef_degrees.size(),coef_degrees.size());
  unsigned int number_elements_network = i_vec.size();
  // For the calculation of B we only have to regard network information (relating to the first entries)
  
  for(unsigned int i = 0; i < number_elements_network; i++){
    if(overlap_vec.at(i)){
      if(directed) {
        exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1+ n_units));  
      } else {
        exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1));  
      }
    } else {
      if(directed) {
        exp_tmp = arma::exp(offset_nonoverlap + std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1 + n_units));
      } else {
        exp_tmp = arma::exp(offset_nonoverlap + std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1));
      }
    }
    double e_eta = exp_tmp.at(0);
    double p_val = (std::isinf(e_eta)) ? 1.0 : (e_eta / (1.0 + e_eta));
    double v_val = p_val * (1.0 - p_val);
    res = arma::vec({v_val});
    
    A.at(i_vec.at(i)-1, i_vec.at(i)-1)+= res.at(0);
    if(directed) {
      A.at(j_vec.at(i)-1 + n_units, j_vec.at(i)-1 + n_units)+= res.at(0);
      A.at(i_vec.at(i)-1, j_vec.at(i)-1 + n_units)+= res.at(0);
      A.at(j_vec.at(i)-1 + n_units, i_vec.at(i)-1)+= res.at(0);
    } else {
      A.at(j_vec.at(i)-1, j_vec.at(i)-1)+= res.at(0);
      A.at(i_vec.at(i)-1, j_vec.at(i)-1)+= res.at(0);
      A.at(j_vec.at(i)-1, i_vec.at(i)-1)+= res.at(0);
    }
  }
  
  
  return(A);
}

std::tuple< arma::vec, arma::mat> get_B_pl(arma::uvec i_vec, 
                                           arma::uvec  j_vec,
                                           arma::uvec  overlap_vec,
                                           std::tuple<arma::mat,arma::vec> &pseudo_lh, 
                                           arma::vec &coef_degrees, 
                                           arma::vec &coef_nondegrees, 
                                           double &offset_nonoverlap, 
                                           bool directed, 
                                           int n_units) {
  arma::mat B_mat(coef_nondegrees.size(),coef_degrees.size());
  arma::vec A_diag(coef_degrees.size()), res;
  B_mat.fill(0);
  arma::vec exp_tmp, cov_degrees(coef_degrees.size());
  cov_degrees.fill(0);
  unsigned int number_elements_network = i_vec.size();
  // For the calculation of B we only have to regard network information (relating to the first entries)
  
  for(unsigned int i = 0; i < number_elements_network; i++){
    if(overlap_vec.at(i)){
      if(directed) {
        exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1+ n_units));  
        cov_degrees.at(j_vec.at(i)-1+ n_units) = 1;
      } else {
        exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1));  
        cov_degrees.at(j_vec.at(i)-1) = 1;
      }
    } else {
      if(directed) {
        exp_tmp = arma::exp(offset_nonoverlap + std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1 + n_units));
        cov_degrees.at(j_vec.at(i)-1+ n_units) = 1;
      } else {
        exp_tmp = arma::exp(offset_nonoverlap + std::get<0>(pseudo_lh).row(i)*coef_nondegrees +
          coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1));
        cov_degrees.at(j_vec.at(i)-1) = 1;
      }
    }
    cov_degrees.at(i_vec.at(i)-1) = 1;
    B_mat += exp_tmp.at(0)/(1+exp_tmp.at(0))*1/(1+exp_tmp.at(0))*
      std::get<0>(pseudo_lh).row(i).t()*cov_degrees.t();
    
    res = exp_tmp.at(0)/(1+exp_tmp.at(0)) - pow(exp_tmp.at(0)/(1+exp_tmp.at(0)),2);
    A_diag.at(i_vec.at(i)-1) += res.at(0);
    cov_degrees.at(i_vec.at(i)-1) = 0;
    if(directed) { 
      cov_degrees.at(j_vec.at(i)-1 + n_units) = 0;
      A_diag.at(j_vec.at(i)-1 + n_units) += res.at(0);
      
    } else {
      cov_degrees.at(j_vec.at(i)-1) = 0;
      A_diag.at(j_vec.at(i)-1) += res.at(0);
      
    }
    
  }
  return(std::tuple<arma::vec, arma::mat> {A_diag,B_mat});
}


arma::mat get_B(arma::uvec i_vec, arma::uvec  j_vec,
                arma::uvec  overlap_vec,
                std::tuple<arma::mat,arma::vec> &pseudo_lh, 
                arma::vec &coef_degrees,
                arma::vec &coef_nondegrees, 
                double &offset_nonoverlap, 
                bool directed, 
                int n_units) {
  arma::mat B_mat(coef_nondegrees.size(),coef_degrees.size());
  B_mat.fill(0);
  // arma::vec exp_tmp, cov_degrees(coef_degrees.size());
  // cov_degrees.fill(0);
  unsigned int number_elements_network = i_vec.size();
  // For the calculation of B we only have to regard network information (relating to the first entries)
  const double max_eta = 20.0;
  
  const arma::mat& X_net = std::get<0>(pseudo_lh);
  
  arma::vec eta = arma::exp(offset_nonoverlap*(1-overlap_vec)+std::get<0>(pseudo_lh).rows(0,number_elements_network-1)*coef_nondegrees +
    coef_degrees.elem(i_vec-1) + coef_degrees.elem(j_vec-1+ n_units*directed));
  
  for (unsigned int i = 0; i < number_elements_network; i++) {
    double current_eta = eta.at(i);
    double weight;
    
    // --- Numerical Stability Logic ---
    // weight = exp(eta) / (1 + exp(eta))^2  => which is p * (1-p)
    if (std::abs(current_eta) > max_eta) {
      // At extreme eta, p*(1-p) is effectively 0 in double precision.
      // This prevents inf/inf = NaN errors.
      weight = 0.0;
    } else {
      double exp_eta = std::exp(current_eta);
      double one_plus_exp = 1.0 + exp_eta;
      // Stable calculation of p * (1 - p)
      weight = exp_eta / (one_plus_exp * one_plus_exp);
    }
    
    // Only update if weight is meaningful and finite
    if (weight > 1e-18) {
      arma::uword i_idx = i_vec[i] - 1;
      arma::uword j_idx = j_vec[i] - 1 + (directed ? n_units : 0);
      
      // Apply weight to the transpose of the change statistics row
      const arma::rowvec& x_row = X_net.row(i);
      
      // B_mat is (p x 2n), we update columns corresponding to units i and j
      B_mat.col(i_idx) += weight * x_row.t();
      B_mat.col(j_idx) += weight * x_row.t();
    }
  }
  
  // for(unsigned int i = 0; i < number_elements_network; i++){
  //   arma::uword i_idx = i_vec[i] - 1;
  //   arma::uword j_idx = j_vec[i] - 1 + (directed ? n_units : 0);
  //   double weight = eta.at(i) / std::pow(1.0 + eta.at(i), 2);
  //   const arma::rowvec& x_row = std::get<0>(pseudo_lh).row(i);
  //   
  //   B_mat.col(i_idx) += weight * x_row.t();
  //   B_mat.col(j_idx) += weight * x_row.t();
  //   
  // } 
  
  return(B_mat);
}



std::tuple<arma::vec, arma::vec, arma::mat, arma::mat> cond_estimation_nondegrees_pl_old(arma::vec coef,
                                                                                         arma::uvec &i_vec,
                                                                                         arma::uvec  &j_vec,
                                                                                         arma::uvec  &overlap_vec,
                                                                                         bool directed,
                                                                                         std::tuple<arma::mat,arma::vec> &pseudo_lh,
                                                                                         int max_iteration,
                                                                                         double tol,
                                                                                         arma::vec &coef_degrees,
                                                                                         double offset_nonoverlap,
                                                                                         bool &non_stop, 
                                                                                         std::string attr_x_family, 
                                                                                         std::string attr_y_family, 
                                                                                         double x_scale, 
                                                                                         double y_scale, 
                                                                                         bool fix_x) {
  int n_coef = coef.size();
  List score_ind(std::get<0>(pseudo_lh).n_rows);
  List fisher_ind(std::get<0>(pseudo_lh).n_rows);
  
  arma::vec score(n_coef), score_tmp(n_coef), m(n_coef), exp_tmp(n_coef);
  arma::mat fisher(n_coef, n_coef),x_trans(n_coef, n_coef), coefs(max_iteration+1, n_coef);
  coefs.row(0)= coef.t();
  score.fill(0);
  fisher.fill(0);
  bool non_converged = true;
  int k = 1;
  unsigned int n_units = coef_degrees.n_elem/2;
  unsigned int number_elements_network = i_vec.size();
  while(non_converged) {
    // Update the score and fisher info
    for(unsigned int i = 0; i < std::get<0>(pseudo_lh).n_rows; i++){
      // What to do if we are regarding network information (relating to the first entries)
      if(i < number_elements_network){
        if(directed){
          if(overlap_vec.at(i)){
            exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef +
              coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1 + n_units));
          } else {
            exp_tmp = arma::exp(offset_nonoverlap+std::get<0>(pseudo_lh).row(i)*coef +
              coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1+ n_units));
          }
        } else {
          if(overlap_vec.at(i)){
            exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef +
              coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1));
          } else {
            exp_tmp = arma::exp(offset_nonoverlap+std::get<0>(pseudo_lh).row(i)*coef +
              coef_degrees(i_vec.at(i)-1) + coef_degrees(j_vec.at(i)-1));
          }  
        }
        double e_eta = exp_tmp.at(0);
        double p_val = (std::isinf(e_eta)) ? 1.0 : (e_eta / (1.0 + e_eta));
        double v_val = p_val * (1.0 - p_val);
        score_tmp = std::get<0>(pseudo_lh).row(i).t() * (std::get<1>(pseudo_lh).at(i) - p_val);
        score += score_tmp;
        fisher += v_val * std::get<0>(pseudo_lh).row(i).t() * std::get<0>(pseudo_lh).row(i);
        
      } else if((i >= number_elements_network) && (i < number_elements_network +n_units) && (fix_x == false)){
        // 
        // What to do if we are regarding attribute information (relating to the last entries)
        if(attr_x_family == "binomial"){
          exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef);
          score_tmp = std::get<0>(pseudo_lh).row(i).t()*
            std::get<1>(pseudo_lh).at(i) -
            exp_tmp.at(0)/(1+exp_tmp.at(0))*std::get<0>(pseudo_lh).row(i).t();
          score += score_tmp;
          fisher += exp_tmp.at(0)/(pow(1+exp_tmp.at(0), 2))*
            std::get<0>(pseudo_lh).row(i).t()*std::get<0>(pseudo_lh).row(i);
          
        }
        if(attr_x_family == "poisson"){
          exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef);
          score += (std::get<1>(pseudo_lh).at(i) - exp_tmp.at(0))*std::get<0>(pseudo_lh).row(i).t();
          fisher += exp_tmp.at(0)*
            std::get<0>(pseudo_lh).row(i).t()*std::get<0>(pseudo_lh).row(i);
        } else if(attr_x_family == "normal"){
          exp_tmp = std::get<0>(pseudo_lh).row(i)*coef;
          score += (std::get<1>(pseudo_lh).at(i) - exp_tmp.at(0))*std::get<0>(pseudo_lh).row(i).t()/x_scale;
          fisher += std::get<0>(pseudo_lh).row(i).t()*std::get<0>(pseudo_lh).row(i)/x_scale;
        }
        
        
      } else {
        
        if(attr_y_family == "binomial"){
          exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef);
          score_tmp = std::get<0>(pseudo_lh).row(i).t()*
            std::get<1>(pseudo_lh).at(i) -
            exp_tmp.at(0)/(1+exp_tmp.at(0))*std::get<0>(pseudo_lh).row(i).t();
          score += score_tmp;
          fisher += exp_tmp.at(0)/(pow(1+exp_tmp.at(0), 2))*
            std::get<0>(pseudo_lh).row(i).t()*std::get<0>(pseudo_lh).row(i);
          
        }
        if(attr_y_family == "poisson"){
          exp_tmp = arma::exp(std::get<0>(pseudo_lh).row(i)*coef);
          score += (std::get<1>(pseudo_lh).at(i) - exp_tmp.at(0))*std::get<0>(pseudo_lh).row(i).t();
          fisher += exp_tmp.at(0)*
            std::get<0>(pseudo_lh).row(i).t()*std::get<0>(pseudo_lh).row(i);
        } else if(attr_y_family == "normal"){
          exp_tmp = std::get<0>(pseudo_lh).row(i)*coef;
          score += (std::get<1>(pseudo_lh).at(i) - exp_tmp.at(0))*std::get<0>(pseudo_lh).row(i).t()/y_scale;
          fisher += std::get<0>(pseudo_lh).row(i).t()*std::get<0>(pseudo_lh).row(i)/y_scale;
        }
        
      }
      
    }
    coef += (arma::inv(fisher)*score);
    coefs.row(k) = coef.t();
    // If the maximal value of iterations is met end the estimation also if the convergence criteria is met
    // (otherwise start another iteration and reset score and info)
    // TODO there should be a check for identifiabiliy -> degrees
    if(k == max_iteration){
      non_converged = false;
    } else if ((sqrt(sum(arma::pow(coefs.row(k)- coefs.row(k-1),2)))<tol) && !non_stop){
      non_converged = false;
    } else {
      // Reset the score and fisher info
      score.fill(0);
      fisher.fill(0);
    }
    k++;
  }
  return(std::tuple<arma::vec, arma::vec, arma::mat, arma::mat> {coef,score, fisher, coefs.rows(0,k-1)});
}

std::tuple<arma::vec, arma::vec, arma::mat, arma::mat> 
  cond_estimation_nondegrees_pl(
    arma::vec coef,
    arma::uvec &i_vec,
    arma::uvec &j_vec,
    arma::uvec &overlap_vec, // Assuming this is arma::uvec (0 or 1)
    bool directed,
    std::tuple<arma::mat, arma::vec> &pseudo_lh,
    int max_iteration,
    double tol,
    arma::vec &coef_degrees,
    double offset_nonoverlap,
    bool &non_stop,
    std::string attr_x_family,
    std::string attr_y_family,
    double x_scale,
    double y_scale,
    bool fix_x) {
    
    int n_coef = coef.size();
    unsigned int n_units = coef_degrees.n_elem / 2;
    unsigned int n_net = i_vec.n_elem;
    
    // Extract the full design matrix and response vector
    const arma::mat& X_all = std::get<0>(pseudo_lh);
    const arma::vec& Y_all = std::get<1>(pseudo_lh);
    
    // --- 1. Define subviews for each component ---
    // Network component is always present
    const arma::mat X_net = X_all.rows(0, n_net - 1);
    const arma::vec Y_net = Y_all.subvec(0, n_net - 1);
    arma::mat X_x, X_y;
    arma::vec Y_x, Y_y;
    
    if (fix_x == false) {
      X_x = X_all.rows(n_net, n_net + n_units - 1);
      Y_x = Y_all.subvec(n_net, n_net + n_units - 1);
      
      X_y = X_all.rows(n_net + n_units, X_all.n_rows - 1);
      Y_y = Y_all.subvec(n_net + n_units, Y_all.n_elem - 1);
    } else {
      X_y = X_all.rows(n_net, X_all.n_rows - 1);
      Y_y = Y_all.subvec(n_net, Y_all.n_elem - 1);
    }
    
    // Pre-calculate network offsets
    const arma::vec net_offsets = (1.0 - arma::conv_to<arma::vec>::from(overlap_vec)) * offset_nonoverlap;
    
    // Pre-calculate degrees indices
    arma::uvec j_pop_indices; 
    if(directed){
      j_pop_indices = j_vec - 1 + n_units ;
    } else {
      j_pop_indices =j_vec - 1;
    }
    const arma::uvec i_pop_indices = i_vec - 1;
    
    // --- 2. Initialize estimation variables ---
    arma::vec score(n_coef);
    arma::mat fisher(n_coef, n_coef);
    arma::mat coefs(max_iteration + 1, n_coef);
    coefs.row(0) = coef.t();
    
    bool non_converged = true;
    int k = 1;
    
    // --- 3. Iterative Estimation (Newton-Raphson) ---
    while (non_converged) {
      score.zeros();
      fisher.zeros();
      
      // --- Component 1: Network (Logistic Model) ---
      arma::vec eta_net = X_net * coef + net_offsets + 
        coef_degrees.elem(i_pop_indices) + 
        coef_degrees.elem(j_pop_indices);
      
      arma::vec exp_eta_net = arma::exp(eta_net);
      arma::vec prob_net = exp_eta_net / (1.0 + exp_eta_net);
      arma::vec var_net = prob_net % (1.0 - prob_net);
      
      score += X_net.t() * (Y_net - prob_net);
      fisher += X_net.t() * arma::diagmat(var_net) * X_net;
      
      // --- Component 2: Attribute 'x' ---
      if (fix_x == false) {
        if (attr_x_family == "binomial") {
          arma::vec eta_x = X_x * coef;
          arma::vec exp_eta_x = arma::exp(eta_x);
          arma::vec prob_x = exp_eta_x / (1.0 + exp_eta_x);
          arma::vec var_x = prob_x % (1.0 - prob_x);
          
          score += X_x.t() * (Y_x - prob_x);
          fisher += X_x.t() * arma::diagmat(var_x) * X_x;
          
        } else if (attr_x_family == "poisson") {
          arma::vec eta_x = X_x * coef;
          arma::vec mu_x = arma::exp(eta_x);
          
          score += X_x.t() * (Y_x - mu_x);
          fisher += X_x.t() * arma::diagmat(mu_x) * X_x;
          
        } else if (attr_x_family == "normal") {
          arma::vec mu_x = X_x * coef; 
          score += X_x.t() * (Y_x - mu_x) / x_scale;
          fisher += (X_x.t() * X_x) / x_scale;
        }
      }
      
      // --- Component 3: Attribute 'y' ---
      if (attr_y_family == "binomial") {
        arma::vec eta_y = X_y * coef;
        arma::vec exp_eta_y = arma::exp(eta_y);
        arma::vec prob_y = exp_eta_y / (1.0 + exp_eta_y);
        arma::vec var_y = prob_y % (1.0 - prob_y);
        
        score += X_y.t() * (Y_y - prob_y);
        fisher += X_y.t() * arma::diagmat(var_y) * X_y;
        
      } else if (attr_y_family == "poisson") {
        arma::vec eta_y = X_y * coef;
        arma::vec mu_y = arma::exp(eta_y);
        
        score += X_y.t() * (Y_y - mu_y);
        fisher += X_y.t() * arma::diagmat(mu_y) * X_y;
        
      } else if (attr_y_family == "normal") {
        arma::vec mu_y = X_y * coef;
        
        score += X_y.t() * (Y_y - mu_y) / y_scale;
        fisher += (X_y.t() * X_y) / y_scale;
      }
      // Rcout << "Iteration " << k << score << std::endl;
      // Rcout << "Iteration " << k << fisher << std::endl;
      // --- 4. Update and Check Convergence ---
      coef += (arma::solve(fisher, score));
      coefs.row(k) = coef.t();
      
      if (k == max_iteration) {
        non_converged = false;
      } else if ((arma::norm(coefs.row(k) - coefs.row(k - 1)) < tol) && !non_stop) {
        non_converged = false;
      }
      
      k++;
    }
    
    return std::make_tuple(coef, score, fisher, coefs.rows(0, k - 1));
  }

double calculate_llh(
    const arma::vec& coef,
    arma::vec& coef_degrees,
    const arma::uvec& i_vec,
    const arma::uvec& j_vec,
    const arma::uvec& overlap_vec,
    bool directed,
    const std::tuple<arma::mat, arma::vec>& pseudo_lh,
    double offset_nonoverlap,
    const std::string& attr_x_family,
    const std::string& attr_y_family,
    double x_scale,
    double y_scale,
    int n_units,
    bool fix_x, 
    bool fix_z, 
    bool nonoverlap_random) {
  // Rcout << "Start" << std::endl;
  
  if(coef_degrees.size() ==1){
    if(directed){
      arma::vec tmp = arma::vec(2*n_units);
      tmp.fill(0.0);
      coef_degrees = tmp;
    } else {
      arma::vec tmp = arma::vec(n_units);
      tmp.fill(0.0);
      coef_degrees = tmp;
    }
  }
  unsigned int n_net = i_vec.n_elem*!fix_z;
  // Rcout << coef_degrees.n_elem << std::endl;
  const arma::mat& X_all = std::get<0>(pseudo_lh);
  const arma::vec& Y_all = std::get<1>(pseudo_lh);
  
  
  arma::mat X_x, X_y, X_net;
  arma::vec Y_x, Y_y, Y_net;
  
  if (fix_z == false) {
    X_net = X_all.rows(0, n_net - 1);
    Y_net = Y_all.subvec(0, n_net - 1);  
  }
  // Rcout << "c" << std::endl;
  if (fix_x == false) {
    X_x = X_all.rows(n_net, n_net + n_units - 1);
    Y_x = Y_all.subvec(n_net, n_net + n_units - 1);
    X_y = X_all.rows(n_net + n_units, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net + n_units, Y_all.n_elem - 1);
  } else {
    X_y = X_all.rows(n_net, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net, Y_all.n_elem - 1);
  }
  // Rcout << "b" << std::endl;
  const arma::vec net_offsets = (1.0 - arma::conv_to<arma::vec>::from(overlap_vec)) * offset_nonoverlap;
  
  arma::uvec j_pop_indices;
  if(directed){
    j_pop_indices = j_vec - 1 + n_units;
  } else {
    j_pop_indices = j_vec - 1;
  }
  const arma::uvec i_pop_indices = i_vec - 1;
  
  double llh = 0.0;
  // --- Component 1: Network (Logistic Model) ---
  if (fix_z == false) {
    arma::vec eta_net = X_net * coef + net_offsets +
      coef_degrees.elem(i_pop_indices) +
      coef_degrees.elem(j_pop_indices);
    arma::vec exp_eta_net = arma::exp(eta_net);
    arma::vec llh_contributions =Y_net % eta_net - arma::log1p(exp_eta_net);
    if (!nonoverlap_random) {
      llh_contributions = overlap_vec % llh_contributions;
    }
    llh += arma::sum(llh_contributions);
  }
  // --- Component 2: Attribute 'x' ---
  if (fix_x == false) {
    if (attr_x_family == "binomial") {
      // logL = sum( Y*eta - log(1 + exp(eta)) )
      arma::vec eta_x = X_x * coef;
      arma::vec exp_eta_x = arma::exp(eta_x);
      llh += arma::sum(Y_x % eta_x - arma::log1p(exp_eta_x));
      
    } else if (attr_x_family == "poisson") {
      // logL = sum( Y*eta - exp(eta) - lgamma(Y+1) )
      arma::vec eta_x = X_x * coef;
      arma::vec mu_x = arma::exp(eta_x);
      llh += arma::sum(Y_x % eta_x - mu_x - arma::lgamma(Y_x + 1.0));
      
    } else if (attr_x_family == "normal") {
      // logL = sum( -0.5*log(2*pi*scale) - (Y - mu)^2 / (2*scale) )
      arma::vec mu_x = X_x * coef;
      double const_x = -0.5 * std::log(2.0 * M_PI * x_scale);
      llh += arma::sum(const_x - arma::pow(Y_x - mu_x, 2) / (2.0 * x_scale));
    }
  }
  // Rcout << "Log-likelihood after x component: " << llh << std::endl;
  
  // --- Component 3: Attribute 'y' ---
  if (attr_y_family == "binomial") {
    // logL = sum( Y*eta - log(1 + exp(eta)) )
    arma::vec eta_y = X_y * coef;
    arma::vec exp_eta_y = arma::exp(eta_y);
    llh += arma::sum(Y_y % eta_y - arma::log1p(exp_eta_y));
    
  } else if (attr_y_family == "poisson") {
    // logL = sum( Y*eta - exp(eta) - lgamma(Y+1) )
    arma::vec eta_y = X_y * coef;
    arma::vec mu_y = arma::exp(eta_y);
    llh += arma::sum(Y_y % eta_y - mu_y - arma::lgamma(Y_y + 1.0));
    
  } else if (attr_y_family == "normal") {
    // logL = sum( -0.5*log(2*pi*scale) - (Y - mu)^2 / (2*scale) )
    arma::vec mu_y = X_y * coef;
    double const_y = -0.5 * std::log(2.0 * M_PI * y_scale);
    llh += arma::sum(const_y - arma::pow(Y_y - mu_y, 2) / (2.0 * y_scale));
  }
  // Rcout << "Log-likelihood after y component: " << llh << std::endl;
  
  return llh;
}


arma::mat get_C_new(
    const arma::vec& coef,
    const arma::uvec& i_vec,
    const arma::uvec& j_vec,
    const arma::uvec& overlap_vec,
    bool directed,
    std::tuple<arma::mat, arma::vec>& pseudo_lh, 
    const arma::vec& coef_degrees,
    double offset_nonoverlap,
    bool fix_x,
    const std::string& attr_x_family,
    const std::string& attr_y_family,
    double attr_x_scale,
    double attr_y_scale)
{
  unsigned int n_units;
  if (directed) {
    n_units = coef_degrees.n_elem / 2;
  } else { 
    n_units = coef_degrees.n_elem;
  }
  // number of network dyads
  const unsigned int n_net = i_vec.n_elem;
  
  // full design matrix and response (pseudo_lh)
  const arma::mat& X_all = std::get<0>(pseudo_lh);
  const arma::vec& Y_all = std::get<1>(pseudo_lh);
  const arma::mat X_net = X_all.rows(0, n_net - 1);
  
  arma::mat X_x; arma::vec Y_x;
  arma::mat X_y; arma::vec Y_y;
  
  if (!fix_x) {
    X_x = X_all.rows(n_net, n_net + n_units - 1);
    Y_x = Y_all.subvec(n_net, n_net + n_units - 1);
    
    X_y = X_all.rows(n_net + n_units, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net + n_units, Y_all.n_elem - 1);
  } else { 
    X_y = X_all.rows(n_net, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net, Y_all.n_elem - 1);
  }
  // 1) network block, which is logistic
  arma::vec net_offsets = (1.0 - arma::conv_to<arma::vec>::from(overlap_vec)) * offset_nonoverlap;
  arma::vec eta_net = X_net * coef + net_offsets + coef_degrees.elem(i_vec-1) + coef_degrees.elem(j_vec-1+ n_units*directed);
  arma::vec exp_eta_net = arma::exp(eta_net);
  arma::vec prob_net(eta_net.n_elem);
  
  // Find indices for positive and non-positive eta
  arma::uvec pos_idx = arma::find(eta_net > 0);
  arma::uvec nonpos_idx = arma::find(eta_net <= 0);
  
  if (pos_idx.n_elem > 0) {
    prob_net.elem(pos_idx) = 1.0 / (1.0 + arma::exp(-eta_net.elem(pos_idx)));
  }
  if (nonpos_idx.n_elem > 0) {
    arma::vec exp_eta_neg = arma::exp(eta_net.elem(nonpos_idx));
    prob_net.elem(nonpos_idx) = exp_eta_neg / (1.0 + exp_eta_neg);
  }
  arma::vec var_net = prob_net % (1.0 - prob_net);
  
  // Prepare w vector of length X_all.n_rows
  arma::vec w(X_all.n_rows, arma::fill::zeros);
  
  // assign network block
  w.rows(0, n_net - 1) = var_net;
  
  // Rcout << "Min variance network block: " << min(exp_eta_net) << std::endl;
  // Rcout << "Max variance network block: " << max(exp_eta_net) << std::endl;
  // Rcout << "Mean variance network block: " << mean(var_net) << std::endl;
  // Rcout << "Mean per col: " << arma::mean(X_net, 0) << std::endl;
  
  
  // 2) attribute x (if present)
  if (!fix_x) {
    arma::vec var_x;
    if (attr_x_family == "binomial") {
      arma::vec eta_x = X_x * coef;
      arma::vec ex = arma::exp(eta_x);
      arma::vec p = ex / (1.0 + ex);
      var_x = p % (1.0 - p);
    } else if (attr_x_family == "poisson") { 
      arma::vec eta_x = X_x * coef;
      arma::vec mu = arma::exp(eta_x);
      var_x = mu;               // variance = mu
    } else if (attr_x_family == "normal") { 
      // For normal, variance is constant = attr_x_scale
      var_x = arma::vec(X_x.n_rows, arma::fill::value(1.0/attr_x_scale));
    } else { 
      Rcpp::stop("Unknown attr_x_family: must be 'binomial', 'poisson' or 'normal'.");
    } 
    // place into w
    w.rows(n_net, n_net + X_x.n_rows - 1) = var_x;
    // Rcout << "Mean variance x block: " << arma::mean(var_x) << std::endl;
  } 
  
  // 3) attribute y
  arma::vec var_y;
  if (attr_y_family == "binomial") {
    arma::vec eta_y = X_y * coef;
    arma::vec ey = arma::exp(eta_y);
    arma::vec p = ey / (1.0 + ey);
    var_y = p % (1.0 - p);
  } else if (attr_y_family == "poisson") { 
    arma::vec eta_y = X_y * coef;
    arma::vec mu = arma::exp(eta_y);
    var_y = mu;
  } else if (attr_y_family == "normal") { 
    var_y = arma::vec(X_y.n_rows, arma::fill::value(1.0/attr_y_scale));
  } else { 
    Rcpp::stop("Unknown attr_y_family: must be 'binomial', 'poisson' or 'normal'.");
  } 
  arma::uword start_y;
  if (!fix_x) start_y = n_net + X_x.n_rows;
  else start_y = n_net;
  w.rows(start_y, start_y + X_y.n_rows - 1) = var_y;
  arma::mat X_weighted = X_all.each_col() % w;
  arma::mat fisher = X_all.t() * X_weighted;
  return fisher; 
}

arma::mat get_C(arma::vec coef, arma::uvec &i_vec,
                arma::uvec  &j_vec,
                arma::uvec  &overlap_vec,
                bool directed,
                std::tuple<arma::mat,arma::vec> &pseudo_lh,
                arma::vec &coef_degrees,
                double offset_nonoverlap,
                const std::string& attr_x_family,
                const std::string& attr_y_family,
                double attr_x_scale,
                double attr_y_scale) {
  unsigned int n_units;
  if(directed){
    n_units = coef_degrees.n_elem/2;
  } else {
    n_units = coef_degrees.n_elem;
  }
  // The idea is that we first go over the connections  and then the attributes 
  unsigned int number_elements_network = i_vec.size();
  
  arma::vec vec_network = arma::exp(offset_nonoverlap*(1-overlap_vec)+std::get<0>(pseudo_lh).rows(0,number_elements_network-1)*coef +
    coef_degrees.elem(i_vec-1) + coef_degrees.elem(j_vec-1+ n_units*directed));
  arma::vec vec_attribute = arma::exp(std::get<0>(pseudo_lh).rows(number_elements_network,std::get<0>(pseudo_lh).n_rows-1)*coef); 
  arma::vec result = join_cols(vec_network, vec_attribute); 
  arma::vec prob = result/(1+result);
  arma::vec values(i_vec.size());
  values.fill(1.0);
  
  const arma::mat& X = std::get<0>(pseudo_lh);
  const arma::vec& w = prob % (1 - prob);
  arma::mat X_weighted = X.each_col() % w;
  // arma::mat cov_degrees_weighted = cov_degrees.each_col() % w;
  arma::mat fisher_alt = X.t() * X_weighted;
  return fisher_alt;
  
}

std::tuple<arma::vec,arma::vec, arma::mat, arma::mat>  cond_estimation_degrees_pl(arma::vec coef, 
                                                                                  arma::uvec &i_vec, 
                                                                                  arma::uvec  &j_vec,
                                                                                  arma::uvec  &overlap_vec,
                                                                                  bool directed,
                                                                                  std::tuple<arma::mat,arma::vec> &pseudo_lh, 
                                                                                  int max_iteration, 
                                                                                  double tol, 
                                                                                  arma::vec coef_nondegrees, 
                                                                                  double offset_nonoverlap, 
                                                                                  arma::mat A_inv, 
                                                                                  int it, 
                                                                                  bool &non_stop, 
                                                                                  bool nonoverlap_random) {
  int n_units;
  if(directed){
    n_units = coef.n_elem/2;
  } else { 
    n_units = coef.n_elem;
  }
  
  arma::vec  score;
  arma::mat  coefs;
  if(directed){
    score.reshape(2*n_units, 1);
    score.fill(0);
    coefs.reshape(max_iteration+1,2*n_units);
  } else {
    score.reshape(n_units, 1);
    score.fill(0);
    coefs.reshape(max_iteration+1,n_units);
  }
  arma::vec weights, exp_tmp; 
  coefs.row(0)= coef.t();
  bool non_converged = true;
  int k = 1;
  while(non_converged) {
    for(int i: seq(0,i_vec.size() -1)){
      if(directed){
        if(overlap_vec.at(i)){
          exp_tmp = arma::exp((coef.at(i_vec.at(i)-1) +
            coef.at(j_vec.at(i)-1 + n_units)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
        } else {
          if(nonoverlap_random){
            exp_tmp = arma::exp(offset_nonoverlap+(coef.at(i_vec.at(i)-1) +
              coef.at(j_vec.at(i)-1 + n_units)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);    
          } else {
            exp_tmp = 0.0;
          }
          
        }
        weights = exp_tmp/(1+exp_tmp);
        if(j_vec.at(i) != n_units){
          score.at(j_vec.at(i)-1+ n_units) += std::get<1>(pseudo_lh).at(i) - weights.at(0);
        }
      } else {
        if(overlap_vec.at(i)){
          exp_tmp = arma::exp( (coef.at(i_vec.at(i)-1) + 
            coef.at(j_vec.at(i)-1)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
        } else {
          if(nonoverlap_random){
            exp_tmp = arma::exp(offset_nonoverlap+(coef.at(i_vec.at(i)-1) + 
              coef.at(j_vec.at(i)-1)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);  
          } else {
            exp_tmp = 0.0;
          }
          
        }
        weights = exp_tmp/(1+exp_tmp);
        score.at(j_vec.at(i)-1) += std::get<1>(pseudo_lh).at(i) - weights.at(0);
        
      }
      score.at(i_vec.at(i)-1) +=  std::get<1>(pseudo_lh).at(i) - weights.at(0);
    }
    coef += (A_inv*score);
    coefs.row(k) = coef.t();
    // If the maximal value of iterations is met end the estimation also if the convergence criteria is met 
    // (otherwise start another iteration and reset score and info)
    if(k == max_iteration){
      non_converged = false;
    } else if ((sqrt(sum(arma::pow(coefs.row(k)- coefs.row(k-1),2)))<tol) && !non_stop){
      non_converged = false;
    } else { 
      // Reset the score and fisher info
      // score_alt.fill(0);
      score.fill(0);
    } 
    k++;
  } 
  
  arma::vec coef_MM = coef + A_inv*score;
  // arma::vec exp_tmp_MM,  exp_rest; 
  // // TODO update the calculation of the llh 
  // double ll_MM,  ll_attributes; 
  // if(directed){
  //   exp_tmp_MM = arma::exp(offset_nonoverlap+(coef_MM.elem(i_vec  -1) +
  //     coef_MM.elem(j_vec  -1 + n_units)) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees);
  //   Rcout << mean(exp_tmp_MM) << std::endl;
  //   exp_rest = arma::exp(std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees);
  //   arma::vec log_one_min_pi_MM =  - log(1+exp_tmp_MM);
  //   arma::vec log_one_min_pi_rest =  - log(1+exp_rest);
  //   
  //   ll_MM = sum(log_one_min_pi_MM + std::get<1>(pseudo_lh).head_rows(i_vec.size())%(offset_nonoverlap+coef_MM.elem(i_vec-1) +
  //     coef_MM.elem(j_vec  -1 + n_units) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees));
  //   ll_attributes =  sum(log_one_min_pi_rest + std::get<1>(pseudo_lh).tail_rows(2*n_units)%
  //     (std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees));
  // } else {
  //   exp_tmp_MM = arma::exp(offset_nonoverlap+(coef_MM.elem(i_vec  -1) +
  //     coef_MM.elem(j_vec  -1)) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees);
  //   // Rcout << mean(exp_tmp_MM) << std::endl;
  //   
  //   exp_rest = arma::exp(std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees);
  //   arma::vec log_one_min_pi_MM =  - log(1+exp_tmp_MM);
  //   arma::vec log_one_min_pi_rest =  - log(1+exp_rest);
  //   
  //   ll_MM = sum(log_one_min_pi_MM + std::get<1>(pseudo_lh).head_rows(i_vec.size())%(offset_nonoverlap+coef_MM.elem(i_vec-1) +
  //     coef_MM.elem(j_vec  -1) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees));
  //   ll_attributes =  sum(log_one_min_pi_rest + std::get<1>(pseudo_lh).tail_rows(2*n_units)%
  //     (std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees));
  //   
  // }
  // double llh_alt; 
  return(std::tuple<arma::vec, arma::vec, arma::mat, arma::mat> {coef,score, A_inv, coefs.rows(0,k-1)});
}


std::tuple<arma::vec,arma::vec, arma::mat , arma::mat>  cond_estimation_degrees_pl_accelerated(arma::vec coef, 
                                                                                               arma::uvec &i_vec, 
                                                                                               arma::uvec  &j_vec,
                                                                                               arma::uvec  &overlap_vec,
                                                                                               bool directed,
                                                                                               std::tuple<arma::mat,arma::vec> &pseudo_lh, 
                                                                                               int max_iteration, 
                                                                                               double tol, 
                                                                                               arma::vec coef_nondegrees, 
                                                                                               double offset_nonoverlap, 
                                                                                               arma::mat A_inv, 
                                                                                               bool &non_stop, 
                                                                                               arma::vec & old_score_pop, 
                                                                                               arma::vec & old_coef_pop, 
                                                                                               arma::mat & old_M, 
                                                                                               int it, 
                                                                                               bool first_it, 
                                                                                               bool nonoverlap_random) {
  int n_units;
  if(directed){
    n_units = coef.n_elem/2;
  } else {
    n_units = coef.n_elem;
  }
  
  // Define the additional stuff needed for the quasi Newton acceleration
  arma::mat M_new;
  
  arma::vec  score, score_old;
  arma::mat  coefs;
  if(directed){
    score.reshape(2*n_units, 1);
    score.fill(0);
    score_old.reshape(2*n_units, 1);
    score_old.fill(0);
    coefs.reshape(max_iteration+1,2*n_units);
  } else {
    score.reshape(n_units, 1);
    score.fill(0);
    score_old.reshape(n_units, 1);
    score_old.fill(0);
    coefs.reshape(max_iteration+1,n_units);
  }
  arma::vec weights,weights_old,  exp_tmp, exp_tmp_old; 
  coefs.row(0)= coef.t();
  if(first_it){
    old_coef_pop = coef;
  }
  // Rcout << "Hi"<< std::endl;
  bool non_converged = true;
  int k = 1;
  while(non_converged) {
    for(int i: seq(0,i_vec.size() -1)){
      if(directed){
        if(overlap_vec.at(i)){
          exp_tmp = arma::exp((coef.at(i_vec.at(i)-1) +
            coef.at(j_vec.at(i)-1 + n_units)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
          exp_tmp_old = arma::exp((old_coef_pop.at(i_vec.at(i)-1) +
            old_coef_pop.at(j_vec.at(i)-1 + n_units)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
          
        } else {
          if(nonoverlap_random){
            exp_tmp = arma::exp(offset_nonoverlap+(coef.at(i_vec.at(i)-1) +
              coef.at(j_vec.at(i)-1 + n_units)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);  
            exp_tmp_old =  arma::exp(offset_nonoverlap+(old_coef_pop.at(i_vec.at(i)-1) +
              old_coef_pop.at(j_vec.at(i)-1 + n_units)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);  
          }else {
            exp_tmp = 0.0; 
            exp_tmp_old= 0.0; 
          }
        }
        weights = exp_tmp/(1+exp_tmp);
        weights_old = exp_tmp_old/(1+exp_tmp_old);
        if(j_vec.at(i) != n_units){
          score.at(j_vec.at(i)-1+ n_units) += std::get<1>(pseudo_lh).at(i) - weights.at(0);
          score_old.at(j_vec.at(i)-1+ n_units) += std::get<1>(pseudo_lh).at(i) - weights_old.at(0);  
        }
      } else {
        if(overlap_vec.at(i)){
          exp_tmp = arma::exp( (coef.at(i_vec.at(i)-1) + 
            coef.at(j_vec.at(i)-1)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
          exp_tmp_old = arma::exp( (old_coef_pop.at(i_vec.at(i)-1) + 
            old_coef_pop.at(j_vec.at(i)-1)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
        } else { 
          if(nonoverlap_random){
            exp_tmp = arma::exp(offset_nonoverlap+(coef.at(i_vec.at(i)-1) + 
              coef.at(j_vec.at(i)-1)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
            exp_tmp_old =  arma::exp(offset_nonoverlap+(old_coef_pop.at(i_vec.at(i)-1) + 
              old_coef_pop.at(j_vec.at(i)-1)) + std::get<0>(pseudo_lh).row(i)*coef_nondegrees);
          } else {
            exp_tmp = 0.0; 
            exp_tmp_old= 0.0; 
          }
        } 
        
        weights = exp_tmp/(1+exp_tmp);
        weights_old = exp_tmp_old/(1+exp_tmp_old);
        score.at(j_vec.at(i)-1) += std::get<1>(pseudo_lh).at(i) - weights.at(0);
        score_old.at(j_vec.at(i)-1) += std::get<1>(pseudo_lh).at(i) - weights_old.at(0);
      } 
      score.at(i_vec.at(i)-1) +=  std::get<1>(pseudo_lh).at(i) - weights.at(0);
      score_old.at(i_vec.at(i)-1) +=  std::get<1>(pseudo_lh).at(i) - weights_old.at(0);
    } 
    
    // Rcout <<score.at(2*n_units-1) << std::endl;
    // Rcout <<coef.at(2*n_units-1) << std::endl;
    
    if(first_it){
      M_new = old_M;
      old_score_pop = score;
      old_coef_pop = coef;
    } else {
      // Rcout <<sum(old_score_pop)<< std::endl;
      // Rcout <<sum(score_old)<< std::endl;
      // Rcout <<sum(score)<< std::endl;
      // 
      arma::vec score_change_pop = score - score_old;
      old_score_pop = score;
      arma::vec coef_change_pop = coef - old_coef_pop;
      old_coef_pop = coef;
      
      arma::vec r_new = coef_change_pop  + A_inv*score_change_pop;
      arma::vec q_new = r_new - old_M*score_change_pop;
      arma::vec c_new_alt = q_new.t()*score_change_pop;
      M_new = old_M + q_new*q_new.t()/c_new_alt.at(0);
      old_M = M_new;
    }
    
    arma::vec coef_MM = coef + A_inv*score;
    arma::vec coef_accel = coef + (A_inv-M_new)*score;
    arma::vec exp_tmp_MM, exp_tmp_accel; 
    double ll_MM, ll_accel; 
    if(directed){
      exp_tmp_MM = arma::exp(offset_nonoverlap+(coef_MM.elem(i_vec  -1) +
        coef_MM.elem(j_vec  -1 + n_units)) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees);
      exp_tmp_accel = arma::exp(offset_nonoverlap+(coef_accel.elem(i_vec  -1) +
        coef_accel.elem(j_vec  -1 + n_units)) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees);
      // exp_rest = arma::exp(std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees);
      arma::vec log_one_min_pi_MM =  - log(1+exp_tmp_MM);
      arma::vec log_one_min_pi_accel =  - log(1+exp_tmp_accel);
      // arma::vec log_one_min_pi_rest =  - log(1+exp_rest);
      
      ll_MM = sum(log_one_min_pi_MM + std::get<1>(pseudo_lh).head_rows(i_vec.size())%(offset_nonoverlap+coef_MM.elem(i_vec-1) +
        coef_MM.elem(j_vec  -1 + n_units) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees));
      ll_accel = sum(log_one_min_pi_accel + std::get<1>(pseudo_lh).head_rows(i_vec.size())%(offset_nonoverlap+ coef_accel.elem(i_vec-1) +
        coef_accel.elem(j_vec  -1 + n_units) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees));
      // ll_attributes =  sum(log_one_min_pi_rest + std::get<1>(pseudo_lh).tail_rows(2*n_units)%
      //   (std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees));
    } else {
      exp_tmp_MM = arma::exp(offset_nonoverlap+(coef_MM.elem(i_vec  -1) +
        coef_MM.elem(j_vec  -1)) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees);
      exp_tmp_accel = arma::exp(offset_nonoverlap+(coef_accel.elem(i_vec  -1) +
        coef_accel.elem(j_vec  -1)) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees);
      // exp_rest = arma::exp(std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees);
      arma::vec log_one_min_pi_MM =  - log(1+exp_tmp_MM);
      arma::vec log_one_min_pi_accel =  - log(1+exp_tmp_accel);
      // arma::vec log_one_min_pi_rest =  - log(1+exp_rest);
      
      ll_MM = sum(log_one_min_pi_MM + std::get<1>(pseudo_lh).head_rows(i_vec.size())%(offset_nonoverlap+coef_MM.elem(i_vec-1) +
        coef_MM.elem(j_vec  -1) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees));
      ll_accel = sum(log_one_min_pi_accel + std::get<1>(pseudo_lh).head_rows(i_vec.size())%(offset_nonoverlap+ coef_accel.elem(i_vec-1) +
        coef_accel.elem(j_vec  -1) + std::get<0>(pseudo_lh).head_rows(i_vec.size())*coef_nondegrees));
      // ll_attributes =  sum(log_one_min_pi_rest + std::get<1>(pseudo_lh).tail_rows(2*n_units)%
      //   (std::get<0>(pseudo_lh).tail_rows(2*n_units)*coef_nondegrees));
      
    }
    
    if(ll_accel>ll_MM){
      coef = coef_accel;
      
    } else {
      coef = coef_MM;
      
      
    }
    // coef += (A_inv-M_new)*score;
    coefs.row(k) = coef.t();
    // If the maximal value of iterations is met end the estimation also if the convergence criteria is met 
    // (otherwise start another iteration and reset score and info)
    // TODO there should be a check for identifiabiliy -> degrees 
    if(k == max_iteration){
      non_converged = false;
    } else if ((sqrt(sum(arma::pow(coefs.row(k)- coefs.row(k-1),2)))<tol) && !non_stop){
      non_converged = false;
    } else {  
      // Reset the score and fisher info
      // score_alt.fill(0);
      score.fill(0);
    }  
    k++;
  }  
  
  return(std::tuple<arma::vec, arma::vec, arma::mat, arma::mat> {coef,score, M_new, coefs.rows(0,k-1)});
}

// arma::uvec find_target_indices(const std::vector<std::string>& source) {
//   // 1. Construct the target set for O(1) average lookup complexity
//   const std::unordered_set<std::string> targets = {
//     "edges", "attribute_x", "attribute_y"
//   };
//   arma::uvec indices(targets.size());
//   for (size_t i = 0; i < source.size(); ++i) {
//     indices.at(targets.find(source[i]))= i;
//   }
//   
//   return indices;
// }
// [[Rcpp::export]]
List pl_estimation(arma::vec coef,
                   const arma::mat& z_network ,
                   const arma::vec& x_attribute ,
                   const arma::vec& y_attribute ,
                   const arma::mat& neighborhood,
                   const arma::mat& overlap,
                   bool directed,
                   std::vector<std::string> terms,
                   std::vector<arma::mat> &data_list,
                   std::vector<double> &type_list, 
                   bool display_progress, 
                   int max_iteration, 
                   double tol, 
                   double offset_nonoverlap, 
                   bool non_stop, 
                   bool fix_x, 
                   bool fix_z, 
                   std::string attr_x_family, 
                   std::string attr_y_family, 
                   double attr_x_scale, 
                   double attr_y_scale, 
                   bool nonoverlap_random) {
  List res; 
  std::tuple<arma::mat,arma::vec> pseudo_lh;
  int n_units = y_attribute.size();
  arma::uvec  i_vec, j_vec, overlap_vec; 
  if(directed){
    i_vec = arma::uvec(n_units*(n_units-1)); 
    j_vec = arma::uvec(n_units*(n_units-1)); 
    overlap_vec = arma::uvec(n_units*(n_units-1)); 
  } else { 
    i_vec= arma::uvec(n_units*(n_units-1)/2); 
    j_vec= arma::uvec(n_units*(n_units-1)/2); 
    overlap_vec = arma::uvec(n_units*(n_units-1)/2); 
  } 
  // Calculates the data in a suitable format -> a vector or 32 x p 
  // (being the dimension of the sufficient statistics) matrices corresponding to the data of each dyad
  XYZ_class object(n_units,directed, x_attribute, y_attribute,z_network,neighborhood,overlap, attr_x_family, attr_y_family,attr_x_scale, attr_y_scale);
  
  int k = 1;
  bool non_converged = true;
  
  
  if(display_progress) {
    Rcout << "Starting with the preprocessing" << std::endl;
  }
  
  arma::vec exp_tmp, score_tmp; 
  pseudo_lh = xyz_get_info_pl(object,terms,data_list,type_list, display_progress,
                              i_vec,j_vec, overlap_vec, n_units, fix_x, fix_z);
  
  // arma::uvec where_wrong = find(arma::var(std::get<0>(pseudo_lh), 0) == 0);
  arma::rowvec variances = arma::var(std::get<0>(pseudo_lh), 0, 0);
  
  
  
  const std::unordered_set<std::string> protected_terms = {
    "edges", "attribute_x", "attribute_y"
  };
  
  std::vector<uint8_t> keep_flags(variances.n_elem);
  bool exclusions_exist = false;
  
  for (size_t i = 0; i < variances.n_elem; ++i) {
    bool is_constant = (variances[i] == 0);
    bool is_protected = (protected_terms.count(terms[i]) > 0);
    
    if (!is_constant || is_protected) {
      keep_flags[i] = 1;  
    } else {
      keep_flags[i] = 0;
      exclusions_exist = true;
    }
  }
  arma::uvec where_right = arma::find(arma::conv_to<arma::uvec>::from(keep_flags) == 1);
  arma::uvec where_wrong = arma::find(arma::conv_to<arma::uvec>::from(keep_flags) == 0);
  if (exclusions_exist) {
    
    Rcout << "Certain statistics strictly invariant across dyads have been identified.\n"
          << "Excluding " << where_wrong.n_elem << " terms to ensure model identifiability.\n"
          << std::endl;
    
    
    std::get<0>(pseudo_lh) = std::get<0>(pseudo_lh).cols(where_right);
    coef = coef.rows(where_right);
    
    // 6. Subset the terms vector safely
    std::vector<std::string> new_terms;
    new_terms.reserve(where_right.n_elem);
    for (arma::uword idx : where_right) {
      new_terms.push_back(terms[idx]);
    } 
    terms = new_terms;
  }
  
  // if(where_wrong.size() >0){
  //   Rcout << "Some statistics (other than the intercept terms, edges, attribute_x, attribute_y) do not change over all pairs/units (they are excluded from the model since their MLE is negative infinity)" << std::endl;
  //   arma::uvec where_right = find(arma::var(std::get<0>(pseudo_lh), 0) != 0);
  //   // terms(where_right);
  //   std::get<0>(pseudo_lh) = std::get<0>(pseudo_lh).cols(where_right);
  //   coef = coef.rows(where_right);
  // }
  int n_coef = coef.size();
  unsigned int n_net = i_vec.n_elem;
  
  arma::mat coefs(max_iteration+1, coef.size()), fisher(coef.size(),coef.size(), arma::fill::zeros);
  k++;
  // Extract the full design matrix and response vector
  const arma::mat& X_all = std::get<0>(pseudo_lh);
  const arma::vec& Y_all = std::get<1>(pseudo_lh);
  
  // --- 1. Define subviews for each component ---
  arma::mat X_x, X_y, X_net;
  arma::vec Y_x, Y_y, Y_net;
  if(fix_z == false){
    X_net = X_all.rows(0, n_net - 1);
    Y_net = Y_all.subvec(0, n_net - 1);  
  }
  
  if (fix_x == false) {
    X_x = X_all.rows(n_net*!fix_z, n_net*!fix_z + n_units - 1);
    Y_x = Y_all.subvec(n_net*!fix_z, n_net*!fix_z + n_units - 1);
    
    X_y = X_all.rows(n_net*!fix_z + n_units, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net*!fix_z + n_units, Y_all.n_elem - 1);
  } else {
    X_y = X_all.rows(n_net*!fix_z, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net*!fix_z, Y_all.n_elem - 1);
  }
  
  // Pre-calculate network offsets
  const arma::vec net_offsets = (1.0 - arma::conv_to<arma::vec>::from(overlap_vec)) * offset_nonoverlap;
  
  // --- 2. Initialize estimation variables ---
  arma::vec score(n_coef);
  coefs.row(0) = coef.t();
  arma::vec llhs(max_iteration);
  arma::vec stand_in(1);
  stand_in.fill(0.0);
  
  if(display_progress) {
    Rcout << "Starting with the estimation" << std::endl;
  }
  
  // --- 3. Iterative Estimation (Newton-Raphson) ---
  while (non_converged) {
    if(display_progress){
      Rcout << "Iteration " << k - 1 << "\r";
      Rcpp::Rcout.flush();
      
    }
    score.zeros();
    fisher.zeros();
    Rcpp::checkUserInterrupt();
    // --- Component 1: Network (Logistic Model) ---
    if(!fix_z){
      arma::vec eta_net = X_net * coef + net_offsets;
      arma::vec exp_eta_net = arma::exp(eta_net);
      arma::vec prob_net = exp_eta_net / (1.0 + exp_eta_net);
      if(!nonoverlap_random){
        prob_net = overlap_vec%prob_net;  
      }
      
      arma::vec var_net = prob_net % (1.0 - prob_net);
      
      score += X_net.t() * (Y_net - prob_net);
      fisher += X_net.t() * arma::diagmat(var_net) * X_net;
    }
    // Rcout <<  fisher << std::endl;
    // Rcout <<  arma::sum(X_net, 0) << std::endl;
    // 
    // --- Component 2: Attribute 'x' ---
    if (fix_x == false) {
      if (attr_x_family == "binomial") {
        arma::vec eta_x = X_x * coef;
        arma::vec exp_eta_x = arma::exp(eta_x);
        arma::vec prob_x = exp_eta_x / (1.0 + exp_eta_x);
        arma::vec var_x = prob_x % (1.0 - prob_x);
        
        score += X_x.t() * (Y_x - prob_x);
        fisher += X_x.t() * arma::diagmat(var_x) * X_x;
        
      } else if (attr_x_family == "poisson") {
        arma::vec eta_x = X_x * coef;
        arma::vec mu_x = arma::exp(eta_x);
        
        score += X_x.t() * (Y_x - mu_x);
        fisher += X_x.t() * arma::diagmat(mu_x) * X_x;
        
      } else if (attr_x_family == "normal") {
        arma::vec mu_x = X_x * coef; 
        score += X_x.t() * (Y_x - mu_x) / attr_x_scale;
        fisher += (X_x.t() * X_x) / attr_x_scale;
      }
    }
    
    // --- Component 3: Attribute 'y' ---
    if (attr_y_family == "binomial") {
      arma::vec eta_y = X_y * coef;
      arma::vec exp_eta_y = arma::exp(eta_y);
      arma::vec prob_y = exp_eta_y / (1.0 + exp_eta_y);
      arma::vec var_y = prob_y % (1.0 - prob_y);
      
      score += X_y.t() * (Y_y - prob_y);
      fisher += X_y.t() * arma::diagmat(var_y) * X_y;
      
    } else if (attr_y_family == "poisson") {
      arma::vec eta_y = X_y * coef;
      arma::vec mu_y = arma::exp(eta_y);
      
      score += X_y.t() * (Y_y - mu_y);
      fisher += X_y.t() * arma::diagmat(mu_y) * X_y;
      
    } else if (attr_y_family == "normal") {
      arma::vec mu_y = X_y * coef;
      
      score += X_y.t() * (Y_y - mu_y) / attr_y_scale;
      fisher += (X_y.t() * X_y) / attr_y_scale;
    }
    // --- 4. Update and Check Convergence ---
    // Rcout <<  fisher << std::endl;
    // Rcout <<  arma::inv(fisher) << std::endl;
    coef += (arma::solve(fisher, score));
    // Rcout << coef <<  std::endl;
    // Rcout << coefs.n_rows <<  std::endl;
    // Rcout << k <<  std::endl;
    // Rcout << coefs.row(k) <<  std::endl;
    // Rcout << "Here" <<  std::endl;
    coefs.row(k) = coef.t();
    // Rcout << "Here" <<  std::endl;
    // Rcout << stand_in <<  std::endl;
    
    llhs.at(k-2) = calculate_llh(coef, 
            stand_in,
            i_vec,
            j_vec,
            overlap_vec,
            directed,
            pseudo_lh,
            offset_nonoverlap,
            attr_x_family,
            attr_y_family,
            attr_x_scale,
            attr_y_scale,
            n_units,
            fix_x, 
            fix_z,nonoverlap_random );
    // Rcout << "Here A" <<  std::endl;
    if (k == max_iteration) {
      non_converged = false;
    } else if ((arma::norm(coefs.row(k) - coefs.row(k - 1)) < tol) && !non_stop) {
      non_converged = false;
    }
    
    k++;
  }
  if(display_progress) {
    Rcpp::Rcout.flush();  
    Rcout << "Done with the estimation" << std::endl;
  }
  // coef.rows(ind_degrees) = coef_degrees;
  // coef.rows(ind_nondegrees) = coef_nondegrees;
  return(List::create(_["coefficients"] =coef,
                      _["coefficients_path"] =coefs.rows(2,k-1),
                      _["score"] =score,
                      _["where_wrong"] = where_wrong,
                      _["fisher"] = fisher,
                      _["var"] = arma::inv(fisher), 
                      _["llh"] = llhs.head(k-2)
  ));
}
// [[Rcpp::export]]
arma::mat invert_mat(double diag, double offdiag,int n_units){
  return(1/(diag-offdiag)*arma::mat(n_units,n_units, arma::fill::eye) - arma::mat(n_units, n_units, arma::fill::value(1/((1/offdiag + n_units/(diag-offdiag))*pow(diag-offdiag,2)))));
}

// [[Rcpp::export]]
arma::mat get_A_inv(double n_units){
  arma::mat A = (n_units-1)/4*arma::mat(n_units,n_units, arma::fill::eye);
  arma::mat B = arma::mat(n_units,n_units-1, arma::fill::value(0.25));
  B.diag() = arma::vec(n_units-1, arma::fill::zeros);
  
  arma::mat D = (n_units-1)/4*arma::mat(n_units-1,n_units-1, arma::fill::eye);
  arma::mat A_inv = 1/A.at(0,0)*arma::mat(n_units,n_units, arma::fill::eye);
  arma::mat D_inv = 1/D.at(0,0)*arma::mat(n_units-1,n_units-1, arma::fill::eye);
  arma::mat part_2 = arma::mat(n_units-1,n_units-1, arma::fill::value((n_units-2)/(n_units-1)*0.25));
  part_2.diag() = arma::vec(n_units-1, arma::fill::value(0.25));
  part_2 = D - part_2;
  arma::mat part_1 = arma::mat(n_units,n_units, arma::fill::value((n_units-3)/(n_units-1)*0.25));
  part_1.diag() = arma::vec(n_units, arma::fill::value((n_units-2)/(n_units-1)*0.25));
  part_1.row(n_units-1) = arma::vec(n_units, arma::fill::value((n_units-2)/(n_units-1)*0.25)).as_row();
  part_1.col(n_units-1) = arma::vec(n_units, arma::fill::value((n_units-2)/(n_units-1)*0.25)).as_col();
  part_1.at(n_units-1, n_units-1) = 0.25;
  part_1 = A - part_1;
  
  // arma::mat part_2 = D - B.t()*A_inv*B;
  // arma::mat part_1 = A - B*D_inv*B.t();
  
  arma::mat a = part_1.submat(0,0,n_units-2,n_units-2);
  arma::mat b = part_1.submat(n_units-1,0,n_units-1,n_units-2);
  arma::mat c = b.t();
  arma::mat d = arma::mat(1,1, arma::fill::zeros);
  d.at(0,0) = part_1.at(n_units-1,n_units-1);
  
  
  arma::mat Part_2 = d-b*invert_mat(a.at(1,1), a.at(0,1), a.n_rows)*b.t();
  arma::mat Part_2_invert = arma::mat(1,1, arma::fill::zeros);
  Part_2_invert.at(0,0) = 1/Part_2.at(0,0);
  arma::mat Part_1 = a-b.t()*b/d.at(0,0);
  arma::mat Part_1_invert = invert_mat(Part_1.at(1,1), Part_1.at(0,1), Part_1.n_rows);
  arma::mat Part_3 = -b*1/d.at(0,0);
  arma::mat Part_4 = -c.t()*invert_mat(a.at(1,1), a.at(0,1), a.n_rows);
  arma::mat inv_part_1 = arma::join_rows(arma::join_cols( Part_1_invert, (Part_1_invert*Part_3.t()).t()), 
                                         arma::join_cols( (Part_2_invert.at(0,0)*Part_4).t(), Part_2_invert));
  arma::mat inv_part_2 = invert_mat(part_2.at(1,1), part_2.at(0,1), part_2.n_rows);
  arma::mat part_3 = -arma::mat(n_units, n_units-1, arma::fill::value(1/(n_units-1)));
  part_3.diag() = arma::vec(n_units-1, arma::fill::zeros); 
  // arma::mat part_3 = -B*D_inv;
  // arma::mat tmp_mat = (inv_part_1* part_3); 
  // Rcout << arma::accu(part_3)<< std::endl;
  // Rcout << "Diagonal"<< std::endl;
  // Rcout << part_3.at(0,1)*(inv_part_1.at(1,2)*(n_units-2)+inv_part_1.at(1,n_units-1))<< std::endl;
  // Rcout << "Off-Diagonal"<< std::endl;
  // Rcout << part_3.at(0,1)*(inv_part_1.at(0,1)*(n_units-3)+
  //   inv_part_1.at(0,0)+inv_part_1.at(0,n_units-1))<< std::endl;
  // Rcout << "Last Row"<< std::endl;
  // Rcout << part_3.at(0,1)*(inv_part_1.at(0,n_units-1)*(n_units-2)+inv_part_1.at(n_units-1,n_units-1))<< std::endl;
  
  
  arma::mat tmp_mat = arma::mat(n_units, n_units-1,
                                arma::fill::value(part_3.at(0,1)*(inv_part_1.at(0,1)*(n_units-3)+
                                  inv_part_1.at(0,0)+inv_part_1.at(0,n_units-1)))); 
  tmp_mat.diag() = arma::vec(n_units-1, arma::fill::value(part_3.at(0,1)*(inv_part_1.at(1,2)*(n_units-2)+inv_part_1.at(1,n_units-1))));
  tmp_mat.row(n_units-1) = arma::vec(n_units-1,arma::fill::value(part_3.at(0,1)*(inv_part_1.at(0,n_units-1)*(n_units-2)+inv_part_1.at(n_units-1,n_units-1)))).as_row();
  // Rcout << tmp_mat<< std::endl;
  
  arma::mat res = arma::join_rows(arma::join_cols( inv_part_1, tmp_mat.t()), 
                                  arma::join_cols( tmp_mat, inv_part_2));
  // Rcout << arma::accu(tmp_mat)<< std::endl;
  res = arma::join_cols(res, arma::mat(1,2*n_units-1, arma::fill::zeros));
  res = arma::join_rows(res, arma::mat(2*n_units,1, arma::fill::zeros));
  return(res);
}

// [[Rcpp::export]]
List outerloop_estimation_pl(arma::vec coef,
                             arma::vec coef_degrees,
                             const arma::mat& z_network ,
                             const arma::vec& x_attribute ,
                             const arma::vec& y_attribute ,
                             const arma::mat& neighborhood,
                             const arma::mat& overlap,
                             bool directed,
                             std::vector<std::string> terms,
                             std::vector<arma::mat> &data_list,
                             std::vector<double> &type_list, 
                             bool display_progress, 
                             int max_iteration_outer, 
                             int max_iteration_inner_degrees, 
                             int max_iteration_inner_nondegrees, 
                             double tol, 
                             double offset_nonoverlap, 
                             bool non_stop, 
                             bool var, 
                             bool accelerated, 
                             bool fix_x, 
                             std::string family_x, 
                             std::string family_y, 
                             double attr_x_scale, 
                             double attr_y_scale, 
                             bool nonoverlap_random = true,
                             int start = 0) {
  List res; 
  std::tuple<arma::mat,arma::vec> pseudo_lh;
  int n_units = y_attribute.size();
  
  arma::mat id_mat = arma::mat((double) n_units,n_units,arma::fill::eye);
  arma::mat ones_mat = arma::mat(n_units,n_units,arma::fill::ones);
  arma::mat A_inv = 4/((double)n_units -2)*(id_mat - 1/(2*(double)n_units-2)*ones_mat);
  if(directed){
    A_inv = get_A_inv(n_units);
  }
  
  std::tuple<arma::vec, arma::vec,  arma::mat, arma::mat> res_degrees, res_nondegrees, res_nondegrees_alt;
  arma::uvec i_vec, j_vec,overlap_vec;
  arma::mat coefs_degrees; 
  //  coefs_nondegrees(terms.size()), coefs_degrees(n_units)
  if(directed){
    // network_vec = arma::vec(n_units*(n_units-1)); 
    i_vec = arma::uvec(n_units*(n_units-1)); 
    j_vec = arma::uvec(n_units*(n_units-1)); 
    overlap_vec = arma::uvec(n_units*(n_units-1)); 
    coef_degrees.reshape(n_units*2,1);
    coefs_degrees.reshape(0, n_units*2);  
  } else {
    // network_vec= arma::vec(n_units*(n_units-1)/2); 
    i_vec= arma::uvec(n_units*(n_units-1)/2); 
    j_vec= arma::uvec(n_units*(n_units-1)/2);
    overlap_vec= arma::uvec(n_units*(n_units-1)/2);
    coef_degrees.reshape(n_units,1);
    coefs_degrees.reshape(max_iteration_inner_degrees, n_units);
  }
  XYZ_class object(n_units,directed, x_attribute, y_attribute,z_network,neighborhood,overlap, family_x, family_y,attr_x_scale, attr_y_scale);
  // Rcout << "Start"<< std::endl;
  // Rcout << object.n_units<< std::endl;
  
  if(display_progress) {
    Rcout << "Starting with the preprocessing" << std::endl;
  }
  pseudo_lh = xyz_get_info_pl(object,terms,data_list,type_list, display_progress,
                              i_vec,j_vec,overlap_vec, n_units, fix_x, false);
  arma::uvec where_wrong = find(arma::sum(std::get<0>(pseudo_lh), 0) == 0);
  if(where_wrong.size() >0){
    Rcout << "Some statistics do not change over all pairs/units (they are excluded from the model since their MLE is negative infinity)" << std::endl;
    arma::uvec where_right = find(arma::var(std::get<0>(pseudo_lh), 0) != 0);
    std::get<0>(pseudo_lh) = std::get<0>(pseudo_lh).cols(where_right);
    coef = coef.rows(where_right);
  }
  arma::mat coefs(max_iteration_outer+1, coef.size() + coef_degrees.size()),
  fisher_degrees(terms.size(),terms.size()), fisher_nondegrees(terms.size(),terms.size()), 
  coefs_nondegrees(max_iteration_inner_nondegrees, terms.size());
  
  coefs.row(0)= arma::join_rows(coef.t(), coef_degrees.t());
  arma::vec score_degrees(n_units), 
  score_nondegrees(terms.size()),coef_nondegrees(terms.size());
  
  int k = 1;
  bool non_converged = true;
  bool first_it = true;
  
  coef_nondegrees = coef;
  arma::vec old_coef_pop, old_score_pop, llh(max_iteration_outer +1);
  arma::mat old_M;
  llh.at(0) = 0; 
  
  if(accelerated){
    old_M.reshape(coef_degrees.size(), coef_degrees.size());
    old_M.fill(arma::fill::zeros); 
    old_coef_pop.reshape(coef_degrees.size(),1);
    old_coef_pop.fill(arma::fill::zeros);
  }
  if(display_progress) {
    Rcout << "Starting with the estimation" << std::endl;
  }
  
  while(non_converged) {
    Rcpp::checkUserInterrupt();
    
    if(display_progress) {
      Rcpp::Rcout.flush();
      Rcout << "Iteration = " + std::to_string(k+start)  << "\r";
    }
    if(accelerated){
      res_degrees = cond_estimation_degrees_pl_accelerated(coef_degrees,
                                                           i_vec, 
                                                           j_vec,
                                                           overlap_vec,
                                                           directed,
                                                           pseudo_lh, 
                                                           max_iteration_inner_degrees, 
                                                           tol, 
                                                           coef_nondegrees, 
                                                           offset_nonoverlap, 
                                                           A_inv, 
                                                           non_stop, 
                                                           old_score_pop,
                                                           old_coef_pop, 
                                                           old_M,  k, first_it, nonoverlap_random);
      first_it = false;  
    } else {
      res_degrees = cond_estimation_degrees_pl(coef_degrees,
                                               i_vec, 
                                               j_vec,
                                               overlap_vec,
                                               directed,
                                               pseudo_lh, 
                                               max_iteration_inner_degrees, 
                                               tol, 
                                               coef_nondegrees, 
                                               offset_nonoverlap, 
                                               A_inv, k, non_stop, 
                                               nonoverlap_random);
      
    }
    coef_degrees = std::get<0>(res_degrees);
    // res_nondegrees_alt = cond_estimation_nondegrees_pl_old(coef_nondegrees, 
    //                                                      i_vec, 
    //                                                      j_vec,
    //                                                      overlap_vec,
    //                                                      directed,
    //                                                      pseudo_lh, 
    //                                                      max_iteration_inner_nondegrees, 
    //                                                      tol, 
    //                                                      coef_degrees, 
    //                                                      offset_nonoverlap, 
    //                                                      non_stop, 
    //                                                      family_x, family_y,  
    //                                                      attr_x_scale, attr_y_scale, fix_x);
    // Rcout << std::get<0>(res_nondegrees_alt)<< std::endl;
    if (terms.size() > 0) {
      res_nondegrees = cond_estimation_nondegrees_pl(coef_nondegrees, 
                                                     i_vec, 
                                                     j_vec,
                                                     overlap_vec,
                                                     directed,
                                                     pseudo_lh, 
                                                     max_iteration_inner_nondegrees, 
                                                     tol, 
                                                     coef_degrees, 
                                                     offset_nonoverlap, 
                                                     non_stop, 
                                                     family_x, family_y, 
                                                     attr_x_scale, attr_y_scale, fix_x);
      coef_nondegrees = std::get<0>(res_nondegrees);
    }
    llh.at(k) = calculate_llh(coef_nondegrees, 
           coef_degrees,
           i_vec,
           j_vec,
           overlap_vec,
           directed,
           pseudo_lh,
           offset_nonoverlap,
           family_x,
           family_y,
           attr_x_scale,
           attr_y_scale,
           n_units,
           fix_x, false, nonoverlap_random);
    coefs.row(k) = join_cols(coef_nondegrees, coef_degrees).t();
    if(k == max_iteration_outer){
      non_converged = false;
    } else if ((arma::max(arma::vec{arma::norm(coefs.row(k)- coefs.row(k-1), 2), 
                          std::abs((llh.at(k) -llh.at(k-1)) / (std::abs(llh.at(k)) > 1e-10 ? llh.at(k) : 1.0))})<tol) && !non_stop){
      non_converged = false;
    }
    k++;
  }
  if(display_progress) {
    Rcpp::Rcout.flush();  
    Rcout << "Done with the estimation" << std::endl;
  }
  
  if (terms.size() > 0) {
    std::tie(coef_nondegrees,score_nondegrees,fisher_nondegrees,coefs_nondegrees) = res_nondegrees;
  }
  std::tie(coef_degrees,score_degrees,fisher_degrees,coefs_degrees) = res_degrees;
  
  if(var){
    std::tuple<arma::vec,  arma::mat> res_mat = get_B_pl(i_vec, 
                                                         j_vec,overlap_vec,
                                                         pseudo_lh, 
                                                         coef_degrees, 
                                                         coef_nondegrees, offset_nonoverlap, 
                                                         object.z_network.directed,object.n_units );
    arma::mat exact_A = get_A_exact(i_vec, 
                                    j_vec,overlap_vec,
                                    pseudo_lh, 
                                    coef_degrees, 
                                    coef_nondegrees, offset_nonoverlap, 
                                    object.z_network.directed,object.n_units );
    arma::mat B_mat;
    arma::vec A_diag;
    std::tie(A_diag,B_mat) = res_mat;
    // coefs(ind_degrees) = coef_degrees;
    // coef.rows(ind_nondegrees) = coef_nondegrees;
    if(accelerated){
      return(List::create(_["coefficients_nondegrees"] =coef_nondegrees,
                          _["coefficients_degrees"] =coef_degrees,
                          _["coefficients_path"] =coefs.rows(0,k-1),
                          _["score_degrees"] =score_degrees,
                          _["score_nondegrees"] =score_nondegrees,
                          _["fisher_degrees"] = fisher_degrees, 
                          _["fisher_nondegrees"] = fisher_nondegrees, 
                          _["A_diag"] = A_diag, 
                          _["M"] = old_M, 
                          _["A_approx"] = A_inv, 
                          _["exact_A"] =exact_A,
                          _["B_mat"] = B_mat, 
                          _["llh"] = llh.rows(1,k-1), 
                          _["where_wrong"] = where_wrong));
    } else {
      return(List::create(_["coefficients_nondegrees"] =coef_nondegrees,
                          _["coefficients_degrees"] =coef_degrees,
                          _["coefficients_path"] =coefs.rows(0,k-1),
                          _["score_degrees"] =score_degrees,
                          _["score_nondegrees"] =score_nondegrees,
                          _["fisher_degrees"] = fisher_degrees, 
                          _["fisher_nondegrees"] = fisher_nondegrees, 
                          _["A_diag"] = A_diag, 
                          _["B_mat"] = B_mat, 
                          _["llh"] = llh.rows(1,k-1), 
                          _["where_wrong"] = where_wrong));
    }
  } else {
    // coefs(ind_degrees) = coef_degrees;
    // coef.rows(ind_nondegrees) = coef_nondegrees;
    return(List::create(_["coefficients_nondegrees"] =coef_nondegrees,
                        _["coefficients_degrees"] =coef_degrees,
                        _["coefficients_path"] =coefs.rows(0,k-1),
                        _["score_degrees"] =score_degrees,
                        _["score_nondegrees"] =score_nondegrees,
                        _["fisher_degrees"] = fisher_degrees, 
                        _["fisher_nondegrees"] = fisher_nondegrees, 
                        _["A_inv"] = A_inv,
                        _["llh"] = llh.rows(1,k-1), 
                        _["where_wrong"] = where_wrong
    ));
  }
  
  
  
}



// std::vector<arma::mat> xyz_prepare_composite_estimation_internal_approx(XYZ_class object,
//                                                                         std::vector<std::string> terms,
//                                                                         std::vector<arma::mat> &data_list,
//                                                                         std::vector<double> &type_list, 
//                                                                         bool add_info, double prob, int seed) {
//   // Set up objects
//   int n_units = object.n_units;
//   bool is_full_neighborhood = object.check_if_full_neighborhood();
//   
//   std::vector<xyz_ValidateFunction> functions;
//   functions = xyz_change_statistics_generate(terms);
//   std::string z = "z", x = "x", y = "y";
//   arma::vec change_stat_x_i(functions.size()),
//   change_stat_x_j(functions.size()), change_stat_y_i(functions.size()),
//   change_stat_y_j(functions.size()), change_stat_z_ij(functions.size());
//   bool x_i, x_j, z_ij, y_i,y_j;
//   std::vector<arma::mat> res(n_units*(n_units-1)/2);
//   // int now = 0;
//   set_seed(seed);
//   arma::vec change_stat;
//   NumericVector random_accept= runif(n_units*(n_units-1),0,1);
//   // Number of trials
//   int n_trials = 0; 
//   // Number of success
//   int n_success = 0; 
//   
//   for(int i: seq(1,n_units-1)){
//     for(int j: seq(i+1,n_units)){
//       if(random_accept.at(n_trials)>prob){
//         n_trials++;
//         continue;
//       }
//       n_trials++;
//       
//       // Get present values of x_ij, y_i, y_j
//       x_i = object.x_attribute.get_val(i);
//       x_j = object.x_attribute.get_val(j);
//       y_i = object.y_attribute.get_val(i);
//       y_j = object.y_attribute.get_val(j);
//       z_ij = object.z_network.get_val(i,j);
//       
//       if(add_info) {
//         res.at(n_success) = get_all_combinations_xyz(i,j,x_i, x_j,y_i,y_j,z_ij,
//                n_success,  object,functions, data_list, type_list, is_full_neighborhood);
//       } else { 
//         res.at(n_success) = get_all_combinations_xyz(i,j,x_i, x_j,y_i,y_j,z_ij,
//                n_success,  object,functions, data_list, type_list, is_full_neighborhood).submat(0,8,31,7+functions.size());
//       } 
//       // offset_new.at(n_success) = offset.at()
//       n_success ++;
//     } 
//   }
//   return(res);
// } 

// arma::vec calculate_score_approx(XYZ_class object,
//                                  arma::vec coef,
//                                  std::vector<std::string> terms,
//                                  std::vector<arma::mat> &data_list,
//                                  std::vector<double> &type_list, 
//                                  double prob, int seed) {
//   double w_tmp;
//   std::vector<arma::mat> composite_lh;
//   // Rcout << "A"<< std::endl;
//   
//   composite_lh = xyz_prepare_composite_estimation_internal_approx(object,
//                                                                   terms,
//                                                                   data_list,
//                                                                   type_list,
//                                                                   false, prob, seed);
//   int n_coef = terms.size();
//   arma::vec score(n_coef), score_tmp(n_coef), m(n_coef), exp_tmp(n_coef);
//   // // Rcout << "Iteration = " + std::to_string(k)  << std::endl;
//   // Update the score and fisher info
//   int end = 0;
//   for(unsigned int i = 0; i < composite_lh.size(); i++){
//     if(composite_lh.at(i).size()== 0) {
//       continue;
//     }  
//     end ++;
//     // Rcout << "Iteration = " + std::to_string(i - composite_lh.size())  << std::endl;
//     exp_tmp = arma::exp(composite_lh.at(i)*coef);
//     // Rcout << exp_tmp  << std::endl;
//     // Rcout << "B"  << std::endl;
//     w_tmp = sum(exp_tmp);
//     // Rcout << w_tmp  << std::endl;
//     // Rcout << "C"  << std::endl;
//     m = arma::sum(composite_lh.at(i).t()*exp_tmp, 1);
//     // Rcout << m  << std::endl;
//     // Rcout << "D"  << std::endl;
//     score_tmp = m/w_tmp;
//     // Rcout << score_tmp  << std::endl;
//     // Rcout << "E"  << std::endl;
//     score -= score_tmp;
//   }
//   // Rcout << composite_lh.size()  << std::endl;
//   // Rcout << end  << std::endl;
//   score = score*composite_lh.size()/end;
//   return(score);
// }


arma::vec calculate_score_pl(XYZ_class & object,
                             arma::vec coef,
                             std::vector<std::string> terms,
                             std::vector<arma::mat> &data_list,
                             std::vector<double> &type_list, 
                             double &offset_nonoverlap, 
                             bool fix_x, 
                             bool fix_z, 
                             std::string attr_x_family, 
                             std::string attr_y_family, 
                             double attr_x_scale, 
                             double attr_y_scale, 
                             bool nonoverlap_random) {
  // double w_tmp;
  arma::uvec i_vec, j_vec,overlap_vec;
  if(object.z_network.directed){
    // network_vec = arma::vec(n_units*(n_units-1)); 
    i_vec = arma::uvec(object.n_units*(object.n_units-1)); 
    j_vec = arma::uvec(object.n_units*(object.n_units-1)); 
    overlap_vec = arma::uvec(object.n_units*(object.n_units-1)); 
  } else { 
    // network_vec= arma::vec(n_units*(n_units-1)/2); 
    i_vec= arma::uvec(object.n_units*(object.n_units-1)/2); 
    j_vec= arma::uvec(object.n_units*(object.n_units-1)/2); 
    overlap_vec= arma::uvec(object.n_units*(object.n_units-1)/2); 
  } 
  
  std::tuple<arma::mat, arma::vec> pseudo_lh = xyz_get_info_pl(object,terms,data_list,
                                                               type_list, false,
                                                               i_vec,j_vec, overlap_vec,
                                                               object.n_units, fix_x, fix_z);
  
  int n_units = object.n_units, n_coef = coef.size();
  unsigned int n_net = i_vec.n_elem*(!fix_z);
  
  const arma::mat& X_all = std::get<0>(pseudo_lh);
  const arma::vec& Y_all = std::get<1>(pseudo_lh);
  
  // --- 1. Define subviews for each component ---
  // Network component is always present
  
  
  arma::mat X_x, X_y, X_net;
  arma::vec Y_x, Y_y, Y_net;
  if(!fix_z){
    X_net = X_all.rows(0, n_net - 1);
    Y_net = Y_all.subvec(0, n_net - 1);
  }
  
  
  if (fix_x == false) {
    X_x = X_all.rows(n_net, n_net + n_units - 1);
    Y_x = Y_all.subvec(n_net, n_net + n_units - 1);
    
    X_y = X_all.rows(n_net + n_units, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net + n_units, Y_all.n_elem - 1);
  } else {
    X_y = X_all.rows(n_net, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net, Y_all.n_elem - 1);
  }
  arma::vec score(n_coef);
  
  if(!fix_z){
    // Pre-calculate network offsets
    const arma::vec net_offsets = (1.0 - arma::conv_to<arma::vec>::from(overlap_vec)) * offset_nonoverlap;
    // --- 2. Initialize estimation variables ---
    score.zeros();
    
    // --- Component 1: Network (Logistic Model) ---
    arma::vec eta_net = X_net * coef + net_offsets;
    
    arma::vec exp_eta_net = arma::exp(eta_net);
    arma::vec prob_net = exp_eta_net / (1.0 + exp_eta_net);
    if(!nonoverlap_random){
      prob_net = overlap_vec % prob_net;
    }
    arma::vec var_net = prob_net % (1.0 - prob_net);
    
    score += X_net.t() * (Y_net - prob_net);
  }
  // --- Component 2: Attribute 'x' ---
  if (fix_x == false) {
    if (attr_x_family == "binomial") {
      arma::vec eta_x = X_x * coef;
      arma::vec exp_eta_x = arma::exp(eta_x);
      arma::vec prob_x = exp_eta_x / (1.0 + exp_eta_x);
      arma::vec var_x = prob_x % (1.0 - prob_x);
      score += X_x.t() * (Y_x - prob_x);
    } else if (attr_x_family == "poisson") {
      arma::vec eta_x = X_x * coef;
      arma::vec mu_x = arma::exp(eta_x);
      score += X_x.t() * (Y_x - mu_x);
    } else if (attr_x_family == "normal") {
      arma::vec mu_x = X_x * coef; 
      score += X_x.t() * (Y_x - mu_x) / attr_x_scale;
    }
  }
  
  // --- Component 3: Attribute 'y' ---
  if (attr_y_family == "binomial") {
    arma::vec eta_y = X_y * coef;
    arma::vec exp_eta_y = arma::exp(eta_y);
    arma::vec prob_y = exp_eta_y / (1.0 + exp_eta_y);
    arma::vec var_y = prob_y % (1.0 - prob_y);
    score += X_y.t() * (Y_y - prob_y);
  } else if (attr_y_family == "poisson") {
    arma::vec eta_y = X_y * coef;
    arma::vec mu_y = arma::exp(eta_y);
    score += X_y.t() * (Y_y - mu_y);
  } else if (attr_y_family == "normal") {
    arma::vec mu_y = X_y * coef;
    score += X_y.t() * (Y_y - mu_y) / attr_y_scale;
  }
  return(score);
}

arma::vec calculate_score_pl_degrees(XYZ_class & object,
                                     arma::vec coef_nondegrees,
                                     arma::vec coef_degrees,
                                     std::vector<std::string> terms,
                                     std::vector<arma::mat> &data_list,
                                     std::vector<double> &type_list, 
                                     double &offset_nonoverlap, 
                                     bool fix_x, 
                                     bool updated_uncertainty,
                                     bool exact, 
                                     std::string attr_x_family, 
                                     std::string attr_y_family, 
                                     double attr_x_scale, 
                                     double attr_y_scale, 
                                     bool nonoverlap_random) {
  // double w_tmp;
  arma::uvec i_vec, j_vec,overlap_vec;
  if(object.z_network.directed){
    i_vec = arma::uvec(object.n_units*(object.n_units-1)); 
    j_vec = arma::uvec(object.n_units*(object.n_units-1)); 
    overlap_vec = arma::uvec(object.n_units*(object.n_units-1)); 
  } else {  
    i_vec= arma::uvec(object.n_units*(object.n_units-1)/2); 
    j_vec= arma::uvec(object.n_units*(object.n_units-1)/2); 
    overlap_vec= arma::uvec(object.n_units*(object.n_units-1)/2); 
  }  
  std::tuple<arma::mat, arma::vec> pseudo_lh = xyz_get_info_pl(object,terms,data_list,
                                                               type_list, false,
                                                               i_vec,j_vec, overlap_vec,
                                                               object.n_units, fix_x, false);
  
  
  int n_coef = coef_nondegrees.size();
  unsigned int n_net = i_vec.n_elem;
  
  // Extract the full design matrix and response vector
  const arma::mat& X_all = std::get<0>(pseudo_lh);
  const arma::vec& Y_all = std::get<1>(pseudo_lh);
  
  // --- 1. Define subviews for each component ---
  // Network component is always present
  const arma::mat X_net = X_all.rows(0, n_net - 1);
  const arma::vec Y_net = Y_all.subvec(0, n_net - 1);
  arma::mat X_x, X_y;
  arma::vec Y_x, Y_y;
  
  if (fix_x == false) {
    X_x = X_all.rows(n_net, n_net + object.n_units - 1);
    Y_x = Y_all.subvec(n_net, n_net + object.n_units - 1);
    
    X_y = X_all.rows(n_net + object.n_units, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net + object.n_units, Y_all.n_elem - 1);
  } else {
    X_y = X_all.rows(n_net, X_all.n_rows - 1);
    Y_y = Y_all.subvec(n_net, Y_all.n_elem - 1);
  }
  
  // Pre-calculate network offsets
  const arma::vec net_offsets = (1.0 - arma::conv_to<arma::vec>::from(overlap_vec)) * offset_nonoverlap;
  
  // Pre-calculate degrees indices
  arma::uvec j_pop_indices; 
  if(object.z_network.directed){
    j_pop_indices = j_vec - 1 + object.n_units ;
  } else {
    j_pop_indices =j_vec - 1;
  }
  const arma::uvec i_pop_indices = i_vec - 1;
  
  // --- 2. Initialize estimation variables ---
  arma::vec score_nondegrees(n_coef, arma::fill::zeros);
  
  // --- Component 1: Network (Logistic Model) ---
  arma::vec eta_net = X_net * coef_nondegrees + net_offsets + 
    coef_degrees.elem(i_pop_indices) + 
    coef_degrees.elem(j_pop_indices);
  
  arma::vec exp_eta_net = arma::exp(eta_net);
  arma::vec prob_net = exp_eta_net / (1.0 + exp_eta_net);
  if(!nonoverlap_random){
    prob_net = overlap_vec % prob_net;
  }
  arma::vec var_net = prob_net % (1.0 - prob_net);
  
  score_nondegrees += X_net.t() * (Y_net - prob_net);
  // --- Component 2: Attribute 'x' ---
  if (fix_x == false) {
    if (attr_x_family == "binomial") {
      arma::vec eta_x = X_x * coef_nondegrees;
      arma::vec exp_eta_x = arma::exp(eta_x);
      arma::vec prob_x = exp_eta_x / (1.0 + exp_eta_x);
      arma::vec var_x = prob_x % (1.0 - prob_x);
      
      score_nondegrees += X_x.t() * (Y_x - prob_x);
    } else if (attr_x_family == "poisson") {
      arma::vec eta_x = X_x * coef_nondegrees;
      arma::vec mu_x = arma::exp(eta_x);
      score_nondegrees += X_x.t() * (Y_x - mu_x);
    } else if (attr_x_family == "normal") {
      arma::vec mu_x = X_x * coef_nondegrees; 
      score_nondegrees += X_x.t() * (Y_x - mu_x) / attr_x_scale;
    }
  }
  
  // --- Component 3: Attribute 'y' ---
  if (attr_y_family == "binomial") {
    arma::vec eta_y = X_y * coef_nondegrees;
    arma::vec exp_eta_y = arma::exp(eta_y);
    arma::vec prob_y = exp_eta_y / (1.0 + exp_eta_y);
    arma::vec var_y = prob_y % (1.0 - prob_y);
    
    score_nondegrees += X_y.t() * (Y_y - prob_y);
  } else if (attr_y_family == "poisson") {
    arma::vec eta_y = X_y * coef_nondegrees;
    arma::vec mu_y = arma::exp(eta_y);
    
    score_nondegrees += X_y.t() * (Y_y - mu_y);
  } else if (attr_y_family == "normal") {
    arma::vec mu_y = X_y * coef_nondegrees;
    score_nondegrees += X_y.t() * (Y_y - mu_y) / attr_y_scale;
  }
  
  arma::vec score_degrees(coef_degrees.size(), arma::fill::zeros);
  for(unsigned int i = 0; i < i_vec.size(); i++){
    score_degrees.at(i_vec.at(i)-1) +=  std::get<1>(pseudo_lh).at(i) - prob_net.at(i);
    if(object.z_network.directed){
      score_degrees.at(j_vec.at(i)-1+ object.n_units) +=  std::get<1>(pseudo_lh).at(i) - prob_net.at(i);
    } else {
      score_degrees.at(j_vec.at(i)-1) +=  std::get<1>(pseudo_lh).at(i) - prob_net.at(i);  
    }
  }  
  arma::vec res_vec;
  if(updated_uncertainty){
    // arma::mat C = get_C(coef_nondegrees,i_vec,j_vec,overlap_vec,
    //                        object.z_network.directed,
    //                        pseudo_lh,coef_degrees,
    //                        offset_nonoverlap, 
    //                        attr_x_family, 
    //                        attr_y_family, 
    //                        attr_x_scale, 
    //                        attr_y_scale);
    arma::mat C = get_C_new(coef_nondegrees,i_vec,j_vec,overlap_vec,
                            object.z_network.directed,
                            pseudo_lh,coef_degrees, 
                            offset_nonoverlap, 
                            fix_x,
                            attr_x_family, 
                            attr_y_family, 
                            attr_x_scale, 
                            attr_y_scale);
    arma::mat A = get_A_exact(i_vec, j_vec,overlap_vec,pseudo_lh, 
                              coef_degrees, 
                              coef_nondegrees, 
                              offset_nonoverlap,
                              object.z_network.directed, 
                              object.n_units);
    // clock.tock("A");
    // Rcpp::Rcout << "Calculating A and B matrices for variance estimation" << std::endl;
    // clock.tick("B");
    arma::mat B = get_B(i_vec, j_vec,overlap_vec,
                        pseudo_lh, coef_degrees,coef_nondegrees, offset_nonoverlap,
                        object.z_network.directed,
                        object.n_units).t();
    
    arma::mat X;
    // clock.tick("solve");
    if(exact){
      X = arma::pinv(A) * B;
      // Rcout << "Using exact inversion for A" << std::endl;
      arma::mat residual = A * X - B;
      
    } else {
      // Step 1: Inverse of diagonal A (robust version with epsilon)
      arma::vec ainv = 1.0 / (A.diag() + 1e-12);  // Elementwise inverse with regularizer
      // Step 2: Compute X = A^{-1} B using column-wise scaling
      X = B.each_col() % ainv;
    }
    // Compute Schur complement
    arma::mat S = C - B.t() * X;
    // Invert S
    // Rcout << "Inversion for S" << std::endl;
    // Rcout << C << std::endl;
    // Rcout << C_new << std::endl;
    // Rcout << X << std::endl;
    // Rcout << B << std::endl;
    arma::mat S_inv = arma::pinv(S);
    
    // Compute inverse blocks
    arma::mat M22_inv = S_inv;
    arma::mat M12_inv = -X * S_inv;
    if (n_coef > 0) {
      res_vec = join_cols(M12_inv.t()*score_degrees + M22_inv*score_nondegrees, score_degrees); 
    } else {
      res_vec = score_degrees;
    }
  } else {
    if (n_coef > 0) {
      res_vec = join_cols(score_nondegrees, score_degrees); 
    } else {
      res_vec = score_degrees;
    }
  }
  return(res_vec);
} 

// [[Rcpp::export]]
List xyz_approximate_variability(arma::vec& coef,
                                 arma::vec& coef_degrees,
                                 std::vector<std::string>& terms,
                                 int& n_units,
                                 arma::mat z_network,
                                 arma::mat neighborhood,
                                 arma::mat overlap,
                                 arma::vec y_attribute,
                                 arma::vec x_attribute,
                                 bool init_empty,
                                 bool directed,
                                 std::vector<arma::mat>& data_list,
                                 std::vector<double>& type_list,
                                 int n_proposals_x,
                                 int n_proposals_y,
                                 int n_proposals_z,
                                 int seed,
                                 int n_burn_in,
                                 int n_simulation,
                                 bool display_progress,
                                 bool degrees,
                                 double offset_nonoverlap, 
                                 bool return_samples, 
                                 bool fix_x, 
                                 bool fix_z, 
                                 bool updated_uncertainty, 
                                 bool exact, 
                                 std::string family_x, 
                                 std::string family_y, 
                                 double attr_x_scale, 
                                 double attr_y_scale, 
                                 bool nonoverlap_random,
                                 bool tnt = true){
  // Generate the class with the provided information
  XYZ_class object(n_units,directed, neighborhood, overlap, family_x, family_y,attr_x_scale, attr_y_scale);
  if(!init_empty){
    // Rcout << "Here" << std::endl;
    object.set_info_arma(x_attribute,y_attribute, z_network);
  } else{
    if(fix_x){
      object.x_attribute.set_attr_from_armavec(x_attribute);  
    }
    if(fix_z){
      // Rcout << "Setting initial empty network" << std::endl;
      object.set_network_from_mat(n_units, directed, z_network);  
    } else if(!nonoverlap_random){
      for(size_t r = 0; r < z_network.n_rows; ++r){
        int u = static_cast<int>(z_network(r, 0));
        int v = static_cast<int>(z_network(r, 1));
        if(!object.get_val_overlap(u, v)){
          object.add_edge(u, v);
        }
      }
    }
  }
  bool is_full_neighborhood = object.check_if_full_neighborhood();
  // Generate change statistic function from the terms
  std::vector<xyz_ValidateFunction> functions;
  
  functions = xyz_change_statistics_generate_new(terms);
  arma::vec global_stats = xyz_count_global_internal( object,
                                                      terms,
                                                      n_units,
                                                      data_list,
                                                      type_list, 
                                                      family_x, family_y, 
                                                      attr_x_scale, 
                                                      attr_y_scale);
  // Matrix of statistics that hold the simulated statistics
  // (we should probably return those as well, since they might be useful later on)
  arma::mat stats(n_simulation,functions.size());
  // The statistics are filled with zeroes at first
  stats.fill(0);
  // We also need to evaluate the gradients of the composite lh for each simulation
  // These are saved in the matrix "gradients"
  int size_gradient;
  if(degrees){
    size_gradient = coef.size() + coef_degrees.size();
  } else {
    size_gradient = coef.size();
  }
  arma::mat gradients(n_simulation,size_gradient);
  arma::mat current_hession(size_gradient, size_gradient);
  
  gradients.fill(0);
  
  // arma::mat gradients_approx(n_simulation,functions.size());
  // gradients_approx.fill(0);
  
  // Progress p(n_simulation + n_burn_in, display_progress);
  // We generate vectors for the exact values of all simulates of xyz objects
  // (since x and y are attributes, they are represented as sets of integers,
  //  z is a networks and hence an unordered_map of sets)
  std::vector<arma::vec> res_x(n_simulation);
  std::vector<arma::vec> res_y(n_simulation);
  std::vector<std::vector<std::vector<int>>> res_z(n_simulation);
  
  arma::vec z_tmp(32); 
  z_tmp.zeros();
  for(int i: seq(0,31)){
    if (i % 2 == 0)
      z_tmp.at(i) = 1;
  }
  // arma::vec gradient_tmp;
  if (seed != NA_INTEGER) {
    Rcpp::Function set_seed_r("set.seed");
    set_seed_r(seed);
  }
  for(int i = 1; i <=(n_simulation+n_burn_in);i ++) {
    Rcpp::checkUserInterrupt();
    // Rcout << "Updated Global Statistics: " << global_stats.t() << std::endl;
    if(display_progress) {
      Rcpp::Rcout.flush();
      Rcout << "Sample: " + std::to_string(i)  << "\r";
    }
    
    if(!fix_x){
      // Sample X| Y,Z
      xyz_simulate_attribute_mh(coef,object,
                                n_proposals_x,
                                data_list,
                                type_list,
                                is_full_neighborhood,
                                functions,
                                global_stats, "x");  
    }
    // Sample Y| X,Z
    xyz_simulate_attribute_mh(coef,object,
                              n_proposals_y,
                              data_list, type_list,
                              is_full_neighborhood, functions,
                              global_stats, "y");
    // Sample Z|X,Y
    if(!fix_z){
      if(degrees){
        xyz_simulate_network_mh_degrees(coef,
                                        coef_degrees,
                                        object,
                                        n_proposals_z,
                                        data_list, type_list,
                                        is_full_neighborhood, functions,
                                        global_stats, tnt); 
      } else {
        xyz_simulate_network_mh(coef,object,
                                n_proposals_z,
                                data_list, type_list,
                                is_full_neighborhood, functions,
                                global_stats,  tnt);  
      }
      
      if(nonoverlap_random){
        if(degrees){
          if(object.z_network.directed) {
            xyz_simulate_network_consecutive_degrees_mh_directed(coef,
                                                        coef_degrees,object,
                                                        data_list, type_list,
                                                        is_full_neighborhood, functions,
                                                        global_stats, offset_nonoverlap);
          } else {
            xyz_simulate_network_consecutive_degrees_mh(coef,
                                                        coef_degrees,object,
                                                        data_list, type_list,
                                                        is_full_neighborhood, functions,
                                                        global_stats, offset_nonoverlap);
          }
        } else {
          if(object.z_network.directed) {
            xyz_simulate_network_consecutive_mh_directed(coef,object,
                                                data_list, type_list,
                                                is_full_neighborhood, functions,
                                                global_stats, offset_nonoverlap);
          } else {
            xyz_simulate_network_consecutive_mh(coef,object,
                                                data_list, type_list,
                                                is_full_neighborhood, functions,
                                                global_stats, offset_nonoverlap);
          }
        }
      }
    }
    
    
    if(i>n_burn_in){
      if(degrees){
        gradients.row(i - n_burn_in-1) = calculate_score_pl_degrees(object,
                      coef,
                      coef_degrees,
                      terms,
                      data_list,
                      type_list,
                      offset_nonoverlap, fix_x, updated_uncertainty, exact, 
                      family_x, 
                      family_y, 
                      attr_x_scale, 
                      attr_y_scale, 
                      nonoverlap_random).as_row();
      } else {
        gradients.row(i - n_burn_in-1) = calculate_score_pl(object,
                      coef,
                      terms,
                      data_list,
                      type_list,
                      offset_nonoverlap, fix_x, fix_z, 
                      family_x, 
                      family_y, 
                      attr_x_scale, 
                      attr_y_scale, 
                      nonoverlap_random).t();
      }
      if(return_samples){
        res_x.at(i - n_burn_in-1) =object.x_attribute.attribute;
        res_z.at(i - n_burn_in-1) =object.z_network.adj_list;  
        res_y.at(i - n_burn_in-1) =object.y_attribute.attribute;  
      }
      // Count global statistics
      stats.row(i - n_burn_in-1) = global_stats.as_row();
      // n ++;
    }
    
  }
  if(degrees){
    int n_deg_terms = directed ? (n_units * 2) : n_units;
    arma::uvec ind_degrees = arma::regspace<arma::uvec>(terms.size(), terms.size() + n_deg_terms - 1);
    
    arma::mat gradients_degrees = gradients.cols(ind_degrees);
    arma::mat gradients_nondegrees;
    if (terms.size() > 0) {
      arma::uvec ind_nondegrees = arma::regspace<arma::uvec>(0, terms.size() - 1);
      gradients_nondegrees = gradients.cols(ind_nondegrees);
    } else {
      gradients_nondegrees = arma::mat(gradients.n_rows, 0);
    }
    if(return_samples){
      return(List::create(_["simulation_x_attributes"] =res_x,
                          _["simulation_y_attributes"] =res_y,
                          _["simulation_z_networks"] =res_z,
                          _["stats"] = stats, _["gradients"] = gradients, 
                          _["gradients_nondegrees"] = gradients_nondegrees,
                          _["gradients_degrees"] = gradients_degrees));  
    } else {
      return(List::create(_["stats"] = stats, _["gradients"] = gradients, 
                          _["gradients_nondegrees"] = gradients_nondegrees,
                          _["gradients_degrees"] = gradients_degrees));
    }
    
  } else{
    if(return_samples){
      return(List::create(_["simulation_x_attributes"] =res_x,
                          _["simulation_y_attributes"] =res_y,
                          _["simulation_z_networks"] =res_z,
                          _["stats"] = stats, _["gradients"] = gradients));
    }else {
      return(List::create(_["stats"] = stats, _["gradients"] = gradients));
    }
    
  }
  
}


// [[Rcpp::export]]
Rcpp::List xyz_prepare_pseudo_estimation(const arma::mat& z_network,
                                         const arma::vec& x_attribute,
                                         const arma::vec& y_attribute ,
                                         const arma::mat& neighborhood,
                                         const arma::mat& overlap,
                                         bool directed,
                                         std::vector<std::string> terms,
                                         std::vector<arma::mat> &data_list,
                                         std::vector<double> &type_list, 
                                         bool display_progress, 
                                         std::string family_x,
                                         std::string family_y,
                                         double attr_x_scale,
                                         double attr_y_scale,
                                         bool return_x = false,
                                         bool return_y = false,
                                         bool return_z = false) {
  // Set up objects
  Rcpp::List res, res_x, res_y, res_z;
  int n_units = y_attribute.size();
  // Rcout << "Read Data" << std::endl;
  XYZ_class object(n_units,directed, x_attribute, y_attribute,z_network,neighborhood,overlap, family_x, family_y,attr_x_scale, attr_y_scale);
  // Check whether its a fully observed neighbhorhood (this means that everyone knows everyone)
  // This is provided to the sufficient statistics as this might make some calculations unnecessary
  bool is_full_neighborhood = object.check_if_full_neighborhood();
  // Generate vector of functions that calculate the sufficient statistics
  std::vector<xyz_ValidateFunction> functions;
  functions = xyz_change_statistics_generate_new(terms);
  // strings z, x, and y later needed to tell the sufficient statistics
  // what type of change statistic is wanted
  std::string z = "z", x = "x", y = "y";
  arma::vec i_vec, j_vec,overlap_vec;
  if(directed){
    i_vec = arma::vec(n_units*(n_units-1)); 
    j_vec = arma::vec(n_units*(n_units-1)); 
    overlap_vec = arma::vec(n_units*(n_units-1)); 
  } else {  
    i_vec= arma::vec(n_units*(n_units-1)/2); 
    j_vec= arma::vec(n_units*(n_units-1)/2); 
    overlap_vec = arma::vec(n_units*(n_units-1)/2); 
  } 
  
  // Just a temporary vector of the change statistics for one dyad of the network
  arma::vec change_stat_network(functions.size()),
  // The same thing but for the attributes i and j
  change_stat_attribute_i(functions.size());
  double x_i, y_i, z_ij;
  arma::mat res_covs, res_target;
  if(directed){
    res_covs = arma::mat(n_units*(n_units-1) + n_units*2,terms.size());
    res_target = arma::mat(n_units*(n_units-1) +n_units*2,1);
  } else {
    res_covs = arma::mat(n_units*(n_units-1)/2 + n_units*2,terms.size());
    res_target = arma::mat(n_units*(n_units-1)/2 +n_units*2,1);
  }
  arma::vec res_unit = arma::linspace<arma::vec>(1.0, (double)n_units, n_units);
  int now = 0;
  if(return_z){
    for(int i: seq(1,n_units-1)){
      for(int j: seq(1,n_units)){
        if(i == j){
          continue;
        }
        if((i > j) & (!directed)){
          continue;
        }
        
        // Get present values of x_ij, y_i, y_j
        z_ij = object.z_network.get_val(i,j);
        xyz_calculate_change_stats(change_stat_network, i,
                                   j,
                                   object,
                                   data_list,
                                   type_list,
                                   z,
                                   is_full_neighborhood,
                                   functions);
        res_covs.row(now)= (change_stat_network).as_row();
        res_target.row(now) = z_ij;
        i_vec.at(now) = i;
        j_vec.at(now) = j;
        overlap_vec.at(now) = object.get_val_overlap(i, j);
        
        now += 1;
      }
    }
    res_z.push_back(arma::join_rows(res_target.rows(0, now-1),arma::join_rows(i_vec.rows(0, now-1),
                                                    j_vec.rows(0, now-1),
                                                    overlap_vec.rows(0, now-1)), 
                                                    res_covs.rows(0, now-1)), "data");
    res.push_back(res_z, "res_z");
    res_covs.fill(0);
    res_target.fill(0);
    now = 0;
  }
  if(return_x){
    for(int i: seq(1,n_units)){
      x_i = object.x_attribute.get_val_no_scale(i);
      xyz_calculate_change_stats(change_stat_attribute_i, i,
                                 i,
                                 object,
                                 data_list,
                                 type_list,
                                 x,
                                 is_full_neighborhood,
                                 functions);
      res_covs.row(now)= (change_stat_attribute_i).as_row();
      res_target.row(now) = x_i;
      now += 1;
    }
    res_x.push_back(arma::join_rows(res_target.rows(0, now-1),res_unit,
                                    res_covs.rows(0, now-1)), "data");
    res.push_back(res_x, "res_x");
    res_covs.fill(0);
    res_target.fill(0);
    now = 0;
  }
  
  if(return_y){
    for(int i: seq(1,n_units)){
      
      y_i = object.y_attribute.get_val_no_scale(i);
      
      xyz_calculate_change_stats(change_stat_attribute_i, i,
                                 i,
                                 object,
                                 data_list,
                                 type_list,
                                 y,
                                 is_full_neighborhood,
                                 functions);
      res_covs.row(now)= (change_stat_attribute_i).as_row();
      res_target.row(now) = y_i;
      now += 1;
    }
    res_y.push_back(arma::join_rows(res_target.rows(0, now-1),res_unit,res_covs.rows(0, now-1)), "data");
    res.push_back(res_y, "res_y");
    res_covs.fill(0);
    res_target.fill(0);
    now = 0;
  }
  
  return(res);
}

