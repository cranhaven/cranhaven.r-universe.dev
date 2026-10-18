#include <RcppArmadillo.h>
#include "iglm/extension_api.hpp"
#include <random>
#include <set>
#include <unordered_map>
#include "iglm/xyz_class.h"


// The KEY MACRO: Define the signature wrapper (the lambda capture list is empty)
#define CHANGESTAT [](const XYZ_class &object,const int &unit_i,const int &unit_j, const arma::mat &data,const double &type,const std::string &mode,const bool &is_full_neighborhood) -> double


// Alias for a function pointer used to calculate a validation metric or score.
// This signature defines a **mapping** from a complex state space (captured by the seven
// reference arguments) to a single **double** metric. All parameters are passed by 
// constant reference, indicating the function may modify the state variables 
// (`unit_i`, `data`, `mode`, etc.) during its execution.
//  @param object A reference to the state container, `XYZ_class`.
//  @param unit_i, unit_j References to integer indices, likely representing **agents** or **nodes**.
//  @param data A reference to an `arma::mat` (Armadillo matrix), likely a **system matrix** (e.g., adjacency, feature set).
//  @param type A reference to a `double`, likely a **model parameter** or **hyperparameter**.
//  @param mode A reference to a `std::string`, specifying the **operational regime** (e.g., "fast", "full").
//  @param is_full_neighborhood A reference to a `bool` flag, controlling the scope or **boundary condition**.
//  @returns double The calculated validation score or fitness value.

auto xyz_stat_repetition = CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "z"){
    return(object.z_network.get_val(unit_j, unit_i));
  }  
  else{
    return(0);
  }  
}; 
EFFECT_REGISTER("mutual_global", ::xyz_stat_repetition, "mutual_global", 0);

auto xyz_stat_edges= CHANGESTAT{
  
  if (mode == "z")
    return 1.0;
  else
    return 0.0;
}; 
// Register: name, function pointer, short name, double
EFFECT_REGISTER("edges_global", ::xyz_stat_edges, "edges_global", 0);

auto xyz_stat_repetition_nonb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "z"){
    return(object.z_network.get_val(unit_j, unit_i)*(1-object.get_val_overlap(unit_i,unit_j)));
  } 
  else{
    return(0);
  }  
};
EFFECT_REGISTER("mutual_alocal", ::xyz_stat_repetition_nonb, "mutual_alocal", 0);
auto xyz_stat_repetition_nb= CHANGESTAT{
  if(mode == "z"){
    return(object.z_network.get_val(unit_j, unit_i)*object.get_val_overlap(unit_i,unit_j));
  }
  else{
    return(0);
  } 
};
EFFECT_REGISTER("mutual_local", ::xyz_stat_repetition_nb, "mutual_local", 0);


auto xyz_stat_cov_z_out_nb= CHANGESTAT{
  if(mode == "z"){ 
    return(data.at(0, unit_i-1)*object.get_val_overlap(unit_i,unit_j));
  }else {
    return(0);
  } 
};
EFFECT_REGISTER("cov_z_out_local", ::xyz_stat_cov_z_out_nb, "cov_z_out_local", 0);


auto xyz_stat_cov_z_in_nb= CHANGESTAT{
  if(mode == "z"){ 
    return(data.at(0, unit_j-1)*object.get_val_overlap(unit_i,unit_j));
  }else {
    return(0);
  } 
};
EFFECT_REGISTER("cov_z_in_local", ::xyz_stat_cov_z_in_nb, "cov_z_in_local", 0);



auto xyz_stat_cov_z_out_nonb= CHANGESTAT{
  if(mode == "z"){ 
    return(data.at(0, unit_i-1)*(1-object.get_val_overlap(unit_i,unit_j)));
  }else {
    return(0);
  }  
};
EFFECT_REGISTER("cov_z_out_alocal", ::xyz_stat_cov_z_out_nonb, "cov_z_out_alocal", 0);

auto xyz_stat_cov_z_in_nonb= CHANGESTAT{
  if(mode == "z"){ 
    return(data.at(0, unit_j-1)*(1-object.get_val_overlap(unit_i,unit_j)));
  }else {
    return(0);
  } 
};
EFFECT_REGISTER("cov_z_in_alocal", ::xyz_stat_cov_z_in_nonb, "cov_z_in_alocal", 0);

auto xyz_stat_cov_z_out= CHANGESTAT{
  if(mode == "z"){ 
    return(data.at(0, unit_i-1));
  }else {
    return(0);
  }  
};
EFFECT_REGISTER("cov_z_out_global", ::xyz_stat_cov_z_out, "cov_z_out_global", 0);

auto xyz_stat_cov_z_in= CHANGESTAT{
  if(mode == "z"){ 
    return(data.at(0, unit_j-1));
  }else {
    return(0);
  } 
};
EFFECT_REGISTER("cov_z_in_global", ::xyz_stat_cov_z_in, "cov_z_in_global", 0);


auto xyz_stat_cov_z_nb= CHANGESTAT{
  if(mode == "z"){
    if(object.get_val_overlap(unit_i,unit_j)){
      return(data.at(unit_i-1, unit_j-1));  
    } else {
      return(0);
    }
    
  }
  else{
    return(0);
  }
};
EFFECT_REGISTER("cov_z_local", ::xyz_stat_cov_z_nb, "cov_z_local", 0);


auto xyz_stat_cov_z_nonb= CHANGESTAT{
  if(mode == "z"){
    if((1-object.get_val_overlap(unit_i,unit_j))){
      return(data.at(unit_i-1, unit_j-1));  
    } else {
      return(0);
    }
    
  }
  else{
    return(0);
  }
};
EFFECT_REGISTER("cov_z_alocal", ::xyz_stat_cov_z_nonb, "cov_z_alocal", 0);

auto xyz_stat_cov_z= CHANGESTAT{
  if(mode == "z"){
    return(data.at(unit_i-1, unit_j-1));
  }
  else{
    return(0);
  }
};
EFFECT_REGISTER("cov_z_global", ::xyz_stat_cov_z, "cov_z_global", 0);


auto xyz_stat_cov_x= CHANGESTAT{
  if(mode == "x"){
    return(data.at(0, unit_i-1));
  }
  else{
    return(0);
  }
};
EFFECT_REGISTER("cov_x", ::xyz_stat_cov_x, "cov_x", 0);

auto xyz_stat_cov_y= CHANGESTAT{
  if(mode == "y"){
    return(data.at(0, unit_i-1));
  }
  else{
    return(0);
  }
};
EFFECT_REGISTER("cov_y", ::xyz_stat_cov_y, "cov_y", 0);



auto xyz_stat_edges_nonb= CHANGESTAT{
  if(mode == "z"){
    if(is_full_neighborhood) {
      return(0);
    } else { 
      return(1-object.get_val_overlap(unit_i,unit_j));
      // bool same_group;
      // std::unordered_set<int> intersect_group;
      // std::set_intersection(std::begin(object.overlap.at(unit_i)),
      //                       std::end(object.overlap.at(unit_i)),
      //                       std::begin(object.overlap.at(unit_j)),
      //                       std::end(object.overlap.at(unit_j)),
      //                       std::inserter(intersect_group, std::begin(intersect_group)));
      // // The union of the two neighborhoods is saved in intersect_group
      // same_group = intersect_group.size()>0;
      // if(object.get_val_overlap(unit_i, unit_j) != object.get_val_overlap(unit_i,unit_j)){
      //   Rcout << "There is an issue between units:" ;
      //   Rcout << unit_i ;
      //   Rcout << " and " ;
      //   Rcout << unit_j << std::endl;
      // }
      // if(same_group){
      //   return(0);
      // } else {
      //   return(1);
      // }
      // if(object.get_val_overlap(unit_i,unit_j)){
      //   return(0);
      // } else {
      //   return(1);
      // }
    }
    
  }
  else{
    return(0);
  } 
};
EFFECT_REGISTER("edges_alocal", ::xyz_stat_edges_nonb, "edges_alocal", 0);


auto xyz_stat_edges_nb= CHANGESTAT{
  if(mode == "z"){
    if(is_full_neighborhood) {
      return(1);
    } else {
      return(object.get_val_overlap(unit_i,unit_j));
      // bool same_group;
      // std::unordered_set<int> intersect_group;
      // std::set_intersection(std::begin(object.overlap.at(unit_i)),
      //                       std::end(object.overlap.at(unit_i)),
      //                       std::begin(object.overlap.at(unit_j)),
      //                       std::end(object.overlap.at(unit_j)),
      //                       std::inserter(intersect_group, std::begin(intersect_group)));
      // // The union of the two neighborhoods is saved in intersect_group
      // same_group = intersect_group.size()>0;
      // if(same_group){
      //   return(1);
      // } else {
      //   return(0);
      // }
    }
    
  }
  else{
    return(0);
  }
};
EFFECT_REGISTER("edges_local", ::xyz_stat_edges_nb, "edges_local", 0);


auto xyz_stat_attribute_xy_nb= CHANGESTAT{
  if(mode == "y"){
    double res = 0.0;
    for (auto k = object.overlap.at(unit_i).begin(); k != object.overlap.at(unit_i).end(); k++) {
      res+= object.y_attribute.get_val(*k);
    }
    return(res);
  } else if(mode == "x"){
    double res = 0.0;
    for (auto k = object.overlap.at(unit_i).begin(); k != object.overlap.at(unit_i).end(); k++) {
      res+= object.x_attribute.get_val(*k);
    }
    return(res);
  } else {
    return(0.0);
  }
};
EFFECT_REGISTER("attribute_xy_local", ::xyz_stat_attribute_xy_nb, "attribute_xy_local", 0);

auto xyz_stat_attribute_xy_nonb= CHANGESTAT{
  if(mode == "y"){
    
    std::vector<int> difference_result =
      get_difference_vec(object.all_units, object.overlap.at(unit_i));
    
    
    double res = 0.0;
    for (int k : difference_result) {
      res+= object.y_attribute.get_val(k);
    }
    return(res);
  } else if(mode == "x"){ 
    std::vector<int> difference_result = 
      get_difference_vec(object.all_units, object.overlap.at(unit_i));
    double res = 0.0;
    for (int k : difference_result) {
      res+= object.x_attribute.get_val(k);
    } 
    return(res);
  } else { 
    return(0.0);
  }
};
EFFECT_REGISTER("attribute_xy_alocal", ::xyz_stat_attribute_xy_nonb, "attribute_xy_alocal", 0);


auto xyz_stat_attribute_yz_nb= CHANGESTAT{
  if(mode == "y"){
    return(object.adj_list_nb.at(unit_i).size());
  } else if(mode == "z"){  
    return(object.y_attribute.get_val(unit_i) + object.y_attribute.get_val(unit_j));
  } else { 
    return(0);
  } 
};
EFFECT_REGISTER("attribute_yz_local", ::xyz_stat_attribute_yz_nb, "attribute_yz_local", 0);

auto xyz_stat_attribute_xz_nb= CHANGESTAT{
  if(mode == "x"){
    return(object.adj_list_nb.at(unit_i).size());
  } else if(mode == "z"){ 
    return(object.x_attribute.get_val(unit_i) + object.x_attribute.get_val(unit_j));
  } else { 
    return(0);
  } 
};
EFFECT_REGISTER("attribute_xz_local", ::xyz_stat_attribute_xz_nb, "attribute_xz_local", 0);


auto xyz_stat_edges_x_out_nb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "x"){
    return(object.out_degrees_nb.at(unit_i));
  } else if(mode == "z"){  
    return(object.x_attribute.get_val(unit_i)*object.get_val_overlap(unit_i,unit_j));
  }else { 
    return(0);
  }
}; 
EFFECT_REGISTER("outedges_x_local", ::xyz_stat_edges_x_out_nb, "outedges_x_local", 0);

auto xyz_stat_edges_x_match_global = CHANGESTAT {
  if (mode == "z") {
    if (object.x_attribute.get_val(unit_i) ==  object.x_attribute.get_val(unit_j)) {
      return 1.0;
    } else {
      return 0.0;
    }
    
  } else if (mode == "x") { 
    if(object.x_attribute.family != "binomial") {
      Rcpp::stop("The x attribute should be binary for this statistic");
    }
    double res = 0.0; 
    
    if (object.z_network.directed) {
      const auto& out_connections = object.z_network.adj_list.at(unit_i);
      for (int k : out_connections) {
        res += (2.0 * object.x_attribute.get_val(k) - 1.0);
      }
      const auto& in_connections = object.z_network.adj_list_in.at(unit_i);
      for (int k : in_connections) {
        res += (2.0 * object.x_attribute.get_val(k) - 1.0);
      }
    } else {
      const auto& connections_of_i = object.z_network.adj_list.at(unit_i);
      for (int k : connections_of_i) {
        res += (2.0 * object.x_attribute.get_val(k) - 1.0);
      } 
    }
    
    return res;
    
  } else if (mode == "y") { 
    return 0.0; 
  } 
  
  return 0.0;
}; 

EFFECT_REGISTER("edges_x_match_global", ::xyz_stat_edges_x_match_global, "edges_x_match_global", 0);

auto xyz_stat_edges_x_match_local = CHANGESTAT {
  if (mode == "z") {
    if(!object.get_val_overlap(unit_i, unit_j)) {
      return 0.0;
    }
    if (object.x_attribute.get_val(unit_i) ==  object.x_attribute.get_val(unit_j)) {
      return 1.0;
    } else {
      return 0.0;
    } 
    
  } else if (mode == "x") {  
    double res = 0.0; 
    if(object.x_attribute.family != "binomial") {
      Rcpp::stop("The x attribute should be binary for this statistic");
    }
    if (object.z_network.directed) {
      const auto& out_connections = object.adj_list_nb.at(unit_i);
      for (int k : out_connections) {
        res += (2.0 * object.x_attribute.get_val(k) - 1.0);
      }
      const auto& in_connections = object.adj_list_in_nb.at(unit_i);
      for (int k : in_connections) {
        res += (2.0 * object.x_attribute.get_val(k) - 1.0);
      }
    } else {
      const auto& connections_of_i = object.adj_list_nb.at(unit_i);
      for (int k : connections_of_i) {
        res += (2.0 * object.x_attribute.get_val(k) - 1.0);
      }  
    }
    
    return res;
    
  } else if (mode == "y") {  
    return 0.0; 
  }  
  return 0.0;
};  

EFFECT_REGISTER("edges_x_match_local", ::xyz_stat_edges_x_match_local, "edges_x_match_local", 0);


auto xyz_stat_edges_y_match = CHANGESTAT {
  if (mode == "z") {
    double Y_i = object.y_attribute.get_val(unit_i);
    double Y_j = object.y_attribute.get_val(unit_j);
    
    if (Y_i == Y_j) {
      return 1.0;
    } else {
      return 0.0;
    }
    
  } else if (mode == "y") { 
    if(object.y_attribute.family != "binomial") {
      Rcpp::stop("The y attribute should be binary for this statistic");
    }
    double res = 0.0; 
    
    if (object.z_network.directed) {
      const auto& out_connections = object.z_network.adj_list.at(unit_i);
      for (int k : out_connections) {
        double Y_k = object.y_attribute.get_val(k);
        res += (2.0 * Y_k - 1.0);
      }
      const auto& in_connections = object.z_network.adj_list_in.at(unit_i);
      for (int k : in_connections) {
        double Y_k = object.y_attribute.get_val(k);
        res += (2.0 * Y_k - 1.0);
      }
    } else {
      const auto& connections_of_i = object.z_network.adj_list.at(unit_i);
      for (int k : connections_of_i) {
        double Y_k = object.y_attribute.get_val(k);
        res += (2.0 * Y_k - 1.0);
      } 
    }
    
    return res;
    
  } else if (mode == "x") { 
    return 0.0; 
  } 
  
  return 0.0;
}; 

EFFECT_REGISTER("edges_y_match_global", ::xyz_stat_edges_y_match, "edges_y_match_global", 0);

auto xyz_stat_edges_y_match_local = CHANGESTAT {
  if (mode == "z") {
    if(!object.get_val_overlap(unit_i, unit_j)) {
      return 0.0;
    }
    double Y_i = object.y_attribute.get_val(unit_i);
    double Y_j = object.y_attribute.get_val(unit_j);
    
    if (Y_i == Y_j) {
      return 1.0;
    } else {
      return 0.0;
    }
    
  } else if (mode == "y") { 
    if(object.y_attribute.family != "binomial") {
      Rcpp::stop("The y attribute should be binary for this statistic");
    }
    double res = 0.0; 
    
    if (object.z_network.directed) {
      const auto& out_connections = object.adj_list_nb.at(unit_i);
      for (int k : out_connections) {
        double Y_k = object.y_attribute.get_val(k);
        res += (2.0 * Y_k - 1.0);
      }
      const auto& in_connections = object.adj_list_in_nb.at(unit_i);
      for (int k : in_connections) {
        double Y_k = object.y_attribute.get_val(k);
        res += (2.0 * Y_k - 1.0);
      }
    } else {
      const auto& connections_of_i = object.adj_list_nb.at(unit_i);
      for (int k : connections_of_i) {
        double Y_k = object.y_attribute.get_val(k);
        res += (2.0 * Y_k - 1.0);
      } 
    }
    
    return res;
    
  } else if (mode == "x") { 
    return 0.0; 
  } 
  
  return 0.0;
}; 

EFFECT_REGISTER("edges_y_match_local", ::xyz_stat_edges_y_match_local, "edges_y_match_local", 0);

auto xyz_stat_edges_x_out_nonb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "x"){
    auto& connections_of_i_all =  object.z_network.adj_list.at(unit_i);
    std::vector<int> connections_of_i;
    
    // If there is no full neighborhood we need to cut the connections of i to only include other units within the same neighborhood
    if(!is_full_neighborhood){
      // Next we only want to get the connections within the same group
      connections_of_i = get_difference_vec(connections_of_i_all, object.overlap.at(unit_i));
    } else {    
      connections_of_i = connections_of_i_all;
    }     
    return(connections_of_i.size());
  } else if(mode == "z"){  
    return(object.x_attribute.get_val(unit_i)*(1-object.get_val_overlap(unit_i,unit_j)));
  }else { 
    return(0);
  }
};
EFFECT_REGISTER("outedges_x_alocal", ::xyz_stat_edges_x_out_nonb, "outedges_x_alocal", 0);


auto xyz_stat_edges_x_out= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "x"){
    return(object.z_network.out_degrees.at(unit_i));
  } else if(mode == "z"){ 
    return(object.x_attribute.get_val(unit_i));
  }else {
    return(0);
  }
}; 
EFFECT_REGISTER("outedges_x_global", ::xyz_stat_edges_x_out, "outedges_x_global", 0);

auto xyz_stat_edges_x_in_nb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "x"){
    return(object.in_degrees_nb.at(unit_i));
  } else if(mode == "z"){ 
    return(object.x_attribute.get_val(unit_j)*object.get_val_overlap(unit_i, unit_j));
  }else {
    return(0);
  }
}; 
EFFECT_REGISTER("inedges_x_local", ::xyz_stat_edges_x_in_nb, "inedges_x_local", 0);

auto xyz_stat_edges_x_in_nonb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "x"){
    auto& connections_of_i_all =  object.z_network.adj_list_in.at(unit_i);
    std::vector<int> connections_of_i;
    
    // If there is no full neighborhood we need to cut the connections of i to only include other units within the same neighborhood
    if(!is_full_neighborhood){
      // Next we only want to get the connections within the same group
      connections_of_i = get_difference_vec(connections_of_i_all, object.overlap.at(unit_i));
    } else {   
      return(0.0);
    }   
    return(connections_of_i.size());
  } else if(mode == "z"){ 
    return(object.x_attribute.get_val(unit_j)*(1-object.get_val_overlap(unit_i, unit_j)));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("inedges_x_alocal", ::xyz_stat_edges_x_in_nonb, "inedges_x_alocal", 0);

auto xyz_stat_edges_x_in= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "x"){
    return(object.z_network.in_degrees.at(unit_i));
  } else if(mode == "z"){ 
    return(object.x_attribute.get_val(unit_j));
  }else {
    return(0);
  }
}; 
EFFECT_REGISTER("inedges_x_global", ::xyz_stat_edges_x_in, "inedges_x_global", 0);


auto xyz_stat_edges_y_out_nb= CHANGESTAT{
  if(mode == "y"){
    return(object.adj_list_nb.at(unit_i).size());
  } else if(mode == "z"){ 
    return(object.y_attribute.get_val(unit_i)*object.get_val_overlap(unit_i, unit_j));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("outedges_y_local", ::xyz_stat_edges_y_out_nb, "outedges_y_local", 0);

auto xyz_stat_edges_y_out_nonb= CHANGESTAT{
  if(mode == "y"){
    auto& connections_of_i_all =  object.z_network.adj_list.at(unit_i);
    std::vector<int> connections_of_i;
    
    // If there is no full neighborhood we need to cut the connections of i to only include other units within the same neighborhood
    if(!is_full_neighborhood){
      // Next we only want to get the connections within the same group
      connections_of_i = get_difference_vec(connections_of_i_all, object.overlap.at(unit_i));
      return(0.0);
    }   
    return(connections_of_i.size());
  } else if(mode == "z"){ 
    return(object.y_attribute.get_val(unit_i)*(1-object.get_val_overlap(unit_i, unit_j)));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("outedges_y_alocal", ::xyz_stat_edges_y_out_nonb, "outedges_y_alocal", 0);


auto xyz_stat_edges_y_out = CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "y"){
    return(object.z_network.adj_list.at(unit_i).size());
  } else if(mode == "z"){ 
    return(object.y_attribute.get_val(unit_i));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("outedges_y_global", ::xyz_stat_edges_y_out, "outedges_y_global", 0);


auto xyz_stat_edges_y_in_nb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "y"){
    return(object.adj_list_in_nb.at(unit_i).size());
  } else if(mode == "z"){ 
    return(object.y_attribute.get_val(unit_j)*object.get_val_overlap(unit_i, unit_j));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("inedges_y_local", ::xyz_stat_edges_y_in_nb, "inedges_y_local", 0);

auto xyz_stat_edges_y_in_nonb= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "y"){
    auto& connections_of_i_all =  object.z_network.adj_list_in.at(unit_i);
    std::vector<int> connections_of_i;
    
    // If there is no full neighborhood we need to cut the connections of i to only include other units within the same neighborhood
    if(!is_full_neighborhood){
      // Next we only want to get the connections within the same group
      connections_of_i = get_difference_vec(connections_of_i_all, object.overlap.at(unit_i));
    } else {    
      return(0.0);
    }    
    return(connections_of_i.size());
  } else if(mode == "z"){ 
    return(object.y_attribute.get_val(unit_j)*(1-object.get_val_overlap(unit_i, unit_j)));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("inedges_y_alocal", ::xyz_stat_edges_y_in_nonb, "inedges_y_alocal", 0);

auto xyz_stat_edges_y_in= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "y"){
    return(object.z_network.adj_list_in.at(unit_i).size());
  } else if(mode == "z"){ 
    return(object.y_attribute.get_val(unit_j));
  }else {
    return(0);
  }
};
EFFECT_REGISTER("inedges_y_global", ::xyz_stat_edges_y_in, "inedges_y_global", 0);


auto xyz_stat_attribute_x= CHANGESTAT{
  if(mode == "x"){
    return(1);
  } else {
    return(0);
  }
};
EFFECT_REGISTER("attribute_x", ::xyz_stat_attribute_x, "attribute_x", 0);

auto xyz_stat_attribute_y= CHANGESTAT{
  if(mode == "y"){
    return(1);
  } else {
    return(0);
  }
};
EFFECT_REGISTER("attribute_y", ::xyz_stat_attribute_y, "attribute_y", 0);


// cov_i *cov_j * z_ij*c_ij
auto xyz_stat_interaction_edges_cov= CHANGESTAT{
  if(object.z_network.directed){
    Rcpp::stop("This statistic is only for undirected networks");  
  }
  if(mode == "z"){
    // What to do if the network change stat is desired
    // z_ij from 0 -> 1
    if(object.get_val_overlap(unit_i, unit_j)){
      
      return(data.at(0, unit_i-1)*object.y_attribute.get_val(unit_j)+
             data.at(0, unit_j-1)*object.y_attribute.get_val(unit_i));  
    } else {
      return(0);
    }
    
  } else if (mode == "x"){
    // What to do if the attribute change stat is wanted
    // x_i from 0 -> 1
    double res = 0.0;
    return(res);
  } else{
    // What to do if the attribute change stat is wanted
    // y_i from 0 -> 1
    double res = 0.0;
    auto& connections_of_i =  object.adj_list_nb.at(unit_i);
    
    for (int k : connections_of_i) {
      // if(k != unit_j){
      res+= data.at(0, k-1);  
      // }
    } 
    // Rcout << res << std::endl;
    return(res);
  }
};
EFFECT_REGISTER("spillover_yc_symm", ::xyz_stat_interaction_edges_cov, "spillover_yc_symm", 0);


auto xyz_stat_interaction_edges_xy= CHANGESTAT{
  double res = 0.0;
  // Undirected case
  if(!object.z_network.directed){
    if(mode == "z"){
      // What to do if the network change stat is desired
      // z_ij from 0 -> 1
      if(object.get_val_overlap(unit_i, unit_j)){
        res = object.x_attribute.get_val(unit_i)*object.y_attribute.get_val(unit_j)+
          object.x_attribute.get_val(unit_j)*object.y_attribute.get_val(unit_i);
      } 
    } else if (mode == "x"){
      // What to do if the attribute change stat is wanted
      // x_i from 0 -> 1
      auto& connections_of_i =  object.adj_list_nb.at(unit_i);
      for (int k : connections_of_i) {
        if(k != unit_i){
          res+= object.y_attribute.get_val(k);
        } else {
          Rcout << "Here" << std::endl;
        }
      } 
    } else{
      // What to do if the attribute change stat is wanted
      // y_i from 0 -> 1
      auto& connections_of_i =  object.adj_list_nb.at(unit_i);
      
      for (int k : connections_of_i) {
        if(k != unit_i){
        res+= object.x_attribute.get_val(k);  
        } else {
          Rcout << "Here" << std::endl;
        }
      }
    }
  } else {
    if(mode == "z"){
      // What to do if the network change stat is desired
      // z_ij from 0 -> 1
      if(object.get_val_overlap(unit_i, unit_j)){
        res = object.x_attribute.get_val(unit_i)*object.y_attribute.get_val(unit_j);
      } 
    } else if (mode == "x"){ 
      // What to do if the attribute change stat is wanted
      // x_i from 0 -> 1
      auto& connections_of_i =  object.adj_list_nb.at(unit_i);
      
      for (int k : connections_of_i) {
        if(k != unit_i){
          res+= object.y_attribute.get_val(k);   
        }
      } 
    } else{ 
      // What to do if the attribute change stat is wanted
      // y_i from 0 -> 1
      auto& connections_of_i =  object.adj_list_in_nb.at(unit_i);
      
      for (int k : connections_of_i) {
        if(k != unit_i){
          res+= object.x_attribute.get_val(k);   
        }
      } 
    }
  }
  return(res);
  
};
EFFECT_REGISTER("spillover_xy", ::xyz_stat_interaction_edges_xy, "spillover_xy", 0);

auto xyz_stat_interaction_edges_y_cov= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "z"){
    // What to do if the network change stat is desired
    // z_ij from 0 -> 1
    if(object.get_val_overlap(unit_i, unit_j)){
      return(data.at(0,unit_i-1)*object.y_attribute.get_val(unit_j));  
    } else { 
      return(0);
    }  
  } else if (mode == "x"){  
    // What to do if the attribute change stat is wanted
    // x_i from 0 -> 1
    // Do nothing!
    double res = 0.0;
    return(res);
  } else{  
    // What to do if the attribute change stat is wanted
    // y_i from 0 -> 1
    double res = 0.0;
    auto& connections_of_i =  object.adj_list_nb.at(unit_i);
    
    for (int k : connections_of_i) {
      // if(k != unit_j){
      res+= data.at(0,k-1);   
      // }
    }  
    // Rcout << res << std::endl;
    return(res);
  }
};
EFFECT_REGISTER("spillover_yc", ::xyz_stat_interaction_edges_y_cov, "spillover_yc", 0);

auto xyz_stat_interaction_edges_yx= CHANGESTAT{
  
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    // What to do if the network change stat is desired
    // z_ij from 0 -> 1
    if(object.get_val_overlap(unit_i, unit_j)){
      return(object.x_attribute.get_val(unit_j)*object.y_attribute.get_val(unit_i));  
    } else { 
      return(0.0);
    } 
    
  } else if (mode == "x"){ 
    // What to do if the attribute change stat is wanted
    // x_i from 0 -> 1
    double res = 0.0;
    // Rcout << unit_i << std::endl;
    // Rcout << object.adj_list_in_nb.size() << std::endl;
    auto& connections_of_i =  object.adj_list_in_nb.at(unit_i);
    
    for (int k : connections_of_i) {
      // Rcout << "Attribute of unit";
      // Rcout << k << std::endl;
      // Rcout << object.attribute.get_val(k) << std::endl;
      // if(k != unit_j){
      res+= object.y_attribute.get_val(k); 
      // }
    } 
    // Rcout <<  res << std::endl;
    return(res);
  } else{ 
    // What to do if the attribute change stat is wanted
    // y_i from 0 -> 1
    double res = 0.0;
    auto& connections_of_i =  object.adj_list_nb.at(unit_i);
    
    for (int k : connections_of_i) {
      // if(k != unit_j){
      res+= object.x_attribute.get_val(k);   
      // }
    } 
    // Rcout << res << std::endl;
    return(res);
  }
};
EFFECT_REGISTER("spillover_yx", ::xyz_stat_interaction_edges_yx, "spillover_yx", 0);

auto xyz_stat_attribute_xy= CHANGESTAT{
  // Rcout <<  "Starting" << std::endl;
  if (mode == "x"){
    // Rcout <<  unit_i << std::endl;
    // Rcout <<  object.y_attribute.get_val(unit_i) << std::endl;
    return(object.y_attribute.get_val(unit_i));
  } else if (mode == "y") {  
    // Rcout <<  unit_i << std::endl;
    // Rcout <<  object.x_attribute.attribute.size() << std::endl;
    return(object.x_attribute.get_val(unit_i));
  } else {
    return(0);
  }
};
EFFECT_REGISTER("attribute_xy_global", ::xyz_stat_attribute_xy, "attribute_xy_global", 0);

// y_i*y_j*z_ij * c_ij
auto xyz_stat_matching_edges_y= CHANGESTAT{
  if(mode == "z"){
    // What to do if the network change stat is desired
    // z_ij from 0 -> 1
    return(object.y_attribute.get_val(unit_i)*object.y_attribute.get_val(unit_j)*object.get_val_overlap(unit_i, unit_j));
  } else if (mode == "y"){
    // What to do if the attribute change stat is wanted
    // y_i from 0 -> 1
    double res = 0.0;
    const auto& connections_of_i =  object.adj_list_nb.at(unit_i);
    // Rcpp::Rcout << connections_of_i.size() <<std::endl;
    for (int k : connections_of_i) {
      res+= object.y_attribute.get_val(k);
    }
    if(object.z_network.directed){
      const auto& connections_in_of_i =  object.adj_list_in_nb.at(unit_i);
      // Rcpp::Rcout << connections_in_of_i.size() <<std::endl;
      for (int k : connections_in_of_i) {
        res+= object.y_attribute.get_val(k);
      }
    }
    return(res);
  } else {
    return(0.0);
  }
};
EFFECT_REGISTER("spillover_yy", ::xyz_stat_matching_edges_y, "spillover_yy", 0);

auto xyz_stat_matching_edges_x= CHANGESTAT{
  if(mode == "z"){
    // What to do if the network change stat is desired
    // z_ij from 0 -> 1
    return(object.x_attribute.get_val(unit_i)*object.x_attribute.get_val(unit_j)*object.get_val_overlap(unit_i, unit_j));
  } else if (mode == "x"){
    // What to do if the attribute change stat is wanted
    // x_i from 0 -> 1
    double res = 0.0;
    const auto& connections_of_i =  object.adj_list_nb.at(unit_i);
    // Rcpp::Rcout << connections_of_i.size() <<std::endl;
    for (int k : connections_of_i) {
      res+= object.x_attribute.get_val(k);
    } 
    if(object.z_network.directed){
      const auto& connections_in_of_i =  object.adj_list_in_nb.at(unit_i);
      // Rcpp::Rcout << connections_in_of_i.size() <<std::endl;
      for (int k : connections_in_of_i) {
        res+= object.x_attribute.get_val(k);
      }
    }
    return(res);
  } else {
    return(0.0);
  }
};
EFFECT_REGISTER("spillover_xx", ::xyz_stat_matching_edges_x, "spillover_xx", 0);

auto xyz_stat_spillover_yx_scaled_global = CHANGESTAT{
  if(mode == "z"){
    if(object.z_network.directed){
      double Y_i = object.y_attribute.get_val(unit_i);
      if (Y_i == 0) return 0.0;
      
      double X_j = object.x_attribute.get_val(unit_j);
      
      double current_sum_y = 0;
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum_y += object.x_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      // 2. Back-out / Add-in Logic
      if (tie_exists) {
        S_with = current_sum_y;
        d_with = current_deg;
        S_without = current_sum_y - X_j;
        d_without = current_deg - 1.0;
      } else {
        S_without = current_sum_y;
        d_without = current_deg;
        S_with = current_sum_y + X_j;
        d_with = current_deg + 1.0;
      }
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return Y_i * (A_with - A_without);
    } else{
      double delta_total = 0.0;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      double Y_i = object.y_attribute.get_val(unit_i);
      double X_i = object.x_attribute.get_val(unit_i); 
      
      double Y_j = object.y_attribute.get_val(unit_j);
      double X_j = object.x_attribute.get_val(unit_j); 
      
      if (Y_i != 0) {
        double sum_x_neighbors_i = 0;
        auto& neighbors_i = object.z_network.adj_list.at(unit_i);
        double deg_i = neighbors_i.size();
        
        for (int l : neighbors_i) {
          sum_x_neighbors_i += object.x_attribute.get_val(l);
        } 
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_x_neighbors_i;
          d_with = deg_i;
          S_without = sum_x_neighbors_i - X_j;
          d_without = deg_i - 1.0;
        } else {
          S_without = sum_x_neighbors_i;
          d_without = deg_i;
          S_with = sum_x_neighbors_i + X_j;    
          d_with = deg_i + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += Y_i * (A_with - A_without);
      }
      
      if (Y_j != 0) {
        double sum_x_neighbors_j = 0;
        auto& neighbors_j = object.z_network.adj_list.at(unit_j);
        double deg_j = neighbors_j.size();
        
        for (int l : neighbors_j) {
          sum_x_neighbors_j += object.x_attribute.get_val(l);
        }
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_x_neighbors_j;
          d_with = deg_j;
          S_without = sum_x_neighbors_j - X_i; 
          d_without = deg_j - 1.0;
        } else {
          S_without = sum_x_neighbors_j;
          d_without = deg_j;
          S_with = sum_x_neighbors_j + X_i;   
          d_with = deg_j + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += Y_j * (A_with - A_without);
      }
      
      return delta_total;
    }
    
  } else if (mode == "x"){
    if(object.z_network.directed){
      double res = 0;
      auto& in_neighbors = object.z_network.adj_list_in.at(unit_i);
      
      for (int k : in_neighbors) {
        double deg_k = object.z_network.adj_list.at(k).size();
        if (deg_k > 0.5) {
          double Y_k = object.y_attribute.get_val(k);
          res += Y_k * (1.0 / deg_k);
        } 
      }
      return res;
    } else{
      double res = 0;
      auto& neighbors = object.z_network.adj_list.at(unit_i);
      
      for (int k : neighbors) {
        double deg_k = object.z_network.adj_list.at(k).size();
        if (deg_k > 0.5) {
          double Y_k = object.y_attribute.get_val(k);
          res += Y_k * (1.0 / deg_k);
        }
      }
      return res;
    }
    
  } else if (mode == "y"){
    if(object.z_network.directed){
      double S_i = 0;
      double deg_i = object.z_network.out_degrees[unit_i];
      for (int j : object.z_network.adj_list.at(unit_i)) {
        S_i += object.x_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    } else{
      double S_i = 0;
      double deg_i = object.z_network.out_degrees[unit_i];
      for (int j : object.z_network.adj_list.at(unit_i)) {
        S_i += object.x_attribute.get_val(j);
      } 
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    } 
    
  }  
  return 0.0; 
};
EFFECT_REGISTER("spillover_yx_scaled_global", ::xyz_stat_spillover_yx_scaled_global, "spillover_yx_scaled_global", 0);

auto xyz_stat_spillover_yx_scaled = CHANGESTAT{
  if(mode == "z"){
    if(object.z_network.directed){
      if (!object.get_val_overlap(unit_i, unit_j)) return 0.0; 
      
      double Y_i = object.y_attribute.get_val(unit_i);
      if (Y_i == 0) return 0.0;
      
      double X_j = object.x_attribute.get_val(unit_j);
      
      double current_sum_x = 0;
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum_x += object.x_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      // 2. Back-out / Add-in Logic
      if (tie_exists) {
        S_with = current_sum_x;
        d_with = current_deg;
        S_without = current_sum_x - X_j;
        d_without = current_deg - 1.0;
      } else {
        S_without = current_sum_x;
        d_without = current_deg;
        S_with = current_sum_x + X_j;
        d_with = current_deg + 1.0;
      }
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return Y_i * (A_with - A_without);
    } else{
      if (!object.get_val_overlap(unit_i, unit_j)) return 0.0;
      
      double delta_total = 0.0;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      double Y_i = object.y_attribute.get_val(unit_i);
      double X_i = object.x_attribute.get_val(unit_i); 
      
      double Y_j = object.y_attribute.get_val(unit_j);
      double X_j = object.x_attribute.get_val(unit_j); 
      
      if (Y_i != 0) {
        double sum_x_neighbors_i = 0;
        auto& neighbors_i = object.adj_list_nb.at(unit_i);
        double deg_i = neighbors_i.size();
        
        for (int l : neighbors_i) {
          sum_x_neighbors_i += object.x_attribute.get_val(l);
        } 
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_x_neighbors_i;
          d_with = deg_i;
          S_without = sum_x_neighbors_i - X_j;
          d_without = deg_i - 1.0;
        } else {
          S_without = sum_x_neighbors_i;
          d_without = deg_i;
          S_with = sum_x_neighbors_i + X_j;    
          d_with = deg_i + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += Y_i * (A_with - A_without);
      }
      
      if (Y_j != 0) {
        double sum_x_neighbors_j = 0;
        auto& neighbors_j = object.adj_list_nb.at(unit_j);
        double deg_j = neighbors_j.size();
        
        for (int l : neighbors_j) {
          sum_x_neighbors_j += object.x_attribute.get_val(l);
        }
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_x_neighbors_j;
          d_with = deg_j;
          S_without = sum_x_neighbors_j - X_i; 
          d_without = deg_j - 1.0;
        } else {
          S_without = sum_x_neighbors_j;
          d_without = deg_j;
          S_with = sum_x_neighbors_j + X_i;   
          d_with = deg_j + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += Y_j * (A_with - A_without);
      }
      
      return delta_total;
    }
    
  } else if (mode == "x"){
    if(object.z_network.directed){
      double res = 0;
      auto& in_neighbors = object.adj_list_in_nb.at(unit_i);
      
      for (int k : in_neighbors) {
        if (object.get_val_overlap(k, unit_i)) {
          double deg_k = object.out_degrees_nb[k];
          if (deg_k > 0.5) {
            double Y_k = object.y_attribute.get_val(k);
            res += Y_k * (1.0 / deg_k);
          }
        } 
      }
      return res;
    } else{
      double res = 0;
      auto& neighbors = object.adj_list_nb.at(unit_i);
      
      for (int k : neighbors) {
        double deg_k = object.adj_list_nb.at(k).size();
        if (deg_k > 0.5) {
          double Y_k = object.y_attribute.get_val(k);
          res += Y_k * (1.0 / deg_k);
        }
      }
      return res;
    }
    
  } else if (mode == "y"){
    if(object.z_network.directed){
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double S_i = 0;
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        S_i += object.x_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    } else{
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double S_i = 0;
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        S_i += object.x_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    }
    
  } 
  return 0.0; 
};
EFFECT_REGISTER("spillover_yx_scaled_local", ::xyz_stat_spillover_yx_scaled, "spillover_yx_scaled_local", 0);


auto xyz_stat_spillover_xy_scaled = CHANGESTAT{
  if(mode == "z"){
    if(object.z_network.directed){
      if (!object.get_val_overlap(unit_i, unit_j)) return 0.0; 
      
      double X_i = object.x_attribute.get_val(unit_i);
      if (X_i == 0) return 0.0;
      
      double Y_j = object.y_attribute.get_val(unit_j);
      
      double current_sum_y = 0;
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum_y += object.y_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      // 2. Back-out / Add-in Logic
      if (tie_exists) {
        S_with = current_sum_y;
        d_with = current_deg;
        S_without = current_sum_y - Y_j;
        d_without = current_deg - 1.0;
      } else {
        S_without = current_sum_y;
        d_without = current_deg;
        S_with = current_sum_y + Y_j;
        d_with = current_deg + 1.0;
      }
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return X_i * (A_with - A_without);
    } else{
      if (!object.get_val_overlap(unit_i, unit_j)) return 0.0;
      
      double delta_total = 0.0;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      double X_i = object.x_attribute.get_val(unit_i);
      double Y_i = object.y_attribute.get_val(unit_i); 
      
      double X_j = object.x_attribute.get_val(unit_j);
      double Y_j = object.y_attribute.get_val(unit_j); 
      
      if (X_i != 0) {
        double sum_y_neighbors_i = 0;
        auto& neighbors_i = object.adj_list_nb.at(unit_i);
        double deg_i = neighbors_i.size();
        
        for (int l : neighbors_i) {
          sum_y_neighbors_i += object.y_attribute.get_val(l);
        } 
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_y_neighbors_i;
          d_with = deg_i;
          S_without = sum_y_neighbors_i - Y_j;
          d_without = deg_i - 1.0;
        } else {
          S_without = sum_y_neighbors_i;
          d_without = deg_i;
          S_with = sum_y_neighbors_i + Y_j;    
          d_with = deg_i + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += X_i * (A_with - A_without);
      }
      
      if (X_j != 0) {
        double sum_y_neighbors_j = 0;
        auto& neighbors_j = object.adj_list_nb.at(unit_j);
        double deg_j = neighbors_j.size();
        
        for (int l : neighbors_j) {
          sum_y_neighbors_j += object.y_attribute.get_val(l);
        }
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_y_neighbors_j;
          d_with = deg_j;
          S_without = sum_y_neighbors_j - Y_i; 
          d_without = deg_j - 1.0;
        } else {
          S_without = sum_y_neighbors_j;
          d_without = deg_j;
          S_with = sum_y_neighbors_j + Y_i;   
          d_with = deg_j + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += X_j * (A_with - A_without);
      }
      
      return delta_total;
    }
    
  } else if (mode == "y"){
    if(object.z_network.directed){
      double res = 0;
      auto& in_neighbors = object.adj_list_in_nb.at(unit_i);
      
      for (int k : in_neighbors) {
        if (object.get_val_overlap(k, unit_i)) {
          double deg_k = object.out_degrees_nb[k];
          if (deg_k > 0.5) {
            double X_k = object.x_attribute.get_val(k);
            res += X_k * (1.0 / deg_k);
          }
        } 
      }
      return res;
    } else{
      double res = 0;
      auto& neighbors = object.adj_list_nb.at(unit_i);
      
      for (int k : neighbors) {
        double deg_k = object.adj_list_nb.at(k).size();
        if (deg_k > 0.5) {
          double X_k = object.x_attribute.get_val(k);
          res += X_k * (1.0 / deg_k);
        }
      }
      return res;
    }
    
  } else if (mode == "x"){
    if(object.z_network.directed){
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double S_i = 0;
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        S_i += object.y_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    } else{
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double S_i = 0;
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        S_i += object.y_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    }
    
  } 
  return 0.0; 
};
EFFECT_REGISTER("spillover_xy_scaled_local", ::xyz_stat_spillover_xy_scaled, "spillover_xy_scaled_local", 0);

auto xyz_stat_spillover_xy_scaled_global = CHANGESTAT{
  if(mode == "z"){
    if(object.z_network.directed){
      double X_i = object.x_attribute.get_val(unit_i);
      if (X_i == 0) return 0.0;
      
      double Y_j = object.y_attribute.get_val(unit_j);
      
      double current_sum_y = 0;
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum_y += object.y_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      // 2. Back-out / Add-in Logic
      if (tie_exists) {
        S_with = current_sum_y;
        d_with = current_deg;
        S_without = current_sum_y - Y_j;
        d_without = current_deg - 1.0;
      } else {
        S_without = current_sum_y;
        d_without = current_deg;
        S_with = current_sum_y + Y_j;
        d_with = current_deg + 1.0;
      } 
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return X_i * (A_with - A_without);
    } else{ 
      double delta_total = 0.0;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      double X_i = object.x_attribute.get_val(unit_i);
      double Y_i = object.y_attribute.get_val(unit_i); 
      
      double X_j = object.x_attribute.get_val(unit_j);
      double Y_j = object.y_attribute.get_val(unit_j); 
      
      if (X_i != 0) {
        double sum_y_neighbors_i = 0;
        auto& neighbors_i = object.z_network.adj_list.at(unit_i);
        double deg_i = neighbors_i.size();
        
        for (int l : neighbors_i) {
          sum_y_neighbors_i += object.y_attribute.get_val(l);
        }  
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_y_neighbors_i;
          d_with = deg_i;
          S_without = sum_y_neighbors_i - Y_j;
          d_without = deg_i - 1.0;
        } else { 
          S_without = sum_y_neighbors_i;
          d_without = deg_i;
          S_with = sum_y_neighbors_i + Y_j;    
          d_with = deg_i + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += X_i * (A_with - A_without);
      }
      
      if (X_j != 0) {
        double sum_y_neighbors_j = 0;
        auto& neighbors_j = object.z_network.adj_list.at(unit_j);
        double deg_j = neighbors_j.size();
        
        for (int l : neighbors_j) {
          sum_y_neighbors_j += object.y_attribute.get_val(l);
        }
        
        double S_with, d_with, S_without, d_without;
        
        if (tie_exists) {
          S_with = sum_y_neighbors_j;
          d_with = deg_j;
          S_without = sum_y_neighbors_j - Y_i; 
          d_without = deg_j - 1.0;
        } else {
          S_without = sum_y_neighbors_j;
          d_without = deg_j;
          S_with = sum_y_neighbors_j + Y_i;   
          d_with = deg_j + 1.0;
        }
        
        double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
        double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
        
        delta_total += X_j * (A_with - A_without);
      }
      
      return delta_total;
    }
    
  } else if (mode == "y"){
    if(object.z_network.directed){
      double res = 0;
      auto& in_neighbors = object.z_network.adj_list_in.at(unit_i);
      
      for (int k : in_neighbors) {
        double deg_k = object.z_network.out_degrees[k];
        if (deg_k > 0.5) {
          double X_k = object.x_attribute.get_val(k);
          res += X_k * (1.0 / deg_k);
        }
      }
      return res;
    } else{
      double res = 0;
      auto& neighbors = object.z_network.adj_list.at(unit_i);
      
      for (int k : neighbors) {
        double deg_k = object.z_network.out_degrees[k];
        if (deg_k > 0.5) {
          double X_k = object.x_attribute.get_val(k);
          res += X_k * (1.0 / deg_k);
        }
      }
      return res;
    }
    
  } else if (mode == "x"){
    if(object.z_network.directed){
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double S_i = 0;
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        S_i += object.y_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    } else{
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double S_i = 0;
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        S_i += object.y_attribute.get_val(j);
      }
      
      return (deg_i > 0.5) ? (S_i / deg_i) : 0.0;
    }
    
  } 
  return 0.0; 
};
EFFECT_REGISTER("spillover_xy_scaled_global", ::xyz_stat_spillover_xy_scaled_global, "spillover_xy_scaled_global", 0);

auto xyz_stat_spillover_yy_scaled = CHANGESTAT{
  // Statistic: y_i * Average(y_neighbors)
  if (mode == "z") {
    bool tie_exists = object.z_network.get_val(unit_i, unit_j);
    if(object.z_network.directed){
      if (!object.get_val_overlap(unit_i, unit_j)) return 0.0;
      
      double Y_i = object.y_attribute.get_val(unit_i);
      if (Y_i == 0) return 0.0; 
      double Y_j = object.y_attribute.get_val(unit_j);
      
      double current_sum = 0;
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum += object.y_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      
      if (tie_exists) {
        S_with = current_sum;
        d_with = current_deg;
        S_without = current_sum - Y_j;
        d_without = current_deg - 1.0;
      } else {
        S_without = current_sum;
        d_without = current_deg;
        S_with = current_sum + Y_j;
        d_with = current_deg + 1.0;
      }
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return Y_i * (A_with - A_without);
    } else {
      if (!object.get_val_overlap(unit_i, unit_j)) return 0.0;
      double delta_total = 0.0;
      double Y_i = object.y_attribute.get_val(unit_i);
      double Y_j = object.y_attribute.get_val(unit_j);
      
      if (Y_i != 0) {
        double sum_i = 0;
        auto& neighbors_i = object.adj_list_nb.at(unit_i);
        double deg_i = neighbors_i.size();
        for (int l : neighbors_i) {
          sum_i += object.y_attribute.get_val(l);
        }
        
        double S_with_i, d_with_i, S_without_i, d_without_i;
        // bool tie_exists = object.z_network.get_val(unit_i, unit_j);
        if (tie_exists) {
          S_with_i = sum_i;
          d_with_i = deg_i;
          S_without_i = sum_i - Y_j;
          d_without_i = deg_i - 1.0;
        } else { 
          S_without_i = sum_i;
          d_without_i = deg_i;
          S_with_i = sum_i + Y_j;
          d_with_i = deg_i + 1.0;
        }  
        
        double A_without_i = (d_without_i > 0.5) ? (S_without_i / d_without_i) : 0.0;
        double A_with_i    = (d_with_i > 0.5)    ? (S_with_i / d_with_i)    : 0.0;
        
        delta_total += Y_i * (A_with_i - A_without_i);
      }
      
      if (Y_j != 0) { 
        double sum_j = 0;
        auto& neighbors_j = object.adj_list_nb.at(unit_j);
        double deg_j = neighbors_j.size();
        for (int l : neighbors_j) {
          sum_j += object.y_attribute.get_val(l);
        }  
        
        double S_with_j, d_with_j, S_without_j, d_without_j;
        // bool tie_exists = object.z_network.get_val(unit_i, unit_j); 
        
        if (tie_exists) {
          S_with_j = sum_j;
          d_with_j = deg_j;
          S_without_j = sum_j - Y_i; 
          d_without_j = deg_j - 1.0;
        } else { 
          S_without_j = sum_j;
          d_without_j = deg_j;
          S_with_j = sum_j + Y_i; // j gains i
          d_with_j = deg_j + 1.0;
        }  
        
        double A_without_j = (d_without_j > 0.5) ? (S_without_j / d_without_j) : 0.0;
        double A_with_j    = (d_with_j > 0.5)    ? (S_with_j / d_with_j)    : 0.0;
        
        delta_total += Y_j * (A_with_j - A_without_j);
      }
      
      return delta_total; 
    }
    
    
  } else if (mode == "y") {
    if(object.z_network.directed){
      double total_diff = 0;
      // Part A: i's own average
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.y_attribute.get_val(j);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      
      auto& in_neighbors = object.adj_list_in_nb.at(unit_i);
      for (int k : in_neighbors) {
        double deg_k = object.adj_list_nb.at(k).size();
        if (deg_k > 0.5) {
          double Y_k = object.y_attribute.get_val(k);
          total_diff += Y_k * (1.0 / deg_k);
        } 
      }
      return total_diff;
    } else {
      double total_diff = 0;
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.y_attribute.get_val(j);
        double deg_tmp =object.adj_list_nb.at(j).size();
        if (deg_tmp > 0.5) total_diff += (object.y_attribute.get_val(j) / deg_tmp);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      return total_diff;
    }
    
  }  
  return 0.0;
};
EFFECT_REGISTER("spillover_yy_scaled_local", ::xyz_stat_spillover_yy_scaled, "spillover_yy_scaled_local", 0);

auto xyz_stat_spillover_yy_scaled_global = CHANGESTAT{
  // Statistic: y_i * Average(y_neighbors)
  if (mode == "z") {
    if(object.z_network.directed){
      double Y_i = object.y_attribute.get_val(unit_i);
      if (Y_i == 0) return 0.0; 
      double Y_j = object.y_attribute.get_val(unit_j);
      
      double current_sum = 0;
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum += object.y_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      if (tie_exists) {
        S_with = current_sum;
        d_with = current_deg;
        S_without = current_sum - Y_j;
        d_without = current_deg - 1.0;
      } else { 
        S_without = current_sum;
        d_without = current_deg;
        S_with = current_sum + Y_j;
        d_with = current_deg + 1.0;
      } 
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return Y_i * (A_with - A_without);
    } else {
      double delta_total = 0.0;
      double Y_i = object.y_attribute.get_val(unit_i);
      double Y_j = object.y_attribute.get_val(unit_j);
      
      if (Y_i != 0) {
        double sum_i = 0;
        auto& neighbors_i = object.z_network.adj_list.at(unit_i);
        double deg_i = neighbors_i.size();
        for (int l : neighbors_i) {
          sum_i += object.y_attribute.get_val(l);
        }
        
        double S_with_i, d_with_i, S_without_i, d_without_i;
        bool tie_exists = object.z_network.get_val(unit_i, unit_j);
        
        if (tie_exists) {
          S_with_i = sum_i;
          d_with_i = deg_i;
          S_without_i = sum_i - Y_j;
          d_without_i = deg_i - 1.0;
        } else {
          S_without_i = sum_i;
          d_without_i = deg_i;
          S_with_i = sum_i + Y_j;
          d_with_i = deg_i + 1.0;
        } 
        
        double A_without_i = (d_without_i > 0.5) ? (S_without_i / d_without_i) : 0.0;
        double A_with_i    = (d_with_i > 0.5)    ? (S_with_i / d_with_i)    : 0.0;
        
        delta_total += Y_i * (A_with_i - A_without_i);
      }
      
      if (Y_j != 0) { 
        double sum_j = 0;
        auto& neighbors_j = object.z_network.adj_list.at(unit_j);
        double deg_j = neighbors_j.size();
        for (int l : neighbors_j) {
          sum_j += object.y_attribute.get_val(l);
        } 
        
        double S_with_j, d_with_j, S_without_j, d_without_j;
        bool tie_exists = object.z_network.get_val(unit_i, unit_j); 
        
        if (tie_exists) {
          S_with_j = sum_j;
          d_with_j = deg_j;
          S_without_j = sum_j - Y_i; // j loses i
          d_without_j = deg_j - 1.0;
        } else {
          S_without_j = sum_j;
          d_without_j = deg_j;
          S_with_j = sum_j + Y_i; // j gains i
          d_with_j = deg_j + 1.0;
        } 
        
        double A_without_j = (d_without_j > 0.5) ? (S_without_j / d_without_j) : 0.0;
        double A_with_j    = (d_with_j > 0.5)    ? (S_with_j / d_with_j)    : 0.0;
        
        delta_total += Y_j * (A_with_j - A_without_j);
      }
      return delta_total; 
    }
  } else if (mode == "y") {
    if(object.z_network.directed){
      double total_diff = 0;
      // Part A: i's own average
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.y_attribute.get_val(j);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      
      auto& in_neighbors = object.z_network.adj_list_in.at(unit_i);
      for (int k : in_neighbors) {
        double deg_k = object.z_network.adj_list.at(k).size();
        if (deg_k > 0.5) {
          double X_k = object.y_attribute.get_val(k);
          total_diff +=X_k * (1.0 / deg_k);
        } 
      }
      return total_diff;
    } else {
      double total_diff = 0;
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.y_attribute.get_val(j);
        double deg_tmp =object.z_network.adj_list.at(j).size();
        if (deg_tmp > 0.5) total_diff += (object.y_attribute.get_val(j) / deg_tmp);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      return total_diff;
    }
  }   
  return 0.0;
};
EFFECT_REGISTER("spillover_yy_scaled_global", ::xyz_stat_spillover_yy_scaled_global, "spillover_yy_scaled_global", 0);

auto xyz_stat_spillover_xx_scaled = CHANGESTAT{
  // Statistic: y_i * Average(y_neighbors)
  if (mode == "z") {
    if (!object.get_val_overlap(unit_i, unit_j)) return 0.0;
    
    if(object.z_network.directed){
      
      double X_i = object.x_attribute.get_val(unit_i);
      if (X_i == 0) return 0.0; 
      double X_j = object.x_attribute.get_val(unit_j);
      
      double current_sum = 0;
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum += object.x_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      if (tie_exists) {
        S_with = current_sum;
        d_with = current_deg;
        S_without = current_sum - X_j;
        d_without = current_deg - 1.0;
      } else { 
        S_without = current_sum;
        d_without = current_deg;
        S_with = current_sum + X_j;
        d_with = current_deg + 1.0;
      }
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return X_i * (A_with - A_without);
    } else {
      double delta_total = 0.0;
      double X_i = object.x_attribute.get_val(unit_i);
      double X_j = object.x_attribute.get_val(unit_j);
      
      if (X_i != 0) {
        double sum_i = 0;
        auto& neighbors_i = object.adj_list_nb.at(unit_i);
        double deg_i = neighbors_i.size();
        for (int l : neighbors_i) {
          sum_i += object.x_attribute.get_val(l);
        }
        
        double S_with_i, d_with_i, S_without_i, d_without_i;
        bool tie_exists = object.z_network.get_val(unit_i, unit_j);
        
        if (tie_exists) {
          S_with_i = sum_i;
          d_with_i = deg_i;
          S_without_i = sum_i - X_j;
          d_without_i = deg_i - 1.0;
        } else {
          S_without_i = sum_i;
          d_without_i = deg_i;
          S_with_i = sum_i + X_j;
          d_with_i = deg_i + 1.0;
        } 
        
        double A_without_i = (d_without_i > 0.5) ? (S_without_i / d_without_i) : 0.0;
        double A_with_i    = (d_with_i > 0.5)    ? (S_with_i / d_with_i)    : 0.0;
        
        delta_total += X_i * (A_with_i - A_without_i);
      }
      
      if (X_j != 0) { 
        double sum_j = 0;
        auto& neighbors_j = object.adj_list_nb.at(unit_j);
        double deg_j = neighbors_j.size();
        for (int l : neighbors_j) {
          sum_j += object.x_attribute.get_val(l);
        } 
        
        double S_with_j, d_with_j, S_without_j, d_without_j;
        bool tie_exists = object.z_network.get_val(unit_i, unit_j); 
        
        if (tie_exists) {
          S_with_j = sum_j;
          d_with_j = deg_j;
          S_without_j = sum_j - X_i; // j loses i
          d_without_j = deg_j - 1.0;
        } else {
          S_without_j = sum_j;
          d_without_j = deg_j;
          S_with_j = sum_j + X_i; // j gains i
          d_with_j = deg_j + 1.0;
        } 
        
        double A_without_j = (d_without_j > 0.5) ? (S_without_j / d_without_j) : 0.0;
        double A_with_j    = (d_with_j > 0.5)    ? (S_with_j / d_with_j)    : 0.0;
        
        delta_total += X_j * (A_with_j - A_without_j);
      }
      
      return delta_total; 
    }
  } else if (mode == "x") {
    if(object.z_network.directed){
      double total_diff = 0;
      // Part A: i's own average
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.x_attribute.get_val(j);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      
      auto& in_neighbors = object.adj_list_in_nb.at(unit_i);
      for (int k : in_neighbors) {
        double deg_k = object.adj_list_nb.at(k).size();
        if (deg_k > 0.5) {
          double X_k = object.x_attribute.get_val(k);
          total_diff +=X_k * (1.0 / deg_k);
        } 
      }
      return total_diff;
    } else {
      double total_diff = 0;
      auto& out_neighbors = object.adj_list_nb.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.x_attribute.get_val(j);
        double deg_tmp =object.adj_list_nb.at(j).size();
        if (deg_tmp > 0.5) total_diff += (object.x_attribute.get_val(j) / deg_tmp);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      return total_diff;
    }
  }   
  return 0.0;
};
EFFECT_REGISTER("spillover_xx_scaled_local", ::xyz_stat_spillover_xx_scaled, "spillover_xx_scaled_local", 0);


auto xyz_stat_spillover_xx_scaled_global = CHANGESTAT{
  // Statistic: y_i * Average(y_neighbors)
  if (mode == "z") {
    if(object.z_network.directed){
      double X_i = object.x_attribute.get_val(unit_i);
      if (X_i == 0) return 0.0; 
      double X_j = object.x_attribute.get_val(unit_j);
      
      double current_sum = 0;
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double current_deg = out_neighbors.size();
      for (int l : out_neighbors) {
        current_sum += object.x_attribute.get_val(l);
      }
      double S_with, d_with, S_without, d_without;
      bool tie_exists = object.z_network.get_val(unit_i, unit_j);
      
      if (tie_exists) {
        S_with = current_sum;
        d_with = current_deg;
        S_without = current_sum - X_j;
        d_without = current_deg - 1.0;
      } else { 
        S_without = current_sum;
        d_without = current_deg;
        S_with = current_sum + X_j;
        d_with = current_deg + 1.0;
      }
      
      double A_without = (d_without > 0.5) ? (S_without / d_without) : 0.0;
      double A_with    = (d_with > 0.5)    ? (S_with / d_with)    : 0.0;
      
      return X_i * (A_with - A_without);
    } else {
      double delta_total = 0.0;
      double X_i = object.x_attribute.get_val(unit_i);
      double X_j = object.x_attribute.get_val(unit_j);
      
      if (X_i != 0) {
        double sum_i = 0;
        auto& neighbors_i = object.z_network.adj_list.at(unit_i);
        double deg_i = neighbors_i.size();
        for (int l : neighbors_i) {
          sum_i += object.x_attribute.get_val(l);
        }
        
        double S_with_i, d_with_i, S_without_i, d_without_i;
        bool tie_exists = object.z_network.get_val(unit_i, unit_j);
        
        if (tie_exists) {
          S_with_i = sum_i;
          d_with_i = deg_i;
          S_without_i = sum_i - X_j;
          d_without_i = deg_i - 1.0;
        } else {
          S_without_i = sum_i;
          d_without_i = deg_i;
          S_with_i = sum_i + X_j;
          d_with_i = deg_i + 1.0;
        } 
        
        double A_without_i = (d_without_i > 0.5) ? (S_without_i / d_without_i) : 0.0;
        double A_with_i    = (d_with_i > 0.5)    ? (S_with_i / d_with_i)    : 0.0;
        
        delta_total += X_i * (A_with_i - A_without_i);
      }
      
      if (X_j != 0) { 
        double sum_j = 0;
        auto& neighbors_j = object.z_network.adj_list.at(unit_j);
        double deg_j = neighbors_j.size();
        for (int l : neighbors_j) {
          sum_j += object.x_attribute.get_val(l);
        } 
        
        double S_with_j, d_with_j, S_without_j, d_without_j;
        bool tie_exists = object.z_network.get_val(unit_i, unit_j); 
        
        if (tie_exists) {
          S_with_j = sum_j;
          d_with_j = deg_j;
          S_without_j = sum_j - X_i; // j loses i
          d_without_j = deg_j - 1.0;
        } else {
          S_without_j = sum_j;
          d_without_j = deg_j;
          S_with_j = sum_j + X_i; // j gains i
          d_with_j = deg_j + 1.0;
        } 
        
        double A_without_j = (d_without_j > 0.5) ? (S_without_j / d_without_j) : 0.0;
        double A_with_j    = (d_with_j > 0.5)    ? (S_with_j / d_with_j)    : 0.0;
        
        delta_total += X_j * (A_with_j - A_without_j);
      }
      
      return delta_total; 
    }
  } else if (mode == "x") {
    if(object.z_network.directed){
      double total_diff = 0;
      // Part A: i's own average
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.x_attribute.get_val(j);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      
      auto& in_neighbors = object.z_network.adj_list_in.at(unit_i);
      for (int k : in_neighbors) {
        double deg_k = object.z_network.adj_list.at(k).size();
        if (deg_k > 0.5) {
          double X_k = object.x_attribute.get_val(k);
          total_diff +=X_k * (1.0 / deg_k);
        } 
      }
      return total_diff;
    } else {
      double total_diff = 0;
      auto& out_neighbors = object.z_network.adj_list.at(unit_i);
      double sum_i = 0; 
      double deg_i = out_neighbors.size();
      for (int j : out_neighbors) {
        sum_i += object.x_attribute.get_val(j);
        double deg_tmp =object.z_network.adj_list.at(j).size();
        if (deg_tmp > 0.5) total_diff += (object.x_attribute.get_val(j) / deg_tmp);
      } 
      if (deg_i > 0.5) total_diff += (sum_i / deg_i);
      return total_diff;
    }
  }   
  return 0.0;
};
EFFECT_REGISTER("spillover_xx_scaled_global", ::xyz_stat_spillover_xx_scaled_global, "spillover_xx_scaled_global", 0);

static inline bool has_alternative_h_to_j(
    int h, int excluded, int j,
    const XYZ_class &object,
    const std::vector<int> &in_connections_of_j_nb,
    const std::vector<int> &neighborhood_h,
    const std::vector<int> &neighborhood_j
) {
  if (h >= object.z_network.adj_list.size()) return false;
  const auto &out_h = object.z_network.adj_list.at(h);
  
  for (int k : out_h) {
    if (k == excluded) continue;
    if (std::find(neighborhood_h.begin(), neighborhood_h.end(), k) != neighborhood_h.end() &&
        std::find(neighborhood_j.begin(), neighborhood_j.end(), k) != neighborhood_j.end() &&
        std::find(in_connections_of_j_nb.begin(), in_connections_of_j_nb.end(), k) != in_connections_of_j_nb.end()) {
      return true;
    }
  }
  return false;
}

static inline bool has_alternative_i_to_h(
    int i, int excluded, int h,
    const XYZ_class &object,
    const std::vector<int> &out_connections_of_i_nb,
    const std::vector<int> &neighborhood_h,
    const std::vector<int> &neighborhood_i
) {
  if (h >= object.z_network.adj_list_in.size()) return false;
  const auto &in_h = object.z_network.adj_list_in.at(h);
  
  for (int k : in_h) {
    if (k == excluded) continue;
    if (std::find(neighborhood_h.begin(), neighborhood_h.end(), k) != neighborhood_h.end() &&
        std::find(neighborhood_i.begin(), neighborhood_i.end(), k) != neighborhood_i.end() &&
        std::find(out_connections_of_i_nb.begin(), out_connections_of_i_nb.end(), k) != out_connections_of_i_nb.end()) {
      return true;
    }
  }
  return false;
}


auto xyz_stat_transitive_edges = CHANGESTAT {
  if (mode != "z") return 0.0;
  if (object.z_network.directed && 
     (unit_i >= object.z_network.adj_list_in.size() || 
      unit_j >= object.z_network.adj_list_in.size()))
  {
    return 0.0;
  }
  
  double res = 0.0;
  
  const auto &neighborhood_i = object.neighborhood.at(unit_i);
  const auto &neighborhood_j = object.neighborhood.at(unit_j);
  const auto &out_i_all = object.z_network.adj_list.at(unit_i);
  const auto &out_j_all = object.z_network.adj_list.at(unit_j);
  
  if (object.z_network.directed) {
    const auto &in_i_all = object.z_network.adj_list_in.at(unit_i);
    const auto &in_j_all = object.z_network.adj_list_in.at(unit_j);
    
    std::vector<int> in_connections_of_j_nb;
    in_connections_of_j_nb.reserve(in_j_all.size());
    for (int n : in_j_all) if (std::find(neighborhood_j.begin(), neighborhood_j.end(), n) != neighborhood_j.end()) in_connections_of_j_nb.push_back(n);
    
    std::vector<int> out_connections_of_i_nb;
    out_connections_of_i_nb.reserve(out_i_all.size());
    for (int n : out_i_all) if (std::find(neighborhood_i.begin(), neighborhood_i.end(), n) != neighborhood_i.end()) out_connections_of_i_nb.push_back(n);
    
    int simple_transitivity_count = 0;
    const auto *small_nb = (&neighborhood_i);
    const auto *large_nb = (&neighborhood_j);
    if (neighborhood_j.size() < neighborhood_i.size()) { small_nb = &neighborhood_j; large_nb = &neighborhood_i; }
    for (int h : *small_nb) {
      if (h == unit_i || h == unit_j) continue;
      if (std::find(large_nb->begin(), large_nb->end(), h) == large_nb->end()) continue; 
      if (std::find(out_i_all.begin(), out_i_all.end(), h) != out_i_all.end() && std::find(in_j_all.begin(), in_j_all.end(), h) != in_j_all.end()) {
        simple_transitivity_count = 1; 
        break;
      }
    }
    if (simple_transitivity_count) res += 1;
    
    bool check_cond_part2 = std::find(neighborhood_j.begin(), neighborhood_j.end(), unit_i) != neighborhood_j.end();
    
    if (check_cond_part2) {
      const auto *small_in = &in_i_all;
      const auto *large_in = &in_j_all;
      if (in_j_all.size() < in_i_all.size()) { small_in = &in_j_all; large_in = &in_i_all; }
      
      for (int h : *small_in) {
        if (h == unit_i || h == unit_j) continue;
        if (std::find(large_in->begin(), large_in->end(), h) == large_in->end()) continue;
        if (std::find(object.neighborhood.at(h).begin(), object.neighborhood.at(h).end(), unit_i) == object.neighborhood.at(h).end()) continue;
        
        if (!has_alternative_h_to_j(h, unit_i, unit_j, object, in_connections_of_j_nb,
                                    object.neighborhood.at(h), neighborhood_j)) {
          res += 1;
        }
      }
    }
    
    bool check_cond_part3 = std::find(neighborhood_i.begin(), neighborhood_i.end(), unit_j) != neighborhood_i.end();
    
    if (check_cond_part3) {
      const auto *small_out = &out_i_all;
      const auto *large_out = &out_j_all;
      if (out_j_all.size() < out_i_all.size()) { small_out = &out_j_all; large_out = &out_i_all; }
      
      for (int h : *small_out) {
        if (h == unit_i || h == unit_j) continue;
        if (std::find(large_out->begin(), large_out->end(), h) == large_out->end()) continue;
        if (std::find(object.neighborhood.at(h).begin(), object.neighborhood.at(h).end(), unit_j) == object.neighborhood.at(h).end()) continue;
        
        if (!has_alternative_i_to_h(unit_i, unit_j, h, object, out_connections_of_i_nb,
                                    object.neighborhood.at(h), neighborhood_i)) {
          res += 1;
        }
      }
    }
  }
  else { // undirected
    const auto *small_conn = &out_i_all;
    const auto *large_conn = &out_j_all;
    if (out_j_all.size() < out_i_all.size()) { small_conn = &out_j_all; large_conn = &out_i_all; }
    
    int simple_triangle_count = 0;
    std::vector<int> common_neighbors;
    common_neighbors.reserve(std::min(small_conn->size(), large_conn->size()));
    
    for (int h : *small_conn) {
      if (h == unit_i || h == unit_j) continue;
      if (std::find(large_conn->begin(), large_conn->end(), h) == large_conn->end()) continue;
      common_neighbors.push_back(h);
      if (std::find(neighborhood_i.begin(), neighborhood_i.end(), h) != neighborhood_i.end() && std::find(neighborhood_j.begin(), neighborhood_j.end(), h) != neighborhood_j.end()) {
        simple_triangle_count = 1;
      }
    }
    if (simple_triangle_count) res += 1;
    bool check_cond_part2 = std::find(neighborhood_j.begin(), neighborhood_j.end(), unit_i) != neighborhood_j.end();
    if (check_cond_part2 && !common_neighbors.empty()) {
      std::vector<int> connections_of_i_nb;
      connections_of_i_nb.reserve(out_i_all.size());
      for (int n : out_i_all) if (std::find(neighborhood_i.begin(), neighborhood_i.end(), n) != neighborhood_i.end()) connections_of_i_nb.push_back(n);
      std::vector<int> connections_of_j_nb;
      connections_of_j_nb.reserve(out_j_all.size());
      for (int n : out_j_all) if (std::find(neighborhood_j.begin(), neighborhood_j.end(), n) != neighborhood_j.end()) connections_of_j_nb.push_back(n);
      
      for (int h : common_neighbors) {
        if (std::find(object.neighborhood.at(h).begin(), object.neighborhood.at(h).end(), unit_j) != object.neighborhood.at(h).end()) {
          if (!has_alternative_h_to_j(h, unit_j, unit_i, object, connections_of_i_nb,
                                      object.neighborhood.at(h), neighborhood_i)) {
            res += 1;
          }
        }
        if (std::find(object.neighborhood.at(h).begin(), object.neighborhood.at(h).end(), unit_i) != object.neighborhood.at(h).end()) {
          if (!has_alternative_h_to_j(h, unit_i, unit_j, object, connections_of_j_nb,
                                      object.neighborhood.at(h), neighborhood_j)) {
            res += 1;
          }
        }
      }
    }
  } // end undirected
  
  return static_cast<double>(res);
};
EFFECT_REGISTER("transitive", ::xyz_stat_transitive_edges, "transitive", 0);

auto xyz_stat_nonisolates= CHANGESTAT{
  if(mode == "z"){ 
    int degree_i,degree_j;  
    if(object.z_network.directed){
      degree_i = object.z_network.out_degrees[unit_i] + 
        object.z_network.in_degrees[unit_i];
      degree_j = object.z_network.out_degrees[unit_j] + 
        object.z_network.in_degrees[unit_j];
    } else {
      degree_i = object.z_network.out_degrees[unit_i];
      degree_j = object.z_network.out_degrees[unit_j];
    }
    // If the edge is already there, we need to substract one of the degrees
    if(object.z_network.get_val(unit_i, unit_j)){
      degree_i -= 1;
      degree_j -= 1;
    }
    return((degree_i == 0) +(degree_j == 0));
  }else {
    return(0);
  }  
};
EFFECT_REGISTER("nonisolates", ::xyz_stat_nonisolates, "nonisolates", 0);

auto xyz_stat_isolates= CHANGESTAT{
  if(mode == "z"){ 
    arma::vec res(3);
    int degree_i,degree_j;  
    if(object.z_network.directed){
      degree_i = object.z_network.out_degrees[unit_i] + 
        object.z_network.in_degrees[unit_i];
      degree_j = object.z_network.out_degrees[unit_j] + 
        object.z_network.in_degrees[unit_j];
    } else {
      degree_i = object.z_network.out_degrees[unit_i];
      degree_j = object.z_network.out_degrees[unit_j];
    } 
    // If the edge is already there, we need to substract one of the degrees
    if(object.z_network.get_val(unit_i, unit_j)){
      degree_i -= 1;
      degree_j -= 1;
    }
    return(-(degree_i == 0)-(degree_j == 0));
  }else {
    return(0);
  }  
};
EFFECT_REGISTER("isolates", ::xyz_stat_isolates, "isolates", 1.0);

auto xyz_stat_gwesp_local_ITP= CHANGESTAT{
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    }
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    
    // 1. Step: For all ISP of i and j 
    std::vector<int> itp_ij = object.get_common_partners_nb(unit_i, unit_j, "ITP");
    double res = expo_pos*(1- pow(expo_min, 
                                  itp_ij.size()));
    // 2. Step: For all h in ITP of i and j check their ISP between j and h 
    
    
    for (int k : itp_ij) {
      tmp_count = object.count_common_partners_nb(unit_j, k, "ITP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
      tmp_count = object.count_common_partners_nb(k,unit_i, "ITP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else {  
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_local_ITP", ::xyz_stat_gwesp_local_ITP, "gwesp_local_ITP",0.0);

auto xyz_stat_gwesp_local_ISP= CHANGESTAT{
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    }
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    // 1. Step: For all ISP of i and j 
    double res = expo_pos*(1- pow(expo_min, 
                                  object.count_common_partners_nb(unit_i, unit_j, "ISP")));
    // 2. Step: For all h in OSP of i and j check their ISP between j and h 
    std::vector<int> osp_ij = object.get_common_partners_nb(unit_i, unit_j, "OSP");
    
    
    for (int k : osp_ij) {
      tmp_count = object.count_common_partners_nb(unit_j, k, "ISP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    // 3. Step: For all h in OTP of i and j check their ISP between h and j
    std::vector<int> otp_ij = object.get_common_partners_nb(unit_i, unit_j, "OTP");
    for (int k : otp_ij) {
      tmp_count = object.count_common_partners_nb(k, unit_j, "ISP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    } 
    return(res); 
  }else {
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_local_ISP", ::xyz_stat_gwesp_local_ISP, "gwesp_local_ISP",0.0);

auto xyz_stat_gwesp_local_symm= CHANGESTAT{
  if(mode == "z"){
    if(object.z_network.directed){
      Rcpp::stop("This statistic is only for undirected networks");  
    }
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    } 
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    
    // 1. Step: For all OTP of i and j 
    // 1. Step: 
    std::vector<int> osp_ij = object.get_common_partners_nb(unit_i, unit_j);
    double res = expo_pos*(1- pow(expo_min, osp_ij.size()));
    
    
    
    for (int k : osp_ij) {
      tmp_count = object.count_common_partners_nb(unit_i, k);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
      tmp_count = object.count_common_partners_nb(unit_j, k);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else { 
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_local_symm", ::xyz_stat_gwesp_local_symm, "gwesp_local_symm",0.0);

auto xyz_stat_gwesp_global_symm= CHANGESTAT{
  if(mode == "z"){
    if(object.z_network.directed){
      Rcpp::stop("This statistic is only for undirected networks");  
    }
    
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    
    // 1. Step: For all common partner of i and j 
    std::vector<int> osp_ij = object.z_network.get_common_partners(unit_i, unit_j);
    double res = expo_pos*(1- pow(expo_min, osp_ij.size()));
    
    
    
    for (int k : osp_ij) {
      tmp_count = object.z_network.count_common_partners(unit_i, k);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
      tmp_count = object.z_network.count_common_partners(unit_j, k);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else { 
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_global_symm", ::xyz_stat_gwesp_global_symm, "gwesp_global_symm",0.0);



auto xyz_stat_gwesp_local_OTP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    } 
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    
    // 1. Step: For all OTP of i and j 
    
    double res = expo_pos*(1- pow(expo_min, 
                                  object.count_common_partners_nb(unit_i, unit_j, "OTP")));
    // 2. Step: 
    std::vector<int> osp_ij = object.get_common_partners_nb(unit_i, unit_j, "OSP");
    
    for (int k : osp_ij) {
      tmp_count = object.count_common_partners_nb(unit_i, k, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    // 3. Step:
    std::vector<int> isp_ij = object.get_common_partners_nb(unit_i, unit_j, "ISP");
    for (int k : isp_ij) {
      tmp_count = object.count_common_partners_nb(k, unit_j, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else { 
    return(0.0);
  }
}; 
EFFECT_REGISTER("gwesp_local_OTP", ::xyz_stat_gwesp_local_OTP, "gwesp_local_OTP",0.0);

auto xyz_stat_gwesp_local_OSP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    }  
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    
    // 1. Step: For all OSP of i and j 
    double res = expo_pos*(1- pow(expo_min, 
                                  object.count_common_partners_nb(unit_i, unit_j, "OSP")));
    // 2. Step: 
    
    std::vector<int> otp_ij = object.get_common_partners_nb(unit_i, unit_j, "OTP");
    for (int k : otp_ij) {
      tmp_count = object.count_common_partners_nb(unit_i, k, "OSP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    // 3. Step:
    std::vector<int> isp_ij = object.get_common_partners_nb(unit_i, unit_j, "ISP");
    for (int k : isp_ij) {
      tmp_count = object.count_common_partners_nb(k, unit_i, "OSP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else { 
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_local_OSP", ::xyz_stat_gwesp_local_OSP, "gwesp_local_OSP",0.0);

auto xyz_stat_gwesp_ITP = CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    
    
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    std::vector<int> itp_ij = object.get_common_partners_nb(unit_i, unit_j, "ITP");
    
    if (itp_ij.empty()) return 0.0;
    double total_change = 0;
    total_change +=expo_pos*(1- pow(expo_min, itp_ij.size()));
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : itp_ij) {
      tmp_count = object.count_common_partners_nb(unit_j, k, "ITP");
      total_change += std::pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count);
      tmp_count = object.count_common_partners_nb(k, unit_i, "ITP");
      total_change += std::pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count);
    } 
    return total_change;
  } else {  
    return 0.0;
  } 
};
EFFECT_REGISTER("gwesp_global_ITP", ::xyz_stat_gwesp_ITP, "gwesp_global_ITP", 0.0);

auto xyz_stat_gwesp_ISP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    // 1. Step: For all ISP of i and j 
    double res = expo_pos*(1- pow(expo_min, 
                                  object.count_common_partners(unit_i, unit_j, "ISP")));
    // 2. Step: For all h in OSP of i and j check their ISP between j and h 
    std::vector<int> osp_ij = object.get_common_partners(unit_i, unit_j, "OSP");
    
    // Check if the edge (i,j) currently exists physically in the object
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : osp_ij) {
      tmp_count = object.count_common_partners(unit_j, k, "ISP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    // 3. Step: For all h in OTP of i and j check their ISP between h and j
    std::vector<int> otp_ij = object.get_common_partners(unit_i, unit_j, "OTP");
    for (int k : otp_ij) {
      tmp_count = object.count_common_partners(k, unit_j, "ISP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else {
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_global_ISP", ::xyz_stat_gwesp_ISP, "gwesp_global_ISP",0.0);

auto xyz_stat_gwesp_OTP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    // 1. Step: For all OTP of i and j 
    
    double res = expo_pos*(1- pow(expo_min, 
                                  object.count_common_partners(unit_i, unit_j, "OTP")));
    // 2. Step: 
    std::vector<int> osp_ij = object.get_common_partners(unit_i, unit_j, "OSP");
    
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : osp_ij) {
      tmp_count = object.count_common_partners(unit_i, k, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    // 3. Step:
    std::vector<int> isp_ij = object.get_common_partners(unit_i, unit_j, "ISP");
    for (int k : isp_ij) {
      tmp_count = object.count_common_partners(k, unit_j, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else {  
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_global_OTP", ::xyz_stat_gwesp_OTP, "gwesp_global_OTP",0.0);

auto xyz_stat_gwesp_OSP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double expo_pos = exp(data.at(0,0));
    // 1. Step: For all OSP of i and j 
    double res = expo_pos*(1- pow(expo_min, 
                                  object.count_common_partners(unit_i, unit_j, "OSP")));
    // 2. Step: 
    
    std::vector<int> otp_ij = object.get_common_partners(unit_i, unit_j, "OTP");
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : otp_ij) {
      tmp_count = object.count_common_partners(unit_i, k, "OSP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    // 3. Step:
    std::vector<int> isp_ij = object.get_common_partners(unit_i, unit_j, "ISP");
    for (int k : isp_ij) {
      tmp_count = object.count_common_partners(k, unit_i, "OSP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }
    return(res);
  }else { 
    return(0);
  }
}; 
EFFECT_REGISTER("gwesp_global_OSP", ::xyz_stat_gwesp_OSP, "gwesp_global_OSP",0.0);

auto xyz_stat_gwdsp_symm= CHANGESTAT{
  if(object.z_network.directed){
    Rcpp::stop("This statistic is only for undirected networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    // 1. Step: 
    
    auto& out_j = object.z_network.adj_list.at(unit_j);
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : out_j) {
      if(unit_i == k) continue;
      tmp_count = object.count_common_partners(unit_i, k);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }  
    // 2. Step: 
    auto& out_i = object.z_network.adj_list.at(unit_i);
    for (int k : out_i) {
      if(unit_j == k) continue;
      tmp_count = object.count_common_partners(k, unit_j);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }  
    return(res);
  }else {     
    return(0);
  } 
}; 
EFFECT_REGISTER("gwdsp_global_symm", ::xyz_stat_gwdsp_symm, "gwdsp_global_symm",0.0);

auto xyz_stat_gwdsp_local_symm= CHANGESTAT{
  if(object.z_network.directed){
    Rcpp::stop("This statistic is only for undirected networks");  
  }
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    } 
    
    double res = 0.0;
    // 1. Step: 
    
    auto& out_j = object.adj_list_nb.at(unit_j);
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : out_j) {
      if(unit_i == k) continue;
      tmp_count = object.count_common_partners_nb(unit_i, k);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }   
    // 2. Step: 
    auto& out_i = object.z_network.adj_list.at(unit_i);
    for (int k : out_i) {
      if(unit_j == k) continue;
      tmp_count = object.count_common_partners_nb(k, unit_j);
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }   
    return(res);
  }else {      
    return(0);
  } 
}; 
EFFECT_REGISTER("gwdsp_local_symm", ::xyz_stat_gwdsp_local_symm, "gwdsp_local_symm",0.0);


auto xyz_stat_gwdsp_ITP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    // 1. Step: 
    
    auto& out_j = object.z_network.adj_list.at(unit_j);
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : out_j) {
      if(unit_i == k) continue;
      tmp_count = object.count_common_partners(unit_i, k, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    } 
    // 2. Step: 
    auto& in_i = object.z_network.adj_list_in.at(unit_i);
    for (int k : in_i) {
      if(unit_j == k) continue;
      tmp_count = object.count_common_partners(k, unit_j, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    } 
    return(res);
  }else {    
    return(0);
  } 
}; 
EFFECT_REGISTER("gwdsp_global_ITP", ::xyz_stat_gwdsp_ITP, "gwdsp_global_ITP",0.0);
EFFECT_REGISTER("gwdsp_global_OTP", ::xyz_stat_gwdsp_ITP, "gwdsp_global_OTP",0.0);

auto xyz_stat_gwdsp_ISP= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    // 1. Step: 
    
    auto& out_i = object.z_network.adj_list.at(unit_i);
    for (int k : out_i) {
      if(unit_j == k) continue;
      tmp_count = object.count_common_partners(unit_j, k, "ISP");
      if (edge_exists) {
        tmp_count = (tmp_count > 0) ? (tmp_count - 1) : 0.0; 
      }
      res += 2.0*pow(expo_min, tmp_count); 
    }
    return(res);
  }else {   
    return(0);
  }
};  
EFFECT_REGISTER("gwdsp_global_ISP", ::xyz_stat_gwdsp_ISP, "gwdsp_global_ISP",0.0);


auto xyz_stat_gwdsp_OSP= CHANGESTAT{
  
  
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    // 1. Step:
    
    auto& in_j = object.z_network.adj_list_in.at(unit_j);
    for (int k : in_j) {
      if(unit_i == k) continue;
      tmp_count = object.count_common_partners(unit_i, k, "OSP");
      if (edge_exists) {
        tmp_count = (tmp_count > 0) ? (tmp_count - 1) : 0.0; 
      }
      res += 2.0*pow(expo_min, tmp_count); 
    }
    return(res);
  }else {  
    return(0);
  }
}; 
EFFECT_REGISTER("gwdsp_global_OSP", ::xyz_stat_gwdsp_OSP, "gwdsp_global_OSP",0.0);


auto xyz_stat_gwdsp_ITP_local= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    // 1. Step: 
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    }
    
    auto& out_j = object.adj_list_nb.at(unit_j);
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    for (int k : out_j) {
      if(unit_i == k) continue;
      tmp_count = object.count_common_partners_nb(unit_i, k, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    } 
    // 2. Step: 
    auto& in_i = object.adj_list_in_nb.at(unit_i);
    for (int k : in_i) {
      if(unit_j == k) continue;
      tmp_count = object.count_common_partners_nb(k, unit_j, "OTP");
      res += pow(expo_min, edge_exists ? (tmp_count - 1) : tmp_count); 
    }  
    return(res);
  }else {     
    return(0);
  } 
}; 
EFFECT_REGISTER("gwdsp_local_ITP", ::xyz_stat_gwdsp_ITP_local, "gwdsp_local_ITP",0.0);
EFFECT_REGISTER("gwdsp_local_OTP", ::xyz_stat_gwdsp_ITP_local, "gwdsp_local_OTP",0.0);

auto xyz_stat_gwdsp_ISP_local= CHANGESTAT{
  if(mode == "z"){
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    }
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    // 1. Step: 
    
    auto& out_i = object.adj_list_nb.at(unit_i);
    for (int k : out_i) {
      if(unit_j == k) continue;
      tmp_count = object.count_common_partners_nb(unit_j, k, "ISP");
      if (edge_exists) {
        tmp_count = (tmp_count > 0) ? (tmp_count - 1) : 0.0; 
      }
      res += 2.0*pow(expo_min, tmp_count); 
    } 
    return(res);
  }else {    
    return(0);
  } 
};  
EFFECT_REGISTER("gwdsp_local_ISP", ::xyz_stat_gwdsp_ISP_local, "gwdsp_local_ISP",0.0);

auto xyz_stat_gwdsp_OSP_local= CHANGESTAT{
  if(!object.z_network.directed){
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if(mode == "z"){
    if(object.get_val_overlap(unit_i, unit_j) == false){
      return(0);
    }
    double expo_min = (1-exp(-data.at(0,0)));  
    double res = 0.0;
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count;
    // 1. Step:
    
    auto& in_j = object.adj_list_in_nb.at(unit_j);
    for (int k : in_j) {
      if(unit_i == k) continue;
      tmp_count = object.count_common_partners_nb(unit_i, k, "OSP");
      if (edge_exists) {
        tmp_count = (tmp_count > 0) ? (tmp_count - 1) : 0.0; 
      } 
      res += 2.0*pow(expo_min, tmp_count); 
    }
    return(res); 
  }else {  
    return(0);
  }
}; 

EFFECT_REGISTER("gwdsp_local_OSP", ::xyz_stat_gwdsp_OSP_local, "gwdsp_local_OSP",0.0);

auto xyz_stat_gwidegree = CHANGESTAT {
  if (!object.z_network.directed) {
    Rcpp::stop("This statistic is only for directed networks");  
  }
  
  if (mode == "z") {
    double decay = (data.n_elem >= 1) ? data.at(0, 0) : 0.0;
    double expo_min = (1.0 - exp(-decay));  
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count = object.z_network.in_degrees[unit_j];
    if (edge_exists) {
      tmp_count = (tmp_count > 0) ? (tmp_count - 1.0) : 0.0; 
    }
    return pow(expo_min, tmp_count);
  } else {  
    return 0.0;
  }
}; 
EFFECT_REGISTER("gwidegree_global", ::xyz_stat_gwidegree, "gwidegree_global", 0.0);

auto xyz_stat_gwodegree = CHANGESTAT {
  if (mode == "z") {
    double decay = (data.n_elem >= 1) ? data.at(0, 0) : 0.0;
    double expo_min = (1.0 - exp(-decay));
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double res = 0.0;

    // Node i as sender
    double tmp_count = object.z_network.out_degrees[unit_i];
    if (edge_exists) {
      tmp_count = (tmp_count > 0) ? (tmp_count - 1.0) : 0.0; 
    }
    res += pow(expo_min, tmp_count);

    // Add Node j as sender for undirected networks
    if (!object.z_network.directed) {
      double tmp_count_j = object.z_network.out_degrees[unit_j];
      if (edge_exists) {
        tmp_count_j = (tmp_count_j > 0) ? (tmp_count_j - 1.0) : 0.0; 
      }
      res += pow(expo_min, tmp_count_j);
    }
    return res;
  } else {  
    return 0.0;
  }
}; 
EFFECT_REGISTER("gwodegree_global", ::xyz_stat_gwodegree, "gwodegree_global", 0.0);
EFFECT_REGISTER("gwdegree_global", ::xyz_stat_gwodegree, "gwdegree_global", 0.0);

auto xyz_stat_gwidegree_local = CHANGESTAT {
  if (!object.z_network.directed) {
    Rcpp::stop("This statistic is only for directed networks");  
  }
  if (mode == "z") {
    if (!object.get_val_overlap(unit_i, unit_j)) {
      return 0.0;
    }
    double decay = (data.n_elem >= 1) ? data.at(0, 0) : 0.0;
    double expo_min = (1.0 - exp(-decay));  
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double tmp_count = object.in_degrees_nb[unit_j];
    if (edge_exists) {
      tmp_count = (tmp_count > 0) ? (tmp_count - 1.0) : 0.0; 
    }
    return pow(expo_min, tmp_count);
  } else {  
    return 0.0;
  }
}; 
EFFECT_REGISTER("gwidegree_local", ::xyz_stat_gwidegree_local, "gwidegree_local", 0.0);

auto xyz_stat_gwodegree_local = CHANGESTAT {
  if (mode == "z") {
    if (!object.get_val_overlap(unit_i, unit_j)) {
      return 0.0;
    }
    double decay = (data.n_elem >= 1) ? data.at(0, 0) : 0.0;
    double expo_min = (1.0 - exp(-decay));  
    bool edge_exists = object.z_network.get_val(unit_i, unit_j);
    double res = 0.0;

    // Node i as sender
    double tmp_count = object.out_degrees_nb[unit_i];
    if (edge_exists) {
      tmp_count = (tmp_count > 0) ? (tmp_count - 1.0) : 0.0; 
    }
    res += pow(expo_min, tmp_count);

    // Add Node j as sender for undirected networks
    if (!object.z_network.directed) {
      double tmp_count_j = object.out_degrees_nb[unit_j];
      if (edge_exists) {
        tmp_count_j = (tmp_count_j > 0) ? (tmp_count_j - 1.0) : 0.0; 
      }
      res += pow(expo_min, tmp_count_j);
    }
    return res;
  } else {  
    return 0.0;
  }
}; 
EFFECT_REGISTER("gwodegree_local", ::xyz_stat_gwodegree_local, "gwodegree_local", 0.0);
EFFECT_REGISTER("gwdegree_local", ::xyz_stat_gwodegree_local, "gwdegree_local", 0.0);


