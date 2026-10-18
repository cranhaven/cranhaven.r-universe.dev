#pragma once

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include "xz_class.h"
#define DARMA_USE_CURRENT

class IGLM_API XYZ_class: public XZ_class {
public:
  // Additional Member
  Attribute y_attribute;
  // Constructors
  XYZ_class(int n_units_, bool directed_, std::string family_x_,std::string family_y_, 
                       double scale_x_, double scale_y_): XZ_class(n_units_,directed_, family_x_, scale_x_), y_attribute(n_units_, family_y_, scale_y_){
    
  } 
  XYZ_class(int n_units_, bool directed_, arma::mat neighborhood_, arma::mat overlap_, std::string family_x_,std::string family_y_, double scale_x_, double scale_y_): 
    XZ_class(n_units_,directed_, neighborhood_, overlap_, family_x_, scale_x_), y_attribute(n_units_, family_y_, scale_y_){
  }
  
  XYZ_class(int n_units_, bool directed_, std::vector<std::vector<int>> neighborhood_,
                       std::vector<std::vector<int>> overlap_, 
                       arma::mat overlap_mat_, 
                       std::string family_x_,std::string family_y_,
                       double scale_x_, double scale_y_): XZ_class(n_units_,directed_, neighborhood_, overlap_, overlap_mat_, family_x_, scale_x_),  y_attribute(n_units_, family_y_, scale_y_){
  }
  
  
  XYZ_class(int n_units_, bool directed_,  arma::vec x_attribute_, arma::vec y_attribute_, arma::mat z_network_,arma::mat neighborhood_, 
                       arma::mat overlap_, std::string family_x_,std::string family_y_, double scale_x_, double scale_y_):
    XZ_class(n_units_,directed_, z_network_,x_attribute_, neighborhood_,overlap_, family_x_, scale_x_),  y_attribute(n_units_,y_attribute_, family_y_, scale_y_){
  }
  
  void print();
  
  void set_info(arma::vec x_attribute_,arma::vec y_attribute_,
                           std::vector<std::vector<int>> z_network_) {
    // Deprecated for vector refactor
  }
  
  void set_info_arma(arma::vec x_attribute_, arma::vec y_attribute_, arma::mat z_network_);
  
  void copy_from(const XYZ_class& obj);
  
};

