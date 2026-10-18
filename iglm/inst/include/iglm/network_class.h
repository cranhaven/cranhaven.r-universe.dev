// Defines a header file containing function signatures for functions in src/

// Protect signatures using an inclusion guard.
#ifndef network_class_H
#define network_class_H
#define DARMA_USE_CURRENT
#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include "iglm/helper_functions.h"

class IGLM_API Network {
public:
  // Members
  bool directed;
  unsigned int number_edges; 
  std::vector<std::vector<int>> adj_list;
  std::vector<std::vector<int>> adj_list_in;
  std::vector<char> adj_mat;
  std::vector<int> out_degrees;
  std::vector<int> in_degrees;

  inline size_t get_mat_idx(int from, int to) const {
    if (from < 1 || from > n_units || to < 1 || to > n_units) {
        return 0; // Or some safe default, though get_val will check it
    }
    return (size_t)(from - 1) * n_units + (to - 1);
  }

  // Constructors
  Network (int n_units_, bool directed_);
  Network (int n_units_, bool directed_, arma::mat mat);
  
  void set_network_from_mat(int n_units_, bool directed_, arma::mat mat);
  void change_edge(int from, int to);
  
  size_t count_common_partners(unsigned int from, unsigned int to, std::string type = "OSP") const;
  std::vector<int> get_common_partners(unsigned int from,unsigned int to, std::string type = "OSP")const;
  
  double count_edges() const;
  
  inline double get_val(int from, int to) const {
    if (from < 1 || from > n_units || to < 1 || to > n_units) {
        return 0.0;
    }
    size_t idx = get_mat_idx(from, to);
    if (idx >= adj_mat.size()) return 0.0; 
    return adj_mat[idx] ? 1.0 : 0.0;
  }
  
  int get_n_units() const { return n_units; }
  
  void add_edge(int from, int to);
  void delete_edge(int from, int to);
  void add_edges_from_mat(arma::mat mat);
  
private:
  int n_units;
};
#endif
