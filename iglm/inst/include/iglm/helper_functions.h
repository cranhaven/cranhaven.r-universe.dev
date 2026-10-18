
#ifndef helper_H
#define helper_H

// #include <Rcpp.h>
#include <RcppArmadillo.h>
#include <set>
#include <unordered_map>
#include <iostream>
#include <vector>
#include <algorithm>
#ifndef IGLM_API
#if defined(_WIN32)
#ifdef IGLM_COMPILING_IGLM
#define IGLM_API __declspec(dllexport)
#else
#define IGLM_API __declspec(dllimport)
#endif
#else
#define IGLM_API __attribute__ ((visibility ("default")))
#endif
#endif
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

template<typename T>
void print_vector(std::vector<T> tmp){
  // std::vector<T>::iterator itr;
  for (std::vector<int>::iterator it = tmp.begin() ; it != tmp.end(); ++it) {
    Rcout << *it << std::endl;
  }
  // for (itr = tmp.begin(); itr != tmp.end(); itr++) {
  //
  // }
}



template<typename T>
std::vector<int> findItems(std::vector<T> const &v, int target) {
  std::vector<int> indices;
  auto it = v.begin();
  while ((it = std::find_if(it, v.end(), [&] (T const &e) { return e == target; }))
           != v.end())
  {
    indices.push_back(std::distance(v.begin(), it));
    it++;
  }
  return indices;
}

inline void print_set(std::unordered_set<int> tmp){
  std::unordered_set<int>::iterator itr;
  for (itr = tmp.begin(); itr != tmp.end(); itr++) {
    Rcout << *itr << std::endl;
  }
}



// This is a function to transform a map to one matrix
inline arma::mat map_to_mat(std::unordered_map< int, std::unordered_set<int>> &adj_list,
                     int & n_units) {
  arma::mat res(n_units,n_units);
  res.fill(0);
  arma::mat tmp_row(n_units,1);
  NumericVector tmp_vec(n_units);
  for (int i = 1; i <= n_units; i++){
    // Set the vector to 0
    tmp_row.fill(0);
    tmp_vec = adj_list[i];
    // Go through each row and write the matrix
    for (int j = 0; j < tmp_vec.length() ; j++){
      tmp_row.at(tmp_vec.at(j)-1) = 1;
    }
    res.col(i-1) = tmp_row.as_col();
  }
  return(res);
}

// This is a function to transform a matrix to a map
inline void mat_to_map(arma::mat mat, int n_units, bool directed,
                       std::unordered_map< int, std::unordered_set<int>> &adj_list,
                       std::unordered_map< int, std::unordered_set<int>> &adj_list_in) {

  for (int i = 1; i <= n_units; i++){ 
    adj_list[i] = std::unordered_set<int>();
  } 
  if(directed){
    for (int i = 1; i <= n_units; i++){
      adj_list_in[i] = std::unordered_set<int>();
    }  
  }
  if (mat.is_empty() || mat.n_elem == 0) {
    return;
  }
  //  This checks whether an edge list or adjacency matrix is provided
  if(mat.n_cols==2){
    if(directed){
      arma::vec tmp_row1 = mat.col(0);
      arma::vec tmp_row2 = mat.col(1);
      for (arma::uword i = 1; i <= n_units; i++){
        arma::vec ids1 = tmp_row1.elem(find(mat.col(1) == i)); // Find indices
        arma::vec ids2 = tmp_row2.elem(find(mat.col(0) == i)); // Find indices
        adj_list.at(i)= std::unordered_set<int>(ids2.begin(), ids2.end());
        adj_list_in.at(i)= std::unordered_set<int>(ids1.begin(), ids1.end());
      }
    } else { 
      arma::vec tmp_row1 = mat.col(0);
      arma::vec tmp_row2 = mat.col(1);

      // double u = 0.0;
      for (arma::uword i = 1; i <= n_units; i++){
        std::unordered_set<int> part1,part2, res;
        arma::vec ids1 = tmp_row1.elem(find(mat.col(1) == i)); // Find indices from
        arma::vec ids2 = tmp_row2.elem(find(mat.col(0) == i)); // Find indices to
        part1= std::unordered_set<int>(ids1.begin(), ids1.end());
        part2= std::unordered_set<int>(ids2.begin(), ids2.end());
        std::set_union(std::begin(part1), std::end(part1),
                              std::begin(part2), std::end(part2),
                              std::inserter(res, std::begin(res)));
        
        adj_list.at(i)= std::unordered_set<int>(res.begin(), res.end());
        // u += res.size();
      }
      // Rcout << "The undirected network has " << u/2 << " edges." << std::endl;
    } 
  } else {
    arma::rowvec tmp_row;
    for (arma::uword i = 1; i <= n_units; i++){
      tmp_row = mat.row(i-1);
      arma::uvec ids = find(tmp_row == 1) + 1; // Find indices
      // Rcout << ids << std::endl;
      adj_list.at(i)= std::unordered_set<int>(ids.begin(),ids.end());
    } 
    if(directed){
      // Rcout << "Starting with the in degree network" << std::endl;
      for (arma::uword i = 1; i <= n_units; i++){
        tmp_row = mat.col(i-1).as_row();
        // Rcout << tmp_col << std::endl;
        arma::uvec ids = find(tmp_row == 1) + 1; // Find indices
        // Rcout << ids << std::endl;
        adj_list_in.at(i)= std::unordered_set<int>(ids.begin(),ids.end());
      } 
    } 
  }
}

inline void mat_to_map_neighborhood(arma::mat neighborhood_,arma::mat overlap_, int n_units, bool directed,
                       std::unordered_map< int, std::unordered_set<int>> &neighborhood,
                       std::unordered_map< int, std::unordered_set<int>> &overlap) {
  for (int i = 1; i <= n_units; i++){ 
    neighborhood[i] = std::unordered_set<int>();
    overlap[i] = std::unordered_set<int>();
  } 

  if(neighborhood_.n_cols==2){
    arma::vec tmp_row1 = neighborhood_.col(0);
    arma::vec tmp_row2 = neighborhood_.col(1);
    arma::vec tmp_row1_ov = overlap_.col(0);
    arma::vec tmp_row2_ov = overlap_.col(1);
    for (int i = 1; i <= n_units; i++){
      std::unordered_set<int> part1,part2, res;
      arma::vec ids1 = tmp_row1.elem(find(neighborhood_.col(1) == i)); // Find indices
      arma::vec ids2 = tmp_row2.elem(find(neighborhood_.col(0) == i)); // Find indices
      part1= std::unordered_set<int>(ids1.begin(), ids1.end());
      part2= std::unordered_set<int>(ids2.begin(), ids2.end());
      std::set_union(std::begin(part1), std::end(part1),
                     std::begin(part2), std::end(part2),
                     std::inserter(res, std::begin(res)));
      neighborhood.at(i)= std::unordered_set<int>(res.begin(), res.end());
      
      std::unordered_set<int> res_nb;
      ids1 = tmp_row1_ov.elem(find(overlap_.col(1) == i)); // Find indices
      ids2 = tmp_row2_ov.elem(find(overlap_.col(0) == i)); // Find indices
      part1= std::unordered_set<int>(ids1.begin(), ids1.end());
      part2= std::unordered_set<int>(ids2.begin(), ids2.end());
      std::set_union(std::begin(part1), std::end(part1),
                     std::begin(part2), std::end(part2),
                     std::inserter(res_nb, std::begin(res_nb)));
      overlap.at(i)= std::unordered_set<int>(res_nb.begin(), res_nb.end());
    }
  } else {
    arma::rowvec tmp_row;
    arma::uvec ids;
    for (int i = 1; i <= n_units; i++){
      tmp_row = neighborhood_.row(i-1);
      ids = find(tmp_row == 1) + 1; // Find indices
      neighborhood.at(i)= std::unordered_set<int>(ids.begin(),ids.end());
      
      tmp_row = overlap_.row(i-1);
      ids = find(tmp_row == 1) + 1; // Find indices
      overlap.at(i)= std::unordered_set<int>(ids.begin(),ids.end());
    } 
  } 
}

// This is a function to transform an arma::vec to a std::vector<int>
inline std::unordered_set< int> armavec_to_set(arma::vec vec, int type) {
  std::unordered_set< int> res;
  arma::uvec ids = find(vec == type) + 1; // Find indices
  for (unsigned int i = 0; i < ids.size(); i++){
    res.insert(ids.at(i));
  }
  // print_vector(res);
  return(res);
}


// This is a function to transform an arma::vec to a std::vector<int>
inline std::vector< int> armavec_to_vector(arma::vec vec) {
  int n_units = vec.size();
  std::vector< int> res;

  for (int i = 0; i < n_units; i++){
    res.push_back(vec.at(i));
  }
  // print_vector(res);
  return(res);
}

inline std::unordered_set<int> get_set_difference(
    const std::unordered_set<int>& set1,
    const std::unordered_set<int>& set2)
{
  std::unordered_set<int> difference;
  difference.reserve(set1.size()); 
   
  for (const int& element : set1) {
    if (!set2.count(element)) {
      difference.insert(element);
    }
  } 
  
  return difference;
}

inline std::unordered_set<int> get_intersection(
    const std::unordered_set<int>& set1,
    const std::unordered_set<int>& set2)
{
  const auto& smaller_set = (set1.size() < set2.size()) ? set1 : set2;
  const auto& larger_set  = (set1.size() < set2.size()) ? set2 : set1;
  
  // Allocate the result set
  std::unordered_set<int> intersection;
  intersection.reserve(smaller_set.size());
  
  for (const int& element : smaller_set) {
    if (larger_set.count(element)) {
      intersection.insert(element);
    }
  }
  
  return intersection;
}


inline size_t count_difference(
    const std::unordered_set<int>& set1,
    const std::unordered_set<int>& set2)
{
  size_t count = 0;
  for (const int& element : set1) {
    if (!set2.count(element)) {
      count++;
    }
  } 
  
  return count;
} 

inline size_t count_intersection(
    const std::unordered_set<int>& set1,
    const std::unordered_set<int>& set2)
{ 
  const auto& smaller_set = (set1.size() < set2.size()) ? set1 : set2;
  const auto& larger_set  = (set1.size() < set2.size()) ? set2 : set1;
  size_t count = 0;
  // Allocate the result set
  
  for (const int& element : smaller_set) {
    if (larger_set.count(element)) {
      count++;
    }
  }
  
  return count;
}

// Vector implementations
inline void mat_to_map_vec(arma::mat mat, int n_units, bool directed,
                    std::vector<std::vector<int>>& adj_list,
                    std::vector<std::vector<int>>& adj_list_in,
                    std::vector<char>& adj_mat) {

  for (int i = 0; i <= n_units; i++) { 
    adj_list[i].clear();
    if (directed) {
      adj_list_in[i].clear();
    }
  } 
  adj_mat.assign(n_units * n_units, 0);

  if (mat.is_empty() || mat.n_elem == 0) {
    return;
  }

  auto get_mat_idx = [n_units](int r, int c) {
    return (r - 1) * n_units + (c - 1);
  };

  // Check whether an edge list or adjacency matrix is provided
  if(mat.n_cols == 2) {
    const arma::vec& tmp_row1 = mat.col(0);
    const arma::vec& tmp_row2 = mat.col(1);
    
    for (arma::uword k = 0; k < mat.n_rows; ++k) {
      int from = (int)tmp_row1[k];
      int to = (int)tmp_row2[k];
      
      if (from < 1 || from > n_units || to < 1 || to > n_units) {
          continue; 
      }
      
      adj_mat[get_mat_idx(from, to)] = 1;
      
      if(directed) {
        adj_list[from].push_back(to);
        adj_list_in[to].push_back(from);
      } else {
        adj_mat[get_mat_idx(to, from)] = 1;
        adj_list[from].push_back(to);
        adj_list[to].push_back(from);
      }
    }
    
    if(!directed) {
      for(int i = 1; i <= n_units; ++i) {
        std::sort(adj_list[i].begin(), adj_list[i].end());
        adj_list[i].erase(std::unique(adj_list[i].begin(), adj_list[i].end()), adj_list[i].end());
      }
    }
  } else {
    for (arma::uword i = 1; i <= n_units; i++) {
      for(arma::uword j = 1; j <= n_units; j++) {
        if(mat(i-1, j-1) == 1) {
          adj_list[i].push_back(j);
          adj_mat[get_mat_idx(i, j)] = 1;
          if(directed) {
            adj_list_in[j].push_back(i);
          }
        }
      }
    } 
  }
}

inline std::vector<int> get_intersection_vec(
    const std::vector<int>& v1,
    const std::vector<int>& v2)
{
  std::vector<int> intersection;
  intersection.reserve(std::min(v1.size(), v2.size()));
  std::set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(), std::back_inserter(intersection));
  return intersection;
}

inline size_t count_intersection_vec(
    const std::vector<int>& v1,
    const std::vector<int>& v2)
{ 
  size_t count = 0;
  auto it1 = v1.begin();
  auto it2 = v2.begin();
  while (it1 != v1.end() && it2 != v2.end()) {
    if (*it1 < *it2) {
      ++it1;
    } else if (*it2 < *it1) {
      ++it2;
    } else {
      ++count;
      ++it1;
      ++it2;
    }
  }
  return count;
}

inline std::vector<int> get_difference_vec(
    const std::vector<int>& v1,
    const std::vector<int>& v2)
{
  std::vector<int> diff;
  diff.reserve(v1.size());
  std::set_difference(v1.begin(), v1.end(), v2.begin(), v2.end(), std::back_inserter(diff));
  return diff;
}

#endif

