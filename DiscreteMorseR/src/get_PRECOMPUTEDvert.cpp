#include <Rcpp.h>
#include <vector>
#include <string>
#include <unordered_map>
#include <cstring>

using namespace Rcpp;

// [[Rcpp::export]]
List get_PRECOMPUTEDvert_cpp(CharacterVector lexi_ids, CharacterVector lexi_labels) {
  int n = lexi_ids.size();
  
  // Use pre-allocation for better performance
  std::unordered_map<std::string, std::vector<int>> vertex_map;
  vertex_map.reserve(n * 3); // Reserve space to avoid rehashing
  
  NumericVector first_verts(n);
  
  for (int i = 0; i < n; i++) {
    // MAX SPEED: First vertex extraction
    const char* label = lexi_labels[i];
    first_verts[i] = std::atof(label); // atof automatically stops at space
    
    // MAX SPEED: Manual tokenization without string copies
    std::string simplex = as<std::string>(lexi_ids[i]);
    size_t start = 0, end = 0;
    
    while ((end = simplex.find(' ', start)) != std::string::npos) {
      std::string vertex_id = simplex.substr(start, end - start);
      vertex_map[vertex_id].push_back(i + 1);
      start = end + 1;
    }
    // Do not forget the last token
    if (start < simplex.length()) {
      std::string vertex_id = simplex.substr(start);
      vertex_map[vertex_id].push_back(i + 1);
    }
  }
  
  // Efficient conversion to R data structures
  int map_size = vertex_map.size();
  List vertex_index(map_size);
  CharacterVector names(map_size);
  int idx = 0;
  
  for (const auto& pair : vertex_map) {
    names[idx] = pair.first;
    vertex_index[idx] = IntegerVector(pair.second.begin(), pair.second.end());
    idx++;
  }
  
  vertex_index.attr("names") = names;
  
  return List::create(
    _["vertex_index"] = vertex_index,
    _["first_verts_z"] = first_verts
  );
}
