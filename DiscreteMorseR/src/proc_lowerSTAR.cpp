#include <Rcpp.h>
#include <vector>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <sstream>

using namespace Rcpp;

struct SimplexInfo {
  int dimension;
  std::vector<int> vertices;
  std::string id;
  std::string label;
};

SimplexInfo parseSimplex(const std::string& simplex_id, const std::string& simplex_label) {
  SimplexInfo info;
  info.id = simplex_id;
  info.label = simplex_label;
  
  // Check for empty or NA
  if (simplex_id.empty() || simplex_id == "NA" || simplex_id == "NA_STRING") {
    info.dimension = -1;
    return info;
  }
  
  std::istringstream iss(simplex_id);
  std::string token;
  while (iss >> token) {
    // Clean token: only digits
    std::string clean_token;
    for (char c : token) {
      if (std::isdigit(c)) {
        clean_token += c;
      }
    }
    if (!clean_token.empty()) {
      try {
        info.vertices.push_back(std::stoi(clean_token));
      } catch (...) {
        // Skip invalid tokens
      }
    }
  }
  
  info.dimension = info.vertices.size() - 1;
  return info;
}

// Get first value from label (for sorting by height)
double getFirstValue(const std::string& label) {
  // Check for empty string or "NA"
  if (label.empty() || label == "NA" || label == "NA_STRING") {
    return 0.0;
  }
  
  std::istringstream iss(label);
  std::string token;
  if (iss >> token) {
    // Remove any non-numeric characters except '.' and '-'
    std::string clean_token;
    for (char c : token) {
      if (std::isdigit(c) || c == '.' || c == '-') {
        clean_token += c;
      }
    }
    if (!clean_token.empty()) {
      try {
        return std::stod(clean_token);
      } catch (...) {
        return 0.0;
      }
    }
  }
  return 0.0;
}

// Implementing Forman gradient rules
// [[Rcpp::export]]
List proc_lowerSTAR_cpp(List list_lowerSTAR, DataFrame vertex) {
  int n = list_lowerSTAR.size();
  std::unordered_set<std::string> VE_set;
  std::unordered_set<std::string> CR_set;
  
  IntegerVector vertex_ids = vertex["i123"];
  
  // Process each vertex's lower star
  for (int i = 0; i < n; i++) {
    DataFrame df = as<DataFrame>(list_lowerSTAR[i]);
    if (df.nrows() == 0) {
      // Empty lower star: vertex is critical (minimum)
      CR_set.insert(std::to_string(vertex_ids[i]));
      continue;
    }
    
    CharacterVector lexi_id = df["lexi_id"];
    CharacterVector lexi_label = df["lexi_label"];
    
    // Parse all simplices in this lower star
    std::vector<SimplexInfo> simplices;
    simplices.reserve(lexi_id.size());  // Pre-allocate for performance
    
    for (int j = 0; j < lexi_id.size(); j++) {
      simplices.push_back(parseSimplex(
          as<std::string>(lexi_id[j]),
          as<std::string>(lexi_label[j])
      ));
    }
    
    // Sort simplices by their first label value (height)
    std::sort(simplices.begin(), simplices.end(),
              [](const SimplexInfo& a, const SimplexInfo& b) {
                return getFirstValue(a.label) < getFirstValue(b.label);
              });
    
    // Separate by dimension
    std::vector<SimplexInfo> vertices, edges, faces;
    
    // Pre-allocate memory to avoid reallocations
    vertices.reserve(simplices.size() / 3);
    edges.reserve(simplices.size() / 3);
    faces.reserve(simplices.size() / 3);
    
    for (const auto& simplex : simplices) {
      if (simplex.dimension == 0) vertices.push_back(simplex);
      else if (simplex.dimension == 1) edges.push_back(simplex);
      else if (simplex.dimension == 2) faces.push_back(simplex);
    }
    
    // Apply Forman gradient rules:
    // 1. Pair vertices with edges (0D → 1D)
    // 2. Pair edges with faces (1D → 2D)
    
    std::unordered_set<std::string> paired_edges;
    std::unordered_set<std::string> paired_faces;
    
    // First pass: try to pair each vertex with an edge
    for (const auto& v : vertices) {
      bool vertex_paired = false;
      
      for (const auto& e : edges) {
        // Check if vertex is part of this edge
        if (std::find(e.vertices.begin(), e.vertices.end(), v.vertices[0]) != e.vertices.end()) {
          if (paired_edges.find(e.id) == paired_edges.end()) {
            // Pair vertex with edge: vertex:edge
            VE_set.insert(v.id + ":" + e.id);
            paired_edges.insert(e.id);
            vertex_paired = true;
            break;
          }
        }
      }
      
      if (!vertex_paired) {
        // Vertex remains critical
        CR_set.insert(v.id);
      }
    }
    
    // Second pass: pair edges with faces
    for (const auto& e : edges) {
      if (paired_edges.find(e.id) != paired_edges.end()) {
        continue; // Already paired with a vertex
      }
      
      bool edge_paired = false;
      for (const auto& f : faces) {
        // Check if edge is part of this face
        bool is_face_of = true;
        for (int v : e.vertices) {
          if (std::find(f.vertices.begin(), f.vertices.end(), v) == f.vertices.end()) {
            is_face_of = false;
            break;
          }
        }
        
        if (is_face_of && paired_faces.find(f.id) == paired_faces.end()) {
          // Pair edge with face: edge:face
          VE_set.insert(e.id + ":" + f.id);
          paired_faces.insert(f.id);
          edge_paired = true;
          break;
        }
      }
      
      if (!edge_paired) {
        // Edge remains critical (1-saddle)
        CR_set.insert(e.id);
      }
    }
    
    // Third pass: unpaired faces are critical (maxima)
    for (const auto& f : faces) {
      if (paired_faces.find(f.id) == paired_faces.end()) {
        CR_set.insert(f.id);
      }
    }
  }
  
  // Convert to vectors
  std::vector<std::string> VE_final(VE_set.begin(), VE_set.end());
  std::vector<std::string> CR_final(CR_set.begin(), CR_set.end());
  
  Rcout << "\n   -------------\n";
  Rcout << "   Morse complex:\n";
  Rcout << "   Gradient pairs: " << VE_final.size() << "\n";
  Rcout << "   Critical simplices: " << CR_final.size() << "\n";
  
  // Count by dimension
  int crit_vertices = 0, crit_edges = 0, crit_faces = 0;
  for (const auto& crit : CR_final) {
    std::istringstream iss(crit);
    std::string token;
    int count = 0;
    while (iss >> token) count++;
    
    if (count == 1) crit_vertices++;
    else if (count == 2) crit_edges++;
    else if (count == 3) crit_faces++;
  }
  
  Rcout << "   Critical vertices (minima): " << crit_vertices << "\n";
  Rcout << "   Critical edges (1-saddles): " << crit_edges << "\n";
  Rcout << "   Critical faces (maxima): " << crit_faces << "\n";
  Rcout << "   -------------\n\n";
  
  return List::create(
    _["VE_"] = VE_final,
    _["CR_"] = CR_final
  );
}

// Main function
// [[Rcpp::export]]
List compute_MORSE_complex_cpp(List list_lowerSTAR, DataFrame vertex) {
  return proc_lowerSTAR_cpp(list_lowerSTAR, vertex);
}
