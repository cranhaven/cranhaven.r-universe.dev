#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <queue>
#include <unordered_set>

using namespace Rcpp;

// Ultra-fast integer pair for edges
typedef std::pair<int, int> Edge;

// Custom hash for edges - maximum speed
struct EdgeHash {
  inline std::size_t operator()(const Edge& e) const {
    return (static_cast<size_t>(e.first) << 32) | static_cast<size_t>(e.second);
  }
};

// Ultra-fast component extractor - zero dynamic allocation in hot loops
class FastComponentExtractor {
private:
  std::vector<int> queue;              // declared first
  std::vector<bool> visited;           // declared second  
  std::vector<int> component_vertices;
  std::unordered_set<Edge, EdgeHash> component_edges;
  int queue_start, queue_end;
  
public:
  FastComponentExtractor(int n_vertices) : 
  queue(n_vertices),              // initialized first
  visited(n_vertices + 1, false), // initialized second
  component_vertices() {
    component_vertices.reserve(n_vertices);
    component_edges.reserve(n_vertices * 3);
  }
  
  List extract_component_fast(const NumericMatrix& vertices, 
                              const NumericMatrix& faces,
                              const IntegerVector& input_truth,
                              int start_vertex,
                              const std::vector<std::vector<int>>& adj) {
    
    // Reset for new component
    queue_start = 0;
    queue_end = 0;
    component_vertices.clear();
    component_edges.clear();
    
    // Manual queue implementation
    queue[queue_end++] = start_vertex;
    visited[start_vertex] = true;
    component_vertices.push_back(start_vertex);
    
    // Ultra-fast BFS
    while (queue_start < queue_end) {
      int current = queue[queue_start++];
      const std::vector<int>& neighbors = adj[current];
      const int num_neighbors = neighbors.size();
      
      for (int j = 0; j < num_neighbors; ++j) {
        int neighbor = neighbors[j];
        if (!visited[neighbor]) {
          visited[neighbor] = true;
          queue[queue_end++] = neighbor;
          component_vertices.push_back(neighbor);
        }
        
        // Create edge
        int v1 = current, v2 = neighbor;
        if (v1 > v2) std::swap(v1, v2);
        component_edges.insert(std::make_pair(v1, v2));
      }
    }
    
    return build_component_ultrafast(vertices, faces, input_truth);
  }
  
  // Getter for visited array
  const std::vector<bool>& getVisited() const { return visited; }
  
  // Reset visited array (for finding multiple components)
  void resetVisited() {
    std::fill(visited.begin(), visited.end(), false);
  }
  
  // Find largest component by BFS
  List find_largest_component(const NumericMatrix& vertices,
                              const NumericMatrix& faces,
                              const IntegerVector& input_truth,
                              const std::vector<std::vector<int>>& adj) {
    
    int n_vertices = vertices.nrow();
    List largest_component;
    int largest_size = -1;
    int largest_start = -1;
    
    // First pass: find largest component
    for (int i = 1; i <= n_vertices; ++i) {
      if (!visited[i] && !adj[i].empty()) {
        // Do a quick BFS to measure component size
        queue_start = 0;
        queue_end = 0;
        int component_size = 0;
        
        queue[queue_end++] = i;
        visited[i] = true;
        component_size++;
        
        while (queue_start < queue_end) {
          int current = queue[queue_start++];
          const std::vector<int>& neighbors = adj[current];
          
          for (size_t j = 0; j < neighbors.size(); ++j) {
            int neighbor = neighbors[j];
            if (!visited[neighbor]) {
              visited[neighbor] = true;
              queue[queue_end++] = neighbor;
              component_size++;
            }
          }
        }
        
        // Track largest component
        if (component_size > largest_size) {
          largest_size = component_size;
          largest_start = i;
        }
      }
    }
    
    // Reset visited for second pass
    resetVisited();
    
    // Extract the largest component
    if (largest_start != -1) {
      largest_component = extract_component_fast(vertices, faces, input_truth, 
                                                 largest_start, adj);
    }
    
    return largest_component;
  }
  
private:
  List build_component_ultrafast(const NumericMatrix& vertices, 
                                 const NumericMatrix& faces,
                                 const IntegerVector& input_truth) {
    
    // Sort vertices once
    std::sort(component_vertices.begin(), component_vertices.end());
    
    // Build vertex mapping array
    std::vector<int> vertex_map(vertices.nrow() + 1, -1);
    const int num_comp_vertices = component_vertices.size();
    NumericMatrix comp_vertices(num_comp_vertices, 3);
    
    // Extract input_truth for component if it exists
    IntegerVector comp_input_truth;
    if (input_truth.length() > 0) {
      comp_input_truth = IntegerVector(num_comp_vertices);
    }
    
    for (int i = 0; i < num_comp_vertices; ++i) {
      const int old_idx = component_vertices[i];
      vertex_map[old_idx] = i + 1;
      comp_vertices(i, 0) = vertices(old_idx - 1, 0);
      comp_vertices(i, 1) = vertices(old_idx - 1, 1);
      comp_vertices(i, 2) = vertices(old_idx - 1, 2);
      
      if (input_truth.length() > 0) {
        comp_input_truth[i] = input_truth[old_idx - 1];
      }
    }
    
    // Count faces in component
    int face_count = 0;
    const int num_faces = faces.nrow();
    for (int i = 0; i < num_faces; ++i) {
      if (visited[faces(i, 0)] && visited[faces(i, 1)] && visited[faces(i, 2)]) {
        face_count++;
      }
    }
    
    // Build faces with new indices
    NumericMatrix comp_faces(face_count, 3);
    int face_idx = 0;
    for (int i = 0; i < num_faces; ++i) {
      const int v1 = faces(i, 0);
      const int v2 = faces(i, 1);
      const int v3 = faces(i, 2);
      
      if (visited[v1] && visited[v2] && visited[v3]) {
        comp_faces(face_idx, 0) = vertex_map[v1];
        comp_faces(face_idx, 1) = vertex_map[v2];
        comp_faces(face_idx, 2) = vertex_map[v3];
        face_idx++;
      }
    }
    
    // Build edges from component edges with new vertex indices
    std::unordered_set<Edge, EdgeHash> unique_edges;
    unique_edges.reserve(component_edges.size());
    
    for (const auto& edge : component_edges) {
      const int new_v1 = vertex_map[edge.first];
      const int new_v2 = vertex_map[edge.second];
      if (new_v1 != -1 && new_v2 != -1) {
        unique_edges.insert(std::make_pair(new_v1, new_v2));
      }
    }
    
    NumericMatrix comp_edges(unique_edges.size(), 2);
    int edge_idx = 0;
    for (const auto& edge : unique_edges) {
      comp_edges(edge_idx, 0) = edge.first;
      comp_edges(edge_idx, 1) = edge.second;
      edge_idx++;
    }
    
    List mesh;
    mesh["vertices"] = comp_vertices;
    mesh["faces"] = comp_faces;
    mesh["edges"] = comp_edges;
    
    if (input_truth.length() > 0) {
      mesh["input_truth"] = comp_input_truth;
    }
    
    return mesh;
  }
};

// Ultra-fast adjacency list builder
std::vector<std::vector<int>> build_adjacency_ultrafast(int n_vertices, const NumericMatrix& faces) {
  std::vector<std::vector<int>> adj(n_vertices + 1);
  
  // Single pass to count degrees
  std::vector<int> degree(n_vertices + 1, 0);
  const int num_faces = faces.nrow();
  for (int i = 0; i < num_faces; ++i) {
    degree[faces(i, 0)] += 2;
    degree[faces(i, 1)] += 2;
    degree[faces(i, 2)] += 2;
  }
  
  // Pre-allocate exact memory needed
  for (int i = 1; i <= n_vertices; ++i) {
    adj[i].reserve(degree[i]);
  }
  
  // Single pass to build adjacency
  for (int i = 0; i < num_faces; ++i) {
    const int v1 = faces(i, 0);
    const int v2 = faces(i, 1);
    const int v3 = faces(i, 2);
    
    adj[v1].push_back(v2);
    adj[v1].push_back(v3);
    adj[v2].push_back(v1);
    adj[v2].push_back(v3);
    adj[v3].push_back(v1);
    adj[v3].push_back(v2);
  }
  
  // Fast duplicate removal in-place
  for (int i = 1; i <= n_vertices; ++i) {
    if (!adj[i].empty()) {
      std::sort(adj[i].begin(), adj[i].end());
      auto last = std::unique(adj[i].begin(), adj[i].end());
      adj[i].resize(last - adj[i].begin());
    }
  }
  
  return adj;
}

// [[Rcpp::export]]
List get_CCMESH_cpp(NumericMatrix vertices, 
                    NumericMatrix faces, 
                    Nullable<IntegerVector> input_truth_ = R_NilValue,
                    bool return_largest = true) {
  
  const int n_vertices = vertices.nrow();
  const int n_faces = faces.nrow();
  
  // Input validation
  if (n_vertices == 0 || n_faces == 0) {
    stop("Empty vertices or faces matrix");
  }
  
  // Extract input_truth attribute (handle NULL value)
  IntegerVector input_truth;
  
  if (input_truth_.isNotNull()) {
    input_truth = as<IntegerVector>(input_truth_);
  }
  
  // Build adjacency list (needed for both modes)
  std::vector<std::vector<int>> adj = build_adjacency_ultrafast(n_vertices, faces);
  
  if (return_largest) {
    // Find and return only the largest connected component
    FastComponentExtractor extractor(n_vertices);
    List largest_component = extractor.find_largest_component(vertices, faces, 
                                                              input_truth, adj);
    
    // If no component found (shouldn't happen), return empty mesh
    if (largest_component.length() == 0) {
      List empty_mesh;
      empty_mesh["vertices"] = NumericMatrix(0, 3);
      empty_mesh["faces"] = NumericMatrix(0, 3);
      empty_mesh["edges"] = NumericMatrix(0, 2);
      return empty_mesh;
    }
    
    return largest_component;
    
  } else {
    // Return all connected components
    FastComponentExtractor extractor(n_vertices);
    std::vector<List> components;
    components.reserve(10);
    
    for (int i = 1; i <= n_vertices; ++i) {
      if (!extractor.getVisited()[i] && !adj[i].empty()) {
        List component = extractor.extract_component_fast(vertices, faces, 
                                                          input_truth, i, adj);
        components.push_back(component);
      }
    }
    
    // Handle no components found
    if (components.empty()) {
      List empty_result;
      empty_result["components"] = List::create();
      empty_result["n_components"] = 0;
      return empty_result;
    }
    
    // Return all components as a list
    List result;
    result["components"] = wrap(components);
    result["n_components"] = components.size();
    return result;
  }
}
