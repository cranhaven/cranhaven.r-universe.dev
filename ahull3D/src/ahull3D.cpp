#include "ahull3D.h"
#include <map>
#include <list>
#include <limits>  

struct Point3Comparator {
  bool operator()(const Point3& a, const Point3& b) const {
    const double eps = 1e-9;
    if (std::abs(a.x() - b.x()) > eps) return a.x() < b.x();
    if (std::abs(a.y() - b.y()) > eps) return a.y() < b.y();
    if (std::abs(a.z() - b.z()) > eps) return a.z() < b.z();
    return false; 
  }
};

// [[Rcpp::export]]
Rcpp::List FAS_cpp_with_labels(Rcpp::NumericMatrix pts, double alpha, 
                               Rcpp::NumericVector input_labels, bool volume) {
  const int npoints = pts.ncol();
  
  // Store points and build label map
  std::list<Point3> points_list;
  std::map<Point3, double, Point3Comparator> point_to_label;
  
  for(int i = 0; i < npoints; i++) {
    const Rcpp::NumericVector pt_i = pts(Rcpp::_, i);
    Point3 p(pt_i(0), pt_i(1), pt_i(2));
    points_list.push_back(p);
    point_to_label[p] = input_labels[i];
  }
  
  // Compute alpha shape
  Fixed_alpha_shape_3 as(points_list.begin(), points_list.end(), alpha);
  
  // Get the facets
  std::list<Facet> facets;
  as.get_alpha_shape_facets(std::back_inserter(facets),
                            Fixed_alpha_shape_3::REGULAR);
  as.get_alpha_shape_facets(std::back_inserter(facets),
                            Fixed_alpha_shape_3::SINGULAR);
  
  const int nfacets = facets.size();
  
  // Check if any facets found
  if (nfacets == 0) {
    Rcpp::warning("No facets found in alpha shape");
    return Rcpp::List::create(
      Rcpp::Named("vertices") = Rcpp::NumericMatrix(3, 0),
      Rcpp::Named("vertex_labels") = Rcpp::NumericVector(0)
    );
  }
  
  // Output matrices - SAME FORMAT AS ORIGINAL
  Rcpp::NumericMatrix Vertices(3, 3 * nfacets);
  Rcpp::NumericVector VertexLabels(3 * nfacets);
  
  std::list<Facet>::iterator it_facet;
  int i = 0;
  
  for(it_facet = facets.begin(); it_facet != facets.end(); it_facet++) {
    Facet facet = *it_facet;
    
    // to have a consistent orientation, always consider an exterior cell
    if(as.classify(facet.first) != Fixed_alpha_shape_3::EXTERIOR) {
      facet = as.mirror_facet(facet);
    }
    
    int indices[3] = {
      (facet.second + 1) % 4,
      (facet.second + 2) % 4,
      (facet.second + 3) % 4,
    };
    
    // needed to get a consistent orientation
    if(facet.second % 2 == 0) {
      std::swap(indices[0], indices[1]);
    }
    
    const Point3 v1 = facet.first->vertex(indices[0])->point();
    const Point3 v2 = facet.first->vertex(indices[1])->point();
    const Point3 v3 = facet.first->vertex(indices[2])->point();
    
    // Store vertices
    const Rcpp::NumericVector V1 = {v1.x(), v1.y(), v1.z()};
    const Rcpp::NumericVector V2 = {v2.x(), v2.y(), v2.z()};
    const Rcpp::NumericVector V3 = {v3.x(), v3.y(), v3.z()};
    
    Vertices(Rcpp::_, 3*i)   = V1;
    Vertices(Rcpp::_, 3*i+1) = V2;
    Vertices(Rcpp::_, 3*i+2) = V3;
    
    // Assign labels
    auto assign_label = [&](const Point3& p) -> double {
      auto it = point_to_label.find(p);
      if(it != point_to_label.end()) {
        return it->second;  // Exact match
      }
      
      // Fallback: linear search (should be rare)
      double min_dist = std::numeric_limits<double>::max();
      double label = NA_REAL;
      
      // Need to search through original points
      // We don't have them in a vector, so we'll iterate
      // This is inefficient but should be rare
      for(const auto& kv : point_to_label) {
        double dx = p.x() - kv.first.x();
        double dy = p.y() - kv.first.y();
        double dz = p.z() - kv.first.z();
        double dist = dx*dx + dy*dy + dz*dz;
        
        if(dist < min_dist) {
          min_dist = dist;
          label = kv.second;
        }
      }
      
      return label;
    };
    
    VertexLabels[3*i]   = assign_label(v1);
    VertexLabels[3*i+1] = assign_label(v2);
    VertexLabels[3*i+2] = assign_label(v3);
    
    i++;
  }
  
  // volume computation
  if(volume) {
    double vol1 = 0.0;
    double vol2 = 0.0;
    std::list<Cell_handle> cells1;
    std::list<Cell_handle> cells2;
    
    as.get_alpha_shape_cells(std::back_inserter(cells1),
                             Fixed_alpha_shape_3::EXTERIOR);
    as.get_alpha_shape_cells(std::back_inserter(cells2),
                             Fixed_alpha_shape_3::INTERIOR);
    
    for(auto& cell : cells1) {
      Tetrahedron t(
          cell->vertex(0)->point(),
          cell->vertex(1)->point(),
          cell->vertex(2)->point(),
          cell->vertex(3)->point()
      );
      vol1 += fabs(t.volume());
    }
    
    for(auto& cell : cells2) {
      Tetrahedron t(
          cell->vertex(0)->point(),
          cell->vertex(1)->point(),
          cell->vertex(2)->point(),
          cell->vertex(3)->point()
      );
      vol2 += fabs(t.volume());
    }
    
    Vertices.attr("volume") = Rcpp::NumericVector::create(
      Rcpp::Named("exterior") = vol1,
      Rcpp::Named("interior") = vol2
    );
  }
  
  // Return as list
  return Rcpp::List::create(
    Rcpp::Named("vertices") = Vertices,
    Rcpp::Named("vertex_labels") = VertexLabels
  );
}
