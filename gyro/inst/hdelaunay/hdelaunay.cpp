#ifndef _GYROHEADER_
#include "gyro.h"
#endif

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/Triangulation_data_structure_2.h>
#include <CGAL/Hyperbolic_Delaunay_triangulation_2.h>
#include <CGAL/Hyperbolic_Delaunay_triangulation_traits_2.h>
#include <CGAL/Triangulation_vertex_base_with_id_2.h>
#include <CGAL/number_utils.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef CGAL::Exact_predicates_exact_constructions_kernel EK;
typedef CGAL::Hyperbolic_Delaunay_triangulation_traits_2<K> HDtt;
typedef CGAL::Hyperbolic_Delaunay_triangulation_traits_2<EK> EHDtt;
typedef HDtt::Point_2 HPoint;
typedef EHDtt::Point_2 EHPoint;
typedef CGAL::Triangulation_data_structure_2<
  CGAL::Triangulation_vertex_base_with_id_2<HDtt>,
  CGAL::Hyperbolic_triangulation_face_base_2<HDtt>>
    HTds;
typedef CGAL::Triangulation_data_structure_2<
  CGAL::Triangulation_vertex_base_with_id_2<EHDtt>,
  CGAL::Hyperbolic_triangulation_face_base_2<EHDtt>>
    EHTds;
typedef CGAL::Hyperbolic_Delaunay_triangulation_2<HDtt, HTds> HDt;
typedef CGAL::Hyperbolic_Delaunay_triangulation_2<EHDtt, EHTds> EHDt;

// Rcpp::IntegerVector unique(Rcpp::IntegerVector v) {
//   size_t s = v.size();
//   if(s == 0 || s == 1) {
//     return v;
//   }
//   for(size_t i = 0; i < s - 1; i++) {
//     size_t j = i + 1;
//     while(j < s) {
//       if(v(i) == v(j)) {
//         v.erase(v.begin() + j);
//         s--;
//       } else {
//         j++;
//       }
//     }
//   }
//   return v;
// }

// int htest(){
//   HDt hdt;
//   hdt.insert(HPoint(0.1, 0.1));
//   hdt.insert(HPoint(0.8, 0.2));
//   hdt.insert(HPoint(0.4, 0.6));
//   hdt.insert(HPoint(-0.5, -0.5));
//   return 0;
// } 


template <typename HDtT, typename HPointT, typename NTT>
Rcpp::List hdelaunay_cpp(const DMatrix points,
                         const bool isolations) {
  std::vector<HPointT> hpts;
  const unsigned npoints = points.ncol();
  hpts.reserve(npoints);
  for(size_t i = 0; i != npoints; i++) {
    const DVector pt = points(Rcpp::_, i);
    hpts.emplace_back(HPointT(pt(0), pt(1)));
  }
  HDtT hdt;
  // std::vector<HPointT> x(0);
  // HDtT hdt(x.begin(), x.end());
  //HDtT* hdtptr = (HDtT*) malloc(sizeof(HDtT)); //(hpts.begin(), hpts.end());
  //typename HDtT::Vertex_handle vh = hdt.push_back(HPointT(points(0, 0), points(1, 0)));
  //CGAL::Euler::reserve(hdt, npoints, 2*npoints - 2, 6*npoints - 6);
  //typename HDtT::Vertex_handle vh;
  for(unsigned i = 0; i < npoints; i++) {
    const DVector ptrcpp = points(Rcpp::_, i);
    const HPointT pt = HPointT(ptrcpp(0), ptrcpp(1));
    //vh = 
    hdt.insert(pt);
    //vh->id() = i;
  }
  //HDtT hdt = *hdtptr;
  //hdt.insert(hpts.begin(), hpts.end());
  DMatrix Vertices(2, hdt.number_of_vertices());
//  Rcpp::Rcout << "nvertices: " << hdt.number_of_vertices();
  {
    int index = 0;
    for(typename HDtT::All_vertices_iterator vd = hdt.all_vertices_begin();
        vd != hdt.all_vertices_end(); ++vd) {
//      Rcpp::Rcout << " i: " << index;
      vd->id() = index;
      HPointT pt = vd->point();
      Vertices(0, index) = CGAL::to_double<NTT>(pt.x());
      Vertices(1, index) = CGAL::to_double<NTT>(pt.y());
      index++;
    }
  }
  const size_t nedges = hdt.number_of_hyperbolic_edges();
  Rcpp::IntegerMatrix Edges(2, nedges);
//  Rcpp::Rcout << " nedges: " << nedges;
  {
    size_t i = 0;
    for(typename HDtT::All_edges_iterator ed = hdt.all_edges_begin();
        ed != hdt.all_edges_end(); ++ed) {
//      Rcpp::Rcout << " ied: " << i;
      Rcpp::IntegerVector edge_i(2);
      const typename HDtT::Vertex_handle sVertex =
          ed->first->vertex(HDtT::cw(ed->second));
      edge_i(0) = sVertex->id() + 1;
      const typename HDtT::Vertex_handle tVertex =
          ed->first->vertex(HDtT::ccw(ed->second));
      edge_i(1) = tVertex->id() + 1;
      Edges(Rcpp::_, i) = edge_i;
      i++;
    }
  }
  const size_t nfaces = hdt.number_of_hyperbolic_faces();
  Rcpp::IntegerMatrix Faces(3, nfaces);
  {
    size_t i = 0;
    for(typename HDtT::All_faces_iterator fd = hdt.all_faces_begin();
        fd != hdt.all_faces_end(); ++fd) {
      Rcpp::IntegerVector face_i(3);
      face_i(0) = fd->vertex(0)->id() + 1;
      face_i(1) = fd->vertex(1)->id() + 1;
      face_i(2) = fd->vertex(2)->id() + 1;
      Faces(Rcpp::_, i) = face_i;
      i++;
    }
  }
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("vertices") = Vertices,
                                      Rcpp::Named("edges") = Edges,
                                      Rcpp::Named("faces") = Faces);
  if(isolations) {
    std::vector<int> v(Faces.begin(), Faces.end());
    std::sort(v.begin(), v.end());
    auto last = std::unique(v.begin(), v.end());
    v.erase(last, v.end());
    Rcpp::IntegerVector mVertices(v.begin(), v.end());
    //Rcpp::IntegerVector mVertices = unique(Faces);
    //mVertices.sort(false);
    out["mvertices"] = mVertices;
  }
  //free(hdtptr);
  return out;
}

// [[Rcpp::export]]
Rcpp::List hdelaunay_K(const DMatrix points,
                       const bool isolations) {
  return hdelaunay_cpp<HDt, HPoint, K::FT>(points, isolations);
}

// [[Rcpp::export]]
Rcpp::List hdelaunay_EK(const DMatrix points,
                        const bool isolations) {
  return hdelaunay_cpp<EHDt, EHPoint, EK::FT>(points, isolations);
}
