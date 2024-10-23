#include "sphereTessellation.h"

// [[Rcpp::export]]
Rcpp::List delaunay_cpp(
    Rcpp::NumericMatrix pts, double radius, Rcpp::NumericVector O, int niter
) {
  const int npoints = pts.ncol();
  std::vector<SPoint3> points;
  points.reserve(npoints);
  for(int i = 0; i < npoints; i++) {
    Rcpp::NumericVector pt_i = pts(Rcpp::_, i);
    points.emplace_back(pt_i(0), pt_i(1), pt_i(2));
  }
  // ball
  Traits ball(SPoint3(O(0), O(1), O(2)), radius);
  // projection on this ball
  Traits::Construct_point_on_sphere_2 projection =
    ball.construct_point_on_sphere_2_object();
  // Rcpp matrix to store the projected vertices
  Rcpp::NumericMatrix Vertices(3, npoints);
  // make Delaunay triangulation
  DToS dtos(ball);
  {
    int i = 0;
    for(const auto& pt : points) {
      Traits::Point_on_sphere_2 pos = projection(pt);
      DToS::Vertex_handle vh = dtos.insert(pos);
      SPoint3 p = pos.get_projection(ball.center(), ball.radius());
      Rcpp::NumericVector v_i = {p.x(), p.y(), p.z()};
      Vertices(Rcpp::_, i++) = v_i;
      int& index = vh->info();
      index = i;
    }
  }
  // check dimension
  int dim = dtos.dimension();
  if(dim == -2) {
    Rcpp::stop("The triangulation is empty.");
  }
  if(dim == -1) {
    Rcpp::stop("The triangulation contains only one vertex.");
  }
  if(dim == 0) {
    Rcpp::stop("The triangulation contains only two vertices.");
  }
  if(dim == 1) {
    Rcpp::stop("The triangulation is just a polygon drawn on a circle.");
  }
  // messages
  const int nsolidFaces = dtos.number_of_solid_faces();
  const int nghostFaces = dtos.number_of_ghost_faces();
  const std::string word1 = nsolidFaces > 1 ? "faces" : "face";
  const std::string word2 = nghostFaces > 1 ? "faces" : "face";
  const std::string msg =
    "The triangulation has " + std::to_string(nsolidFaces) + " solid " + word1
      + " and " + std::to_string(nghostFaces) + " ghost " + word2 + ".";
  Message(msg);
  // Rcpp matrix to store the faces
  const int nfaces = dtos.number_of_faces();
  Rcpp::IntegerMatrix Faces(3, nfaces);
  // Rcpp vector to store the indices of the solid faces
  Rcpp::IntegerVector SolidFaces(nsolidFaces);
  // iterate over all faces
  DToS::All_faces_iterator itbegin = dtos.all_faces_begin();
  DToS::All_faces_iterator itend = dtos.all_faces_end();
  int faceIndex = 0;
  int solidfaceIndex = 0;
  for(auto f = itbegin; f != itend; f++) {
    if(!f->is_ghost()) {
      SolidFaces(solidfaceIndex++) = faceIndex + 1;
    }
    // make the face
    Rcpp::IntegerVector Face = {
      f->vertex(0)->info(),
      f->vertex(1)->info(),
      f->vertex(2)->info()
    };
    Faces(Rcpp::_, faceIndex++) = Face;
  }
  // Meshes of spherical triangles
  const int nmeshes = dtos.number_of_solid_faces();
  Rcpp::List Meshes(nmeshes);
  for(int i = 0; i < nmeshes; i++) {
    const Rcpp::IntegerVector face = Faces(Rcpp::_, SolidFaces(i)-1);
    const Rcpp::NumericVector A = Vertices(Rcpp::_, face(0)-1);
    const Rcpp::NumericVector B = Vertices(Rcpp::_, face(1)-1);
    const Rcpp::NumericVector C = Vertices(Rcpp::_, face(2)-1);
    Meshes(i) = sTriangle(A, B, C, radius, O, niter);
  }
  //
  return Rcpp::List::create(
    Rcpp::Named("vertices")   = Rcpp::transpose(Vertices),
    Rcpp::Named("faces")      = Rcpp::transpose(Faces),
    Rcpp::Named("solidFaces") = SolidFaces,
    Rcpp::Named("meshes")     = Meshes
  );
}
