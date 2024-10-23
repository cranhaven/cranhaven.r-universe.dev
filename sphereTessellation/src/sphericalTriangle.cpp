#include "sphereTessellation.h"

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
Point3 SMidpoint(Point3& A, Point3& B, double radius, Point3 O) {
  const Point3 M = CGAL::midpoint(A, B);
  const double OM = sqrt(CGAL::squared_distance(O, M));
  const double scale = radius / OM;
  const Point3 sM(scale * M.x(), scale * M.y(), scale * M.z());
  return sM;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
std::pair<VX3, VX3> orderedPair(VX3 vi, VX3 vj) {
  return int(vi) < int(vj) ? std::make_pair(vi, vj) : std::make_pair(vj, vi);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
Mesh3 quadrisection(Mesh3& mesh, double radius, Point3 O) {
  Mesh3 newmesh;
  for(const VX3& vd : mesh.vertices()) {
    newmesh.add_vertex(mesh.point(vd));
  }
  int nverts = mesh.number_of_vertices();
  std::map<std::pair<VX3, VX3>, VX3> middles;
  for(const Mesh3::Edge_index& ed : mesh.edges()) {
    const VX3 v1 = source(ed, mesh);
    const VX3 v2 = target(ed, mesh);
    const Point3 smidpoint = SMidpoint(mesh.point(v1), mesh.point(v2), radius, O);
    newmesh.add_vertex(smidpoint);
    middles.insert(std::make_pair(orderedPair(v1, v2), VX3(nverts)));
    nverts++;
  }
  for(const Mesh3::Face_index& fd: mesh.faces()) {
    auto vs = vertices_around_face(mesh.halfedge(fd), mesh).begin();
    const VX3 v1 = *(vs++);
    const VX3 v2 = *(vs++);
    const VX3 v3 = *vs;
    const VX3 m12 = middles[orderedPair(v1, v2)];
    const VX3 m23 = middles[orderedPair(v2, v3)];
    const VX3 m31 = middles[orderedPair(v3, v1)];
    newmesh.add_face(v1, m12, m31);
    newmesh.add_face(v2, m23, m12);
    newmesh.add_face(v3, m31, m23);
    newmesh.add_face(m12, m23, m31);
  }
  return newmesh;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List sTriangle(
    Rcpp::NumericVector A, Rcpp::NumericVector B, Rcpp::NumericVector C,
    double radius, Rcpp::NumericVector O, int iterations
) {
  const Point3 pa(A(0), A(1), A(2));
  const Point3 pb(B(0), B(1), B(2));
  const Point3 pc(C(0), C(1), C(2));
  const Point3 po(O(0), O(1), O(2));
  Mesh3 mesh;
  mesh.add_vertex(pa);
  mesh.add_vertex(pb);
  mesh.add_vertex(pc);
  mesh.add_face(VX3(0), VX3(1), VX3(2));
  std::vector<Mesh3> meshes(iterations);
  meshes[0] = mesh;
  for(int i = 1; i < iterations; i++) {
    meshes[i] = quadrisection(meshes[i-1], radius, po);
  }
  Mesh3 newmesh = meshes[iterations-1];
  // get vertices & normals
  const int nvertices = newmesh.number_of_vertices();
  Rcpp::NumericMatrix Vertices(3, nvertices);
  Rcpp::NumericMatrix Normals(3, nvertices);
  {
    int i = 0;
    for(const VX3& vd : newmesh.vertices()) {
      const Point3 pt = newmesh.point(vd);
      Rcpp::NumericVector col_i = {pt.x(), pt.y(), pt.z()};
      Vertices(Rcpp::_, i) = col_i;
      Normals(Rcpp::_, i++) = col_i - O;
    }
  }
  // get faces
  const int nfaces = newmesh.number_of_faces();
  Rcpp::IntegerMatrix Faces(3, nfaces);
  {
    int i = 0;
    for(const Mesh3::Face_index& fd : newmesh.faces()) {
      Rcpp::IntegerVector col_i(3);
      int j = 0;
      for(const VX3& vd :
            vertices_around_face(newmesh.halfedge(fd), newmesh)) {
        col_i(j++) = (int)vd + 1;
      }
      Faces(Rcpp::_, i++) = col_i;
    }
  }
  //
  return Rcpp::List::create(
    Rcpp::Named("vertices") = Vertices,
    Rcpp::Named("faces")    = Faces,
    Rcpp::Named("normals")  = Normals
  );
}
