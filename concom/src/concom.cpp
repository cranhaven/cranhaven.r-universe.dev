// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <algorithm>
#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <utility>
#include <vector>
using namespace boost;
typedef adjacency_list<vecS, vecS, undirectedS> Graph;

// [[Rcpp::export]]
Rcpp::List concom_cpp(const Rcpp::IntegerMatrix edges) {
  size_t nedges = edges.nrow();
  Rcpp::IntegerVector verts1 = edges(Rcpp::_, 0);
  Rcpp::IntegerVector verts2 = edges(Rcpp::_, 1);
  Graph G;
  for(size_t i = 0; i != nedges; i++) {
    add_edge(verts1(i) - 1, verts2(i) - 1, G);
  }
  std::vector<int> component(num_vertices(G));
  int ncc = connected_components(G, &component[0]);
  Rcpp::IntegerVector Indices(component.size());
  Rcpp::IntegerVector Sizes(ncc);
  std::vector<int>::size_type i;
  for(i = 0; i != component.size(); ++i) {
    int c = component[i];
    Indices(i) = c + 1;
    Sizes(c)++;
  }
  return Rcpp::List::create(
    Rcpp::Named("indices") = Indices,
    Rcpp::Named("sizes") = Sizes,
    Rcpp::Named("ncomponents") = ncc
  );
}
