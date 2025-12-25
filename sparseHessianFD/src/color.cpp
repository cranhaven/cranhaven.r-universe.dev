// Part of the sparseHessianFD package
// Copyright (C) 2013-2016 Michael Braun

#include <Rcpp.h>

using Rcpp::List;
using Rcpp::IntegerVector;
using Rcpp::Rcout;

typedef std::set<int> S;

//' @title Vertex coloring of a sparse undirected graph
//' @description Generate proper vertex coloring of a sparse undirected graph.
//' @param pntr,idx row pointers and column indices of the adjacency matrix, in compressed column-oriented format. Must use zero-based indexing.
//' @param nvars Number of vertices.
//' @return An integer vector of length nvars, where each element represents the color of the corresponding vertex. Indices are zero-based.
//' @details  For internal use.  You should not have to call this function directly.
//[[Rcpp::export]]
Rcpp::IntegerVector get_colors(const IntegerVector& pntr, //row/col pointer
			 const IntegerVector& idx, // col/row index
			 const int nvars) {
  
  std::vector<std::set<int> > P(nvars);
  std::vector<std::set<int> > forb(nvars);
  Rcpp::IntegerVector colors(nvars);
  std::set<int> used;
  std::set<int> valid;

  for (int m=0; m < nvars; m++) {
    P[m] = S(idx.begin()+pntr(m), idx.begin()+pntr(m+1)); // rows
  }

  int max_color = 0;
  used.insert(0);
  for (int i=0; i<nvars; i++) { 
    if (forb[i].empty()) {
      colors[i] = 0;
    } else {
      valid.clear();
      set_difference(used.begin(), used.end(),
		     forb[i].begin(), forb[i].end(),
		     std::inserter(valid,valid.begin()));
      if (valid.empty()) { // add new color
	max_color++;
	used.insert(max_color);
	colors[i] = max_color;
      } else {
	colors[i] = *valid.begin();
      }
    }

    for (auto j : P[i]) {
      forb[j].insert(colors[i]);
    }     
  }

  return(Rcpp::wrap(colors));
}
