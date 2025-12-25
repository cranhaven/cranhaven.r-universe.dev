//' @useDynLib hilbertSimilarity
//' @importFrom Rcpp sourceCpp
//'
//' @name hilbertMapping
//' @title Map High Dimensional Coordinates to Hilbert Index and back
//'
//' @description
//' \code{hilbertMapping} will compute the \href{https://en.wikipedia.org/wiki/Hilbert_curve}{Hilbert index} for each
//' row of a matrix of integer coordinates corresponding to sub-cubes in a high dimensional space.
//'
//' @param x a matrix of a matrix of integer coordinates (see \code{\link{do.hilbert}})
//' @param bits the hilbert order, \emph{i.e.} the number of cuts in each dimension
//' @return a vector of hilbert index, one for each line in \code{x}
//'
//' @details
//' Functions: TransposetoAxes AxestoTranspose
//' Purpose: Transform in-place between Hilbert transpose and geometrical axes
//' Example: b=5 bits for each of n=3 coordinates.
//' 15-bit Hilbert integer = A B C D E F G H I J K L M N O is stored
//' as its Transpose
//' X[0] = A D G J M X[2]|
//' X[1] = B E H K N <-------> | /X[1]
//' X[2] = C F I L O axes |/
//' high low 0------ X[0]
//' Axes are stored conventionally as b-bit integers.
//' Author: John Skilling 20 Apr 2001 to 11 Oct 2003
//'
//' The source code includes the correction suggested in the following
//' \href{https://stackoverflow.com/a/10384110}{StackOverflow discussion}.
//'
//' @author Marilisa Neri
//' @author Yann Abraham
//' @author John Skilling
//'
#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

typedef unsigned long long int coord_t;

void TransposetoAxes( coord_t* X, int b, int n ) { // position, #bits, dimension
  coord_t N = 2 << (b-1), P, Q, t;
  int i;
  // Gray decode by H ^ (H/2)
  t = X[n-1] >> 1;
  for( i = n-1; i > 0; i-- ) X[i] ^= X[i-1];
  X[0] ^= t;
  // Undo excess work
  for( Q = 2; Q != N; Q <<= 1 ) {
    P = Q - 1;
    for( i = n-1; i >= 0 ; i-- )
      if( X[i] & Q ) X[0] ^= P; // invert
      else {
        t = (X[0]^X[i]) & P; X[0] ^= t; X[i] ^= t;
      } // exchange
  }
}

void AxestoTranspose( coord_t* X, int b, int n ){ // position, #bits, dimension
  coord_t M = 1 << (b-1), P, Q, t;
  int i;
  // Inverse undo
  for( Q = M; Q > 1; Q >>= 1 ) {
    P = Q - 1;
    for( i = 0; i < n; i++ )
      if( X[i] & Q ) X[0] ^= P; // invert
      else{ t = (X[0]^X[i]) & P; X[0] ^= t; X[i] ^= t; } } // exchange
  // Gray encode
  for( i = 1; i < n; i++ ) X[i] ^= X[i-1];
  t = 0;
  for( Q = M; Q > 1; Q >>= 1 )
    if( X[n-1] & Q ) t ^= Q-1;
    for( i = 0; i < n; i++ ) X[i] ^= t;
}

// [[Rcpp::export]]
NumericVector hilbertMapping(NumericMatrix x, int bits) {
  int nrow = x.nrow();
  int ndim = x.ncol();

  coord_t *X = new coord_t[ndim] ;
  NumericVector out(nrow);

  // loop through the input matrix
  for(int k=0; k<nrow; k++) {                                   // for each line
    for(int i=0; i<ndim; i++) {                                 // copy the line to X
      X[i] = x(k,i);
    }
    AxestoTranspose (X, bits, ndim);                            // Hilbert transpose for bits and Ndim dimensions
    {
        int j;
        float power;
        for ( j=bits-1, power=(bits*ndim)-1; j>=0; j--) {
            for (int i=0; i<ndim; i++, power--) {
                out(k) += (X[i]>>j & 1) * pow (2.0, power);
            }
        }
    }
  }
  return out;
}
