#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;

//' Return a list of the indices of the primary shared ancestors
//'
//' This operates on an ancestry match matrix and uses a simple, divide-by_two
//' relationship between an ancestor and its descendants in the ordering
//' of an ancestry vector to determine which of the matching ancestors are secondary,
//' and then return the ones that are primary.
//' @param M an ancestry match matrix (it is a logical matrix)
//' @return A list of pairs.  Each pair is the 1-based index of ancestor of ind_1, then
//' ind_2 of the primary shared ancestors.
//' @keywords internal
//' @export
//' @examples
//' # find primary ancestor pairs of example AMMs
//' lapply(example_amms, primary_ancestor_pairs)
// [[Rcpp::export]]
List primary_ancestor_pairs(LogicalMatrix M) {
  int r, c, i, j, R, C;
  List ret;

  R = M.nrow();
  C = M.ncol();

  // initialize a new matrix to hold results.  It is initialized to all zeroes.
  IntegerMatrix I(R, C);

  // put the AMM values in there as 0 and 1
  for(r=0;r<R;r++) {
    for(c=0;c<C;c++) {
      if(M(r, c) == 1) I(r,c) = 1;
    }
  }

  // now, cycle again and add one to each element
  // that is not a primary shared ancestor
  for(r=0;r<R;r++) {
    for(c=0;c<C;c++) {
      if(I(r,c) > 0) {
        i = r + 1;  // get the 1-based indexes of these ancestors
        j = c + 1;

        // if i or j are odd, subtract 1
        i -= ((i % 2) == 1);
        j -= ((j % 2) == 1);

        //Rcout << r+1 << c+1 << i << j << "\n";

        // now, we test if the corresponding cell at i/2 and j/2 is also > 0,
        // in which case, r,c is a secondary shared ancestor.  But, we most only
        // test this when i and j are both >= 2, otherwise one of them is a self,
        // in which case it is by definition primary.
        if( (i > 1) &&
            (j > 1) &&
            (I(i/2 - 1, j/2 - 1) > 0)
        ) {
          I(r, c) += 1;
        }
      }  // close if I(r,c) > 0
    }
  }

  // finally, at the end, go back through I and record all pairs r, c
  // that have the value 1.  These are the primary shared ancestors.
  // Note that we want to send it back base-1 indexed, so we send pairs
  // r+1 and c+1.
  for(r=0;r<R;r++) {
    for(c=0;c<C;c++) {
      if(I(r,c) == 1) {
        ret.push_back(IntegerVector::create(r + 1, c + 1));
      }
    }
  }

  return(ret);
}
