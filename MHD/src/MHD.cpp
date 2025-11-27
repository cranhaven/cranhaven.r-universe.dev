#include <Rcpp.h>


// [[Rcpp::export]]
Rcpp::NumericVector 
MHD_cpp(const Rcpp::NumericMatrix distM) {

  // int nDat = distM.nrow();
  int nAnchor = distM.ncol();
  // The evaluation points are default to the data points
  // Rcpp::IntegerVector xInd = Rcpp::seq(0, distM.nrow() - 1);
  int nx = distM.nrow();

  Rcpp::NumericVector Prop(nAnchor * nAnchor);

  for (int j = 0; j < nAnchor; j++) {
    const Rcpp::NumericVector b = distM( Rcpp::_, j);
  for (int i = 0; i < nAnchor; i++) {
    const Rcpp::NumericVector a = distM( Rcpp::_, i);
    Prop(i + j * nAnchor) = mean(a <= b + 
        5.0 * std::numeric_limits<double>::epsilon()); // Take average over the data points
  }
  }

  Rcpp::NumericVector res(nx);
  for (int i = 0; i < nx; i++ ) {
    const Rcpp::NumericVector xAnchorDist = distM(i, Rcpp::_);
    Rcpp::LogicalVector tmpInd(nAnchor * nAnchor);
    for (int k = 0; k < nAnchor; k++) {
    for (int j = 0; j < nAnchor; j++) {
      tmpInd[j + k * nAnchor] = (xAnchorDist[j] <= xAnchorDist[k] + 
          5.0 * std::numeric_limits<double>::epsilon());
    }
    }
    const Rcpp::NumericVector tmp = Prop[tmpInd];
    res[i] = min(tmp);
  }

  return res;

}



// // datAnc A distance matrix, giving distances of the data points to the anchor points. The rows correspond to different data, and columns correspond to anchors
// // ancX A distance matrix of the anchor points to the eval points. Rows correspond to anchor points and columns to eval points.
// //
// //' @export
// [[Rcpp::export]]
Rcpp::NumericVector MHD2_cpp(const Rcpp::NumericMatrix datAnc, const Rcpp::NumericMatrix ancX) {

  // int nDat = datAnc.nrow();
  int nAnchor = datAnc.ncol();
  // The evaluation points are default to the data points
  // Rcpp::IntegerVector xInd = Rcpp::seq(0, ancX.ncol() - 1);
  int nx = ancX.ncol();

  Rcpp::NumericVector Prop(nAnchor * nAnchor);

  // Get the halfspace proportions for all pairs of anchors
  for (int j = 0; j < nAnchor; j++) {
    const Rcpp::NumericVector b = datAnc( Rcpp::_, j);
  for (int i = 0; i < nAnchor; i++) {
    const Rcpp::NumericVector a = datAnc( Rcpp::_, i);
    Prop(i + j * nAnchor) = mean(a <= b + 
        5.0 * std::numeric_limits<double>::epsilon()); // Take average over the data points
  }
  }

  Rcpp::NumericVector res(nx);
  Rcpp::LogicalVector tmpInd(nAnchor * nAnchor);
  for (int i = 0; i < nx; i++ ) {
    const Rcpp::NumericVector xAnchorDist = ancX(Rcpp::_, i);
    for (int k = 0; k < nAnchor; k++) {
    for (int j = 0; j < nAnchor; j++) {
      tmpInd[j + k * nAnchor] = 
        (xAnchorDist[j] <= xAnchorDist[k] + 
         5.0 * std::numeric_limits<double>::epsilon());
    }
    }
    const Rcpp::NumericVector tmp = Prop[tmpInd];
    res[i] = min(tmp);
  }

  return res;

}


// Return both the matrix of probability and the in-sample depths
// [[Rcpp::export]]
Rcpp::List MHD3_cpp(const Rcpp::NumericMatrix datAnc, const Rcpp::NumericMatrix ancX) {

  // int nDat = datAnc.nrow();
  int nAnchor = datAnc.ncol();
  // The evaluation points are default to the data points
  // Rcpp::IntegerVector xInd = Rcpp::seq(0, ancX.ncol() - 1);
  int nx = ancX.ncol();

  Rcpp::NumericVector Prop(nAnchor * nAnchor);

  // Get the halfspace proportions for all pairs of anchors
  for (int j = 0; j < nAnchor; j++) {
    const Rcpp::NumericVector b = datAnc( Rcpp::_, j);
  for (int i = 0; i < nAnchor; i++) {
    const Rcpp::NumericVector a = datAnc( Rcpp::_, i);
    Prop(i + j * nAnchor) = mean(a <= b + 
        5.0 * std::numeric_limits<double>::epsilon()); // Take average over the data points
  }
  }

  Rcpp::NumericVector res(nx);
  Rcpp::LogicalVector tmpInd(nAnchor * nAnchor);
  for (int i = 0; i < nx; i++ ) {
    const Rcpp::NumericVector xAnchorDist = ancX(Rcpp::_, i);
    for (int k = 0; k < nAnchor; k++) {
    for (int j = 0; j < nAnchor; j++) {
      tmpInd[j + k * nAnchor] = 
        (xAnchorDist[j] <= xAnchorDist[k] + 
         5.0 * std::numeric_limits<double>::epsilon());
    }
    }
    const Rcpp::NumericVector tmp = Prop[tmpInd];
    res[i] = min(tmp);
  }
  Prop.attr("dim") = Rcpp::Dimension(nAnchor, nAnchor);

  return Rcpp::List::create(Rcpp::Named("depth")=res, Rcpp::Named("prop")=Prop);

}


