// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include "pow4inthead.h"
using namespace Rcpp;
using namespace RcppParallel;

struct RDPKmerProd_worker : public Worker {
  
  std::vector<std::vector<int> > Seqs;
  int K;
  std::vector<int> Where;
  RMatrix<double> Qmat;
  RVector<double> prior;
  RVector<int> First_ind;

  RDPKmerProd_worker(const std::vector<std::vector<int> > Seqs, const int K,
                  const std::vector<int> Where, const RMatrix<double> Qmat,
                  const RVector<double> prior, RVector<int> First_ind)
    : Seqs(Seqs), K(K), Where(Where), Qmat(Qmat), prior(prior), First_ind(First_ind) { }
  
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      std::vector<double> X(prior.size());
      for(unsigned int j=0; j<prior.size(); j++){
        X[j] = prior[j];           // Accumulation matrix
      }
      int num_substr = Seqs[i].size()-K+1;      // antall ord av lengde K i sekvens i
      int where = 0;
      for( int j=0; j<num_substr; j++ ) {       // looper over alle ord
        where = 0;
        for( int k=0; k<K; k++){                // looper over posisjon i ord
          where += Seqs[i][j+k]*Where[k];       // where blir kolonnen til ordet i X, beregnet i 4-talls systemet
        }                                       // dette er alltid et tall fra 0 til (4^K)-1, med mindre
        if(where >= 0){                         // ett av elementene i sekvensen har verdien -4^K, da blir
          for(unsigned int j=0; j<Qmat.nrow(); j++){
            X[j] += Qmat(j,where);
          }
        }
      }
      int m = 1;
      double mm = X[0];
      for(unsigned int j=1; j<X.size(); j++){ // Search for largest element
        if(X[j]>mm){
          mm = X[j];
          m = j+1;
        }
      }
      First_ind[i] = m;
    }
  }
};

struct RDPKmerProdPost_worker : public Worker {
  
  std::vector<std::vector<int> > Seqs;
  int K;
  std::vector<int> Where;
  RMatrix<double> Qmat;
  RVector<double> prior;
  RVector<int> First_ind;
  RVector<double> First;
  RVector<int> Second_ind;
  RVector<double> Second;
  
  RDPKmerProdPost_worker(const std::vector<std::vector<int> > Seqs, const int K,
                      const std::vector<int> Where, const RMatrix<double> Qmat, const RVector<double> prior, RVector<int> First_ind,
                      RVector<double> First, RVector<int> Second_ind, RVector<double> Second)
    : Seqs(Seqs), K(K), Where(Where), Qmat(Qmat), prior(prior), First_ind(First_ind), First(First),
      Second_ind(Second_ind), Second(Second) { }
  
  void operator()(std::size_t begin_row, std::size_t end_row) {
    for (std::size_t i = begin_row; i < end_row; i++) {
      std::vector<double> X(prior.size());
      for(unsigned int j=0; j<prior.size(); j++){
        X[j] = prior[j];           // Accumulation matrix
      }
      int num_substr = Seqs[i].size()-K+1;      // antall ord av lengde K i sekvens i
      int where = 0;
      for( int j=0; j<num_substr; j++ ) {       // looper over alle ord
        where = 0;
        for( int k=0; k<K; k++){                // looper over posisjon i ord
          where += Seqs[i][j+k]*Where[k];       // where blir kolonnen til ordet i X, beregnet i 4-talls systemet
        }                                       // dette er alltid et tall fra 0 til (4^K)-1, med mindre
        if(where >= 0){                         // ett av elementene i sekvensen har verdien -4^K, da blir
          for(unsigned int j=0; j<Qmat.nrow(); j++){
            X[j] += Qmat(j,where);
          }
        }
      }
      int m1 = 1, m2 = 2;
      double mm1 = X[0], mm2 = X[1];
      if(mm1 < mm2){ // Store first and second elements as starting points
        mm1 = X[1]; // Largest
        mm2 = X[0]; // Second largest
        m1 = 2;
        m2 = 1;
      }
      for(unsigned int j=2; j<X.size(); j++){ // Search for two largest elements
        if(X[j]>mm2){
          if(X[j]>mm1){
            mm2 = mm1;
            mm1 = X[j];
            m2  = m1;
            m1  = j+1;
          } else {
            mm2 = X[j];
            m2  = j+1;
          }
        }
      }
      First_ind[i]  = m1;
      First[i]      = mm1;
      Second_ind[i]  = m2;
      Second[i]     = mm2;
    }
  }
};

// [[Rcpp::export]]
List rdpClassifyCpp(List seqs, int K, NumericMatrix QMat, NumericVector Prior, bool posterior){
  // Convert input from R list to vector of vectors
  std::vector<std::vector<int> > Seqs = Rcpp::as< std::vector<std::vector<int> > >(seqs);
  int num_strings = Seqs.size();
  RVector<double> prior(Prior);
  RMatrix<double> Qmat(QMat);
  IntegerVector first_ind(num_strings);  // Result matrix 1
  RVector<int> First_ind(first_ind);

  std::vector<int> Where(K); // Position translation vector
  for(int i=0; i<K; i++){
    Where[i] = pow4int(K-i-1);
  }
  
  if(posterior){
    NumericVector first(num_strings);      // Result matrix 2
    RVector<double> First(first);
    IntegerVector second_ind(num_strings);  // Result matrix 3
    RVector<int> Second_ind(second_ind);
    NumericVector second(num_strings);     // Result matrix 4
    RVector<double> Second(second);
    RDPKmerProdPost_worker rDPKmerprodpost_worker(Seqs, K, Where, Qmat, prior, First_ind, First, Second_ind, Second);
    parallelFor(0, num_strings, rDPKmerprodpost_worker);
    return Rcpp::List::create(Rcpp::Named("first_ind") = first_ind, Rcpp::Named("first") = first,
                              Rcpp::Named("second_ind") = second_ind, Rcpp::Named("second") = second);
  } else {
    RDPKmerProd_worker rDPkmerprod_worker(Seqs, K, Where, Qmat, prior, First_ind);
    parallelFor(0, num_strings, rDPkmerprod_worker);
    return Rcpp::List::create(Rcpp::Named("first_ind") = first_ind);
  }
}
// #include <Rcpp.h>
// using namespace Rcpp;

// // [[Rcpp::export]]
// Rcpp::List rdpClassifyCpp( SEXP Qin, SEXP seqs, int K ) {
//   
//   NumericMatrix Q(Qin);
//   Rcpp::List strings(seqs);
//   int nclass = Q.nrow();
//   int where = 0;
//   int nElem = pow4int(K);
//   int num_strings = strings.length();
//   NumericVector p(nElem);
//   NumericVector Ci(nclass);
//   LogicalVector X(nElem);
//   IntegerVector C(num_strings);
//   NumericMatrix RAW(num_strings,nclass);
//   std::vector<int> Where(K);
// 
//   // Apply log2 to training set
// //  for(int i=0; i < Q.nrow(); i++){
// //    for(int j=0; j < Q.ncol(); j++){
// //      Q(i,j) = log2(Q(i,j));
// //    }
// //  }
// 
//   // Prepare powers of 4
//   for(int i=0; i < K; i++){
//     Where[i] = pow4int(K-i-1);
//   }
//   
//   // Loop over sequences
//   for( int i=0; i < num_strings; i++ ) {
//     SEXP s1 = strings[i];
//     Rcpp::IntegerVector seq1(s1);
//     int num_substr = seq1.length() - K + 1;
//     
//     // Loop over characters in sequence
//     for( int j=0; j < num_substr; j++ ) {
//       // Find location in result by looping over K positions
//       where = 0;
//       for( int k=0; k<K; k++){
//         where += seq1[j+k]*Where[k];
//       }
//       
//       // Negative values for alien characters
//       if(where >= 0){
//         X(where) = true;
//       }
//     }
//     
//     // Fill classification vector
//     bool first = true;
//     for(int j=0; j < nElem; j++){
//       if(X(j)){
//         if(first){
//           first = false;
//           for(int k=0; k < nclass; k++){
//             Ci(k) = Q(k,j);          
//           }
//         } else {
//           for(int k=0; k < nclass; k++){
//             Ci(k) += Q(k,j);          
//           }
//         }
//         X(j) = false;
//       }
//     }
//     
//     // Find maximum/maxima
//     int ind    = 1;
//     double val = Ci(0);
//     bool two   = false;
//     RAW(i,0)   = Ci(0);
//     for(int j=1; j < nclass; j++){
//       RAW(i,j) = Ci(j);
//       if( Ci(j) == val ){
//         two = true;
//       } else {
//         if( Ci(j) > val ){
//           two = false;
//           ind = j+1;
//           val = Ci(j);
//         }
//       }
//     }
//     
//     // Classify
//     if(two){
//       C(i) = -1;
//     } else {
//       C(i) = ind;
//     }
//   }
//   
//   return Rcpp::List::create(Rcpp::Named("Raw") = RAW, Rcpp::Named("C") = C);
// }
