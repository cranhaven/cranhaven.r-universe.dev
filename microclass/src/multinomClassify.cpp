// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>
#include "pow4inthead.h"
using namespace Rcpp;
using namespace RcppParallel;


struct KmerProd_worker : public Worker {
  
  std::vector<std::vector<int> > Seqs;
  int K;
  std::vector<int> Where;
  RMatrix<double> Qmat;
  RVector<double> prior;
  RVector<int> First_ind;

  KmerProd_worker(const std::vector<std::vector<int> > Seqs, const int K,
                  const std::vector<int> Where, const RMatrix<double> Qmat, const RVector<double> prior, RVector<int> First_ind)
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

struct KmerProdPost_worker : public Worker {
  
  std::vector<std::vector<int> > Seqs;
  int K;
  std::vector<int> Where;
  RMatrix<double> Qmat;
  RVector<double> prior;
  RVector<int> First_ind;
  RVector<double> First;
  RVector<int> Second_ind;
  RVector<double> Second;

  KmerProdPost_worker(const std::vector<std::vector<int> > Seqs, const int K,
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
List multinomClassifyCpp(List seqs, int K, NumericMatrix QMat, NumericVector Prior, bool posterior){
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
    KmerProdPost_worker kmerprodpost_worker(Seqs, K, Where, Qmat, prior, First_ind, First, Second_ind, Second);
    parallelFor(0, num_strings, kmerprodpost_worker);
    return Rcpp::List::create(Rcpp::Named("first_ind") = first_ind, Rcpp::Named("first") = first,
                              Rcpp::Named("second_ind") = second_ind, Rcpp::Named("second") = second);
  } else {
    KmerProd_worker kmerprod_worker(Seqs, K, Where, Qmat, prior, First_ind);
    parallelFor(0, num_strings, kmerprod_worker);
    return Rcpp::List::create(Rcpp::Named("first_ind") = first_ind);
  }
}


// struct CountsToMultinom_worker : public Worker {
//   
//   arma::imat X;
//   arma::imat rs;
//   RMatrix<double> CC;
//   int p;
//   double np;
//   double nPseudo;
// 
//   CountsToMultinom_worker(arma::imat X, arma::imat rs, RMatrix<double> CC, int p, double np, double nPseudo)
//     : X(X), rs(rs), CC(CC), p(p), np(np), nPseudo(nPseudo) { }
//   
//   void operator()(std::size_t begin_row, std::size_t end_row) {
//     for (std::size_t i = begin_row; i < end_row; i++) {
//       double c = log2(rs(i) + nPseudo);
//       for(int j=0; j<p; j++){
//         CC(i,j) = log2(X(i,j)+np) - c;
//       }
//       // log2((X+nPseudo/ncol(X)))-log2((rowSums(X)+nPseudo))
//       
//     }
//   }
// };
// 
// // [[Rcpp::export]]
// NumericMatrix countsToMultinom(IntegerMatrix Xin, double nPseudo){
//   arma::imat X = Rcpp::as< arma::imat >(Xin);
//   arma::imat rs = sum(X,1);
//   int n = X.n_rows, p = X.n_cols;
//   double np = nPseudo/p;
//   NumericMatrix C(n, p);
//   RMatrix<double> CC(C);
//   
//   CountsToMultinom_worker countsToMultinom_worker(X, rs, CC, p, np, nPseudo);
//   parallelFor(0, n, countsToMultinom_worker);
//   return(wrap(X));
// }

// SparseCountsToMultinom <- function(X,nPseudo,names){
//   log2((X+nPseudo/ncol(X)))-log2((rowSums(X)+nPseudo))
// }

// struct Kmer_worker : public Worker {
//   
//   std::vector<std::vector<int> > Seqs;
//   int K;
//   std::vector<int> Where;
//   RMatrix<int> X;
//   // arma::imat& X;
//   
//   Kmer_worker(const std::vector<std::vector<int> > Seqs, const int K,
//               const std::vector<int> Where, IntegerMatrix X)
//     : Seqs(Seqs), K(K), Where(Where), X(X) { }
//   
//   void operator()(std::size_t begin_row, std::size_t end_row) {
//     for (std::size_t i = begin_row; i < end_row; i++) {
//       int num_substr = Seqs[i].size()-K+1;      // antall ord av lengde K i sekvens i
//       int where = 0;
//       for( int j=0; j<num_substr; j++ ) {       // looper over alle ord
//         where = 0;
//         for( int k=0; k<K; k++){                // looper over posisjon i ord
//           where += Seqs[i][j+k]*Where[k];       // where blir kolonnen til ordet i X, beregnet i 4-talls systemet
//         }                                       // dette er alltid et tall fra 0 til (4^K)-1, med mindre
//         if(where >= 0){                         // ett av elementene i sekvensen har verdien -4^K, da blir
//           ++X(i, where);                        // where negativ, og ordet ignoreres
//         }
//       }
//     }
//   }
// };
// 
// // [[Rcpp::export]]
// IntegerMatrix Kmer_parallel(List seqs, int K, bool names){
//   // Convert input from R list to vector of vectors
//   std::vector<std::vector<int> > Seqs = Rcpp::as< std::vector<std::vector<int> > >(seqs);
//   int num_strings = Seqs.size();
//   IntegerMatrix X(num_strings, pow4int(K)); // Result matrix
//   
//   std::vector<int> Where(K); // Position translation vector
//   for(int i=0; i<K; i++){
//     Where[i] = pow4int(K-i-1);
//   }
//   
//   Kmer_worker kmer_worker(Seqs, K, Where, X);
//   parallelFor(0, num_strings, kmer_worker);
//   
//   if(names){
//     int N = pow4int(K);
//     Rcpp::CharacterVector ACGT = Rcpp::CharacterVector::create("A","C","G","T");
//     Rcpp::CharacterVector ACGTs(N);
//     std::vector< std::vector< std::string > > matr;
//     matr.resize( K , std::vector<std::string>( N ) );
//     Rcpp::CharacterVector cnms(N);
//     for(int i=0; i<K; i++){
//       ACGTs = rep(rep_each(ACGT, pow4int(K-i-1)), pow4int(i));
//       matr[i] = Rcpp::as< std::vector< std::string > >(ACGTs);
//     }
//     for(int i=0; i<N; i++){
//       std::stringstream ss;
//       for(int j=0; j<K; j++){
//         ss << matr[j][i];
//       }
//       cnms[i] = ss.str();
//     }
//     Rcpp::List dimnms = Rcpp::List::create(R_NilValue, cnms);
//     X.attr("dimnames") = dimnms;
//   }
//   
//   return(wrap(X));
// }
// 
// // [[Rcpp::plugins(cpp11)]]
// 
// // [[Rcpp::export]]
// NumericMatrix CountsToMultinom(SEXP X, double nPseudo, bool names){
//   arma::imat Xs = as<arma::imat>(X);
//   int n = Xs.n_rows, p = Xs.n_cols;
//   std::vector<int> x(n);
//   NumericMatrix C(n, p);
//   
//   double p1 = nPseudo/p;
//   int c;
//   for(int i=0; i<n; i++){
//     c = 0;
//     for(int j=0; j<p; j++){
//       c += Xs(i,j);
//     }
//     for(int j=0; j<p; j++){
//       C(i,j) = log2((Xs(i,j)+p1)/(c+nPseudo));
//     }
//   }
// 
//   if(names){
//     int N = p;
//     int K = log2(p)/2;
//     Rcpp::CharacterVector ACGT = Rcpp::CharacterVector::create("A","C","G","T");
//     Rcpp::CharacterVector ACGTs(N);
//     std::vector< std::vector< std::string > > matr;
//     matr.resize( K , std::vector<std::string>( N ) );
//     Rcpp::CharacterVector cnms(N);
//     for(int i=0; i<K; i++){
//       ACGTs = rep(rep_each(ACGT, pow4int(K-i-1)), pow4int(i));
//       matr[i] = Rcpp::as< std::vector< std::string > >(ACGTs);
//     }
//     for(int i=0; i<N; i++){
//       std::stringstream ss;
//       for(int j=0; j<K; j++){
//         ss << matr[j][i];
//       }
//       cnms[i] = ss.str();      
//     }
//     Rcpp::List dimnms = Rcpp::List::create(R_NilValue, cnms);
//     C.attr("dimnames") = dimnms;
//   }
//   return(C);
// }
