#include <Rcpp.h>
#include "pow4inthead.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP multinomTrainCpp(List seqs, int K, bool names, List classesIn, double nPseudo){
  // Convert input from R list to vector of vectors
  std::vector<std::vector<int> > Seqs = Rcpp::as< std::vector<std::vector<int> > >(seqs);
  std::vector<std::vector<int> > ClassesIn = Rcpp::as< std::vector<std::vector<int> > >(classesIn);
  int nClasses = ClassesIn.size();
  int p = pow4int(K), where = 0;
  int num_substr;
  IntegerMatrix X(nClasses, p); // Result matrix
  std::vector<int> x;
  
  std::vector<int> Where(K); // Position translation vector
  for(int i=0; i<K; i++){
    Where[i] = pow4int(K-i-1);
  }
  
  for(int j=0; j<nClasses; j++){ // looper over alle klasser
    x = std::vector<int>(p);
    for(unsigned int i = 0; i < ClassesIn[j].size(); i++) {      // looper over alle medlemmer i en klasse
      num_substr = Seqs[ClassesIn[j][i]-1].size()-K+1;  // antall ord av lengde K i sekvens i
      for( int m=0; m<num_substr; m++ ) {               // looper over alle ord
        where = 0;
        for( int k=0; k<K; k++){                          // looper over posisjon i ord
          where += Seqs[ClassesIn[j][i]-1][m+k]*Where[k]; // where blir kolonnen til ordet i X, beregnet i 4-talls systemet
        }                                                 // dette er alltid et tall fra 0 til (4^K)-1, med mindre
        if(where >= 0){                                   // ett av elementene i sekvensen har verdien -4^K, da blir
          ++x[where];                                     // where negativ, og ordet ignoreres
        }
      }
    }
    for(int m=0; m<p; m++){
      X(j,m) = x[m];
    }
  }
  
  if(names){ // Legger til navn
    int N = pow4int(K);
    Rcpp::CharacterVector ACGT = Rcpp::CharacterVector::create("A","C","G","T");
    Rcpp::CharacterVector ACGTs(N);
    std::vector< std::vector< std::string > > matr;
    matr.resize( K , std::vector<std::string>( N ) );
    Rcpp::CharacterVector cnms(N);
    for(int i=0; i<K; i++){
      ACGTs = rep(rep_each(ACGT, pow4int(K-i-1)), pow4int(i));
      matr[i] = Rcpp::as< std::vector< std::string > >(ACGTs);
    }
    for(int i=0; i<N; i++){
      std::stringstream ss;
      for(int j=0; j<K; j++){
        ss << matr[j][i];
      }
      cnms[i] = ss.str();      
    }
    Rcpp::List dimnms = Rcpp::List::create(R_NilValue, cnms);
    X.attr("dimnames") = dimnms;
  }
  
  if(nPseudo == -1){ // Returnerer telling hvis nPseudo == -1
    return(wrap(X));
    
  } else {
    NumericMatrix C(nClasses,p); // Alternative if nPseudo >= 0
    double p1 = nPseudo/p;
    int c;
    for(int i=0; i < nClasses; i++){
      c = 0;
      for(int j=0; j < p; j++){
        c += X(i,j);
      }
      for(int j=0; j < p; j++){
        C(i,j) = log2((X(i,j)+p1)/(c+nPseudo));
      }
    }
    
    if(names){
      int N = pow4int(K);
      Rcpp::CharacterVector ACGT = Rcpp::CharacterVector::create("A","C","G","T");
      Rcpp::CharacterVector ACGTs(N);
      std::vector< std::vector< std::string > > matr;
      matr.resize( K , std::vector<std::string>( N ) );
      Rcpp::CharacterVector cnms(N);
      for(int i=0; i<K; i++){
        ACGTs = rep(rep_each(ACGT, pow4int(K-i-1)), pow4int(i));
        matr[i] = Rcpp::as< std::vector< std::string > >(ACGTs);
      }
      for(int i=0; i<N; i++){
        std::stringstream ss;
        for(int j=0; j<K; j++){
          ss << matr[j][i];
        }
        cnms[i] = ss.str();      
      }
      Rcpp::List dimnms = Rcpp::List::create(R_NilValue, cnms);
      C.attr("dimnames") = dimnms;
    }
    return(wrap(C));
  }
}

// // [[Rcpp::export]]
// Rcpp::List multinomTrainCpp2(List seqs, int K, bool names, List classesIn, double nPseudo){
//   // Convert input from R list to vector of vectors
//   std::vector<std::vector<int> > Seqs = Rcpp::as< std::vector<std::vector<int> > >(seqs);
//   std::vector<std::vector<int> > ClassesIn = Rcpp::as< std::vector<std::vector<int> > >(classesIn);
//   int nClasses = ClassesIn.size();
//   int p = pow4int(K), where = 0;
//   int num_substr;
//   IntegerMatrix X(nClasses, p); // Result matrix
//   IntegerVector Sizes(nClasses); // Class size vector
//   std::vector<int> x;
//   
//   std::vector<int> Where(K); // Position translation vector
//   for(int i=0; i<K; i++){
//     Where[i] = pow4int(K-i-1);
//   }
//   
//   for(int j=0; j<nClasses; j++){ // looper over alle klasser
//     x = std::vector<int>(p);
//     for(int i = 0; i < ClassesIn[j].size(); i++) {      // looper over alle medlemmer i en klasse
//       num_substr = Seqs[ClassesIn[j][i]-1].size()-K+1;  // antall ord av lengde K i sekvens i
//       for( int m=0; m<num_substr; m++ ) {               // looper over alle ord
//         where = 0;
//         for( int k=0; k<K; k++){                          // looper over posisjon i ord
//           where += Seqs[ClassesIn[j][i]-1][m+k]*Where[k]; // where blir kolonnen til ordet i X, beregnet i 4-talls systemet
//         }                                                 // dette er alltid et tall fra 0 til (4^K)-1, med mindre
//         if(where >= 0){                                   // ett av elementene i sekvensen har verdien -4^K, da blir
//           ++x[where];                                     // where negativ, og ordet ignoreres
//         }
//       }
//       ++Sizes(j);
//     }
//     for(int m=0; m<p; m++){
//       X(j,m) = x[m];
//     }
//   }
//   
//   if(names){ // Legger til navn
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
//   if(nPseudo == -1){ // Returnerer telling hvis nPseudo == -1
//     return Rcpp::List::create(Rcpp::Named("Counts") = wrap(X), Rcpp::Named("Sizes") = wrap(Sizes));
//     // return(wrap(X));
//     
//   } else {
//     NumericMatrix C(nClasses,p); // Alternative if nPseudo >= 0
//     double p1 = nPseudo/p;
//     int c;
//     for(int i=0; i < nClasses; i++){
//       c = 0;
//       for(int j=0; j < p; j++){
//         c += X(i,j);
//       }
//       for(int j=0; j < p; j++){
//         C(i,j) = log2((X(i,j)+p1)/(c+nPseudo));
//       }
//     }
//     
//     if(names){
//       int N = pow4int(K);
//       Rcpp::CharacterVector ACGT = Rcpp::CharacterVector::create("A","C","G","T");
//       Rcpp::CharacterVector ACGTs(N);
//       std::vector< std::vector< std::string > > matr;
//       matr.resize( K , std::vector<std::string>( N ) );
//       Rcpp::CharacterVector cnms(N);
//       for(int i=0; i<K; i++){
//         ACGTs = rep(rep_each(ACGT, pow4int(K-i-1)), pow4int(i));
//         matr[i] = Rcpp::as< std::vector< std::string > >(ACGTs);
//       }
//       for(int i=0; i<N; i++){
//         std::stringstream ss;
//         for(int j=0; j<K; j++){
//           ss << matr[j][i];
//         }
//         cnms[i] = ss.str();      
//       }
//       Rcpp::List dimnms = Rcpp::List::create(R_NilValue, cnms);
//       C.attr("dimnames") = dimnms;
//     }
//     return Rcpp::List::create(Rcpp::Named("Counts") = wrap(C), Rcpp::Named("Sizes") = wrap(Sizes));
//   }
// }
// 
// // [[Rcpp::export]]
// NumericMatrix multinomTrainCpp( SEXP seqs, int K, bool names, SEXP classesIn, int nclass, double npseudo ) {
//   
//   Rcpp::List strings(seqs);
//   IntegerVector classes(classesIn);
//   int where = 0;
//   int nElem = pow4int(K);
//   double p1 = npseudo/nElem;
//   int num_strings = strings.length();
//   NumericMatrix C(nclass, nElem);
//   NumericVector c(nclass);
//   NumericVector X(nElem);
//   std::vector<int> Where(K);
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
//       for( int k=0; k < K; k++){
//         where += seq1[j+k]*Where[k];
//       }
//       
//       // Negative values for alien characters
//       if(where >= 0){
//         X(where) += 1;
//       }
//     }
//     
//     // Summarize for groups, e.g. genera
//     for(int j=0; j < nElem; j++){
//       C(classes[i],j) += X(j);
//       c(classes[i]) += X(j);
//       X(j) = 0;
//     }
//   }
//   
//   // Convert from counts to log2 of pseudo probabilities
//   for(int j=0; j < nElem; j++){
//     for(int i=0; i < nclass; i++){
//       C(i,j) = log2((C(i,j)+p1)/(c(i)+1));
//     }
//   }  
//   
//   // Create dimnames for output matrix
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
//     C.attr("dimnames") = dimnms;
//   }
//   
//   return C;
// }
