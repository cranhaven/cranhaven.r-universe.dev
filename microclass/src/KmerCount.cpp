#include <Rcpp.h>
#include "pow4inthead.h"
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix Kmer_count( SEXP seqs, int K, bool names ) {
  
  Rcpp::List strings(seqs);
  int num_strings = strings.length();
  IntegerMatrix X(num_strings, pow4int(K));
  int where = 0;
  std::vector<int> Where(K);
  
  for(int i=0; i<K; i++){
    Where[i] = pow4int(K-i-1);  // med K=3 blir Where lik 16 og 4 og 1
  }
  
  for( int i=0; i<num_strings; i++ ) {        // looper over sekvenser
    SEXP s1 = strings[i];                     // s1 er sekvens i
    Rcpp::IntegerVector seq1(s1);             // konverterer til IntegerVector, seq1 er sekvens i
    int num_substr = seq1.length()-K+1;       // antall ord av lengde K i sekvens i
    for( int j=0; j<num_substr; j++ ) {       // looper over alle ord
      where = 0;
      for( int k=0; k<K; k++){                // looper over posisjon i ord
        where += seq1[j+k]*Where[k];          // where blir kolonnen til ordet i X, beregnet i 4-talls systemet
      }                                       // dette er alltid et tall fra 0 til (4^K)-1, med mindre
      if(where >= 0){                         // ett av elementene i sekvensen har verdien -4^K, da blir
        ++X(i, where);                        // where negativ, og ordet ignoreres 
      }
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
    X.attr("dimnames") = dimnms;
  }
  
  return X;
}
