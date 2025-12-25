#include <Rcpp.h>
#include <string>
#include <sstream>
#include <string.h>
#include <iostream>

using namespace std;

char returnChar (int x)
{
  char c = (char) x + 97;
  return c;
}

// // [[Rcpp::export]]
// Rcpp::NumericMatrix simMtx(int n, int p, Rcpp::NumericMatrix& memtx) {
//   //Randomly generates the model matrix for simulations
//   int numCol = 2*p*(p-1)+p;
//   Rcpp::NumericMatrix ret(n,numCol);
//   Rcpp::CharacterVector coln(numCol);
//   bool simme;
//   if (memtx.ncol() == 0){
//     //Sample independently
//     simme = true;
//   }
//   else{
//     simme = false;
//   }
//
//   //Simulating ME
//   for (int j=0; j < p; j++){
//     for (int i=0; i < n; i++){
//       if (simme){
//         ret(i,j) = ( (double) rand() / (RAND_MAX) );
//         if (ret(i,j) > 0.5){
//           ret(i,j) = 1;
//         }
//         else{
//           ret(i,j) = -1;
//         }
//       }
//       else{
//         ret(i,j) = memtx(i,j);
//       }
//     }
//     stringstream ss;
//     string s;
//     ss << returnChar(j);
//     ss >> s;
//     coln(j) = s;
//   }
//
//   //Computing CME
//   int curCol = p;
//   for (int i=0; i < p; i++){ // parent effect
//     for (int j=0; (j < p) ; j++){ // conditioned effect
//       if (i != j){
//         //cout << "curCol: " << curCol << ", (i,j): " << i << ", " << j << endl;
//         //i|j+
//         for (int k=0; k<n; k++){
//           if (ret(k,j) > 0){
//             ret(k,curCol) = ret(k,i);
//           }
//           //Otherwise 0
//           //Add colname
//           char str[4];
//           str[0] = returnChar(i);
//           str[1] = 124;
//           str[2] = returnChar(j);
//           str[3] = 43;
//
//           stringstream ss;
//           string s;
//           ss << str;
//           ss >> s;
//           coln(curCol) = s;
//
//         }
//
//         curCol++;
//
//         //i|j-
//         for (int k=0; k<n; k++){
//           if (ret(k,j) < 0){
//             ret(k,curCol) = ret(k,i);
//           }
//           //Otherwise 0
//           //Add colname
//           char str[4];
//           str[0] = returnChar(i);
//           str[1] = 124;
//           str[2] = returnChar(j);
//           str[3] = 45;
//
//           stringstream ss;
//           string s;
//           ss << str;
//           ss >> s;
//           coln(curCol) = s;
//         }
//
//         curCol++;
//       }
//     }
//   }
//   colnames(ret) = coln;
//   return(ret);
//
// }

// // [[Rcpp::export]]
// Rcpp::NumericMatrix getCME(NumericMatrix& xme) {
//   //Returns CME matrix for selection
//   int numCol = 2*p*(p-1)+p;
//   Rcpp::NumericMatrix ret(n,numCol);
//   Rcpp::CharacterVector coln(numCol);
//
//   //Simulating ME
//   for (int j=0; j < p; j++){
//     for (int i=0; i < n; i++){
//       ret(i,j) = ( (double) rand() / (RAND_MAX+1) );
//       if (ret(i,j) > 0.5){
//         ret(i,j) = 1;
//       }
//       else{
//         ret(i,j) = -1;
//       }
//     }
//     stringstream ss;
//     string s;
//     ss << returnChar(j);
//     ss >> s;
//     coln(j) = s;
//   }
//
//   //Computing CME
//   int curCol = p;
//   for (int i=0; i < p; i++){
//     for (int j=0; (j < p) ; j++){
//       if (i != j){
//         //cout << "curCol: " << curCol << ", (i,j): " << i << ", " << j << endl;
//         //i|j+
//         for (int k=0; k<n; k++){
//           if (ret(k,j) > 0){
//             ret(k,curCol) = ret(k,i);
//           }
//           //Otherwise 0
//           //Add colname
//           char str[4];
//           str[0] = returnChar(i);
//           str[1] = 124;
//           str[2] = returnChar(j);
//           str[3] = 43;
//
//           stringstream ss;
//           string s;
//           ss << str;
//           ss >> s;
//           coln(curCol) = s;
//
//         }
//
//         curCol++;
//
//         //i|j-
//         for (int k=0; k<n; k++){
//           if (ret(k,j) < 0){
//             ret(k,curCol) = ret(k,i);
//           }
//           //Otherwise 0
//           //Add colname
//           char str[4];
//           str[0] = returnChar(i);
//           str[1] = 124;
//           str[2] = returnChar(j);
//           str[3] = 45;
//
//           stringstream ss;
//           string s;
//           ss << str;
//           ss >> s;
//           coln(curCol) = s;
//         }
//
//         curCol++;
//       }
//     }
//   }
//   colnames(ret) = coln;
//   return(ret);
//
// }
