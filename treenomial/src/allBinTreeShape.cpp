// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include "coeffMatrix.h"

using namespace arma;

// [[Rcpp::export]]
Rcpp::List allBinaryTreeShapesReal(int numTips){
  int currTips = 2;
  int treeCount = 1;
  uword lengthA = 0;
  uword lengthB = 0;


  std::vector<std::vector<SpMat<double>>> nTipsTrees;
  std::vector<SpMat<double>> temp;

  SpMat<double> leaf(1,2);
  leaf(0,1) = 1;

  std::vector<SpMat<double>> oneTips;
  oneTips.push_back(leaf);
  nTipsTrees.push_back(oneTips);


  while(currTips != (numTips + 1)){
    nTipsTrees.push_back(temp);

    treeCount = 0;
    for(int i = 1; i < currTips; i++){
      for(int j = 1; j <= i; j++){

        /*
         *  first two for loops combined with the if
         *  below cover all combinations of smaller
         *  trees tips that could be used to create
         *  curr tips
         *
         */
        if((i+j) == currTips){
          lengthA = nTipsTrees[i-1].size();
          lengthB = nTipsTrees[j-1].size();

          // now loop over all trees in each tip level
          for(uword k = 1; k <= lengthA; k++){
            for(uword l = 1; l <= lengthB; l++){
              if(i == j){
                // ensure only one order of combination is taken
                if(k >= l){
                  treeCount ++;
                  nTipsTrees[currTips-1].push_back(wedge( nTipsTrees[i-1][k-1], nTipsTrees[j-1][l-1]));
                }
              } else {

                treeCount ++;
                nTipsTrees[currTips-1].push_back(wedge( nTipsTrees[i-1][k-1], nTipsTrees[j-1][l-1]));
              }
            }
          }
        }
      }
    }
    currTips++;
  }


  Rcpp::List A(nTipsTrees.size());
  for(uword i = 0; i < nTipsTrees.size(); i++){
    Rcpp::List B(nTipsTrees[i].size());
    for(uword j = 0; j < nTipsTrees[i].size(); j++){
      B[j] = Rcpp::wrap(mat(nTipsTrees[i][j]));
    }
    A[i] = B;
  }

  return(A);
}

/*
 * The phylo version only returns the combinations of trees
 * so the phylo wedge can be used in R
 *
 */
// [[Rcpp::export]]
Rcpp::List allBinaryTreeShapesPhylo(int numTips){
  int currTips = 2;
  int treeCount = 1;
  int lengthA = 0;
  int lengthB = 0;

  std::vector<std::vector<vec>> nTipsTrees;
  std::vector<vec> temp;

  vec leaf(1, fill::ones);

  std::vector<vec> oneTips;
  oneTips.push_back(leaf);
  nTipsTrees.push_back(oneTips);

  while(currTips != (numTips + 1)){
    nTipsTrees.push_back(temp);

    treeCount = 0;
    for(int i = 1; i < currTips; i++){
      for(int j = 1; j <= i; j++){
        if((i+j) == currTips){
          lengthA = (int)nTipsTrees[i-1].size();
          lengthB = (int)nTipsTrees[j-1].size();

          for(int k = 1; k <= lengthA; k++){
            for(int l = 1; l <= lengthB; l++){
              if(i == j){
                if(k >= l){
                  treeCount ++;
                  vec res(4);
                  res[0] = i;
                  res[1] = k;
                  res[2] = j;
                  res[3] = l;
                  nTipsTrees[currTips-1].push_back(res);
                }
              } else {

                treeCount ++;
                vec res(4);
                res[0] = i;
                res[1] = k;
                res[2] = j;
                res[3] = l;
                nTipsTrees[currTips-1].push_back(res);
              }
            }
          }
        }
      }
    }
    currTips++;
  }

  Rcpp::List A(nTipsTrees.size());
  for(uword i = 0; i < nTipsTrees.size(); i++){
    Rcpp::List B(nTipsTrees[i].size());
    for(uword j = 0; j < nTipsTrees[i].size(); j++){
      if(i != 0){
        B[j] = Rcpp::wrap(nTipsTrees[i][j]);
      } else {
        B[j] = "single node";
      }
    }
    A[i] = B;
  }

  return(A);
}

// [[Rcpp::export]]
Rcpp::List allBinaryTreeShapesComplex(int numTips, arma::cx_double y){
  int currTips = 2;
  int treeCount = 1;
  uword lengthA = 0;
  uword lengthB = 0;

  std::vector<std::vector<cx_rowvec>> nTipsTrees;
  std::vector<cx_rowvec> temp;

  cx_rowvec leaf(2);
  leaf[1] = cx_double(1, 0);

  std::vector<cx_rowvec> oneTips;
  oneTips.push_back(leaf);
  nTipsTrees.push_back(oneTips);

  while(currTips != (numTips + 1)){
    nTipsTrees.push_back(temp);

    treeCount = 0;
    for(int i = 1; i < currTips; i++){
      for(int j = 1; j <= i; j++){

        if((i+j) == currTips){
          lengthA = nTipsTrees[i-1].size();
          lengthB = nTipsTrees[j-1].size();

          for(uword k = 1; k <= lengthA; k++){
            for(uword l = 1; l <= lengthB; l++){
              if(i == j){
                if(k >= l){
                  treeCount ++;
                  nTipsTrees[currTips-1].push_back(wedgeConv( nTipsTrees[i-1][k-1], nTipsTrees[j-1][l-1],y));
                }
              } else {

                treeCount ++;
                nTipsTrees[currTips-1].push_back(wedgeConv( nTipsTrees[i-1][k-1], nTipsTrees[j-1][l-1],y));
              }
            }
          }
        }
      }
    }
    currTips++;
  }

  Rcpp::List A(nTipsTrees.size());
  for(uword i = 0; i < nTipsTrees.size(); i++){
    Rcpp::List B(nTipsTrees[i].size());
    for(uword j = 0; j < nTipsTrees[i].size(); j++){
      B[j] = Rcpp::wrap(nTipsTrees[i][j]);
    }
    A[i] = B;
  }

  return(A);
}

