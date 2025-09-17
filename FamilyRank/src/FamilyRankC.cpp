#include <algorithm>
#include <RcppArmadillo.h>
#include <iostream>
#include <armadillo>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// rcpp in; arma out
mat submat_arma2(NumericMatrix X, NumericVector T, int index) {
    mat Xmat(X.begin(), X.nrow(), X.ncol(), false);
    colvec tIdx(T.begin(), T.size(), false); 
    mat y = Xmat.rows(find(tIdx == index));
    return y;
}

NumericMatrix mmult1(NumericMatrix a, NumericMatrix b) {
    int acoln = a.ncol();
    int bcoln = b.ncol();
    NumericMatrix out = no_init_matrix(a.nrow(), acoln + bcoln);
    for (int j = 0; j < acoln + bcoln; j++) {
        if (j < acoln) {
            out(_, j) = a(_, j);
        } else {
            out(_, j) = b(_, j - acoln);
        }
    }
    return out;
}

// Grow Feature Set
// [[Rcpp::export]]
NumericMatrix grow(int n, int f, double d, NumericMatrix graph, NumericVector scores, NumericMatrix feat_mat, 
                   NumericMatrix score_mat, double tol, NumericMatrix weight_mat, NumericVector selected) {
    NumericVector index1 = graph.column(0);
    NumericVector index2 = graph.column(1);
    
    for(int c = 0; c < f; c++){
        int nextfeat = feat_mat(0, c);
        double tol_i = score_mat(0, c);
        
        NumericVector wscores = clone(scores);
        NumericVector wscores2 = clone(scores);
        NumericVector selected2 = clone(selected);
        selected2(nextfeat - 1) = 0;
        int r = 1;
        
        while(r < n && tol_i > tol){
            // update weights
            NumericMatrix sub_graph1 = Rcpp::wrap(submat_arma2(graph, index1, nextfeat));
            NumericMatrix sub_graph2 = Rcpp::wrap(submat_arma2(graph, index2, nextfeat));
            
            NumericVector indexi1 = sub_graph1.column(1);
            NumericVector indexi2 = sub_graph2.column(0);
            NumericVector csi1 = sub_graph1.column(2);
            NumericVector csi2 = sub_graph2.column(2);
            
            NumericVector interactions(n);
            
            int sz1 = indexi1.size(); 
            for(int i = 0; i < sz1; i++){
                interactions(indexi1(i) - 1) = csi1[i];
            }
            
            int sz2 = indexi2.size(); 
            for(int i = 0; i < sz2; i++){
                interactions(indexi2(i) - 1) = csi2[i];
            }
            
            // double feat_score = wscores(nextfeat - 1);
            for(int i = 0; i < n; i++){
                // wscores(i) = (1-d) * wscores(i) + d * (feat_score * interactions(i));
                wscores(i) = (1-d) * wscores(i) + d * (interactions(i));
            }
            
            wscores(nextfeat - 1) = 0;
            tol_i = max(wscores);
            nextfeat = which_max(wscores) + 1;
            
            bool s = selected2(nextfeat - 1) == 0;
            if(s){
                tol_i = tol;
            }else{
                selected2(nextfeat - 1) = 0;
                
                score_mat(r,c) = tol_i;
                feat_mat(r,c) = nextfeat;
                //Rprintf("%f", tol_i);
                weight_mat(nextfeat - 1, 0) = weight_mat(nextfeat - 1, 0) + tol_i;
                r = r+1;
            }
        }
    }
    
    NumericMatrix mat = mmult1(feat_mat, score_mat);
    NumericMatrix out = mmult1(weight_mat, mat);
    
    return(out); 
}



