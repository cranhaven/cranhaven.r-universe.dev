#include "naive.h"
#include "medianHeuristic.h"
#include "eummd.h"
#include "meammd.h"
#include <Rcpp.h>
// for size_t
#include <cstddef>

// [[Rcpp::export]]
Rcpp::List mmd_lap_Rcpp(Rcpp::NumericVector X, 
                        Rcpp::NumericVector Y, 
                        Rcpp::IntegerVector nX_, 
                        Rcpp::IntegerVector dX_,
                        Rcpp::IntegerVector nY_, 
                        Rcpp::IntegerVector dY_,
                        Rcpp::NumericVector beta_){

    // convert Rcpp types to primitive types
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    double beta = Rcpp::as<double> (beta_);
    if ( !(beta > 0) ){
        std::vector<double> Z( X.begin(), X.end() );
        // insert Y
        Z.insert( Z.end(), Y.begin(), Y.end() );
        int nZ = nX + nY;
        int dZ = dX;
        int kmethod = 1;
        double md = naive_multiv_medianHeuristic(Z, dZ, nZ, kmethod);
        beta = 1.0 / md;
    }

    // call C++ function; double*s use pointer
    double stat = cpp_mmd_lap(X.begin(), Y.begin(), nX, dX, nY, dY, beta);

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = -1, 
                              Rcpp::Named("beta") = beta);
}


// [[Rcpp::export]]
Rcpp::List mmd_gau_Rcpp(Rcpp::NumericVector X, 
                        Rcpp::NumericVector Y, 
                        Rcpp::IntegerVector nX_, 
                        Rcpp::IntegerVector dX_,
                        Rcpp::IntegerVector nY_, 
                        Rcpp::IntegerVector dY_,
                        Rcpp::NumericVector beta_){

    // convert Rcpp types to primitive types
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    double beta = Rcpp::as<double> (beta_);
    if ( !(beta > 0) ){
        std::vector<double> Z( X.begin(), X.end() );
        // insert Y
        Z.insert( Z.end(), Y.begin(), Y.end() );
        int nZ = nX + nY;
        int dZ = dX;
        // kmethod != 1 will mean Gaussian kernel is used
        int kmethod = 2;
        double md = naive_multiv_medianHeuristic(Z, dZ, nZ, kmethod);
        beta = 1.0 / md;
    }

    // call C++ function; double*s use pointer
    double stat = cpp_mmd_gau(X.begin(), Y.begin(), nX, dX, nY, dY, beta);

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = -1, 
                              Rcpp::Named("beta") = beta);
}

// [[Rcpp::export]]
Rcpp::NumericVector fast_median_diff_Rcpp(Rcpp::NumericVector X_){
    // copy to doule vector
    std::vector<double> X( X_.begin(), X_.end() );
    // call medianHeuristic, which actually computes median diff
    double ans = medianHeuristic(X); 
    return( Rcpp::NumericVector::create(ans) );
}



// [[Rcpp::export]]
Rcpp::NumericVector naive_median_diff_Rcpp(Rcpp::NumericVector Z_, 
                                           Rcpp::IntegerVector nZ_, 
                                           Rcpp::IntegerVector dZ_,
                                           Rcpp::IntegerVector kmethod_){

    // convert Rcpp types to primitive types
    int nZ = Rcpp::as<int> (nZ_);
    int dZ = Rcpp::as<int> (dZ_);
    int kmethod = Rcpp::as<int> (kmethod_);

    // copy to double vector
    std::vector<double> Z( Z_.begin(), Z_.end() );

    // call naive_multov_medianHeuristic, which actually computes median diff
    double ans = naive_multiv_medianHeuristic(Z, dZ, nZ, kmethod); 
    return( Rcpp::NumericVector::create(ans) );
}


// [[Rcpp::export]]
Rcpp::List mmd_lap_pval_Rcpp(Rcpp::NumericVector X, 
                             Rcpp::NumericVector Y, 
                             Rcpp::IntegerVector nX_, 
                             Rcpp::IntegerVector dX_,
                             Rcpp::IntegerVector nY_, 
                             Rcpp::IntegerVector dY_,
                             Rcpp::IntegerVector numperm_,
                             Rcpp::IntegerVector seednum_,
                             Rcpp::NumericVector beta_, 
                             Rcpp::IntegerVector twosided_, 
                             Rcpp::IntegerVector boundedminpval_){

    // convert Rcpp types to primitive types
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    int numperm = Rcpp::as<int> (numperm_);
    int seednum = Rcpp::as<int> (seednum_);
    double beta = Rcpp::as<double> (beta_);

    //twosided = 0 means false
    int twosided = Rcpp::as<int> (twosided_);
    int boundedminpval = Rcpp::as<int> (boundedminpval_);

    if ( !(beta > 0) ){
        std::vector<double> Z( X.begin(), X.end() );
        // insert Y
        Z.insert( Z.end(), Y.begin(), Y.end() );
        int nZ = nX + nY;
        int dZ = dX;
        // Laplacian, so kernel is 2
        int kmethod = 1;
        double md = naive_multiv_medianHeuristic(Z, dZ, nZ, kmethod);
        beta = 1.0 / md;
    }

    // call C++ function; double*s use pointer; returns vector of two elements
    std::vector<double> pvalstat = cpp_mmd_lap_pval(X.begin(), Y.begin(), 
                                                    nX, dX, nY, dY, 
                                                    numperm, seednum, beta, 
                                                    twosided, boundedminpval);
    // extract to doubles
    double pval = pvalstat[0];
    double stat = pvalstat[1];

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = pval, 
                              Rcpp::Named("beta") = beta);
}


// [[Rcpp::export]]
Rcpp::List mmd_gau_pval_Rcpp(Rcpp::NumericVector X, 
                             Rcpp::NumericVector Y, 
                             Rcpp::IntegerVector nX_, 
                             Rcpp::IntegerVector dX_,
                             Rcpp::IntegerVector nY_, 
                             Rcpp::IntegerVector dY_,
                             Rcpp::IntegerVector numperm_,
                             Rcpp::IntegerVector seednum_,
                             Rcpp::NumericVector beta_, 
                             Rcpp::IntegerVector twosided_, 
                             Rcpp::IntegerVector boundedminpval_){

    // convert Rcpp types to primitive types
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    int numperm = Rcpp::as<int> (numperm_);
    int seednum = Rcpp::as<int> (seednum_);
    double beta = Rcpp::as<double> (beta_);
    int twosided = Rcpp::as<int> (twosided_);
    int boundedminpval = Rcpp::as<int> (boundedminpval_);
    if ( !(beta > 0) ){
        std::vector<double> Z( X.begin(), X.end() );
        // insert Y
        Z.insert( Z.end(), Y.begin(), Y.end() );
        int nZ = nX + nY;
        int dZ = dX;
        // Gaussian, so kernel is 2
        int kmethod = 2;
        double md = naive_multiv_medianHeuristic(Z, dZ, nZ, kmethod);
        beta = 1.0 / md;
    }

    // call C++ function; double*s use pointer; returns vector of two elements
    std::vector<double> pvalstat = cpp_mmd_gau_pval(X.begin(), Y.begin(), 
                                                    nX, dX, nY, dY, 
                                                    numperm, seednum, beta, 
                                                    twosided, boundedminpval);
    // extract to doubles
    double pval = pvalstat[0];
    double stat = pvalstat[1];

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = pval, 
                              Rcpp::Named("beta") = beta);
}



// [[Rcpp::export]]
Rcpp::List eummd_Rcpp(Rcpp::NumericVector X_, 
                      Rcpp::NumericVector Y_, 
                      Rcpp::NumericVector beta_){

    // copy to double vectors
    std::vector<double> X( X_.begin(), X_.end() );
    std::vector<double> Y( Y_.begin(), Y_.end() );
    double beta = Rcpp::as<double> (beta_);

    // compute statistic (and return beta)
    std::vector<double> statbeta = cpp_eummd(X, Y, beta);

    // extract to doubles
    double stat = statbeta[0];
    beta = statbeta[1];

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = -1, 
                              Rcpp::Named("beta") = beta);
}


// [[Rcpp::export]]
Rcpp::List eummd_pval_Rcpp(Rcpp::NumericVector X_, 
                           Rcpp::NumericVector Y_, 
                           Rcpp::NumericVector beta_,
                           Rcpp::IntegerVector numperm_,
                           Rcpp::IntegerVector seednum_, 
                           Rcpp::IntegerVector twosided_, 
                           Rcpp::IntegerVector boundedminpval_){

    // copy to double vectors
    std::vector<double> X( X_.begin(), X_.end() );
    std::vector<double> Y( Y_.begin(), Y_.end() );
    double beta = Rcpp::as<double> (beta_);
    int numperm = Rcpp::as<int> (numperm_);
    int seednum = Rcpp::as<int> (seednum_);
    int twosided = Rcpp::as<int> (twosided_);
    int boundedminpval = Rcpp::as<int> (boundedminpval_);


    // get stat, pval and beta from cpp_eummd
    // twosided==0 means alternative is greater
    std::vector<double> pvalstatbeta = cpp_eummd_pval_faster(X, Y, beta, 
                                                             numperm, seednum,
                                                             twosided,
                                                             boundedminpval);

    double pval = pvalstatbeta[0];
    double stat = pvalstatbeta[1];
    beta = pvalstatbeta[2];

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = pval, 
                              Rcpp::Named("beta") = beta);
}



// [[Rcpp::export]]
double meammd_proj_Rcpp(Rcpp::NumericVector X, 
                        Rcpp::NumericVector Y, 
                        Rcpp::IntegerVector nX_, 
                        Rcpp::IntegerVector dX_, 
                        Rcpp::IntegerVector nY_, 
                        Rcpp::IntegerVector dY_, 
                        Rcpp::IntegerVector numproj_,
                        Rcpp::IntegerVector seednum_,
                        Rcpp::NumericVector beta_){

    // call a cpp_meammd_proj_pval_faster with numperm = 0
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    int numproj = Rcpp::as<int> (numproj_);
    int seednum = Rcpp::as<int> (seednum_);
    double beta = Rcpp::as<double> (beta_);

    double stat = cpp_meammd_proj_stat(X.begin(), 
                                       Y.begin(), 
                                       nX, dX,
                                       nY, dY,
                                       numproj,
                                       seednum, 
                                       beta);


    return(stat);
}


// [[Rcpp::export]]
Rcpp::List meammd_proj_pval_Rcpp(Rcpp::NumericVector X, 
                                 Rcpp::NumericVector Y, 
                                 Rcpp::IntegerVector nX_, 
                                 Rcpp::IntegerVector dX_, 
                                 Rcpp::IntegerVector nY_, 
                                 Rcpp::IntegerVector dY_, 
                                 Rcpp::IntegerVector numperm_,
                                 Rcpp::IntegerVector numproj_,
                                 Rcpp::IntegerVector seednum_,
                                 Rcpp::NumericVector beta_, 
                                 Rcpp::IntegerVector twosided_,
                                 Rcpp::IntegerVector boundedminpval_, 
                                 Rcpp::IntegerVector faster_){

    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    int numperm = Rcpp::as<int> (numperm_);
    int numproj = Rcpp::as<int> (numproj_);
    int seednum =  Rcpp::as<int> (seednum_);
    double beta = Rcpp::as<double> (beta_);
    int twosided = Rcpp::as<int> (twosided_);
    int boundedminpval = Rcpp::as<int> (boundedminpval_);
    int faster = Rcpp::as<int> (faster_);
    std::vector<double> pvalstat;
    if (faster == 0){
        pvalstat = cpp_meammd_proj_pval_faster(X.begin(), 
                                               Y.begin(), 
                                               nX, dX,
                                               nY, dY,
                                               numperm, 
                                               numproj,
                                               seednum, 
                                               beta, 
                                               twosided,
                                               boundedminpval);
    } else {
        pvalstat = cpp_meammd_proj_pval(X.begin(), 
                                        Y.begin(), 
                                        nX, dX,
                                        nY, dY,
                                        numperm, 
                                        numproj,
                                        seednum, 
                                        beta);
    }
    // pval is the first element
    // stat is the second element
    double pval = pvalstat[0];
    double stat = pvalstat[1];

    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = pval); 
}


// [[Rcpp::export]]
double meammd_dist_pval_Rcpp(Rcpp::NumericVector X, 
                             Rcpp::NumericVector Y, 
                             Rcpp::IntegerVector nX_, 
                             Rcpp::IntegerVector dX_, 
                             Rcpp::IntegerVector nY_, 
                             Rcpp::IntegerVector dY_, 
                             Rcpp::IntegerVector numperm_,
                             Rcpp::IntegerVector seednum_,
                             Rcpp::NumericVector beta_, 
                             Rcpp::NumericVector pmethod_, 
                             Rcpp::NumericVector nmethod_,
                             Rcpp::IntegerVector twosided_,
                             Rcpp::IntegerVector boundedminpval_){

    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    int numperm = Rcpp::as<int> (numperm_);
    int seednum =  Rcpp::as<int> (seednum_);
    double beta = Rcpp::as<double> (beta_);
    int pmethod = Rcpp::as<int> (pmethod_);
    int nmethod = Rcpp::as<int> (nmethod_);
    int twosided = Rcpp::as<int> (twosided_);
    int boundedminpval = Rcpp::as<int> (boundedminpval_);


    double pval = cpp_meammd_dist_pval(X.begin(), 
                                       Y.begin(), 
                                       nX, dX,
                                       nY, dY,
                                       numperm, 
                                       seednum, 
                                       beta, 
                                       pmethod, 
                                       nmethod, 
                                       twosided,
                                       boundedminpval);

    return(pval);
}


// [[Rcpp::export]]
Rcpp::List energydist_Rcpp(Rcpp::NumericVector X, 
                           Rcpp::NumericVector Y, 
                           Rcpp::IntegerVector nX_, 
                           Rcpp::IntegerVector dX_,
                           Rcpp::IntegerVector nY_, 
                           Rcpp::IntegerVector dY_){

    // convert Rcpp types to primitive types
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);

    // call C++ function; double*s use pointer
    double stat = cpp_energydist(X.begin(), Y.begin(), nX, dX, nY, dY);

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = -1);
}


// [[Rcpp::export]]
Rcpp::List energydist_pval_Rcpp(Rcpp::NumericVector X, 
                                Rcpp::NumericVector Y, 
                                Rcpp::IntegerVector nX_, 
                                Rcpp::IntegerVector dX_,
                                Rcpp::IntegerVector nY_, 
                                Rcpp::IntegerVector dY_,
                                Rcpp::IntegerVector numperm_,
                                Rcpp::IntegerVector seednum_, 
                                Rcpp::IntegerVector twosided_, 
                                Rcpp::IntegerVector boundedminpval_){

    // convert Rcpp types to primitive types
    int nX = Rcpp::as<int> (nX_);
    int dX = Rcpp::as<int> (dX_);
    int nY = Rcpp::as<int> (nY_);
    int dY = Rcpp::as<int> (dY_);
    int numperm = Rcpp::as<int> (numperm_);
    int seednum = Rcpp::as<int> (seednum_);
    int twosided = Rcpp::as<int> (twosided_);
    int boundedminpval = Rcpp::as<int> (boundedminpval_);

    // call C++ function; double*s use pointer; returns vector of two elements
    std::vector<double> pvalstat = cpp_energydist_pval(X.begin(), Y.begin(), 
                                                       nX, dX, nY, dY, 
                                                       numperm, seednum,
                                                       twosided, 
                                                       boundedminpval);
    // extract to doubles
    double pval = pvalstat[0];
    double stat = pvalstat[1];

    //return( Rcpp::NumericVector::create(ans) );
    return Rcpp::List::create(Rcpp::Named("stat") = stat, 
                              Rcpp::Named("pval") = pval);
}
