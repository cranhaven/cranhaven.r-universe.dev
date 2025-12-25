
// [[Rcpp::depends(RcppArmadillo)]]
#include "RcppArmadillo.h"


// [[Rcpp::export]]
Rcpp::List getCenters_cpp2(arma::mat & X,
                          arma::vec clusID,
                          arma::uword k,
                          double lambda) {
  
  
  try {
    //convert to 0-based indexing
    clusID = clusID - 1;
    
    arma::uword n = X.n_rows;
    arma::uword p = X.n_cols;
    arma::mat centers(k, p, arma::fill::zeros);
    arma::vec center_clas(k, arma::fill::zeros);
    
    for (arma::uword j = 0; j < p; j++) {
      arma::vec x = X.col(j);
      double withinss = 0;
      for (arma::uword i = 0; i < k; i++) {
        arma::uvec idxi = arma::find(clusID == i);
        center_clas(i) = arma::mean(x(idxi));
        withinss = withinss + arma::accu(arma::square(x(idxi) - center_clas(i)));
      }
      if (arma::accu(arma::square(x)) <= withinss +  (double)n * lambda) {
        center_clas.zeros();
      }
      centers.col(j) = center_clas;
    }
    
    
    return Rcpp::List::create(Rcpp::Named("centers") = centers);
    
  } catch( std::exception& __ex__ )
  {
    forward_exception_to_r( __ex__ );
  } catch(...)
  {
    ::Rf_error( "c++ exception " "(unknown reason)" );
  }
  return Rcpp::wrap(NA_REAL);
}


void getCenters_cpp(arma::mat & X,
                    arma::mat & centers,
                    arma::vec & clusID,
                    arma::uword k,
                    double lambda) {
  // Updates the centers for the regularized k-means, assuming fixed
  // assignment of the observations to k clusters
  // args:
  //   X: n x p data matrix
  //   clusID: vector of length n with cluster memberships in 1, ..., k
  //   k: number of clusters
  //   lambda: penalization parameter
  // returns:
  //   void: updates &centers
  
  arma::uword n = X.n_rows;
  arma::uword p = X.n_cols;
  
  arma::vec center_clas(k, arma::fill::zeros);
  
  for (arma::uword j = 0; j < p; j++) {
    arma::vec x = X.col(j);
    double withinss = 0;
    for (arma::uword i = 0; i < k; i++) {
      arma::uvec idxi = arma::find(clusID == i);
      center_clas(i) = arma::mean(x(idxi));
      withinss = withinss + arma::accu(arma::square(x(idxi) - center_clas(i)));
    }
    if (arma::accu(arma::square(x)) <= withinss +  (double)n * lambda) {
      center_clas.zeros();
    }
    centers.col(j) = center_clas;
  }
}



void getIDs_cpp(arma::mat & X,
                arma::mat & centers,
                arma::vec & clusID) {
  // calculate the new cluster assignments
  
  for (arma::uword i = 0; i < X.n_rows; i++) {
    arma::rowvec x = X.row(i);
    arma::mat centers0 = centers;
    centers0.each_row() -= x; 
    arma::vec cost = arma::sum(arma::square(centers0), 1); //rowsums
    clusID(i) = arma::index_min(cost);
  }
}




// [[Rcpp::export]]
Rcpp::List HTKmeans_inner_cpp(arma::mat X,
                              arma::mat initcenters,
                              arma::vec initIDs, 
                              double lambda,
                              arma::uword iter_max) {
  
  try {
    //convert to 0-based indexing
    initIDs = initIDs - 1;
    
    arma::vec clusIDs0 = initIDs;
    arma::vec clusIDs  = initIDs;
    arma::mat centers = initcenters;
    arma::uword k = centers.n_rows;
    int converged = 0;
    arma::uword itnb = 0;
    while (itnb < iter_max && (converged == 0)) {
      
      // Get new cluster assignments (in-place )
      getIDs_cpp(X, centers, clusIDs);
      
      arma::uvec nbUniqueCenters = arma::find_unique(clusIDs, false);
      if (nbUniqueCenters.n_elem < k) {
        centers.zeros();
        converged = 0;
        break;
      }
      // get new centers (update in-place)
      getCenters_cpp(X, centers, clusIDs, k, lambda);
      
      // bookkeeping
      converged = (arma::accu(arma::abs(clusIDs - clusIDs0)) == 0)? 1 : 0;
      clusIDs0  = clusIDs;
      itnb++;
    }
    
    //convert to 1-based indexing
    clusIDs = clusIDs + 1;
    return Rcpp::List::create(Rcpp::Named("centers") = centers,
                              Rcpp::Named("cluster") = clusIDs,
                              Rcpp::Named("itnb") = itnb,
                              Rcpp::Named("converged") = converged);
    
  } catch( std::exception& __ex__ )
  {
    forward_exception_to_r( __ex__ );
  } catch(...)
  {
    ::Rf_error( "c++ exception " "(unknown reason)" );
  }
  return Rcpp::wrap(NA_REAL);
}


// [[Rcpp::export]]
Rcpp::List getObjective_cpp(arma::mat & X,
                            arma::mat & centers,
                            arma::uvec IDs, 
                            double lambda) {
  
  try {
    //convert to 0-based indexing
    IDs = IDs - 1;
    
    double obj = 0, obj_penalty = 0, obj_WCSS = 0;
    double WCSS_nonZero = 0;
    arma::uword n = X.n_rows;
    
    arma::uvec activeVars = arma::find(arma::max(arma::abs(centers), 0) > 1e-10);
    arma::uword nbActive = activeVars.n_elem;
    
    if (nbActive == 0) {
      obj = obj_WCSS = ((double) arma::accu(arma::square(X))) /
        ((double) n);
    } else {
      for (arma::uword i = 0; i < n; i++) {
        arma::rowvec x = X.row(i);
        arma::uword id = IDs(i);
        arma::rowvec wcss = arma::square(x - centers.row(id));
        obj_WCSS = obj_WCSS + arma::accu(wcss);
        WCSS_nonZero = WCSS_nonZero + arma::accu(wcss(activeVars));
      }
      obj_WCSS = obj_WCSS / ((double) n);
      WCSS_nonZero = WCSS_nonZero / ((double) n);
      obj_penalty = lambda * (double) nbActive;
      obj = obj_WCSS + obj_penalty;
    }
    
    // convert back to 1-based indexing
    activeVars = activeVars + 1;
    return Rcpp::List::create(Rcpp::Named("obj") = obj,
                              Rcpp::Named("obj_penalty") = obj_penalty,
                              Rcpp::Named("obj_WCSS") = obj_WCSS,
                              Rcpp::Named("WCSS_nonZero") = WCSS_nonZero,
                              Rcpp::Named("nbActive") = nbActive,
                              Rcpp::Named("activeVars") = activeVars);
    
  } catch( std::exception& __ex__ )
  {
    forward_exception_to_r( __ex__ );
  } catch(...)
  {
    ::Rf_error( "c++ exception " "(unknown reason)" );
  }
  return Rcpp::wrap(NA_REAL);
}

