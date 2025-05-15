// #include <Rcpp.h>
#include <Rmath.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::export]]
arma::vec mwmw_opti_cpp(arma::mat rank_data, arma::mat matrix_clusters){
  float nb_clusters = matrix_clusters.n_cols ;
  float nb_var = rank_data.n_cols ;
  float nb_sites = rank_data.n_rows ;

  List in_cluster(nb_clusters) ;
  for(int c = 0 ; c < nb_clusters ; c++){
    in_cluster[c] = find(matrix_clusters.col(c)==1);
  }

  float coeff = sum(sum(pow(rank_data,2),1)) ;
  coeff = nb_var * nb_sites / coeff ;

  arma::mat mean_inside(nb_clusters, nb_var, fill::zeros);
  arma::mat mean_outside(nb_clusters, nb_var, fill::zeros);

  for(int c = 0 ; c < nb_clusters ; c++){

    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;

    for(int i = 0 ; i < nb_sites ; i++){
      if(count(vtmp.begin(), vtmp.end(), i)){
        mean_inside.row(c) = mean_inside.row(c) + rank_data.row(i) ;
      }
      else{
        mean_outside.row(c) = mean_outside.row(c) + rank_data.row(i) ;
      }
    }

    mean_inside.row(c) = mean_inside.row(c)/vtmp.n_elem ;
    mean_outside.row(c) = mean_outside.row(c)/(nb_sites - vtmp.n_elem) ;

  }

  arma::vec norm2_inside = sum(pow(mean_inside,2), 1) ;
  arma::vec norm2_outside = sum(pow(mean_outside,2), 1) ;

  arma::vec stat(nb_clusters, fill::zeros);

  for(int c = 0 ; c < nb_clusters ; c++){

    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;
    stat(c) = coeff * ( vtmp.n_elem * norm2_inside(c) + (nb_sites - vtmp.n_elem) * norm2_outside(c) );
  }

  return stat ;

}


