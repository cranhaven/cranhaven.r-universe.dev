// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::export]]
arma::vec fanova_cpp(arma::mat data, arma::mat matrix_clusters){
  float nb_sites = data.n_rows;
  float nb_times = data.n_cols;
  float nb_clusters = matrix_clusters.n_cols ;

  List in_cluster(nb_clusters) ;
  for(int c = 0 ; c < nb_clusters ; c++){
    in_cluster[c] = find(matrix_clusters.col(c)==1);
  }

  arma::mat mean_inside(nb_times, nb_clusters, fill::zeros);
  arma::mat mean_outside(nb_times, nb_clusters, fill::zeros);

  for(int c = 0 ; c < nb_clusters ; c++){
    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;
    for(int i = 0 ; i < nb_sites ; i++){
      if(count(vtmp.begin(), vtmp.end(), i)){
        mean_inside.col(c) = mean_inside.col(c) + (data.row(i)).t();
      }
      else{
        mean_outside.col(c) = mean_outside.col(c) + (data.row(i)).t();
      }
    }
    mean_inside.col(c) = mean_inside.col(c)/vtmp.n_elem ;
    mean_outside.col(c) = mean_outside.col(c)/(nb_sites - vtmp.n_elem) ;
  }
  arma::mat mean_tot = mean(data).t();

  arma::vec norm_inside(nb_clusters, fill::zeros);
  arma::vec norm_outside(nb_clusters, fill::zeros);

  for(int c = 0 ; c < nb_clusters ; c++){
    norm_inside(c) = sum(pow(mean_inside.col(c) - mean_tot,2));
    norm_outside(c) = sum(pow(mean_outside.col(c) - mean_tot,2));
  }

  arma::mat temp_norm_inside(nb_sites, nb_clusters, fill::zeros);
  arma::mat temp_norm_outside(nb_sites, nb_clusters, fill::zeros);

  for(int c = 0 ; c < nb_clusters ; c++){
    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;
    for(int j = 0 ; j < nb_sites ; j++){
      if(count(vtmp.begin(), vtmp.end(), j)){
        temp_norm_inside(j,c) = sum(pow(data.row(j) - (mean_inside.col(c)).t(),2));
      }
      else{
        temp_norm_outside(j,c) = sum(pow(data.row(j) - (mean_outside.col(c)).t(),2));
      }
    }
  }

  arma::vec stats(nb_clusters, fill::zeros);

  for(int c = 0 ; c < nb_clusters ; c++){
    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;
    stats(c) = (nb_sites-2)* (vtmp.n_elem * norm_inside(c) + (nb_sites - vtmp.n_elem) * norm_outside(c)) / (sum(temp_norm_inside.col(c)) + sum(temp_norm_outside.col(c)));
  }

  return stats;
}
