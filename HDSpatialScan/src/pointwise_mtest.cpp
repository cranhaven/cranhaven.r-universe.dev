// #include <Rcpp.h>
#include <Rmath.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;


// [[Rcpp::export]]
List pointwise_mtest_cpp(List data_transp, arma::mat matrix_clusters){
  double nb_clusters = matrix_clusters.n_cols ;
  double nb_temps = data_transp.size() ;
  arma::mat mtmp = as<arma::mat>(data_transp[0]) ;
  double nb_processus = mtmp.n_cols ;
  double nb_composantes = mtmp.n_rows ;
  List in_cluster(nb_clusters) ;
  List out_cluster(nb_clusters) ;
  for(int c = 0 ; c < nb_clusters ; c++){
    in_cluster[c] = find(matrix_clusters.col(c)==1);
    out_cluster[c] = find(matrix_clusters.col(c)==0);
  }
  arma::vec stat_2(nb_clusters, fill::zeros);

  for(int temps = 0 ; temps < nb_temps ; temps++){
    for(int c = 0 ; c < nb_clusters ; c++){
      Mat<double> moy_in_cluster = mat(nb_composantes, 1, fill::zeros);
      Mat<double> moy_out_cluster = mat(nb_composantes, 1, fill::zeros);
      arma::uvec vtmp = as<arma::uvec>(in_cluster[c]) ;
      arma::uvec vtmpout = as<arma::uvec>(out_cluster[c]) ;

      moy_in_cluster = mean(as<arma::mat>(data_transp[temps]).cols(vtmp),1);
      moy_out_cluster = mean(as<arma::mat>(data_transp[temps]).cols(vtmpout),1);



      Mat<double> Gamma;
      if(vtmp.n_elem == 1){
        Gamma = ((nb_processus - vtmp.n_elem - 1)* cov((as<arma::mat>(data_transp[temps]).cols(vtmpout)).t()))/(nb_processus - 2);
      }else{
        if(nb_processus - vtmp.n_elem == 1){
          Gamma = ((vtmp.n_elem-1)* cov((as<arma::mat>(data_transp[temps]).cols(vtmp)).t()))/(nb_processus - 2);
        }else{
          Gamma = ((vtmp.n_elem-1)* cov((as<arma::mat>(data_transp[temps]).cols(vtmp)).t()) + (nb_processus - vtmp.n_elem - 1)* cov((as<arma::mat>(data_transp[temps]).cols(vtmpout)).t()))/(nb_processus - 2);
        }
      }

    Mat<double> stat_T_temp = vtmp.n_elem * vtmpout.n_elem / nb_processus * (moy_in_cluster - moy_out_cluster).t() * inv(Gamma) * (moy_in_cluster - moy_out_cluster);
    stat_2(c) = max(stat_2(c), stat_T_temp(0,0));
    }
  }



  List resultats = List::create(Named("stat2") = stat_2);

  return resultats;

}
