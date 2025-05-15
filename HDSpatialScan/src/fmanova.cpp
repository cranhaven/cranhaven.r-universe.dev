// #include <Rcpp.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;


// [[Rcpp::export]]
List fmanova_cpp(List jeu, arma::mat matrice_clusters, arma::mat cst1, arma::mat cst2){

  float nb_clusters = matrice_clusters.n_cols ;
  float nb_processus = jeu.size() ;
  arma::mat mtmp = as<arma::mat>(jeu[0]) ;
  float nb_temps = mtmp.n_cols ;
  float nb_composantes = mtmp.n_rows ;

  List in_cluster(nb_clusters) ;
  for(int c = 0 ; c < nb_clusters ; c++){
    in_cluster[c] = find(matrice_clusters.col(c)==1);
  }

  List A_w_in(nb_clusters) ;
  List B_w_out(nb_clusters) ;
  List C_w_in_out(nb_clusters) ;

  for(int c = 0 ; c < nb_clusters ; c++){
    arma::mat moy_in_cluster(nb_composantes, nb_temps, fill::zeros);
    arma::mat moy_out_cluster(nb_composantes, nb_temps, fill::zeros);
    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;

    for(int i = 0 ; i < nb_processus ; i++){
      if(count(vtmp.begin(), vtmp.end(), i)){
        moy_in_cluster = moy_in_cluster + as<arma::mat>(jeu[i]);
      }
      else{
        moy_out_cluster += as<arma::mat>(jeu[i]);
      }
    }

    moy_in_cluster = moy_in_cluster/vtmp.n_elem ;
    moy_out_cluster = moy_out_cluster/(nb_processus - vtmp.n_elem) ;
    A_w_in[c] = moy_in_cluster * moy_in_cluster.t() ;
    B_w_out[c] = moy_out_cluster * moy_out_cluster.t() ;
    C_w_in_out[c] = moy_out_cluster * moy_in_cluster.t() + moy_in_cluster * moy_out_cluster.t() ;
    A_w_in[c] = as<arma::mat>(A_w_in[c]) / nb_temps ;
    B_w_out[c] = as<arma::mat>(B_w_out[c]) / nb_temps ;
    C_w_in_out[c] = as<arma::mat>(C_w_in_out[c]) / nb_temps ;

  }

  List H(nb_clusters) ;
  List E(nb_clusters) ;

  for(int c = 0 ; c < nb_clusters ; c++){
    arma::vec vtmp = as<arma::vec>(in_cluster[c]) ;
    H[c] = (vtmp.n_elem - 2/nb_processus * pow(vtmp.n_elem,2))*as<arma::mat>(A_w_in[c]) + (nb_processus - vtmp.n_elem - 2/nb_processus * pow((nb_processus - vtmp.n_elem),2))*as<arma::mat>(B_w_out[c])  - (2/nb_processus * vtmp.n_elem * (nb_processus - vtmp.n_elem)) * as<arma::mat>(C_w_in_out[c]) + cst2 ;
    E[c] = cst1 - vtmp.n_elem * as<arma::mat>(A_w_in[c]) - (nb_processus - vtmp.n_elem)*as<arma::mat>(B_w_out[c]);
  }

  arma::vec statslh(nb_clusters) ;
  arma::vec statsw(nb_clusters) ;
  arma::vec statsp(nb_clusters) ;
  arma::vec statsr(nb_clusters) ;

  for(int c = 0 ; c < nb_clusters ; c++){
    arma::mat invtmp1 = (as<arma::mat>(H[c])*inv(as<arma::mat>(E[c])));
    arma::mat invtmp2 = (as<arma::mat>(H[c])*inv(as<arma::mat>(H[c]) + as<arma::mat>(E[c])));
    statslh(c) = sum(invtmp1.diag()) ;
    statsp(c) = sum(invtmp2.diag()) ;
    statsw(c) = det(as<arma::mat>(E[c]))/det(as<arma::mat>(H[c]) + as<arma::mat>(E[c]));
    statsr(c) = real(eig_gen(invtmp1)).max() ;
  }

  List resultats = List::create(Named("LH") = statslh , _["W"] = statsw,
                                _["P"] = statsp, _["R"] = statsr);

  return resultats;

}
