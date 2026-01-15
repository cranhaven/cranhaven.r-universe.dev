#include <Rmath.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List PleioSeqTestc(List imput_list, List Contrast_matrices, List Contrast_indices, double LoopBreaker)
{
  List output_list(imput_list.size());
  for(int k = 0; k < imput_list.size(); k++) {
    List result = imput_list[k];
    mat betas = as<mat>(result["betas"]).t();
    mat lhss = as<mat>(result["lhss"]);
    vec pvalues(Contrast_matrices.size());
    StringVector indices(Contrast_matrices.size());
    mat cmatrix1 = as<mat>(as<List>(Contrast_matrices[0])[0]);
    mat betas1 = cmatrix1 * betas.t();
    mat Var_1 = cmatrix1 * lhss * cmatrix1.t();
    double stat = as_scalar(betas1.t() * Var_1.i() * betas1);
    pvalues[0] = R::pchisq(stat, cmatrix1.n_rows, false, false);
    for (int j = 1; j < Contrast_matrices.size(); j++){
      if(pvalues[j - 1] > LoopBreaker){
        for (int i = j; i < Contrast_matrices.size(); i++){
          pvalues[i] = pvalues[j - 1];
        }
        break;
      }else{
        List cmatrix_j = Contrast_matrices[j];
        List cmatrix_j_1 = Contrast_indices[j - 1];
        vec p(cmatrix_j.size());
        for(int w = 0; w < cmatrix_j.size(); w++){
          mat cmatrix_w = as<mat>(cmatrix_j[w]);
          mat betas_w = cmatrix_w * betas.t();
          mat Var_w = cmatrix_w * lhss * cmatrix_w.t();
          double stat = as_scalar(betas_w.t() * Var_w.i() * betas_w);
          p[w] = R::pchisq(stat, cmatrix_w.n_rows, false, false);
        }
        indices[j - 1] = as<String>(cmatrix_j_1[p.index_max()]);
        pvalues[j] = p[p.index_max()];
      }
    }

    output_list[k] = List::create(Named("pValues") = pvalues,
                                  Named("index") = indices);
  }
  return output_list;
}
