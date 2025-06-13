#include <iostream>
#include <iomanip>
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace std;
using namespace arma;
using namespace Rcpp;


Mat<double> convertVecToMat(const Col<double>&  vec, const int  dim){
  Mat<double> mat(dim, dim);
  mat.zeros(dim, dim);
  int cp=0;
  for (int j=0; j<dim; j++){
    for (int i=j; i<dim; i++){
      mat(i,j) = vec(cp);
      cp++;
    }
  }
  return mat;
}

Row<double> convertMatToVec(const Mat<double>  mat){
  int dim = ((mat.n_rows - 1) * mat.n_rows) /2;
  Row<double> out(dim);
  out.zeros(dim);
  int cp=0;
  for (int j=0; j<mat.n_cols; j++){
    for (int i=j; i<(mat.n_rows-1); i++){
      out(cp) = mat(i,j);
      cp++;
    }
  }
  return out;
}

Mat<double> phiinvall(const Mat<double>& loguref, const Mat<double> mumat){
  const Mat<double> mumatinv = inv(mumat);
  Row<double> tmp = 0.5 * trans(mumatinv * sum(mumat % mumat, 1));
  Mat<double> out = loguref * trans(mumatinv);
  out.each_row() += tmp;
  return out;
}

Row<double> gradphiinvhh(const Row<double> yi, const Mat<double>& mumatupref, const int h){
  Row<double> out = yi*0;
  for (int l=0; l<=h; l++)
    out(l) = (mumatupref(h,l) - yi(l)) /mumatupref(h,h);

  return out ;
}

vector< Mat<double> > computegradPhiInv(const Row<double> yi, const Mat<double>& mumatupref){
  vector< Mat<double> > listgradphiinvhkl;
  const int dim=mumatupref.n_cols;
  listgradphiinvhkl.resize(dim);
  for (int h=0; h<dim; h++){
    listgradphiinvhkl[h] = zeros<mat>(dim + 1, dim);
    listgradphiinvhkl[h].row(h) = gradphiinvhh(yi, mumatupref, h);
    if (h>0){
      Mat<double> add=zeros<mat>(dim + 1, dim);
      for (int hp=0; hp<h; hp++)
        add += listgradphiinvhkl[hp] * mumatupref(h, hp);

      listgradphiinvhkl[h] -= (add/mumatupref(h,h));
    }
  }
  return listgradphiinvhkl;
}

Mat<double> gradl1ind(const Row<double> w, const Row<double> yi, const Mat<double>& mumatupref){
  const int dim = mumatupref.n_cols;
  Mat<double> out=zeros<mat>(dim + 1, dim);
  vector< Mat<double> > gradphiinv = computegradPhiInv(yi, mumatupref);
  Mat<double> delta=zeros<mat>(dim + 1, dim);
  for (int k=0; k<=dim; k++)
    delta.row(k) = (mumatupref.row(k) - yi) * w(k);

  // for (int l=0; l<dim; l++)
  //   delta.col(l) = delta.col(l) * w(l);

  Row<double> sumdelta = sum(delta, 0);

  for (int h=0; h<dim; h++)
    out += gradphiinv[h] * sumdelta(h);

  for (int k=0; k<dim; k++){
    for(int l=0; l<=k; l++){
      out(k,l) -= delta(k,l) ;
    }
  }
  return out;
}

double Sign(const double number){
  double out = 1.0;
  if (number < 0) out= -1.0;
  return out;
}

Row<double> computeGradient(const Col<double>& param, const Mat<double>& loguref, const Mat<double>& tikref){
  Mat<double> mumat = convertVecToMat(param, loguref.n_cols);
  Mat<double> y = phiinvall(loguref, mumat);
  Mat<double> grr = zeros(mumat.n_rows + 1, mumat.n_cols);
  grr.zeros(mumat.n_rows+1, mumat.n_cols);
  for (int k=0; k<grr.n_cols; k++)
     grr(k,k) = - (Sign(mumat(k,k))/mumat(k,k)) * (loguref.n_rows);

  Mat<double> mumatup=resize(mumat, mumat.n_rows + 1, mumat.n_cols);
  mumatup.row(mumatup.n_rows -1) = 0 * mumatup.row(mumatup.n_rows -1);
  Mat<double>& mumatupref = mumatup;
  for (int i=0; i<(loguref.n_rows); i++)
    grr += gradl1ind(tikref.row(i), y.row(i), mumatupref);

  return convertMatToVec(grr);
}

double computeCompleteLogLikelihood(const Col<double>& param, const vector<double>& prop, const Mat<double>& loguref, const Mat<double>& tikref){
  Mat<double> mumat = convertVecToMat(param, loguref.n_cols);
  Mat<double> y = phiinvall(loguref, mumat);
  Mat<double> mumatup=resize(mumat, mumat.n_rows + 1, mumat.n_cols);
  mumatup.row(mumatup.n_rows -1) = 0 * mumatup.row(mumatup.n_rows -1);
  double out=0;
  double d=y.n_cols;
  for (int k=0; k<=d; k++){
    Mat<double> tmp = (y.each_row() - mumatup.row(k));
    out+= (-0.5) * sum(sum(tmp % tmp, 1) % tikref.col(k)) + sum(tikref.col(k)) * log(prop[k]) - sum(tikref.col(k))*(d)*log(sqrt(2*M_PI));
  }
  for (int k=0; k<d; k++)
    out -= sum(log(abs(mumat(k,k)))) * (loguref.n_rows);
  out -= sum(sum(loguref));
  return out;
}

//[[Rcpp::export]]
NumericVector  computeGradientCPP(NumericVector Rparam, NumericVector Rprop, NumericMatrix Rlogu, NumericMatrix Rtik){
  Mat<double> logu(Rlogu.nrow(), Rlogu.ncol());
  logu= as<mat>(Rlogu);
  Mat<double>& loguref = logu;
  Mat<double> tik(Rtik.nrow(), Rtik.ncol());
  tik= as<mat>(Rtik);
  Mat<double>& tikref= tik;
  Col<double> param(Rparam.size());
  param= as<vec>(Rparam);
  Col<double>& paramref = param;
  Row<double> out=computeGradient(paramref, loguref, tikref);
  return wrap(out);
}

//[[Rcpp::export]]
NumericVector  computeLikelihoodCPP(NumericVector Rparam, NumericVector Rprop, NumericMatrix Rlogu, NumericMatrix Rtik){
  Col<double> param(Rparam.size());
  param= as<vec>(Rparam);
  Col<double>& paramref = param;


  vector< double > prop(Rprop.size());
  for (int j=0; j<prop.size(); j++) prop[j] = Rprop[j];
  vector<double>& propref = prop;

  Mat<double> logu(Rlogu.nrow(), Rlogu.ncol());
  logu= as<mat>(Rlogu);
  Mat<double>& loguref = logu;

  Mat<double> tik(Rtik.nrow(), Rtik.ncol());
  tik= as<mat>(Rtik);
  Mat<double>& tikref= tik;

  double out=computeCompleteLogLikelihood(paramref, propref, loguref, tikref);
  return wrap(out);
}
