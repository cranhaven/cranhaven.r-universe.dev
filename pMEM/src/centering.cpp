/***************************************************************************\
 *
 * (c) 2023-2024 Guillaume Guénard
 *     Department de sciences biologiques,
 *     Université de Montréal
 *     Montreal, QC, Canada
 *
 * Calculations of predictive Moran's Eigenvector Maps (pMEM), which extends
 * classical MEM for making predictions between the sampling points. It also
 * implements 
 *
\***************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

List centerReal(const NumericMatrix& x, const bool dcnt) {
  
  int i, j, os;
  double m = 0.0;
  
  int n = x.size();
  int nrow = x.nrow();
  int ncol = x.ncol();
  
  NumericVector rm(nrow);
  NumericVector cm(ncol);
  NumericMatrix xc = clone(x);
  List out;
  
  if(dcnt)
    for(i = 0; i < nrow; i++) {
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        rm[i] += x[os];
      rm[i] /= ncol;
    }
  
  for(j = 0, os = 0; j < ncol; j++) {
    for(i = 0; i < nrow; i++, os++)
      cm[j] += x[os];
    if(dcnt)
      m += cm[j];
    cm[j] /= nrow;
  }
  
  if(dcnt) {
    m /= n;
    for(j = 0; j < ncol; j++)
      cm[j] -= m;
  }
  
  if(dcnt)
    for(j = 0, os = 0; j < ncol; j++)
      for(i = 0; i < nrow; i++, os++)
        xc[os] = x[os] - rm[i] - cm[j];
  else
    for(j = 0, os = 0; j < ncol; j++)
      for(i = 0; i < nrow; i++, os++)
        xc[os] = x[os] - cm[j];
  
  out = List::create(_["centered"] = xc, _["centers"] = cm);
  
  return(out);
}

List centerCplx(const ComplexMatrix& x, const bool dcnt) {
  
  int i, j, os;
  Rcomplex m;
  m = {0.0, 0.0};
  
  int n = x.size();
  int nrow = x.nrow();
  int ncol = x.ncol();
  
  Rcomplex cplx_n, cplx_nrow, cplx_ncol;
  cplx_n.r = double(n), cplx_n.i = 0.0;
  cplx_nrow.r = double(nrow), cplx_nrow.i = 0.0;
  cplx_ncol.r = double(ncol), cplx_ncol.i = 0.0;
  
  ComplexVector rm(nrow);
  ComplexVector cm(ncol);
  ComplexMatrix xc = clone(x);
  List out;
  
  if(dcnt)
    for(i = 0; i < nrow; i++) {
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        rm[i] = rm[i] + x[os];
      rm[i] = rm[i]/cplx_ncol;
    }
  
  for(j = 0, os = 0; j < ncol; j++) {
    for(i = 0; i < nrow; i++, os++)
      cm[j]  = cm[j] + x[os];
    if(dcnt)
      m = m + cm[j];
    cm[j] = cm[j]/cplx_nrow;
  }
  
  if(dcnt) {
    m = m/cplx_n;
    for(j = 0; j < ncol; j++)
      cm[j] = cm[j] - m;
  }
  
  if(dcnt)
    for(j = 0, os = 0; j < ncol; j++)
      for(i = 0; i < nrow; i++, os++)
        xc[os] = x[os] - rm[i] - cm[j];
  else
    for(j = 0, os = 0; j < ncol; j++)
      for(i = 0; i < nrow; i++, os++)
        xc[os] = x[os] - cm[j];
  
  out = List::create(_["centered"] = xc, _["centers"] = cm);
  
  return(out);
}

NumericMatrix recenterReal(const NumericMatrix& x, const NumericVector& c,
                           const bool dcnt) {
  
  int i, j, os;
  double rm;
  
  // int n = x.size();
  int nrow = x.nrow();
  int ncol = x.ncol();
  
  if(ncol != c.size())
    stop("%d column centers given for %d columns!", c.size(), ncol);
  
  NumericMatrix y = clone(x);
  
  if(dcnt)
    for(i = 0; i < nrow; i++) {
      rm = 0.0;
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        rm += x[os];
      rm /= ncol;
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        y[os] = x[os] - rm - c[j];
    }
  else
    for(i = 0; i < nrow; i++)
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        y[os] = x[os] - c[j];
  
  return(y);
}

ComplexMatrix recenterCplx(const ComplexMatrix& x, const ComplexVector& c,
                           const bool dcnt) {
  
  int i, j, os;
  Rcomplex rm;
  
  // int n = x.size();
  int nrow = x.nrow();
  int ncol = x.ncol();
  
  Rcomplex cplx_ncol;
  cplx_ncol.r = double(ncol), cplx_ncol.i = 0.0;
  
  if(ncol != c.size())
    stop("%d column centers given for %d columns!", c.size(), ncol);
  
  ComplexMatrix y = clone(x);
  
  if(dcnt)
    for(i = 0; i < nrow; i++) {
      rm = {0.0, 0.0};
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        rm = rm + x[os];
      rm = rm / cplx_ncol;
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        y[os] = x[os] - rm - c[j];
    }
  else
    for(i = 0; i < nrow; i++)
      for(j = 0, os = i; j < ncol; j++, os += nrow)
        y[os] = x[os] - c[j];
  
  return(y);
}
