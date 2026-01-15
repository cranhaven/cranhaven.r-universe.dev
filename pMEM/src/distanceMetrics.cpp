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
 * Calculation of distance metrics describing a symmetrical spatial processes
 * (i.e., the plain, real-values, Euclidean distance) or an asymmetrical process
 * (i.e., an Hermitian complex-values metric based on the Euclidean distance).
 *
 \***************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix EuclidReal(const NumericMatrix& x, const NumericMatrix& y) {
  
  int i, j, k = 0;
  double tmp, acc;
  
  int nc = x.ncol();
  if(nc != y.ncol())
    stop("'x' has %d columns and 'y' has %d!", x.ncol(), y.ncol());
  
  int nx = x.nrow();
  int ny = y.nrow();
  List dmy = y.attr("dimnames");
  List dmx = x.attr("dimnames");
  NumericMatrix out(ny, nx);
  out.attr("dimnames") = List::create(dmy[0], dmx[0]);
  
  if(nc > 1)
    for(i = 0; i < ny; i++)
      for(j = 0; j < nx; j++) {
        acc = 0.0;
        for(k = 0; k < nc; k++) {
          tmp = y(i,k) - x(j,k);
          tmp *= tmp;
          acc += tmp;
        }
        out(i,j) = sqrt(acc);
      }
      else
        for(i = 0; i < ny; i++)
          for(j = 0; j < nx; j++)
            out(i,j) = fabs(y(i,k) - x(j,k));
      
      return(out);
}

ComplexMatrix EuclidCplx2D(const NumericMatrix& x, const NumericMatrix& y,
                           const double delta, const double theta) {
  
  int i, j, k = 0;
  double tmp, mod, arg;
  
  int nc = x.ncol();
  if(nc > 2)
    stop("This implementation handles only two-dimensions or less!");
  if(nc != y.ncol())
    stop("'x' has %d columns and 'y' has %d!", x.ncol(), y.ncol());
  
  int nx = x.nrow();
  int ny = y.nrow();
  List dmy = y.attr("dimnames");
  List dmx = x.attr("dimnames");
  ComplexMatrix out(ny, nx);
  out.attr("dimnames") = List::create(dmy[0], dmx[0]);
  
  if(nc > 1)
    for(i = 0; i < ny; i++)
      for(j = 0; j < nx; j++) {
        mod = 0.0;
        for(k = 0; k < nc; k++) {
          tmp = y(i,k) - x(j,k);
          tmp *= tmp;
          mod += tmp;
        }
        mod = sqrt(mod);
        arg = delta*cos(atan2(x(j,1) - y(i,1), x(j,0) - y(i,0)) - theta);
        out(i,j).r = mod*cos(arg);
        out(i,j).i = mod*sin(arg);
        
      }
      else
        for(i = 0; i < ny; i++)
          for(j = 0; j < nx; j++) {
            tmp = x(j,k) - y(i,k);
            mod = fabs(tmp);
            arg = (tmp > 0.0) ? delta : ((tmp < 0.0) ? -delta : 0.0);
            out(i,j).r = mod*cos(arg);
            out(i,j).i = mod*sin(arg);
          }
  
  return(out);
}
