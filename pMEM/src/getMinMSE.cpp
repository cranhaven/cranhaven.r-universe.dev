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

List getMinMSEReal(const NumericMatrix& U, const NumericVector& y,
                   const NumericMatrix& Up, const NumericVector& yy,
                   const bool complete = false) {
  
  int n = U.nrow(), nn = Up.nrow(), p = U.ncol();
  if(n != y.size())
    stop("Training set: %d rows, but %d observations!", n, y.size());
  if(nn != yy.size())
    stop("Testing set: %d rows, but %d observations!", nn, yy.size());
  if(p != Up.ncol())
    stop("Descriptors: %d for training, but %d for testing!", p, Up.ncol());
  
  double ym = 0.0, yn2 = 0.0, tmp, null_mse, ltabsb, lrabsb, b, min_mse,
    max_betasq;
  NumericVector yp(nn);
  NumericVector mse(p);
  NumericVector betasq(p);
  IntegerVector ord(p);
  
  int i, j, k, os;
  
  betasq[0] = 1.0;
  
  // Calculate the mean y (m):
  for(i = 0; i < n; i++)
    ym += y[i];
  ym /= n;
  
  // Calculate the sum of square mean deviation (yn2):
  for(i = 0; i < n; i++) {
    tmp = y[i] - ym;
    tmp *= tmp;
    yn2 += tmp;
  }
  
  // Init the prediction vector (yp) with the mean fitted value:
  for(i = 0; i < nn; i++)
    yp[i] = ym;
  
  // Calculates the mean square error:
  ym = 0.0;
  for(i = 0; i < nn; i++) {
    tmp = yp[i] - yy[i];
    tmp *= tmp;
    ym += tmp;
  }
  ym /= nn;
  null_mse = ym;
  
  // Calculate the b:
  for(j = 0, os = 0; j < p; j++) {
    ym = 0.0;
    for(i = 0; i < n; i++, os++)
      ym += U[os]*y[i];
    betasq[j] = ym;
  }
  
  // Find which b has the highest absolute value:
  ltabsb = fabs(betasq[0]);
  j = 0;
  for(i = 1; i < p; i++)
    if(fabs(betasq[i]) > ltabsb) {
      ltabsb = fabs(betasq[i]);
      j = i;
    }
  ord[0] = j;
  
  b = betasq[j];
  for(i = 0, os = j*nn; i < nn; i++, os++)
    yp[i] += b*Up[os];
  
  ym = 0.0;
  for(i = 0; i < nn; i++) {
    tmp = yp[i] - yy[i];
    tmp *= tmp;
    ym += tmp;
  }
  ym /= nn;
  
  mse[0] = ym;
  
  for(k = 1; k < p; k++) {
    lrabsb = 0.0;
    for(i = 0; i < p; i++) {
      tmp = fabs(betasq[i]);
      if(tmp < ltabsb)
        if(tmp > lrabsb) {
          lrabsb = tmp;
          j = i;
        }
    }
    ord[k] = j;
    ltabsb = lrabsb;
    
    b = betasq[j];
    for(i = 0, os = j*nn; i < nn; i++, os++)
      yp[i] += b*Up[os];
    
    ym = 0.0;
    for(i = 0; i < nn; i++) {
      tmp = yp[i] - yy[i];
      tmp *= tmp;
      ym += tmp;
    }
    ym /= nn;
    
    mse[k] = ym;
  }
  
  // Find the smallest mse:
  j = 0;
  tmp = mse[0];
  for(i = 1; i < p; i++)
    if(mse[i] < tmp) {
      tmp = mse[i];
      j = i;
    }
    
  min_mse = (tmp > null_mse) ? null_mse : tmp;
  max_betasq = (tmp > null_mse) ? 1.0 : betasq[j]*betasq[j]/yn2;
  
  if(complete) {
    for(i = 0; i < p; i++)  {
      betasq[i] = betasq[i]*betasq[i]/yn2;
      ord[i] += 1;
    }
    j = (tmp > null_mse) ? 0 : j + 1;
    
    return(List::create(_["betasq"] = betasq, _["nullmse"] = null_mse,
                        _["mse"] = mse, _["ord"] = ord, _["wh"] = j));
  } else
    return(List::create(_["betasq"] = max_betasq, _["mse"] = min_mse));
}
