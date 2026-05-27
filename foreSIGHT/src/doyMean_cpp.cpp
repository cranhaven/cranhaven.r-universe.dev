#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector doyMean_cpp(NumericVector obs, IntegerVector doy) {
  int nTot = obs.size();
  NumericVector clim(366); 
  //FILE *fptr;
  IntegerVector count(366);

    
  for (int d = 0; d < 365; ++d) {
    clim[d] = 0;
    count[d] = 0;
//    clim[d] = 100.;
    for (int n = 0; n < nTot; ++n) {
      //      printf("n = %d\n", n);
      //      printf("d = %d\n", d);
      if (doy[n] == d+1) {
        clim[d] = clim[d] + obs[n];
        count[d] = count[d] + 1;
      }
    }  
    clim[d] = clim[d]/count[d];
  }
  clim[365] = clim[364]; 

   

/*for (int d = 1; d < 365; ++d) {
//  clim[d] = pow(1.,d)+100.;
  clim[d] = 100.;
}
*/
 /* 
  // use appropriate location if you are using MacOS or Linux
//  fptr = fopen("C:/Users/a1065639/Box/2021_foreSIGHT/foreSIGHT_2.0/check.txt","w");
  fptr = fopen("check.txt","w");
  fprintf(fptr,"%d",10);
  fclose(fptr);
  */

  return clim;
}


