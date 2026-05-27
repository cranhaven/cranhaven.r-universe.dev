#include <Rcpp.h>
# include "fittingmeasure.h"
using namespace Rcpp;

//' @rdname loglikelihood
//' @export
// [[Rcpp::export]]
double loglikPLMIX(NumericMatrix p, NumericMatrix ref_order, NumericVector weights, NumericMatrix pi_inv) {

int N = pi_inv.nrow();
int K = pi_inv.ncol();
int G = p.nrow();

int    s ;
int    j ;

int    slot ;
int    slot3 ;

int    tt ;

NumericVector f(G);
double g  ;
double logh  ;
double m  ;
double ll ;

ll = 0.0 ;


for(s=0; s<N; s++){

for(j=0; j<G; j++){

f[j] = 0.0 ;

slot  = 0 ;
tt    = 1 ; 


/* FIRST compute the INITIAL denominator g */
/* i.e. sum of all support parameters  */

g = 0.0;

for( slot3=0; slot3<K; slot3++){
g = g + p(j,slot3);
}

while(tt>-1 && slot<K){       

tt = pi_inv(s,slot)-1 ;
f[j] = f[j] + (log(p(j,tt)) - log(g)) ;

/* UPDATE the denominator g removing the support for the current item */

g = g - p(j,tt);

if(g<0){
/* printf(" SOMETHING IS WRONG WITH THE DENOMINATOR !!! \\n"); */
}

slot = slot+1 ; 
          if(slot<K){
              tt = pi_inv(s,slot)-1 ;
            }
}
    
    f[j] = f[j] + log(weights[j]) ;

 
}
    
    f = dbl_sort(f) ;

    logh = f[0] ;
    m = 1.0 ;

    if(G>1){ ///'to avoid overflow of too small negative log-probability values in some components of the mixture,
        ///we refer to the maximum (negative) log-prob as the starting summand contributor in the log-scale.
        ///The other summands, being smaller, than the reference one, can be safely expontiated because their contribution
        ///to the sum would add something or nothing according to whether the difference (w.r.t. the reference) is relatively small or large.
        ///Idea: if G=4 and c1>c2>c3>c4, then log(exp(c1)+exp(c2)+exp(c3)+exp(c4))=c1+log(1+exp(c2-c1)+exp(c3-c1)+exp(c4-c1))
        
        for(j=1; j<G; j++){
            
            m = m + exp(f[j]-f[0]) ;
            
        }
                
    }
    
logh = logh + log(m) ;

ll = ll + logh ;

}

return ll;

}
