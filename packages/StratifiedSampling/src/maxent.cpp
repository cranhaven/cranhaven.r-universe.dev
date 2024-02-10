#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]



//' @title q from w
//' @name qfromw
//' @description This function finds the matrix \code{q} form a particular \code{w}.
//'  
//' @param w A vector of weights.
//' @param n An integer that is equal to the sum of the inclusion probabilities.
//' 
//' @details
//' 
//' \code{w} is generally computed by the formula \code{pik/(1-pik)}, where \code{n} is equal to the sum of the vector \code{pik}.
//' More details could be find in Tille (2006).
//' 
//' @return A matrix of size \code{N} x \code{n}, where \code{N} is equal to the length of the vector \code{w}.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @references 
//' Tille, Y. (2006), Sampling Algorithms, springer
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix qfromw(NumericVector& w,const int& n){

  
  
  
  
  // transform to arma for subvec 
  arma::vec wr(w.begin(),w.size(),false); // same memory
  
  int N = wr.size();
  arma::mat expa(N,n,arma::fill::zeros);
  for(int i = 0;i < N;i++){
    expa(i,0) = arma::sum(wr.subvec(i,N-1));
  }
  for(int i = 1;i < n; i++){
    expa(N-i-1,i) = exp(arma::sum(log(wr.subvec(N-i-1,N-1))));
  }
  for(int i = N-3; i >= 0; i--){
    for(int z = 1; z < std::min(N-i,n); z++){
      expa(i,z) = wr[i]*expa(i+1,z-1) + expa(i+1,z);
    }
  }
  
  NumericMatrix q(N,n);
  long double num(0.0);
  long double den(0.0);
  long double ratio(0.0);
  for(int i = 0;i < N;i++){
    q(i,0) = wr[i]/expa(i,0);
  }
  for(int i = 1;i < n; i++){
    q(N-i-1,i) = 1;
  }
  for(int i = N-3; i >= 0; i--){
    for(int z = 1; z < std::min(N-i-1,n); z++){
      /*
       * Here we might have a stability problem
       * num/den might be value very very close to 0
       * which ends with NaN value,
       * 
       * We round the ratio of the den and num are bother almost 0
       */
      num = expa(i+1,z-1);
      den = expa(i,z);
      ratio = num/den;
      if(std::isnan(ratio)){
        ratio = 1;
      }
      // if((num < 1e-16) && (den < 1e-16)){
      //   ratio = 1;
      // }else{
      //   ratio = num/den;
      // }
      // Rcout << wr[i] << std::endl;
      // Rcout << num << std::endl;
      // Rcout << den << std::endl;
      // Rcout << wr[i]*ratio << std::endl<< std::endl;
      
      q(i,z) = wr[i]*ratio;
      // Rcout << expa(i+1,z-1) << std::endl;
      // Rcout << expa(i,z) << std::endl; 
      // q(i,z) = w[i]*expa(i+1,z-1)/expa(i,z);
    }
  }
  return(q);
}

/*** R

set.seed(9)

pik <-inclprob(runif(8000),500)

w <- pik/(1-pik)


q <- qfromw(w,6)
any(is.na(q))
any(is.na(pikfromq(q))) # NA true

pik - pikfromq(q)

pikt <- piktfrompik(pik)
sum(pikt)
*/







//' @title s from q
//' @name sfromq
//' @description This function finds sample \code{s} form the matrix \code{q}.
//'  
//' @param q A matrix that is computed from the function \code{\link{qfromw}}. 
//' 
//' @details
//' 
//' More details could be find in Tille (2006).
//' 
//' @return A vector with elements equal to 0 or 1. The value 1 indicates that the unit is selected while the value 0 is for rejected units.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @references 
//' Tille, Y. (2006), Sampling Algorithms, springer
//' 
//' @export
// [[Rcpp::export]]
IntegerVector sfromq(const NumericMatrix& q){
  int N = q.nrow();
  int n = q.ncol();
  IntegerVector s(N);
 
  for(int k = 0;k < N;k++){
    if(n != 0){
      if(runif(1)[0] < q(k,n-1)){
        s[k] = 1;
        n = n-1;
      }
    }
  }
  return(s);
}







//' @title pik from q
//' @name pikfromq
//' @description This function finds the \code{pik} from an initial \code{q}.
//'  
//' @param q A matrix that is computed from the function \code{\link{qfromw}}. 
//' 
//' @details
//' 
//' More details could be find in Tille (2006).
//' 
//' @return A vector of inclusion probability computed from the matrix \code{q}.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @references 
//' Tille, Y. (2006), Sampling Algorithms, springer
//' 
//' @export
// [[Rcpp::export]]
NumericVector pikfromq(NumericMatrix& q){
  int N = q.nrow();
  int n = q.ncol();
  
  arma::mat qr(q.begin(),N,n,false); // same memory 
  arma::mat pro(N,n,arma::fill::zeros);
  
  pro(0,n-1) = 1.0;
  for(int i = 1;i< N;i++){
    for(int j = 1;j< n;j++){
      pro(i,j) += pro(i-1,j)*(1-qr(i-1,j));
      pro(i,j-1) +=  pro(i-1,j)*(qr(i-1,j));
    }
  }
  for(int i = 1;i < N; i++){
    pro(i,0) += pro(i-1,0)*(1-qr(i-1,0));
  }
  arma::vec out = arma::sum(pro%qr,1);
  
  // out of NumericVector and not arma
  NumericVector out2(N);
  for(int i = 0; i < N;i++){
    out2[i] = out[i];
  }
  return(out2);
}




//' @title pikt from pik
//' @name piktfrompik
//' @description This function finds the \code{pikt} from an initial \code{pik}.
//'  
//' @param pik A vector of inclusion probabilities. The vector must contains only value that are not integer.
//' @param max_iter An integer that specify the maximum iteration in the Newton-Raphson algorithm. Default \code{500}.
//' @param tol A scalar that specify the tolerance convergence for the Newton-Raphson algorithm. Default \code{1e-8}.
//' 
//' @details
//' 
//' The management of probabilities equal to 0 or 1 is done in the cps function.
//' 
//' \code{pikt} is the vector of inclusion probabilities of a Poisson sampling with the right parameter. The vector is found by Newtwon-Raphson algorithm.
//' 
//' More details could be find in Tille (2006).
//' 
//' @return An updated vector of inclusion probability.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @references 
//' Tille, Y. (2006), Sampling Algorithms, springer
//' 
//' @export
// [[Rcpp::export]]
NumericVector piktfrompik(NumericVector& pik, int max_iter = 500,double tol = 1e-8){

 int N = pik.size();
 int n = round(sum(pik));
 NumericVector pikt(Rcpp::clone(pik)); // get pik that will be update into piktilde
 NumericVector p(Rcpp::clone(pik)); // get wo modif 
 
 /*
  * RANDOMIZATION NOT ANYMORE NEEDED AS WE FIX qfromw
  */
 
 // IntegerVector index(N); // index 
 // for(int k=0; k < N;k++){
 //   index[k]=k;
 // }
 // 
 // // randomize order of index list
 // int tempInt = 0;
 // int j = 0;
 // NumericVector rnd = runif(N);
 // for(int k = 0;k < N;k++){
 //   j = k + floor(rnd[k] * (N-k));
 //   tempInt = index[k];
 //   index[k] = index[j];
 //   index[j] = tempInt;
 // }
 // 
 // // apply the random order on pik and p
 // pikt = pikt[index];
 // p = p[index];
 


 // temporary vector and matrix
 NumericVector w(N);
 NumericMatrix q(N,n);
 NumericVector pikt1(N);

 /*
  * Main loop, we compute q for a particular pik,
  * then we recalculate pik from q
  * and we use Newton raphson to pikt.
  * 
  * We use qfromw because it is more stable than try to estimate directly the 
  * pikt or the w by using the recursive formula.
  * 
  */
 int count = 1;
 double arr = 1.0;
 while((arr > tol) && (count < max_iter)){
   w = pikt/(1.0 - pikt);
  // w = log(pikt/(1.0 - pikt));
  // w = w - mean(w);
  // w = exp(w);
  q = qfromw(w,n);
  pikt1 = pikt + p - pikfromq(q);
  arr = sum(Rcpp::abs(pikt - pikt1));
  pikt = pikt1;
  count++;
 }
 
 
 
 NumericVector out(N);
 for(int j = 0; j < N;j++){
   // out[index[j]] = pikt[j]; // RANDOMIZATION
   out[j] = pikt[j];
 }
 
 return(out);

}


//' @title Conditional Poisson sampling design
//' @name cps
//' @description Maximum entropy sampling with fixed sample size. It select a sample with fixed sample size with unequal inclusion probabilities.
//'  
//' @param pik A vector of inclusion probabilities. 
//' @param eps A scalar that specify the tolerance to transform a small value to the value 0.
//' 
//' @details
//' Conditional Poisson sampling, the sampling design maximizes the entropy:
//' \deqn{I(p) = - \sum s p(s) log[p(s)].}
//' where s is of fixed sample size. Indeed, Poisson sampling is known for maximizing the entropy but has no fixed sample size. The function selects a sample of fixed sample that maximizes entropy.
//' 
//' This function is a C++ implementation of \code{\link[sampling:UPmaxentropy]{UPmaxentropy}} of the package \code{sampling}. More details could be find in Tille (2006).
//' 
//' @return A vector with elements equal to 0 or 1. The value 1 indicates that the unit is selected while the value 0 is for rejected units.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @references 
//' Tille, Y. (2006), Sampling Algorithms, springer
//' 
//' @examples
//' 
//' pik <- inclprob(seq(100,1,length.out = 100),10)
//' s <-  cps(pik)
//' 
//' 
//' 
//' # simulation with piktfrompik MUCH MORE FASTER
//' s <- rep(0,length(pik))
//' SIM <- 100
//' pikt <- piktfrompik(pik)
//' w <- pikt/(1-pikt)
//' q <- qfromw(w,sum(pik))
//' for(i in 1 :SIM){
//'   s <- s + sfromq(q)
//' }
//' p <- s/SIM # estimated inclusion probabilities
//' t <- (p-pik)/sqrt(pik*(1-pik)/SIM)
//' 1 - sum(t > 1.6449)/length(pik) # should be approximately equal to 0.95 
//' 
//' @export
// [[Rcpp::export]]
IntegerVector cps(NumericVector& pik,
                     double eps = 1e-6){

  int N = pik.size(); // full size
  arma::vec pik_tmp(pik.begin(),pik.size(),false); // transform to arma to use find (same memory allocation)
  arma::uvec i = arma::find(pik_tmp < 1-eps && pik_tmp > eps);// find not equal to 0 or 1 value
  int N_tmp = i.size(); // new size
  IntegerVector index(i.begin(),i.end()); // transform to IntegerVector
  NumericVector pikout = pik[index]; // get pik in NumericVector
  

  /*
   * All step computation
   * 1. find pikt with Newtown-Raphson (randomization to avoid problem with consecutive small inclusion probabilities) see piktfrompik
   * 2. calculate w
   * 3. calculate q
   * 4. find s from q
   */
  int n = round(sum(pikout));
  NumericVector pikt = piktfrompik(pikout); 
  NumericVector w = pikt/(1-pikt);
  NumericMatrix q = qfromw(w,n);
  IntegerVector s2 = sfromq(q);
  
  
  //put right 0-1 value at the right place
  IntegerVector s(N);
  s.fill(1);
  for(int j = 0; j < N_tmp;j++){
    s[i[j]] = s2[j];
  }
  
  return(s);
}


/*** R


pik <- sampling::inclusionprobabilities(runif(8000),500)

s <- rep(0,length(pik))
SIM <- 1000
pikt <- piktfrompik(pik)
w <- pikt/(1-pikt)
q <- qfromw(w,sum(pik))
for(i in 1 :SIM){
  print(i)
  tmp <- sfromq(q)
  s <- s + tmp
}

p <- s/SIM
pik 

1 -length(s[which(abs((p-pik)/sqrt(pik*(1-pik)/SIM)) > 1.96)]/SIM)/length(pik)







## EXAMPLE

pik <- inclprob(seq(100,1,length.out = 100),10)
s <-  cps(pik)
# simulation with piktfrompik MUCH MORE FASTER
s <- rep(0,length(pik))
SIM <- 100
pikt <- piktfrompik(pik)
w <- pikt/(1-pikt)
q <- qfromw(w,sum(pik))
for(i in 1 :SIM){
  s <- s + sfromq(q)
}
p <- s/SIM # estimated inclusion probabilities
t <- (p-pik)/sqrt(pik*(1-pik)/SIM)
1 - sum(t > 1.664)/length(pik) # should be approximately equal to 0.95 


pik <- inclprob(seq(100,1,length.out = 100),10)
s <-  maxent(pik)


# simulation with piktfrompik MUCH MORE FASTER
s <- rep(0,length(pik))
SIM <- 1000
pikt <- piktfrompik(pik)
w <- pikt/(1-pikt)
q <- qfromw(w,sum(pik))
for(i in 1 :SIM){
  s <- s + sfromq(q)
}
p <- s/SIM # estimated inclusion probabilities
t <- (p-pik)/sqrt(pik*(1-pik)/SIM)
1 - length(s[t > 1.644]/SIM)/length(pik) # should be approximatively equal to 0.95 

# system.time(UPmaxentropy(pik))
# system.time(cps(pik))

## swissmunicipalitites were not working

library(sampling)
data("swissmunicipalities")
eps <- 1e-7 # epsilon tolerance
n <- 200 # sample size
pik <- inclusionprobabilities(swissmunicipalities$POPTOT,n)
cps(pik)
mask <- (pik < (1 - eps)) & (pik > eps)
pik <-  pik[mask]

pikt <- piktfrompik(pik) # that were not working
pik

# simuation with maxent 
s <- rep(0,length(pik))
SIM <- 1000
for(i in 1 :SIM){
  print(i)
  tmp <- cps(pik)
  if(any(abs(sum(tmp) - sum(pik)) > eps)){
    cat("error")
    break;
  }
  s <- s + tmp
}

# simulation with piktfrompik SO FASTER
s <- rep(0,length(pik))
SIM <- 1000
pikt <- piktfrompik(pik)
w <- pikt/(1-pikt)
q <- qfromw(w,sum(pik))
for(i in 1 :SIM){
  print(i)
  tmp <- sfromq(q)
  if(any(abs(sum(tmp) - sum(pik)) > eps)){
    cat("error")
    break;
  }
  s <- s + tmp
}


p <- s/SIM
pik 

1 -length(s[which((p-pik)/sqrt(pik*(1-pik)/SIM) > 2)]/SIM)/length(pik)


pik[which((p-pik)/sqrt(pik*(1-pik)/SIM) > 2)]

s/SIM
pik


###### GENERAL example bigger

pik <- sampling::inclusionprobabilities(runif(8000),500)

s <- rep(0,length(pik))
SIM <- 1000
pikt <- piktfrompik(pik)
w <- pikt/(1-pikt)
q <- qfromw(w,sum(pik))
for(i in 1 :SIM){
  print(i)
  tmp <- sfromq(q)
  s <- s + tmp
}

p <- s/SIM
pik 

1 -length(s[which((p-pik)/sqrt(pik*(1-pik)/SIM) > 1.64)]/SIM)/length(pik)




*/



/*
 * 
 * 
 * 
 * 
 * 
 * INLCUSION PROBABILITIES OF SECOND ORDER
 * 
 * 
 * 
 * 
 * 
 */



NumericMatrix pik2frompik(NumericVector pikr, NumericVector wr){
  
  
  double eps = 1e-6;
  int N = pikr.size();
  arma::vec pik(pikr.begin(),pikr.size(),false);
  arma::vec w(wr.begin(),wr.size(),false);
  int n = arma::sum(pik);
  
  NumericMatrix M(N,N);
  
  
  for(int k = 0; k < N; k++){
    for(int l = 0; l < N; l++){
      if( (std::abs(pik[k] - pik[l]) > eps) & (k != l)){
      // if(pik[k] != pik[l] & k != l){
        M(k,l) = (pik[k]* w[l] - pik[l]*w[k])/(w[l] - w[k]);
      }else{
        M(k,l) = -1.0;
      }
    }
  }
  
  for(int i = 0; i < N; i++){
    M(i,i) = pik[i];
  }
  
  
  double tt = 0.0;
  int comp = 0;
  double cc = 0.0;
  for(int k = 0; k < N; k++){
    tt = 0.0;
    comp = 0;
    for(int l = 0; l < N; l++){
      // if(M(k,l) != -1.0){
      if(std::abs(M(k,l) + 1.0) > eps){  
        tt = tt + M(k,l);  
      }else{
        comp = comp + 1.0;
      }
    }
    cc = (n * pik[k] - tt)/comp;
    for(int l = 0; l < N; l++){
      if(std::abs(M(k,l) + 1) < eps){
      // if(M(k,l) == -1.0){
        M(k,l) = cc;
      }
    }  
  }
  
  return(M);
  
  
}




//' @title Joint inclusion probabilities of maximum entropy.
//' @name maxentpi2
//' @description This function computes the matrix of the joint inclusion of the maximum entropy sampling with fixed sample size. It can handle unequal inclusion probabilities.
//' 
//' @param pikr A vector of inclusion probabilities.
//' 
//' @details
//' The sampling design maximizes the entropy design:
//' \deqn{I(p) = - \sum s p(s) log[p(s)].}
//' 
//' This function is a C++ implementation of \code{\link[sampling:UPMEpik2frompikw]{UPMEpik2frompikw}}.
//' More details could be find in Tille (2006).
//' @return A matrix, the joint inclusion probabilities.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @references 
//' Tille, Y. (2006), Sampling Algorithms, springer
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix maxentpi2(NumericVector pikr){
  
  
  double eps = 1e-6;
  int N = pikr.size();
  arma::vec pik_tmp(pikr.begin(),pikr.size(),false);
  
  // find not equl to 0 value
  arma::uvec i = arma::find(pik_tmp > eps && pik_tmp < 1-eps);
  arma::uvec i1 = arma::find(pik_tmp > 1-eps);
  
  // arma to Numeric vector 
  arma::vec pik_tmp2 = pik_tmp.elem(i);
  int N_tmp = pik_tmp2.size();
  NumericVector pik(N_tmp);
  for(int j = 0;j< N_tmp;j++){
    pik[j] = pik_tmp2[j];
  }
  
  NumericMatrix M2(N_tmp,N_tmp);
  NumericVector pikt = piktfrompik(pik);
  NumericVector w =  pikt/(1 - pikt);
  M2 = pik2frompik(pik,w);
  
  
  //put right value at the right place
  NumericMatrix M(N,N);
  
  int ni = i.size();
  int ni1 = i1.size();
  
  for(int j = 0; j < ni;j++){
    for(int ii = 0;ii < ni;ii++){
      M(i(ii),i(j)) = M2(ii,j);
    }
    // M(_,i(j)) = M2(_,j);
  }
  for(int j = 0; j < ni1; j++){
    M(_,i1(j)) = pikr;
    M(i1(j),_) = pikr;
  }
  
  return(M);
}


/*** R

pik=c(0.07,0.17,0.41,0.61,0.83,0.91)

n=sum(pik)
pikt=UPMEpiktildefrompik(pik)
w=pikt/(1-pikt)
q=UPMEqfromw(w,n)
UPMEsfromq(q)

pik2frompik(pik,w)
UPMEpik2frompikw(pik,w)


pik=c(0.07,0.17,0.41,0.61,1,1,0.83,0.91)


UPmaxentropypi2(pik)
maxentpi2(pik)



pik <- inclusionprobabilities(runif(1000),100)

system.time(M1 <- round(UPmaxentropypi2(pik),4))
system.time(M2 <- round(maxentpi2(pik),4))


sum(abs(M1-M2))




*/



/*** R
library(sampling)
# pik=c(0.07,0.17,0.41,0.61,0.83,0.91)
# 
# k = 1


# w = (pik)/(1 - pik)
# qfromw(w,sum(pik))
# maxent::UPMEqfromw(w,sum(pik))
# 
# q <- qfromw(w,sum(pik))
# 
# pik2 = pik[pik != 1]
# n = sum(pik2)
# # 
# pik2
# maxent::UPMEpiktildefrompik(pik2)
# # maxent::UPMEqfromw(piktilde/(1 - piktilde),n)
# 
# pikfromq(q)
# maxent::UPMEpikfromq(q)
# 
# piktfrompik(pik2)
# maxent::UPMEpiktildefrompik(pik2)
# sum(piktfrompik(pik2))
# sum(maxent::UPMEpiktildefrompik(pik2))

index <- c()
piktmp <- pik
while(length(index) != length(pik)){
  piktmp[index] <- 1e100
  m <- which.min(piktmp)
  piktmp[index] <- 1e-100
  M <- which.max(piktmp)
  index <- c(index,m,M)
  piktmp <- pik
}


system.time(s2 <- sampling::UPmaxentropy(pik))


system.time(s <- maxent(pik))
piktfrompik(pik)

w <- pik/(1-pik)
any(is.na(qfromw(w,sum(pik))))


sum(s)
sum(s2)
k = k+1
.# 
# sfromq(q)
# UPMEsfromq(q)
# UPMEqfromw(w, sum(pik))




UPMEpikfromq(q)
pikfromq(q)
sum(UPMEpiktildefrompik(pik))

sum(piktfrompik(pik))


N=1000
n=200
pik=sampling::inclusionprobabilities(runif(N),n)
SIM=1000
PPP=rep(0,N)
for(i in 1:SIM){
  print(i)
  s <- maxent(pik)
  PPP=PPP + s
  print(sum(s))
}
PPP=PPP/SIM

t=(PPP-pik)/sqrt(pik*(1-pik)/SIM)
sum(abs(t)<1.96)/N




*/
