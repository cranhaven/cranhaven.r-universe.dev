// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
using namespace Rcpp;
#include <cmath>
#include <cfloat>
#include <boost/math/special_functions/owens_t.hpp>
#include <boost/math/constants/constants.hpp>

const double sqrt2pi = boost::math::constants::root_two_pi<double>();
const double onedivsqrt2pi =
             boost::math::constants::one_div_root_two_pi<double>();
const double logsqrt2pi = boost::math::constants::log_root_two_pi<double>();
const double logtwo = 0.693147180559945309417232121458176568;

NumericVector xdnormx(NumericVector x){
  return exp(log(x) - 0.5*x*x - logsqrt2pi);
}


// [[Rcpp::export]]
double RcppOwenT(double h, double a){
  return boost::math::owens_t(h, a);
}
double RcppOwenT(double h, double a);


//****************************************************************************80
NumericVector OwenStudent_C(double q, int nu, NumericVector delta){
  const double nudbl = nu;
  const double a = R::sign(q)*sqrt(q*q/nudbl);
  NumericVector dsB = delta*sqrt(nudbl/(nudbl+q*q));
  const int J = delta.size();
  NumericVector C = pnorm(-dsB);
  for(int i=0; i<J; i++){
    C[i] += 2*RcppOwenT(dsB[i], a);
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenStudent(double q, int nu, NumericVector delta){
  if(nu==1){
    return OwenStudent_C(q, nu, delta);
  }
  const double nudbl = nu;
  const double a = R::sign(q)*sqrt(q*q/nudbl);
  const double b = nudbl/(nudbl+q*q);
  NumericVector dsB = delta*sqrt(b);
  const int J = delta.size();
  NumericMatrix M(nu-1,J);
  M(0,_) = a * sqrt(b) * dnorm(dsB) * pnorm(a*dsB);
  if(nu>2){
    M(1,_) = b * (delta * a * M(0,_) + a * dnorm(delta) * onedivsqrt2pi);
    if(nu>3){
      NumericVector A(nu-3); A[0] = 1;
      int k;
      if(nu>4){
        for(k=1; k<nu-3; k++){
          A[k] = 1.0/k/A[k-1];
        }
      }
      for(k=2; k<nu-1; k++){
        M(k,_) = (k-1) * b * (A[k-2] * delta * a * M(k-1,_) + M(k-2L,_)) / k;
      }
    }
  }
  NumericVector sum(J);
  if(nu%2==1){
    for(int i=1; i<nu-1; i+=2){
      sum += M(i,_);
    }
    return OwenStudent_C(q, nu, delta) + 2*sum;
  }else{
    for(int i=0; i<nu-1; i+=2){
      sum += M(i,_);
    }
    return pnorm(-delta) + sqrt2pi*sum;
  }
}

//****************************************************************************80
NumericVector isPositive(NumericVector x){
  int n = x.size();
  NumericVector out(n);
  int i;
  for(i=0; i<n; i++){
    out[i] = x[i] >= 0;
  }
  return out;
}
NumericVector isPositive(NumericVector x);

//****************************************************************************80
NumericVector OwenQ1_C
    (int nu, double t, NumericVector delta, NumericVector R){
  const double nudbl = nu;
  const double a = R::sign(t)*sqrt(t*t/nudbl);
  const double b = nudbl/(nudbl+t*t);
  const double sB = sqrt(b);
  const double ab = sqrt(nudbl)/(nudbl/t + t);
  const int J = delta.size();
  NumericVector C = pnorm(R) - isPositive(delta);
  for(int i=0; i<J; i++){
    double C1 = RcppOwenT(delta[i]*sB, a);
    double C2 = RcppOwenT(R[i], a-delta[i]/R[i]);
    double C3 = RcppOwenT(delta[i]*sB, (ab-R[i]/delta[i])/b);
    C[i] += 2*(C1 - C2 - C3);
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenQ1
    (int nu, double t, NumericVector delta, NumericVector R, int algo=1){
  if(nu==1){
    return OwenQ1_C(nu, t, delta, R);
  }
  const double nudbl = nu;
  const double a = R::sign(t)*sqrt(t*t/nudbl);
  const double b = nudbl/(nudbl+t*t);
  const double sB = sqrt(b);
  const double ab = sqrt(nudbl)/(nudbl/t + t);
  const double asB = R::sign(t)/sqrt(nudbl/(t*t)+1);
  const int J = delta.size();
  const int n = nu-1;
  NumericMatrix H(n,J); NumericMatrix M(n,J);
  NumericVector Lfactor = ab * dnorm(a*R-delta);
  H(0,_) = dnorm(R);
  M(0,_) = asB*dnorm(delta*sB)*(pnorm(delta*asB)-pnorm((delta*ab-R)/sB));
  if(nu >= 3){
    H(1,_) = xdnormx(R);
    M(1,_) = delta*ab*M(0,_) +
                ab*dnorm(delta*sB)*(dnorm(delta*asB)-dnorm((delta*ab-R)/sB));
    if(nu >= 4){
      if(algo==1){
        NumericVector A(n); A[0] = 1.0; A[1] = 1.0;
        NumericVector L = H(0,_);
        for(int k=2; k<n; k++){
            A[k] = 1.0 / (k*A[k-1]);
            NumericVector AkRj = A[k] * R;
            L = AkRj * L;
            H(k,_) = AkRj * H(k-1,_);
            M(k,_) = (k-1.0)/k * (A[k-2]*delta*ab*M(k-1,_) + b*M(k-2,_)) -
                        Lfactor*L;
        }
      }else{ // algo 2
        NumericVector W = -(0.5 * R*R + logsqrt2pi);
        NumericVector logR = log(R);
        double A = 1.0;
        double u = 0.0; double v = 0.0; double ldf;
        for(int k=0; k<n-2; k++){
          if(k % 2 == 0){
            u += log(k+2.0); ldf = u;
          }else{
            v += log(k+2.0); ldf = v;
          }
          double r = (k+1.0)/(k+2.0);
          NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
          H(k+2,_) = K*R;
          M(k+2,_) = r*(A*delta*ab*M(k+1,_)+b*M(k,_)) - K*Lfactor;
          A = 1.0 / ((k+1)*A);
        }
      }
    }
  }
  NumericVector sumM(J); NumericVector sumH(J);
  if(nu % 2 == 0){
    for(int i=0; i<nu-1; i+=2){
      sumM += M(i,_); sumH += H(i,_);
    }
    return pnorm(-delta) + sqrt2pi * (sumM - pnorm(a*R-delta)*sumH);
  }else{
    for(int i=1; i<nu-1; i+=2){
      sumM += M(i,_); sumH += H(i,_);
    }
    return OwenQ1_C(nu, t, delta, R) + 2.0*(sumM - pnorm(a*R-delta)*sumH);
  }
}

NumericVector dnormtimes10pown(NumericVector x, int n){
  return exp(n*2.302585092994045684017991454684364207 - 0.5*x*x - logsqrt2pi);
}


//****************************************************************************80
NumericVector OwenQ2_C
    (int nu, double t, NumericVector delta, NumericVector R){
  const double nudbl = nu;
  const double a = R::sign(t)*sqrt(t*t/nudbl);
  const double b = nudbl/(nudbl+t*t);
  const double sB = sqrt(b);
  const double ab = a*b;
  const int J = delta.size();
  NumericVector C = pnorm(-delta*sB) - pnorm(R) + isPositive(delta);
  for(int i=0; i<J; i++){
    double C2 = RcppOwenT(R[i], (a*R[i]-delta[i])/R[i]);
    double C3 = RcppOwenT(delta[i]*sB, (delta[i]*ab-R[i])/b/delta[i]);
    C[i] += 2.0*(C2 + C3);
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenQ2
    (int nu, double t, NumericVector delta, NumericVector R, int algo=1){
  if(nu==1){
    return OwenQ2_C(nu, t, delta, R);
  }
  const double nudbl = nu;
  const double a = R::sign(t)*sqrt(t*t/nudbl);
  const double b = nudbl/(nudbl+t*t);
  const double sB = sqrt(b);
  const double ab = a*b;
  const double asB = R::sign(t)*sqrt(t*t/(nudbl+t*t));
  const int J = delta.size();
  const int n = nu-1;
  NumericMatrix H(n,J); NumericMatrix M(n,J);
  NumericVector Lfactor = ab * dnorm(a*R-delta);
  H(0,_) = dnorm(R);
  M(0,_) = asB*dnorm(delta*sB)*pnorm((delta*ab-R)/sB);
  if(nu >= 3){
    H(1,_) = xdnormx(R);
    M(1,_) = delta*ab*M(0,_) + ab*dnorm(delta*sB)*dnorm((delta*ab-R)/sB);
    if(nu >= 4){
      if(algo==1){
        NumericVector A(n); A[0] = 1.0; A[1] = 1.0;
        NumericVector L = H(0,_);
        for(int k=2; k<n; k++){
            A[k] = 1.0 / (k*A[k-1]);
            NumericVector AkRj = A[k] * R;
            L = AkRj * L;
            H(k,_) = AkRj * H(k-1,_);
            M(k,_) = (k-1.0)/k * (A[k-2]*delta*ab*M(k-1,_) + b*M(k-2,_)) +
                        Lfactor*L;
        }
      }else{ // algo 2
        NumericVector W = -(0.5 * R*R + logsqrt2pi);
        NumericVector logR = log(R);
        double A = 1.0;
        double u = 0.0; double v = 0.0; double ldf;
        for(int k=0; k<n-2; k++){
          if(k % 2 == 0){
            u += log(k+2.0); ldf = u;
          }else{
            v += log(k+2.0); ldf = v;
          }
          double r = (k+1.0)/(k+2.0);
          NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
          H(k+2,_) = K*R;
          M(k+2,_) = r*(A*delta*ab*M(k+1,_)+b*M(k,_)) + K*Lfactor;
          A = 1.0 / ((k+1)*A);
        }
      }
    }
  }
  NumericVector sumM(J); NumericVector sumH(J);
  if(nu % 2 == 0){
    for(int i=0; i<nu-1; i+=2){
      sumM += M(i,_); sumH += H(i,_);
    }
    return sqrt2pi * (sumM + pnorm(a*R-delta)*sumH);
  }else{
    for(int i=1; i<nu-1; i+=2){
      sumM += M(i,_); sumH += H(i,_);
    }
    return OwenQ2_C(nu, t, delta, R) + 2*(sumM + pnorm(a*R-delta)*sumH);
  }
}

//****************************************************************************80
NumericVector OwenCDF4_C(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2){
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const int J = delta1.size();
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  NumericVector C = isPositive(delta1) - isPositive(delta2);
  for(int i=0; i<J; i++){
    double C1 = RcppOwenT(delta2[i]*sB2, a2) - RcppOwenT(delta1[i]*sB1, a1);
    double C2 = RcppOwenT(R[i], (a2*R[i]-delta2[i])/R[i]) -
                  RcppOwenT(R[i], (a1*R[i]-delta1[i])/R[i]);
    double C3 =
      RcppOwenT(delta2[i]*sB2, (delta2[i]*ab2-R[i])/b2/delta2[i]) -
        RcppOwenT(delta1[i]*sB1, (delta1[i]*ab1-R[i])/b1/delta1[i]);
    C[i] += 2.0*(C1 - C2 - C3);
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenCDF4(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2, int algo=1){
  if(nu==1){
    return OwenCDF4_C(nu, t1, t2, delta1, delta2);
  }
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double asB1 = R::sign(t1)*sqrt(t1*t1/(nudbl+t1*t1));
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const double asB2 = R::sign(t2)*sqrt(t2*t2/(nudbl+t2*t2));
  const int J = delta1.size();
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  const int n = nu-1;
  NumericMatrix H(n,J);
  NumericMatrix M1(n,J); NumericMatrix M2(n,J);
  NumericVector Lfactor1 = ab1 * dnorm(a1*R-delta1);
  NumericVector Lfactor2 = ab2 * dnorm(a2*R-delta2);
  H(0,_) = dnorm(R);
  M1(0,_) = asB1*dnorm(delta1*sB1) *
                (pnorm(delta1*asB1)-pnorm((delta1*ab1-R)/sB1));
  M2(0,_) = asB2*dnorm(delta2*sB2) *
                (pnorm(delta2*asB2)-pnorm((delta2*ab2-R)/sB2));
  if(nu >= 3){
    H(1,_) = xdnormx(R);
    M1(1,_) = delta1*ab1*M1(0,_) +
                ab1*dnorm(delta1*sB1) *
                    (dnorm(delta1*asB1)-dnorm((delta1*ab1-R)/sB1));
    M2(1,_) = delta2*ab2*M2(0,_) +
                ab2*dnorm(delta2*sB2) *
                    (dnorm(delta2*asB2)-dnorm((delta2*ab2-R)/sB2));
    if(nu >= 4){
      if(algo==1){
        NumericVector A(n); A[0] = 1.0; A[1] = 1.0;
        NumericVector L = H(0,_);
        for(int k=2; k<n; k++){
            A[k] = 1.0 / (k*A[k-1]);
            NumericVector AkRj = A[k] * R;
            L = AkRj * L;
            H(k,_) = AkRj * H(k-1,_);
            M1(k,_) = (k-1.0)/k * (A[k-2]*delta1*ab1*M1(k-1,_) + b1*M1(k-2,_)) -
                        Lfactor1*L;
            M2(k,_) = (k-1.0)/k * (A[k-2]*delta2*ab2*M2(k-1,_) + b2*M2(k-2,_)) -
                        Lfactor2*L;
        }
      }else{ // algo 2
        NumericVector W = -(0.5 * R*R + logsqrt2pi);
        NumericVector logR = log(R);
        double A = 1.0;
        double u = 0.0; double v = 0.0; double ldf;
        for(int k=0; k<n-2; k++){
          if(k % 2 == 0){
            u += log(k+2.0); ldf = u;
          }else{
            v += log(k+2.0); ldf = v;
          }
          double r = (k+1.0)/(k+2.0);
          NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
          H(k+2,_) = K*R;
          M1(k+2,_) = r*(A*delta1*ab1*M1(k+1,_)+b1*M1(k,_)) - K*Lfactor1;
          M2(k+2,_) = r*(A*delta2*ab2*M2(k+1,_)+b2*M2(k,_)) - K*Lfactor2;
          A = 1.0 / ((k+1)*A);
        }
      }
    }
  }
  NumericVector sumM(J); NumericVector sumH(J);
  if(nu % 2 == 0){
    for(int i=0; i<nu-1; i+=2){
      sumM += M2(i,_)-M1(i,_); sumH += H(i,_);
    }
    return pnorm(-delta2) - pnorm(-delta1) +
            sqrt2pi*(sumM + (pnorm(a1*R-delta1) - pnorm(a2*R-delta2))*sumH);
  }else{
    for(int i=1; i<nu-1; i+=2){
      sumM += M2(i,_)-M1(i,_); sumH += H(i,_);
    }
    return OwenCDF4_C(nu, t1, t2, delta1, delta2) +
            2.0*(sumM + (pnorm(a1*R-delta1) - pnorm(a2*R-delta2))*sumH);
  }
}

//****************************************************************************80
NumericVector OwenCDF3_C(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2){
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const int J = delta1.size();
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  NumericVector C = - isPositive(delta1) + isPositive(delta2) -
                        pnorm(-delta1*sB1);
  for(int i=0; i<J; i++){
    double C1 = - RcppOwenT(delta2[i]*sB2, a2);
    double C2 = RcppOwenT(R[i], (a2*R[i]-delta2[i])/R[i]) -
                  RcppOwenT(R[i], (a1*R[i]-delta1[i])/R[i]);
    double C3 =
      RcppOwenT(delta2[i]*sB2, (delta2[i]*ab2-R[i])/b2/delta2[i]) -
        RcppOwenT(delta1[i]*sB1, (delta1[i]*ab1-R[i])/b1/delta1[i]);
    C[i] += 2.0*(C1 + C2 + C3) + 1.0;
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenCDF3(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2, int algo=1){
  if(nu==1){
    return OwenCDF3_C(nu, t1, t2, delta1, delta2);
  }
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double asB1 = R::sign(t1)*sqrt(t1*t1/(nudbl+t1*t1));
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const double asB2 = R::sign(t2)*sqrt(t2*t2/(nudbl+t2*t2));
  const int J = delta1.size();
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  const int n = nu-1;
  NumericMatrix H(n,J); NumericMatrix M1(n,J); NumericMatrix M2(n,J);
  NumericVector Lfactor1 = ab1 * dnorm(a1*R-delta1);
  NumericVector Lfactor2 = ab2 * dnorm(a2*R-delta2);
  H(0,_) = dnorm(R);
  M1(0,_) = asB1*dnorm(delta1*sB1)*pnorm((delta1*ab1-R)/sB1);
  M2(0,_) = asB2*dnorm(delta2*sB2) *
            (pnorm(delta2*asB2)-pnorm((delta2*ab2-R)/sB2));
  if(nu >= 3){
    H(1,_) = R * H(0,_);
    M1(1,_) = delta1*ab1*M1(0,_) +
                ab1*dnorm(delta1*sB1)*dnorm((delta1*ab1-R)/sB1);
    M2(1,_) = delta2*ab2*M2(0,_) +
                ab2*dnorm(delta2*sB2) *
                    (dnorm(delta2*asB2)-dnorm((delta2*ab2-R)/sB2));
    if(nu >= 4){
      if(algo==1){
        NumericVector A(n); A[0] = 1.0; A[1] = 1.0;
        NumericVector L = H(0,_);
        for(int k=2; k<n; k++){
            A[k] = 1.0 / (k*A[k-1]);
            NumericVector AkRj = A[k] * R;
            L = AkRj * L;
            H(k,_) = AkRj * H(k-1,_);
            M1(k,_) = (k-1.0)/k * (A[k-2]*delta1*ab1*M1(k-1,_) + b1*M1(k-2,_)) +
                        Lfactor1*L;
            M2(k,_) = (k-1.0)/k * (A[k-2]*delta2*ab2*M2(k-1,_) + b2*M2(k-2,_)) -
                        Lfactor2*L;
        }
      }else{ // algo 2
        NumericVector W = -(0.5 * R*R + logsqrt2pi);
        NumericVector logR = log(R);
        double A = 1.0;
        double u = 0.0; double v = 0.0; double ldf;
        for(int k=0; k<n-2; k++){
          if(k % 2 == 0){
            u += log(k+2.0); ldf = u;
          }else{
            v += log(k+2.0); ldf = v;
          }
          double r = (k+1.0)/(k+2.0);
          NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
          H(k+2,_) = K*R;
          M1(k+2,_) = r*(A*delta1*ab1*M1(k+1,_)+b1*M1(k,_)) + K*Lfactor1;
          M2(k+2,_) = r*(A*delta2*ab2*M2(k+1,_)+b2*M2(k,_)) - K*Lfactor2;
          A = 1.0 / ((k+1)*A);
        }
      }
    }
  }
  NumericVector sumM(J); NumericVector sumH(J);
  if(nu % 2 == 0){
    for(int i=0; i<nu-1; i+=2){
      sumM += -M2(i,_)-M1(i,_); sumH += H(i,_);
    }
    return 1.0 - pnorm(-delta2) +
            sqrt2pi*(sumM + (pnorm(a1*R-delta1) - pnorm(a2*R-delta2))*sumH);
  }else{
    for(int i=1; i<nu-1; i+=2){
      sumM += -M2(i,_)-M1(i,_); sumH += H(i,_);
    }
    return OwenCDF3_C(nu, t1, t2, delta1, delta2) +
            2.0*(sumM + (pnorm(a1*R-delta1) - pnorm(a2*R-delta2))*sumH);
  }
}

//****************************************************************************80
NumericVector OwenCDF2_C(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2){
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const int J = delta1.size();
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  NumericVector C = isPositive(delta1) - isPositive(delta2) +
                      pnorm(-delta1*sB1) - pnorm(-delta2*sB2);
  for(int i=0; i<J; i++){
    double C2 = RcppOwenT(R[i], (a2*R[i]-delta2[i])/R[i]) -
                  RcppOwenT(R[i], (a1*R[i]-delta1[i])/R[i]);
    double C3 =
      RcppOwenT(delta2[i]*sB2, (delta2[i]*ab2-R[i])/b2/delta2[i]) -
        RcppOwenT(delta1[i]*sB1, (delta1[i]*ab1-R[i])/b1/delta1[i]);
    C[i] += -2.0*(C2 + C3);
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenCDF2(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2, int algo=1){
  if(nu == 1){
    return OwenCDF2_C(nu, t1, t2, delta1, delta2);
  }
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double asB1 = R::sign(t1)*sqrt(t1*t1/(nudbl+t1*t1));
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const double asB2 = R::sign(t2)*sqrt(t2*t2/(nudbl+t2*t2));
  const int J = delta1.size();
  const int n = nu-1;
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  NumericMatrix H(n,J); NumericMatrix M1(n,J); NumericMatrix M2(n,J);
  NumericVector Lfactor1 = ab1 * dnorm(a1*R-delta1);
  NumericVector Lfactor2 = ab2 * dnorm(a2*R-delta2);
  H(0,_) = dnorm(R);
  M1(0,_) = asB1*dnorm(delta1*sB1)*pnorm((delta1*ab1-R)/sB1);
  M2(0,_) = asB2*dnorm(delta2*sB2)*pnorm((delta2*ab2-R)/sB2);
  if(nu >= 3){
    H(1,_) = xdnormx(R);
    M1(1,_) = delta1*ab1*M1(0,_) + ab1*dnorm(delta1*sB1)*dnorm((delta1*ab1-R)/sB1);
    M2(1,_) = delta2*ab2*M2(0,_) + ab2*dnorm(delta2*sB2)*dnorm((delta2*ab2-R)/sB2);
    if(nu >= 4){
      if(algo==1){
        NumericVector A(n); A[0] = 1.0; A[1] = 1.0;
        NumericVector L = H(0,_);
        for(int k=2; k<n; k++){
            A[k] = 1.0 / (k*A[k-1]);
            NumericVector AkRj = A[k] * R;
            L = AkRj * L;
            H(k,_) = AkRj * H(k-1,_);
            M1(k,_) = (k-1.0)/k * (A[k-2]*delta1*ab1*M1(k-1,_) + b1*M1(k-2,_)) +
                        Lfactor1*L;
            M2(k,_) = (k-1.0)/k * (A[k-2]*delta2*ab2*M2(k-1,_) + b2*M2(k-2,_)) +
                        Lfactor2*L;
        }
      }else{ // algo 2
        NumericVector W = -(0.5 * R*R + logsqrt2pi);
        NumericVector logR = log(R);
        double A = 1.0;
        double u = 0.0; double v = 0.0; double ldf;
        for(int k=0; k<n-2; k++){
          if(k % 2 == 0){
            u += log(k+2.0); ldf = u;
          }else{
            v += log(k+2.0); ldf = v;
          }
          double r = (k+1.0)/(k+2.0);
          NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
          H(k+2,_) = K*R;
          M1(k+2,_) = r*(A*delta1*ab1*M1(k+1,_)+b1*M1(k,_)) + K*Lfactor1;
          M2(k+2,_) = r*(A*delta2*ab2*M2(k+1,_)+b2*M2(k,_)) + K*Lfactor2;
          A = 1.0 / ((k+1)*A);
        }
      }
    }
  }
  NumericVector sumM(J); NumericVector sumH(J);
  if(nu % 2 == 0){
    for(int i=0; i<nu-1; i+=2){
      sumM += M1(i,_) - M2(i,_); sumH += H(i,_);
    }
    return sqrt2pi*(sumM -
            (pnorm(a1*R-delta1) - pnorm(a2*R-delta2))*sumH);
  }else{
    for(int i=1; i<nu-1; i+=2){
      sumM += M1(i,_) - M2(i,_); sumH += H(i,_);
    }
    return OwenCDF2_C(nu, t1, t2, delta1, delta2) +
            2.0*(sumM - (pnorm(a1*R-delta1) - pnorm(a2*R-delta2))*sumH);
  }
}

//****************************************************************************80
NumericVector OwenCDF1_C(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2){
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const int J = delta1.size();
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  NumericVector C = -isPositive(delta1) + isPositive(delta2) +
                      pnorm(-delta2*sB2);
  for(int i=0; i<J; i++){
    double C1 = RcppOwenT(delta1[i]*sB1, a1);
    double C2 = RcppOwenT(R[i], (a2*R[i]-delta2[i])/R[i]) -
        RcppOwenT(R[i], (a1*R[i]-delta1[i])/R[i]);
    double C3 =
      RcppOwenT(delta2[i]*sB2, (delta2[i]*ab2-R[i])/b2/delta2[i]) -
        RcppOwenT(delta1[i]*sB1, (delta1[i]*ab1-R[i])/b1/delta1[i]);
    C[i] += 2.0*(C1 + C2 + C3);
  }
  return C;
}

//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppOwenCDF1(int nu, double t1, double t2, NumericVector delta1,
    NumericVector delta2, int algo=1){
  if(nu == 1){
    return OwenCDF1_C(nu, t1, t2, delta1, delta2);
  }
  const double nudbl = nu;
  const double a1 = R::sign(t1)*sqrt(t1*t1/nudbl);
  const double b1 = nudbl/(nudbl+t1*t1);
  const double sB1 = sqrt(b1);
  const double ab1 = a1*b1;
  const double asB1 = R::sign(t1)*sqrt(t1*t1/(nudbl+t1*t1));
  const double a2 = R::sign(t2)*sqrt(t2*t2/nudbl);
  const double b2 = nudbl/(nudbl+t2*t2);
  const double sB2 = sqrt(b2);
  const double ab2 = a2*b2;
  const double asB2 = R::sign(t2)*sqrt(t2*t2/(nudbl+t2*t2));
  const int J = delta1.size();
  const int n = nu-1;
  const NumericVector R = sqrt(nudbl)*(delta1 - delta2)/(t1-t2);
  NumericMatrix M1(n,J); NumericMatrix M2(n,J);
  NumericVector Lfactor1 = ab1 * dnorm(a1*R-delta1);
  NumericVector Lfactor2 = ab2 * dnorm(a2*R-delta2);
  M1(0,_) = asB1*dnorm(delta1*sB1) *
                (pnorm(delta1*asB1)-pnorm((delta1*ab1-R)/sB1));
  M2(0,_) = asB2*dnorm(delta2*sB2)*pnorm((delta2*ab2-R)/sB2);
  if(nu >= 3){
    M1(1,_) = delta1*ab1*M1(0,_) +
              ab1*dnorm(delta1*sB1)*(dnorm(delta1*asB1)-dnorm((delta1*ab1-R)/sB1));
    M2(1,_) = delta2*ab2*M2(0,_) +
              ab2*dnorm(delta2*sB2)*dnorm((delta2*ab2-R)/sB2);
    if(algo==1){
      NumericVector A(n); A[0] = 1.0; A[1] = 1.0;
      NumericVector L = dnorm(R);
      for(int k=2; k<n; k++){
          A[k] = 1.0 / (k*A[k-1]);
          NumericVector AkRj = A[k] * R;
          L = AkRj * L;
          M1(k,_) = (k-1.0)/k * (A[k-2]*delta1*ab1*M1(k-1,_) + b1*M1(k-2,_)) -
                      Lfactor1*L;
          M2(k,_) = (k-1.0)/k * (A[k-2]*delta2*ab2*M2(k-1,_) + b2*M2(k-2,_)) +
                      Lfactor2*L;
      }
    }else{ // algo 2
      NumericVector W = -(0.5 * R*R + logsqrt2pi);
      NumericVector logR = log(R);
      double A = 1.0;
      double u = 0.0; double v = 0.0; double ldf;
      for(int k=0; k<n-2; k++){
        if(k % 2 == 0){
          u += log(k+2.0); ldf = u;
        }else{
          v += log(k+2.0); ldf = v;
        }
        double r = (k+1.0)/(k+2.0);
        NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
        M1(k+2,_) = r*(A*delta1*ab1*M1(k+1,_)+b1*M1(k,_)) - K*Lfactor1;
        M2(k+2,_) = r*(A*delta2*ab2*M2(k+1,_)+b2*M2(k,_)) + K*Lfactor2;
        A = 1.0 / ((k+1)*A);
      }
    }
  }
  NumericVector sumM(J);
  if(nu % 2 == 0){
    for(int i=0; i<nu-1; i+=2){
      sumM += M1(i,_) + M2(i,_);
    }
    return pnorm(-delta1) + sqrt2pi*sumM;
  }else{
    for(int i=1; i<nu-1; i+=2){
      sumM += M1(i,_) + M2(i,_);
    }
    return OwenCDF1_C(nu, t1, t2, delta1, delta2) + 2.0*sumM;
  }
}

//****************************************************************************80
NumericVector SpecialOwenCDF2_C(int nu, double t, NumericVector delta){
  const double nudbl = nu;
  const double a = sqrt(t*t/nudbl);
  const double b = nudbl/(nudbl+t*t);
  const double sB = sqrt(b);
  const double ab = a*b;
  const int J = delta.size();
  const NumericVector R = sqrt(nudbl)*delta/t;
  NumericVector C = 2*pnorm(-delta*sB);
  for(int i=0; i<J; i++){
    C[i] += 4 * (RcppOwenT(R[i], (a*R[i]-delta[i])/R[i]) +
                  RcppOwenT(delta[i]*sB, (delta[i]*ab-R[i])/b/delta[i]));
  }
  return C;
}
//****************************************************************************80
// [[Rcpp::export]]
NumericVector RcppSpecialOwenCDF2(int nu, double t, NumericVector delta, int algo=1){
  if(nu == 1){
    return SpecialOwenCDF2_C(nu, t, delta);
  }
  const double nudbl = nu;
  const double a = sqrt(t*t/nudbl);
  const double b = nudbl/(nudbl+t*t);
  const double sB = sqrt(b);
  const double ab = a*b;
  const double asB = sqrt(t*t/(nudbl+t*t));
  const int J = delta.size();
  const NumericVector R = sqrt(nudbl)*delta/t;
  const int n = nu-1;
  NumericMatrix H(n,J); NumericMatrix M(n,J);
  NumericVector Lfactor = ab * dnorm(a*R-delta);
  // Hfactor = 2*pnorm(a*R-delta) - 1
  H(0,_) = dnorm(R);
  M(0,_) = 2.0*asB*dnorm(delta*sB)*pnorm((delta*ab-R)/sB);
  if(nu >= 3){
    H(1,_) = xdnormx(R);
    M(1,_) = delta*ab*M(0,_) + 2*ab*dnorm(delta*sB)*dnorm((delta*ab-R)/sB);
    if(nu >= 4){
      if(algo == 1){
        NumericVector A(n); A[0] = 1; A[1] = 1;
        NumericVector L = 2.0*H(0,_);
        for(int k=2; k<n; k++){
          A[k] = 1.0 / (k*A[k-1]);
          NumericVector AkRj = A[k] * R;
          L = AkRj * L;
          H(k,_) = AkRj * H(k-1,_);
          M(k,_) = (k-1.0)/k *
                    (A[k-2] * delta * ab * M(k-1,_) + b*M(k-2,_)) +
                      Lfactor*L;
        }
      }else{ // algo 2
        NumericVector W = -(0.5 * R*R + logsqrt2pi);
        NumericVector logR = log(R);
        double A = 1.0;
        double u = 0.0; double v = 0.0; double ldf;
        for(int k=0; k<n-2; k++){
          if(k % 2 == 0){
            u += log(k+2.0); ldf = u;
          }else{
            v += log(k+2.0); ldf = v;
          }
          NumericVector K =  exp(-ldf + (k+1.0)*logR + W);
          H(k+2,_) = K*R;
          M(k+2,_) = (k+1.0)/(k+2.0)*(A*delta*ab*M(k+1,_) + b*M(k,_)) +
                      2.0*K*Lfactor;
          A = 1.0 / ((k+1)*A);
        }
      }
    }
  }
  NumericVector sumM(J); NumericVector sumH(J);
  if(nu % 2 == 0){
    for(int i=0; i<n; i+=2){
      sumM += M(i,_); sumH += H(i,_);
    }
    return sqrt2pi * (sumM - (2.0*pnorm(a*R-delta) - 1.0)*sumH);
  }else{
    for(int i=1; i<n; i+=2){
      sumM += M(i,_); sumH += H(i,_);
    }
    return SpecialOwenCDF2_C(nu, t, delta) +
            2.0*(sumM - (2*pnorm(a*R-delta) - 1.0)*sumH);
  }
}
