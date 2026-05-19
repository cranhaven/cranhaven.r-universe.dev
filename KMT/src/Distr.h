
#ifndef __DISTR_H
#define __DISTR_H


class Distr{

private:

  const double pi = arma::datum::pi;

  const double M_sqrt2=1.414214;
  const double M_sqrt2pi=2.506628;


  int n;
  arma::vec X;

  int nLen;
  double dGap;

  int LB;
  int UB;

  Rcpp::String strDistr;

  arma::vec S1Vec;
  arma::vec S2Vec;
  arma::vec S3Vec;
  arma::vec ReVec;

  arma::vec v0Vec;
  arma::vec v1Vec;
  arma::vec v2Vec;


  arma::vec xVec;


public:

  Distr(double _dGap,
        int _LB, int _UB,
        Rcpp::String _strDistr,
        arma::vec _S1Vec, arma::vec _S2Vec, arma::vec _S3Vec,
        arma::vec _xVec){
    nLen = _S1Vec.n_elem;
    dGap = _dGap;

    LB = _LB;
    UB = _UB;

    strDistr = _strDistr;
    S1Vec=_S1Vec;
    S2Vec=_S2Vec;
    S3Vec=_S3Vec;
    xVec=_xVec;

  }

  Distr(double _dGap,
        int _LB, int _UB,
        Rcpp::String _strDistr,
        arma::vec _S1Vec, arma::vec _S2Vec, arma::vec _S3Vec,
        arma::vec _ReVec, arma::vec _xVec){
    nLen = _S1Vec.n_elem;
    dGap = _dGap;

    LB = _LB;
    UB = _UB;

    strDistr = _strDistr;
    S1Vec=_S1Vec;
    S2Vec=_S2Vec;
    S3Vec=_S3Vec;

    ReVec = _ReVec;
    xVec=_xVec;
  }

  Distr(double _dGap,
        int _LB, int _UB,
        Rcpp::String _strDistr,
        arma::vec _S1Vec, arma::vec _S2Vec, arma::vec _S3Vec,
        arma::vec _v0Vec, arma::vec _v1Vec, arma::vec _v2Vec,
        arma::vec _xVec){
    nLen = _S1Vec.n_elem;
    dGap = _dGap;

    LB = _LB;
    UB = _UB;

    strDistr = _strDistr;
    S1Vec=_S1Vec;
    S2Vec=_S2Vec;
    S3Vec=_S3Vec;

    v0Vec=_v0Vec;
    v1Vec=_v1Vec;
    v2Vec=_v2Vec;

    xVec=_xVec;
  }

  Distr(double _dGap,
        int _LB, int _UB,
        Rcpp::String _strDistr,
        arma::vec _S1Vec, arma::vec _S2Vec, arma::vec _S3Vec,
        arma::vec _v0Vec, arma::vec _v1Vec, arma::vec _v2Vec,
        arma::vec _ReVec, arma::vec _xVec){
    nLen = _S1Vec.n_elem;
    dGap = _dGap;

    LB = _LB;
    UB = _UB;

    strDistr = _strDistr;
    S1Vec=_S1Vec;
    S2Vec=_S2Vec;
    S3Vec=_S3Vec;

    v0Vec=_v0Vec;
    v1Vec=_v1Vec;
    v2Vec=_v2Vec;

    ReVec = _ReVec;
    xVec=_xVec;
  }




  double fl(double x);
  double Fl(double x);

  double Gam11(double x);
  double Gam12(double x);
  double Gam13(double x);

  double Gam23(double x);
  double Gam22(double x);
  double Gam33(double x);

  arma::mat GammaMatrix(double x);

  double phix(double x);

  double v0(double x);
  double v1(double x);
  double v2(double x);


  double c0(double x);
  double c1(double x);
  double c2(double x);

  double A11(double x);
  double A12(double x);
  double A13(double x);
  double A21(double x);
  double A22(double x);
  double A23(double x);
  double A31(double x);
  double A32(double x);
  double A33(double x);

  arma::mat Gamma(double x);

  double B11(double x);
  double B12(double x);
  double B13(double x);
  double B21(double x);
  double B22(double x);
  double B23(double x);
  double B31(double x);
  double B32(double x);
  double B33(double x);

  arma::mat Inv_Gamma(double x);

  double s1(double x);
  double s2(double x);
  double s3(double x);


  double S1(double x);
  double S2(double x);
  double S3(double x);


  double gi(double y, double Zi);
  double Gi(double y, double Zi);

  arma::mat GetGiMat(arma::vec _X);


  ///// for logistic only
  double re(double x);
  double Re(double x);

  ///// for Gumbel only

  double v1_integrand(double x);
  double v2_integrand(double x);


};



double Distr::re(double x){

  double ex = exp(x);
  double x2 = pow(x,2);
  double exm1 = (1-ex);
  double exp1 = (1+ex);
  double out = x2*ex*pow(exm1,2)/pow(exp1, 4);

  return out;
}


double Distr::Re(double x){

  int nInd = floor((x-LB)/dGap);

  if(nInd>=(nLen-1)){
    nInd=(nLen-1);
  }

  if(nInd<0){
    nInd=0;
    return ReVec[nInd];
  }


  double xi = xVec[nInd];

  double dMajor = ReVec[nInd];
  double remaining = (x-xi)*re(xi);

  double out = dMajor-remaining;
  return out;

}


double Distr::fl(double x){
  double out=0;

  if(strDistr=="Normal"){
    out = R::dnorm(x, 0, 1, 0);

  }else if(strDistr=="Cauchy"){
    out = 1 / ( pi*(1+pow(x,2)) );
  }else if(strDistr=="Logistic"){
    out= exp(x) / pow((1+exp(x)),2);


  }else{
    out= 0;
  }
  return out;

}



double Distr::Fl(double x){

  double out=0;

  if(strDistr=="Normal"){
    out=R::pnorm(x, 0, 1, 1, 0);
  }else if(strDistr=="Cauchy"){
    out = 0.5+atan(x)/pi;
  }else if(strDistr=="Logistic"){
    out= exp(x) / (1+exp(x));
  }else{
    out=0;
  }
  return out;
}


double Distr::phix(double x){

  double out=0;

  if(strDistr=="Normal"){
    out=x;
  }else if(strDistr=="Cauchy"){
    out=(2*x)/(1+pow(x,2));
  }else if(strDistr=="Logistic"){
    out = (exp(x)-1)/( 1+exp(x) );
  }else{
    out=0;
  }
  return out;
}


double Distr::v1_integrand(double x){
  double phival = phix(x);
  double phival2 = x*phival-1;
  double fval = fl(x);
  double ans = phival * phival2 *fval;
  return ans;
}

double Distr::v2_integrand(double x){

  double phival2 = x*phix(x)-1;
  double fval = fl(x);
  double ans = pow(phival2, 2)*fval;
  return ans;
}


double Distr::v0(double x){
  double out=0;

  if(strDistr=="Normal"){
    out=1+x*fl(x)-Fl(x);
  }else if(strDistr=="Cauchy"){
    out = 0.5*(  1-Fl(x) +x*(1-pow(x,2))/( pi*(pow( (1+pow(x,2)),2))) );
  }else if(strDistr=="Logistic"){
    out = (3*pow(exp(x),2)+1) / (3*pow((1+exp(x)),3));
  }else if(strDistr=="Gumbel"){
    out = 1 - exp(-x)*fl(x)-Fl(x);

  }else{
    out=0;
  }
  return out;

}

double Distr::v1(double x){
  double out=0;

  if(strDistr=="Normal"){
    out=(1+pow(x,2))*fl(x);
  }else if(strDistr=="Cauchy"){
    out = fl(x)*pow(x,2)/(1+pow(x,2));
  }else if(strDistr=="Logistic"){
    out = log(1+exp(x))/3 -  exp(x)*(x*(3+pow(exp(x),2))+(1+exp(x))) / (3*pow((1+exp(x)),3));
  }else if(strDistr=="Gumbel"){

    int nInd = floor((x-LB)/dGap);

    if(nInd>(nLen-1)){
      nInd=(nLen-1);
    }

    if(nInd<0){
      nInd=0;
      return v1Vec[nInd];
    }

    double xi = xVec[nInd];

    double dMajor = v1Vec[nInd];
    double remaining = (x-xi)*v1_integrand(xi);
    out = dMajor-remaining;

  }else{
    out=0;
  }
  return out;

}

double Distr::v2(double x){
  double out=0;

  if(strDistr=="Normal"){
    out=(pow(x,3)+x)*fl(x)+2*(1-Fl(x));
  }else if(strDistr=="Cauchy"){
    out = 0.5*(  1-Fl(x) - x*(1-pow(x,2))/( pi*(pow( (1+pow(x,2)),2) )));
  }else if(strDistr=="Logistic"){
    out = -1/(1+exp(x)) - 2*x*exp(x)/(pow((1+exp(x)),2))+Re(x);
  }else if(strDistr=="Gumbel"){

    int nInd = floor((x-LB)/dGap);

    if(nInd>(nLen-1)){
      nInd=(nLen-1);
    }

    if(nInd<0){
      nInd=0;
      return v2Vec[nInd];
    }


    double xi = xVec[nInd];

    double dMajor = v2Vec[nInd];
    double remaining = (x-xi)*v2_integrand(xi);
    out = dMajor-remaining;

  }else{
    out=0;
  }
  return out;

}


double Distr::c0(double x){
  double out=pow(x,2)*v0(x) -2*x*v1(x) + v2(x);
  return out;

}

double Distr::c2(double x){
  double out=v0(x)*v2(x) - pow(v1(x),2);
  return out;

}

double Distr::c1(double x){

  double out=(1-Fl(x)) - (fl(x)*c0(x))*(fl(x)/c2(x));

  double adj = 0;

  if(strDistr=="Normal"){
    adj=0;
  }else if(strDistr=="Logistic"){
    adj = 1e-16;
  }
  out += adj;

  return out;

}


double Distr::A11(double x){

  return 1 - Fl(x);
}


double Distr::A12(double x){
  return fl(x);
}

double Distr::A13(double x){
  return x*fl(x);
}

double Distr::A21(double x){
  return fl(x);
}


double Distr::A22(double x){
  return v0(x);
}

double Distr::A23(double x){
  return v1(x);
}

double Distr::A31(double x){
  return x*fl(x);
}


double Distr::A32(double x){
  return v1(x);
}

double Distr::A33(double x){
  return v2(x);
}


arma::mat Distr::Gamma(double x){

  arma::mat out(3,3);
  out(0,0) = A11(x);
  out(0,1) = A12(x);
  out(0,2) = A13(x);

  out(1,0) = out(0,1);
  out(1,1) = A22(x);
  out(1,2) = A23(x);

  out(2,0) = out(0,2);
  out(2,1) = out(1,2);
  out(2,2) = A33(x);

  return out;
}


double Distr::B11(double x){

  return 1;

}


double Distr::B12(double x){
  return -(fl(x)/c2(x)) * (v2(x) -x*v1(x)) ;
}

double Distr::B13(double x){
  return -fl(x)*(x*v0(x)-v1(x)) /(c2(x))  ;
}

double Distr::B21(double x){
  return B12(x);
}


double Distr::B22(double x){
  double v1val = v1(x);
  double v2val = v2(x);

  double c1val = c1(x);
  double c2val = c2(x);

  double tmp = x*v1val-v2val;
  double fval = fl(x);
  double num = c1val*c2val*v2val + pow(fval*tmp,2);
  double denum = pow(c2val,2);

  return num/denum;
}

double Distr::B23(double x){
  double v0val = v0(x);

  double v1val = v1(x);
  double v2val = v2(x);

  double c1val = c1(x);
  double c2val = c2(x);

  double tmp1 = x*v1val-v2val;
  double tmp2 = x*v0val-v1val;

  double fval = fl(x);
  //double num = pow(fval,2)*tmp1*tmp2+c1val*c2val*v1val;   <= Bug
  double num = pow(fval/c2val,2)*tmp1*tmp2+ (c1val/c2val)*v1val;

  return -num;
}

double Distr::B31(double x){
  return B13(x);
}


double Distr::B32(double x){
  return B23(x);
}

double Distr::B33(double x){
  double v0val = v0(x);
  double v1val = v1(x);

  double c1val = c1(x);
  double c2val = c2(x);

  double tmp = x*v0val-v1val;
  double fval = fl(x);
  double num = (c1val/c2val)*v0val + pow(fval*tmp/c2val,2);

  return num;
}


arma::mat Distr::Inv_Gamma(double x){

  arma::mat out=inv(Gamma(x));

  return out;
}


double Distr::s1(double x){

  double fval = fl(x);
  double b11 = B11(x);
  double b12 = B12(x);
  double b13 = B13(x);
  double phival = phix(x);
  double c1val = c1(x);
  double ans = (fval/c1val)*(b11+b12*phival+b13*(x*phival-1));

  return ans;

}

double Distr::s2(double x){

  double fval = fl(x);
  double b21 = B21(x);
  double b22 = B22(x);
  double b23 = B23(x);
  double phival = phix(x);
  double c1val = c1(x);
  double ans = (fval/c1val)*(b21+b22*phival+b23*(x*phival-1));

  return ans;

}

double Distr::s3(double x){

  double fval = fl(x);
  double b31 = B31(x);
  double b32 = B32(x);
  double b33 = B33(x);
  double phival = phix(x);
  double c1val = c1(x);
  double ans = (fval/c1val)*(b31+b32*phival+b33*(x*phival-1));

  return ans;


}


double Distr::S1(double x){

  int nInd = floor((x-LB)/dGap);

  if(nInd>=(nLen-1)){
    nInd=(nLen-1);
  }

  if(nInd<0){
    nInd=0;
    return S1Vec[nInd];
  }


  double xi = xVec[nInd];

  double dMajor = S1Vec[nInd];
  double remaining = (x-xi)*s1(xi);
  double out = dMajor+remaining;

  return out;

}

double Distr::S2(double x){

  int nInd = floor((x-LB)/dGap);

  if(nInd>=(nLen-1)){
    nInd=(nLen-1);
  }

  if(nInd<0){
    nInd=0;
    return S2Vec[nInd];
  }


  double xi = xVec[nInd];

  double dMajor = S2Vec[nInd];
  double remaining = (x-xi)*s2(xi);
  double out = dMajor+remaining;

  return out;


}

double Distr::S3(double x){

  int nInd = floor((x-LB)/dGap);

  if(nInd>=(nLen-1)){
    nInd=(nLen-1);
  }

  if(nInd<0){
    nInd=0;
    return S3Vec[nInd];
  }


  double xi = xVec[nInd];

  double dMajor = S3Vec[nInd];
  double remaining = (x-xi)*s3(xi);
  double out = dMajor+remaining;

  return out;

}


double Distr::gi(double y, double Zi){

  double out = s1(y) + phix(Zi)*s2(y)+(Zi*phix(Zi)-1)*s3(y);
  return out;

}

double Distr::Gi(double y, double Zi){

  double out = S1(y) + phix(Zi)*S2(y)+(Zi*phix(Zi)-1)*S3(y);
  return out;

}

arma::mat Distr::GetGiMat(arma::vec X){

  int n = X.size();

  arma::mat prod1(n,3);
  arma::mat prod2(3,n);


  double Xi=0;

  for(int i=1;i<=n;i++){
    Xi = X[(i-1)];
    prod1(i-1,0)=1;
    prod1(i-1,1)=phix(Xi);
    prod1(i-1,2)=(Xi*phix(Xi)-1);

    prod2(0,i-1) = S1(Xi);
    prod2(1,i-1) = S2(Xi);
    prod2(2,i-1) = S3(Xi);

  }


  return prod1*prod2;

}


#endif



















