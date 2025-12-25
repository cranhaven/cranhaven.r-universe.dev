

#include <RcppArmadillo.h>
#include <math.h>
#include <iostream>
//[[Rcpp:depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;

#define eps 2.2204e-16


// [[Rcpp::export(.epan)]]
arma::mat epan(arma::mat t, double h0) {
  
  arma::mat idx = 1 - t%t/pow(h0,2);
  arma::mat kenerl = 0.5*(idx+arma::abs(idx));
  //arma::mat kenerl = 0.75*(idx+arma::abs(idx));
  return(kenerl);
}

// [[Rcpp::export(.epand)]]
arma::mat epand(arma::mat t, double h0) {
  
  
  arma::mat idx = t/h0;
  arma::mat kenerld = (-2*idx/h0)%(arma::abs(idx)<=1);
  //arma::mat kenerld = (-1.5*idx/h0)%(arma::abs(idx)<=1);
  return(kenerld);
}


// [[Rcpp::export(.etandder)]]
List etandder(arma::vec a,arma::vec b, double h,
              arma::vec y, arma::mat z, arma::mat x, bool flag = 1)
{
  
  int n = z.n_rows;
  int dz = z.n_cols;
  
  int dx;
  if(flag)
  {
    dx = x.n_cols;
  }
  else
  {
    dx = 0;
  }
  
  arma::mat za = z*a;
  arma::mat t0 = arma::repmat(za,1,n);
  arma::mat t1 = t0 - t0.t();
  
  arma::mat t2;
  if(flag)
  {
    t2 = x*b - y;
  }
  else
  {
    t2 = -y;
  }
  
  
  arma::mat kt1 = epan(t1,h);
  
  arma::mat k00 = sum(kt1,0).t();
  arma::mat k10 = sum(kt1%t1,0).t();
  arma::mat k20 = sum(kt1%(t1%t1),0).t();
  arma::mat k01 = kt1.t()*t2;
  arma::mat k11 = (kt1%t1).t()*t2;
  
  arma::mat eta = (k20%k01 - k10%k11)/(k00%k20 - k10%k10 + eps);
  
  arma::mat kdt1 = epand(t1,h);
  
  arma::mat ka00(dz,n), ka10(dz,n), ka20(dz,n), ka01(dz,n), ka11(dz,n);
  
  arma::mat zi, kdt1i, kt1i, t1i;
  for(int i=0; i<n; i++)
  {
    zi = (z - arma::repmat(z.row(i),n,1)).t();
    
    kdt1i = kdt1.col(i);
    kt1i = kt1.col(i);
    t1i = t1.col(i);
    
    ka00.col(i) = zi*kdt1i;
    ka10.col(i) = zi*(kdt1i%t1i+kt1i);
    ka20.col(i) = zi*((kdt1i%t1i+kt1i*2)%t1i);
    ka01.col(i) = zi*(kdt1i%t2);
    ka11.col(i) = zi*((kdt1i%t1i+kt1i)%t2);
    
  }
  
  
  ka00 = ka00.t();
  ka10 = ka10.t();
  ka20 = ka20.t();
  ka01 = ka01.t();
  ka11 = ka11.t();
  
  arma::mat etab;
  if(flag)
  {
    arma::mat kb01 = kt1*x;
    arma::mat kb11 = (kt1%t1)*x;
    etab = (repmat(k20,1,dx)%kb01 - repmat(k10,1,dx)%kb11)/(repmat(k00%k20-k10%k10,1,dx) + eps);
  }
  
  arma::mat t11 = ka20%repmat(k01,1,dz) + ka01%repmat(k20,1,dz) -
    ka10%repmat(k11,1,dz) - ka11%repmat(k10,1,dz);
  
  arma::mat t22 = ka00%repmat(k20,1,dz) + ka20%repmat(k00,1,dz) - 2*ka10%repmat(k10,1,dz);
  
  arma::mat etaa = t11%repmat(k00%k20-k10%k10,1,dz) - t22%repmat(k20%k01-k10%k11,1,dz);
  etaa = etaa/(repmat(pow(k00%k20-k10%k10,2),1,dz)+eps);
  
  
  arma::mat etad;
  
  if(flag)
  {
    etad = join_rows(etaa,etab);
  }
  else
  {
    etad = etaa;
  }
  
  
  
  eta = -eta;
  etad = -etad;
  
  List result;
  
  result["eta"] = eta;
  result["etad"] = etad;
  return(result);
  
}


// [[Rcpp::export(.lasso)]]
List lasso(arma::mat x, arma::mat y, double lambda, bool verbose = 1, int MaxStep = 20)
{
  //if(verbose)
  //  cout<<"Adopt LASSO pentaly function"<<endl;
  int n_col = x.n_cols;
  int zh = arma::rank(x.t()*x);
  
  List res;
  
  if(n_col!=zh){
    res["error"] = 1;
    return(res);
  }
  
  arma::mat beta_int = inv(x.t()*x)*x.t()*y;
  
  
  int n = x.n_rows;
  int dx = x.n_cols + 1;
  
  
  double epsilon = 1e-8;
  double delta = 2;
  int step = 0;
  arma::mat beta(dx-1,1);
  
  arma::mat s0 = sum(pow(y - x*beta_int,2))/(n-dx+1);
  arma::mat th0 = 0.5*sqrt(arma::diagvec(inv(x.t()*x))*s0(0,0));
  
  arma::uvec tmp1, tmp2, tmp3;
  arma::mat x0, beta0, abeta0, pbeta0;
  while(delta >= epsilon and step < MaxStep)
  {
    step++;
    
    tmp1 = find(abs(beta_int)<=th0);
    tmp2 = find(abs(beta_int)>th0);
    
    
    beta.elem(tmp1) = arma::zeros(tmp1.n_elem);
    
    
    x0 = x.cols(tmp2);
    beta0 = beta_int.elem(tmp2);
    abeta0 = abs(beta0);
    pbeta0 = arma::ones(abeta0.n_elem);
    
    if(beta0.n_elem)
    {
      beta0 = inv( x0.t()*x0 + n*lambda*arma::diagmat(pbeta0/abeta0) )
      *x0.t()*y;
      beta.elem(tmp2) = beta0;
      delta = abs(beta-beta_int).max();
      beta_int = beta;
    }
    else
    {
      delta = epsilon/2;
    }
    
    tmp3 = find(beta!=0);
    
    if(tmp3.n_elem)
    {
      x0 = x.cols(tmp3);
      s0 = sum(pow(y - x0*inv(x0.t()*x0)*x0.t()*y,2))/(n-tmp3.n_elem);
      th0.elem(tmp3) = 0.5*sqrt(arma::diagvec(inv(x0.t()*x0))*s0(0,0));
    }
    
  }
  
  arma::mat std_beta = arma::zeros(dx-1);
  
  if(tmp3.n_elem)
  {
    s0 = sum(pow(x*beta-y,2))/(n-dx+1);
    x0 = x.cols(tmp3);
    arma::mat abeta = abs(beta0);
    pbeta0 = arma::ones(abeta.n_elem);
    arma::mat temp = inv( x0.t()*x0 + n*lambda*arma::diagmat(pbeta0/abeta) );
    std_beta.elem(tmp3) = sqrt( arma::diagvec(temp*x0.t()*x0*temp)*s0(0,0) );
  }
  
  List result;
  
  result["beta"] = beta;
  result["std_beta"] = std_beta;
  result["step"] = step;
  return(result);
}


// [[Rcpp::export(.scad)]]
List scad(arma::mat x, arma::mat y, double lambda, bool verbose = 1, int MaxStep = 20)
{
  //if(verbose)
  //  cout<<"Adopt SCAD pentaly function"<<endl;
  
  
  double a = 3.7;
  int n_col = x.n_cols;
  int zh = arma::rank(x.t()*x);
  
  List res;
  
  if(n_col!=zh){
    res["error"] = 1;
    return(res);
  }
  
  arma::mat beta_int = inv(x.t()*x)*x.t()*y;
  
  int n = x.n_rows;
  int dx = x.n_cols + 1;
  
  double epsilon = 1e-8;
  double delta = 2;
  int step = 0;
  
  arma::mat beta(dx-1,1);
  
  arma::mat s0 = sum(pow(y - x*beta_int,2))/(n-dx+1);
  
  arma::mat th0 = 0.5*sqrt(arma::diagvec(inv(x.t()*x))*s0(0,0));
  
  arma::uvec tmp1, tmp2, tmp3;
  arma::mat x0, beta0, abeta, pbeta0;
  
  
  
  while(delta >= epsilon and step < MaxStep)
  {
    step++;
    tmp1 = find(abs(beta_int)<=th0);
    tmp2 = find(abs(beta_int)>th0);
    
    beta.elem(tmp1) = arma::zeros(tmp1.n_elem);
    
    x0 = x.cols(tmp2);
    beta0 = beta_int.elem(tmp2);
    abeta = abs(beta0);
    pbeta0 = (abeta <= lambda) + (a*lambda - abeta)/((a-1)* (lambda+1e-8) )
      %((abeta>lambda)%(abeta<a*lambda));
    
    if(beta0.n_elem)
    {
      beta0 = inv( x0.t()*x0 + n*lambda*arma::diagmat(pbeta0/abeta) )
      *x0.t()*y;
      beta.elem(tmp2) = beta0;
      delta = abs(beta-beta_int).max();
      beta_int = beta;
    }
    else
    {
      delta = epsilon/2;
    }
    
    tmp3 = find(beta!=0);
    if(tmp3.n_elem)
    {
      x0 = x.cols(tmp3);
      s0 = sum(pow(y - x0*inv(x0.t()*x0)*x0.t()*y,2))/(n-sum(tmp3));
      th0.elem(tmp3) = 0.5*sqrt(arma::diagvec(inv(x0.t()*x0))*s0(0,0));
    }
  }
  
  arma::mat std_beta = arma::zeros(dx-1);
  
  if(tmp3.n_elem)
  {
    s0 = sum(pow(x*beta-y,2))/(n-dx+1);
    x0 = x.cols(tmp3);
    abeta = abs(beta0);
    pbeta0 = (abeta <= lambda) + (a*lambda - abeta)/((a-1)*lambda)
      %((abeta>lambda)%(abeta<a*lambda));
    arma::mat temp = inv( x0.t()*x0 + n*lambda*arma::diagmat(pbeta0/abeta) );
    std_beta.elem(tmp3) = sqrt( arma::diagvec(temp*x0.t()*x0*temp)*s0(0,0) );
  }
  
  List result;
  
  result["beta"] = beta;
  result["std_beta"] = std_beta;
  result["step"] = step;
  
  return(result);
}



// [[Rcpp::export(.elasticNet)]]
List elasticNet(arma::mat x, arma::mat y, double lambda, double l1_ratio, bool verbose = 1, int MaxStep = 20)
{
  //if(verbose)
  //  cout<<"Adopt Elastic Net pentaly function"<<endl;
  
  int n_col = x.n_cols;
  int zh = arma::rank(x.t()*x);
  
  List res;
  
  if(n_col!=zh){
    res["error"] = 1;
    return(res);
  }
  
  arma::mat beta_int = inv(x.t()*x)*x.t()*y;
  
  
  int n = x.n_rows;
  int dx = x.n_cols + 1;
  
  
  double epsilon = 1e-8;
  double delta = 2;
  int step = 0;
  arma::mat beta(dx-1,1);
  
  arma::mat s0 = sum(pow(y - x*beta_int,2))/(n-dx+1);
  arma::mat th0 = 0.5*sqrt(arma::diagvec(inv(x.t()*x))*s0(0,0));
  
  arma::uvec tmp1, tmp2, tmp3;
  arma::mat x0, beta0, abeta0, pbeta0;
  while(delta >= epsilon and step < MaxStep)
  {
    step++;
    
    tmp1 = find(abs(beta_int)<=th0);
    tmp2 = find(abs(beta_int)>th0);
    
    
    beta.elem(tmp1) = arma::zeros(tmp1.n_elem);
    
    
    x0 = x.cols(tmp2);
    beta0 = beta_int.elem(tmp2);
    abeta0 = abs(beta0);
    pbeta0 = l1_ratio * arma::ones(abeta0.n_elem) + (1-l1_ratio) * pow(beta0,2);
    
    if(beta0.n_elem)
    {
      beta0 = inv( x0.t()*x0 + n*lambda*arma::diagmat(pbeta0/abeta0) )
      *x0.t()*y;
      beta.elem(tmp2) = beta0;
      delta = abs(beta-beta_int).max();
      beta_int = beta;
    }
    else
    {
      delta = epsilon/2;
    }
    
    tmp3 = find(beta!=0);
    
    if(tmp3.n_elem)
    {
      x0 = x.cols(tmp3);
      s0 = sum(pow(y - x0*inv(x0.t()*x0)*x0.t()*y,2))/(n-tmp3.n_elem);
      th0.elem(tmp3) = 0.5*sqrt(arma::diagvec(inv(x0.t()*x0))*s0(0,0));
    }
    
  }
  
  arma::mat std_beta = arma::zeros(dx-1);
  
  if(tmp3.n_elem)
  {
    s0 = sum(pow(x*beta-y,2))/(n-dx+1);
    x0 = x.cols(tmp3);
    arma::mat abeta = abs(beta0);
    //pbeta0 = arma::ones(abeta.n_elem);
    pbeta0 = l1_ratio * arma::ones(abeta0.n_elem) + (1-l1_ratio) * pow(beta0,2);
    arma::mat temp = inv( x0.t()*x0 + n*lambda*arma::diagmat(pbeta0/abeta) );
    std_beta.elem(tmp3) = sqrt( arma::diagvec(temp*x0.t()*x0*temp)*s0(0,0) );
  }
  
  List result;
  
  result["beta"] = beta;
  result["std_beta"] = std_beta;
  result["step"] = step;
  return(result);
}


// [[Rcpp::export(.plsimCore)]]
List plsimCore(arma::mat x, arma::mat y, arma::mat z,
               double h, arma::vec zetaini, double lambda, double l1_ratio, int MaxStep=1,
               std::string Method = "SCAD", bool flag=1, bool verbose=1)
{
  
  int n = z.n_rows;
  int dz = z.n_cols;
  List error;
  int size;
  
  error["error"] = 1;
  
  int dx;
  if(flag)
  {
    dx = x.n_cols;
  }
  else
  {
    dx = 0;
  }
  
  
  arma::mat a = zetaini.subvec(0,dz-1);
  arma::mat b;
  if(flag)  b = zetaini.subvec(dz,dz+dx-1);
  
  double epsilon = 1e-8;
  double delta = 2;
  int step = 0;
  
  arma::mat eta, f1, r, ys, zeta, stdzeta;
  List etandder_result, result_linear;
  
  while(delta > epsilon and step < MaxStep)
  {
    step++;
    //cout<<step<<endl;
    
    etandder_result = etandder(a,b,h,y,z,x,flag);
    eta = as<arma::mat>(etandder_result["eta"]);
    f1 = as<arma::mat>(etandder_result["etad"]);
    
    if(flag)
    {
      r = y - eta - x*b;
      f1.cols(dz,dx+dz-1) = f1.cols(dz,dx+dz-1) + x;
    }
    else
    {
      r = y - eta;
    }
    
    ys = r + f1*zetaini;
    
    if(Method == "SCAD")
    {
      result_linear = scad(f1,ys,lambda,verbose);
    }
    else if(Method == "LASSO")
    {
      result_linear = lasso(f1,ys,lambda,verbose);
    }
    else if(Method == "ElasticNet")
    {
      result_linear = elasticNet(f1,ys,lambda,l1_ratio,verbose);
      
    }
    
    size = result_linear.size();
    
    if(size==1)
      return(error);
    
    zeta = as<arma::mat>(result_linear["beta"]);
    stdzeta = as<arma::mat>(result_linear["std_beta"]);
    
    a = zeta.rows(0,dz-1);
    arma::uvec tmp_a = find(a==0);
    
    if( tmp_a.n_elem < dz )
    {
      if(norm(a,2)!=0)
      {
        a = sign(a)[0]*a/norm(a,2);
      }
    }
    
    if(flag)
    {
      b = zeta.rows(dz,dx+dz-1);
      zeta = join_cols(a,b);
    }
    else
    {
      zeta = a;
    }
    
    delta = abs(zeta-zetaini).max();
    zetaini = zeta;
    
  }
  
  etandder_result = etandder(a,b,h,y,z,x,flag);
  eta = as<arma::mat>(etandder_result["eta"]);
  
  
  arma::mat y_hat;
  if(flag)
  {
    y_hat = eta + x*b;
    //r = y-eta-x*b;
  }
  else
  {
    y_hat = eta;
    //r = y - eta;
  }
  r = y - y_hat;
  
  arma::mat mse = sum(pow(r,2))/n;
  arma::mat variance = var(y_hat);
  
  List result;
  result["zeta"] = zeta;
  result["mse"] = mse;
  result["variance"] = variance;
  result["eta"] = eta;
  result["y_hat"] = y_hat;
  result["stdzeta"] = stdzeta;
  
  return(result);
}

// [[Rcpp::export(.plsimestCore)]]
List plsimestCore(arma::mat x, arma::mat y, arma::mat z,
                  double h, arma::vec zetaini, int MaxStep=200,
                  bool flag=1)
{
  int n = z.n_rows;
  int dz = z.n_cols;
  
  int dx;
  if(flag)
  {
    dx = x.n_cols;
  }
  else
  {
    dx = 0;
  }
  
  
  arma::mat a = zetaini.subvec(0,dz-1);
  
  arma::mat b;
  if(flag) b = zetaini.subvec(dz,dz+dx-1);
  
  double epsilon = 1e-8;
  double delta = 2;
  int step = 0;
  
  arma::mat eyep1 = arma::diagmat(arma::ones(dx+dz))/pow(n,2);
  
  List etandder_result;
  arma::mat eta, f1, r, q1, q2inv, difference, zeta;
  
  while( delta > epsilon and step < MaxStep)
  {
    step++;
    
    etandder_result = etandder(a,b,h,y,z,x,flag);
    eta = as<arma::mat>(etandder_result["eta"]);
    f1 = as<arma::mat>(etandder_result["etad"]);
    
    //cout<<"eta:"<<eta<<"f1:"<<f1<<endl;
    
    
    if(flag)
    {
      r = y-eta-x*b;
      f1.cols(dz,dx+dz-1) = f1.cols(dz,dx+dz-1) + x;
    }
    else
    {
      r = y-eta;
    }
    
    q1 = -1*f1.t()*r;
    q2inv = inv(f1.t()*f1+eyep1);
    
    difference = q2inv*q1;
    zeta = zetaini - difference;
    
    a = zeta.rows(0,dz-1);
    
    a = sign(a)[0]*a/norm(a,2);
    
    if(flag)
    {
      b = zeta.rows(dz,dx+dz-1);
      zeta = join_cols(a,b);
    }
    else
    {
      zeta = a;
    }
    
    delta = abs(zeta-zetaini).max();
    zetaini = zeta;
    
    //cout<<"step:"<<step<<endl<<"zetaini"<<zetaini<<endl;
    
  }
  
  
  etandder_result = etandder(a,b,h,y,z,x,flag);
  eta = as<arma::mat>(etandder_result["eta"]);
  f1 = as<arma::mat>(etandder_result["etad"]);
  
  
  arma::mat y_hat;
  if(flag)
  {
    y_hat = eta + x*b;
    //r = y-eta-x*b;
    //f1.cols(dz,dx+dz-1) = f1.cols(dz,dx+dz-1) + x;
  }
  else
  {
    y_hat = eta;
    //r = y-eta;
  }
  r = y - y_hat;
  
  q2inv = inv(f1.t()*f1+eyep1);
  
  arma::vec stdzeta = sqrt( arma::diagvec( q2inv* (f1.t()*arma::diagmat(pow(r,2))
                                                     *f1*q2inv )) );
  
  arma::mat mse = sum(pow(r,2))/n;
  arma::mat variance = var(y_hat);
  
  
  List result;
  result["stdzeta"] = stdzeta;
  result["zeta"] = zeta;
  //result["step"] = step;
  result["eta"] = eta;
  result["mse"] = mse;
  result["variance"] = variance;
  result["y_hat"] = y_hat;
  
  return(result);
}

