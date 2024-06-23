#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
using namespace arma;
using namespace Rcpp;

#define TOLERANCE 1.0e-200
#define NA_VALUE R_NaReal
#define SQRT2 1.414214



double Kernel(double x, int ind = 2){
  if(ind == 0){/*	uniform kernel	*/
    if(fabs(x) <= 1) return(1/2); else return( 0 );
  }else if(ind == 1){/*	Triangular kernel	*/
    if(fabs(x) <= 1) return( 1-fabs(x)); else return( 0 );
  }else if(ind == 2){/*	Epanechnikov kernel	*/
    if(fabs(x) <= 1) return( 3*(1-x*x)/4 ); else return( 0 );
  }else if(ind == 3){/*	Quartic kernel	*/
    if(fabs(x) <= 1) return( 15*(1-x*x)*(1-x*x)/16 ); else return( 0 );
  }else if(ind == 4){/*	Triweight kernel	*/
    if(fabs(x) <= 1) return( 35*(1-x*x)*(1-x*x)*(1-x*x)/32 ); else return( 0 );
  }else if(ind == 5){/*	Triweight kernel	*/
    if(fabs(x) <= 1) return( 70*pow((1-pow(fabs(x),3)), 3)/81 ); else return( 0 );
  }else{
    if(fabs(x) <= 1) return( 3*(1-x*x)/4 ); else return( 0 );
  }

}
double EpaK(double x){
  return(Kernel(x));
}

// double EpaK0(double x1_0, double x2_0)
//   /* v0	*/
// {
//   double x1,x2;
//   if(x1_0 >=-1) x1 = x1_0;
//   else x1 = -1;
//
//   if(x2_0 <= 1) x2 = x2_0;
//   else x2 = 1;
//
//   return(0.75*((x2 - pow(x2,3)/3)-(x1 - pow(x1,3)/3)));
// }
//
// double EpaK1(double x1_0, double x2_0)
//   /* v0	*/
// {
//   double x1,x2;
//   if(x1_0 >=-1) x1 = x1_0;
//   else x1 = -1;
//
//   if(x2_0 <= 1) x2 = x2_0;
//   else x2 = 1;
//
//   return(0.75*((pow(x2,2)/2 - pow(x2,4)/4)-(pow(x1,2)/2 - pow(x1,4)/4)));
// }
//
// double EpaK2(double x1_0, double x2_0)
//   /* v0	*/
// {
//   double x1,x2;
//   if(x1_0 >=-1) x1 = x1_0;
//   else x1 = -1;
//
//   if(x2_0 <= 1) x2 = x2_0;
//   else x2 = 1;
//
//   return(0.75*((pow(x2,3)/3 - pow(x2,5)/5)-(pow(x1,3)/3 - pow(x1,5)/5)));
// }



double db_EpaK(double x)
  /*	Epanechnikov kernel	*/
{
  return( 2*SQRT2*EpaK(SQRT2*x)- EpaK(x));
}



double psum(NumericVector e, double mean_e, int m, int j)
{
  double sjm;

  sjm = - m * mean_e;
  for(int k = 0; k <= m - 1; k++){
    sjm += e[j + k - 1];
    //j-1 becuase we take j as defined in the paper, starting from 1
  }
  return(sjm);
}

NumericVector all_psum(int m, int n, NumericVector e, bool mean = 1)
{
  double mean_e = 0.0;
  if(mean) mean_e = std::accumulate(e.begin(),e.end(),0.0)/n;
  NumericVector all_partial_sum(n - m + 1);

  for(int i = 0; i <  n - m + 1; i++){
    all_partial_sum[i] = psum(e, mean_e, m , i + 1); // all the S_{i,m}-m/n*S_n needed
  }
  return(all_partial_sum);
}


NumericVector Compute_kernel_vector(int n, double bw, int type, int ind = 2)
{
  //type: 0 for naive kernel 1 for Jackknife 2 for Jackknife equivalent kernel
  NumericVector kernel_vector(n);

  if(type == 0){
    for(int i = 0; i < n * bw ;i++)
      kernel_vector[i] =  Kernel(((i + 0.)/n)/bw, ind);//interval
  }else if(type == 1 || type == 2){
    for(int i = 0; i < n * bw ;i++)
      kernel_vector[i] =  db_EpaK(((i + 0.)/n)/bw);//interval
  }
  else{
    Rcout<<"Bad type!Please choose from 0,1,2"<<endl;
  }

  return(kernel_vector);
}


arma::vec locLinSmootherC (double bw, int n, arma::vec x, arma::vec y, bool db_kernel =0)
{
  //int n = y.size();
  int i,j;
  double s0,s1,s2,t0,t1,auxK,aux;
  double den;
  arma::vec beta0(n);

  for(i = 0; i < n; i++)
  {
    s0 = s1 =s2 = t0 = t1 = 0.;
    beta0[i] = den = 0.;

    for(j = 0; j < n; j++)
    {
      aux = (x[j]-x[i]) / bw;

      if(db_kernel)
      {//debias kernel
        auxK = db_EpaK(aux)/ bw;
      }else{
        auxK = EpaK(aux)/ bw;
      }

      t0 += y[j] * auxK;
      t1 += y[j] * auxK * aux;
      s0 += auxK;
      s1 += auxK * aux;
      s2 += auxK * aux * aux;
    }
    den = (s2*s0 - s1*s1);

    if(abs(den)> TOLERANCE)
    {
      beta0[i] = (s2*t0 - s1*t1)/den;
    }
    else
      beta0[i] = NA_VALUE;
  }
  return(beta0);
}

//' @export
//' @name loc_constant
//' @title Nonparametric smoothing
//' @param bw, double, bandwidth, between 0 and 1.
//' @param x, vector, covariates
//' @param y, matrix, response variables
//' @param db_kernel, bool, whether to use jackknife kernel, default 0
//' @return a matrix of smoothed values
//' @examples
//' n <- 800
//' p <- 3
//' t <- (1:n)/n
//' V <-  matrix(rnorm(n * p), nrow = p)
//' V3 <- loc_constant(0.2, t, V,1)
// [[Rcpp::export]]
arma::mat loc_constant (double bw, arma::vec x, arma::mat y, bool db_kernel = 0){
  //int n = y.size();
  int i,j;
  int p = y.n_rows;
  int n = y.n_cols;
  double auxK,aux;
  arma::mat t0(p, n);
  t0.zeros();

  for(i = 0; i < n; i++){

    for(j = 0; j < n; j++){
      aux = (x[j]-x[i]) / bw;

      if(db_kernel)
      {//debias kernel
        auxK = db_EpaK(aux)/ bw;
      }else{
        auxK = EpaK(aux)/ bw;
      }

      t0.col(i) += y.col(j) * auxK;
    }
  }

  return(t0/n);
}
//' @export
//' @name LocLinear
//' @title Local linear Regression
//' @description  Local linear estimates for time varying coefficients
//' @param bw double, bandwidth
//' @param t vector, time, 1:n/n
//' @param y vector, response series to be tested for long memory in the next step
//' @param X matrix, covariates matrix
//' @param db_kernel bool, whether to use jackknife kernel, default 0
//' @param deriv2 bool,whether to return second-order derivative, default 0
//' @param scb bool, whether to use the result for further calculation of simultaneous confidence bands.
//' @return a list of results
//' \itemize{
//' \item mu: the estimated trend
//' \item beta0: time varying coefficient
//' \item X_reg: a matrix whose j'th row is \mjseqn{x_j^T\hat{M}(t_j)}
//' \item t: 1:n/n
//' \item bw: bandwidth used
//' \item X: covariates matrix
//' \item y: response
//' \item n: sample size
//' \item p: dimension of covariates including the intercept
//' \item invM: inversion of M matrix, when scb = 1
//' }
//' @details
//' The time varying coefficients are estimated by
//' \mjsdeqn{(\hat{\boldsymbol{\beta}}_{b_{n}}(t), \hat{\boldsymbol{\beta}}_{b_{n}}^{\prime}(t)) = \mathbf{arg min}_{\eta_{0},\eta_{1}}[\sum_{i=1}^{n}\{y_{i}-\mathbf{x}_{i}^{\mathrm{T}}\eta_{0}-\mathbf{x}_{i}^{\mathrm{T}} \eta_{1}(t_{i}-t)\}^{2} \boldsymbol{K}_{b_{n}}(t_{i}-t)]}
//' where beta0 is \mjseqn{\hat{\boldsymbol{\beta}}_{b_{n}}(t)}, mu is \mjseqn{X^T \hat{\boldsymbol{\beta}}_{b_{n}}(t)}
//' @examples
//' param = list(d = -0.2, heter = 2, tvd = 0,
//'  tw = 0.8, rate = 0.1, cur = 1, center = 0.3,
//'   ma_rate =  0, cov_tw =  0.2, cov_rate = 0.1,
//'    cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
//' n = 500
//' t = (1:n)/n
//' data = Qct_reg(n, param)
//' result = LocLinear(0.2, t, data$y, data$x)
//' @references
//' Zhou, Z., & Wu, W. B. (2010). Simultaneous inference of linear models with time varying coefficients. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 72(4), 513-531.
// [[Rcpp::export]]
List LocLinear( double bw,
               arma::vec t, arma::vec y,
               arma::mat X,
               bool db_kernel = 0,
               bool deriv2 = 0,
               bool scb = 0)
{
  //Local linear estimates with Covariates using Epanechnikov kernel
  //t: time
  //y: response
  //X: covariates
  //n: sample size
  //p: dimension of covariates
  //db_kernel: use jackknife or not

  int n,p;
  double  b2_int = 0;
  p = X.n_cols;
  n = X.n_rows;

  arma::mat beta0(n,p),beta,mu(n,1),tmpl(p,p),tmpr(p,p),M(p,p);
  arma::mat s0(p,p),s1(p,p),s2(p,p),t0(p,1),t1(p,1),tmpx(p,p);
  arma:: mat S(2*p, 2*p), R(2*p,1), X_reg(n,p);
  arma::mat mu_W(n,p), mu_reg(n,p);
  double auxK, aux;
  List result;
  arma::cube Mt(p,p,n);
  vec b2(p), tmpb(p);
  b2.zeros();
  tmpb.zeros();

  mu_W.col(0).ones();
  for(int i = 1; i < p; i++ )
    mu_W.col(i) = locLinSmootherC (0.2 , n, t, X.col(i), db_kernel =0);

  for(int i = 0; i < n; i++)
  {
    s0.zeros();s1.zeros();s2.zeros();
    t0.zeros();t1.zeros();

    for(int j  = i - n * bw; j <= i + n * bw; j++)
    {
      if(j >= 0 && j < n)
      {
        aux = (t[j] - t[i]) / bw;
        if(db_kernel) auxK = db_EpaK(aux)/(n*bw);
        else auxK = EpaK(aux)/ (n*bw);

        // std::cout<<auxK<<endl;
        // (X.row(j).t()*y[j]* auxK).print();

        t0 += X.row(j).t() * y[j] * auxK;
        t1 += X.row(j).t() * y[j] * auxK * aux;
        tmpx = X.row(j).t() * X.row(j);
        // t0 += (X.row(j).t() - X.row(i).t())*y[j] * auxK;
        // t1 += (X.row(j).t() - X.row(i).t())*y[j] * auxK * aux;
        // tmpx = (X.row(j).t() - X.row(i).t())*(X.row(j) - X.row(i));
        s0 += tmpx * auxK;
        s1 += tmpx * auxK * aux;
        s2 += tmpx * auxK * aux * aux;
      }
    }
    // s0.print();
    // t0.print();
    if( i - n * bw >= 0 &&  (n - 1 - n * bw) - i >= 0){
      M.zeros(); //initalization
      //if(p > 1) M.submat(1, 1, p - 1, p - 1) = s0.submat(1, 1, p - 1, p - 1); restriction for covariance matrix
      //however not right if X has a time varying mean
      M = s0;
      M(0,0) = 1;

      X_reg.row(i) = X.row(i) * inv(M);
      mu_reg.row(i) = mu_W.row(i) * inv(M);
      if(scb) Mt.slice(i) = inv(M);

      if( i - n * bw >= 0  && i - n * bw < 1 ) tmpl = inv(M);
      if((n - 1 - n * bw) - i < 1 && (n - 1 - n * bw) - i >= 0) tmpr = inv(M);
    }

    S = join_cols(join_rows(s0,s1), join_rows(s1,s2));
    R = join_cols(t0,t1);
    beta = solve(S,R);
    beta0.row(i) = beta.rows(0, p - 1).t();
    mu.row(i)=  X.row(i) * beta.rows(0, p - 1);
    if(deriv2){
      if(i == 0){
        tmpb = beta.rows(p, 2*p - 1);
      }else{
        b2 = (beta.rows(p, 2*p - 1) - tmpb)*n;
        tmpb = beta.rows(p, 2*p - 1);
        b2_int += norm(b2)*norm(b2);
      }
      result["b2"] = b2_int/n;
    }
  }

  for(int i = 0; i < n * bw + 1; i++){
    X_reg.row(i) = X.row(i) * tmpl;
    mu_reg.row(i) = mu_W.row(i) * tmpl;
    if(scb) Mt.slice(i) = tmpl;
  }
  for(int i = n - 1; i >= (n - 1 - n*bw) - 1; i--){
    X_reg.row(i) = X.row(i) * tmpr;
    mu_reg.row(i) = mu_W.row(i) * tmpr;
    if(scb) Mt.slice(i) = tmpr;
  }

  if(scb) result["invM"]  = Mt;
  result["mu"] = mu;
  result["beta0"] = beta0;
  result["X_reg"] = X_reg;
  result["mu_reg"] = mu_reg;
  result["t"] = t;
  result["bw"] = bw;
  result["X"] = X;
  result["y"] = y;
  result["n"] = n;
  result["p"] = p;

  return result;
}




arma::mat LocLinear_B( double bw,
                arma::vec t, arma::vec y,
                arma::mat X){
  //Local linear estimates with Covariates using Epanechnikov kernel
  //t: time
  //y: response
  //X: covariates
  //n: sample size
  //p: dimension of covariates
  //db_kernel: use jackknife or not

  int n,p;
  p = X.n_cols;
  n = X.n_rows;

  arma::mat beta0(n,p),beta;
  arma::mat s0(p,p),s1(p,p),s2(p,p),t0(p,1),t1(p,1),tmpx(p,p);
  arma:: mat S(2*p, 2*p), R(2*p,1);
  double auxK, aux;

  for(int i = 0; i < n; i++){
    s0.zeros();s1.zeros();s2.zeros();
    t0.zeros();t1.zeros();

    for(int j  = i - n * bw; j <= i + n * bw; j++){
      if(j >= 0 && j < n){

        aux = (t[j] - t[i]) / bw;
        auxK = EpaK(aux)/ (n*bw);

        t0 += X.row(j).t() * y[j] * auxK;
        t1 += X.row(j).t() * y[j] * auxK * aux;
        tmpx = X.row(j).t() * X.row(j);

        s0 += tmpx * auxK;
        s1 += tmpx * auxK * aux;
        s2 += tmpx * auxK * aux * aux;
      }
    }


    S = join_cols(join_rows(s0,s1), join_rows(s1,s2));
    R = join_cols(t0,t1);
    beta = solve(S,R);
    beta0.row(i) = beta.rows(0, p - 1).t();
    }

  return beta0;
}


//' @export
//' @name sim_T
//' @title bootstrap distribution
//' @description  bootstrap distribution of the gradient based structural stability test
//' @param X, matrix of covariates
//' @param t, vector of time points
//' @param sigma, a cube of long-run covariance function.
//' @param m, int value of window size
//' @param B, int, number of iteration
//' @param type, type of tests, residual-based or coefficient-based
//' @examples
//' param = list(B = 50, bw_set = c(0.15, 0.25), gcv =1, neighbour = 1, lb = 10, ub = 20, type = 0)
//' n = 300
//' data = bregress2(n, 2, 1) # time series regression model with 2 changes points
//' sigma = Heter_LRV(data$y, data$x, 3, 0.3, lrv_method = 1)
//' bootstrap = sim_T(data$x, (1:n)/n, sigma, 3, 20) ### 20 iterations
//' @return a vector of bootstrap statistics
// [[Rcpp::export]]
arma::vec sim_T(arma::mat X,  arma::vec t, arma::cube sigma, int m,  int B, int type =0){
  //type 0 res
  //type 1 coef
  int n = X.n_rows;
  int p = X.n_cols;

  // double auxK, aux;
  arma::mat tmpx(p,p), tmp(p,B), U1(p,B), s0(p, p), s2(p, B),  maxs(1,B), res(1,B);
  arma::cube M(p, p, n), invM(p, p, n), R(p, B, n);
  R.randn();

   s0.zeros(); //
   M.zeros();
   invM.zeros();

   for(int i = 0; i < n; i++){
    // initalization
    tmpx = X.row(i).t() * X.row(i);
    s0 += tmpx /n;
    if(i >= m-1){
      invM.slice(i) = pinv(s0);
       M.slice(i) = s0;
    }
  }
  // std::cout<<"M"<<endl;
  U1.zeros();
  for(int r = 0; r < n; r++){
     U1 += sigma.slice(r) * R.slice(r);//p*B
  }
  U1 /= sqrt(n);

  for(int r = 0; r < n - m; r++){
     tmp += sigma.slice(r) * R.slice(r); // p*B
    if(r >= m-1){
        if(type){
        s2 = invM.slice(r)*tmp/(n-2*m+1) - invM.slice(n-1)*U1 ;//p*B
      }else{
        s2 = tmp/sqrt(n-2*m+1) - M.slice(r)* invM.slice(n-1)*U1 ;//p*B
      }
      res = sum(pow(s2,2), 0);
      res = sqrt(res);

      maxs = (res > maxs)%res + (res <= maxs)%maxs;
    }
  }

  return maxs.t();
}



// [[Rcpp::export]]
arma::cube Diff1(arma::vec e, arma::mat X, int m, double tau_n = 0, int ind = 2)
{
  int n = X.n_rows;
  int p = X.n_cols;
  double regular_factor;
  arma::cube all_delta2(p, p, n), sigma(p, p, n);
  arma::vec L(p), kernel_vector(n);
  vec eigval;
  mat eigvec;

  if(!tau_n) tau_n = powf(n,-5.0/29);
  kernel_vector = Compute_kernel_vector(n, tau_n, 0, ind);

  L.zeros();

  for(int j = 0; j < m; j++)
  { //e=y
    L = L + e(j + m) * X.row(j + m).t()- e(j) * X.row(j).t();
  }
  for(int j = m - 1  ; j < n - m ; j++)//j needs  conversion starts from 0
  {
    //std::cout<<j<<endl;
    if(j > m - 1)
    {
      L = L - 2* e(j) * X.row(j).t() + e(j + m) * X.row(j + m).t() + e(j - m) * X.row(j - m).t();
    }
    // std::cout<<arma::size(all_delta2.slice(j))<<endl;
    // std::cout<<arma::size(L * L.t())<<endl;
    all_delta2.slice(j) = L * L.t() / (2 * m);
  }
  //std::cout<<all_delta2[m-1]<<endl;
  for(int j = 0; j < m - 1; j++) all_delta2.slice(j).zeros();
  for(int j = n - m; j < n; j++) all_delta2.slice(j).zeros();

  for(int i = m - 1 ; i < n - m; i++)
  {
    sigma.slice(i).zeros();//t=(i+1.0)/n
    regular_factor = 0;
    for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
    {//all_delta2[j] is \Delta_j^2
      if(j > 0 && j < n){
        sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
        regular_factor += kernel_vector[abs(j - i)];
      }
    }
    sigma.slice(i) = sigma.slice(i)/regular_factor;
    // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
  }

  for(int i = 0; i < m -1; i++) sigma.slice(i) = sigma.slice(m - 1);
  for(int i = n - m ; i < n; i++) sigma.slice(i) = sigma.slice(n - m - 1);

  return(sigma);//Sigma2
}

//
// arma::cube DiffX0(arma::mat X, int m, double tau_n = 0)
// {
//   int n = X.n_rows;
//   int p = X.n_cols;
//   double regular_factor;
//   arma::cube all_delta2(p, p, n), sigma(p, p, n);
//   arma::vec kernel_vector(n);
//   arma::mat L(p,p);
//   vec eigval;
//   mat eigvec;
//
//   if(!tau_n) tau_n = powf(n,-5/29);
//   kernel_vector = Compute_kernel_vector(n, tau_n, 0);
//
//   L.zeros();
//
//   for(int j = 0; j < m; j++)
//   { //e=y
//     L = L +  X.row(j + m).t() *  X.row(j + m) - X.row(j).t() *  X.row(j);
//   }
//   for(int j = m - 1  ; j < n - m ; j++)//j needs  conversion starts from 0
//   {
//     //std::cout<<j<<endl;
//     if(j > m - 1)
//     {
//       L = L - 2* X.row(j).t()*X.row(j) +
//         X.row(j + m).t()* X.row(j + m) +
//         X.row(j - m).t()*X.row(j - m);
//     }
//     // std::cout<<arma::size(all_delta2.slice(j))<<endl;
//     // std::cout<<arma::size(L * L.t())<<endl;
//     all_delta2.slice(j) = L * L.t() / (2 * m);
//   }
//   //std::cout<<all_delta2[m-1]<<endl;
//   for(int j = 0; j < m - 1; j++) all_delta2.slice(j).zeros();
//   for(int j = n - m; j < n; j++) all_delta2.slice(j).zeros();
//
//   for(int i = m - 1 ; i < n - m; i++)
//   {
//     sigma.slice(i).zeros();//t=(i+1.0)/n
//     regular_factor = 0;
//     for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
//     {//all_delta2[j] is \Delta_j^2
//       if(j > 0 && j < n){
//         sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
//         regular_factor += kernel_vector[abs(j - i)];
//       }
//     }
//     sigma.slice(i) = sigma.slice(i)/regular_factor;
//     // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
//   }
//
//   for(int i = 0; i < m -1; i++) sigma.slice(i) = sigma.slice(m - 1);
//   for(int i = n - m ; i < n; i++) sigma.slice(i) = sigma.slice(n - m - 1);
//
//   return(sigma);//Sigma2
// }
//
// arma::cube DiffA0(arma::vec y, arma::mat X, int m, double tau_n = 0)
// {
//   int n = X.n_rows;
//   int p = X.n_cols;
//   double regular_factor;
//   arma::cube all_delta2(p, 1, n), sigma(p, 1, n);
//   arma::vec kernel_vector(n), L1(p);
//   arma::mat L(p,p);
//   vec eigval;
//   mat eigvec;
//
//   if(!tau_n) tau_n = powf(n,-5/29);
//   kernel_vector = Compute_kernel_vector(n, tau_n, 0);
//
//   L.zeros();
//   L1.zeros();
//
//   for(int j = 0; j < m; j++)
//   { //e=y
//     L = L +  X.row(j + m).t() *  X.row(j + m) - X.row(j).t() *  X.row(j);
//     L1 = L1 + y(j + m) * X.row(j + m).t()- y(j) * X.row(j).t();
//   }
//   for(int j = m - 1  ; j < n - m ; j++)//j needs  conversion starts from 0
//   {
//     //std::cout<<j<<endl;
//     if(j > m - 1)
//     {
//       L = L - 2* X.row(j).t()*X.row(j) +
//         X.row(j + m).t()* X.row(j + m) +
//         X.row(j - m).t()*X.row(j - m);
//       L1 = L1 - 2* y(j) * X.row(j).t() +
//         y(j + m) * X.row(j + m).t() +
//         y(j - m) * X.row(j - m).t();
//     }
//     // std::cout<<arma::size(all_delta2.slice(j))<<endl;
//     // std::cout<<arma::size(L * L.t())<<endl;
//     all_delta2.slice(j) = L * L1 / (2 * m);
//   }
//   //std::cout<<all_delta2[m-1]<<endl;
//   for(int j = 0; j < m - 1; j++) all_delta2.slice(j).zeros();
//   for(int j = n - m; j < n; j++) all_delta2.slice(j).zeros();
//
//   for(int i = m - 1 ; i < n - m; i++)
//   {
//     sigma.slice(i).zeros();//t=(i+1.0)/n
//     regular_factor = 0;
//     for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
//     {//all_delta2[j] is \Delta_j^2
//       if(j > 0 && j < n){
//         sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
//         regular_factor += kernel_vector[abs(j - i)];
//       }
//     }
//     sigma.slice(i) = sigma.slice(i)/regular_factor;
//     // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
//   }
//
//   for(int i = 0; i < m -1; i++) sigma.slice(i) = sigma.slice(m - 1);
//   for(int i = n - m ; i < n; i++) sigma.slice(i) = sigma.slice(n - m - 1);
//
//   return(sigma);//Sigma2
// }


// [[Rcpp::export]]
arma::cube DiffX(arma::mat X, int m, double tau_n = 0, int ind = 2)
{
  int n = X.n_rows;
  int p = X.n_cols;
  double regular_factor;
  arma::cube all_delta2(p, p, n), sigma(p, p, n);
  arma::vec kernel_vector(n);
  arma::mat L(p,p), tmp(p, p), tmp1(p,p);
  vec eigval;
  mat eigvec;

  if(!tau_n) tau_n = powf(n,-5.0/29);
  kernel_vector = Compute_kernel_vector(n, tau_n, 0, ind);

  L.zeros();

  for(int j = 0; j < m; j++)
  { //e=y
    tmp  =  X.row(j).t() *  X.row(j) - X.row(j + m).t() *  X.row(j + m);
    L = L +  tmp*tmp;
  }
  for(int j = m - 1  ; j < n - m ; j++)//j needs  conversion starts from 0
  {
    //std::cout<<j<<endl;
    if(j > m - 1)
    {
      tmp  = X.row(j).t() *  X.row(j) - X.row(j + m).t() *  X.row(j + m);
      tmp1  = X.row(j - m).t() *  X.row(j - m) - X.row(j).t() *  X.row(j);
      L = L + tmp * tmp - tmp1 * tmp1;
    }
    // std::cout<<arma::size(all_delta2.slice(j))<<endl;
    // std::cout<<arma::size(L * L.t())<<endl;
    all_delta2.slice(j) = L / 2;
  }
  //std::cout<<all_delta2[m-1]<<endl;
  for(int j = 0; j < m - 1; j++) all_delta2.slice(j).zeros();
  for(int j = n - m; j < n; j++) all_delta2.slice(j).zeros();

  for(int i = m - 1 ; i < n - m; i++)
  {
    sigma.slice(i).zeros();//t=(i+1.0)/n
    regular_factor = 0;
    for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
    {//all_delta2[j] is \Delta_j^2
      if(j > 0 && j < n){
        sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
        regular_factor += kernel_vector[abs(j - i)];
      }
    }
    sigma.slice(i) = sigma.slice(i)/regular_factor;
    // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
  }

  for(int i = 0; i < m -1; i++) sigma.slice(i) = sigma.slice(m - 1);
  for(int i = n - m ; i < n; i++) sigma.slice(i) = sigma.slice(n - m - 1);

  return(sigma);//Sigma2
}

// [[Rcpp::export]]
arma::cube DiffA(arma::vec y, arma::mat X, int m, double tau_n = 0, int ind = 2){
  int n = X.n_rows;
  int p = X.n_cols;
  double regular_factor;
  arma::cube all_delta2(p, 1, n), sigma(p, 1, n);
  arma::vec kernel_vector(n), tmpL(p),tmpL1(p), L(p);
  arma::mat tmp(p,p), tmp1(p,p);
  vec eigval;
  mat eigvec;

  if(!tau_n) tau_n = powf(n,-5.0/29);
  kernel_vector = Compute_kernel_vector(n, tau_n, 0, ind);

  L.zeros();

  for(int j = 0; j < m; j++)
  { //e=y
    tmp = X.row(j + m).t() *  X.row(j + m) - X.row(j).t() *  X.row(j);
    tmpL = y(j + m) * X.row(j + m).t()- y(j) * X.row(j).t();
    L = L +  tmp*tmpL;
  }
  for(int j = m - 1  ; j < n - m ; j++)//j needs  conversion starts from 0
  {
    //std::cout<<j<<endl;
    if(j > m - 1)
    {
      tmp = X.row(j + m).t() *  X.row(j + m) - X.row(j).t() *  X.row(j);
      tmpL = y(j + m) * X.row(j + m).t()- y(j) * X.row(j).t();
      tmp1 = X.row(j).t() *  X.row(j) - X.row(j-m).t() *  X.row(j-m);
      tmpL1 = y(j) * X.row(j).t()- y(j-m) * X.row(j-m).t();
      L = L  + tmp*tmpL - tmp1*tmpL1;
    }
    // std::cout<<arma::size(all_delta2.slice(j))<<endl;
    // std::cout<<arma::size(L * L.t())<<endl;
    all_delta2.slice(j) = L / 2;
  }
  //std::cout<<all_delta2[m-1]<<endl;
  for(int j = 0; j < m - 1; j++) all_delta2.slice(j).zeros();
  for(int j = n - m; j < n; j++) all_delta2.slice(j).zeros();

  for(int i = m - 1 ; i < n - m; i++)
  {
    sigma.slice(i).zeros();//t=(i+1.0)/n
    regular_factor = 0;
    for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
    {//all_delta2[j] is \Delta_j^2
      if(j > 0 && j < n){
        sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
        regular_factor += kernel_vector[abs(j - i)];
      }
    }
    sigma.slice(i) = sigma.slice(i)/regular_factor;
    // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
  }

  for(int i = 0; i < m -1; i++) sigma.slice(i) = sigma.slice(m - 1);
  for(int i = n - m ; i < n; i++) sigma.slice(i) = sigma.slice(n - m - 1);

  return(sigma);//Sigma2
}


//
// // [[Rcpp::export]]
// arma::cube DiffAn(arma::vec y, arma::mat X, int m, double tau_n = 0, int ind = 2)
// {
//   int n = X.n_rows;
//   int p = X.n_cols;
//   double regular_factor;
//   arma::cube all_delta2(p, 1, n);
//   arma::cube sigma(p, 1, n);
//   arma::vec tmpL(p),tmpL1(p), L(p);
//   arma::mat tmp(p,p), tmp1(p,p);
//   vec eigval;
//   mat eigvec;
//   // arma::vec kernel_vector(n);
//
//   if(!tau_n) tau_n = powf(n,-5/29);
//   // kernel_vector = Compute_kernel_vector(n, tau_n, 0, ind);
//
//   L.zeros();
//
//   for(int j = 0; j < m; j++)
//   { //e=y
//     tmp = X.row(j + m).t() *  X.row(j + m) - X.row(j).t() *  X.row(j);
//     tmpL = y(j + m) * X.row(j + m).t()- y(j) * X.row(j).t();
//     L = L +  tmp*tmpL;
//   }
//   for(int j = m - 1  ; j < n - m ; j++)//j needs  conversion starts from 0
//   {
//     //std::cout<<j<<endl;
//     if(j > m - 1){
//       tmp = X.row(j + m).t() *  X.row(j + m) - X.row(j).t() *  X.row(j);
//       tmpL = y(j + m) * X.row(j + m).t() - y(j) * X.row(j).t();
//       tmp1 = X.row(j).t() *  X.row(j) - X.row(j-m).t() *  X.row(j-m);
//       tmpL1 = y(j) * X.row(j).t() - y(j-m) * X.row(j-m).t();
//       L = L  + tmp*tmpL - tmp1*tmpL1;
//     }
//     // std::cout<<arma::size(all_delta2.slice(j))<<endl;
//     // std::cout<<arma::size(L * L.t())<<endl;
//     all_delta2.slice(j) = L / 2;
//   }
//   //std::cout<<all_delta2[m-1]<<endl;
//   for(int j = 0; j < m - 1; j++) all_delta2.slice(j) = all_delta2.slice(m-1);
//   for(int j = n - m; j < n; j++) all_delta2.slice(j) = all_delta2.slice(n - m -1);
//   // sigma.zeros();//t=(i+1.0)/n
//   for(int i = m - 1 ; i < n - m; i++){
//     regular_factor = 2*m+1;
//     for(int j = i - m; j <= i + m; j++)//j needs no conversion starts from m
//     {//all_delta2[j] is \Delta_j^2
//       if(j > 0 && j < n){
//         // sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
//         sigma.slice(i) += all_delta2.slice(j);
//         // regular_factor += kernel_vector[abs(j - i)];
//       }
//     }
//     sigma.slice(i) = sigma.slice(i)/regular_factor;
//     // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
//     // sigma.slice(i) = all_delta2.slice(i);
//   }
//
//   for(int i = 0; i < m -1; i++) sigma.slice(i) = sigma.slice(m - 1);
//   for(int i = n - m ; i < n; i++) sigma.slice(i) = sigma.slice(n - m - 1);
//
//   return(all_delta2);
// }
//' @export
//' @name Heter_LRV
//' @title Long-run covariance matrix estimators
//' @description \loadmathjax The function provides a wide range of estimators for the long-run covariance matrix estimation in non-stationary time series with covariates.
//' @param e, vector, if the plug-in estimator is used, e should be the vector of residuals, OLS or nonparametric ones. If the difference-based debiased method is adopted, e should be the response time series, i.e., \mjseqn{y}. Specially, e should also be the response time series, i.e., \mjseqn{y}, if the plug-in estimator using the \mjseqn{\breve{\beta}}, the pilot estimator proposed in Bai and Wu (2023).
//' @param X, a matrix \mjseqn{n\times p}
//' @param m, integer, the window size.
//' @param tau_n, double, the smoothing parameter in the estimator. If tau_n is 0, a rule-of-thumb value will be automatically used.
//' @param lrv_method, the method of long-run variance estimation, lrvmethod = 0 uses the plug-in estimator in Zhou (2010), lrvmethod = 1 offers the debias difference-based estimator in Bai and Wu (2023), lrvmethod = 2 provides the plug-in estimator using the \mjseqn{\breve{\beta}}, the pilot estimator proposed in Bai and Wu (2023)
//' @param ind,  types of kernels
//' @param ncp,  1 no change points, 0 possible change points
//'* 1 Triangular \mjseqn{1-|u|}, \mjseqn{u \le 1}
//'* 2 Epanechnikov kernel \mjseqn{3/4(1-u^{2})}, \mjseqn{u \le 1}
//'* 3 Quartic \mjseqn{15/16(1-u^{2})^{2}}, \mjseqn{u \le 1}
//'* 4 Triweight \mjseqn{35/32(1-u^{2})^{3}}, \mjseqn{u \le 1}
//'* 5 Tricube  \mjseqn{70/81(1-|u|^{3})^{3}}, \mjseqn{u \le 1}
//' @param print_deg, bool, whether to print information of non-positiveness, default 0\mjseqn{n\times p}
//' @param rescale, bool, whether to use rescaling to correct the negative eigenvalues, default 0
//' @return a cube. The time-varying long-run covariance matrix \mjseqn{p \times p \times n}, where \eqn{p} is the dimension of the time series vector, and \eqn{n} is the sample size.
//' @examples
//' param = list(d = -0.2, heter = 2, tvd = 0,
//' tw = 0.8, rate = 0.1, cur = 1, center = 0.3,
//' ma_rate =  0, cov_tw =  0.2, cov_rate = 0.1,
//' cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
//' data = Qct_reg(1000, param)
//' sigma = Heter_LRV(data$y, data$x, 3, 0.3, lrv_method = 1)
//' @references
//' Bai, L., & Wu, W. (2023). Difference-based covariance matrix estimate in time series nonparametric regression with applications to specification tests.
//'
//' Zhou, Z. and Wu, W. B. (2010). Simultaneous inference of linear models with time varying coefficients.J. R. Stat. Soc. Ser. B. Stat. Methodol., 72(4):513–531.
// [[Rcpp::export]]
arma::cube Heter_LRV(arma::vec e, arma::mat X, int m, double tau_n = 0,
                     int lrv_method = 1, int ind = 2, bool print_deg = 0, bool rescale = 0, bool ncp = 0){
  int n = X.n_rows;
  int p = X.n_cols;
  double regular_factor = 1;
  double rescale_factor = 1;
  double tau1;
  int count_deg = 0;
  arma::cube sigma(p, p, n);
  vec eigval;
  mat eigvec;

  if(!tau_n) tau_n = powf(n,-2/15.0);
  if(ncp){
    tau1 = tau_n;
  }else{
    tau1 = powf(tau_n, 3.0/2);
  }


  if(lrv_method == 4){// difference-based with no correction
    // e is y here
    sigma = Diff1(e, X, m, tau_n, ind);

    for(int i = 0; i < n; i++){
      eig_sym(eigval, eigvec, sigma.slice(i));
      uvec zero = find(eigval < 1.0/n);
      eigval.elem(zero).fill(1.0/n);
      sigma.slice(i) = eigvec * diagmat(sqrt(eigval))* eigvec.t();
    }
  }else if(lrv_method == 3){// correction use local linear estimation beta_{\tau}

    arma::cube Ajmhat(p,p,n), Ajmc(p, p, n), Ajmb(p,1,n);
    vec xb(n), beta(p);
    vec t = linspace(0, 1, n);
    List result;
    mat beta0(n,p);
    // e is y here
    sigma = Diff1(e, X, m, tau1, ind);
    if(p > 1){
      Ajmc = DiffX(X, m, tau_n, ind);
      Ajmb = DiffA(e, X, m, tau_n, ind);
      beta0 = LocLinear_B(tau_n, t, e, X);
      for(int i = 0; i < n; i++){
        beta = beta0.row(i).t();
        xb(i) = dot(X.row(i), beta);
      }
      Ajmhat = Diff1(xb, X, m, tau1, ind);
      sigma = sigma - Ajmhat;
    }

    for(int i = 0; i < n; i++){
      eig_sym(eigval, eigvec, sigma.slice(i));
      uvec zero = find(eigval < 1.0/n);
      eigval.elem(zero).fill(1.0/n);
      sigma.slice(i) = eigvec * diagmat(sqrt(eigval))* eigvec.t();
    }

  }else if(lrv_method == 2){// plugging-in breve beta
  arma::cube Ajmhat(p,p,n), Ajmc(p, p, n), Ajmb(p,1,n);
  vec xb(n), beta(p), res(n);
  // e is y here
  Ajmc = DiffX(X, m , tau1, ind);
  Ajmb = DiffA(e, X, m, tau1, ind);
  for(int i = 0; i < n; i++){
    beta = inv(Ajmc.slice(i)) * Ajmb.slice(i);
    xb(i) = dot(X.row(i), beta);
  }
  res = e - xb;
  sigma = Heter_LRV(res, X, m, tau_n, 0, ind, 0, rescale);
  return(sigma);

}else if(lrv_method == 1){// difference-based debias

    arma::cube Ajmhat(p,p,n), Ajmc(p, p, n), Ajmb(p,1,n);
    vec xb(n),beta(p);
    // e is y here
    sigma = Diff1(e, X, m, tau_n, ind);
    if(p > 1){
      Ajmc = DiffX(X, m , tau1, ind);
      Ajmb = DiffA(e, X, m, tau1, ind);
      for(int i = 0; i < n; i++){
        beta = inv(Ajmc.slice(i)) * Ajmb.slice(i);
        xb(i) = dot(X.row(i), beta);
      }
      Ajmhat = Diff1(xb, X, m, tau_n, ind);
      sigma = sigma - Ajmhat;
    }

    for(int i = 0; i < n; i++){
      eig_sym(eigval, eigvec, sigma.slice(i));
      uvec zero = find(eigval < 1.0/n);
      if(rescale)
        rescale_factor = mean(eigval);
      eigval.elem(zero).fill(1.0/n);
      if(print_deg && zero.size() > 0)
        count_deg += 1;
      if(rescale){
        rescale_factor /= mean(eigval);
      }
      else rescale_factor = 1;
      sigma.slice(i) = eigvec * diagmat(sqrt(rescale_factor*eigval))* eigvec.t();
    }


  }else if(lrv_method == 0){ // plug-in

    arma::vec L(p), kernel_vector(n);
    kernel_vector = Compute_kernel_vector(n, tau_n, 0, ind);
    L.zeros();
    arma::cube all_delta2(p, p, n);

    for(int j = 0; j < 2*m + 1; j++){
      L = L + e(j)*X.row(j).t();
    }
    for(int j = m ; j < n - m ; j++)//j needs  conversion starts from 0
    {//all_delta2[j] is m\Delta_j^2/2
      if(j > m)
      {
        L = L - e(j - m - 1) * X.row(j - m - 1).t() + e(j + m) * X.row(j + m).t();
      }
      all_delta2.slice(j) = L * L.t() / (2 * m + 1);
    }
    for(int j = 0; j < m; j++) all_delta2.slice(j).zeros();
    for(int j = n - m; j < n; j++) all_delta2.slice(j).zeros();

    for(int i = m; i < n - m; i++)
    {
      sigma.slice(i).zeros();//t=(i+1.0)/n
      regular_factor = 0;
      for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
      {//all_delta2[j] is \Delta_j^2
        if(j > 0 && j < n){
          sigma.slice(i)+= all_delta2.slice(j) * kernel_vector[abs(j - i)];
          regular_factor += kernel_vector[abs(j - i)];
        }
      }
      // sigma.slice(i) = chol(sigma.slice(i)/regular_factor);
      // sigma.slice(i) = sqrtmat_sympd(sigma.slice(i)/regular_factor);
      eig_sym(eigval, eigvec, sigma.slice(i)/regular_factor);
      sigma.slice(i) = eigvec * diagmat(sqrt(eigval))* eigvec.t();
    }

    for(int  i = 0; i < m; i++) sigma.slice(i) = sigma.slice(m);
    for(int i = n - m; i < n; i++) sigma.slice(i)= sigma.slice(n - m - 1);
  }
  if(print_deg){
    Rcout << count_deg << std::endl;
  }
  return(sigma);//Sigma2
}

// [[Rcpp::export]]
arma::vec sim_Phi_heter(List data,
                         int B,
                         arma::cube sigma, arma::cube R)
{//bootstrap kpss test statistics distribution
  int n = data["n"];
  int p = data["p"];
  double bw = data["bw"];
  arma::mat X_reg = data["X_reg"];
  arma::mat mu_reg = data["mu_reg"];
  arma::mat X = data["X"];

  int m = 0;
   m = int(n*bw);
  //int lb,ub;
  arma::mat s2(B,1), K_bias(1, B),c(1, p), tmp(1,B);
  arma::vec KPSS_dist(B);
  arma::vec kernel_vector(n);
  kernel_vector = Compute_kernel_vector(n, bw, 1);

  s2.zeros();
  KPSS_dist.zeros();

  for(int r = m ; r < n - m ; r ++ )//simluate \hat{e}_r
  {
    K_bias.zeros() ;
    for(int i = 0 ; i < n; i++){
      if(abs(i - r) < n*bw){//in the local region
        // if(limiting == 1){
        //   c = kernel_vector[abs(i-r)]/(n*bw)* sigma.slice(i).row(0);
        // }else if(limiting ==2) {
        //   c = (mu_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);
        //   // std::cout<< "c"<<endl;
        // }else{
          c = (X_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);//1*p
        // }
        K_bias = K_bias +  c * R.slice(i);//1*B
      }
    }
    //tmp = sigma.slice(r).row(0) * R.slice(r);//1*B
    tmp = sigma.slice(r)(0,0) * R.slice(r).row(0);//1*B
    s2 = s2 + tmp.t() - K_bias.t(); ;//B*1
    KPSS_dist = KPSS_dist + s2 % s2;
  }
  KPSS_dist = KPSS_dist/(n*(n - 2*m));
  return(KPSS_dist);
};


// [[Rcpp::export]]
arma::vec sim_Phi_heter_RS(List data,
                        int B,
                        arma::cube sigma, arma::cube R)
{//bootstrap kpss test statistics distribution
  int n = data["n"];
  int p = data["p"];
  double bw = data["bw"];
  arma::mat X_reg = data["X_reg"];
  arma::mat mu_reg = data["mu_reg"];
  arma::mat X = data["X"];

  int m = 0;
  m = int(n*bw);
  //int lb,ub;
  arma::mat s2(B,1), K_bias(1, B),c(1, p), tmp(1,B), maxs(B,1), mins(B,1);
  arma::vec RS_dist(B);
  arma::vec kernel_vector(n);
  kernel_vector = Compute_kernel_vector(n, bw, 1);

  s2.zeros();
  RS_dist.zeros();

  for(int r = m ; r < n - m ; r ++ )//simluate \hat{e}_r
  {
    K_bias.zeros() ;
    for(int i = 0 ; i < n; i++){
      if(abs(i - r) < n*bw){//in the local region
        // if(limiting == 1){
        //   c = kernel_vector[abs(i-r)]/(n*bw)* sigma.slice(i).row(0);
        // }else if(limiting ==2) {
        //   c = (mu_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);
        //   // std::cout<< "c"<<endl;
        // }else{
          c = (X_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);//1*p
        // }
        K_bias = K_bias +  c * R.slice(i);//1*B
      }
    }
    //tmp = sigma.slice(r).row(0) * R.slice(r);//1*B
    tmp = sigma.slice(r)(0,0) * R.slice(r).row(0);//1*B
    s2 = s2 + tmp.t() - K_bias.t(); ;//B*1
    if(r == m){
      maxs = s2;
      mins = s2;
    }else{
      maxs = (s2 > maxs)%s2 + (s2 <= maxs)%maxs;
      mins = (s2 < mins)%s2 + (s2 >= mins)%mins;
    }
  }
  RS_dist = maxs - mins;
  return(RS_dist);
};


// [[Rcpp::export]]
arma::vec sim_Phi_heter_KS(List data,
                           int B,
                           arma::cube sigma, arma::cube R)
{//bootstrap kpss test statistics distribution
  int n = data["n"];
  int p = data["p"];
  double bw = data["bw"];
  arma::mat X_reg = data["X_reg"];
  arma::mat mu_reg = data["mu_reg"];
  arma::mat X = data["X"];

  int m = 0;
  m = int(n*bw);
  //int lb,ub;
  arma::mat s2(B,1), K_bias(1, B),c(1, p), tmp(1,B), maxs(B,1);
  arma::vec KS_dist(B);
  arma::vec kernel_vector(n);
  kernel_vector = Compute_kernel_vector(n, bw, 1);

  s2.zeros();
  KS_dist.zeros();

  for(int r = m ; r < n - m ; r ++ )//simluate \hat{e}_r
  {
    K_bias.zeros() ;
    for(int i = 0 ; i < n; i++){
      if(abs(i - r) < n*bw){//in the local region
        // if(limiting == 1){
        //   c = kernel_vector[abs(i-r)]/(n*bw)* sigma.slice(i).row(0);
        // }else if(limiting ==2) {
        //   c = (mu_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);
        //   // std::cout<< "c"<<endl;
        // }else{
          c = (X_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);//1*p
        // }
        K_bias = K_bias +  c * R.slice(i);//1*B
      }
    }
    //tmp = sigma.slice(r).row(0) * R.slice(r);//1*B
    tmp = sigma.slice(r)(0,0) * R.slice(r).row(0);//1*B
    s2 = s2 + tmp.t() - K_bias.t(); ;//B*1
    if(r == m){
      maxs = abs(s2);
    }else{
      maxs = (abs(s2) > maxs)%abs(s2) + (abs(s2) <= maxs)%maxs;
    }
  }
  KS_dist = maxs ;
  return(KS_dist);
};

// [[Rcpp::export]]
arma::vec sim_Phi_heter_VS(List data,
                        int B,
                        arma::cube sigma, arma::cube R){
  //bootstrap kpss test statistics distribution
  int n = data["n"];
  int p = data["p"];
  double bw = data["bw"];
  arma::mat X_reg = data["X_reg"];
  arma::mat mu_reg = data["mu_reg"];
  arma::mat X = data["X"];

  int m = 0;
  m = int(n*bw);
  //int lb,ub;
  arma::mat s2(B,1), K_bias(1, B),c(1, p), tmp(1,B), vs(B,1);
  arma::vec VS_dist(B);
  arma::vec kernel_vector(n);
  kernel_vector = Compute_kernel_vector(n, bw, 1);

  s2.zeros();
  VS_dist.zeros();
  vs.zeros();

  for(int r = m ; r < n - m ; r ++ )//simluate \hat{e}_r
  {
    K_bias.zeros() ;
    for(int i = 0 ; i < n; i++){
      if(abs(i - r) < n*bw){//in the local region
        // if(limiting == 1){
        //   c = kernel_vector[abs(i-r)]/(n*bw)* sigma.slice(i).row(0);
        // }else if(limiting ==2) {
        //   c = (mu_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);
        //   // std::cout<< "c"<<endl;
        // }else{
          c = (X_reg.row(r) * kernel_vector[abs(i-r)])/(n*bw) * sigma.slice(i);//1*p
        // }
        K_bias = K_bias +  c * R.slice(i);//1*B
      }
    }
    //tmp = sigma.slice(r).row(0) * R.slice(r);//1*B
    tmp = sigma.slice(r)(0,0) * R.slice(r).row(0);//1*B
    s2 = s2 + tmp.t() - K_bias.t(); ;//B*1
    vs = vs + s2;
    VS_dist = VS_dist + s2 % s2;
  }
  VS_dist = (VS_dist-  vs%vs/(n - 2*m))/(n*(n - 2*m)) ; //modified KPSS to  VS
  return(VS_dist);
};


//' @export
//' @name gcv_cov
//' @title Generalized Cross Validation
//' @description Given a bandwidth, compute its corresponding GCV value \loadmathjax
//' @param bw double, bandwidth
//' @param t vector, scaled time \mjseqn{[0,1]}
//' @param y vector, response
//' @param X matrix, covariates matrix
//' @param verbose bool, whether to print the  numerator and denominator in GCV value
//' @return GCV value
//' @examples
//' param = list(d = -0.2, heter = 2, tvd = 0,
//'  tw = 0.8, rate = 0.1, cur = 1, center = 0.3,
//'   ma_rate =  0, cov_tw =  0.2, cov_rate = 0.1,
//'    cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
//' data = Qct_reg(1000, param)
//' value <- gcv_cov(0.2, (1:1000)/1000, data$y, data$x)
//' @details
//' Generalized cross validation value is defined as
//' \mjsdeqn{n^{-1}| Y-\hat{Y}|^2/[1- \mathrm{tr}(Q(b)) / n]^2}
//' When computing \mjseqn{\mathrm{tr}(Q(b))},
//' we use the fact that the first derivative of coefficient function is zero at central point
//' The ith diagonal value of \mjseqn{Q(b)} is actually \mjseqn{x^T(t_i)S^{-1}_{n}x(t_i)}
//' where \mjseqn{S^{-1}_{n}} means the top left p-dimension square matrix of
//'  \mjseqn{S_{n}(t_i) = X^T W(t_i) X}, \mjseqn{W(t_i)} is the kernel weighted matrix. Details on
//' the computation of \mjseqn{S_n} could be found in \code{LocLinear} and its reference
// [[Rcpp::export]]
double gcv_cov (double bw,
                arma::vec t,arma::vec y,arma::mat X,
                bool verbose = 1)
{
  int n,p;
  p = X.n_cols;
  n = X.n_rows;

  arma::mat beta0(n,p), beta,tmpl(p,p), tmpr(p,p), M(p,p), tmp(1,1);
  arma::mat s0(p,p), s1(p,p), s2(p,p), t0(p,1), t1(p,1), tmpx(p,p);
  arma:: mat S(2*p, 2*p), R(2*p,1), X_reg(n,p), inv_S(2*p, 2*p);
  double auxK, aux,mu;
  double trace_Qb = 0, res = 0, GCV;

  for(int i = 0; i < n; i++)
  {
    s0.zeros();s1.zeros();s2.zeros();
    t0.zeros();t1.zeros();

    for(int j  = i - n * bw; j <= i + n * bw; j++)
    {
      if(j >= 0 && j < n)
      {
        aux = (t[j] - t[i]) / bw;
        auxK = EpaK(aux)/ (n*bw);
        // std::cout<<auxK<<endl;
        // (X.row(j).t()*y[j]* auxK).print();
        t0 += X.row(j).t()*y[j] * auxK;
        t1 += X.row(j).t()*y[j] * auxK * aux;
        tmpx = X.row(j).t()*X.row(j);
        s0 += tmpx * auxK;
        s1 += tmpx * auxK * aux;
        s2 += tmpx * auxK * aux * aux;
      }
    }
    S = join_cols(join_rows(s0, s1), join_rows(s1, s2));
    R = join_cols(t0, t1);
    inv_S = inv(S);
    tmp = X.row(i) * inv_S.submat(0, 0, p - 1, p - 1) * X.row(i).t();//type error
    trace_Qb += tmp(0,0) * 0.75/(n*bw); //0.75 is for Epak at center
    // Qb =  X * inv_S.submat(0, 0, p - 1, p - 1) * X.t();
    //cout<<tmp(0,0) <<endl;
    beta = inv_S * R;
    tmp = X.row(i) * beta.rows(0, p - 1);//type error
    mu = tmp(0, 0);
    res += (y[i] - mu) * (y[i] - mu);

  }
  if(verbose)
  {
    Rcout << "res: " << res << endl;
    Rcout << "1-trace_Qb/Tn: " << (1 - trace_Qb/n) << endl;

  }
  GCV = (res / n)/((1 - trace_Qb / n) * (1 - trace_Qb / n));
  return(GCV);

}


//' @export
//' @name MV_critical
//' @title Statistics-adapted values for extended minimum volatility selection.
//' @description  Calculation of the variance of the bootstrap statistics for the extended minimum volatility selection.
//' @param y, vector, as used in the Heter_LRV
//' @param data, list, a list of data
//' @param R, a cube of standard.normal random variables.
//' @param gridm, vector, a grid of candidate m's.
//' @param gridtau, vector, a grid of candidate tau's.
//' @param type, integer, 1 KPSS 2 RS 3 VS 4 KS
//' @param cvalue, double, 1-quantile for the calculation of bootstrap variance, default 0.1.
//' @param B, integer, number of iterations for the calculation of bootstrap variance
//' @param lrvmethod, integer, see also Heter_LRV
//' @param ind, integer, the type of kernel,  see also Heter_LRV
//' @param rescale, bool, whether to rescale when positiveness of the matrix is not obtained. default 0
//' @seealso Heter_LRV
//' @return a matrix of critical values
//' @examples
//' ###with Long memory parameter 0.2
//' param = list(d = -0.2, heter = 2,
//'  tvd = 0, tw = 0.8, rate = 0.1, cur = 1,
//'   center = 0.3, ma_rate =  0, cov_tw =  0.2,
//'   cov_rate = 0.1, cov_center = 0.1,
//'   all_tw  = 1, cov_trend = 0.7)
//' n = 1000
//' data = Qct_reg(n, param)
//' p = ncol(data$x)
//' t = (1:n)/n
//' B_c = 100 ##small value for testing
//' Rc = array(rnorm(n*p*B_c),dim = c(p,B_c,n))
//' result1 = LocLinear(0.2, t, data$y, data$x)
//' critical <- MV_critical(data$y, result1, Rc, c(3,4,5), c(0.2, 0.25, 0.3))
//' @references #' Bai, L., and Wu, W. (2023). Detecting long-range dependence for time-varying linear models. To appear in Bernoulli
// [[Rcpp::export]]
arma::mat MV_critical(arma::vec y, List data, arma::cube R,
                      arma::vec gridm,
                      arma::vec  gridtau,
                      int type = 1,//1 KPSS 2 RS 3 VS 4 KS
                      double cvalue = 0.1,
                      int B = 100,
                      int lrvmethod = 1, int ind = 2, bool rescale = 0){

  arma::mat X = data["X"];
  int n = X.n_rows;
  int p = X.n_cols;
  int M = gridm.n_elem;
  int Tau = gridtau.n_elem;
  // double c;
  arma::mat critical(M, Tau);
  arma::cube sigma(p, p, n);
  arma::vec bootstrap(B);
  // int P = B*(1-cvalue);
  double Q;

  for(int i = 0; i < M; i++){
    for(int j = 0; j < Tau; j++){
      //base on
      // std::cout<<"sigma"<<endl;
      sigma = Heter_LRV(y, data["X"], gridm[i], gridtau[j], lrvmethod, ind, 0, rescale);
      if(type == 1){
        bootstrap = sim_Phi_heter(data,B,sigma,R);
        // c = 1.5;
      }else if(type == 2){
        // c = 0;
        bootstrap = sim_Phi_heter_RS(data,B,sigma,R);
      }else if(type == 3){
        // c = 1.5;
        bootstrap = sim_Phi_heter_VS(data,B,sigma,R);
      }else if(type == 4){
        // c = 0;
        bootstrap = sim_Phi_heter_KS(data,B,sigma,R);
      }else Rcout<<"wrong type!"<<endl;
      // bootstrap = arma::sort(bootstrap);
      // Q = bootstrap(P);
      Q = var(bootstrap);
      // Q = var(bootstrap) + mean(bootstrap)*mean(bootstrap)*c;
      // std::cout<<i<<","<<j << "Q" <<Q<<endl;
      critical(i,j) = Q;
    }
  }
  return(critical);
}



//' @export
//' @name MV_ise_heter_critical
//' @title MV method
//' @description Selection of smoothing parameters for bootstrap tests by choosing the index minimizing the volatility of bootstrap statistics or long-run variance estimators in the neighborhood computed before.
//' @param critical, a matrix of critical values
//' @param neighbour, integer, number of neighbours
//' @return a list of results,
//' \itemize{
//' \item minp: optimal row number
//' \item minq: optimal column number
//' \item min_ise: optimal value
//' }
//' @examples
//' param = list(d = -0.2, heter = 2,
//'  tvd = 0, tw = 0.8, rate = 0.1,
//'  cur = 1, center = 0.3, ma_rate =  0,
//'  cov_tw =  0.2, cov_rate = 0.1,
//'  cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
//' n = 1000
//' data = Qct_reg(n, param)
//' p = ncol(data$x)
//' t = (1:n)/n
//' B_c = 100 ##small value for testing
//' Rc = array(rnorm(n*p*B_c),dim = c(p,B_c,n))
//' result1 = LocLinear(0.2, t, data$y, data$x)
//' gridm = c(3,4,5)
//' gridtau = c(0.2, 0.25, 0.3)
//' critical <- MV_critical(data$y, result1, Rc, gridm, gridtau)
//' mv_result = MV_ise_heter_critical(critical,  1)
//' m = gridm[mv_result$minp + 1]
//' tau_n = gridtau[mv_result$minq + 1]
//' @references  Bai, L., and Wu, W. (2023). Detecting long-range dependence for time-varying linear models. To appear in Bernoulli
// [[Rcpp::export]]
List MV_ise_heter_critical(arma::mat critical, int neighbour)
{
  int M = critical.n_rows;
  int Tau = critical.n_cols;
  List result;
  double tmp;
  int pl,pu,ql,qu;
  int minp, minq;
  double min_ise = R_PosInf;

  for(int p = neighbour; p < M - neighbour; p++){
    for(int q = 0; q < Tau; q++){

      if(p - neighbour < 0) pl = 0;
      else pl = p - neighbour;
      if(p + neighbour >= M) pu = M - 1;
      else pu = p + neighbour;
      if(q - neighbour < 0) ql = 0;
      else ql = q - neighbour;
      if(q + neighbour >= Tau) qu = Tau - 1;
      else qu = q + neighbour;
      // std::cout<<"p"<<pl<<pu<<"q"<<ql<<qu<<endl;
      // lrv_cub.slice(0).submat(pl,ql,pu,qu).print();

      if(qu >= q +1 && ql <= q-1) {
        tmp = stddev(join_cols(join_cols(vectorise(critical.submat(pl,q,pu,q)),
                                          vectorise(critical.submat(p,ql,p,q-1))),
                                          vectorise(critical.submat(p,q+1,p,qu))));
      }
      else if(ql <= q-1){
        tmp = stddev(join_cols(vectorise(critical.submat(pl,q,pu,q)),
                                 vectorise(critical.submat(p,ql,p,q-1))));
      }else
        tmp = stddev(join_cols(vectorise(critical.submat(pl,q,pu,q)),
                                 vectorise(critical.submat(p,q+1,p,qu))));
      // lrv_cub.print();
      //std::cout<<"p"<<p<<"q"<<q<<"ise"<<tmp<<endl;
      if(tmp < min_ise){
        min_ise = tmp;
        minp = p;
        minq = q;
      }
    }
  }

  result["minp"] = minp;
  result["minq"] = minq;
  result["min_ise"] = min_ise;
  return(result);
}


double Mat_std(arma::vec lrv_vec, int p)
{
  int l = lrv_vec.n_elem;
  int npar = l/(p*p);
  double result = 0;
  arma::vec tmp(p * p);

  tmp.zeros();
  for(int i = 0; i < npar; i++)
  {
    for(int j = 0; j< p*p; j++)
    {
      result += lrv_vec(i * p * p + j) * lrv_vec(i * p * p + j);
      tmp(j) += lrv_vec(i * p * p + j);
    }
  }
  result = result - sum(tmp % tmp)/npar;
  result = sqrt(result/(npar-1));
  return(result);
}



// [[Rcpp::export]]
List MV_ise_heter(arma::cube lrv_cub, int dim, int n,int neighbour)
{
  int M = lrv_cub.n_rows;
  int Tau = lrv_cub.n_cols;
  List result;
  double s, tmp, tmps;
  int pl,pu,ql,qu;
  int minp, minq;
  double min_ise = R_PosInf;

  for(int p = neighbour; p < M - neighbour; p++){
    for(int q = 0; q < Tau; q++){
      s = 0;
      if(p - neighbour < 0) pl = 0;
      else pl = p - neighbour;
      if(p + neighbour >= M) pu = M - 1;
      else pu = p + neighbour;
      if(q - neighbour < 0) ql = 0;
      else ql = q - neighbour;
      if(q + neighbour >= Tau) qu = Tau - 1;
      else qu = q + neighbour;
      // std::cout<<"p"<<pl<<pu<<"q"<<ql<<qu<<endl;
      // lrv_cub.slice(0).submat(pl,ql,pu,qu).print();
      for(int r = 0; r < n; r++){
        if(qu >= q +1 && ql <= q-1)
        {
          tmps = Mat_std(join_cols(join_cols(vectorise(lrv_cub.subcube(pl,q,r*dim*dim,pu,q,(r+1)*dim*dim-1)),
                                             vectorise(lrv_cub.subcube(p,ql,r*dim*dim,p,q-1,(r+1)*dim*dim-1))),
                                             vectorise(lrv_cub.subcube(p,q + 1,r*dim*dim,p,qu,(r+1)*dim*dim-1))),dim);
        }
        else if(ql <= q-1)
        {
          tmps = Mat_std(join_cols(vectorise(lrv_cub.subcube(pl,q,r*dim*dim,pu,q,(r+1)*dim*dim-1)),
                                   vectorise(lrv_cub.subcube(p,ql,r*dim*dim,p,q-1,(r+1)*dim*dim-1))),dim);
        }

        else
          tmps = Mat_std(join_cols(vectorise(lrv_cub.subcube(pl,q,r*dim*dim,pu,q,(r+1)*dim*dim-1)),
                                   vectorise(lrv_cub.subcube(p,q+1,r*dim*dim,p,qu,(r+1)*dim*dim-1))),dim);
        if(tmps > s) s = tmps;//max error
      }
      //tmp = s/n;
      tmp = s;
      // lrv_cub.print();
      //std::cout<<"p"<<p<<"q"<<q<<"ise"<<tmp<<endl;
      if(tmp < min_ise){
        min_ise = tmp;
        minp = p;
        minq = q;
      }
    }
  }

  result["minp"] = minp;
  result["minq"] = minq;
  result["min_ise"] = min_ise;
  return(result);
}


arma::vec Heter_LRV2(arma::vec e, arma::mat X, int m, double tau_n = 0,
                     int lrv_method = 0, int ind = 2, bool ncp = 0){
  int n = X.n_rows;
  int p = X.n_cols;
  double regular_factor;
  arma::vec all_delta2(p* p *n), sigma(p* p * n);
  arma::cube sigma_c(p,p,n);
  arma::vec L(p), kernel_vector(n);
  if(!tau_n) tau_n = powf(n,-2/15.0);
  double tau1;

  if(ncp){
    tau1 = tau_n;
  }else{
    tau1 = powf(tau_n, 3.0/2);
  }

  kernel_vector = Compute_kernel_vector(n, tau_n, 0, ind);

  L.zeros();

  if(lrv_method == 3){
    arma::cube Ajmhat(p,p,n), Ajmc(p, p, n), Ajmb(p,1,n);
    vec xb(n), beta(p);
    vec t = linspace(0, 1, n);
    List result;
    mat beta0(n,p);
    // e is y here
    sigma_c = Diff1(e, X, m, tau_n, ind);
    if(p > 1){
      Ajmc = DiffX(X, m, tau1, ind);
      Ajmb = DiffA(e, X, m, tau1, ind);
      beta0 = LocLinear_B(tau_n, t, e, X);
      // std::cout<<"beta0"<<endl;
      for(int i = 0; i < n; i++){
        beta = beta0.row(i).t();
        // std::cout<<"beta0"<<endl;
        xb(i) = dot(X.row(i), beta);
      }
      Ajmhat = Diff1(xb, X, m, tau_n, ind);
      sigma_c = sigma_c - Ajmhat;
    }
    sigma = vectorise(sigma_c);
  }else if(lrv_method == 2){
    arma::cube Ajmhat(p,p,n), Ajmc(p, p, n), Ajmb(p,1,n);
    vec xb(n), beta(p), res(n);
    // e is y here
    Ajmc = DiffX(X, m , tau1);
    Ajmb = DiffA(e, X, m, tau1);
    for(int i = 0; i < n; i++){
      beta = inv(Ajmc.slice(i)) * Ajmb.slice(i);
      xb(i) = dot(X.row(i), beta);
    }
    res = e - xb;
    // std::cout<<"compute res"<<endl;
    sigma = Heter_LRV2(res, X, m, tau_n, 0, ind);
  }else if(lrv_method == 1){
    arma::cube Ajmhat(p,p,n), Ajmc(p, p, n), Ajmb(p,1,n);
    vec xb(n),beta(p);
    // e is y here
    sigma_c = Diff1(e, X, m, tau_n, ind);
    if(p > 1){
      Ajmc = DiffX(X, m , tau1, ind);
      Ajmb = DiffA(e, X, m, tau1, ind);
      for(int i = 0; i < n; i++){
        beta = inv(Ajmc.slice(i)) * Ajmb.slice(i);
        xb(i) = dot(X.row(i), beta);
      }
      Ajmhat = Diff1(xb, X, m, tau_n, ind);
      sigma_c = sigma_c - Ajmhat;
    }
    sigma = vectorise(sigma_c);
  }else if(lrv_method == 0){

    for(int j = 0; j < 2*m + 1; j++)
    {
      L = L + e(j)*X.row(j).t();
    }
    for(int j = m ; j < n - m ; j++)//j needs  conversion starts from 0
    {//all_delta2[j] is m\Delta_j^2/2
      if(j > m)
      {
        L = L - e(j - m - 1) * X.row(j - m - 1).t() + e(j + m) * X.row(j + m).t();
      }
      all_delta2.subvec(p*p*j, p*p*(j+1)-1) = vectorise(L * L.t() / (2 * m + 1));
    }
    for(int j = 0; j < m; j++) all_delta2.subvec(p*p*j, p*p*(j+1)-1).zeros();
    for(int j = n - m; j < n; j++) all_delta2.subvec(p*p*j, p*p*(j+1)-1).zeros();

    for(int i = m; i < n - m; i++)
    {
      sigma.subvec(p*p*i, p*p*(i+1)-1).zeros();//t=(i+1.0)/n
      regular_factor = 0;
      for(int j = i - n*tau_n; j <= i + n*tau_n; j++)//j needs no conversion starts from m
      {//all_delta2[j] is \Delta_j^2
        if(j > 0 && j < n){
          sigma.subvec(p*p*i, p*p*(i+1)-1)+= all_delta2.subvec(p*p*j, p*p*(j+1)-1) * kernel_vector[abs(j - i)];
          regular_factor += kernel_vector[abs(j - i)];
        }
      }
      sigma.subvec(p*p*i, p*p*(i+1)-1) = sigma.subvec(p*p*i, p*p*(i+1)-1)/regular_factor;
    }

    for(int i = 0; i < m; i++) sigma.subvec(p*p*i, p*p*(i+1)-1) = sigma.subvec(p*p*m, p*p*(m+1)-1);
    for(int i = n - m; i < n; i++) sigma.subvec(p*p*i, p*p*(i+1)-1)=  sigma.subvec(p*p*(n-m-1), p*p*(n-m)-1);
  }
  return(sigma);
}


// [[Rcpp::export]]
arma::cube MV_cov_heter(arma::vec e, arma::mat X,
                        Rcpp::IntegerVector gridm,
                        Rcpp::NumericVector gridtau,
                        int lrv_method = 0, int ind = 2, bool ncp = 0)
{
  int n = e.size();
  int dim = X.n_cols;
  int M = gridm.size();
  int Tau = gridtau.size();
  arma::cube lrv_cub = cube(M, Tau, n * dim * dim);
  // std::cout<<M<<Tau<<endl;
  for(int p = 0; p < M; p++){
    for(int q = 0; q < Tau; q++){
      // std::cout<<p<<q<<endl;
      lrv_cub.subcube(p,q,0,p,q,n*dim*dim-1) = Heter_LRV2(e, X, gridm[p], gridtau[q], lrv_method, ind, ncp);
    }
  }
  return(lrv_cub);
}


//' @export
//' @name MV_critical_cp
//' @title Statistics-adapted values for extended minimum volatility selection.
//' @description  Smoothing parameter selection for bootstrap tests for change point tests
//' @param y, vector, as used in the Heter_LRV
//' @param X, matrix, covariates
//' @param t, vector, time points.
//' @param gridm, vector, a grid of candidate m's.
//' @param gridtau, vector, a grid of candidate tau's.
//' @param cvalue, double, 1-quantile for the calculation of bootstrap variance, default 0.1.
//' @param B, integer, number of iterations for the calculation of bootstrap variance
//' @param lrvmethod, integer, see also Heter_LRV
//' @param ind, integer, the type of kernel,  see also Heter_LRV
//' @param rescale, bool, whether to rescale when positiveness of the matrix is not obtained. default 0
//' @return  a matrix of critical values
//' @examples
//' n = 300
//' t = (1:n)/n
//' data = bregress2(n, 2, 1) # time series regression model with 2 changes points
//' critical = MV_critical_cp(data$y, data$x,t,  c(3,4,5), c(0.2,0.25, 0.3))
//' @references  Bai, L., and Wu, W. (2023). Detecting long-range dependence for time-varying linear models. To appear in Bernoulli
// [[Rcpp::export]]
 arma::mat MV_critical_cp(arma::vec y, arma::mat X, arma::vec t,
                          arma::vec gridm,
                          arma::vec  gridtau,
                          double cvalue = 0.1,
                          int B = 100,
                          int lrvmethod = 1, int ind = 2, bool rescale = 0){

   int n = X.n_rows;
   int p = X.n_cols;
   int M = gridm.n_elem;
   int Tau = gridtau.n_elem;
   // double c;
   arma::mat critical(M, Tau);
   arma::cube sigma(p, p, n);
   arma::vec bootstrap(B);
   // int P = B*(1-cvalue);
   double Q;

   for(int i = 0; i < M; i++){
     for(int j = 0; j < Tau; j++){
       //base on
       // std::cout<<"sigma"<<endl;
       sigma = Heter_LRV(y, X, gridm[i], gridtau[j], lrvmethod, ind, 0, rescale);
       bootstrap = sim_T(X,  t, sigma, gridm[i], B, 0);
       // bootstrap = arma::sort(bootstrap);
       // Q = bootstrap(P);
       Q = var(bootstrap);
       // Q = var(bootstrap) + mean(bootstrap)*mean(bootstrap)*c;
       // std::cout<<i<<","<<j << "Q" <<Q<<endl;
       critical(i,j) = Q;
     }
   }
   return(critical);
 }



