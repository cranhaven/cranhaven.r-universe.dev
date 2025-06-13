#include <RcppArmadillo.h> 

using namespace Rcpp;
using namespace std;
using namespace arma;


//// [[Rcpp::export]]
arma::mat Newtonlogistic(arma::mat GZ, arma::mat B, arma::mat alpha, arma::mat beta, double mu, arma::mat Y, double h, arma::mat Zini){
  int maxiter = 20;
  int n = GZ.n_rows;
  int q = GZ.n_cols;
  arma::mat Z = Zini;
  arma::mat M = - GZ/h + B*beta + ones(n, 1)*alpha;
  arma::mat expZ, dz, Hz;
  // arma::vec funcVal, diff=ones(1);
  // double diffend = 1;
  int iter = 0;
  for (iter = 0; iter < maxiter; iter++){ 
    expZ = exp(Z);
    dz = - mu*(Y - expZ/(1+expZ))/n + h*(Z - M);
    Hz = mu*expZ/square(1+expZ)/n + h;
    Hz = Hz*ones(1, q);
    Z = Z - dz/Hz;           
    
    // funcVal = join_cols(funcVal, (-mu*(accu(Z.t()*Y) - accu(log(1+exp(Z))))/n + h/2*accu(square(Z-M)))*ones(1, 1));
    // if (iter > 0){
    //   diffend = (funcVal(iter)-funcVal(iter-1))/abs(funcVal(iter-1));
    //   diff = join_cols(diff, diffend*ones(1, 1));
    // }
    // if (abs(diffend) < 1e-6) break;
  } 
  
  //Rcpp::List out;
  //out["Z"] = Z;
  // out["iter"] = iter;
  // out["funcVal"] = funcVal;
  // out["diff"] = diff;
  //return(out);
  return(Z);
}

//// [[Rcpp::export]]
arma::mat Newtonpoisson(arma::mat GZ, arma::mat B, arma::mat alpha, arma::mat beta, double mu, arma::mat Y, double h, arma::mat Zini){
  int maxiter = 20;
  int n = GZ.n_rows;
  int q = GZ.n_cols;
  arma::mat Z = Zini;
  arma::mat M = - GZ/h + B*beta + ones(n, 1)*alpha;
  arma::mat expZ, dz, Hz;
  // arma::vec funcVal, diff=ones(1);
  // double diffend = 1;
  int iter = 0;
  for (iter = 0; iter < maxiter; iter++){ 
    expZ = exp(Z);
    dz = - mu*(Y - expZ)/n + h*(Z - M);
    Hz = mu*expZ/n + h;
    Hz = Hz*ones(1, q);
    Z = Z - dz/Hz;           
    
    // funcVal = join_cols(funcVal, (-mu*(accu(Z.t()*Y) - accu(exp(Z)))/n + h/2*accu(square(Z-M)))*ones(1, 1));
    // if (iter > 0){
    //   diffend = (funcVal(iter)-funcVal(iter-1))/abs(funcVal(iter-1));
    //   diff = join_cols(diff, diffend*ones(1, 1));
    // }
    // if (abs(diffend) < 1e-6) break;
  } 
  
  // Rcpp::List out;
  // out["Z"] = Z;
  // out["iter"] = iter;
  // out["funcVal"] = funcVal;
  // out["diff"] = diff;
  //return(out);
  return(Z);
}


//// [[Rcpp::export]]
arma::mat MGlasso_C(arma::mat Y, arma::mat X, arma::vec lam, arma::mat B0, double conv, int maxiter) {
  // min_B {|Y-XB|^2 + lam*|B|}
  //cout << "checknode1" << endl; 
  int  p=X.n_cols, iter=0, j;  // n=Y.n_rows, q=Y.n_cols,
  double  diff=10*conv, l2B1;
  rowvec sh; 
  arma::mat mat1=eye(p,p), B1, res1, res1j, XRj; 
  // Rcpp::List out;
  //cout << "checknode2" << endl;   
  if (lam.size() == 1) {lam = as_scalar(lam)*ones(p);}
  sh = sum(square(X), 0);
  if (B0.is_finite()) {
    B1 = B0;
  } else {
    //B1 = inv(X.t()*X)*X.t()*Y;  //OLS 
    Rcpp::Rcout <<"B need to be initialized"<< std::endl;
// cout << "B need to be initialized" << endl;
    //Rcpp::List ini = RRR_C(Y, X, 1, FALSE, mat1, TRUE, TRUE);
    //arma::mat iniC = ini["C_ls"];
    //B1 = iniC;
    B1 = solve(X.t()*X, X.t()*Y, solve_opts::fast + solve_opts::no_approx);
  }
  
  //cout << "X*B1:" <<size(X*B1) << endl;
  res1 = Y - X * B1;
  while ((diff > conv) & (iter < maxiter)) {
    B0 = B1;
    for (j = 0; j < p; j++) {
      res1j = res1 +  X.col(j)* B1.row(j); //n q
      XRj =   trans(X.col(j)) * res1j;    //1 q
      rowvec t1=XRj/as_scalar(sh(j))*max(0.0,1-lam(j)/sqrt(accu(square(XRj))));
      B1.row(j) = t1;
      res1 = res1j - X.col(j)* B1.row(j);
    }
    l2B1 = accu(square(B1));
    if (l2B1 == 0) {
      iter = maxiter; 
    } else {
      diff = sqrt(accu(square(B0 - B1))/l2B1);
      iter = iter + 1;
    }
  }
  //cout << "checknode3" << endl;
  //sse = accu(square(Y - X * B0));
  //out["B"] = B1;
  //out["sse"] = sse;
  //out["iter"] = iter;
  return(B1);
}


////////////////////////////////////////////////////
////////////////////////////////////////////////////

//' @title Canonical Variate Regression.
//' 
//' @description  Perform canonical variate regression with a set of fixed tuning parameters.
//'  
//' @usage cvrsolver(Y, Xlist, rank, eta, Lam, family, Wini, penalty, opts) 
//' 
//' @param Y A response matrix. The response can be continuous, binary or Poisson. 
//' @param Xlist A list of covariate matrices. Cannot contain missing values.
//' @param rank Number of pairs of canonical variates.
//' @param eta Weight parameter between 0 and 1.  
//' @param Lam A vector of penalty parameters \eqn{\lambda} for regularizing the loading matrices 
//'         corresponding to the covariate matrices in \code{Xlist}.  
//' @param family Type of response. \code{"gaussian"} if Y is continuous, \code{"binomial"} if Y is binary, and \code{"poisson"} if Y is Poisson. 
//' @param Wini A list of initial loading matrices W's. It must be provided. See \code{cvr} and \code{scca} for using sCCA solution as the default.
//' @param penalty Type of penalty on W's. "GL1" for rowwise sparsity and 
//'                   "L1" for entrywise sparsity.  
//' @param opts A list of options for controlling the algorithm. Some of the options are: 
//'         
//'         \code{standardization}:  need to standardize the data? Default is TRUE.
//'                        
//'         \code{maxIters}:         maximum number of iterations allowed in the algorithm. The default is 300. 
//'         
//'         \code{tol}:              convergence criterion. Stop iteration if the relative change in the objective is less than \code{tol}.
//'         
//' @details CVR is used for extracting canonical variates and also predicting the response 
//'           for multiple sets of covariates (Xlist = list(X1, X2)) and response (Y). 
//'           The covariates can be, for instance, gene expression, SNPs or DNA methylation data. 
//'           The response can be, for instance, quantitative measurement or binary phenotype.            
//'           The criterion minimizes the objective function 
//'                                 
//'   \deqn{(\eta/2)\sum_{k < j} ||X_kW_k - X_jW_j||_F^2 + (1-\eta)\sum_{k} l_k(\alpha, \beta, Y,X_kW_k)
//'   + \sum_k \rho_k(\lambda_k, W_k),}{%
//'   (\eta/2) \Sigma_\{k<j\}||X_kW_k - X_jW_j||_F^2 + (1 - \eta) \Sigma_k l_k(\alpha, \beta, Y, X_kW_k) + \Sigma_k \rho_k(\lambda_k, W_k),}
//'      s.t. \eqn{W_k'X_k'X_kW_k = I_r,}    for  \eqn{k = 1, 2, \ldots, K}. 
//'      \eqn{l_k()} are general loss functions with intercept \eqn{\alpha} and coefficients \eqn{\beta}. \eqn{\eta} is the weight parameter and 
//'           \eqn{\lambda_k} are the regularization parameters. \eqn{r} is the rank, i.e. the number of canonical pairs.          
//'           By adjusting \eqn{\eta}, one can change the weight of the first correlation term and the second prediction term. 
//'           \eqn{\eta=0} is reduced rank regression and \eqn{\eta=1} is sparse CCA (with orthogonal constrained W's). By choosing appropriate \eqn{\lambda_k} 
//'           one can induce sparsity of \eqn{W_k}'s to select useful variables for predicting Y.                       
//'           \eqn{W_k}'s with \eqn{B_k}'s and (\eqn{\alpha, \beta}) are iterated using an ADMM algorithm. See the reference for details.
//'           
//' @return An object containing the following components
//'   \item{iter}{The number of iterations the algorithm takes.}
//' @return \item{W}{A list of fitted loading matrices.}
//' @return \item{B}{A list of fitted \eqn{B_k}'s.}
//' @return \item{Z}{A list of fitted \eqn{B_kW_k}'s.}
//' @return \item{alpha}{Fitted intercept term in the general loss term.}
//' @return \item{beta}{Fitted regression coefficients in the general loss term.}
//' @return \item{objvals}{A sequence of the objective values.}
//' @author Chongliang Luo, Kun Chen.
//' @references Chongliang Luo, Jin Liu, Dipak D. Dey and Kun Chen (2016) Canonical variate regression. 
//'             Biostatistics, doi: 10.1093/biostatistics/kxw001.
//' @examples ## see  SimulateCVR for simulation examples, see CVR for parameter tuning.
//' @seealso \code{\link{SimulateCVR}}, \code{\link{CVR}}.
//' @export
// [[Rcpp::export]]
Rcpp::List cvrsolver(arma::mat Y, Rcpp::List Xlist, int rank, double eta, arma::vec Lam, 
                     std::string family, Rcpp::List Wini, std::string penalty, Rcpp::List opts){
  int K = Xlist.size();
  int n = Y.n_rows;
  int q = Y.n_cols;
  int nrank = rank, maxIters = opts["maxIters"];
  bool  standardization = as<bool>(opts["standardization"]);
  double  tol = opts["tol"], h = 2, heps = 1;        
  //string family = opts["family"], penalty = opts["penalty"];
  
  field <mat> X(K), XX(K), XW(K), W(K), B(K), Z(K), GW(K), GZ(K), W0(K), C(K);
  
  arma::vec p(K), Wknorm,  funcVal,  diff=ones(1);//presNorm, dresNorm, funcVal_1, funcVal_2, funcVal_3, 
  arma::vec Wnz(K);   //Wnz[k]=1: W[k] is all 0
  uvec nzid;
  Rcpp::List obj;   //outputs
  
  arma::mat alpha = zeros(1, q), beta = zeros(nrank, q);
  arma::mat sumCC = zeros(nrank+1, nrank+1);           // (r+1)-by-(r+1)
  arma::mat sumCZ = zeros(nrank+1, q);                 // (r+1)-by-q
  arma::mat bhat  = zeros(nrank+1, q);  
  arma::mat sumB = zeros(n, nrank);                   //n by r
  arma::mat Ztmp, resNorm;        // funcVals,
  arma::mat Mk, IrXk, vecW0k, BGWk, vecBGWk, onesXWk, IrXkalp, vecBGWk0;
  arma::mat  Gchol, U, V;
  arma:vec d, s;
  double  tmp1 = 0, tmp2 = 0, tmp3 = 0, t1, t2, difftmp;
  
  //cout << "Checknode 1: setups" << endl;    
  if (standardization){
    //cout << "Need standardization" << endl;
    for (int k = 0; k < K; k++){
      X[k] = as<mat>(Xlist[k]) - ones(n)*mean(as<mat>(Xlist[k]));
      X[k] = X[k]/(ones(n)*stddev(as<mat>(Xlist[k]))); 
    }
  }
  else {
    for (int k = 0; k < K; k++){
      X[k] = as<mat>(Xlist[k]);
    }
  }
  
  for (int k = 0; k<K; k++){
    p[k] = X[k].n_cols;
    XX[k] = X[k].t()*X[k];
    W[k] = as<mat>(Wini[k]);
    XW[k] = X[k]*W[k];
    B[k] = XW[k]; 
    GW[k] = zeros(n, nrank);
    GZ[k] = zeros(n, q);
  } 
  
  
  if (family=="gaussian"){
    for (int k = 0; k < K; k++){
      Z[k] = (2*(1-eta)*Y/n + h*(B[k]*beta + ones(n, 1)*alpha- GZ[k]/h))/(2*(1-eta)/n + h);
    }
  }
  else if (family=="binomial"){
    for (int k = 0; k < K; k++){
      Z[k] = zeros(n, q);
      //Nltmp = Newtonlogistic(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
      //Z[k] = as<mat>(Nltmp["Z"]);
      Z[k] = Newtonlogistic(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
    }
  }
  else if (family=="poisson"){
    for (int k = 0; k < K; k++){
      Z[k] = zeros(n, q);
      //Nltmp = Newtonpoisson(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
      //Z[k] = as<mat>(Nltmp["Z"]);
      Z[k] = Newtonpoisson(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
    }
  } 
  
  //cout << "Checknode 3: initialization " << endl;  
  
  //iteration begins
  int iter = 0;


  for (iter = 0; iter < maxIters; iter++){
    //cout << "       iter = " << iter << endl;  
    Wnz = zeros(K);
    sumCC = zeros(nrank+1, nrank+1);            
    sumCZ = zeros(nrank+1, q);
    sumB = zeros(n, nrank); 
    for (int k = 0; k < K; k++){
      W0[k] = W[k];
      if(accu(abs(W[k]))==0){Wnz[k] = 1;}
    }
    
    if (accu(Wnz)!=0) break;      // some Wk is all 0
    
    // beta-step
    for (int k = 0; k < K; k++){
      C[k] = join_rows(ones(n, 1), B[k]);                    // n by r+1
      sumCC = sumCC + C[k].t()*C[k];
      sumCZ = sumCZ + C[k].t()*(Z[k]+GZ[k]/h); 
    }

    //Gchol = chol(sumCC);
    //svd_econ(U, d, V, Gchol); 
    //bhat = fastLS(sumCC, sumCZ);                 // (r+1)-by-q
    bhat = solve(sumCC + diagmat(0.00000001 * ones(nrank + 1)), sumCZ );
    alpha = bhat.row(0);                         // 1 by q    
    beta  = bhat.rows(1, nrank);                  // r by q
    
    //cout << "beta updated " << endl;
    // B-step
    
 
    for (int k = 0; k < K; k++){
      // B[k] = OrthProc(B[k],sumB,X[k],W[k],Z[k],GW[k],GZ[k],beta,alpha,h,eta);
      sumB = zeros(n, nrank);
      for (int j = 0; j < K; j++){  
        sumB = sumB + B[j];
      }
      Mk = eta/2*(sumB-B[k]) + h/2*(XW[k]+GW[k]/h) + h/2*(Z[k] - ones(n, 1)*alpha + GZ[k]/h)*beta.t();
      
      bool success = svd_econ(U, s, V, Mk);
      if(!success) {
        Rcpp::Rcout <<"OrthProc failed!"<< std::endl;
//cout << "OrthProc failed!!" << endl;
      }
      B[k] = U*V.t();
      
    }
    
    
    //cout << "B updated " << endl;
    // W-step
    for (int k = 0; k < K; k++){
      BGWk = B[k]-GW[k]/h;
      
      if (penalty=="GL1"){    //row-wise lasso penalty 
        Wknorm = sum(square(W0[k]), 1);
        //remove zero columns of X
        nzid = find(Wknorm!=0);
        W[k].rows(nzid) = MGlasso_C(BGWk, X[k].cols(nzid), ones(nzid.size())*Lam[k]/h, W0[k].rows(nzid), 1e-2, 30);
        //MGltmp = MGlasso_C(BGWk, X[k].cols(nzid), ones(nzid.size())*Lam[k]/h, W0[k].rows(nzid), 1e-2, 50);
        //W[k].rows(nzid)= as<mat>(MGltmp["B"]);
      } else if (penalty=="L1"){  
        //entrywise lasso penalty: vectorize, then lasso
        vecW0k = reshape(W0[k], p[k]*nrank, 1);
        IrXk = kron(eye(nrank, nrank), X[k]);
        vecBGWk = reshape(BGWk, n*nrank, 1);
        //MGltmp = MGlasso_C(vecBGWk, IrXk, ones(p[k]*nrank)*Lam[k]/h, vecW0k, 1e-2, 50);
        //vecW0k = as<mat>(MGltmp["B"]);
        vecW0k = MGlasso_C(vecBGWk, IrXk, ones(p[k]*nrank)*Lam[k]/h, vecW0k, 1e-2, 30);
        W[k] = reshape(vecW0k, p[k], nrank);
      } else if (penalty=="enet"){  
        //entrywise enet penalty: vectorize, then enet, alp=0.1(L2 penalty)
        vecW0k = reshape(W0[k], p[k]*nrank, 1);
        IrXk = kron(eye(nrank, nrank), X[k]);
        double alp = 0.1;
        IrXkalp = join_cols(IrXk, sqrt(alp)*eye(p[k]*nrank, p[k]*nrank)); 
        vecBGWk = reshape(BGWk, n*nrank, 1);
        vecBGWk0 = join_cols(vecBGWk, zeros(p[k]*nrank, 1));
        //MGltmp = MGlasso_C(vecBGWk0, IrXkalp, ones(p[k]*nrank)*Lam[k]*(1-alp)/h, vecW0k, 1e-2, 50);
        //vecW0k = as<mat>(MGltmp["B"]);
        vecW0k = MGlasso_C(vecBGWk0, IrXkalp, ones(p[k]*nrank)*Lam[k]*(1-alp)/h, vecW0k, 1e-2, 30);
        vecW0k = (1+alp*Lam[k])*vecW0k;
        W[k] = reshape(vecW0k, p[k], nrank);
      }
      
      XW[k] = X[k]*W[k];    // store XW[k] fo later use
    }
    //cout << "W updated " << endl;
    // Z-step 
    if (family=="binomial"){ 
      for (int k = 0; k < K; k++){
        // Nltmp = Newtonlogistic(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
        // Z[k] = as<mat>(Nltmp["Z"]);
        Z[k] = Newtonlogistic(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
      }
    }
    else if (family=="gaussian"){  
      for (int k = 0; k < K; k++){
        Z[k] = (2*(1-eta)*Y/n + h*( - GZ[k]/h + B[k]*beta + ones(n, 1)*alpha))/(2*(1-eta)/n + h);
      }
    }
    else if (family=="poisson"){ 
      for (int k = 0; k < K; k++){
        //Nltmp = Newtonpoisson(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
        //Z[k] = as<mat>(Nltmp["Z"]);
        Z[k] = Newtonpoisson(GZ[k],B[k],alpha,beta,1-eta,Y,h,Z[k]);
      }
    }
    //cout << "Z updated " << endl;   
    // Dual-step 
    for (int k = 0; k < K; k++){
      GW[k] = GW[k] + h*(XW[k]-B[k]);
      GZ[k] = GZ[k] + h*(Z[k] - B[k]*beta - ones(n, 1)*alpha);
    }
    
    // objective function
    tmp1 = 0;
    for (int k = 1; k < K; k++){
      for (int j = 0; j < k; j++){
        tmp1 = tmp1 + accu(square(XW[k] - XW[j] ))/2;
      }
    }
    //funcVal_1 = join_cols(funcVal_1, tmp1*ones(1, 1));
    //cout << "funcVal_1 " << endl;
    tmp2 = 0;
    if (family=="binomial"){
      for (int k = 0; k < K; k++){
        Ztmp = XW[k]*beta + ones(n, 1)*alpha;
        tmp2 = tmp2 - accu(Y%Ztmp - log(1+exp(Ztmp)))/n;
      }
    }
    else if (family=="gaussian"){ 
      for (int k = 0; k < K; k++){
        Ztmp = XW[k]*beta + ones(n, 1)*alpha;
        tmp2 = tmp2 + accu(square(Y-Ztmp))/n;
      }
    }
    else if (family=="poisson"){ 
      for (int k = 0; k < K; k++){
        Ztmp = XW[k]*beta + ones(n, 1)*alpha;
        tmp2 = tmp2 - accu(Y%Ztmp - exp(Ztmp))/n;
      }
    }
    
    //funcVal_2 = join_cols(funcVal_2, tmp2*ones(1, 1));
    //cout << "funcVal_2 " << endl;  
    tmp3 = 0;
    for (int k = 0; k < K; k++){
      if(penalty=="GL1"){
        tmp3 = tmp3 + Lam[k]*accu(sqrt(sum(square(W[k]), 1)));
      } else if(penalty=="L1"){
        tmp3 = tmp3 + Lam[k]*accu(abs(W[k]));
      }
    } 
    //funcVal_3 = join_cols(funcVal_3, tmp3*ones(1, 1));
    funcVal = join_cols(funcVal, (eta*tmp1+(1-eta)*tmp2+tmp3)*ones(1, 1));
    //cout << "funcVal " << endl;  
    
    if (iter > 0){
      t1 = as_scalar(funcVal(iter)-funcVal(iter-1));
      t2 = abs(as_scalar(funcVal(iter-1)));
      difftmp = t1/t2;
      diff = join_cols(diff, difftmp*ones(1, 1));
      //cout << diff << endl;
      if (abs(difftmp) < tol) break;
    }
    
    h = h*heps;
    //cout << "obj updated " << endl;
    if (iter == maxIters-1){
Rcpp::Rcout <<"Not converge yet!" << difftmp << std::endl;
//   cout << "Not converge yet!" <<difftmp << endl;
    }
  }     //end of for
  
  
  //  if(funcVal_1.is_empty()){
  //    funcVals = ones(5);
  //  } else{
  //    funcVals = join_rows(funcVal_1, funcVal_2);
  //    funcVals = join_rows(funcVals, funcVal_3);
  //    funcVals = join_rows(funcVals, funcVal);
  //    funcVals = join_rows(funcVals, diff);
  //  }
  
  //obj["family"] = family;
  obj["iter"] = iter;
  //obj["X"] = X;
  obj["W"] = W;
  obj["B"] = B;
  obj["Z"] = Z;
  obj["alpha"] = alpha;
  obj["beta"] = beta;
  obj["objvals"] = funcVal;
  //obj["diff"] = diff;
  obj["ver"] = "test";
  //cout << "CVR done!" << endl;
  return(obj);
}
// 
// RCPP_MODULE(CVRmod)
// {
//   Rcpp::function("cvrsolver", &cvrsolver);
// }

