#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <Rcpp.h>
using namespace arma;
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int nzcount(arma::vec x) {
  vec y = nonzeros(x) ;
  return y.n_elem;
}


// [[Rcpp::export]]
arma::vec wpow(arma::vec x, double gamma0) {
  //uvec ind = find(x==0);
  vec y = pow(abs(x),-1.0*gamma0);
  y.elem(find(x==0)).zeros();
  y.elem(find(x==0)).fill(10000*y.max());
  return y;
}

// // [[Rcpp::export]]
// arma::mat wdiv(arma::mat x, arma::mat y) {
//   // uvec	 sub = ind2sub( size(X), index )
//   uvec ind = find(y==0);
//   arma::mat z = x/y;
//   z.elem(ind).zeros();
//   return z;
// }

// [[Rcpp::export]]
double softThres(double x, double lambda) {
  return((x > lambda) ? x - lambda :
         (x < -lambda) ? x + lambda : 0.);
}

//  Linear constrained Penalized regression with elastic net penalty
//
//  #@param XY X'Y where X is covariance matrix and Y response vector
//  #@param XX  X'X where X is covariance matrix
//  #@param A linear constraint parameter A
//  #@param B linear constraint parameter
//  #@param Lambda1 lasso penalty parameter
//  #@param Lambda2 elastic net penalty parameter
//  #@param Mu linear constraint strength
//  #@param Nu multiplicative factor for linear constraint strength
//  #@param Beta0 initial value of regression coefficient vector
//  #@param control a list of parameters controling the fitting process
//  #@return estimated regression coefficient vector
// [[Rcpp::export]]
arma::vec bregpcdenet_Rcpp(arma::vec XY, arma::mat XX, arma::mat A, arma::vec B,
                           arma::vec Lambda1, arma::vec Lambda2,double Mu, double Nu,
                           arma::vec Beta0, List control){

  double inTol = control["inTol"],diff=2*inTol;
  int inMaxIter = control["inMaxIter"],counter=0,nrA = A.n_rows;

  int p = XX.n_cols,jj;

  arma::mat AA = A.t()*A;
  arma::vec AB = A.t()*B,Beta(p),Betap(p),cp(nrA),cc = zeros<vec>(nrA),shptr(p);
  arma::vec dAA(p),dXX(p),tt(p);
  arma::vec sth;
  bool Aind = all(vectorise(A) == 0);
  // check for dimension match and suggest correction

  // beta current

  Beta = Beta0;
  dXX = diagvec(XX);XX.diag().zeros();
  dAA = diagvec(AA);AA.diag().zeros();

  tt  = dXX+ Lambda2;

  shptr = tt+ Mu* dAA ;

  while ((diff > inTol) & (counter < inMaxIter)){
    cp = cc;
    Betap = Beta;

    // update Beta
    for(jj=0; jj<p; jj++){
      if(Aind){
        sth = XY(jj) - Beta.t()*XX.col(jj);// update it
      }else {
        sth = XY(jj) - Beta.t()*XX.col(jj) + cc.t()*A.col(jj) + Mu*AB(jj) - Mu*accu(AA.col(jj) % Beta);// update it
      }
      Beta(jj) = softThres(as_scalar(sth), as_scalar(Lambda1(jj)))/as_scalar(shptr(jj));
    }

    //update cc
    cc = cp - Mu*(A*Beta-B);
    //update Mu
    Mu  = Nu*Mu;

    shptr = tt+ Mu* dAA ;
    counter = counter+1;

    //check convergence : change when compared
    // diff = norm(Beta-Betap,2)/norm(Betap,2);
    diff = norm(Beta-Betap,2)/norm(Betap,2)  + norm(A*Beta-B,2);
    // diff = norm(Beta-Betap,2)/norm(Betap,2) + norm(A*(Beta-Betap),2)/norm(A*Betap,2) ;
    // diff = pow(norm(Beta-Betap,2),2)/pow(norm(Betap,2),2)  + pow(norm(A*Beta-B,2),2);
  }

  return(Beta);
}





//  Linear constrained Penalized regression with elastic net penalty when X is orthogonal
//
//  #@param XY X'Y where X is covariance matrix and Y response vector
//  #@param XX  X'X where X is covariance matrix
//  #@param A linear constraint parameter A
//  #@param B linear constraint parameter
//  #@param Lambda1 lasso penalty parameter
//  #@param Lambda2 elastic net penalty parameter
//  #@param Mu linear constraint strength
//  #@param Nu multiplicative factor for linear constraint strength
//  #@param Beta0 initial value of regression coefficient vector
//  #@param control a list of parameters controling the fitting process
//  #@return estimated regression coefficient vector
// [[Rcpp::export]]
arma::vec bregpcdenetdiag_Rcpp(arma::vec XY, arma::vec XX, arma::mat A, arma::vec B,
                               arma::vec Lambda1, arma::vec Lambda2,double Mu, double Nu,
                               arma::vec Beta0, List control){

  double inTol = control["inTol"],diff=2*inTol;
  int inMaxIter = control["inMaxIter"],counter=0,nrA = A.n_rows;

  int p = XX.n_rows,jj;

  arma::mat AA = A.t()*A;
  arma::vec AB = A.t()*B,Beta(p),Betap(p),cp(nrA),cc = zeros<vec>(nrA),shptr(p);
  arma::vec dAA(p),dXX(p),tt(p);
  arma::vec sth;
  bool Aind = all(vectorise(A) == 0);
  // check for dimension match and suggest correction

  // beta current

  Beta = Beta0;
  dXX = XX;
  dAA = diagvec(AA);AA.diag().zeros();

  tt  = dXX+ Lambda2;

  shptr = tt+ Mu* dAA ;

  while ((diff > inTol) & (counter < inMaxIter)){
    cp = cc;
    Betap = Beta;

    // update Beta
    for(jj=0; jj<p; jj++){
      if(Aind){
        sth = XY(jj);// update it
      }else {
        sth = XY(jj)  + cc.t()*A.col(jj) + Mu*AB(jj) - Mu*sum(AA.col(jj) % Beta);// update it
      }
      //sth = XY(jj) - Beta.t()*XX.col(jj) + cc.t()*A.col(jj) + Mu*AB(jj) - Mu*sum(AA.col(jj) % Beta);// update it
      Beta(jj) = softThres(as_scalar(sth), as_scalar(Lambda1(jj)))/as_scalar(shptr(jj));
    }

    //update cc
    cc = cp - Mu*(A*Beta-B);
    //update Mu
    Mu  = Nu*Mu;

    shptr = tt+ Mu* dAA ;
    counter = counter+1;

    //check convergence : change when compared
    // diff = norm(Beta-Betap,2)/norm(Betap,2);
    diff = (norm(Beta-Betap,2))/(norm(Betap,2)) + norm(A*Beta-B,2);
    // diff = norm(Beta-Betap,2)/norm(Betap,2) + norm(A*(Beta-Betap),2)/norm(A*Betap,2) ;
    // diff = pow(norm(Beta-Betap,2),2)/pow(norm(Betap,2),2)  + pow(norm(A*Beta-B,2),2);
  }

  return(Beta);
}





//  Sparse unit-rank regression, Rcpp version
//  
//  #@param X covariate matrix
//  #@param Y response matrix
//  #@param Au responsible for orthogonality constraint on XU
//  #@param Av responsible for orthogonality constraint on V
//  #@param bu responsible for orthogonality constraint on XU
//  #@param bv responsible for orthogonality constraint on V
//  #@param uk initial value of kth column of U
//  #@param vk initial value of kth column of V
//  #@param dk initial value of kth diagonal value of D
//  #@param nlambda number of lambda to be specified in solution path
//  #@param control a list of parameters controlling the fitting process
//  #@return S3 \code{secure_SURR_Rcpp} object .....
// [[Rcpp::export]]
Rcpp::List secure_SURR_Rcpp(arma::mat X, arma::mat Y,arma::mat Au, arma::mat Av, arma::vec bu, arma::vec bv,
                         arma::vec uk, arma::vec vk, double dk, int nlambda, Rcpp::List control){

  int p = X.n_cols;
  int q = Y.n_cols;
  int n = Y.n_rows;
  Rcpp::List out;

//   arma::mat Au= zeros<mat>(1,p),Av = zeros<mat>(1,q);
//   arma::vec bu = zeros<vec>(1),bv= zeros<vec>(1);

  double mu = control["mu"];
  double nu = control["nu"];
  double lmif = control["lamMinFac"];
  double lmaf = control["lamMaxFac"];
  double spu = control["spU"], spv = control["spV"];

  Rcpp::List innerControl;
  innerControl["inTol"] = control["inTol"];
  innerControl["inMaxIter"] = control["inMaxIter"];
  double gamma0 = control["gamma0"], elalpha = control["elnetAlpha"];

  arma::mat XX = X.t()*X;
  arma::mat XY = X.t()*Y;

  int chuk = 0, chvk = 0;
  //arma::vec Wu = pow(abs(uk),-1.0*gamma0),Wv = pow(abs(vk),-1.0*gamma0);
  //double Wd  = pow(dk,-1.0*gamma0);
  arma::vec Wu = wpow(uk,gamma0),Wv = wpow(vk,gamma0);
  double Wd; if(dk==0) Wd=0; else Wd  = pow(dk,-1.0*gamma0);


  arma::mat lamMat = XY / (Wu*Wd*Wv.t()); //wdiv(XY , (Wu*Wd*Wv.t()));
  arma::mat ablamMat = abs(lamMat);
  uvec indxx = find(ablamMat == ablamMat.max());indxx =indxx(0);
  double lmax = (lmaf*ablamMat.max())/elalpha;
  double lmin = lmax*lmif;
  arma::vec lamSeq =  exp(linspace<vec>(log(lmax),  log(lmin), nlambda));

  //uvec tss =  ind2sub( size(ablamMat), as_scalar(indxx) );
  uk.zeros();vk.zeros();
  //uk(tss(0)) = 1; vk(tss(1)) = 1; vk = vk*sign(lamMat(indxx));
  uk(as_scalar(indxx)%p) = 1; vk(as_scalar(indxx)/p) = 1; vk = vk*sign(lamMat(indxx));


  // arma::mat XvXv = diagmat( ones<vec>(q) * (uk.t()*XX*uk));
  arma::vec XvXv = ones<vec>(q) * (uk.t()*XX*uk);
  arma::mat XuXu = XX*as_scalar(vk.t()*vk);

  arma::vec XuYk = XY*vk;
  arma::vec XvYk = XY.t()*uk;


  arma::mat Uklam,Vklam, BIClam;
  arma::vec Dklam,lselectSeq,execTime;
  Uklam.zeros(p,nlambda+1);Vklam.zeros(q,nlambda+1);BIClam.zeros(4,nlambda+1);
  Dklam.zeros(nlambda+1);  lselectSeq.zeros(nlambda+1);execTime.zeros(nlambda+1);
  int outMaxIter = control["outMaxIter"],ii=0,i,j;
  double lam, SSE, df,lamv2,elp;
  double svk=0,suk=0,err,outTol = control["outTol"];
  arma::vec lamv1,lambdaV1,lambdaV2,lamu1,lambdaU1,lambdaU2,Vkest,Ukest;
  arma::mat ck,cc;
  // uvec status2;
  SSE = log(pow(norm(Y,"fro"),2)) ;
  BIClam(0,ii) = SSE; //BIC
  BIClam(1,ii) = SSE; //BICP
  BIClam(2,ii) = SSE; //GIC
  BIClam(3,ii) = SSE; //AIC
  wall_clock timer;

  for(j = 0; j < nlambda; j++){
    lam = lamSeq(j);
    ck = uk*dk*vk.t();
    lamv1 = elalpha*lam*Wd*Wu;
    lamv2 = 2*(1-elalpha)*lam;
    lambdaV1 = Wv*(lamv1.t()*abs(uk));
    lambdaV2 = lamv2*ones<vec>(q)*(uk.t()*uk);

    lamu1 =elalpha*lam*Wd*Wv;
    lambdaU1 = Wu*(lamu1.t()*abs(vk));
    lambdaU2 = lamv2*ones<vec>(p)*(vk.t()*vk);

    timer.tic();
    for(i = 1; i < outMaxIter; i++){

        // U-Step
        if(chvk==1){
            XuYk = XY*vk;
            XuXu = XX*as_scalar(vk.t()*vk);
            lambdaU1 = Wu*(lamu1.t()*abs(vk));
            lambdaU2 = lamv2*ones<vec>(p)*(vk.t()*vk);
        }

        Ukest = bregpcdenet_Rcpp(XuYk, XuXu, Au/dk, bu,lambdaU1, lambdaU2,mu, nu, uk*dk, innerControl);

        Ukest.elem( find(abs(Ukest) < 1e-7) ).zeros();
        suk = accu(abs(Ukest));
        // Vk.est[abs(Vk.est)<1e-7] <- 0 ; s.vk <- accu(abs(Vk.est))
        if (suk == 0) {
            chuk = 0;lselectSeq(0) = lam;break;
        } else {
            dk = as_scalar(sqrt(Ukest.t()*XX*Ukest ))/sqrt((double) n);
            uk = Ukest/dk;
            chuk = 1;
        }

        // V-step
        if(chuk==1){
            // XvXv = diagmat( ones<vec>(q) * (uk.t()*XX*uk));
            XvXv = ones<vec>(q) * (uk.t()*XX*uk);
            XvYk = XY.t()*uk;
            lambdaV1 = Wv*(lamv1.t()*abs(uk));
            lambdaV2 = lamv2*ones<vec>(q)*(uk.t()*uk);
        }

        Vkest = bregpcdenetdiag_Rcpp(XvYk, XvXv, Av/dk, bv,lambdaV1, lambdaV2,mu, nu, vk*dk, innerControl);

        Vkest.elem( find(abs(Vkest) < 1e-7) ).zeros();
        svk = accu(abs(Vkest));
        // Vk.est[abs(Vk.est)<1e-7] <- 0 ; s.vk <- accu(abs(Vk.est))
        if (svk == 0) {
            chvk = 0;lselectSeq(0) = lam;break;
        } else {
            dk = norm( Vkest,2);
            vk = Vkest/dk;
            chvk = 1;
        }



      cc  = uk*dk*vk.t(); //<- Dk * tcrossprod(Uk ,Vk)
      err = norm( cc-ck,"fro")/norm(ck,"fro");
      if (err < outTol) {break;} else {ck = cc ;}
    }

    elp = timer.toc();
    //  add solution path information

    if (svk > 0)
      if( suk > 0){
        ii = ii+1;
        Uklam.col(ii) = uk;
        Vklam.col(ii) = vk;
        Dklam(ii) = dk;
        execTime(ii) = elp;
        // exe.time <- c(exe.time,as.numeric(difftime(en.time,st.time,units = "secs")) )
        SSE = log(accu(square(Y-X*cc)));// accu(square(Y-X*cc));
        df = accu(uk != 0) + accu(vk != 0) -1;
        lselectSeq(ii) = lam;

        BIClam(0,ii) = SSE + (df*log((double) q*n))/(q*n); //BIC
        BIClam(1,ii) = SSE + 2*df*log((double) p*q)/(q*n); //BICP
        BIClam(2,ii) = SSE + log(log((double) n*q))*df*log((double) p*q)/(q*n); //GIC 
        BIClam(3,ii) = SSE + 2/q/n*(df); //AIC
      }

    if( (i>1) && ((nzcount(uk) > (p*spu)) || (nzcount(vk) > (q*spv)))  ) {break;}

  }

  out["ukpath"] = Uklam;
  out["vkpath"] = Vklam;
  out["dkpath"] = Dklam;
  out["ICkpath"] = BIClam;
  out["lamkpath"] = lselectSeq;
  out["ExecTimekpath"] = execTime;
  out["nkpath"] = ii+1;
//   out["x1"] = lamMat;
//   out["x2"] = Wu;
//   out["x3"] = Wd;
//   out["x4"] = Wv;
  return(out);

}



//  Sparse unit-rank regression when X is orthogonal, Rcpp version
//  #@param X covariate matrix
//  #@param Y response matrix
//  #@param Au responsible for orthogonality constraint on XU
//  #@param Av responsible for orthogonality constraint on V
//  #@param bu responsible for orthogonality constraint on XU
//  #@param bv responsible for orthogonality constraint on V
//  #@param uk initial value of kth column of U
//  #@param vk initial value of kth column of V
//  #@param dk initial value of kth diagonal value of D
//  #@param nlambda number of lambda to be specified in solution path
//  #@param control a list of parameters controlling the fitting process
//  #@return S3 \code{secure_SURR_Rcpp_ortho} object .....
// [[Rcpp::export]]
Rcpp::List secure_SURR_Rcpp_ortho(arma::mat X, arma::mat Y,arma::mat Au, arma::mat Av, arma::vec bu, arma::vec bv,
                           arma::vec uk, arma::vec vk, double dk, int nlambda, Rcpp::List control){

  int p = X.n_cols;
  int q = Y.n_cols;
  int n = Y.n_rows;
  Rcpp::List out;

  //   arma::mat Au= zeros<mat>(1,p),Av = zeros<mat>(1,q);
  //   arma::vec bu = zeros<vec>(1),bv= zeros<vec>(1);

  double mu = control["mu"];
  double nu = control["nu"];
  double lmif = control["lamMinFac"];
  double lmaf = control["lamMaxFac"];


  Rcpp::List innerControl;
  innerControl["inTol"] = control["inTol"];
  innerControl["inMaxIter"] = control["inMaxIter"];
  double gamma0 = control["gamma0"], elalpha = control["elnetAlpha"];
  double spu = control["spU"], spv = control["spV"];

  arma::vec XX = vectorise(sum(pow(X,2),0));
  arma::mat XY = X.t()*Y;


  int chuk = 0, chvk = 0;
  //arma::vec Wu = pow(abs(uk),-1.0*gamma0),Wv = pow(abs(vk),-1.0*gamma0);
  //double Wd  = pow(dk,-1.0*gamma0);
  arma::vec Wu = wpow(uk,gamma0),Wv = wpow(vk,gamma0);
  double Wd; if(dk==0) Wd=0; else Wd  = pow(dk,-1.0*gamma0);

  arma::mat lamMat = XY / (Wu*Wd*Wv.t()); //wdiv(XY , (Wu*Wd*Wv.t()));
  arma::mat ablamMat = abs(lamMat);
  uvec indxx = find(ablamMat == ablamMat.max());indxx =indxx(0);
  double lmax = (lmaf*ablamMat.max())/elalpha;
  double lmin = lmax*lmif;
  arma::vec lamSeq =  exp(linspace<vec>(log(lmax),  log(lmin), nlambda));

  //uvec tss =  ind2sub( size(ablamMat), as_scalar(indxx) );
  uk.zeros();vk.zeros();
  //uk(tss(0)) = 1; vk(tss(1)) = 1; vk = vk*sign(lamMat(indxx));
  uk(as_scalar(indxx)%p) = 1; vk(as_scalar(indxx)/p) = 1; vk = vk*sign(lamMat(indxx));

  arma::vec XvXv = ones<vec>(q) * (uk.t()*(XX % uk));
  arma::vec XuXu = XX*as_scalar(vk.t()*vk);

  arma::vec XuYk = XY*vk;
  arma::vec XvYk = XY.t()*uk;


  arma::mat Uklam,Vklam, BIClam;
  arma::vec Dklam,lselectSeq,execTime;
  Uklam.zeros(p,nlambda+1);Vklam.zeros(q,nlambda+1);BIClam.zeros(4,nlambda+1);
  Dklam.zeros(nlambda+1);  lselectSeq.zeros(nlambda+1);execTime.zeros(nlambda+1);
  int outMaxIter = control["outMaxIter"],ii=0,i,j;
  double lam, SSE, df,lamv2,elp;
  double svk=0,suk=0,err,outTol = control["outTol"];
  arma::vec lamv1,lambdaV1,lambdaV2,lamu1,lambdaU1,lambdaU2,Vkest,Ukest;
  arma::mat ck,cc;
  // uvec status2;
  SSE = log(pow(norm(Y,"fro"),2)) ;
  BIClam(0,ii) = SSE; //BIC
  BIClam(1,ii) = SSE; //BICP
  BIClam(2,ii) = SSE; //GIC
  BIClam(3,ii) = SSE; //AIC
  wall_clock timer;

  for(j = 0; j < nlambda; j++){
    lam = lamSeq(j);
    ck = uk*dk*vk.t();
    lamv1 = elalpha*lam*Wd*Wu;
    lamv2 = 2*(1-elalpha)*lam;
    lambdaV1 = Wv*(lamv1.t()*abs(uk));
    lambdaV2 = lamv2*ones<vec>(q)*(uk.t()*uk);

    lamu1 =elalpha*lam*Wd*Wv;
    lambdaU1 = Wu*(lamu1.t()*abs(vk));
    lambdaU2 = lamv2*ones<vec>(p)*(vk.t()*vk);

    timer.tic();
    for(i = 1; i < outMaxIter; i++){

        // U-Step
        if(chvk==1){
            XuYk = XY*vk;
            XuXu = XX*as_scalar(vk.t()*vk);
            lambdaU1 = Wu*(lamu1.t()*abs(vk));
            lambdaU2 = lamv2*ones<vec>(p)*(vk.t()*vk);
        }
        Ukest = bregpcdenetdiag_Rcpp(XuYk, XuXu, Au/dk, bu,lambdaU1, lambdaU2,mu, nu, uk*dk, innerControl);

        Ukest.elem( find(abs(Ukest) < 1e-7) ).zeros();
        suk = accu(abs(Ukest));
        // Vk.est[abs(Vk.est)<1e-7] <- 0 ; s.vk <- accu(abs(Vk.est))
        if (suk == 0) {
            chuk = 0;lselectSeq(0) = lam;break;
        } else {
            dk = as_scalar(sqrt(Ukest.t()*(XX % Ukest) ))/sqrt((double) n);
            uk = Ukest/dk;
            chuk = 1;
        }


        // V-step
        if(chuk==1){
            XvXv = ones<vec>(q) * (uk.t()*(XX % uk));
            XvYk = XY.t()*uk;
            lambdaV1 = Wv*(lamv1.t()*abs(uk));
            lambdaV2 = lamv2*ones<vec>(q)*(uk.t()*uk);
        }

        Vkest = bregpcdenetdiag_Rcpp(XvYk, XvXv, Av/dk, bv,lambdaV1, lambdaV2,mu, nu, vk*dk, innerControl);

        Vkest.elem( find(abs(Vkest) < 1e-7) ).zeros();
        svk = accu(abs(Vkest));
        // Vk.est[abs(Vk.est)<1e-7] <- 0 ; s.vk <- accu(abs(Vk.est))
        if (svk == 0) {
            chvk = 0;lselectSeq(0) = lam;break;
        } else {
            dk = norm( Vkest,2);
            vk = Vkest/dk;
            chvk = 1;
        }


      cc  = uk*dk*vk.t(); //<- Dk * tcrossprod(Uk ,Vk)
      err = norm( cc-ck,"fro")/norm(ck,"fro");
      if (err < outTol) {break;} else {ck = cc ;}
    }

    elp = timer.toc();
    //  add solution path information

    if (svk > 0)
      if( suk > 0){
        ii = ii+1;
        Uklam.col(ii) = uk;
        Vklam.col(ii) = vk;
        Dklam(ii) = dk;
        execTime(ii) = elp;
        // exe.time <- c(exe.time,as.numeric(difftime(en.time,st.time,units = "secs")) )
        SSE = log(accu(square(Y-X*cc)));// accu(square(Y-X*cc));
        df = accu(uk != 0) + accu(vk != 0) -1;
        lselectSeq(ii) = lam;

        BIClam(0,ii) = SSE + (df*log((double) q*n))/(q*n); //BIC
        BIClam(1,ii) = SSE + 2*df*log((double) p*q)/(q*n); //BICP
        BIClam(2,ii) = SSE + log(log((double) n*q))*df*log((double) p*q)/(q*n); //GIC 
        BIClam(3,ii) = SSE + 2/q/n*(df); //AIC
      }

      if( (i>1) && ((nzcount(uk) > (p*spu)) || (nzcount(vk) > (q*spv)))  ) {break;}

  }

  out["ukpath"] = Uklam;
  out["vkpath"] = Vklam;
  out["dkpath"] = Dklam;
  out["ICkpath"] = BIClam;
  out["lamkpath"] = lselectSeq;
  out["ExecTimekpath"] = execTime;
  out["nkpath"] = ii+1;
  //   out["x1"] = lamMat;
  //   out["x2"] = Wu;
  //   out["x3"] = Wd;
  //   out["x4"] = Wv;
  return(out);

}



//  Sparse unit-rank regression when missing entries in Y, Rcpp version
//  #@param X covariate matrix
//  #@param Y response matrix
//  #@param naInd Missing entries indicator matrix
//  #@param Au responsible for orthogonality constraint on XU
//  #@param Av responsible for orthogonality constraint on V
//  #@param bu responsible for orthogonality constraint on XU
//  #@param bv responsible for orthogonality constraint on V
//  #@param uk initial value of kth column of U
//  #@param vk initial value of kth column of V
//  #@param dk initial value of kth diagonal value of D
//  #@param nlambda number of lambda to be specified in solution path
//  #@param control a list of parameters controlling the fitting process
//  #@return S3 \code{secure_SURR_miss_Rcpp} object .....
// [[Rcpp::export]]
Rcpp::List secure_SURR_miss_Rcpp(arma::mat X, arma::mat Y,arma::mat  naInd,arma::mat Au, arma::mat Av, arma::vec bu, arma::vec bv,
                           arma::vec uk, arma::vec vk, double dk, int nlambda, Rcpp::List control){

  int p = X.n_cols;
  int q = Y.n_cols;
  int n = Y.n_rows;
  Rcpp::List out;

  //   arma::mat Au= zeros<mat>(1,p),Av = zeros<mat>(1,q);
  //   arma::vec bu = zeros<vec>(1),bv= zeros<vec>(1);

  double mu = control["mu"];
  double nu = control["nu"];
  double lmif = control["lamMinFac"];
  double lmaf = control["lamMaxFac"];

  Rcpp::List innerControl;
  innerControl["inTol"] = control["inTol"];
  innerControl["inMaxIter"] = control["inMaxIter"];
  double gamma0 = control["gamma0"], elalpha = control["elnetAlpha"];
  double spu = control["spU"], spv = control["spV"];

  arma::mat XX = X.t()*X;
  arma::mat XY = X.t()*Y;

  int chuk = 0, chvk = 0;
  //arma::vec Wu = pow(abs(uk),-1.0*gamma0),Wv = pow(abs(vk),-1.0*gamma0);
  //double Wd  = pow(dk,-1.0*gamma0);
  arma::vec Wu = wpow(uk,gamma0),Wv = wpow(vk,gamma0);
  double Wd; if(dk==0) Wd=0; else Wd  = pow(dk,-1.0*gamma0);

  arma::mat lamMat = XY / (Wu*Wd*Wv.t()); //wdiv(XY , (Wu*Wd*Wv.t()));
  arma::mat ablamMat = abs(lamMat);
  uvec indxx = find(ablamMat == ablamMat.max());indxx =indxx(0);
  double lmax = (lmaf*ablamMat.max())/elalpha;
  double lmin = lmax*lmif;
  arma::vec lamSeq =  exp(linspace<vec>(log(lmax),  log(lmin), nlambda));

  //uvec tss =  ind2sub( size(ablamMat), as_scalar(indxx) );
  uk.zeros();vk.zeros();
  //uk(tss(0)) = 1; vk(tss(1)) = 1; vk = vk*sign(lamMat(indxx));
  uk(as_scalar(indxx)%p) = 1; vk(as_scalar(indxx)/p) = 1; vk = vk*sign(lamMat(indxx));


  arma::vec XuYk = XY*vk;
  arma::vec XvYk = XY.t()*uk;

  arma::vec XvXv = naInd.t()*square(X*uk);
  arma::mat XuXu = X.t()*(X.each_col()% (naInd*square(vk)) );


  arma::mat Uklam,Vklam, BIClam;
  arma::vec Dklam,lselectSeq,execTime;
  Uklam.zeros(p,nlambda+1);Vklam.zeros(q,nlambda+1);BIClam.zeros(4,nlambda+1);
  Dklam.zeros(nlambda+1);  lselectSeq.zeros(nlambda+1);execTime.zeros(nlambda+1);
  int outMaxIter = control["outMaxIter"],ii=0,i,j;
  double lam, SSE, df,lamv2,elp;
  double svk=0,suk=0,err,outTol = control["outTol"];
  arma::vec lamv1,lambdaV1,lambdaV2,lamu1,lambdaU1,lambdaU2,Vkest,Ukest;
  arma::mat ck,cc;
  // uvec status2;
  SSE = log(accu(square(Y))) ;
  BIClam(0,ii) = SSE; //BIC
  BIClam(1,ii) = SSE; //BICP
  BIClam(2,ii) = SSE; //GIC
  BIClam(3,ii) = SSE; //AIC
  wall_clock timer;

  for(j = 0; j < nlambda; j++){
    lam = lamSeq(j);
    ck = uk*dk*vk.t();
    lamv1 = elalpha*lam*Wd*Wu;
    lamv2 = 2*(1-elalpha)*lam;
    lambdaV1 = Wv*(lamv1.t()*abs(uk));
    lambdaV2 = lamv2*ones<vec>(q)*(uk.t()*uk);

    lamu1 =elalpha*lam*Wd*Wv;
    lambdaU1 = Wu*(lamu1.t()*abs(vk));
    lambdaU2 = lamv2*ones<vec>(p)*(vk.t()*vk);

    timer.tic();
    for(i = 1; i < outMaxIter; i++){


        // U-Step
        if(chvk==1){
            XuYk = XY*vk;
            XuXu = X.t()*(X.each_col()% (naInd*square(vk)));// there is another way of dealing with it
            // XuXu = XX*as_scalar(vk.t()*vk);
            lambdaU1 = Wu*(lamu1.t()*abs(vk));
            lambdaU2 = lamv2*ones<vec>(p)*(vk.t()*vk);
        }

        Ukest = bregpcdenet_Rcpp(XuYk, XuXu, Au/dk, bu,lambdaU1, lambdaU2,mu, nu, uk*dk, innerControl);

        Ukest.elem( find(abs(Ukest) < 1e-7) ).zeros();
        suk = accu(abs(Ukest));
        // Vk.est[abs(Vk.est)<1e-7] <- 0 ; s.vk <- accu(abs(Vk.est))
        if (suk == 0) {
            chuk = 0;lselectSeq(0) = lam;break;
        } else {
            dk = as_scalar(sqrt(accu(square(X*Ukest))))/sqrt((double) n);    //as_scalar(sqrt(Ukest.t()*XX*Ukest ));
            uk = Ukest/dk;
            chuk = 1;
        }

        // V-step
        if(chuk==1){
            XvXv = naInd.t()*square(X*uk);
            // XvXv = ones<vec>(q) * (uk.t()*XX*uk);
            XvYk = XY.t()*uk;
            lambdaV1 = Wv*(lamv1.t()*abs(uk));
            lambdaV2 = lamv2*ones<vec>(q)*(uk.t()*uk);
        }

        Vkest = bregpcdenetdiag_Rcpp(XvYk, XvXv, Av/dk, bv,lambdaV1, lambdaV2,mu, nu, vk*dk, innerControl);

        Vkest.elem( find(abs(Vkest) < 1e-7) ).zeros();
        svk = accu(abs(Vkest));
        // Vk.est[abs(Vk.est)<1e-7] <- 0 ; s.vk <- accu(abs(Vk.est))
        if (svk == 0) {
            chvk = 0;lselectSeq(0) = lam;break;
        } else {
            dk = as_scalar(sqrt( Vkest.t()*Vkest));
            vk = Vkest/dk;
            chvk = 1;
        }


      cc  = uk*dk*vk.t(); //<- Dk * tcrossprod(Uk ,Vk)
      err = norm( cc-ck,"fro")/norm(ck,"fro");
      if (err < outTol) {break;} else {ck = cc ;}
    }

    elp = timer.toc();
    //  add solution path information

    if (svk > 0)
      if( suk > 0){
        ii = ii+1;
        Uklam.col(ii) = uk;
        Vklam.col(ii) = vk;
        Dklam(ii) = dk;
        execTime(ii) = elp;
        // exe.time <- c(exe.time,as.numeric(difftime(en.time,st.time,units = "secs")) )
        SSE = log(accu(square((Y-X*cc) % naInd) ));// accu(square(Y-X*cc));
        df = accu(uk != 0) + accu(vk != 0) -1;
        lselectSeq(ii) = lam;

        BIClam(0,ii) = SSE + (df*log((double) q*n))/(q*n); //BIC
        BIClam(1,ii) = SSE + 2*df*log((double) p*q)/(q*n); //BICP
        BIClam(2,ii) = SSE + log(log((double) n*q))*df*log((double) p*q)/(q*n); //GIC 
        BIClam(3,ii) = SSE + 2/q/n*(df); //AIC
      }

      if( (i>1) && ((nzcount(uk) > (p*spu)) || (nzcount(vk) > (q*spv)))  ) {break;}

  }


  out["ukpath"] = Uklam;
  out["vkpath"] = Vklam;
  out["dkpath"] = Dklam;
  out["ICkpath"] = BIClam;
  out["lamkpath"] = lselectSeq;
  out["ExecTimekpath"] = execTime;
  out["nkpath"] = ii+1;
  //   out["x1"] = lamMat;
  //   out["x2"] = Wu;
  //   out["x3"] = Wd;
  //   out["x4"] = Wv;
  return(out);

}




