// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <cmath>

using namespace Rcpp;
using namespace std;

//' @title Multivariate Periodogram.
//' @description \code{Peri} calculates the periodogram of a multivariate time series.
//' @details Returns an array of dimension \code{c(q,q,floor(T/2))}. 
//' @param X (Txq) data matrix.
//' @examples
//' series<-FI.sim(T=1000,q=2,rho=0.7,d=c(0.4,0.4))
//' peri<-Peri(series)
//' par(mfrow=c(2,2))
//' for(i in 1:2){
//' for(j in 1:2){
//' plot(Re(peri[i,j,]), type="h")
//' lines(Im(peri[i,j,]), col=2)
//' }}
//' @export
// [[Rcpp::export]]
arma::cx_cube Peri(arma::mat X){
    int ncols=X.n_cols;
    int nrows=X.n_rows;
    int q=std::min(ncols,nrows);
    int T=std::max(ncols,nrows);
    int n=floor(T/2);
    arma::mat Xt(q,T);
    if(q==ncols){Xt=X.t();}else{Xt=X;}
    arma::vec lambdaj=2*M_PI/T*arma::linspace(1,n,n);
    arma::cx_mat weights(T,n);
    for(int i=0; i<T; i++){
      for(int j=0; j<n; j++){
        /* arma::cx_double help=1i*(i+1)*lambdaj(j); */
        std::complex<double> help(0, (i+1)*lambdaj(j));
        weights(i,j)=std::exp(help);
      }}
    arma::cx_mat w=1/std::sqrt(2*M_PI*T)*Xt*weights;      
    arma::cx_cube perX(q,q,n);
    for(int i=0; i<n; i++){
      arma::cx_vec w_aux=w.col(i);
      perX.slice(i)=w_aux*w_aux.t();
    }
    return perX;
}


//' Calculate matrix Lambda_j(d) as part of the spectral density
//' @keywords internal
// [[Rcpp::export]]
arma::cx_cube Lambda_j(int q, int n, int T, arma::vec d_vec){
  
  arma::vec lambdaj=2*M_PI/T*arma::linspace(1,n,n);
  arma::cx_cube Lambda(q,q,n, arma::fill::zeros); 
  /* arma::cx_double phase;  */
  for(int j=0; j<n; j++){
    for(int a=0; a<q; a++){ 
      /* phase=1i*(M_PI-lambdaj(j))*d_vec(a)/2; */
        std::complex<double> phase(0, (M_PI-lambdaj(j))*d_vec(a)/2);
        Lambda.slice(j)(a,a)=pow(lambdaj(j),-d_vec(a))*exp(phase);      
    }
  }
  return Lambda;
}


//' Armadillo has trouble inverting complex matrices. This is circumvented by this function.
//' @keywords internal
// [[Rcpp::export]]
arma::cx_mat invert(arma::cx_mat X){
  arma::mat A=arma::real(X);
  arma::mat B=arma::imag(X);
  double b=arma::sum(arma::vectorise(arma::abs(B)));
  if(b>0){
    arma::mat invA=pinv(A); // Implementation uses Monrose-Pearson Pseudoinverse instead of regular inverse A.i() to avoid instabilities.
    arma::mat invB=pinv(B); //
    arma::mat help1=pinv((A+B*invA*B));
    arma::mat help2=-pinv((B+A*invB*A));
    arma::cx_mat invX= arma::cx_mat(help1,help2);
    return invX;
  }else{
    arma::cx_mat invX=arma::cx_mat(pinv(A),B);
    return invX;
  }
}

//' @title Estimation of G matrix for multivariate long memory processes.
//' @description \code{G.hat} Estimates the matrix G of a multivariate long memory process 
//'              based on an estimate of the vector of memory parameters. The assumed spectral 
//'              density is that of Shimotsu (2007).
//' @param peri cube containing the periodogram of the multivariate process X.
//' @param Lambda_cube  cube containing the Lambda matrices.
//' @param d_vec q-dimensional data vector.
//' @param m bandwith parameter specifying the number of Fourier frequencies
//' used for the estimation usually \code{floor(1+T^delta)}, where 0<delta<1.
//' @param q dimension of the process.
//' @references Shimotsu, K. (2007): Gaussian semiparametric estimation of multivariate
//' fractionally integrated processes. Journal of Econometrics, Vol. 137, No. 2, pp. 277 - 310.
//' @keywords internal
// [[Rcpp::export]]
arma::mat G_hat_cpp(arma::cx_cube peri, arma::cx_cube Lambda_cube, arma::vec d_vec, int m, int l, int q){
  arma::mat aux=arma::zeros(q,q);
  for(int j=(l-1); j<m; j++){
   aux+=arma::real(invert(Lambda_cube.slice(j))*peri.slice(j)*invert(Lambda_cube.slice(j).t()));
  }
  return aux/(m-l);
} 


//' Profiled Likelihood for GSE
//' @keywords internal
// [[Rcpp::export]]
double R_d_multi_GSE(arma::vec d_vec, arma::cx_cube PERI, int m, int l, int T, int q){
  int n=floor(T/2);
  arma::vec lambdaj=2*M_PI/T*arma::linspace(1,n,n);
  arma::cx_cube L=Lambda_j(q,n,T,d_vec);
  double G=log(arma::det(G_hat_cpp(PERI, L, d_vec, m, l, q)));
  double minus=2*arma::sum(d_vec)*arma::sum(log(lambdaj(arma::span((l-1),m-1))))/(m-l);
  double erg=G-minus;
  return erg;
}



//' Profiled Likelihood for GSE under Cointegration
//' @keywords internal
// [[Rcpp::export]]
double R_d_multi_GSE_coint(arma::vec theta_vec, arma::cx_cube PERI, int m, int l, int T, int q, arma::vec elements){
  //int ncols=X.n_cols;
  //int nrows=X.n_rows;
  //int q=std::min(ncols,nrows);
  //int T=std::max(ncols,nrows);
  int n=floor(T/2);
  int ncoint=elements.n_elem-1;
  arma::vec d_vec=theta_vec.subvec((ncoint),(ncoint+q-1));
  //d_vec.print();
  //arma::mat Xt(q,T);
  //if(q==ncols){Xt=X.t();}else{Xt=X;}
  arma::mat B(q,q, arma::fill::eye);
  //theta_vec.print();
  //d_vec.print();
  for(int i=1; i<(ncoint+1); i++){
    B((elements(0)-1),(elements(i)-1))=-theta_vec((i-1));
  }
  //B.print();
  //arma::mat Xt2(q,T);
  //for (int t=0; t<T; t++){
  //  Xt2.col(t)=B*Xt.col(t);
  //}
  arma::vec lambdaj=2*M_PI/T*arma::linspace(1,n,n);
  arma::cx_cube L=Lambda_j(q,n,T,d_vec);
  arma::cx_cube PERI2=PERI;
  for (int j=0; j<n; j++){
    PERI2.slice(j)=B*PERI.slice(j)*B.t();
  }
  double G=log(arma::det(G_hat_cpp(PERI2, L, d_vec, m, l, q)));
  double minus=2*arma::sum(d_vec)*arma::sum(log(lambdaj(arma::span((l-1),m-1))))/(m-l);
  double erg=G-minus;
  return erg;
}




//' @title Helper function for MLWS test for multivariate spurious long memory.
//' @description Multivariate local Whittle Score type test for the null hypothesis of true long
//'              memory against the alternative of spurious long memory suggested by Sibbertsen, 
//'              Leschinski and Holzhausen (2015).
//' @details
//' add details here
//' @param X data matrix
//' @param d_vec estimated vector of memory parameters.
//' @param m bandwith parameter specifying the number of Fourier frequencies used 
//' for the estimation usually \code{floor(1+T^delta)}, where 0.5<delta<0.8 for consistency.
//' @param epsilon trimming parameter \code{epsilon=0.05} by default. Determines
//' minimum number of Fourier frequencies used in test statistic. For T>500 it is recommended 
//' to use \code{epsilon=0.02}. Confer Sibbertsen, Leschinski, Holzhausen (2015) for further details.
//' @param eta weight vector.
//' @references Sibbertsen, P., Leschinski, C. H., Holzhausen, M., (2015): A Multivariate Test 
//'              Against Spurious Long Memory. Hannover Economic Paper.
//' @export
// [[Rcpp::export]]
double W_multi(arma::mat X, arma::vec d_vec, int m, double epsilon, arma::vec eta){
  int ncols=X.n_cols;
  int nrows=X.n_rows;
  int q=std::min(ncols,nrows);
  int T=std::max(ncols,nrows);
  int n=floor(T/2);
  int l=1;
  arma::mat Xt(q,T);
  if(q==ncols){Xt=X.t();}else{Xt=X;}
  arma::cx_cube L=Lambda_j(q,n,T,d_vec);
  arma::cx_cube PERI=Peri(Xt);
  arma::mat Ginv=G_hat_cpp(PERI, L, d_vec, m, l, q).i();
  arma::vec lambdaj=2*M_PI/T*arma::linspace(1,n,n);
  arma::vec nuj=log(lambdaj(arma::span(0,m-1)))-arma::mean(log(lambdaj(arma::span(0,m-1))));
  arma::cube ia(q,q,q,arma::fill::zeros);
  arma::vec r=arma::linspace(epsilon,1,m);
  arma::vec W_grid(m, arma::fill::zeros);
  arma::vec nuj2=arma::square(nuj);
  double Snuj2=arma::sum(nuj2);
  for(int i=0; i<m; i++){
    int mr=ceil(m*r(i));
    double aux1=0;
    for (int a=0; a<q; a++){
      double aux2=0;
      double weight=eta(a);
      for (int j=0; j<mr; j++){
        arma::cx_mat help_mat=invert(L.slice(j))*PERI.slice(j)*invert(L.slice(j).t());
        arma::mat help_mat_real=arma::real(help_mat);
        arma::mat help_mat_imag=arma::imag(help_mat);
        double help2=arma::as_scalar(Ginv.row(a)*help_mat_real.col(a));
        double help3=arma::as_scalar(Ginv.row(a)*help_mat_imag.col(a));
        aux2+=2*weight*(arma::as_scalar(nuj(j))*(help2-1))+weight*(lambdaj(j)-M_PI)/2*help3;
        }
      aux1+=aux2;
    }
    W_grid(i)=aux1/2*1/sqrt(Snuj2);    
  }
  double stat=arma::max(arma::abs(W_grid));
  return stat;
}


//' @title Helper function that returns AR-representation of FI(d)-process.
//' @description returns the first \code{n} coefficients in the AR-infinity representation
//' of an FI(d) process
//' @param n number of coefficients to be returned
//' @param d memory parameter 
//' @export
// [[Rcpp::export]]
arma::vec ddiffw(int n,double d){
  arma::vec w(n+1, arma::fill::zeros);
  w(0)=1;
  w(1)=-d;
  for(int i=2; i<(n+1); i++){
    w(i)=w(i-1)*((i-1)-d)/i;
  }
  return w;
}

//' calculate residuals
//' @inheritParams ll.VARFIMA
//' @keywords internal
// [[Rcpp::export]]
arma::mat ll_inner(arma::mat data, arma::mat pre_sample, int T, arma::vec d_vec, double phi, arma::mat THETA, int q, int approx){
// Find VAR(\infty) Representation of fractional differencing
  arma::cube D(q,q,approx, arma::fill::zeros);
  for(int a=0; a<q; a++){
    D.tube(a,a)=-ddiffw((approx-1),d_vec(a));
  }
  D.slice(0).eye();
  arma::mat IDENT(q,q, arma::fill::eye);
  arma::mat PHI=IDENT*phi;

  arma::cube PHID_a=D;
  arma::cube PHID_b(q,q,approx, arma::fill::zeros);

  PHID_b.slice(0)=-PHI;
  for(int i=1; i<approx; i++){
    PHID_b.slice(i)=PHI*D.slice(i);
  }
  arma::cube PHID(q,q,approx, arma::fill::zeros);
  PHID.slice(0).eye();
  for(int i=1; i<approx; i++){
    PHID.slice(i)=PHID_a.slice(i)-PHID_b.slice(i-1);
  }

  arma::cube A(q,q,approx, arma::fill::zeros);
  A.slice(0).eye();
  arma::mat help_mat(q,q,arma::fill::eye);
  for(int i=1; i<approx; i++){
    help_mat*=THETA;
    A.slice(i)=-help_mat;
  }

  PHID.slice(0)*=(-1);
  A.slice(0)*=(-1);

  arma::cube PI_mat(q,q,approx, arma::fill::zeros);

  for(int i=0; i<(approx); i++){
    arma::mat coef(q,q, arma::fill::zeros);
    for(int j=0; j<(i+1); j++){
      arma::mat auxvar=A.slice(j)*PHID.slice(((i)-j));
      coef+=auxvar;
    }
  PI_mat.slice(i)=-coef;
  }

  PI_mat.slice(0).eye();

  arma::mat series((T+approx),q,arma::fill::zeros);

  series.submat(arma::span(0, (approx-1)), arma::span(0, (q-1)))=pre_sample;
  series.submat(arma::span(approx, (T+approx-1)), arma::span(0, (q-1)))=data;

  arma::mat residuals_pre=series;

  for(int t=0; t<T; t++){
    arma::vec aux=PI_mat.slice(0)*series.row(approx+t).t();
    for(int t2=1; t2<approx; t2++){
      aux-=PI_mat.slice(t2)*series.row(approx+t-t2).t();
    }
  residuals_pre.row(approx+t)=aux.t();
  }
  arma::mat residuals=residuals_pre.submat(arma::span(approx,approx+T-1), arma::span(0, (q-1)));
  return residuals;
}


//' some comment
//' @keywords internal
// [[Rcpp::export]]
arma::cx_vec cumsumcpp(arma::cx_vec input){
  int ncols=input.n_cols;
  int nrows=input.n_rows;
  int T=std::max(ncols,nrows);
  arma::cx_vec out(T, arma::fill::zeros);
  for(int i=0; i<T; i++){
  out(i)=arma::as_scalar(arma::sum(input(arma::span(0,i))));
  }
  return out;
}

//' some comment
//' @keywords internal
// [[Rcpp::export]]
arma::cx_vec repcx(arma::cx_double num, int q){
  arma::cx_vec out(q);
  for(int i=0; i<q; i++){
    out(i)=num;
  }
  return(out);
}


//' some comment
//' @keywords internal
// [[Rcpp::export]]
double simone(arma::mat G, arma::vec eta, double epsilon, double T){
  
  int q=G.n_cols;
  arma::vec s=arma::linspace(1,T,T)/T;
  arma::vec dBs=arma::randn<arma::mat>(T,1)/sqrt(T);
  double Bs=arma::sum(dBs);
  arma::vec helplog=(1+log(s));

  arma::vec Term1help1=helplog*sqrt(arma::as_scalar(2*eta.t()*eta)+arma::as_scalar(2*eta.t()*(G%inv(G))*eta));
  double inner=arma::as_scalar((2*eta.t()*(G%inv(G))*eta-2*eta.t()*eta));
  arma::cx_double helphelp(0,M_PI/2*sqrt(inner));
  
  //Rcpp::Rcout << helphelp <<  std::endl;

  arma::cx_vec Term1help2=repcx(helphelp,T);
  //Term1help2.print();
  arma::cx_vec Term1help3=Term1help1+Term1help2;

  arma::cx_vec Term1help4=(Term1help3)%dBs;
  arma::cx_vec Term1=cumsumcpp(Term1help4);
  
  arma::cx_vec helplogcx=arma::conv_to<arma::cx_vec>::from(helplog);
  arma::cx_vec Term2=arma::as_scalar(2*eta.t()*eta*Bs)*cumsumcpp(helplogcx)/T;

  arma::mat Real=G%inv(G)+arma::eye(q,q);
  arma::mat Imag=G%inv(G)-arma::eye(q,q);
  arma::mat Omega=2*(Real+(pow(M_PI,2)/4)*Imag);
  
  arma::cube Fterm(q,q,T,arma::fill::zeros);
  arma::mat helpF(q,q,arma::fill::zeros);
  for(int i=0; i<T; i++){
    arma::mat newterma=(pow(helplog(i),2)*Real+pow(M_PI,2)/4*Imag);
    arma::mat newterm=2/T*newterma;
    helpF+=newterm;
    Fterm.slice(i)=helpF;
  }
  
  arma::cx_vec Term3(T);
  arma::cx_vec help3(T);
  double help4;
    
  for(int i=0; i<T; i++){
    help4=arma::as_scalar(2*eta.t()*Fterm.slice(i)*inv(Omega)*Imag*inv(Omega).t()*Fterm.slice(i).t()*eta);
    double help4c=M_PI/2*sqrt(help4);
    arma::cx_double help5(0,help4c);
    arma::cx_vec help6=helplogcx%repcx(sqrt(arma::as_scalar(2*eta.t()*Fterm.slice(i)*inv(Omega)*Real*inv(Omega).t()*Fterm.slice(i).t()*eta)),T);
    help3=(help6+repcx(help5,T))%dBs;
    Term3(i)=sum(help3);
  }
  arma::vec collapseterms=arma::abs(Term1-Term2-Term3);
  int start=(floor(epsilon*T));
  int end=(T-1);
  double supa=arma::max(collapseterms.subvec(start,end));
  double sup=supa/2;
  //return Rcpp::List::create(Rcpp::Named("sup")= sup, Rcpp::Named("Term1")= Term1, Rcpp::Named("Term2")= Term2, Rcpp::Named("Term3")= Term3);
  return(sup);
}

//return List::create(Named("result") = mat,
//Named("original") = orig);


//' some comment
//' @title simulate limit distribution of test statistic
//' @keywords internal
// [[Rcpp::export]]
arma::vec simMLWS(arma::mat G, arma::vec eta, double epsilon, int T, int M){
  arma::vec out(M);
  for(int i=0; i<M; i++){  
    out(i)=simone(G, eta, epsilon, T);
  }
  return out;
}



