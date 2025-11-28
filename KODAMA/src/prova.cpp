#define R_NO_REMAP

#include <map>
#include <vector>
#include <iostream>
#include <fstream>

//#include <math.h>           // math routines
#include <stdio.h>
//#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>


#include <R.h>              // R header


#include <RcppArmadillo.h>
#include "irlba.h"


#define Malloc(type,n) (type *)malloc((n)*sizeof(type))


#ifdef _OPENMP
#include <omp.h>
#endif

// RAND_MAX (used in 'uniform_negative') is a constant defined in <cstdlib> ( see: http://www.cplusplus.com/reference/cstdlib/rand/ )
#include <cstdlib>


using namespace std;
using namespace Rcpp;





arma::mat pls_light(arma::mat Xtrain,arma::mat Ytrain,arma::mat Xtest,int ncomp) {
  

  // Xtrain <- scale(Xtrain,center=TRUE,scale=FALSE)
  // Xtest <-scale(Xtest,center=mX)
  arma::mat mX=mean(Xtrain,0);
  Xtrain.each_row()-=mX;
  
  // Y <- scale(Ytrain,center=TRUE,scale=FALSE)
  arma::mat mY=mean(Ytrain,0);
  Ytrain.each_row()-=mY;
  
  // S <- crossprod(X,Y)
  arma::mat S=trans(Xtrain)*Ytrain;

  arma::mat svd_u;
  arma::vec svd_s;
  arma::mat svd_v;
  int Snr=S.n_rows;
  int Snc=S.n_cols;
  if(Snr>5  && Snc>5){

    int nu=ncomp;
    int work=ncomp+10;
    int maxit=2000;
    double tol=1e-6;
    double eps=1e-9;
    double svtol=1e-6;
    
      int m = S.n_rows;
  int n = S.n_cols;
  int iter, mprod;
  int lwork = 7 * work * (1 + work);

  arma::vec s = arma::randn<arma::vec>(nu);
  arma::mat U = arma::randn<arma::mat>(m, work);
  arma::mat V = arma::randn<arma::mat>(n, work);

  arma::mat V1 = arma::zeros<arma::mat>(n, work); // n x work
  arma::mat U1 = arma::zeros<arma::mat>(m, work); // m x work
  arma::mat  W = arma::zeros<arma::mat>(m, work);  // m x work  input when restart > 0
  arma::vec F  = arma::zeros<arma::vec>(n);     // n
  arma::mat B  = arma::zeros<arma::mat>(work, work);  // work x work  input when restart > 0
  arma::mat BU = arma::zeros<arma::mat>(work, work);  // work x work
  arma::mat BV = arma::mat(work, work);  // work x work
  arma::vec BS = arma::zeros<arma::vec>(work);  // work
  arma::vec BW = arma::zeros<arma::vec>(lwork); // lwork
  arma::vec res = arma::zeros<arma::vec>(work); // work
  arma::vec T = arma::zeros<arma::vec>(lwork);  // lwork
  arma::vec svratio = arma::zeros<arma::vec>(work); // work


  irlb (S.memptr(), NULL, 0, m, n, nu, work, maxit, 0,
          tol, NULL, NULL, NULL,
          s.memptr(), U.memptr(), V.memptr(), &iter, &mprod,
          eps, lwork, V1.memptr(), U1.memptr(), W.memptr(),
          F.memptr(), B.memptr(), BU.memptr(), BV.memptr(),
          BS.memptr(), BW.memptr(), res.memptr(), T.memptr(),
          svtol, svratio.memptr());

    
    svd_u=U.cols(0, nu-1);             //as<arma::mat>(temp0("u"));   //u
    svd_v=V.cols(0,nu-1);    //as<arma::mat>(temp0("v"));   //v
  }else{ 
    svd_econ(svd_u,svd_s,svd_v,S,"both");
  }
  arma::mat svd_u_mc = svd_u.cols(0,ncomp-1);
  arma::mat svd_v_mc = svd_v.cols(0,ncomp-1);
  
  // TT <- Xtrain %*% R
  // U <- Ytrain %*% Q
  arma::mat T=Xtrain*svd_u_mc;
  arma::mat U=Ytrain*svd_v_mc;
  
  //B <- R %*% ( tcrossprod(solve(crossprod(TT)), TT)%*%U ) %*% t(Q)
  arma::mat Tt=T.t();
  arma::mat B = svd_u_mc * (arma::inv(Tt * T) * Tt * U) * svd_v_mc.t();
  
  Xtest.each_row()-=mX;
  
  // Ypred <- scale(Xtest %*% B, -meanY, FALSE)
  arma::mat Ypred=Xtest * B;
  Ypred.each_row()+=mY;
  
  return Ypred;

}





// use base R's set.seed() in Rcpp for RNG
// http://thecoatlessprofessor.com/programming/set_rs_seed_in_rcpp_sequential_case
//

// [[Rcpp::export]]
void set_seed(int seed) {
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed_r = base_env["set.seed"];
  set_seed_r(seed);
}




// This function performs a random selection of the elements of a vector "yy".
// The number of elements to select is defined by the variable "size".

IntegerVector samplewithoutreplace(IntegerVector yy,int size){
  IntegerVector xx(size);
  int rest=yy.size();
  int it;
  for(int ii=0;ii<size;ii++){
    it=unif_rand()*rest;
    xx[ii]=yy[it];
    yy.erase(it);
    rest--;
  }
  return xx;
}



arma::uvec which(LogicalVector x) {
  int a=std::accumulate(x.begin(),x.end(), 0.0);

  arma::uvec w(a);
  int counter=0;
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == 1){
      w[counter]=i;
      counter++;
    }
  }
    return w;
}


arma::mat variance(arma::mat x) {
  int nrow = x.n_rows, ncol = x.n_cols;
  arma::mat out(1,ncol);
  
  for (int j = 0; j < ncol; j++) {
    double mean = 0;
    double M2 = 0;
    int n=0;
    double delta, xx;
    for (int i = 0; i < nrow; i++) {
      n = i+1;
      xx = x(i,j);
      delta = xx - mean;
      mean += delta/n;
      M2 = M2 + delta*(xx-mean);
    }
    out(0,j) = sqrt(M2/(n-1));
  }
  return out;
}


List scalecpp(arma::mat Xtrain,arma::mat Xtest,int type){
  arma::mat mX=mean(Xtrain,0);
  Xtrain.each_row()-=mX;
  Xtest.each_row()-=mX;
  arma::mat vX=variance(Xtrain); 
  if(type==2){
    Xtrain.each_row()/=vX;
    Xtest.each_row()/=vX;  
  }
  return List::create(
    Named("Xtrain") = Xtrain,
    Named("Xtest")   = Xtest,
    Named("mean")       = mX,
    Named("sd")       = vX
  ) ;
}


double accuracy(arma::ivec cl,arma::ivec cvpred){
  double acc=0;
  for(unsigned i=0;i<cl.size();i++){
    if(cl[i]==cvpred[i])
      acc++;
  }
  acc=acc/cl.size();
  return acc; 
}


// [[Rcpp::export]]
arma::mat floyd(arma::mat data){
  int n=data.n_cols;
  int i,j,k;
  double temp;
  arma::mat A=data;
  for (i=0; i<n; i++)
    A(i,i) = 0;           
  for (k=0; k<n; k++){
    for (i=0; i<n; i++){
      for (j=0; j<n; j++){
        temp=A(i,k)+A(k,j);
        if (temp < A(i,j))
        {
          A(i,j) = temp;
          
        }
      }
    }
  }
      return A;
}



   



arma::mat simpls_light(arma::mat Xtrain,arma::mat Ytrain,arma::mat Xtest,int ncomp) {
  
  // n <-dim(Xtrain)[1]
  int n = Xtrain.n_rows;
  
  // p <-dim(Xtrain)[2]
  int p = Xtrain.n_cols;
  
  // m <- dim(Y)[2]
  int m = Ytrain.n_cols;
  
  // w <-dim(Xtest)[1]
  int w = Xtest.n_rows;
  
  // arma::mat mm=a*b;
  
  //X=Xtrain
  arma::mat X=Xtrain;
  
  
  // X <- scale(Xtrain,center=TRUE,scale=FALSE)
  // Xtest <-scale(Xtest,center=mX)
  arma::mat mX=mean(Xtrain,0);
  X.each_row()-=mX;
  Xtest.each_row()-=mX;

  arma::mat Y=Ytrain;
  
  // Y <- scale(Ytrain,center=TRUE,scale=FALSE)
  arma::mat mY=mean(Ytrain,0);
  Y.each_row()-=mY;
  
  // S <- crossprod(X,Y)
  arma::mat S=trans(X)*Y;
  
  //  RR<-matrix(0,ncol=ncomp,nrow=p)
  arma::mat RR(p,ncomp);
  RR.zeros();
  
  //  PP<-matrix(0,ncol=ncomp,nrow=p)
  arma::mat PP(p,ncomp);
  PP.zeros();
  
  //  QQ<-matrix(0,ncol=ncomp,nrow=m)
  arma::mat QQ(m,ncomp);
  QQ.zeros();
  
  //  TT<-matrix(0,ncol=ncomp,nrow=n)
  arma::mat TT(n,ncomp);
  TT.zeros();
  
  //  VV<-matrix(0,ncol=ncomp,nrow=p)
  arma::mat VV(p,ncomp);
  VV.zeros();
  
  //  UU<-matrix(0,ncol=ncomp,nrow=n)
  arma::mat UU(n,ncomp);
  UU.zeros();
  
  //  B<-matrix(0,ncol=m,nrow=p)
  arma::mat B(p,m);
  B.zeros();
  
  // Ypred <- matrix(0,ncol=m,nrow=n)
  //arma::cube Ypred(w,m,ncomp);                ///////////////////////////////////////////
  arma::mat Ypred(w,m);
  Ypred.zeros();                  ///////////////////////////////////////////
  
  
  arma::mat qq;
  arma::mat pp;
  arma::mat svd_U;
  arma::vec svd_s;
  arma::mat svd_V;
  arma::mat rr;
  arma::mat tt;
  arma::mat uu;
  arma::mat vv;

  // for(a in 1:ncomp){
  int a;
  for (a=0; a<ncomp; a++) {
    //qq<-svd(S)$v[,1]
    //rr <- S%*%qq    

    svd_econ(svd_U,svd_s,svd_V,S,"left");

    rr=svd_U.col( 0 );
    // tt<-scale(X%*%rr,scale=FALSE)
    tt=X*rr; 
    arma::mat mtt=mean(tt,0);
    tt.each_row()-=mtt;
    //tnorm<-sqrt(sum(tt*tt))
    double tnorm=sqrt(sum(sum(tt%tt)));
    
    //tt<-tt/tnorm
    tt/=tnorm; 
    //rr<-rr/tnorm
    rr/=tnorm;
    // pp <- crossprod(X,tt)
    pp=trans(X)*tt;
    // qq <- crossprod(Y,tt)
    qq=trans(Y)*tt;
    
    //uu <- Y%*%qq
    uu=Y*qq;
    
    //vv<-pp
    vv=pp;
    if(a>0){
      //vv<-vv-VV%*%crossprod(VV,pp)
      vv-=VV*(trans(VV)*pp);

      //uu<-uu-TT%*%crossprod(TT,uu)
      uu-=TT*(trans(TT)*uu);
    }
    //vv <- vv/sqrt(sum(vv*vv))
    vv/=sqrt(sum(sum(vv%vv)));
    
    //S <- S-vv%*%crossprod(vv,S)
    S-=vv*(trans(vv)*S); 
    RR.col(a)=rr;
    TT.col(a)=tt;
    PP.col(a)=pp;
    QQ.col(a)=qq;
    VV.col(a)=vv;
    UU.col(a)=uu;
  } 
  B=RR*trans(QQ);
  Ypred=Xtest*B; 
  Ypred.each_row()+=mY;
  return Ypred;
  
}





// [[Rcpp::export]]
arma::mat transformy(arma::ivec y){
  int n=y.size();
  int nc=max(y);
  arma::mat yy(n,nc);
  yy.zeros();
  for(int i=0;i<nc;i++){
    for(int j=0;j<n;j++){
      yy(j,i)=((i+1)==y(j));
    }
  }
  
  return yy;
}






// [[Rcpp::export]]
arma::ivec PLSDACV_simpls(arma::mat x,arma::ivec cl,arma::ivec constrain,int k) {
  arma::mat clmatrix=transformy(cl);
  arma::mat Ytest(clmatrix.n_rows,clmatrix.n_cols);
  int xsa_t = max(constrain);
  IntegerVector frame = seq_len(xsa_t);
  IntegerVector v=samplewithoutreplace(frame,xsa_t);
  
  int mm=constrain.size();
  arma::ivec fold(mm);
  for (int i=0; i<mm; i++) 
    fold[i]=v[constrain(i)-1]%10;

  for (int i=0; i<10; i++) {
    arma::uvec w1,w9;
    arma::ivec temp;
    arma::mat Xtrain,Xtest;
    arma::mat Ytrain;
    
    w1=find(fold==i);
    w9=find(fold!=i);
    temp=unique(cl(w9));    //  I changed this temp=unique(cl(w1));
    if(temp.size()>1){
      Xtrain=x.rows(w9);
      Xtest=x.rows(w1);
      Ytrain=clmatrix.rows(w9);

      Ytest.rows(w1)=simpls_light(Xtrain,Ytrain,Xtest,k);

    }else{
      Ytest.rows(w1)=clmatrix.rows(w1);

    }
  }  
  
  int mm2=constrain.size();
  arma::ivec pp(mm2);
  
  //min_val is modified to avoid a warning
  double min_val=0;
  min_val++;
  arma::uvec ww;
  for (int i=0; i<mm2; i++) {
    ww=i;
    arma::mat v22=Ytest.rows(ww);
    arma::uword index;                                                                                                                                                                                                                                                                                                                
    min_val = v22.max(index);
    pp(i)=index+1;
  }
  return pp;
}



// [[Rcpp::export]]
arma::ivec PLSDACV_fastpls(arma::mat x,arma::ivec cl,arma::ivec constrain,int k) {
  
  
  arma::mat clmatrix=transformy(cl);
  
  arma::mat Ytest(clmatrix.n_rows,clmatrix.n_cols);
  
  int xsa_t = max(constrain);
  
  IntegerVector frame = seq_len(xsa_t);

  IntegerVector v=samplewithoutreplace(frame,xsa_t);
  
  
  int mm=constrain.size();
  arma::ivec fold(mm);
  for (int i=0; i<mm; i++) 
    fold[i]=v[constrain(i)-1]%10;
  
  
  
  for (int i=0; i<10; i++) {
    arma::uvec w1,w9;
    arma::ivec temp;
    arma::mat Xtrain,Xtest;
    arma::mat Ytrain;
    
    w1=find(fold==i);
    w9=find(fold!=i);
    temp=unique(cl(w9));    //  I changed this temp=unique(cl(w1));
    if(temp.size()>1){
      Xtrain=x.rows(w9);
      Xtest=x.rows(w1);
      Ytrain=clmatrix.rows(w9);

      Ytest.rows(w1)=pls_light(Xtrain,Ytrain,Xtest,k);

    }else{
      Ytest.rows(w1)=clmatrix.rows(w1);

    }
  }  
  
  int mm2=constrain.size();
  arma::ivec pp(mm2);
  
  //min_val is modified to avoid a warning
  double min_val=0;
  min_val++;
  arma::uvec ww;
  for (int i=0; i<mm2; i++) {
    ww=i;
    arma::mat v22=Ytest.rows(ww);
    arma::uword index;                                                                                                                                                                                                                                                                                                                
    min_val = v22.max(index);
    pp(i)=index+1;
  }
  return pp;
}








// [[Rcpp::export]]
List corecpp(arma::mat x,
             arma::mat xTdata,
             arma::ivec clbest,
             const int Tcycle,
             int FUN,
             int fparpls, 
             arma::ivec Xconstrain,
             NumericVector fix,
             bool shake,
             int proj) {
  
  arma::ivec cvpred=clbest;
  arma::ivec cvpredbest;
  arma::ivec clbest_dirty=clbest;
  
  if(FUN==1){
    cvpredbest=PLSDACV_fastpls(x,clbest,Xconstrain,fparpls);   

  }
  if(FUN==2){
    cvpredbest=PLSDACV_simpls(x,clbest,Xconstrain,fparpls);   
  }
  double accbest;
  if (shake == FALSE) {
    accbest = accuracy(clbest,cvpredbest);
  }
  else {
    accbest = 0;
  }
  
  bool success = FALSE;
  int j = 0;
  
  //Inizialization of the vector to store the values of accuracy

  double *vect_acc= new double[Tcycle];
  for(int ii=0;ii<Tcycle;ii++){
    vect_acc[ii]=-1;
  }
  
    
  arma::ivec sup1= unique(Xconstrain);
  int nconc=sup1.size();
  
  NumericVector Xconstrain2=as<NumericVector>(wrap(Xconstrain));
  NumericVector fix2=as<NumericVector>(wrap(fix));
  
  while (j < Tcycle && !success) {
    Rcpp::checkUserInterrupt();
    j++;
    arma::ivec cl = clbest; 
    arma::ivec cl_dirty = clbest_dirty;
    IntegerVector sup2=seq_len(nconc);

    int nn_temp=(unif_rand()*nconc)+1;
  

  //////////////////////////////////////////////////
  
  //////////////////////////////////////////////////

   cl_dirty=cl;
    
  // Constraining cleaning
    
    IntegerVector ss=samplewithoutreplace(sup2,nn_temp);

    for (int* k = ss.begin(); k != ss.end(); ++k) {
      LogicalVector sele=((Xconstrain2 == *k) & (fix2!=1));
      double flag = std::accumulate(sele.begin(),sele.end(), 0.0);
      if (flag != 0) {
        arma::uvec whi=which(sele);
        arma::ivec uni=unique(cvpredbest.elem(whi));

        IntegerVector ss_uni=as<IntegerVector>(wrap(uni));

        int nn_uni=ss_uni.size();
        int nn_t=unif_rand()*nn_uni;
        IntegerVector soso(1);
        soso(0)=ss_uni(nn_t);   //Cambiato da soso[0]=ss_uni[nn_t]
        
        
        IntegerVector soso2=rep(soso,whi.size());

        cl.elem(whi)=as<arma::ivec>(soso2);
      }
    }

    if(FUN==1){
      cvpred=PLSDACV_fastpls(x,cl,Xconstrain,fparpls);  
    }
    if(FUN==2){
      cvpred=PLSDACV_simpls(x,cl,Xconstrain,fparpls);  
    }
    double accTOT= accuracy(cl,cvpred);
    if (accTOT > accbest) {
      cvpredbest = cvpred;
      clbest = cl;
      clbest_dirty=cl_dirty;
      accbest = accTOT;
    }
    
    vect_acc[j-1] = accbest;  //Cambiato da vect_acc[j] = accbest;
    if (accTOT == 1) 
      success = TRUE;
  }
  

  arma::vec vect_acc2(Tcycle);
  vect_acc2.fill(0);
  for(int ii=0;ii<Tcycle;ii++){
    vect_acc2(ii)=vect_acc[ii];
  }
  
  delete [] vect_acc;
  
  
  if(proj==2){
    arma::mat projmat;
    int mm2=xTdata.n_rows;
    arma::ivec pp(mm2); 

    if(FUN==1){
      arma::mat lcm=transformy(clbest);
      projmat=pls_light(x,lcm,xTdata,fparpls);
      
      //min_val is modified to avoid a warning
      double min_val=0;
      min_val++;
      arma::uvec ww;
      for (int i=0; i<mm2; i++) {
        ww=i;
        arma::mat v22=projmat.rows(ww);
        arma::uword index;                                                                                                                                                                                                                                                                                                                
        min_val = v22.max(index);
        pp(i)=index+1;
      }
    }
    if(FUN==2){
      arma::mat lcm=transformy(clbest);
      projmat=simpls_light(x,lcm,xTdata,fparpls);

      
      //min_val is modified to avoid a warning
      double min_val=0;
      min_val++;
      arma::uvec ww;
      for (int i=0; i<mm2; i++) {
        ww=i;
        arma::mat v22=projmat.rows(ww);
        arma::uword index;                                                                                                                                                                                                                                                                                                                
        min_val = v22.max(index);
        pp(i)=index+1;
      }
    }
    

    
    return List::create(Named("clbest") = clbest,
                        Named("clbest_dirty") = clbest_dirty,
                        Named("accbest") = accbest,
                        Named("vect_acc") = vect_acc2,
                        Named("vect_proj") = pp
    );
    
  }else{
    return List::create(Named("clbest") = clbest,
                        Named("accbest") = accbest,
                        Named("vect_acc") = vect_acc2
    );
  }
  
}









