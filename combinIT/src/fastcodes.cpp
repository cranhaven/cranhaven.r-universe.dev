#include <RcppArmadillo.h>
#include <cmath>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
//' @importFrom Rcpp sourceCpp
//' @useDynLib combinIT
//' 
// [[Rcpp::export]]
float Bfc(arma::mat x,int bl, int tr,int p) {
  arma::vec RowMean = arma::mean(x,1);
  arma::vec ColMean= trans(mean(x,0));
  double Mean = accu(x)/(tr*bl);
  arma::mat y(bl,tr),yt1(bl,tr),yt2(bl,tr);
  for(int i=0; i<bl;i++)
    {
      for(int j=0;j< tr;j++)
      {
       y(i,j) = x(i,j)-RowMean(i)-ColMean(j)+Mean; // Zahra: please correct it by yourself
      }
   }
 yt1 = y.t() * y;
 yt2 = yt1*yt1;
  float Boik = trace(yt1)*trace(yt1) / (p*trace(yt2));
  return Boik;
}
//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
arma::vec Bfsim(int nsim,int bl, int tr,int p){
  arma::mat sam(bl,tr);
  arma::vec out(nsim);
  for(int i=0;i<nsim;i++)
  {
    sam.randn(bl,tr);
    out(i)=Bfc(sam,bl,tr,p);
  }
  return out;
}
//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
double picf(arma::vec y,arma::mat kp,float c0){
  arma::vec z= kp * y;
  for(unsigned int i=0;i<kp.n_rows;i++)
    z(i)=fabs(z(i));
  arma::vec s0=median(z,0)/c0;
  arma::uvec ids = find(z <= (5*s0(0)) );
  arma::vec PSE=median(z.elem(ids),0);
  arma::vec PIC=max(z,0)/PSE(0);
  return PIC(0);
}

//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
arma::vec PICfsim(int nsim,arma::mat kp, float c0, int n){
  arma::vec sam(n);
  arma::vec out(nsim);
  for(int i=0;i<nsim;i++)
  {
    sam.randn(n);
    out(i)=picf(sam,kp,c0);
  }
  return out;
}
//' @importFrom Rcpp sourceCpp
//'
// [[Rcpp::export]]
double C0(arma::mat kp, int n,int nc0){
  arma::vec sim(nc0);
  arma::vec norsam(n);
  for(int i=0;i<nc0;i++)
  {
    arma::vec temp=kp*norsam.randn(n);
    for(unsigned int j=0;j<temp.n_rows;j++)
      temp(j) = fabs(temp(j));
    arma::vec me=median(temp,0);
    sim(i)=me(0);
  }
  arma::vec out = mean(sim,0);
  return out(0);
}
//' @importFrom Rcpp sourceCpp
//' @useDynLib combinIT
//' 
// [[Rcpp::export]]
double piephoC(arma::mat x,int bl, int tr) {
  arma::vec RowMean = arma::mean(x,1);
  arma::vec ColMean= arma::trans(arma::mean(x,0));
  double Mean = arma::as_scalar(arma::accu(x)/(tr*bl));
  arma::mat Res2(bl,tr);
  for(int i=0; i<bl;i++)
  {
    for(int j=0;j< tr;j++)
    {
      double Res = arma::as_scalar(x(i,j)-RowMean(i)-ColMean(j)+Mean);
      Res2(i,j) = Res*Res; 
    }
  }
  arma::vec RowSum = arma::sum(Res2,1);
  arma::vec delta = (bl*(bl-1)*RowSum-sum(RowSum));
  double h1 = 0;
  for(int i=0;i< (bl-1);i++)
    for(int j=i+1; j < bl ; j++)
      h1 += arma::as_scalar(delta(i)*delta(j));
  double U = arma::as_scalar(2*bl*h1/((bl-1)*pow(sum(delta),2)));
  double piepho = -(tr-1)*(bl-1)*(bl-2)*log(U)/2;
  return piepho;
}

//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
arma::vec Piephosim(int nsim,int bl, int tr){
  arma::mat sam(bl,tr);
  arma::vec out(nsim);
  for(int i=0;i<nsim;i++)
  {
    sam.randn(bl,tr);
    out(i)=piephoC(sam,bl,tr);
  }
  return out;
}

using namespace arma;
//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
double  M_f(arma::mat x) {            // Hossein's Codes....
  int bl = x.n_rows;
  int tr = x.n_cols;
  int n = bl*tr;
  arma::vec treatment = arma::repelem(arma::regspace(1,  bl), tr, 1);
  arma::vec y = arma::trans(x.as_row());
  arma::mat RES(bl,tr);
  arma::vec RowMean = arma::mean(x,1);
  arma::vec ColMean= trans(mean(x,0));
  double Mean = accu(x)/(tr*bl);
  for(int i=0; i<bl;i++)
  {
    for(int j=0;j< tr;j++)
    {
      RES(i,j) = x(i,j)-RowMean(i)-ColMean(j)+Mean; 
    }
  }
  arma::mat r = RES.as_row();  
  arma::mat centers;
  arma::kmeans(centers, r, 3, static_subset, 100, false);
  arma::mat  Xi(n,3,fill::zeros);
  for(int i=0; i<n;i++)
  {
    if(fabs(r(i)-centers(0,1)) < fabs(r(i)-centers(0,0)) && fabs(r(i)-centers(0,1)) < fabs(r(i)-centers(0,2)))
    {
      Xi(i,1)=1;
    } else if(fabs(r(i)-centers(0,2)) < fabs(r(i)-centers(0,0)) && fabs(r(i)-centers(0,2)) < fabs(r(i)-centers(0,1)))
    {
      Xi(i,2)=1;
    } else
    {
      Xi(i,0)=1;
    }
  }
  arma::vec  a1(n, fill::ones),a2(bl, fill::ones),a3(tr, fill::ones);
  arma::mat B3 = arma::diagmat(a3), B2 = arma::diagmat(a2);
  arma::mat  K1 = arma::kron(a2,B3);
  arma::mat  K2 = arma::kron(B2,a3);
  arma::mat X = arma::join_horiz(a1,K1,K2,Xi);
  arma::vec yhat = X*arma::pinv(arma::trans(X)*X)*arma::trans(X)*y;
  double  SSE = arma::sum(arma::square(y-yhat));
  double  df1=arma::rank(X)-tr-bl+1;
  double df2=n-arma::rank(X);
  double Tc = ((arma::sum(arma::square(arma::vectorise(r)))-SSE)/df1)/(SSE/df2);
  return Tc;
}


using namespace Rcpp;
//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
double kk_f(NumericMatrix x){// Hossein's Codes....
  // calling combn()
  Function combn("combn");
  int bl = x.nrow();
  int tr = x.ncol();
  // int n = tr*bl;
  int count = 0,i,dfn,dfd,Nsplit,nr,kk;
  double rss1,rss2;
  bool flag;
  NumericVector indj,indj2,sybj,fvalues,pvalues;
  NumericMatrix ind,yb1,yb,yb2,I;
  for(i=2; i<=floor(bl/2);i++)
  {
    ind=combn(Named("x")=bl, Named("m")=i);
    Nsplit = ind.ncol();
    nr = ind.nrow();
    if((bl/2.0)==float(i))
        Nsplit = Nsplit/2;
    for(int j=0; j<Nsplit;j++)
    {
      // Computing rss1___________________________________
      indj=ind(_,j);
      yb1 = NumericMatrix(tr,1 , x(indj(0)-1,_).begin());
      for(int k=1; k<nr;k++)
      {
        I = NumericMatrix(tr,1, x(indj(k)-1,_).begin());
        yb1 = cbind(yb1,I);
      }
      yb1 = transpose(yb1);
      
      yb = yb1;
      double myb1=mean(yb1),sybi;
      sybj = rep(0,tr);
      for(int jj=0;jj<tr;jj++)
        sybj(jj)=mean(yb1(_,jj));
      
      for(int ii=0;ii<nr;ii++)
      {
        sybi=mean(yb1(ii,_));
        for(int jj=0;jj<tr;jj++)
        {
          yb(ii,jj)= pow(yb1(ii,jj)-sybi+myb1-sybj(jj),2);
        }
      }
      rss1=sum(yb);
      // Computing rss2___________________________________
      indj2= rep(0,bl-nr);
      kk =0;
      for(int m=1; m<=bl;m++)
        {
        flag=true;
        for(int k=0; k<nr;k++)
          {
            if(m==indj(k))
              {
              flag=false;
              break;
              }
          }
        if(flag)
          {
          indj2(kk)=m;
          kk++;
          }
        
        }
      yb2 = NumericMatrix(tr,1 , x(indj2(0)-1,_).begin());
      for(int k=1; k<(bl-nr);k++)
        {
        I = NumericMatrix(tr,1, x(indj2(k)-1,_).begin());
        yb2 = cbind(yb2,I);
        }
      yb2 = transpose(yb2);
      //_________________
      yb = yb2;
      myb1=mean(yb2);
      sybj = rep(0,tr);
      for(int jj=0;jj<tr;jj++)
        sybj(jj)=mean(yb2(_,jj));
      
      for(int ii=0;ii<(bl-nr);ii++)
      {
        sybi=mean(yb2(ii,_));
        for(int jj=0;jj<tr;jj++)
          yb(ii,jj)= pow(yb2(ii,jj)-sybi+myb1-sybj(jj),2);
      }
      rss2=sum(yb);
      //______________________________
      dfn=(tr-1)*(i-1);
      dfd=(bl-i-1)*(tr-1);
      fvalues.push_back((rss1*(bl-i-1))/(rss2*(i-1)));
      if(fvalues(count)<1)
        fvalues(count)= 1/fvalues[count];
      pvalues.push_back(1-R::pf(fvalues(count),dfn,dfd,true,false)+R::pf(1/fvalues(count),dfn,dfd,true,false));
      count++;
    }
  }
return min(pvalues);
}










using namespace Rcpp;
//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
double hh_f(NumericMatrix x){// Hossein's Codes....
  // calling combn()
  Function combn("combn");
  int bl = x.nrow();
  int tr = x.ncol();
  double mx,sxbi,sse,rss1,rss2,sse7,myb1,sybi;
  int i,j,Nsplit,nr,kk;
  bool flag;
  NumericVector indj,indj2,sxbj,sybj,hvalues,v,indj3;
  NumericMatrix ind,yb1,yb,yb2,I,xb,m,yb4;
  sxbj=rep(0,tr);
  NumericMatrix xx(clone(x));
  
  mx=mean(xx);
  for(j=0;j<tr;j++)
    sxbj(j)=mean(xx(_,j));
  xb = clone(xx);
  for(int i=0;i<bl;i++)
  {
    sxbi=mean(xx(i,_));
    for(int j=0;j<tr;j++)
      xb(i,j)= pow(xx(i,j)-sxbi+mx-sxbj(j),2);
  }
  sse = sum(xb);
  // ________________________
  for(i=2; i<=floor(bl/2);i++)
  {
    ind=combn(Named("x")=bl, Named("m")=i);
    Nsplit = ind.ncol();
    nr = ind.nrow();
    if((bl/2.0)==float(i))
      Nsplit = Nsplit/2;
    for(int j=0; j<Nsplit;j++)
    {
      // Computing rss1___________________________________
      indj=ind(_,j);
      yb1 = NumericMatrix(tr,1 , x(indj(0)-1,_).begin());
      for(int k=1; k<nr;k++)
      {
        I = NumericMatrix(tr,1, x(indj(k)-1,_).begin());
        yb1 = cbind(yb1,I);
      }
      yb1 = transpose(yb1);
      
      yb = clone(yb1);
      myb1=mean(yb1);
      sybj = rep(0,tr);
      for(int jj=0;jj<tr;jj++)
        sybj(jj)=mean(yb1(_,jj));
      
      for(int ii=0;ii<nr;ii++)
      {
        sybi=mean(yb1(ii,_));
        for(int jj=0;jj<tr;jj++)
        {
          yb(ii,jj)= pow(yb1(ii,jj)-sybi+myb1-sybj(jj),2);
        }
      }
      rss1=sum(yb);
      // Computing rss2___________________________________
      indj2= rep(0,bl-nr);
      kk =0;
      for(int m=1; m<=bl;m++)
      {
        flag=true;
        for(int k=0; k<nr;k++)
        {
          if(m==indj(k))
          {
            flag=false;
            break;
          }
        }
        if(flag)
        {
          indj2(kk)=m;
          kk++;
        }
        
      }
      yb2 = NumericMatrix(tr,1 , x(indj2(0)-1,_).begin());
      for(int k=1; k<(bl-nr);k++)
      {
        I = NumericMatrix(tr,1, x(indj2(k)-1,_).begin());
        yb2 = cbind(yb2,I);
      }
      yb2 = transpose(yb2);
      //_________________
      yb = yb2;
      myb1=mean(yb2);
      sybj = rep(0,tr);
      for(int jj=0;jj<tr;jj++)
        sybj(jj)=mean(yb2(_,jj));
      
      for(int ii=0;ii<(bl-nr);ii++)
      {
        sybi=mean(yb2(ii,_));
        for(int jj=0;jj<tr;jj++)
          yb(ii,jj)= pow(yb2(ii,jj)-sybi+myb1-sybj(jj),2);
      }
      rss2=sum(yb);
      //______________________________
      sse7=rss1+rss2;
      hvalues.push_back((sse-sse7)*(bl-2)/sse7);
    }
  }
  // _________________________________________
  xx=clone(x);
  NumericMatrix yb3(tr,1);
  for(i=0; i<bl;i++)
  {
    indj3= seq(0,bl-1);
    indj3.erase(i);
    yb3=NumericMatrix(tr,1,xx(indj3(0),_).begin());
    for(j=1; j<(bl-1);j++)
      {
        m = NumericMatrix(tr,1,xx(indj3(j),_).begin());
        yb3=cbind(yb3,m);
      }
    yb3 = transpose(yb3);
    yb4=clone(yb3);
    myb1=mean(yb3);
    sybj = rep(0,tr);
    for(int jj=0;jj<tr;jj++)
      sybj(jj)=mean(yb3(_,jj));

    for(int ii=0;ii<(bl-1);ii++)
    {
      sybi=mean(yb3(ii,_));
      for(int jj=0;jj<tr;jj++)
        yb4(ii,jj)= pow(yb3(ii,jj)-sybi+myb1-sybj(jj),2);
    }
    sse7=sum(yb4);
    hvalues.push_back((sse-sse7)*(bl-2)/sse7);
  }
  return max(hvalues);
}







using namespace Rcpp;
//' @importFrom Rcpp sourceCpp
//' 
// [[Rcpp::export]]
List kh_f(NumericMatrix x){
  IntegerVector Nrow;
  double mx,sxbi,sse,rss1,rss2,sse7,myb1,sybi,fmin,fmax;
  int count = 0,i,j,Nsplit,nr,kk,dfn,dfd;
  bool flag;
  NumericVector indj,indj2,sxbj,sybj,hvalues,v,indj3,fvalues,pvalues;
  NumericMatrix ind,yb1,yb,yb2,I,xb,m,yb4;
  
  int bl = x.nrow();
  int tr = x.ncol();
  Function combn("combn");
  Nrow = seq(2,floor(bl/2));
  // _________________________________________
  sxbj=rep(0,tr);
  NumericMatrix xx(clone(x));
  mx=mean(xx);
  for(int j=0;j<tr;j++)
    sxbj(j)=mean(xx(_,j));
  xb = clone(xx);
  for(int i=0;i<bl;i++)
  {
    sxbi=mean(xx(i,_));
    for(int j=0;j<tr;j++)
      xb(i,j)= pow(xx(i,j)-sxbi+mx-sxbj(j),2);
  }
  sse = sum(xb);
  count = 0;
  
  
  for(int i=2; i<=floor(bl/2);i++)
  {
    ind=combn(Named("x")=bl, Named("m")=i); 
    Nsplit = ind.ncol();
    nr = ind.nrow();
    if((bl/2.0)==float(i))
      Nsplit = Nsplit/2;
    for(int j=0; j<Nsplit;j++)
    {
      indj=ind(_,j);
      yb1 = NumericMatrix(tr,1 , x(indj(0)-1,_).begin());
      for(int k=1; k<nr;k++)
      {
        I = NumericMatrix(tr,1, x(indj(k)-1,_).begin());
        yb1 = cbind(yb1,I);
      }
      yb1 = transpose(yb1);
      
      yb = clone(yb1);
      myb1=mean(yb1);
      sybj = rep(0,tr);
      for(int jj=0;jj<tr;jj++)
        sybj(jj)=mean(yb1(_,jj));
      
      for(int ii=0;ii<nr;ii++)
      {
        sybi=mean(yb1(ii,_));
        for(int jj=0;jj<tr;jj++)
        {
          yb(ii,jj)= pow(yb1(ii,jj)-sybi+myb1-sybj(jj),2);
        }
      }
      rss1=sum(yb);
      // Computing rss2___________________________________
      indj2= rep(0,bl-nr);
      kk =0;
      for(int m=1; m<=bl;m++)
      {
        flag=true;
        for(int k=0; k<nr;k++)
        {
          if(m==indj(k))
          {
            flag=false;
            break;
          }
        }
        if(flag)
        {
          indj2(kk)=m;
          kk++;
        }
        
      }
      yb2 = NumericMatrix(tr,1 , x(indj2(0)-1,_).begin());
      for(int k=1; k<(bl-nr);k++)
      {
        I = NumericMatrix(tr,1, x(indj2(k)-1,_).begin());
        yb2 = cbind(yb2,I);
      }
      yb2 = transpose(yb2);
      //_________________
      yb = yb2;
      myb1=mean(yb2);
      sybj = rep(0,tr);
      for(int jj=0;jj<tr;jj++)
        sybj(jj)=mean(yb2(_,jj));
      
      for(int ii=0;ii<(bl-nr);ii++)
      {
        sybi=mean(yb2(ii,_));
        for(int jj=0;jj<tr;jj++)
          yb(ii,jj)= pow(yb2(ii,jj)-sybi+myb1-sybj(jj),2);
      }
      rss2=sum(yb);
      //______________________________
      sse7=rss1+rss2;
      hvalues.push_back((sse-sse7)*(bl-2)/sse7);
      dfn=(tr-1)*(i-1);
      dfd=(bl-i-1)*(tr-1);
      fvalues.push_back((rss1*(bl-i-1))/(rss2*(i-1)));
      if(fvalues(count)<1)
        fvalues(count)= 1/fvalues[count];
      pvalues.push_back(1-R::pf(fvalues(count),dfn,dfd,true,false)+R::pf(1/fvalues(count),dfn,dfd,true,false));
      count++;
    }
  }
  fmin = min(pvalues);
  xx=clone(x);
  NumericMatrix yb3(tr,1);
  for(i=0; i<bl;i++)
  {
    indj3= seq(0,bl-1);
    indj3.erase(i);
    yb3=NumericMatrix(tr,1,xx(indj3(0),_).begin());
    for(j=1; j<(bl-1);j++)
    {
      m = NumericMatrix(tr,1,xx(indj3(j),_).begin());
      yb3=cbind(yb3,m);
    }
    yb3 = transpose(yb3);
    yb4=clone(yb3);
    myb1=mean(yb3);
    sybj = rep(0,tr);
    for(int jj=0;jj<tr;jj++)
      sybj(jj)=mean(yb3(_,jj));
    
    for(int ii=0;ii<(bl-1);ii++)
    {
      sybi=mean(yb3(ii,_));
      for(int jj=0;jj<tr;jj++)
        yb4(ii,jj)= pow(yb3(ii,jj)-sybi+myb1-sybj(jj),2);
    }
    sse7=sum(yb4);
    hvalues.push_back((sse-sse7)*(bl-2)/sse7);  
  }
  fmax = max(hvalues);
  List out;
  out = List::create(Named("fmin") = fmin , _["fmax"] = fmax);
  return out;
}



#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
//' @importFrom Rcpp sourceCpp
//' @useDynLib combinIT
//' 
// [[Rcpp::export]]

List bmp_f(arma::mat x) {            // Hossein's Codes....
  arma::vec W,delta;
  int bl = x.n_rows;
  int tr = x.n_cols;
  int n = bl*tr;
  int p = tr - 1;
  if (bl<tr)
    p = bl - 1;
  arma::vec treatment = arma::repelem(arma::regspace(1,  bl), tr, 1);
  arma::vec y = arma::trans(x.as_row());
  arma::mat RES(bl,tr);
  arma::vec RowMean = arma::mean(x,1);
  arma::vec ColMean= trans(mean(x,0));
  double Mean = accu(x)/(tr*bl);
  for(int i=0; i<bl;i++)
  {
    for(int j=0;j< tr;j++)
    {
      RES(i,j) = x(i,j)-RowMean(i)-ColMean(j)+Mean; 
    }
  }
  W=zeros(bl);
  delta=zeros(bl);
  W = arma::sum(pow(RES,2),1);
  delta = bl * (bl - 1) * W - sum(W);
  double h1=0;
  for(int i=0;i<(bl-1);i++)
    for(int j=(i+1);j<bl;j++)
      h1 = delta(i) * delta(j) + h1;
  
  double U = 2 * bl * h1 / ((bl - 1) * pow(sum(delta),2));
  double piepho = -(tr - 1) * (bl - 1) * (bl - 2) * log(U) / 2;

  arma::mat r = RES.as_row();  
  arma::mat centers;
  arma::kmeans(centers, r, 3, static_subset, 100, false);
  arma::vec af(n,fill::ones);
  arma::mat  Xi(n,3,fill::zeros);
  for(int i=0; i<n;i++)
  {
    if(fabs(r(i)-centers(0,1)) < fabs(r(i)-centers(0,0)) && fabs(r(i)-centers(0,1)) < fabs(r(i)-centers(0,2)))
    {
      Xi(i,1)=1;
    } else if(fabs(r(i)-centers(0,2)) < fabs(r(i)-centers(0,0)) && fabs(r(i)-centers(0,2)) < fabs(r(i)-centers(0,1)))
    {
      Xi(i,2)=1;
    } else
    {
      Xi(i,0)=1;
    }
  }
  arma::vec  a1(n, fill::ones),a2(bl, fill::ones),a3(tr, fill::ones);
  arma::mat B3 = arma::diagmat(a3), B2 = arma::diagmat(a2);
  arma::mat  K1 = arma::kron(a2,B3);
  arma::mat  K2 = arma::kron(B2,a3);
  arma::mat X = arma::join_horiz(a1,K1,K2,Xi);
  arma::vec yhat = X*arma::pinv(arma::trans(X)*X)*arma::trans(X)*y;
  double  SSE = arma::sum(arma::square(y-yhat));
  double  df1=arma::rank(X)-tr-bl+1;
  double df2=n-arma::rank(X);
  double Tc = ((arma::sum(arma::square(arma::vectorise(r)))-SSE)/df1)/(SSE/(df2));
  arma::mat EE1 = RES.t()*RES;
  arma::mat EE2 = EE1*EE1;
  double trace1=arma::trace(EE1);
  double trace2=arma::trace(EE2);
  double  Boik = (trace1*trace1) / (p * trace2);
  List out;
  out = List::create(Named("Boik") = Boik , _["Tc"] = Tc, _["piepho"] = piepho);
  return out;
}








