#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


double GetNorm(arma::mat y1, arma::mat y2){

  int n = y1.n_rows;
  double out=0;
  double diff=0;

  for(int i=1;i<=n;i++){
    diff = y1(i-1,0) - y2(i-1,0);
    out += diff*diff;

  }

  return out;

}



arma::mat SortedMatByLoss(arma::mat X) {

  int n = X.n_rows;
  int p = X.n_cols;
  arma::mat out(n,p);
  out.zeros();

  arma::uvec IndexVec(n);

  arma::mat X0Col = X.col(0);
  IndexVec = sort_index(X0Col);


  int nInd=0;

  int nIndexVecVal = 0;

  for(int i=1; i<=n; i++){


    nIndexVecVal = IndexVec[i-1];

    if(i==1){
      out.row(nInd) = X.row(nIndexVecVal);
      nInd += 1;
    }else{

      out.row(nInd) = X.row(nIndexVecVal);
      nInd += 1;



    }

  }


  return out;

}


arma::mat GetSortedMat(arma::mat X) {

  int n = X.n_rows;
  int p = X.n_cols;
  arma::mat out((n+1),p);
  out.zeros();

  arma::uvec IndexVec(n);

  arma::mat X0Col = X.col(0);
  IndexVec = sort_index(X0Col);


  int nInd=0;
  double tmp1=0;
  double tmp2=0;
  double FstSlope = 0;

  int nIndexVecVal = 0;

  for(int i=1; i<=n; i++){

    FstSlope += X(i-1,1);

    nIndexVecVal = IndexVec[i-1];

    if(i==1){
      out.row(nInd) = X.row(nIndexVecVal);
      nInd += 1;
    }else{

      tmp1 = out(nInd-1,0);
      tmp2 = X(nIndexVecVal,0);

      if(tmp1 == tmp2){
        out(nInd-1, 1) += X(nIndexVecVal,1);
      }else{
        out.row(nInd) = X.row(nIndexVecVal);
        nInd += 1;
        if(i==n){
          nInd -= 1;
        }
      }


    }

  }

  nInd += 1;
  out(n, 0) = nInd;
  out(n, 1) = -FstSlope;
  return out;

}


arma::mat GetSlopeMat(arma::mat X) {

  int n = X.n_rows;
  int p = X.n_cols;

  arma::mat out(n,p);
  out.zeros();

  out = GetSortedMat(X);


  double tmp1=0;
  double tmp2=0;

  int nInd=out(n,0);
  double FstSlope = out(n,1);

  arma::mat SlopeMat(nInd,2);
  SlopeMat.zeros();

  SlopeMat(0,0) = FstSlope;

  int nInd2 = 0;

  double tmp3=0;

  for(int i=2;i<=nInd;i++){
    SlopeMat(i-1,0) = SlopeMat(i-2,0) + 2*out(i-2, 1);

    tmp1 = SlopeMat(i-2,0);
    tmp2 = SlopeMat(i-1,0);

    if( tmp1<0  ){

      if( tmp2>0 ){
        SlopeMat(nInd2,1) = (i-1);
        nInd2 += 1;
      }else if(tmp2==0){

        if(i!= nInd){
          tmp3 = SlopeMat(i-1,0) + 2*out(i-1, 1);
          if( tmp3 > 0){
            SlopeMat(nInd2,1) = (i-1);
            nInd2 += 1;
          }
        }

      }


    }
  }
  //out = sort(X);


  return SlopeMat;
}


double CLoss(arma::mat Y, arma::mat X, arma::mat Dstar, arma::mat beta){

  double total = 0;

  int n = X.n_rows;

  arma::mat tmpMat(1,1);

  double tempxbi = 0;
  double tempxbj = 0;
  double absipj = 0;
  double absimj = 0;
  double ei = 0;
  double ej = 0;

  for(int i=0; i<n; i++){

    tempxbi = 0;
    tmpMat = X.row(i) * beta;
    tempxbi = tmpMat(0,0);

    ei = Y[i]- tempxbi;

    for(int j=i; j<n; j++){

      tempxbj=0;

      tmpMat = X.row(j) * beta;
      tempxbj = tmpMat(0,0);

      ej = Y[j]- tempxbj;

      if((ei+ej)<0){
        absipj = 0-(ei+ej);
      }else{
        absipj = (ei+ej);
      }

      if((ei-ej)<0){
        absimj = 0-(ei-ej);
      }else{
        absimj = (ei-ej);
      }

      if( j == i){
        total += Dstar(i,j)* (absipj-absimj);
      }else{
        total += 2*Dstar(i,j)* (absipj-absimj);
      }

    }

  }
  return total;

}


arma::mat GetZeroMat(arma::mat Y, arma::mat X, arma::mat Dstar, arma::mat bVec, int l){

  int n = X.n_rows;

  double xp=0;
  double xm=0;

  double yp=0;
  double ym=0;

  int n2=2*n*n;

  arma::mat out(n2,2);
  arma::mat tmpMat(1,1);

  int nInd = 0;

  for(int i=1;i<=n;i++){

    for(int j=1;j<=n;j++){

      xp = X(i-1,l-1) + X(j-1,l-1);
      yp = Y(i-1,0) + Y(j-1,0);
      if(xp != 0){

        tmpMat = (X.row(i-1)+X.row(j-1))*bVec;

        if(xp<0){
          out(nInd, 0) = -xp*Dstar(i-1,j-1);
        }else{
          out(nInd, 0) = xp*Dstar(i-1,j-1);
        }
        out(nInd, 1) = (yp - tmpMat(0,0)+xp*bVec(l-1,0) )/xp;

        nInd += 1;

      }


      xm = X(i-1,l-1) - X(j-1,l-1);
      ym = Y(i-1,0) - Y(j-1,0);
      if(xm != 0){

        tmpMat = (X.row(i-1)-X.row(j-1))*bVec;

        if(xm<0){
          out(nInd, 0) = xm*Dstar(i-1,j-1);
        }else{
          out(nInd, 0) = -xm*Dstar(i-1,j-1);
        }
        out(nInd, 1) = (ym - tmpMat(0,0)+xm*bVec(l-1,0) )/xm;

        nInd += 1;

      }



    }

  }

  arma::mat ResultOut = out.submat(0,0, (nInd-1), 1);

  return ResultOut;


}





///////////////////////////////////////Huber part
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////


int CheckHuberInt(double x, double HuberC){

  int val=0;

  if(x > HuberC){
    val = 1;
  }
  if(x < -HuberC){
    val = -1;
  }

  return val;

}


double HuberFunc(double x, double HuberC){

  double val=0;

  if(x > HuberC){
    val = HuberC;
  }else if(x < -HuberC){
    val = -HuberC;
  }else{
    val = x;
  }

  return val;

}


double HuberLoss(arma::mat Y, arma::mat X, arma::mat Dstar, arma::mat beta, double HuberC){

  double total = 0;

  int nrow = X.n_rows;


  double tempxbi = 0;
  double tempxbj = 0;
  double absipj = 0;
  double absimj = 0;
  double ei = 0;
  double ej = 0;

  arma::mat tmpMat(1,1);


  for(int i=0; i<nrow; i++){

    tempxbi=0;

    tmpMat = X.row(i)*beta;
    tempxbi = tmpMat(0,0);

    ei = Y[i]- tempxbi;
    ei = HuberFunc(ei, HuberC);

    for(int j=i; j<nrow; j++){
      tempxbj=0;

      tmpMat = X.row(j)*beta;
      tempxbj = tmpMat(0,0);


      ej = Y[j]- tempxbj;
      ej = HuberFunc(ej, HuberC);

      if((ei+ej)<0){
        absipj = 0-(ei+ej);
      }else{
        absipj = (ei+ej);
      }

      if((ei-ej)<0){
        absimj = 0-(ei-ej);
      }else{
        absimj = (ei-ej);
      }

      if( j== i){
        total += Dstar(i,j)* (absipj-absimj);
      }else{
        total += 2*Dstar(i,j)* (absipj-absimj);
      }

    }

  }
  return total;

}



///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////





/////////////////////////////////////// Degenerate
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////


double DegenLoss(arma::mat Y, arma::mat X, arma::mat D, arma::mat b){

  double val=0;
  double tempval;


  int n = X.n_rows;
  int p = X.n_cols;

  double dik;
  double ei;
  int sgn_ei;

  arma::mat tmpMat(1,1);


  double temprod;

  for(int k=0;k<p;k++){
    tempval=0;

    for(int i=0; i<n;i++){
      dik = D(i,k);
      sgn_ei = 1;

      temprod=0;

      tmpMat = X.row(i)*b;
      temprod = tmpMat(0,0);

      ei = Y(i,0)-temprod;
      if(ei > 0){
        sgn_ei = -1;
      }else if(ei < 0){
        sgn_ei = 1;
      }
      tempval += ( dik*sgn_ei );
    }
    val += (tempval*tempval);
  }


  return val;
}



arma::mat GetDegenbVec(arma::mat Y, arma::mat X, arma::mat D, arma::mat bVec, int l){


  int n = X.n_rows;
  int p = X.n_cols;

  arma::mat TriedbVec(p,1);
  arma::mat tmpMat(1,1);

  TriedbVec = bVec;

  int nLen = 0;
  arma::mat TempELVec(n,1);

  double xil;
  double temprod;

  for(int i=0;i<n;i++){
    xil = X(i,l-1);

    if(xil != 0){

      temprod=0;
      tmpMat = X.row(i)*bVec;

      temprod = tmpMat(0,0);

      TempELVec(nLen,0) = ( Y(i,0) - temprod + X(i,l-1)*bVec(l-1,0))/xil;
      nLen += 1;
    }

  }

  arma::mat ELVec(nLen,1);

  for(int i=0;i<nLen;i++){
    ELVec(i,0) = TempELVec(i,0);
  }

  ELVec = SortedMatByLoss(ELVec);

  int TotalLen = 2*nLen+1;
  arma::mat CheckingVec(TotalLen,1);

  CheckingVec(0,0) = ELVec(0,0)-1;
  CheckingVec((TotalLen-1),0) = ELVec((nLen-1),0) + 1;

  for(int i=1;i <= nLen; i++){

    CheckingVec((2*i-1), 0) = ELVec(i-1,0);
    if(i!= nLen){
      CheckingVec((2*i),0) = (ELVec(i-1,0) + ELVec(i,0))/2 ;
    }

  }

  double LossVal=0;
  double OldLossVal=0;
  double Lmat2Val=0;


  for(int i=0; i<TotalLen;i++){

    TriedbVec(l-1,0) = CheckingVec(i,0);
    LossVal = DegenLoss(Y,X,D,TriedbVec);

    if(i==0){OldLossVal = LossVal;}

    Lmat2Val = 0;

    if(LossVal < OldLossVal){
      Lmat2Val = LossVal - OldLossVal;
      OldLossVal = LossVal;
      bVec(l-1,0) = CheckingVec(i,0);
    }


  }

  arma::mat out(p+1,1);
  out.submat(0,0,(p-1),0) = bVec;
  out(p,0) = Lmat2Val;

  return out;
}




///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////
///////////////////////////////////////






arma::mat GetbVec(arma::mat Y, arma::mat X, arma::mat Dstar, arma::mat bVec, int l, int type, double HuberC){

  int p = X.n_cols;

  arma::mat ZeroMat = GetZeroMat(Y, X, Dstar, bVec, l);
  arma::mat SlopeMat = GetSlopeMat(ZeroMat);

  int nLen = SlopeMat.n_rows;
  int OptimalIndex = 0;
  double OptimalZero=0;
  double LossVal = 0;
  double TriedLossVal = 0;

  if(type==1){
    LossVal = CLoss(Y, X, Dstar, bVec);
  }else if(type==2){
    LossVal = HuberLoss(Y, X, Dstar, bVec, HuberC);
  }


  double LMat2Val = 0;

  arma::mat TriedbVec = bVec;

  for(int i=1;i<=nLen;i++){

    if(i==1){
      if(SlopeMat(0,1) == 0){

        if(SlopeMat(0,1)<0 ){
          OptimalZero = SlopeMat(nLen-1,0);
        }else{
          OptimalZero = SlopeMat(0,0);
        }

        break;
      }else{
        OptimalIndex = SlopeMat(i-1,1);
        OptimalZero = SlopeMat(OptimalIndex-1, 0);
      }
      TriedbVec(l-1,0) = OptimalZero;

      if(type==1){
        TriedLossVal = CLoss(Y, X, Dstar, TriedbVec);
      }else if(type==2){
        TriedLossVal = HuberLoss(Y, X, Dstar, TriedbVec, HuberC);
      }



      LMat2Val = 0;
      if(TriedLossVal < LossVal){
        LossVal = TriedLossVal;
        bVec(l-1,0) = OptimalZero;
        TriedbVec(l-1,0) = OptimalZero;
        LMat2Val = TriedLossVal - LossVal;
      }

    }else{

      if(SlopeMat(i-1,1)==0){
        break;
      }else{
        OptimalIndex = SlopeMat(i-1,1);
        OptimalZero = SlopeMat(OptimalIndex-1, 0);
      }

      TriedbVec(l-1,0) = OptimalZero;
      if(type==1){
        TriedLossVal = CLoss(Y, X, Dstar, TriedbVec);
      }else if(type==2){
        TriedLossVal = HuberLoss(Y, X, Dstar, TriedbVec, HuberC);
      }

      LMat2Val = 0;

      if(TriedLossVal < LossVal){
        LossVal = TriedLossVal;
        bVec(l-1,0) = OptimalZero;
        TriedbVec(l-1,0) = OptimalZero;
        LMat2Val = TriedLossVal - LossVal;
      }

    }


  }

  arma::mat out(p+1,1);
  out.submat(0,0,(p-1),0)=bVec;
  out(p,0) = LMat2Val;
  return out;

}


//' @keywords internal
// [[Rcpp::export]]
arma::mat EstimateBetaMDESimple(arma::mat Y, arma::mat X, arma::mat D, arma::mat b0, int iter, double critVal, int type, double HuberC){

  arma::mat Dstar = D*D.t();

  int p = X.n_cols;

  arma::mat bVec(p,1);
  arma::mat OldbVec(p,1);
  bVec = b0;

  arma::mat ResultMat((p+1),1);
  ResultMat.zeros();

  arma::mat LMat(p,2);
  LMat.zeros();

  int l=0;

  double diff = 0;

  for(int i=1;i<=p;i++){
    LMat(i-1,1)=i;
  }

  for(int ith=1;ith<=iter;ith++){

    OldbVec = bVec;

    if(ith == 1){
      for(int lprime=1;lprime<=p;lprime++){
        l = LMat(lprime-1, 1);
      }

      if(type==3){
        ResultMat = GetDegenbVec(Y, X, D, bVec, l);
      }else{
        ResultMat = GetbVec(Y, X, Dstar, bVec, l, type, HuberC);
      }

      bVec = ResultMat.submat(0, 0, (p-1), 0);
      LMat(l-1, 0) = ResultMat(p, 0);
    }else{
      l = LMat(0, 1);

      if(type==3){
        ResultMat = GetDegenbVec(Y, X, D, bVec, l);
      }else{
        ResultMat = GetbVec(Y, X, Dstar, bVec, l, type, HuberC);
      }

      bVec = ResultMat.submat(0, 0, (p-1), 0);
      LMat(0, 0) = ResultMat(p, 0);

    }

    LMat = SortedMatByLoss(LMat);

    diff = GetNorm(bVec, OldbVec);
    if(diff < critVal){break;}

  }

  double ObjVal = 0;
  arma::mat out((p+1),1);

  if(type == 1){
    ObjVal = CLoss(Y, X, Dstar, bVec);

  }else if(type == 2){

    ObjVal = HuberLoss(Y, X, Dstar, bVec, HuberC);

  }else{
    ObjVal = DegenLoss(Y, X, D, bVec);
  }

  out.submat(0,0,(p-1),0) = bVec;
  out(p,0) = ObjVal;
  return out;

}





// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

