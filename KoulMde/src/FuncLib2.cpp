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


double AbsVal(double x){

  double out = 0;

  if(x<0){
    out = 0-x;
  }else{
    out = x;
  }
  return out;
}

double MinVal(double x, double y){

  double out=0;

  if(x<y){
    out = x;
  }else{
    out = y;
  }

  return out;
}


double MaxVal(double x, double y){

  double out=0;

  if(x>y){
    out = x;
  }else{
    out = y;
  }

  return out;
}



IntegerVector RandInts(int nMany, int ceiling) {

  bool bMode = FALSE;
  if(nMany > ceiling){
    bMode = TRUE;
  }
  IntegerVector results(nMany) ;

  IntegerVector frame = seq_len(ceiling) ;

  IntegerVector candidate(nMany) ;
  int maxx=ceiling+1;

  while (maxx > ceiling) {

    candidate = sample(frame, nMany, bMode, NumericVector::create() ) ;

    maxx = max(candidate);
    results = candidate;

  }

  return results;
}

List cppGenerateS12(int nx, int ny, int n1, int bFix){

  int Totaln = nx*ny;
  int n2 = Totaln - n1;

  arma::umat TS(Totaln, 2);

  int nIndex=1;

  for(int i=1;i<=nx;i++){
    for(int j=1;j<=ny;j++){
      nIndex = (i-1)*ny+j;
      TS(nIndex-1,0) = i;
      TS(nIndex-1,1) = j;
    }
  }


  IntegerVector IndexVec(n1);


  if(bFix == 1){
    for(int i=1;i<=n1;i++){
      IndexVec[i-1] = i;
    }
  }else{
    IndexVec = RandInts(n1, Totaln);
  }

  arma::umat S1(n1,2);
  arma::umat S2(n2,2);


  int nInd = 1;


  int bCheck = 0;
  int nInc = 1;
  for(int i=1;i<=Totaln;i++){

    for(int j=1;j<=n1;j++){
      nInd = IndexVec[j-1];
      if(i==nInd){
        bCheck=1;
        S1.row(j-1) = TS.row(nInd-1);
        break;
      }
    }

    if(bCheck==0){
      S2.row(nInc-1) = TS.row(i-1);
      nInc += 1;
    }
    bCheck = 0;
  }

  List lst(3);
  lst[0] = S1;
  lst[1] = S2;
  lst[2] = TS;

  return lst;



}




arma::umat cppUnionMatrix(arma::umat SMat, arma::uvec sVec, int bZero){

  int n=0;


  if(bZero==0){
    n = SMat.n_rows;
  }else{
    n=0;
  }

  arma::umat ans((n+1),2);


  if(bZero==0){

    ans.rows(0, n-1) = SMat;
    ans(n,0) = sVec[0];
    ans(n,1) = sVec[1];

  }else{
    ans(0,0) = sVec[0];
    ans(0,1) = sVec[1];
  }

  return ans;

}


arma::umat cppDiffMatrix(arma::umat SMat, arma::uvec sVec){

  int n=SMat.n_rows;
  int nLen = sVec.n_elem;

  arma::umat ans((n-nLen+1),2);
  ans.ones();

  int nInc=1;
  int nIndex=1;
  int Ind=0;

  if(nLen==n){
    ans(0,0) = 0;
    ans(0,1) = 0;

  }else{
    for(int i=1;i<=n;i++){

      if(nIndex<=nLen){
        Ind = sVec[nIndex-1];
      }

      if(i!=Ind){
        ans(nInc-1,0) = SMat(i-1,0);
        ans(nInc-1,1) = SMat(i-1,1);
        nInc += 1;
      }else{
        nIndex += 1;
      }

    }

  }

  return ans;



}


double cppLossFunc(arma::umat S1, arma::umat S2, arma::mat zMat, double p1, double p2){

  int n1 = S1.n_rows;
  int n2 = S2.n_rows;

  double out = 0;

  int xIndi, yIndi, xIndj, yIndj;
  double tmpVali, tmpValj, tmpVal;

  if(n1!=0){

    for(int i=1;i<=n1;i++){
      xIndi = S1(i-1, 0);
      yIndi = S1(i-1, 1);
      tmpVali = zMat(xIndi-1, yIndi-1);

      for(int j=1;j<=n1;j++){
        xIndj = S1(j-1,0);
        yIndj = S1(j-1,1);
        tmpValj = zMat(xIndj-1, yIndj-1);
        tmpVal = AbsVal(tmpVali+tmpValj-2*p1) - AbsVal(tmpVali-tmpValj);
        out += tmpVal;
      }

    }

  }


  if(n2!=0){

    for(int i=1;i<=n2;i++){
      xIndi = S2(i-1, 0);
      yIndi = S2(i-1, 1);

      tmpVali = zMat(xIndi-1, yIndi-1);

      for(int j=1;j<=n2;j++){
        xIndj = S2(j-1,0);
        yIndj = S2(j-1,1);
        tmpValj = zMat(xIndj-1, yIndj-1);
        tmpVal = AbsVal(tmpVali+tmpValj-2*p2) - AbsVal(tmpVali-tmpValj);
        out += tmpVal;
      }

    }

  }


  return out;

}



double cppNetGain(int k, arma::vec G1, arma::vec G2, int n1, int n2, double p1, double p2){

  double out=0;
  double tmpVal=0;
  double gii=0;

  double gn1 = G1[k-1];

  for(int i=1;i<=n1;i++){
    if(i!=k){
      gii = G1[i-1];
      tmpVal = AbsVal(gii+gn1-2*p1)-AbsVal(gii-gn1);
      out += -2*tmpVal;

    }
  }

  out += 0 - 2*AbsVal(gn1-p1);


  if(n2!=0){
    for(int i=1;i<=n2;i++){
      gii = G2[i-1];
      tmpVal = AbsVal(gii+gn1-2*p2)-AbsVal(gii-gn1);
      out += 2*tmpVal ;

    }

  }

  out += 2*AbsVal(gn1-p2);

  return out;
}


double GetNetGgain(int k, arma::umat S1, arma::umat S2, arma::mat zMat, double p1, double p2){

  int n1 = S1.n_rows;
  int n2 = S2.n_rows;

  double out;

  arma::vec G1(n1);
  arma::vec G2(n2);

  int xi=0;
  int yi=0;

  for(int i=1;i<=n1;i++){
    xi = S1(i-1,0);
    yi = S1(i-1,1);
    G1[i-1] = zMat(xi-1,yi-1);
  }

  for(int i=1;i<=n2;i++){
    xi = S2(i-1,0);
    yi = S2(i-1,1);
    G2[i-1] = zMat(xi-1,yi-1);
  }

  out = cppNetGain(k, G1, G2, n1, n2, p1, p2);


  return out;

}


double cppNetGain_Mat_version(int k, arma::umat S1, arma::umat S2, arma::mat zMat, double p1, double p2){

  int n1 = S1.n_rows;
  int orig_n2 = S2.n_rows;
  int n2=0;

  if(orig_n2==1){
    if(S2(0,0)==0){
      n2=0;
    }else{
      n2=1;
    }
  }else{
    n2 = orig_n2;
  }

  double out;
  arma::vec G1(n1);

  int xi=0;
  int yi=0;

  for(int i=1;i<=n1;i++){
    xi = S1(i-1,0);
    yi = S1(i-1,1);
    G1[i-1] = zMat(xi-1,yi-1);
  }


  if(n2==0){
    out = cppNetGain(k, G1, G1, n1, n2, p1, p2);

  }else{
    arma::vec G2(n2);

    for(int i=1;i<=n2;i++){
      xi = S2(i-1,0);
      yi = S2(i-1,1);
      G2[i-1] = zMat(xi-1,yi-1);
    }

    out = cppNetGain(k, G1, G2, n1, n2, p1, p2);

  }

  return out;

}





double NetGatin_Of_S1_k(arma::uword h, arma::uvec cVec, arma::umat S1, arma::umat S2, arma::mat zMat, double p1, double p2){

  int nLen = cVec.n_elem;
  int n1 = S1.n_rows;
  int n2 = S2.n_rows;

  arma::umat S1k((n1-nLen), 2);
  arma::umat Original_S1k((n1-nLen+1), 2);

  arma::umat S2k((n2+nLen), 2);
  S2k.rows(0,(n2-1)) = S2;

  int k=0;
  arma::uword ks=0;
  arma::uword ke=0;

  arma::uword nIndex=0;

  int xi=0;
  int yi=0;


  for(int i=1;i<=nLen;i++){

    k=cVec[i-1];
    xi=S1(k-1,0);
    yi=S1(k-1,1);
    S2k((n2+i-1),0)=xi;
    S2k((n2+i-1),1)=yi;


  }

  if(h<cVec[0]){
    nIndex=h;

  }else if(h>cVec[nLen-1]){
    nIndex=h-nLen;
  }else{

    for(int i=1;i<=(nLen-1);i++){

      ks=cVec[i-1];
      ke=cVec[i];

      if(h>ks && h<ke ){
        nIndex=h-i;
        break;
      }

    }

  }




  int n11=n1-nLen;
  int n22=n2+nLen;

  Original_S1k=cppDiffMatrix(S1, cVec);
  S1k = Original_S1k.rows(0, n11-1);


  arma::vec G1(n11);
  arma::vec G2(n22);


  for(int i=1;i<=n11;i++){
    xi = S1k(i-1,0);
    yi = S1k(i-1,1);
    G1[i-1] = zMat(xi-1,yi-1);

    
  }


  
  for(int i=1;i<=n22;i++){
    xi = S2k(i-1,0);
    yi = S2k(i-1,1);

    
    G2[i-1] = zMat(xi-1,yi-1);

  }


  double out = cppNetGain(nIndex, G1, G2, n11, n22, p1, p2);


  return out;
}


double NetGatin_Of_S1_k_Zero(arma::uword h, arma::uvec cVec, arma::umat S1, arma::mat zMat, double p1, double p2){

   int nLen = cVec.n_elem;
   int n1 = S1.n_rows;
   int n2 = 0;

   arma::umat S1k((n1-nLen), 2);
   arma::umat Original_S1k((n1-nLen+1), 2);

   arma::umat S2k((n2+nLen), 2);
   S2k.zeros();

   int k=0;
   arma::uword ks=0;
   arma::uword ke=0;

   arma::uword nIndex=0;

   int xi=0;
   int yi=0;


   for(int i=1;i<=nLen;i++){

     k=cVec[i-1];
     xi=S1(k-1,0);
     yi=S1(k-1,1);
     S2k((n2+i-1),0)=xi;
     S2k((n2+i-1),1)=yi;


   }

   if(h<cVec[0]){
     nIndex=h;

   }else if(h>cVec[nLen-1]){
     nIndex=h-nLen;
   }else{

     for(int i=1;i<=(nLen-1);i++){

       ks=cVec[i-1];
       ke=cVec[i];

       if(h>ks && h<ke ){
         nIndex=h-i;
         break;
       }

     }

   }




   int n11=n1-nLen;
   int n22=n2+nLen;

   Original_S1k=cppDiffMatrix(S1, cVec);
   S1k = Original_S1k.rows(0, n11-1);


   arma::vec G1(n11);
   arma::vec G2(n22);


   for(int i=1;i<=n11;i++){
     xi = S1k(i-1,0);
     yi = S1k(i-1,1);
     G1[i-1] = zMat(xi-1,yi-1);

   }

   for(int i=1;i<=n22;i++){
     xi = S2k(i-1,0);
     yi = S2k(i-1,1);

     G2[i-1] = zMat(xi-1,yi-1);

   }


   double out = cppNetGain(nIndex, G1, G2, n11, n22, p1, p2);


   return out;
}



arma::uvec Make_Transfer_List(arma::umat S1, arma::umat S2, arma::mat zMat, double p1, double p2){

  arma::uword n1 = S1.n_rows;
  //int n2 = S2.n_rows;

  arma::uvec ListVec(n1);
  ListVec.zeros();

  int nList=0;
  int nInc=0;
  double NGVal = 0;
  double NGVal2 = 0;

  for(arma::uword i=1;i<=n1;i++){
    NGVal = cppNetGain_Mat_version(i, S1, S2, zMat, p1, p2);

    if(NGVal<0){
      nInc += 1;

      if(nInc==1){
        ListVec[0] = i;
        nList=1;
      }else{
        NGVal2 = NetGatin_Of_S1_k(i, ListVec.subvec(0, nList-1), S1, S2, zMat, p1, p2);

        if(NGVal2<0){
          nList += 1;
          ListVec[nList-1] = i;
        }

      }

    }
  }

  if(nList==0){
    ListVec[0]=0;
    return ListVec;
  }else{
    return ListVec.subvec(0, nList-1);
  }


}


arma::uvec Make_Transfer_List_Zero(arma::umat S1, arma::mat zMat, double p1, double p2){

  arma::uword n1 = S1.n_rows;
  //int n2 = 0;

  arma::umat S2(1,2);
  S2.zeros();

  arma::uvec ListVec(n1);
  ListVec.zeros();

  int nList=0;
  int nInc=0;
  double NGVal = 0;
  double NGVal2 = 0;

  for(arma::uword i=1;i<=n1;i++){
    NGVal = cppNetGain_Mat_version(i, S1, S2, zMat, p1, p2);

    if(NGVal<0){
      nInc += 1;

      if(nInc==1){
        ListVec[0] = i;
        nList=1;
      }else{
        NGVal2 = NetGatin_Of_S1_k_Zero(i, ListVec.subvec(0, nList-1), S1, zMat, p1, p2);

        if(NGVal2<0){
          nList += 1;
          ListVec[nList-1] = i;
        }

      }

    }
  }

  if(nList==0){
    ListVec[0]=0;
    return ListVec;
  }else{
    return ListVec.subvec(0, nList-1);
  }


}



arma::umat Trade_Between_S1_and_S2(arma::umat S1, arma::umat S2, int n1, int n2, arma::mat zMat, double p1, double p2){


  int n = n1+n2;
  arma::umat out(n+2,2);
  out.zeros();


  int nLen = 0;
  arma::uvec tmpVec(2);
  tmpVec.zeros();

  int k=0;
  int nT1=0;
  int nT2=0;

  int n1k=0;
  int n2k=0;

  int nS1T1=0;
  int nS2T2=0;

  arma::umat S1k;
  arma::umat S2k;
  arma::uvec ListVec1_to_2;

  if(n1!=0){   ///// S1!=0
    if(n2==0){  /////  S1=TotalS,  S2=0
      ListVec1_to_2 = Make_Transfer_List_Zero(S1, zMat, p1, p2);

      if(ListVec1_to_2[0]!=0){
        nLen = ListVec1_to_2.n_elem;
        nT1 = nLen;
        arma::umat tmpS2k(nLen,2);

        for(int i=1;i<=nLen;i++){
          k = ListVec1_to_2[i-1];
          tmpS2k(i-1,0) = S1(k-1,0);
          tmpS2k(i-1,1) = S1(k-1,1);

        }

        S2k=tmpS2k;

        n2k=nLen;

        if(nLen==n1){
          ////// S1k=0, S2k=TotalS
          n1k=0;

        }else{

          ////// S1k!=0, S2k!=TotalS
          arma::umat Orig_S1k = cppDiffMatrix(S1, ListVec1_to_2);
          S1k=Orig_S1k.rows(0, n1-nLen-1);
          n1k = n1-nLen;

        }



      }else{

        //////////////////////// S1=TotalS, S2=0, and Transfer List=0

        out.rows(0, n-1)=S1;
        out(n,0)=n;
        out(n+1,0)=0;
        out(n+1,1)=0;
        return out;
      }



    }else{     /////////////// n2!=0, ===>  S1 != 0, S2!=0

      ListVec1_to_2 = Make_Transfer_List(S1, S2, zMat, p1, p2);

      if(ListVec1_to_2[0]!=0){
        nLen = ListVec1_to_2.n_elem;
        nT1=nLen;

        arma::umat tmpS2k( (n2+nLen),2);
        tmpS2k.rows(0, n2-1)=S2;

        n2k = n2+nLen;

        for(int i=1;i<=nLen;i++){
          k = ListVec1_to_2[i-1];
          tmpS2k(n2+i-1,0) = S1(k-1,0);
          tmpS2k(n2+i-1,1) = S1(k-1,1);

        }

        S2k = tmpS2k;

        if(nLen==n1){
          ////// S1k=0, S2k=TotalS
          n1k=0;

        }else{

          ////// S1k!=0, S2k!=TotalS
          arma::umat Orig_S1k = cppDiffMatrix(S1, ListVec1_to_2);
          S1k=Orig_S1k.rows(0, n1-nLen-1);

          n1k = n1-nLen;
        }


      }else{

        S1k=S1;
        S2k=S2;
        nT1=0;

        n1k=n1;
        n2k=n2;

      }
    }



  }else{       /////////// S1=0, S2=TotalS

    ////// S1k=0, S2k=TotalS

    S2k=S2;
    nT1=0;

    n1k=0;
    n2k=n2;
  }



  ////////////////////////////// S2k ===> S1k part



  arma::uvec ListVec2_to_1;
  arma::umat S1T1;
  arma::umat S2T2;

  if(n2k!=0){   ///// S2k!=0
    if(n1k==0){  /////  S2k=TotalS,  S1k=0
      ListVec2_to_1 = Make_Transfer_List_Zero(S2k, zMat, p2, p1);

      

      if(ListVec2_to_1[0]!=0){
        nLen = ListVec2_to_1.n_elem;
        nT2 = nLen;
        arma::umat tmpS1T1(nLen,2);

        for(int i=1;i<=nLen;i++){
          k = ListVec2_to_1[i-1];
          tmpS1T1(i-1,0) = S2k(k-1,0);
          tmpS1T1(i-1,1) = S2k(k-1,1);

        }

        

        S1T1 = tmpS1T1;
        nS1T1=nLen;

        if(nLen==n2k){
          ////// S2T2=0, S1T1=TotalS
          nS2T2=0;

        }else{

          ////// S2k=TotalS, S1k=0
          ////// S1T1!=0, S2T2!=0
          arma::umat Orig_S2T2 = cppDiffMatrix(S2k, ListVec2_to_1);
          S2T2=Orig_S2T2.rows(0, n2k-nLen-1);
          nS2T2 = n2k-nLen;

        }


        


      }else{

        //////////////////////// S2k=TotalS, S1k=0, and Transfer List=0

        out.rows(0, n-1)=S2k;
        out(n,1)=n;
        out(n+1,0)=0;
        out(n+1,1)=0;

        
        return out;
      }



    }else{     /////////////// n2k!=0 n1k!=0, ===>  S1k != 0, S2k !=0

      
      ListVec2_to_1 = Make_Transfer_List(S2k, S1k, zMat, p2, p1);

      
      if(ListVec2_to_1[0]!=0){
        nLen = ListVec2_to_1.n_elem;
        nT2=nLen;

        arma::umat tmpS1T1( (n1k+nLen),2);
        tmpS1T1.rows(0, n1k-1)=S1k;

        nS1T1 = n1k+nLen;

        for(int i=1;i<=nLen;i++){
          k = ListVec2_to_1[i-1];
          tmpS1T1(n1k+i-1,0) = S2k(k-1,0);
          tmpS1T1(n1k+i-1,1) = S2k(k-1,1);

        }

        S1T1 = tmpS1T1;

        if(nLen==n2k){
          ////// S1T1=TotalS, S2T2=0
          nS2T2=0;

        }else{

          ////// S1T1!=0, S2T2!=TotalS
          arma::umat Orig_S2T2 = cppDiffMatrix(S2k, ListVec2_to_1);
          S2T2=Orig_S2T2.rows(0, n2k-nLen-1);

          nS2T2 = n2k-nLen;
        }

        

      }else{

        S1T1=S1k;
        S2T2=S2k;
        nT2=0;

        nS1T1 = n1k;
        nS2T2 = n2k;

      }
    }



  }else{       /////////// S2k=0, S1k=TotalS

    ////// S2T2=0, S1T1=TotalS

    S1T1=S1k;
    nT2=0;

    nS2T2=0;
    nS1T1=n1k;
  }

  
  out.rows(0, nS1T1-1)=S1T1;
  out.rows(nS1T1, n-1)=S2T2;
  out(n,0)=nS1T1;
  out(n,1)=nS2T2;
  out(n+1,0)=nT1;
  out(n+1,1)=nT2;

  
  return out;




}



arma::umat cppEstimateS(arma::umat S1, arma::umat S2, arma::mat zMat, double p1, double p2){

  int n1=S1.n_rows;
  int n2=S2.n_rows;
  int n = n1+n2;

  int nT1=1;
  int nT2=1;
  int nT = nT1+nT2;
  int nInc=1;

  arma::umat out(n+2,2);

  arma::umat Input_S1;
  arma::umat Input_S2;

  Input_S1 = S1;
  Input_S2 = S2;

  while(nT!=0){

    out = Trade_Between_S1_and_S2(Input_S1, Input_S2, n1, n2, zMat, p1, p2);

    n1 = out(n,0);
    n2 = out(n,1);
    nT1 = out(n+1,0);
    nT2 = out(n+1,1);
    n = n1+n2;

    if(n1==0){
      Input_S2=out.rows(0, n-1);
      Input_S1=Input_S2;

    }else if(n2==0){

      Input_S1=out.rows(0, n-1);
      Input_S2=Input_S1;


    }else{
      Input_S1=out.rows(0, n1-1);
      Input_S2=out.rows(n1, n-1);

    }


    nInc +=1;
    nT=nT1+nT2;

  }

  return out;

}

//' @keywords internal
// [[Rcpp::export]]
List cppGet_Estimated_Img(arma::mat zMat, double p1, double p2){


  int nx=zMat.n_rows;
  int ny=zMat.n_cols;

  int Totaln=nx*ny;

  int n1 = std::floor(Totaln/2) ;
  int bFix=1;

  List lst = cppGenerateS12(nx, ny, n1, bFix);

  arma::umat InitS1 = lst[0];
  arma::umat InitS2 = lst[1];
  arma::umat TS = lst[2];

  arma::umat out = cppEstimateS(InitS1, InitS2, zMat, p1, p2);

  int Opt_n1 = out(Totaln,0);
  int Opt_n2 = out(Totaln,1);


  arma::umat Opt_S1;
  arma::umat Opt_S2;
  arma::mat EstImgMat(nx, ny);
  EstImgMat.ones();
  EstImgMat *= p2;

  arma::uword xi=0;
  arma::uword yi=0;

  List ans(3);

  if(Opt_n1!=0){
    Opt_S1 = out.rows(0, Opt_n1-1);
    for(int i=1;i<=Opt_n1;i++){
      xi = Opt_S1(i-1,0);
      yi = Opt_S1(i-1,1);
      EstImgMat(xi-1,yi-1)=p1;
    }
    ans[0] = Opt_S1;
    if(Opt_n2!=0){
      Opt_S2 = out.rows(Opt_n1, Totaln-1);
      ans[1] = Opt_S2;
    }else{
      ans[1] = -1;
    }


  }else{

    ans[0] = -1;
    Opt_S2 = TS;
    ans[1] = Opt_S2;

  }

  ans[2] = EstImgMat;

  return ans;

}










