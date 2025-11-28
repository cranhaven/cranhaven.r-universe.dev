# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
//' calculate likelihood for 1 normal + 3 tumors case
//' @keywords internal
//' @export
// [[Rcpp::export]]
arma::cube calcll_p3(arma::vec& IT_new,arma::vec& B_new,double var_baf,double var_tcn,double scale, int pscnMax, int cnMax){
  int CnMax = cnMax+1;
  arma::cube L_int(CnMax,CnMax,CnMax); // matrix to store likelihood for CN
  arma::cube L_tot(CnMax,CnMax,CnMax); // matrix to store likelihood for CN, BAF and prior combined
  arma::cube L(100,34,51);
  arma::cube CN(CnMax,CnMax,CnMax);arma::cube PSCN(CnMax,CnMax,CnMax);
  double cn = IT_new(4)/scale;double baf = B_new(4);double lmax_baf;
  L_tot.fill(0);var_tcn = var_tcn/scale/scale;
  arma::uword row; arma::uword col; arma::uword slice;
  int r1_min; int r2_min; int r3_min; int r1_max; int r2_max; int r3_max; int r1_len; int r2_len; int r3_len;
  for(int i = 0; i < 100; i++){
    double np = i*0.01; // normal cell percentage
    int top1 = (100-i)/3; // tumor 1 percentage cannot exceed top1
    for(int j = 0; j <= top1; j++){
      double tp1 = j*0.01; // tumor 1 percentage
      int top2 = (100-i-j)/2; // tumor 2 percentage cannot exceed top2
      for(int k = 0; k < j; k++) L(i,j,k) = 999999.0; // it is not possible that tp2<tp1
      for(int k = top2+1; k <=50; k++) L(i,j,k) = 999999.0; // it is not possible that tp2>tp3
      for(int k = j; k <= top2; k++){
        double tp2 = k*0.01; // tumor 2 percentage
        double tp3 = 1.0 - np - tp1 - tp2;// tumor cell 3 percentage (tp1 <= tp2 <= tp3)
        // first calculate the CN and the PSCN
        CN.fill(np*2.0);PSCN.fill(np);
        for(int q=0; q<CnMax; q++){
          CN.tube(q,0,q,cnMax) += q*tp1;PSCN.tube(q,0,q,cnMax) += q*tp1; // row for tumor 1
          CN.tube(0,q,cnMax,q) += q*tp2;PSCN.tube(0,q,cnMax,q) += q*tp2; // col for tumor 2
          CN.slice(q) += q*tp3;PSCN.slice(q) += q*tp3; // slice for tumor 3
        }
        CN.subcube(0,3,0,1,cnMax,cnMax) += 999.0;//cn1<2 and cn2>2
        CN.subcube(3,0,0,cnMax,1,cnMax) += 999.0;//cn1>2 and cn2<2
        CN.subcube(3,0,0,cnMax,cnMax,1) += 999.0;//cn1>2 and cn3<2
        CN.subcube(0,0,3,1,cnMax,cnMax) += 999.0;//cn1<2 and cn3>2

        PSCN.subcube(0,2,0,0,cnMax,cnMax) += 999.0;//pscn1<1 and pscn2>1
        PSCN.subcube(2,0,0,cnMax,1,cnMax) += 999.0;//pscn1>1 and pscn2<1
        PSCN.subcube(2,0,0,cnMax,cnMax,1) += 999.0;//pscn1>1 and pscn3<1
        PSCN.subcube(0,0,2,0,cnMax,cnMax) += 999.0;//pscn1<1 and pscn3>1

        // then calculate the likelihood for intensity and baf.
        // the dimension of BAF and L_baf change based on CN. L_tot does not change, the most likely L_baf is added for each CN combination
        L_int = square(CN-cn)/(var_tcn/IT_new(6))/2.0;
        L_tot = L_int;
        for(int r1=0;r1<CnMax;r1++){
          for(int r2=0;r2<CnMax;r2++){
            for(int r3=0;r3<CnMax;r3++){
              if(r1>pscnMax){r1_max = pscnMax;r1_min = r1 - pscnMax;}else{r1_max=r1;r1_min=0;}
              if(r2>pscnMax){r2_max = pscnMax;r2_min = r2 - pscnMax;}else{r2_max=r2;r2_min=0;}
              if(r3>pscnMax){r3_max = pscnMax;r3_min = r3 - pscnMax;}else{r3_max=r3;r3_min=0;}
              r1_len = r1_max-r1_min;r2_len = r2_max-r2_min;r3_len = r3_max-r3_min;
              arma::cube BAF(r1_len,r2_len,r3_len); // cube to store baf and discrepency
              arma::cube L_baf(r1_len,r2_len,r3_len); // cube to store likelihood for baf plus prior for current CN r1, r2, r3
              BAF = PSCN.subcube(r1_min,r2_min,r3_min,r1_max,r2_max,r3_max)/cn; // need to work on this to solve 0/0 situation
              L_baf = square(BAF-baf)/(var_baf/B_new(6))/2.0;
              lmax_baf = L_baf.min(row,col,slice);L_tot(r1,r2,r3) += lmax_baf;
            }}}
        L(i,j,k) = L_tot.min(row,col,slice);
      }}
    for(int j=(top1+1);j<=33;j++) L.tube(i,j,i,j).fill(999999.0);
  }
  return L;
}
