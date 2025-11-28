# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
//' calculate likelihood for 1 normal + 2 tumors case
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calcll_cpp(arma::vec& IT_new,arma::vec& B_new,arma::mat lp,arma::mat rlp,double var_baf,double var_tcn,double scale, int pscnMax, int cnMax){
  int CnMax = cnMax+1;
  arma::mat L_int(CnMax,CnMax); // matrix to store likelihood for CN
  arma::mat L_tot(CnMax,CnMax); // matrix to store likelihood for CN, BAF and prior combined
  arma::mat L(100,51);
  arma::mat CN(CnMax,CnMax);
  arma::mat PSCN(CnMax,CnMax);
  double cn = IT_new(4)/scale;
  double baf = B_new(4);
  double lmax_baf;
  arma::mat pscn1(100,51);
  arma::mat pscn2(100,51);
  arma::mat cn1(100,51);
  arma::mat cn2(100,51);
  arma::mat pscn_temp1(CnMax,CnMax);
  arma::mat pscn_temp2(CnMax,CnMax);
  int r1_min; int r2_min; int r1_max; int r2_max; int r1_len; int r2_len;
  List out(5);
  L_tot.fill(0);var_tcn = var_tcn/scale/scale;
  arma::uword row; arma::uword col;

  for(int i = 0; i < 100; i++){
    int top = (101-i)/2;
    double np = i*0.01; // normal cell percentage
    for(int j = 0; j <= top; j++){
      CN.fill(np*2.0);PSCN.fill(np);
      double tp1 = j*0.01; // tumor cell 1 percentage
      double tp2 = 1.0-np-tp1; // tumor cell 2 percentage
      for(int q=0; q<CnMax; q++){CN.row(q) += q*tp1;PSCN.row(q)+= q*tp1;CN.col(q) += q*tp2;PSCN.col(q) += q*tp2;}
      (CN.submat(0,3,1,cnMax)).fill(999);(CN.submat(3,0,cnMax,1)).fill(999); // an insertion and an deletion cannot happen at the same location
      (PSCN.submat(0,2,0,cnMax)).fill(999);(PSCN.submat(2,0,cnMax,0)).fill(999); // an insertion and an deletion cannot happen at the same location
      L_int = square(CN-cn)/(var_tcn/IT_new(6))/2.0;
      L_tot = L_int;
      for(int r1=0;r1<CnMax;r1++){
        for(int r2=0;r2<CnMax;r2++){
          if(r1>pscnMax){r1_max = pscnMax;r1_min = r1 - pscnMax;}else{r1_max=r1;r1_min=0;}
          if(r2>pscnMax){r2_max = pscnMax;r2_min = r2 - pscnMax;}else{r2_max=r2;r2_min=0;}
          r1_len = r1_max-r1_min+1; r2_len = r2_max-r2_min+1;
          arma::mat BAF(r1_len,r1_len); // matrix to store baf and discrepency
          BAF = PSCN.submat(r1_min,r2_min,r1_max,r2_max)/cn; // need to work on this to solve 0/0 situation
          arma::mat prior(r1_len,r2_len); // matrix to store prior
          arma::mat L_baf(r1_len,r2_len); // matrix to store likelihood for baf plus prior for current CN r1, r2
          L_baf = square(BAF-baf)/(var_baf/B_new(6))/2.0;
          prior = lp.submat(r1_min,r2_min,r1_max,r2_max)+rlp.submat(cnMax-r1_max,cnMax-r2_max,cnMax-r1_min,cnMax-r2_min);
          L_baf = L_baf + prior;
          lmax_baf = L_baf.min(row,col);
          // store pscn for each cn
          pscn_temp1(r1,r2) = col;
          pscn_temp2(r1,r2) = row;
          L_tot(r1,r2) += lmax_baf;
        }
      }
      L(i,j) = L_tot.min(row,col);
      cn1(i,j) = col;
      cn2(i,j) = row;
      //if (i < 5) {cn1.print("cn1:");}
      pscn1(i,j) = pscn_temp1(row,col);
      pscn2(i,j) = pscn_temp2(row,col);
      //if (i > 98) {pscn1.print("pscn1:");}
      //std::cout << std::setprecision (7) << temp_cn << std::endl;
    }
    for(int j=(top+1);j<=50;j++) L(i,j)=9999.0;
  }
  out[0] = L;
  out[1] = cn1;
  out[2] = cn2;
  out[3] = pscn1;
  out[4] = pscn2;
  return out;
}
