# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
//' calculate baf for 1 normal + 2 tumors case
//' @keywords internal
//' @export
// [[Rcpp::export]]
List calcll_baf(double lrr,double nrc,double baf,double n_baf,arma::mat lprior_f_2d,arma::mat rlprior_f_2d,double scale, int pscnMax, int MaxCn){
  double lmax_baf;
  double trc = exp(lrr)*nrc*2.0;
  int r1_cap; int r2_cap;
  arma::mat L_int(MaxCn+1,MaxCn+1); // matrix to store likelihood for CN
  arma::mat L_tot(MaxCn+1,MaxCn+1); // matrix to store likelihood for CN
  arma::mat CN(MaxCn+1,MaxCn+1);
  arma::mat PSCN(MaxCn+1,MaxCn+1);
  arma::mat Mean(MaxCn+1,MaxCn+1);
  double Var;
  arma::mat L(100,51);
  arma::uword row; arma::uword col;
  // added for cn and pscn
  arma::mat pscn1(100,51);
  arma::mat pscn2(100,51);
  arma::mat cn1(100,51);
  arma::mat cn2(100,51);
  arma::mat pscn_temp1(MaxCn+1,MaxCn+1);
  arma::mat pscn_temp2(MaxCn+1,MaxCn+1);
  List out(5);
  // end

  for(int i = 0; i < 100; i++){
    int top = (101-i)/2;
    double np = i*0.01; // normal cell percentage
    for(int j = 0; j <= top; j++){
      CN.fill(np*2.0);PSCN.fill(np);
      double tp1 = j*0.01; // tumor cell 1 percentage
      double tp2 = 1.0-np-tp1; // tumor cell 2 percentage
      for(int q=0; q<=MaxCn; q++){CN.row(q) += q*tp1;PSCN.row(q)+= q*tp1;CN.col(q) += q*tp2;PSCN.col(q) += q*tp2;}
      Mean = log(CN*scale/2.0);
      Var = sqrt(1.01/trc + 1.01/nrc/2.0);
      L_int = square(exp(lrr)-exp(Mean))/(0.001)/2.0;// likelihood for lrr
      L_tot = L_int;
      for(int r1=0;r1<= MaxCn;r1++){
        for(int r2=0;r2<= MaxCn;r2++){
          //pscnMax is the maximum pscn allowed --- again, not pscn
          r1_cap = (r1>pscnMax)*pscnMax + (r1<=pscnMax)*r1;
          r2_cap = (r2>pscnMax)*pscnMax + (r2<=pscnMax)*r2;
          arma::mat BAF(r1_cap+1,r2_cap+1); // matrix to store baf and discrepency
          arma::mat prior(r1_cap+1,r2_cap+1); // matrix to store prior
          arma::mat L_baf(r1_cap+1,r2_cap+1); // matrix to store likelihood for baf plus prior for current CN r1, r2
          BAF = PSCN.submat(0,0,r1_cap,r2_cap)/CN.at(r1,r2); // need to work on this to solve 0/0 situation
          //          L_baf = square(BAF-baf)/(0.01/sqrt(n_baf))/2.0;prior = lprior_f_2d.submat(0,0,r1_cap,r2_cap) + rlprior_f_2d.submat(MaxCn-r1_cap,MaxCn-r2_cap,MaxCn,MaxCn);
          L_baf = square(BAF-baf)/2.0/0.001;prior = lprior_f_2d.submat(0,0,r1_cap,r2_cap) + rlprior_f_2d.submat(MaxCn-r1_cap,MaxCn-r2_cap,MaxCn,MaxCn);
          L_baf = L_baf + prior;
          lmax_baf = L_baf.min(row,col);
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
    }
    for(int j=(top+1);j<=50;j++) L(i,j)=999999.0;
  }
  out[0] = L;
  out[1] = cn1;
  out[2] = cn2;
  out[3] = pscn1;
  out[4] = pscn2;
  return out;
}
