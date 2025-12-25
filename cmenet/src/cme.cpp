// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <iostream>
#include <float.h>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace std;
using namespace Rcpp;

//Compares two doubles for equality
bool dbleq(double a, double b)
{
  return (fabs(a - b) < DBL_EPSILON);
}

//Provides ranked indices
template <typename T>
vector<int> sort_idx(const vector<T> &v) {

  // initialize original index locations
  vector<int> idx(v.size());
  iota(idx.begin(), idx.end(), 0);

  // sort indexes based on comparing values in v
  sort(idx.begin(), idx.end(),
       [&v](size_t i1, size_t i2) {return v[i1] < v[i2];});

  return idx;
}

//Computes the indices for inverse pairs
vector<int> inv_ind(int pme){

  vector<int> ret(2*pme*(pme-1));

  arma::mat tmpmat(pme,pme); //temporary matrix for obtaining indices
  int count = 0;
  for (int i=0;i<pme;i++){
    for (int j=(i+1);j<pme;j++){
      tmpmat(i,j) = count;
      count++;
    }
  }
  for (int i=0;i<pme;i++){//symmetrize
    for (int j=0;j<i;j++){
      tmpmat(i,j) = tmpmat(j,i);
    }
  }

  //Retrieve indices
  int indct = 0;
  for (int i=0;i<pme;i++){
    for (int j=0;j<pme;j++){
      if (i != j){
        ret[indct] = tmpmat(i,j);
        ret[indct+1] = tmpmat(i,j);
        indct = indct + 2;
      }
    }
  }

  return (ret);
}

//Threshold function for ME (varying lambda)
double s_me(double inprod, NumericVector& lambda, double gamma, NumericVector& delta){

  // inprod - inner product to threshold
  // lambda - penalties for reg, sib, cou and inv (ignore inv for now)
  // gamma - assumed fixed
  // delta - linearized penalties for reg, sib, cou and inv (ignore inv for now)
  // nn - the number of observations, n

  //Rank penalties and their ratios
  std::vector<double> lambda_r(2);
  std::vector<double> delta_r(2);
  std::vector<double> ratio(2);
  if (lambda[0] <= lambda[1]){
    lambda_r[0] = lambda[0];
    lambda_r[1] = lambda[1];
    delta_r[0] = delta[0];
    delta_r[1] = delta[1];
    ratio[0] = delta[0]/lambda[0];
    ratio[1] = delta[1]/lambda[1];
  }
  else{
    lambda_r[0] = lambda[1];
    lambda_r[1] = lambda[0];
    delta_r[0] = delta[1];
    delta_r[1] = delta[0];
    ratio[0] = delta[1]/lambda[1];
    ratio[1] = delta[0]/lambda[0];
  }

  //Compute thresholds
  double ret = 0.0;
  double sgn = 0.0;
  if (inprod < 0.0){
    sgn = -1.0;
  }
  else{
    sgn = 1.0;
  }

  if (abs(inprod) < (lambda_r[0]*gamma + delta_r[1]*(1-lambda_r[0]/lambda_r[1]))  ){
    if (abs(inprod) > (delta_r[0]+delta_r[1]) ){
      ret = ( abs(inprod)-(delta_r[0]+delta_r[1]) ) / (1.0 - 1.0/gamma*(ratio[0]+ratio[1]) );
    }
    else{
      ret = 0.0;
    }
  }
  else if (abs(inprod) < lambda_r[1]*gamma){
    if (abs(inprod) > (delta_r[1])){
      ret = ( abs(inprod)-(delta_r[1]) ) / (1.0 - 1.0/gamma*(ratio[1]) );
    }
    else{
      ret = 0.0;
    }
  }
  else{
    ret = abs(inprod);
  }

  return (sgn*ret);

}

// //Threshold function for CMEs (varying lambda)
// double s_cme(double inprod, NumericVector& lambda, double gamma, std::vector<double>& delta, int nn){
//
//   // inprod - inner product to threshold
//   // lambda - penalties for reg, sib, cou and inv
//   // gamma - assumed fixed
//   // delta - linearized penalties for reg, sib, cou and inv
//   // nn - the number of observations, n
//
//   double n = (double) nn;
//
//   //Rank penalties and their ratios
//   std::vector<double> lambda_r(4);
//   std::vector<double> ratio_r(4);
//   for (int i=0; i<3; i++){
//     lambda_r[i] = lambda[i];
//   }
//   std::vector<int> idx = sort_idx(lambda_r);
//   for (int i=0; i<3; i++){
//     ratio_r[i] = delta[idx[i]]/lambda_r[idx[i]];
//   }
//   std::sort(lambda_r.begin(),lambda_r.end());
//
//   //Compute thresholds
//   double ret;
//   double sgn = 0.0;
//   if (inprod < 0.0){
//     sgn = -1.0;
//   }
//   else{
//     sgn = 1.0;
//   }
//
//   if (abs(inprod) < lambda_r[0]*gamma){
//     if (abs(inprod) > (lambda_r[0]+lambda_r[1]+lambda_r[2]+lambda_r[3]) ){
//       ret = ( abs(inprod)-(lambda_r[0]+lambda_r[1]+lambda_r[2]+lambda_r[3]) ) / (1.0 - 1.0/gamma * (ratio_r[0]+ratio_r[1]+ratio_r[2]+ratio_r[3]) );
//     }
//   }
//   else if (abs(inprod) < lambda_r[1]*gamma){
//     if (abs(inprod) > (lambda_r[1]+lambda_r[2]+lambda_r[3])){
//       ret = ( abs(inprod)-(lambda_r[1]+lambda_r[2]+lambda_r[3]) ) / (1.0 - 1.0/gamma * (ratio_r[1]+ratio_r[2]+ratio_r[3]) );
//     }
//   }
//   else if (abs(inprod) < lambda_r[2]*gamma){
//     if (abs(inprod) > (lambda_r[2]+lambda_r[3])){
//       ret = ( abs(inprod)-(lambda_r[2]+lambda_r[3]) ) / (1.0 - 1.0/gamma * (ratio_r[2]+ratio_r[3]) );
//     }
//   }
//   else if (abs(inprod) < lambda_r[3]*gamma){
//     if (abs(inprod) > (lambda_r[3])){
//       ret = ( abs(inprod)-(lambda_r[3]) ) / (1.0 - 1.0/gamma * (ratio_r[3]) );
//     }
//   }
//   else{//inprod >= lambda_r[2]*gamma
//     ret = abs(inprod);
//   }
//
//   return (sgn*ret);
//
// }
//
// //Threshold function for MCP
// double s_mcp(double inprod, double lambda, double gamma){
//
//   double ret = 0.0;
//   double sgn = 0.0;
//   if (inprod < 0.0){
//     sgn = -1.0;
//   }
//   else{
//     sgn = 1.0;
//   }
//
//   if (abs(inprod) <= lambda){
//     ret = 0.0;
//   }
//   else if(abs(inprod) <= (lambda*gamma)){
//     ret = sgn * (abs(inprod)-lambda) / (1.0 - (1.0/gamma));
//   }
//   else{
//     ret = inprod;
//   }
//
//   return (ret);
// }

//MCP penalty
// [[Rcpp::export]]
double mcp(double beta, double lambda, double gamma){
  double ret = 0.0;
  if (abs(beta) <= (lambda*gamma) ){
    ret = abs(beta) - pow(beta,2.0)/(2.0*lambda*gamma);
  }
  else{
    ret = lambda*gamma/2.0;
  }
  return(ret);
}

//KKT condition
bool kkt(double inprod, NumericVector cur_delta){
  //Checks KKT condition for \beta=0.0
  bool ret;
  double lb = -1.0*inprod - cur_delta[0] - cur_delta[1];
  double ub = -1.0*inprod + cur_delta[0] + cur_delta[1];
  if ((0.0 >= lb)&&(0.0 <= ub)){
    ret = true; //kkt satisfied
  }
  else{
    // cout << "lb: " << lb << ", ub: " << ub << endl;
    ret = false;
  }
  return(ret);
}

//One run of coordinate descent
bool coord_des_onerun(int pme, int nn, NumericVector& lambda, NumericVector& cur_delta,
                      bool dummy, double tau, double gamma,
                      vector<double>& X_me, vector<double>& X_cme,
                      vector<double>& delta_sib, vector<double>& delta_cou,
                      vector<bool>& act_me, vector<bool>& act_cme,
                      vector<double>& beta_me, vector<double>& beta_cme,
                      vector<double>& resid){

  bool chng_flag = false;
  double cur_beta = 0.0;
  double inprod = 0.0;

  //CD for main effects
  for (int j=0;j<pme;j++){
    //Only update if active
    if (act_me[j]){
      cur_beta = beta_me[j];

      //Compute inner product
      inprod = 0.0;
      for (int k=0;k<nn;k++){
        inprod += (resid[k]*X_me[j*nn+k]);
      }
      // inprod = inprod/((double)nn)+beta_me[j];
      inprod = inprod/((double)nn)+(((double)nn)-1)/((double)nn)*beta_me[j]; //checked to pod from update eqn (mod from above eqn)

      //Update cur_delta
      cur_delta[0] = delta_sib[j];
      cur_delta[1] = delta_cou[j];

      //Perform ME thresholding
      beta_me[j] = s_me(inprod,lambda,gamma,cur_delta);

      // Update residual and delta
      if (!dbleq(beta_me[j],cur_beta)){ // if beta changed...

        //Update residual vector
        for (int k=0;k<nn;k++){
          resid[k] = resid[k] - X_me[j*nn+k]*(beta_me[j]-cur_beta);
        }

        //Update deltas
        double offset_sib = mcp(beta_me[j],lambda[0],gamma)-mcp(cur_beta,lambda[0],gamma); // new - old
        double offset_cou = mcp(beta_me[j],lambda[1],gamma)-mcp(cur_beta,lambda[1],gamma);
        delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        delta_cou[j] = delta_cou[j] * (exp(-(tau/lambda[1]) * offset_cou ));

        //Update flag
        chng_flag = true;
      }
    }
  }

  cur_beta = 0.0;
  inprod = 0.0;
  //CD for CME effects
  for (int j=0;j<pme;j++){ //parent effect
    for (int k=0;k<(2*(pme-1));k++){ //conditioned effect

      int cmeind = j*(2*(pme-1))+k; //index for CME
      int condind = 0; //index for condition

      if (act_cme[cmeind]){
        int condind = floor((double)k/2.0);
        if (condind >= j){
          condind ++;
        }
        cur_beta = beta_cme[cmeind]; //beta_cme ordered by parent effects, then condition

        //Update cur_delta
        cur_delta[0] = delta_sib[j];
        cur_delta[1] = delta_cou[condind];

        //Compute inner product
        inprod = 0.0;
        for (int l=0;l<nn;l++){
          inprod += (resid[l]*X_cme[cmeind*nn+l]);
        }
        // inprod = inprod/((double)nn)+beta_cme[cmeind];
        inprod = inprod/((double)nn)+(((double)nn)-1)/((double)nn)*beta_cme[cmeind];

        //Perform CME thresholding
        beta_cme[cmeind] = s_me(inprod,lambda,gamma,cur_delta);

        //Update residual and delta
        if (!dbleq(beta_cme[cmeind],cur_beta)){ // if beta changed...

          //Update residual vector
          for (int ll=0;ll<nn;ll++){
            resid[ll] = resid[ll] - X_cme[cmeind*nn+ll]*(beta_cme[cmeind]-cur_beta);
          }

          //Update deltas
          double offset_sib = mcp(beta_cme[cmeind],lambda[0],gamma)-mcp(cur_beta,lambda[0],gamma); // new - old
          double offset_cou = mcp(beta_cme[cmeind],lambda[1],gamma)-mcp(cur_beta,lambda[1],gamma); // new - old
          delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib )); // update delta for siblings
          delta_cou[condind] = delta_cou[condind] * (exp(-(tau/lambda[1]) * offset_cou )); // update delta for cousins

          //Update flag
          chng_flag = true;
        }

        // //Reduce A|B+ and A|B- to A
        // if (abs(beta_cme[cmeind]) > 0.0){ //if current CME is active
        //   if (k % 2 == 0){ //cme is .|.+
        //     if (abs(beta_cme[cmeind+1]) > 0.0){ //if cme .|.- is also in model...
        //
        //       double chg, cur_beta_me, cur_beta_cme1, cur_beta_cme2;
        //
        //       if ( abs(beta_cme[cmeind]) > abs(beta_cme[cmeind+1]) ){// if abs(.|.+) > abs(.|.-)
        //         chg = beta_cme[cmeind+1]; // change
        //         cur_beta_me = beta_me[j]; // current beta me
        //         cur_beta_cme1 = beta_cme[cmeind]; // current beta cme 1
        //         cur_beta_cme2 = beta_cme[cmeind+1]; // current beta cme 2
        //         beta_me[j] += chg; // update ME with smaller CME
        //         beta_cme[cmeind] -= chg; // update larger CME
        //         beta_cme[cmeind+1] = 0.0; // remove smaller CME
        //       }else{// if abs(.|.+) < abs(.|.-)
        //         chg = beta_cme[cmeind]; // change
        //         cur_beta_me = beta_me[j]; // current beta me
        //         cur_beta_cme1 = beta_cme[cmeind]; // current beta cme 1
        //         cur_beta_cme2 = beta_cme[cmeind+1]; // current beta cme 2
        //         beta_me[j] += chg; // update ME with smaller CME
        //         beta_cme[cmeind+1] -= chg; // update larger CME
        //         beta_cme[cmeind] = 0.0; // remove smaller CME
        //       }
        //
        //       //Update deltas and flag
        //       double offset_sib = mcp(beta_me[j],lambda[0],gamma)-mcp(cur_beta_me,lambda[0],gamma); // new - old (for me)
        //       double offset_cou = mcp(beta_me[j],lambda[1],gamma)-mcp(cur_beta_me,lambda[1],gamma);
        //       delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        //       delta_cou[j] = delta_cou[j] * (exp(-(tau/lambda[1]) * offset_cou ));
        //
        //       offset_sib = mcp(beta_cme[cmeind],lambda[0],gamma)-mcp(cur_beta_cme1,lambda[0],gamma); // new - old (for .|.+)
        //       offset_cou = mcp(beta_cme[cmeind],lambda[1],gamma)-mcp(cur_beta_cme1,lambda[1],gamma);
        //       delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        //       delta_cou[condind] = delta_cou[condind] * (exp(-(tau/lambda[1]) * offset_cou ));
        //
        //       offset_sib = mcp(beta_cme[cmeind+1],lambda[0],gamma)-mcp(cur_beta_cme2,lambda[0],gamma); // new - old (for .|.-)
        //       offset_cou = mcp(beta_cme[cmeind+1],lambda[1],gamma)-mcp(cur_beta_cme2,lambda[1],gamma);
        //       delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        //       delta_cou[condind] = delta_cou[condind] * (exp(-(tau/lambda[1]) * offset_cou ));
        //
        //       //residuals shouldn't change
        //
        //     }
        //   }else{ //cme is .|.-
        //     if (abs(beta_cme[cmeind-1]) > 0.0){ //if cme .|.+ is also in model...
        //
        //       double chg, cur_beta_me, cur_beta_cme1, cur_beta_cme2;
        //
        //       if ( abs(beta_cme[cmeind]) > abs(beta_cme[cmeind-1]) ){// if abs(.|.+) < abs(.|.-)
        //         chg = beta_cme[cmeind-1]; // change
        //         cur_beta_me = beta_me[j]; // current beta me
        //         cur_beta_cme1 = beta_cme[cmeind]; // current beta cme 1
        //         cur_beta_cme2 = beta_cme[cmeind-1]; // current beta cme 2
        //         beta_me[j] += chg; // update ME with smaller CME
        //         beta_cme[cmeind] -= chg; // update larger CME
        //         beta_cme[cmeind-1] = 0.0; // remove smaller CME
        //       }else{// if abs(.|.+) > abs(.|.-)
        //         chg = beta_cme[cmeind]; // change
        //         cur_beta_me = beta_me[j]; // current beta me
        //         cur_beta_cme1 = beta_cme[cmeind]; // current beta cme 1
        //         cur_beta_cme2 = beta_cme[cmeind-1]; // current beta cme 2
        //         beta_me[j] += chg; // update ME with smaller CME
        //         beta_cme[cmeind-1] -= chg; // update larger CME
        //         beta_cme[cmeind] = 0.0; // remove smaller CME
        //       }
        //
        //       //Update deltas and flag
        //       double offset_sib = mcp(beta_me[j],lambda[0],gamma)-mcp(cur_beta_me,lambda[0],gamma); // new - old (for me)
        //       double offset_cou = mcp(beta_me[j],lambda[1],gamma)-mcp(cur_beta_me,lambda[1],gamma);
        //       delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        //       delta_cou[j] = delta_cou[j] * (exp(-(tau/lambda[1]) * offset_cou ));
        //
        //       offset_sib = mcp(beta_cme[cmeind],lambda[0],gamma)-mcp(cur_beta_cme1,lambda[0],gamma); // new - old (for .|.+)
        //       offset_cou = mcp(beta_cme[cmeind],lambda[1],gamma)-mcp(cur_beta_cme1,lambda[1],gamma);
        //       delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        //       delta_cou[condind] = delta_cou[condind] * (exp(-(tau/lambda[1]) * offset_cou ));
        //
        //       offset_sib = mcp(beta_cme[cmeind-1],lambda[0],gamma)-mcp(cur_beta_cme2,lambda[0],gamma); // new - old (for .|.-)
        //       offset_cou = mcp(beta_cme[cmeind-1],lambda[1],gamma)-mcp(cur_beta_cme2,lambda[1],gamma);
        //       delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * offset_sib ));
        //       delta_cou[condind] = delta_cou[condind] * (exp(-(tau/lambda[1]) * offset_cou ));
        //
        //       //residuals shouldn't change
        //
        //     }
        //   }
        // }

      }
    }
  }

  return(chng_flag);

}

// [[Rcpp::export]]
List cme(NumericMatrix& XX_me, NumericMatrix& XX_cme, NumericVector& yy,
         NumericVector& lambda_sib_vec, NumericVector& lambda_cou_vec,
         NumericVector& gamma_vec, NumericVector& tau_vec,
         NumericVector& XX_me_sl, NumericVector& XX_cme_sl, NumericVector& beta_vec, NumericVector& act_vec,
         double lambda_max, int it_max, int it_warm, int reset, bool screen_ind) {
  // // [[Rcpp::plugins(openmp)]]
  //------------------------------------------------------------
  // XX - Full model matrix including both ME and CME effects (assume normalized)
  // yy - Response vector of length nn
  // lambda_sib_vec - Vector of sibling penalties (decr. sequence)
  // lambda_cou_vec - Vector of cousin penalties (decr. sequence)
  // tau - Exponential penalty parameter
  // gamma - MC+ non-convex penalty parameter
  // beta_vec - Initial beta value
  // it_max - Maximum iterations for coordinate descent
  //------------------------------------------------------------

  //Variable initialization
  int pme = XX_me.ncol(); //# of MEs
  int pcme = XX_cme.ncol(); //# of CMEs
  int nn = XX_me.nrow(); //# of observations
  int nlambdasib = lambda_sib_vec.size();
  int nlambdacou = lambda_cou_vec.size();
  int it_inner = 0;
  int it_max_reset = it_max / reset;
  bool cont = true;
  bool chng_flag = false;

  //Vectorize model matrices
  vector<double> X_me(nn*pme); //for ME
  vector<double> X_cme(nn*pcme); //for CME
  for (int i=0;i<pme;i++){
    for (int j=0;j<nn;j++){
      X_me[i*nn+j] = XX_me(j,i);
    }
  }
  for (int i=0;i<pcme;i++){
    for (int j=0;j<nn;j++){
      X_cme[i*nn+j] = XX_cme(j,i);
    }
  }

  //Check whether lambda is to be iterated or not
  bool lambda_it;
  int niter_1; //Number to iterate first
  int niter_2; //Number to iterate next
  if (gamma_vec.size()>1){ //Iterate on gamma and tau
    lambda_it = false;
    // niter_1 = gamma_vec.size(); //ch
    // niter_2 = tau_vec.size();
    niter_1 = tau_vec.size();
    niter_2 = gamma_vec.size();
  }
  else{
    lambda_it = true;
    niter_1 = nlambdasib;
    niter_2 = nlambdacou;
  }

  //Containers for beta and active set (alpha)
  arma::cube beta_cube(pme+pcme,niter_1,niter_2); //betas to return
  arma::cube delta_sib_cube(pme,niter_1,niter_2); //deltas to return
  arma::cube delta_cou_cube(pme,niter_1,niter_2); //deltas to return
  arma::mat nz(niter_1,niter_2);
  arma::mat beta_mat(pme+pcme,niter_1);
  arma::mat delta_sib_mat(pme,niter_1);
  arma::mat delta_cou_mat(pme,niter_1);
  vector<double> beta_me(pme,0.0); //for MEs
  for (int i=0;i<pme;i++){
    beta_me[i] = beta_vec[i];
  }
  vector<double> beta_cme(pcme,0.0); //for CMEs
  for (int i=0;i<pcme;i++){
    beta_cme[i] = beta_vec[pme+i];
  }
  double cur_beta = 0.0; //running beta

  //Set all factors as active to begin
  vector<bool> act_me(pme,true); //Current active set
  vector<bool> act_cme(pcme,true);
  vector<bool> scr_me(pme,true); //Screened active set
  vector<bool> scr_cme(pcme,true);
  bool kkt_bool;

  //Containers for linearized slopes Delta
  vector<double> delta_sib(pme); //Linearized penalty for siblings (sib(A), sib(B), ...)
  vector<double> delta_cou(pme); //Linearized penalty for cousins (cou(A), cou(B), ...)
  NumericVector lambda(2); //Current penalties
  NumericVector cur_delta(2); //Current delta vector
  double gamma;
  double tau;
  lambda[0] = lambda_sib_vec[0];
  lambda[1] = lambda_cou_vec[0];
  gamma = gamma_vec[0];
  tau = tau_vec[0];

  //Update residuals
  vector<double> resid(nn); //Residual vector
  arma::cube resid_cube(nn,niter_1,niter_2);
  arma::mat resid_mat(nn,niter_1);
  arma::cube scr_cube(pme+pcme,niter_1,niter_2); //screening vector
  arma::mat scr_mat(pme+pcme,niter_1);

  double ymean = 0.0;
  for (int i=0;i<nn;i++){
    ymean += (1.0/(double)nn)*yy(i);
  }
  for (int i=0;i<nn;i++){
    resid[i] = yy(i) - ymean;
  }
  double inprod = 0.0; //inner product
  double thresh = 0.0; //threshold for screening
  int num_act = 0;

  vector<bool> kkt_v_me(pme,true);
  vector<bool> kkt_v_cme(pcme,true); //KKT checks

  // Optimize for each penalty combination
  // #pragma omp parallel for
  for (int b=0; b<niter_2; b++){ //iterate over cousins...

    for (int a=0; a<niter_1; a++){ //iterate over siblings...

      // cout << "Tuning ... a = " << a << ", b = " << b << endl;

      //Update iteration variables
      if (lambda_it){
        lambda[0] = lambda_sib_vec[a];
        lambda[1] = lambda_cou_vec[b];
      }
      else{
        // gamma = gamma_vec[a]; //ch
        // tau = tau_vec[b];
        tau = tau_vec[a];
        gamma = gamma_vec[b];
      }

      //Return trivial solution of \beta=0 when \lambda_s + \lambda_c >= \lambda_max
      // if ( (lambda[0]+lambda[1]) >= lambda_max){
      if ( (a==0) || ( (lambda[0]+lambda[1]) >= lambda_max) ){
        for (int i=0;i<pme;i++){//reset beta
          beta_me[i] = 0.0;
        }
        for (int i=0;i<pcme;i++){
          beta_cme[i] = 0.0;
        }
        for (int i=0;i<nn;i++){//reset residuals
          resid[i] = yy(i) - ymean;
        }
        num_act = 0;
        for (int i=0;i<pme;i++){//reset active flag
          act_me[i] = true;
          scr_me[i] = true;
          num_act ++;
        }
        for (int i=0;i<pcme;i++){
          act_cme[i] = true;
          scr_cme[i] = true;
          num_act ++;
        }
        // cout << "num_act: " << num_act << endl;
        if ( (lambda[0]+lambda[1]) >= lambda_max){
          goto cycend;
        }
      }

      // // RESET AFTER EACH RUN
      for (int i=0;i<pme;i++){//reset beta
        beta_me[i] = 0.0;
      }
      for (int i=0;i<pcme;i++){
        beta_cme[i] = 0.0;
      }
      for (int i=0;i<nn;i++){//reset residuals
        resid[i] = yy(i) - ymean;
      }

      //Recompute deltas
      fill(delta_sib.begin(),delta_sib.end(),lambda[0]);
      fill(delta_cou.begin(),delta_cou.end(),lambda[1]);
      for (int j=0; j<pme; j++){
        delta_sib[j] = delta_sib[j] * ( exp( -(tau/lambda[0]) * mcp(beta_me[j],lambda[0],gamma) ) );
        delta_cou[j] = delta_cou[j] * ( exp( -(tau/lambda[1]) * mcp(beta_me[j],lambda[1],gamma) ) );
      }
      for (int j=0;j<pme;j++){ //parent effect
        for (int k=0;k<(2*(pme-1));k++){ //conditioned effect
          int cmeind = j*(2*(pme-1))+k;
          int condind = floor((double)k/2.0);
          if (condind >= j){
            condind ++;
          }
          delta_sib[j] = delta_sib[j] * (exp(-(tau/lambda[0]) * mcp(beta_cme[cmeind],lambda[0],gamma) ));
          delta_cou[condind] = delta_cou[condind] * (exp(-(tau/lambda[1]) * mcp(beta_cme[cmeind],lambda[1],gamma) ));
        }
      }

      //Coordinate descent with warm active set resets
      for (int q=0; q<reset; q++){

        //Active set reset for it_warm iterations
        for (int m=0; m<it_warm; m++){
          chng_flag = coord_des_onerun(pme, nn, lambda, cur_delta, chng_flag, tau, gamma, X_me, X_cme,
                                       delta_sib, delta_cou, act_me, act_cme, beta_me, beta_cme, resid);
        }

        //Update active set
        int num_act = 0;
        for (int j=0;j<pme;j++){
          if ((abs(beta_me[j])>0.0)||(act_vec[j]>0.0)){
            act_me[j] = true;
            num_act ++;
          }
          else{
            act_me[j] = false;
          }
        }
        for (int j=0;j<pcme;j++){
          if ((abs(beta_cme[j])>0.0)||(act_vec[j+pme]>0.0)){
            act_cme[j] = true;
            num_act ++;
          }
          else{
            act_cme[j] = false;
          }
        }

        //Cycle on active set
        it_inner = 0; //inner iteration count
        cont = true; //continue flag
        chng_flag = false; //change flag

        while (cont){
          // cout << it_inner << endl;

          //Increment count and update flags
          it_inner ++;
          chng_flag = coord_des_onerun(pme, nn, lambda, cur_delta, chng_flag, tau, gamma, X_me, X_cme,
                                       delta_sib, delta_cou, act_me, act_cme, beta_me, beta_cme, resid);

          //Update cont flag for termination
          if ( (it_inner >= it_max_reset)||(!chng_flag) ){
            cont = false;
          }
        }//end while

        // Rcout << accumulate(act_me.begin(),act_me.end(),0) << endl;
        // Rcout << accumulate(act_cme.begin(),act_cme.end(),0) << endl;

      }

      cycend:

        //Copy into beta_mat, and warm-start next cycle
        int betacount = 0;
      int betanz = 0;
      for (int k=0;k<pme;k++){
        if (abs(beta_me[k])>0.0){
          betanz++;
        }
        beta_mat(betacount,a) = beta_me[k];
        betacount++;
      }
      for (int k=0;k<pcme;k++){
        if (abs(beta_cme[k])>0.0){
          betanz++;
        }
        beta_mat(betacount,a) = beta_cme[k];
        betacount++;
      }
      nz(a,b) = betanz;

      //Copy deltas
      for (int k=0;k<pme;k++){
        delta_sib_mat(k,a) = delta_sib[k];
        delta_cou_mat(k,a) = delta_cou[k];
      }

      //Copy residuals
      for (int k=0;k<nn;k++){
        resid_mat(k,a) = resid[k];
      }

      //Copy screening data
      for (int k=0;k<pme;k++){
        // scr_mat(k,a) = scr_me[k];
        scr_mat(k,a) = act_me[k];
      }
      for (int k=0;k<pcme;k++){
        // scr_mat(pme+k,a) = scr_cme[k];
        scr_mat(pme+k,a) = act_cme[k];
      }

    }//end nlambda.sib (a)

    //Copy into beta_cube
    beta_cube.slice(b) = beta_mat;
    delta_sib_cube.slice(b) = delta_sib_mat;
    delta_cou_cube.slice(b) = delta_cou_mat;
    resid_cube.slice(b) = resid_mat;
    scr_cube.slice(b) = scr_mat;

  }//end nlambda.cou (b)

  //Rescale betas and compute intercepts
  // arma::mat inter_mat(niter_1,niter_2);
  for (int b=0; b<niter_2; b++){ //iterate over cousins...
    for (int a=0; a<niter_1; a++){ //iterate over siblings...
      //Rescale betas to original scale
      for (int k=0;k<pme;k++){
        // beta_cube(k,a,b) = beta_cube(k,a,b);
        beta_cube(k,a,b) = beta_cube(k,a,b)/XX_me_sl(k);
      }
      for (int k=0;k<pcme;k++){
        // beta_cube(pme+k,a,b) = beta_cube(pme+k,a,b);
        beta_cube(pme+k,a,b) = beta_cube(pme+k,a,b)/XX_cme_sl(k);
      }
    }
  }

  return (List::create(Named("coefficients") = beta_cube,
                       Named("residuals") = resid_cube,
                       // Named("nzero") = nz,
                       Named("lambda_sib") = lambda_sib_vec,
                       Named("lambda_cou") = lambda_cou_vec,
                       Named("act") = scr_cube,
                       Named("gamma") = gamma,
                       Named("tau") = tau,
                       Named("y") = yy
  ));

}
