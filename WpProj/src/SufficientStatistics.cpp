#include "SufficientStatistics.h"
// #include <iostream>
// #include <fstream>
using namespace Rcpp;

void suff_stat_arrange_Y(const refMatConst & mu, const refMatConst & Y,
                       const int S, const int P, const int N,
                       const std::string & transport_method,
                       matrixI & idx, vector & mass,
                       bool a_sort, double epsilon, int niter) {
  double ground_p = 2.0;
  double p_wass = 2.0;
  // transport(const matrix & A, const matrix & B, const double p, const double ground_p,
  //           matrixI & idx, vector & mass, const std::string & method, bool & a_sort,
  //           double epsilon, int niter)
  transport(Y, mu, p_wass, ground_p, idx, mass, transport_method, a_sort,
            epsilon, niter);
}

void arrange_Y(refMat Y, int S, const std::string & transport_method) {
  
  if(transport_method == "rank" || transport_method == "hilbert") {
    double p_wass = 2.0;
    double ground_p = 2.0;
    matrixI idx(S * S,2);
    vector mass(S * S);
    bool a_sort = true;
    transport(Y, Y, p_wass, ground_p, idx, mass, transport_method, a_sort, 0.0, 0);
    vectorI order(idx.rows());
    vecMapI idx_vec( idx.col(0).data(), idx.rows() );
    for(int i = 0; i < idx.rows(); i++ ) order(idx_vec(i)) = i;
    // Rcpp::Rcout << order.transpose() << "\n";
    // Rcpp::Rcout << Y(0,order(0)) << "\n";
    rel_sort_matrix_by_col(Y, order);
    
    // Y = Yt.transpose();
    
  } else if (transport_method == "univariate.approximation.pwr") {
    // Rcpp::Rcout << "\n\n" << idx.col(1)(0)<< "\n\n";
    
    sort_matrix_by_row(Y);
    
  } else {
    Rcpp::stop("Transport method not found");
  }
  // Rcpp::Rcout << Y(0,0) << "\n";
}

void univariate_approximation_xtx_xty(const refMatConst & X, const refMatConst & Y,
                                      const refMatConst & theta,
                                      const int N, const int S, const int P,
                                      matrix & xtx, matrix & xty
) 
{
  matrix temp(P, N); // dim of X
  xtx.fill(0.0);
  xty.fill(0.0);
  double dataWt = 1.0/double(N*S); 
  
  for ( int s = 0; s < S; s++ ) {
    // if(s % 100 == 0) Rcpp::checkUserInterrupt();
    
    temp = X.array().colwise() * theta.col(s).array();
    
    xtx.selfadjointView<Eigen::Lower>().rankUpdate(temp, dataWt);
    xty.noalias() +=  temp * Y.col(s) * dataWt;
  }
  
}

void ot_xtx_xty(const refMatConst & X, const refMatConst & Y,
                const refMatConst & theta,
                const int N, const int S, const int P,
                matrix & xtx, matrix & xty,
                const matrixI & idx, const vector & mass) 
{//updates xtx xty with OT 
  double dataWt = 1.0/double(N);
  matrix temp(P, N); // dim of X
  int num_atom = idx.rows();
  vecMapConstI idx_map0(idx.col(0).data(), num_atom);
  vecMapConstI idx_map1(idx.col(1).data(), num_atom);
  xtx.fill(0.0);
  xty.fill(0.0);
  int oldto = -1;
  
  for ( int i = 0; i < num_atom; i++ ) {
    // if(s % 100 == 0) Rcpp::checkUserInterrupt();
    int toidx = idx_map1(i);
    if( toidx > oldto) {//since can map to multiple atoms, only want to do this once
      temp = X.array().colwise() * theta.col(toidx).array();
      xtx.selfadjointView<Eigen::Lower>().rankUpdate(temp, dataWt/double(S) );
      oldto = toidx;
    }
    // Rcpp::Rcout << idx_map0(i) << ", " <<  mass(i)<<"\n";
    xty.noalias() += temp * Y.col(idx_map0(i)) * dataWt * mass(i);
  }
  // Rcpp::Rcout << mass.sum();
}

void suff_stat_scale_ot(const refMatConst & X, refMat Y,
                        const refMatConst & theta,
                        const bool not_same,
                        const int S, const int P, const int N,
                        matrix & xtx_dens, matrix & xty,
                        const std::string & transport_method,
                        double epsilon, int niter) {
  matrix xtx = matrix::Zero(P,P);
  bool y_sort = false;
  xty.fill(0.0);
  vector mass(S);
  matrixI idx(S,2);
  matrix mu = X.transpose() * theta;
  
  mass.fill(1.0/double(S));
  idx.col(0) = vectorI::LinSpaced(S,0,S-1);
  idx.col(1) = vectorI::LinSpaced(S,0,S-1);
  
  bool sort_end = ((!not_same) & (transport_method == "univariate.approximation.pwr" || transport_method == "hilbert" || transport_method == "rank"));
  int count = 0;
  if ( not_same ) {
    // void suff_stat_arrange_Y(const refMatConst & mu, refMat Y,
    //                          const int S, const int P, const int N,
    //                          const std::string & transport_method,
    //                          matrixI & idx, vector & mass,
    //                          bool a_sort)
    suff_stat_arrange_Y(mu, Y,
                        S, P, N,
                        transport_method,
                        idx, mass,
                        y_sort, epsilon, niter);
    if (transport_method == "univariate.approximation.pwr") {
      // Rcpp::Rcout << (idx.col(1) - idx.col(0)).sum() << "\n";
      // Rcpp::Rcout << Y(0,0) << "\n";
      rel_sort_matrix_by_entry(Y, idx.col(0));
      // Rcpp::Rcout << Y(0,0) << "\n";
    }
  }
  // Rcpp::Rcout << (Y - mu).norm() << "\n";
  if (transport_method == "univariate.approximation.pwr") {
    // void univariate_approximation_xtx_xty(const refMatConst & X, const refMatConst & Y,
    //                                       const refMatConst & theta,
    //                                       const int N, const int S,
    //                                       matrix & xtx, matrix & xty
    // ) 
    univariate_approximation_xtx_xty(X,Y,theta,N,S,P, xtx,xty);
  } else {
    // void ot_xtx_xty(const refMatConst & X, const refMatConst & Y,
    //                 const refMatConst & theta,
    //                 const int N, const int S, const int P,
    //                 matrix & xtx, matrix & xty,
    //                 const matrixI & idx, const vector & mass) 
    ot_xtx_xty(X,Y,theta,N,S,P, xtx,xty, idx, mass);
  }
  xtx_dens = xtx.selfadjointView<Eigen::Lower>();
  
  if ( sort_end ) {
    arrange_Y(Y,S,transport_method);
  }
}

void suff_stat_projection_ot(const refMatConst & X, refMat Y,
                             refMat theta,
                             const bool not_same,
                             const int S, const int P, const int N,
                             matrix & xtx, matrix & xty) {
  if(xty.cols() != S) xty.resize(P,S);
  
  double dataWt = 1.0 / double( N );
  
  xty = X * Y * dataWt; //X is already PxN and Y is NxS so product is PxS (like theta)
  xtx = matrix( P, P ).setZero().selfadjointView<Lower>().rankUpdate(X , dataWt);
  
}

void sufficient_stat(const refMatConst & X, refMat Y,
                     refMat theta,
                     const bool not_same,
                     const int S, const int P, const int N,
                     matrix & xtx, matrix & xty,
                     const Rcpp::CharacterVector & method,
                     const std::string & transport_method,
                     double epsilon, int niter) {
  if ( (method(0) == "scale") || (method(0) == "selection.variable") ){
    // suff_stat_scale(X, Y, theta, not_same, S, P, N, mix_wt, xtx, xty, theta_norm);
    suff_stat_scale_ot(X, Y, theta, not_same, S, P, N, xtx, xty, transport_method,
                       epsilon, niter);
    // } else if (method(0) == "location.scale") {
    // suff_stat_loc_scale(X, Y, theta, not_same, S, P, N,pseudo_obs, xtx, xty, theta_norm);
  } else if ( method(0) == "projection" ) {
    // suff_stat_projection(X, Y, theta, not_same, S, P, N,  mix_wt, xtx, xty, theta_norm);
    suff_stat_projection_ot(X, Y, theta, not_same, S, P, N,  xtx, xty);
  } else {
    Rcpp::stop("Method not found in sufficient statistics calculation");
  }
  
}



void ot_xty(const refMatConst & X, const refMatConst & Y,
            const refMatConst & theta, const refMatConst & mu,
            const int S, const int N, const int P,
            matrix & xty,
            matrixI & idx,
            const std::string & transport_method,
            double epsilon, int niter) 
{//updates xty with OT 
  bool y_sort = true;
  double dataWt = 1.0/double(N);
  matrix temp(P, N); // dim of X
  vector mass(S);
  
  mass.fill(1.0/double(S));
  xty.fill(0.0);
  
  suff_stat_arrange_Y(mu, Y,
                      S, P, N,
                      transport_method,
                      idx, mass,
                      y_sort, epsilon,
                      niter);
  
  int num_atom = idx.rows();
  vecMapConstI idx_map0(idx.col(0).data(), num_atom);
  vecMapConstI idx_map1(idx.col(1).data(), num_atom);
  int oldto = -1;
  
  for ( int i = 0; i < num_atom; i++ ) {
    // if(s % 100 == 0) Rcpp::checkUserInterrupt();
    int toidx = idx_map1(i);
    if( toidx > oldto) {//since can map to multiple atoms, only want to do this once
      temp = X.array().colwise() * theta.col(toidx).array();
      oldto = toidx;
    }
    
    xty.noalias() +=  temp * Y.col(idx_map0(i)) * dataWt * mass(i);
  }
  
}

void univariate_approximation_xty(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted
                                  const refMatConst & theta, const refMatConst & mu,
                                  const int S, const int N, const int P,
                                  matrix & xty, matrixI & idx_mu) {
  xty.fill(0.0);
  bool y_sort = true;
  matrix Y = sorted_Y;
  vector mass(S);
  matrix temp(P,N); //dim of X
  double dataWt = 1.0/double(S * N);
  std::string transp = "univariate.approximation.pwr";
  suff_stat_arrange_Y(mu, Y,
                      S, P, N,
                      transp,
                      idx_mu, mass,
                      y_sort, 0.0, 0);
  // Rcpp::Rcout << Y(0,0) << "\n";
  rel_sort_matrix_by_entry(Y, idx_mu.col(0));
  // Rcpp::Rcout << (idx_mu.col(0) - idx_mu.col(1)).sum() << "\n";
  // Rcpp::Rcout << Y(0,0) << "\n";
  // Rcpp::Rcout << (Y - mu).norm() << "\n";
  
  
  for ( int s = 0; s < S; s++ ) {
    // if(s % 100 == 0) Rcpp::checkUserInterrupt();
    
    temp = X.array().colwise() * theta.col(s).array();
    
    xty.noalias() +=  temp * Y.col(s) * dataWt;

  }
  
}

void xty_update_scale_ot(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted
                         const refMatConst & theta,
                         const refMatConst &mu,
                         const int S, const int N, const int P,
                         matrix & xty, matrixI & idx_mu,
                         const std::string & transport_method,
                         double epsilon, int niter) {
  
  if(transport_method == "univariate.approximation.pwr") {
    // void univariate_approximation_xty(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted
    //                                   const refMatConst & theta, refMatConst & mu,
    //                                   const int S, const int N, const int P,
    //                                   matrix & xty, matrixI & idx_mu)
    univariate_approximation_xty(X, sorted_Y,
                                 theta, mu,
                                 S, N, P,
                                 xty, idx_mu);
  } else {
    // void ot_xty(const refMatConst & X, const refMatConst & Y,
    //             const refMatConst & theta, const refMatConst & mu,
    //             const int S, const int N, const int P,
    //             matrix & xty,
    //              matrixI & idx,
    //             const std::string & transport_method) 
    ot_xty(X, sorted_Y, theta, mu, S, N, P, xty, idx_mu, transport_method, epsilon, niter);
  }
  
}


void xty_update(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted for method = scale/loc.scale
                const refMatConst & theta,
                const refMatConst & result,
                matrix & mu,
                const int S, const int N, const int P,
                matrix & xty, matrixI & idx_mu, 
                const Rcpp::CharacterVector & method,
                const std::string & transport_method,
                double epsilon, int niter){
  
  if( (method(0) == "scale") || (method(0) == "selection.variable") ) {
    mu_update(X, result, theta, mu, method);
    // void xty_update_scale_ot(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted
    //                          const refMatConst & theta,
    //                          const refMatConst &mu,
    //                          const int S, const int N, const int P,
    //                          matrix & xty, matrixI & idx_mu,
    //                          const std::string & transport_method)
    xty_update_scale_ot(X, sorted_Y, theta, mu, 
                        S, N, P, xty, idx_mu, transport_method,
                        epsilon, niter);
    // } else if (method(0) == "location.scale") {
    //   mu_update(X, result, theta, mu, method);
    //   xty_update_loc_scale(X, sorted_Y, theta,
    //                        theta_norm, result, mu, S, N, D, wt, sampleIdx, xty, idx_mu);
    // } else if (method(0) == "projection") {
    // mu_update(X, result, theta, mu, method); //only need if sorting!!
    // xty_update_projection(X, sorted_Y, theta, theta_norm, result, //result=theta_perp
    //                        mu, S, N, mix_wt, xty, idx_mu);
  } else if (method(0) == "projection") {
    //do nothing
  } else {
    Rcpp::stop("Method not found in update xty!");
  }
  
}

//[[Rcpp::export]]
Rcpp::List sufficientStatistics(const NumericMatrix & X_, const NumericMatrix & Y_, 
                                const NumericMatrix & theta_,
                                const Rcpp::List & options_)
{
  int S = theta_.rows();
  const int P = X_.cols();
  const int N = X_.rows();
  if (Y_.rows() != N) stop("Y does not have the same number of rows as X");
  matrix xtx(P,P);
  matrix xty(P,1);
  
  const bool same = Rcpp::as<bool>(options_["same"]);
  const CharacterVector method = Rcpp::as<Rcpp::CharacterVector>(options_["method"]);
  const std::string trans_meth = Rcpp::as<std::string>(options_["transport.method"]);
  double epsilon = Rcpp::as<double>(options_["epsilon"]); 
  int niter = Rcpp::as<int>(options_["niter"]);
  
  const bool not_same = (!same);
  
  const matMap Xt(Rcpp::as<matMap >( X_ ));
  matrix Y = Rcpp::as<matrix >( Y_ );
  const matMap thetat(Rcpp::as<matMap>( theta_ ));

  const matrix X = Xt.transpose();
  // matrix Y(S,N);
  // if (Y.rows() != S){
  //   Y = Yt.transpose();
  // } else {
  //   Y = Yt;
  // }
  
  
  matrix theta;
  if ( thetat.rows() != P ){
    theta = thetat.transpose();
  } else {
    theta = thetat;
    S = theta.cols();
  }
  if (Y_.cols() != S) stop("Y does not have the same number of columns as samples from theta");
  // matrix mu = Xt * theta;
  // vecMap Ytemp(Y.data(), Y.size());
  // vecMap mutemp(mu.data(), mu.size());
  // 
  // int count = 0;
  // for(int s = 0; s < S; s++){
  //   for(int n = 0; n < N; n++){
  //     int i = s*N + n;
  //     // if(idx.col(1)(i) != test(i)){
  //     //   Rcpp::Rcout << n <<", " << s <<": " << test(i) << ", " << idx.col(1)(i) << "\n";
  //     // }
  //     if(mutemp(i) != Ytemp(i)) {
  //       // Rcpp::Rcout << n <<", " << s <<": " << Ytemp(i) << ", " << mutemp(i) << "\n";
  //       count++;
  //     }
  //   }
  // }
  // Rcpp::Rcout << count <<", ";
  // Rcpp::Rcout << Y.isApprox(mu);
  // if(method(0) == NULL) method(0) = Rcpp::as<std::string>("scale");
  sufficient_stat(X, Y,
                  theta,
                  not_same,
                  S, P, N,
                  xtx, xty,
                  method,
                  trans_meth,
                  epsilon, niter);
  
  
  return Rcpp::List::create(Rcpp::Named("XtX") = Rcpp::wrap(xtx),
                            Rcpp::Named("XtY") = Rcpp::wrap(xty));
  
}

// [[Rcpp::export]]
matrix xtyUpdate(const NumericMatrix & X_, const NumericMatrix & Y_,
                 const NumericMatrix & theta_,
                 const NumericVector & result_,
                 const Rcpp::List & options_) 
{
  const int S = Y_.cols();
  const int P = X_.cols();
  const int N = X_.rows();
  
  const matMap Xt(as<matMap >( X_ ));
  const matMap Y(as<matMap >( Y_ ));
  const matMap thetat(as<matMap>( theta_ ));
  const matMap result(as<matMap>( result_ ));
  
  const CharacterVector method = Rcpp::as<std::string>(options_["method"]);
  const std::string transp = Rcpp::as<std::string>(options_["transport.method"]);
  double epsilon = Rcpp::as<double>(options_["epsilon"]);
  int niter = Rcpp::as<int>(options_["niter"]);
    
  const matrix X = Xt.transpose();
  matrix sort_Y = Y;
  matrix theta = thetat.transpose();
  matrix mu(S,N);
  
  if(theta.rows() != P){
    theta.transposeInPlace();
    if(theta.rows() != P) Rcpp::stop("theta must have same dimension as X");
  }
  if(theta.cols() != S) {
    Rcpp::stop("theta must have as many samples (S) as Y, where Y is NxS");
  }
  
  matrixI idx_mu(S,2);
  vector mass(S);
  matrix xty = matrix::Zero(P,1);
  
  if ( transp == "univariate.approximation.pwr" || transp == "hilbert" || transp == "rank" ) {
    arrange_Y(sort_Y, S, transp);
  }
  if (sort_Y.cols() != S) Rcpp::stop("error in arrange_Y");
  // if ( method(0) == "projection") {
  //   // theta.resize(P,S);
  //   theta_norm.resize(P,S);
  //   xty.resize(P,S);
  //   xty = X * Yt * (1.0 - pseudo_obs)/double(N);
  // } // fi projection
  if ( method(0) == "location.scale") {
    Rcpp::stop("location.scale not currently supported");
    // if (result.rows() != 2*P) {
    //   // if(result.rows() == P) {
    //   //   Rcpp::warning("Doubling input result vector to match method");
    //   //   vector temp_result = result;
    //   //   result.resize(2*P,1);
    //   //   result.block(0,0,P,1) = temp_result;
    //   //   result.block(P,0,P,1) = temp_result;
    //   // } else {
    //     Rcpp::stop("Result vector must have dimension twice that of theta");
    //   // }
    // }
    // matrix means(P,S);
    // 
    // {
    //   vector meanvec = theta.rowwise().mean();
    //   for (int s = 0; s < S ; s++) means.col(s) = meanvec;
    // }
    // 
    // matrix c_theta = theta - means;
    // 
    // theta.resize(2*P,S);
    // theta_norm.resize(2*P,1);
    // xty.resize(2*P,1);
    // 
    // theta.block(0,0,P,S) = c_theta;
    // theta.block(P,0,P,S) = means;
    
  } // fi loc.scale
  if (method(0) == "projection"){
    Rcpp::stop("No update needed for projection method");
  }
  // void xty_update(const refMatConst & X, const refMatConst & sorted_Y, //y needs to be pre-sorted for method = scale/loc.scale
  //                 const refMatConst & theta,
  //                 const refMatConst & result,
  //                 matrix & mu,
  //                 const int S, const int N, const int P,
  //                 matrix & xty, matrixI & idx_mu, 
  //                 const Rcpp::CharacterVector & method,
  //                 const std::string & transport_method)
  // Rcpp::Rcout << sort_Y.row(0)(0);
  xty_update(X, sort_Y, theta, result, mu, S,  N, P, xty, idx_mu, 
             method, transp, epsilon, niter);
  
  return( xty );
  
}
