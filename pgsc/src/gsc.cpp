#include <RcppArmadillo.h>
#include <math.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;
using namespace arma ;

// [[Rcpp::export]]
arma::mat W_extract( arma::mat wt, int N){
// Extracts the matrix W from wt
  mat W = zeros(N,N) ;                      // The matrix of weights used in the calculation
  for( int i = 0 ; i < N ; i++ ){           // Row index of W
    int counter = 0 ;                       // Counter for column index of wt
    for( int j = 0 ; j < N ; j++ ){         // Column index of W
      if( i != j ){                         // Only work on the non-diagonal
        if( counter < N-2 ){                // Pre-last entry of wt in the row
          W(i,j) = wt(i,counter) ;          // Fill in the weight
          counter ++ ;                      // Increment counter
        }else{                              // Last entry of wt in the row
          W(i,j) = 1 - sum(wt.row(i)) ;
          // Penalty function for exceeding a sum of unity
        }
      }
    }
  }
  return W ;
}

// [[Rcpp::export]]
double gsc_target( int N, int T, arma::mat wt, arma::mat Y, 
                   arma::cube D, arma::vec b, arma::vec sig_i,
                   int print_level=0, double pen_gain=1e05,
                   bool return_n_obs=false ) {
  // The objective function for minimization.  
  
  // Inputs:
  //  - wt: Matrix of candidate weights. Should be Nx(N-2) as no weight for i=j or i=N
  //  - Y:  Matrix of target data.  Should be NxT.
  //  - D:  Array of treatments.  Should be NxMxT.
  //  - b:  Vector of coefficients.  Should be Mx1.
  //  - sig_i: Vector of weights for fit.  Should be Nx1.
  
  // Output:
  // out = .5 * sum_{n,t} ( Y[n,t] - D[n,,t] * b - W * ( Y[,t] - D[,,t] * b ) )
  // Where W is the matrix of weights expanded to include zeores on the diagonal and 
  
  
  double out = 0 ;
  double trial = 0 ;
  double penalty = 0 ;
  int M = b.n_elem ;                        // Number of treatments
  
  /** Create weighting matrix **/
  mat W = zeros(N,N) ;                      // The matrix of weights used in the calculation
  for( int i = 0 ; i < N ; i++ ){           // Row index of W
    int counter = 0 ;                       // Counter for column index of wt
    for( int j = 0 ; j < N ; j++ ){         // Column index of W
      if( i != j ){                         // Only work on the non-diagonal
        if( counter < N-2 ){                // Pre-last entry of wt in the row
          W(i,j) = wt(i,counter) ;          // Fill in the weight
          counter ++ ;                      // Increment counter
        }else{                              // Last entry of wt in the row
          W(i,j) = 1 - sum(wt.row(i)) ;
          penalty += .5 * pen_gain * pow( std::min( 0.0, W(i,j) ), 2 ) ;
          // Penalty function for exceeding a sum of unity
        }
      }
    }
  }
  
  /** Create the errors matrix **/
  mat eps=zeros(N,T) ;
  mat keep=ones(N,T) ;
  mat this_D = zeros( N, M ) ;
  for( int t=0; t < T ; t++ ){
    this_D = D.slice(t) ; 
    for( int i=0; i < N ; i++ ){
      trial = Y(i,t) - dot( this_D.row(i), b ) ;
      if( std::isnan(trial) ){
        eps(i,t) = 0 ;
        keep(i,t) = 0 ;
      }else{
        eps(i,t) = trial ;
      }
    }
  }
  double n_obs = accu(keep) ;
  // The number of observations
  if(return_n_obs){
    return n_obs ;
  }
  
  /** Create output **/
  mat m_sig_i_inv = ones(N,T) ; 
  for( int t=0 ; t < T ; t++ ){
    m_sig_i_inv.col(t) = 1/ sig_i ;
  }
  out = .5 * accu( m_sig_i_inv % pow( keep % ( eps - W * eps ), 2 ) ) / n_obs ;
  
  if( print_level > 0 ){
    Rcout << "W:\n" << W << std::endl ;
    Rcout << "keep\n" << keep << std::endl ;
    Rcout << "penalty = " << penalty << std::endl ;
  }
  
  return out + penalty ; // / (N*T) ;
}

// [[Rcpp::export]]
arma::vec gsc_target_grad_num( int N, int T, arma::mat wt, arma::mat Y, 
                               arma::cube D, arma::vec b, arma::vec sig_i, double inc=1e-06, 
                               double pen_gain=1e05 ){
  // Numerical gradient of GSC target
  
  int M = b.n_elem ; 
  vec out = zeros(N*(N-2)+M) ;
  
  for( int i = 0 ; i < N*(N-2) ; i ++ ){
    mat wt_up = wt ;
    wt_up(i) += inc ;
    mat wt_down = wt ;
    wt_down(i) -= inc ;
    out(i) = ( gsc_target( N, T, wt_up, Y, D, b, sig_i, 0, pen_gain ) - 
      gsc_target( N, T, wt_down, Y, D, b, sig_i, 0, pen_gain ) ) / 
      ( 2 * inc ) ;
  }
  
  for( int i = 0 ; i < M ; i ++ ){
    vec b_up = b ;
    b_up(i) += inc ;
    vec b_down = b ;
    b_down(i) -= inc ;
    out(N*(N-2)+i) = ( gsc_target( N, T, wt, Y, D, b_up, sig_i, 0, pen_gain ) - 
      gsc_target( N, T, wt, Y, D, b_down, sig_i, 0, pen_gain ) ) / ( 2 * inc ) ;
  }
  return out ;
}

// [[Rcpp::export]]
arma::vec gsc_target_grad( int N, int T, arma::mat wt, arma::mat Y, 
                           arma::cube D, arma::vec b, arma::vec sig_i, 
                           double pen_gain=1e05){
  // Analytical gradient of GSC target
  
  int M = b.n_elem ;                        // Number of treatments
  vec out = zeros(N*(N-2)+M) ;
  vec penalty = zeros(N*(N-2)+M) ;
  double trial = 0 ;
  
  /** Create weighting matrix and penalty gradient **/
  mat W = zeros(N,N) ;                      // The matrix of weights used in the calculation
  for( int i = 0 ; i < N ; i++ ){           // Row index of W
    int counter = 0 ;                       // Counter for column index of wt
    for( int j = 0 ; j < N ; j++ ){         // Column index of W
      if( i != j ){                         // Only work on the non-diagonal
        if( counter < N-2 ){                // Pre-last entry of wt in the row
          W(i,j) = wt(i,counter) ;          // Fill in the weight
          counter ++ ;                      // Increment counter
        }else{                              // Last entry of wt in the row
          W(i,j) = 1 - sum(wt.row(i)) ;
          for( int k=0 ; k < N-2 ; k++){
            // Rcout << "k = " << k << std::endl ;
            penalty(N*k+i) += - pen_gain * std::min( 0.0, W(i,j) ) ;
            // Penalty function for exceeding a sum of unity
          }
        }
      }
    }
  }
  
  /** Create the errors matrix **/
  mat eps=zeros(N,T) ;
  mat keep=ones(N,T) ;
  mat this_D = zeros( N, M ) ;
  for( int t=0; t < T ; t++ ){
    this_D = D.slice(t) ; 
    for( int i=0; i < N ; i++ ){
      trial = Y(i,t) - dot( this_D.row(i), b ) ;
      if( std::isnan(trial) ){
        eps(i,t) = 0 ;
        keep(i,t) = 0 ;
      }else{
        eps(i,t) = trial ;
      }
    }
  }
  double n_obs = accu(keep) ;
  // The number of observations
  
  
  /** Create gradient from weights **/
  double common = 0 ;                       // The common part of the derivative
  double sum_d = 0 ;                        // The part of the derivative coming from sum(wt)=1
  int counter = 0 ;                         // A counter
  for( int t = 0 ; t < T ; t++ ){
    this_D = D.slice(t) ;                   // Cross-sectional treatments at time t
    for( int i = 0 ; i < N ; i++ ){
      // common = 1.0 / (N*T*sig_i(i)) * ( Y(i,t) - dot( this_D.row(i), b ) - 
      //                         dot( W.row(i), Y.col(t) - this_D * b ) ) ;
      common = 1.0 / (sig_i(i)) * keep(i,t) * ( eps(i,t) - dot( W.row(i), eps.col(t) ) ) ;
      // The common multiplier
      if(i<N-1){
        sum_d = keep(N-1,t) * eps(N-1,t) ; // Y(N-1,t) - dot( this_D.row(N-1), b ) ;
      }else{
        sum_d = keep(N-2,t) * eps(N-2,t) ; // Y(N-2,t) - dot( this_D.row(N-2), b ) ;
      }
      counter = 0 ;
      for( int j = 0 ; j < N-2 ; j++ ){     // Contribution of j-th column of W
        if( j == i ) counter ++ ;
        // Need to skip an entry if i==j
        // out(N*j+i) += - common * ( Y(counter,t) - dot( this_D.row(counter), b ) - sum_d ) ;
        out(N*j+i) += - common * ( eps(counter,t) - sum_d ) ;
        counter ++ ;
      }
      
      /** To add: Figuring out what is getting zeroed out in this part of the gradient  **/
      vec this_D_m = zeros(N) ;
      // Container for column of D values. Used for replacing NaNs with zero.
      for( int m = 0 ; m < M ; m++ ){
        // if(keep(i,t)==1){
        this_D_m = this_D.col(m) ;
        this_D_m.elem(find(keep.col(t)==0)).fill(0.0) ;
        // }
        //   if( this_D_m.has_nan() ){
        //     Rcout << this_D_m << std::endl ;
        //     this_D_m.elem( find(this_D_m != this_D_m)).fill(0.0) ;
        //     Rcout << this_D_m << std::endl ;
        //   }
        // if(!this_D_m.has_nan()){
        out(N*(N-2)+m) += common * ( - this_D_m(i) + dot( this_D_m % keep.col(t), W.row(i) ) ) ;
        // NB: keep(i,t) is already in common
        // }
      }
    }
  }
  return out / n_obs + penalty ; // / (N*T) ;
}

// [[Rcpp::export]]
arma::vec gsc_target_grad_b( int N, int T, arma::mat wt, arma::mat Y, 
                             arma::cube D, arma::vec b, arma::vec sig_i ){
  // Analytical gradient of GSC target w.r.t b
  
  int M = b.n_elem ;                        // Number of treatments
  vec out = zeros(M) ;
  double trial = 0 ;
  
  /** Create weighting matrix and penalty gradient **/
  mat W = zeros(N,N) ;                      // The matrix of weights used in the calculation
  for( int i = 0 ; i < N ; i++ ){           // Row index of W
    int counter = 0 ;                       // Counter for column index of wt
    for( int j = 0 ; j < N ; j++ ){         // Column index of W
      if( i != j ){                         // Only work on the non-diagonal
        if( counter < N-2 ){                // Pre-last entry of wt in the row
          W(i,j) = wt(i,counter) ;          // Fill in the weight
          counter ++ ;                      // Increment counter
        }else{                              // Last entry of wt in the row
          W(i,j) = 1 - sum(wt.row(i)) ;
        }
      }
    }
  }
  
  /** Create the errors matrix **/
  mat eps=zeros(N,T) ;
  mat keep=ones(N,T) ;
  mat this_D = zeros( N, M ) ;
  for( int t=0; t < T ; t++ ){
    this_D = D.slice(t) ; 
    for( int i=0; i < N ; i++ ){
      trial = Y(i,t) - dot( this_D.row(i), b ) ;
      if( std::isnan(trial) ){
        eps(i,t) = 0 ;
        keep(i,t) = 0 ;
      }else{
        eps(i,t) = trial ;
      }
    }
  }
  double n_obs = accu(keep) ;
  // The number of observations
  
  
  /** Create gradient from weights **/
  double common = 0 ;                       // The common part of the derivative
  // double sum_d = 0 ;                        // The part of the derivative coming from sum(wt)=1
  // int counter = 0 ;                         // A counter
  for( int t = 0 ; t < T ; t++ ){
    this_D = D.slice(t) ;                   // Cross-sectional treatments at time t
    for( int i = 0 ; i < N ; i++ ){
      // common = 1.0 / (N*T*sig_i(i)) * ( Y(i,t) - dot( this_D.row(i), b ) - 
      //                         dot( W.row(i), Y.col(t) - this_D * b ) ) ;
      common = 1.0 / (sig_i(i)) * keep(i,t) * ( eps(i,t) - dot( W.row(i), eps.col(t) ) ) ;
      // The common multiplier
      
      /** To add: Figuring out what is getting zeroed out in this part of the gradient  **/
      vec this_D_m = zeros(N) ;
      // Container for column of D values. Used for replacing NaNs with zero.
      for( int m = 0 ; m < M ; m++ ){
        // if(keep(i,t)==1){
        this_D_m = this_D.col(m) ;
        this_D_m.elem(find(keep.col(t)==0)).fill(0.0) ;
        out(m) += common * ( - this_D_m(i) + dot( this_D_m % keep.col(t), W.row(i) ) ) ;
            // NB: keep(i,t) is already in common
      }
    }
  }
  return out / n_obs ; // / (N*T) ;
}

// [[Rcpp::export]]
arma::vec gsc_target_i( int N, int T, arma::mat wt, arma::mat Y, 
                  arma::cube D, arma::mat b ) {
  // Computes the target function for each unit
  
  // vec out = zeros(N) ;
  int M = b.n_elem ;                        // Number of treatments
  
  /** Create weighting matrix **/
  mat W = W_extract( wt, N) ;
  
  /** Create the errors matrix **/
  mat eps=zeros(N,T) ;
  mat keep=ones(N,T) ;
  mat this_D = zeros( N, M ) ;
  double trial = 0 ;
  for( int t=0; t < T ; t++ ){
    this_D = D.slice(t) ; 
    for( int i=0; i < N ; i++ ){
      trial = Y(i,t) - dot( this_D.row(i), b.col(i) ) ;
      if( std::isnan(trial) ){
        eps(i,t) = 0 ;
        keep(i,t) = 0 ;
      }else{
        eps(i,t) = trial ;
      }
    }
  }
  
  /** Create output **/
  vec n_obs = sum(keep,1) ;
  // The number of observations
  mat err = .5 * pow( keep % ( eps - W * eps ), 2 )  ;
  vec out = sum( err, 1 ) / n_obs ;
  
  return out ;
}
