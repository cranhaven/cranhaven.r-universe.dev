#include "transport.h"


// .C("shortsimplex",
// as.integer(control$para$slength), 
// as.integer(control$para$kfound), 
// as.double(control$para$psearched),
// as.integer(N), 
// as.integer(N), 
// as.integer(rep.int(1,N)), 
// as.integer(rep.int(1,N)),
// as.double(dd), 
// assignment = as.integer(initassig), 
//   basis = as.integer(initbasis),
// DUP=TRUE, PACKAGE="transport")

void adjustVal(vectorI & x, int N) {
  int sum_x = x.sum();
  
  if (sum_x < N) {
    int n_samp = N - sum_x;
    vectorI select(n_samp);
    vector weights(n_samp);
    weights.fill( 1.0/double(n_samp) );
    
    sample_systematic(select, weights, n_samp);
    
    for( int i = 0; i < select.size(); i ++) {
      x(select(i)) += 1;
    }
  } else {
    while (sum_x > N) {
      int n_samp = N - sum_x;
      vectorI select(n_samp);
      vector weights(n_samp);
      weights.fill( 1.0/double(n_samp) );
      
      sample_systematic(select, weights, n_samp);
      for( int i = 0; i < select.size(); i ++) {
        x(select(i)) -= 1;
      }
      sum_x = x.sum();
    }
  }
}

void intNormalizeMass(const refVecConst & a, const refVecConst & b, 
                      vectorI & mass_a, vectorI & mass_b, double & max_val) {
  //based on "fudge" function from transport pacakge
  double sum_a = a.sum();
  double sum_b = b.sum();
  // double max_val = 1e09;
  
  for(int n = 0; n < a.size(); n++) mass_a(n) = int( a(n)/sum_a * max_val );
  for(int m = 0; m < b.size(); m++) mass_b(m) = int( b(m)/sum_b * max_val );
  
  adjustVal(mass_a, int(max_val));
  adjustVal(mass_b, int(max_val));
  
}

// void (*shortsimplex_trans)(int*, int*, double*, int*, int*,
//       int*, int*, double*, int*,int*);
// 
// extern "C" void R_shortsimplex(DllInfo *dll) {
// shortsimplex_trans = (void (*) (int*, int*, double*, int*, int*,
//                       int*, int*, double*, int*,int*)) R_GetCCallable("transport", "shortsimplex");
// }

void transport_C(const refVecConst & mass_a, const refVecConst & mass_b, 
                 refMat cost_matrix, matrixI & idx, vector & mass, const std::string & method,
                 refMat cost_matrix_A, refMat cost_matrix_B,
                double epsilon, int niter, bool unbiased, int threads) {
  
  int N = mass_a.size();
  int M = mass_b.size();
  matrix assignment = matrix::Zero(N,M);
  
  // Rcpp::Rcout << mass.size() << std::endl;
  // if (mass.size() != (N*M)) {
  //   // Rcpp::Rcout << "resize mass\n";
  //   // Rcpp::Rcout << N*M<<std::endl;
  //   mass.resize(N*M);
  // } //Rcpp::stop("mass vector size not equal to nobs(A) * nobs(B)");
  if (idx.rows() != (N*M)) {
    idx.resize( (N*M), 2);
  } //Rcpp::stop("Index nrows not equal to size of nobs(A) * nobs(B)");
  // Rcpp::Rcout << mass.size() << std::endl;
  
  
  if (method == "shortsimplex" ) {
    double max_val = double(N);
    matrixI assign_mat = matrixI::Zero(N,M);
    matrixI basis_mat = matrixI::Zero(N,M);
    vectorI int_mass_a = vectorI::Ones(N);
    vectorI int_mass_b = vectorI::Ones(M);
    if (N != M || (mass_a.array() != 1.0/double(N)).all() ||  (mass_a.array() != 1.0/double(M)).all() ) {
      max_val = double(1e09);
      intNormalizeMass(mass_a, mass_b, int_mass_a, int_mass_b, max_val);
      // if(method == "univariate") {
      //   method = "shortsimplex";
      // }
    }
    
    trans_shortsimplex(int_mass_a, int_mass_b, cost_matrix, assign_mat, basis_mat);
    if (N != M ) {
      which(basis_mat, N, M, idx); // usage: which(const matrixI & basis, int N, int M, matrixI index )
    } else {
      which(assign_mat, N, M, idx);
    }
    double renorm = double(mass_a.sum())/max_val;
    assignment = assign_mat.cast<double>() * renorm;
  } else if (method == "networkflow" || method == "exact") {
    bool accuracy = false;
    trans_networkflow(mass_a, mass_b, cost_matrix, assignment, threads, accuracy, niter);
    which_nonzero(assignment, N, M, idx);
  } else if (method == "sinkhorn" || method == "sinkhorn_log" ||
    method == "greenkhorn" 
               // || method == "randkhorn" || method == "gandkhorn"
               ) {
    
    // void trans_approxOT(const refVecConst & mass_a, const refVecConst & mass_b, 
    //                     refMat cost_matrix, 
    //                     matrix & assign_mat,
    //                     double epsilon, int niterations,
    //                     const std::string & method);
    trans_approxOT(mass_a, mass_b, cost_matrix, assignment, 
                   epsilon, niter, unbiased, method,
                  cost_matrix_A, cost_matrix_B);
    which_nonzero(assignment, N, M, idx);
  // } else if (method == "randkhorn") {
  //   Rcpp::stop("transport method not found!");
  // } else if (method == "gandkhorn") {
  //   Rcpp::stop("transport method not found!");
  } else if (method == "hilbert") {
    Rcpp::stop("Hilbert method shouldn't rely on this function");
  } else if (method == "univariate") {
    Rcpp::stop("Univariate method shouldn't rely ont his function");
  } else {
    Rcpp::stop("transport method not found!");
  }
  
  if(mass.size() != idx.rows()){
    mass.resize(idx.rows());
  }
  
  for(int i = 0; i < idx.rows(); i ++) {
    mass(i) = assignment( idx(i,0), idx(i,1) );
  }
  
}

void transport(const matrix & A, const matrix & B, const double p, const double ground_p,
               matrixI & idx, vector & mass, const std::string & method, bool & a_sort,
               double epsilon, int niter, bool unbiased, int threads ) {
  
  int N = A.cols();
  int M = B.cols();
  bool univ = false;
  
  if ( method == "univariate" || ((A.rows() == 1) && (B.rows() == 1) && (N == M) ) ){
    univ = true;
  }
  if ( univ ) {
    if(A.rows() != 1 || B.rows() != 1) Rcpp::stop("rows of A and B must be 1 for univariate");
    
    vecMapConst avec(A.data(),N);
    vecMapConst bvec(B.data(),M);
    trans_univariate(avec, bvec, N, M, idx, mass, a_sort);
    
  } else if ( method == "hilbert" ) {
    trans_hilbert(A, B, N, M, idx, mass, a_sort);
  } else if ( method == "rank") {
    trans_rank(A, B, N, M, idx, mass, a_sort);
  } else if ( method == "univariate.approximation.pwr") {
    // void  trans_univariate_approx_pwr(const matrix & A, const matrix & B, int N, int M,
    //                                   matrixI & idx, vector & mass, bool & a_sort)
    trans_univariate_approx_pwr(A, B, N, M, idx, mass, a_sort);
  } else if (method == "swapping") {
    trans_hilbert(A, B, N, M, idx, mass, a_sort);
    trans_swap(A, B, N, M,
               idx, mass, ground_p,
               p, epsilon, niter);
  } else {
    
    matrix cost_matrix(N,M);
    
    vector mass_a(N);
    vector mass_b(M);
    
    mass_a.fill(1.0/double(N));
    mass_b.fill(1.0/double(M));
    
    //cost matrix calculation for non-univariate measures
    if (ground_p == 2.0) {
      cost_calculation_L2(A, B, cost_matrix);
    } else if (ground_p == 1.0){
      cost_calculation_L1(A, B, cost_matrix);
    } else {
      cost_calculation_Lp(A, B, cost_matrix, ground_p);
    }
    
    matrix cost_matrix_A;
    matrix cost_matrix_B;
    
    if (unbiased) {
      cost_matrix_A = matrix::Zero(N,N);
      cost_matrix_B = matrix::Zero(M,M);
      if (ground_p == 2.0) {
        cost_calculation_L2(B, B, cost_matrix_A);
        cost_calculation_L2(B, B, cost_matrix_A);
      } else if (ground_p == 1.0){
        cost_calculation_L1(B, B, cost_matrix_A);
        cost_calculation_L1(B, B, cost_matrix_A);
      } else {
        cost_calculation_Lp(B, B, cost_matrix_A, ground_p);
        cost_calculation_Lp(B, B, cost_matrix_A, ground_p);
      }
    } else {
      cost_matrix_A = matrix::Zero(0,0);
      cost_matrix_B = matrix::Zero(0,0);
    }
    
    cost_matrix.array() = cost_matrix.array().pow(p).eval();
    
    transport_C(mass_a, mass_b, 
                cost_matrix, idx, mass, 
                method, cost_matrix_A, cost_matrix_B,
                epsilon, niter, 
                unbiased, threads
                );
  }
  
}

//[[Rcpp::export]]
Rcpp::List transport_C_(const Rcpp::NumericVector & mass_a_, const Rcpp::NumericVector & mass_b_, 
                        const Rcpp::NumericMatrix & cost_matrix_,
                        const Rcpp::CharacterVector & method_,
                        double epsilon_,
                        int niter_,
                        bool unbiased_,
                        int threads_,
                        const Rcpp::NumericMatrix &  cost_matrix_A_, 
                        const Rcpp::NumericMatrix &  cost_matrix_B_) {
  
  const vecMap mass_a( Rcpp::as< vecMap >(mass_a_) );
  const vecMap mass_b( Rcpp::as< vecMap >(mass_b_) );
  const matMap cost_matrix(Rcpp::as< matMap> (cost_matrix_));
  const matMap cost_matrix_A(Rcpp::as< matMap> (cost_matrix_A_));
  const matMap cost_matrix_B(Rcpp::as< matMap> (cost_matrix_B_));
  
  std::string method(Rcpp::as<std::string>(method_(0)));
  
  int N = mass_a.size();
  int M = mass_b.size();
  
  matrixI idx(N*M,2);
  vector mass(N);
  
  transport_C(mass_a, mass_b, cost_matrix, idx, mass, method, 
              cost_matrix_A, cost_matrix_B,
              epsilon_, niter_, unbiased_, threads_);
  
  for(int i = 0; i < idx.size(); i++) idx(i) += 1;
  
  return  Rcpp::List::create(Rcpp::Named("from")  = Rcpp::wrap(idx.col(0)),
                             Rcpp::Named("to")    = Rcpp::wrap(idx.col(1)),
                             Rcpp::Named("mass")  = Rcpp::wrap(mass));
}


//[[Rcpp::export]]
Rcpp::List transport_(const Rcpp::NumericMatrix & A_, 
                      const Rcpp::NumericMatrix & B_, double p, double ground_p,
                      const Rcpp::CharacterVector & method_,
                      bool a_sort, double epsilon_ = 0.0, int niter_ = 0,
                      bool unbiased_ = false,
                      int threads_ = 1) {
  int N = A_.cols();
  int M = B_.cols();
  
  const matMap A(Rcpp::as<matMap>(A_));
  const matMap B(Rcpp::as<matMap>(B_));
  
  const std::string method(Rcpp::as<std::string>(method_(0)));
  
  matrixI idx(N * M, 2);
  vector mass(N * M);
  
  transport(A, B, p, ground_p,
            idx, mass, method, a_sort, epsilon_, niter_, 
            unbiased_,
            threads_);
  
  
  for(int i = 0; i < idx.size(); i++) idx(i) += 1;
  
  return  Rcpp::List::create(Rcpp::Named("from")  = Rcpp::wrap(idx.col(0)),
                             Rcpp::Named("to")    = Rcpp::wrap(idx.col(1)),
                             Rcpp::Named("mass")  = Rcpp::wrap(mass));
  
}

//[[Rcpp::export]]
Rcpp::List transport_swap_(const Rcpp::NumericMatrix & A_,
                      const Rcpp::NumericMatrix & B_,
                      matrixI & idx_,
                      vector & mass_,
                      double p, double ground_p,
                      double tolerance_, int niter_ = 0) {
  int N = A_.cols();
  int M = B_.cols();

  const matMap A(Rcpp::as<matMap>(A_));
  const matMap B(Rcpp::as<matMap>(B_));


  matrixI idx = idx_;
  vector mass = mass_;
  trans_swap(A, B, N, M,
             idx, mass, ground_p,
             p, tolerance_, niter_);

  for(int i = 0; i < idx.size(); i++) idx(i) += 1;

  return  Rcpp::List::create(Rcpp::Named("from")  = Rcpp::wrap(idx.col(0)),
                             Rcpp::Named("to")    = Rcpp::wrap(idx.col(1)),
                             Rcpp::Named("mass")  = Rcpp::wrap(mass));

}

