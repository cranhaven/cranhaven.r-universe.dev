#include "trans_sinkhorn.h"

// algorithm 3 in Altschuler, J., Weed, J., & Rigollet, P. (2017). 
// Near-linear time approximation algorithms for optimal transport via Sinkhorn iteration. 
// 31st Conference on Neural Information Processing Systems, (1), 1–11. Long Beach, CA.
// void trans_sinkhorn(const refVecConst & mass_a, const refVecConst & mass_b, 
//                     const matrix & exp_cost, 
//                     matrix & A,
//                     double eta, double epsilon, int niterations) {
//   int N = mass_a.size();
//   int M = mass_b.size();
//   
//   
//   matrix A_0 = exp_cost;
//   A = exp_cost.array()/exp_cost.lpNorm<1>();
//   vector log_a = mass_a.array().log();
//   vector log_b = mass_b.array().log();
//   vector x = vector::Zero(N);
//   vector y = vector::Zero(M);
//   vector r = vector::Zero(N);
//   vector c = vector::Zero(M);
//   
//   for( int i = 0; i < niterations; i ++){
//     r = A.rowwise().sum();
//     c = A.colwise().sum();
//     
//     if ((i % 2) == 0) {
//       x.noalias() += log_a;
//       x.array() -= r.array().log();
//       
//     } else {
//       y.noalias() += log_b;
//       y.array() -= c.array().log();
//       
//     }
//     A = x.array().exp().matrix().asDiagonal() * A_0 * y.array().exp().matrix().asDiagonal();
//     // A /= A.sum();
//     if(dist_approx_ot(mass_a, mass_b, r, c, 2.0) <= epsilon) {
//       break;
//     }
//   }
// }


// void trans_sinkhorn(const refVecConst & mass_a, const refVecConst & mass_b,
//                     const matrix & exp_cost,
//                     matrix & A,
//                     double eta, double epsilon, int niterations) {
//   int N = mass_a.size();
//   int M = mass_b.size();
// 
// 
//   // matrix A_0 = exp_cost;
//   A = exp_cost.array()/exp_cost.lpNorm<1>();
//   vector log_a = mass_a.array().log();
//   vector log_b = mass_b.array().log();
//   // vector x = vector::Zero(N);
//   // vector y = vector::Zero(M);
//   vector r = A.rowwise().sum();
//   vector c = A.colwise().sum();
// 
//   for( int i = 0; i < niterations; i ++){
// 
//     if ((i % 2) == 0) {
//       vector x = log_a;
//       x.array() -= r.array().log();
// 
//       A = x.array().exp().matrix().asDiagonal() * A;
//       r = mass_a;
//       c = A.colwise().sum();
//       // Rcpp::Rcout << "Even: ";
//       // // Rcpp::Rcout << "x: " << x.transpose() << " ";
//       // Rcpp::Rcout << "Rsum: " << A.rowwise().sum().sum() << " ";
//       // Rcpp::Rcout << "Csum: " << A.colwise().sum().sum() << " ";
//     } else {
//       vector y = log_b;
//       y.array() -= c.array().log();
// 
//       A = A * y.array().exp().matrix().asDiagonal();
//       r = A.rowwise().sum();
//       c = mass_b;
//       // Rcpp::Rcout << "Odd: ";
//       // // Rcpp::Rcout << "y: " << y.transpose() << " ";
//       // Rcpp::Rcout << "Rsum: " << A.rowwise().sum().sum() << " ";
//       // Rcpp::Rcout << "Csum: " << A.colwise().sum().sum() << " ";
//     }
//     // A /= A.sum();
//     // r = A.rowwise().sum();
//     // c = A.colwise().sum();
//     // Rcpp::Rcout << "R: " << r.transpose() << " ";
//     // Rcpp::Rcout << "Csum: " << A.colwise().sum().sum() << " ";
//     // Rcpp::Rcout << "Asum " << A.sum() << "\n";
//     if(dist_approx_ot(mass_a, mass_b, r, c, 1.0) <= epsilon) {
//       // Rcpp::Rcout << epsilon << "\n";
//       // Rcpp::Rcout << dist_approx_ot(mass_a, mass_b, r, c, 2.0) << "\n";
//       break;
//     }
//   }
//   A /= A.sum();
// }


// avoid log scaling
void trans_sinkhorn(const refVecConst & mass_a, const refVecConst & mass_b,
                    const matrix & exp_cost,
                    matrix & A,
                    double eta, double epsilon, int niterations,
                    vector & f, vector & g) {
  int N = mass_a.size();
  int M = mass_b.size();

  // vector ones_n = vector::Ones(N);
  // vector ones_m = vector::Ones(M);

  vector u = vector::Ones(N); // first margins
  vector v = vector::Ones(M); // second margins
  
  vector u_old = u; // first margins
  // vector v_old = v; // second margins
  
  
  // matrix scaling
  for ( int i = 0; i < niterations; i ++){
    // column margins
    v = mass_b.cwiseQuotient(exp_cost.transpose() * u);
    
    // row margins
    u = mass_a.cwiseQuotient(exp_cost * v);
    
    // calc relative change in scaling vectors to see if approx converged
    if (i % 10) {
      if (sinkhorn_converge(u, u_old) <= epsilon) {
        break;
      } else {
        Rcpp::checkUserInterrupt();
      }
    }
    u_old = u;
    // v_old = v;
  }
  
  // get approximate assignment matrix
  A = u.asDiagonal() * exp_cost * v.asDiagonal();
  
  f = u.array().log() / eta;
  g = v.array().log() / eta;
}

void trans_sinkhorn_self(vector & f, const refVecConst & mass_a,
                         double eta,
                    const matrix & exp_cost,
                    double epsilon, int niterations) {
  int N = mass_a.size();

  vector ones_n = vector::Ones(N);
  vector u = ones_n;
  vector u_old = ones_n; // first margins

  
  // matrix scaling
  for ( int i = 0; i < niterations; i ++){
    // row margins
    u = mass_a.cwiseQuotient(exp_cost * u);
    
    // calc relative change in scaling vectors to see if approx converged
    if (i % 10) {
      if (sinkhorn_converge(u, u_old) <= epsilon) {
        break;
      }
    }
    u_old = u;
  }
  
  f = u.array().log()/eta;
  
}

//[[Rcpp::export]]
matrix generate_S(const matrix & cost, vector & f, vector& g, double eta) {
  matrix S = ((cost.colwise() - f).rowwise() - g.transpose()) * -eta;
  return(S);
}

matrix generate_S_star(const matrix & K, vector & f, vector& g) {
  matrix S = ((K.colwise() + f).rowwise() + g.transpose()) ;
  return(S);
}

//[[Rcpp::export]]
vector rowLogSumExp(matrix Mat) {
  
  vector max = Mat.rowwise().maxCoeff();
  // Rcpp::Rcout << "max: " << max << "\n";
  // Rcpp::Rcout << "sweep:\n" << (Mat.colwise() - max) <<"\n";
  vector sum = (Mat.colwise() - max).array().exp().rowwise().sum().log();
  // Rcpp::Rcout << "sum: " << sum << "\n";
  // Rcpp::Rcout << "out: " << max + sum << "\n";
  
  return max + sum;
  // return (Mat.array().exp().rowwise().sum().log());
}

//[[Rcpp::export]]
vector colLogSumExp(matrix Mat) {
  
  Eigen::RowVectorXd max = Mat.colwise().maxCoeff();
  // Rcpp::Rcout << "max: " << max << "\n";
  // Rcpp::Rcout << "sweep:\n" << (Mat.rowwise() - max) <<"\n";
  vector sum = (Mat.rowwise() - max).array().exp().colwise().sum().log();
  // Rcpp::Rcout << "out: " << max.transpose() + sum << "\n";
  return max.transpose() + sum;
  // return (Mat.array().exp().colwise().sum().log());
}

double LogSumExp(matrix Mat) {
  double max = Mat.maxCoeff();
  return std::log((Mat.array() - max).array().exp().sum()) + max;
}

//[[Rcpp::export]]
vector rowMin_eps(const matrix & cost, vector & f, vector& g, double eta) {
  // return (rowLogSumExp((cost.rowwise() - g.transpose()) * - eta).array()/(-eta));
  matrix S = generate_S(cost, f, g, eta);
  return(
    -(S.array().exp().rowwise().sum().log())/eta 
  );
}

//[[Rcpp::export]]
vector colMin_eps(const matrix & cost, vector & f, vector& g, double eta) {
  
  // return (colLogSumExp((cost.colwise() - f) * - eta).array()/(-eta));
  matrix S = generate_S(cost, f, g, eta);
  return(
    -(S.array().exp().colwise().sum().log())/eta 
  );
}

vector rowMin_eps_star(const matrix & K, vector & f, vector& g) {
  // return (rowLogSumExp((cost.rowwise() - g.transpose()) * - eta).array()/(-eta));
  matrix S = generate_S_star(K, f, g);
  return(
    -(S.array().exp().rowwise().sum().log())
  );
}

vector colMin_eps_star(const matrix & K, vector & f, vector& g) {
  
  // return (colLogSumExp((cost.colwise() - f) * - eta).array()/(-eta));
  matrix S = generate_S_star(K, f, g);
  return(
    -(S.array().exp().colwise().sum().log()) 
  );
}

//[[Rcpp::export]]
vector rowMin_eps_KL(const matrix & cost, vector & f, vector& g, double eta,
                     vector & log_a, vector & log_b) {
  // return (rowLogSumExp((cost.rowwise() - g.transpose()) * - eta).array()/(-eta));
  matrix S = generate_S(cost, f, g, eta);
  return(
    -((S.rowwise() + log_b.transpose()).array().exp().rowwise().sum().log())/eta 
  );
}

//[[Rcpp::export]]
vector colMin_eps_KL(const matrix & cost, vector & f, vector& g, double eta,
                     vector & log_a, vector & log_b) {
  
  // return (colLogSumExp((cost.colwise() - f) * - eta).array()/(-eta));
  matrix S = generate_S(cost, f, g, eta);
  return(
    -((S.colwise() + log_a).array().exp().colwise().sum().log())/eta 
  );
}

void trans_sinkhorn_log(const refVecConst & mass_a, const refVecConst & mass_b,
                    const matrix & cost,
                    matrix & Assign,
                    double eta, double epsilon, int niterations,
                    vector & f_pot, vector & g_pot) {
  int N = mass_a.size();
  int M = mass_b.size();
  
  vector log_a = mass_a.array().log();
  vector log_b = mass_b.array().log();
  
  // matrix K = -eta * cost.array();
  // vector A = -rowLogSumExp(K) + log_a; // first margins
  // vector B = -colLogSumExp(K.colwise() + A) + log_b; // second margins
  
  vector A = -rowLogSumExp(-eta * cost.array()).array()/eta + log_a.array()/eta; // first margins
  vector B = -colLogSumExp((-eta * (cost.colwise() - A).array())).array()/eta + log_b.array()/eta; // second margins
  
  // vector A = vector::Zero(N);
  // vector B = vector::Zero(M);
  
  vector A_old = vector::Zero(N); // first margins
  // vector B_old = vector::Zero(M); // second margins
  
  
  
  
  // Rcpp::Rcout << A << "\n";
  // Rcpp::Rcout << B << "\n";
  // matrix scaling
  for ( int i = 0; i < niterations; i ++) {
    
    // // row margins
    // A = -rowLogSumExp(K.rowwise() + B.transpose()) + log_a;
    // // A += (colMin_eps_star(K, A, B) + log_a).eval();
    //   
    // // column margins
    // B = -rowLogSumExp(K.colwise() + A) + log_b;
    // // B += (rowMin_eps_star(K, A, B) + log_b).eval();

    
    
    // // row margins
    // A += (rowMin_eps(cost, A, B, eta)  + log_a/eta).eval();
    // 
    // // column margins
    // B += (colMin_eps(cost, A, B, eta)  + log_b/eta).eval();
    
    // KL version of sinkhorn
    // row margins
    A += rowMin_eps_KL(cost, A, B, eta, log_a, log_b).eval();
    
    // column margins
    B += colMin_eps_KL(cost, A, B, eta, log_a, log_b).eval();

   
    // Rcpp::Rcout << i << ":\n ";
    // Rcpp::Rcout << "A: \n" << A.transpose() << "\n";
    // Rcpp::Rcout << "B: \n" << B.transpose() << "\n";
    // calc relative change in scaling vectors to see if approx converged
    // if (i % 10) {
      if (sinkhorn_converge_log(A, A_old) <= epsilon) {
        break;
      } else {
        Rcpp::checkUserInterrupt();
      }
    // }
    A_old = A;
    // B_old = B;
  }
  // f_pot = A / eta;
  // g_pot = B / eta;
  f_pot = A;
  g_pot = B;
  // Rcpp::Rcout << "epsilon: " << epsilon <<"\n";
  // Rcpp::Rcout << (mass_a.array() * f.array()).sum() +
  //   (mass_b.array() * g.array()).sum() << "\n";
  // Rcpp::Rcout << (mass_a.array() * (u.array().log() - u_pot.array().log()) / eta).sum() +
  //   (mass_b.array() * (v.array().log() - v_pot.array().log()) / eta).sum();
  // f = f - f_pot;
  // g = g - g_pot;
  
  // Rcpp::Rcout << "using biased potentials " << ((A.asDiagonal() * K.exp().matrix() * B.asDiagonal()).array() * cost.array()).sum() << "\n";
  
  // Rcpp::Rcout << (mass_a.array() * f.array()).sum() +
  //   (mass_b.array() * g.array()).sum() << "\n";
  
  // Rcpp::Rcout << "using biased potentials " << ((A.array().exp().matrix().asDiagonal() * K.array().exp().matrix() * B.array().exp().matrix().asDiagonal()).array() * cost.array()).sum() << "\n";
  
  // Rcpp::Rcout << "using biased potentials from paper " << ((K + ( A * vector::Ones(M).transpose() + vector::Ones(N) * B.transpose())).exp().array() * (mass_a * mass_b.transpose()).array() * cost.array()).sum() << "\n";
  
  // unbiased potentials if available
  // A = f * eta;
  // B = g * eta;
  
  // get approximate assignment matrix
  // Assign = ((K.colwise() + A).rowwise() + B.transpose()).array().exp();
  matrix S = generate_S(cost, f_pot, g_pot, eta);
  Assign = (S.array() - LogSumExp(S)).array().exp();
  // Rcpp::Rcout << "using unbiased potentials " << ((A.array().exp().matrix().asDiagonal() * K.array().exp().matrix() * B.array().exp().matrix().asDiagonal()).array() * cost.array()).sum() << "\n";
  // Rcpp::Rcout << "using Assign from paper" << (Assign.array() * cost.array()).sum() << "\n";
  
}

void trans_sinkhorn_autocorr_log(vector & f, const refVecConst & mass_a,
                             const matrix & cost,
                             double eta, double epsilon, int niterations) {
  int N = mass_a.size();
  
  vector log_a = mass_a.array().log();
  
  // matrix K = -eta * cost.array();
  // vector A = -rowLogSumExp(K) + log_a; // first margins
  // vector B = -colLogSumExp(K.colwise() + A) + log_b; // second margins
  
  f = -rowLogSumExp(-eta * cost.array()).array()/eta + log_a.array()/eta; // first margins
  
  // vector A = vector::Zero(N);
  // vector B = vector::Zero(M);
  
  vector f_old = vector::Zero(N); // second margins
  
  
  
  
  // Rcpp::Rcout << A << "\n";
  // Rcpp::Rcout << B << "\n";
  // matrix scaling
  for ( int i = 0; i < niterations; i ++) {
    
    
    // column margins
    f += colMin_eps_KL(cost, f, f, eta, log_a, log_a).eval();
    
    
    if (sinkhorn_converge_log(f, f_old) <= epsilon) {
      break;
    }
    // }
    f_old = f;
  }
  // f_pot = A / eta;
  // g_pot = B / eta;
}

// void trans_sinkhorn_autocorr(vector & u_pot, const refVecConst & mass_a,
//                     const matrix & exp_cost,
//                     double eta, double epsilon, int niterations) {
//   int N = mass_a.size();
// 
//   vector ones_n = vector::Ones(N);
// 
//   vector u = ones_n; // first margins
// 
//   vector u_old = ones_n; // first margins save old iteration
// 
//   matrix K = exp_cost * mass_a.asDiagonal();
// 
//   // matrix scaling
//   for ( int i = 0; i < niterations; i ++){
// 
//     // row margins
//     u = (u.array() / (K * u).array()).sqrt();
// 
//     // calc relative change in scaling vectors to see if approx converged
//     // if (i % 3) { // calc every 10 iterations to avoid log calculation every step
//       if (sinkhorn_converge(u, u_old) <= epsilon) {
//         break;
//       }
//     // }
// 
//     // save current iteration
//     u_old = u;
//   }
// 
//   // get potential
//   // f = u.array().log() / eta;
//   u_pot = u;
// }


//[[Rcpp::export]]
Rcpp::List sinkhorn_pot_(const vector & mass_a, const vector & mass_b, 
                         const matrix & cost_matrix, 
                         double epsilon, int niterations,
                         bool unbiased,
                         const matrix & cost_matrix_A, 
                         const matrix & cost_matrix_B) {
  double med_cost = median(cost_matrix);
  double eta = 1.0 / (epsilon * med_cost); //avoid underflow
  // double eta = 4 * log(double(mass_a.size())) / epsilon;
  const matrix exp_cost = (-eta * cost_matrix.array() ).exp();
  double epsilon_prime = epsilon / (8 * cost_matrix.maxCoeff());
  
  
  int N = mass_a.size();
  int M = mass_b.size();
  
  matrix assign_mat = matrix::Zero(N,M);
  
  vector f = vector::Ones(N);
  vector g = vector::Ones(M);
  
  
  if (unbiased) {
    const matrix exp_cost_a = (-eta * cost_matrix_A.array() ).exp();
    const matrix exp_cost_b = (-eta * cost_matrix_B.array() ).exp();
    vector p = vector::Ones(N);
    vector p_unused = vector::Ones(N);
    vector q = vector::Ones(M);
    vector q_unused = vector::Ones(M);
    matrix assign_mat_a = matrix::Zero(N,N);
    matrix assign_mat_b = matrix::Zero(M,M);
    
    trans_sinkhorn(mass_a, mass_a, exp_cost_a, assign_mat_a, eta, epsilon_prime/2.0, niterations,
                   p, p_unused);
    trans_sinkhorn(mass_b, mass_b, exp_cost_b, assign_mat_b, eta, epsilon_prime/2.0, niterations,
                   q_unused, q);
    trans_sinkhorn(mass_a, mass_b, exp_cost, assign_mat, eta, epsilon_prime/2.0, niterations,
                       f, g);
    // Rcpp::Rcout << f << "\n";
    // Rcpp::Rcout << g << "\n";
    // Rcpp::Rcout << p << "\n";
    // Rcpp::Rcout << q << "\n";
    f = f - p;
    g = g - q;
    
  } else {
    trans_sinkhorn(mass_a, mass_b, exp_cost, assign_mat, eta, epsilon_prime/2.0, niterations,
                   f, g);
  }
  
  return(Rcpp::List::create(Rcpp::Named("f") = Rcpp::wrap(f),
                            Rcpp::Named("g") = Rcpp::wrap(g)));
}


//[[Rcpp::export]]
Rcpp::List sinkhorn_pot_log_(const vector & mass_a, const vector & mass_b, 
                         const matrix & cost_matrix, 
                         double epsilon, int niterations,
                         bool unbiased,
                         const matrix & cost_matrix_A, 
                         const matrix & cost_matrix_B) {
  double med_cost = median(cost_matrix);
  double eta = 1.0 / (epsilon * med_cost); //avoid underflow
  // double eta = 4 * log(double(mass_a.size())) / epsilon;
  // const matrix exp_cost = (-eta * cost_matrix.array() ).exp();
  double epsilon_prime = 1e-8;
  
  
  int N = mass_a.size();
  int M = mass_b.size();
  
  matrix assign_mat = matrix::Zero(N,M);
  
  vector f = vector::Ones(N);
  vector g = vector::Ones(M);
  
  
  if (unbiased) {
    // const matrix exp_cost_a = (-eta * cost_matrix_A.array() ).exp();
    // const matrix exp_cost_b = (-eta * cost_matrix_B.array() ).exp();
    vector p = vector::Ones(N);
    vector p_unused = vector::Ones(N);
    vector q = vector::Ones(M);
    vector q_unused = vector::Ones(M);
    // matrix assign_mat_a = matrix::Zero(N,N);
    // matrix assign_mat_b = matrix::Zero(M,M);
    
    // vector & f, const refVecConst & mass_a,
    // const matrix & cost,
    // double eta, double epsilon, int niterations
    trans_sinkhorn_autocorr_log(p, mass_a, cost_matrix_A, eta, epsilon_prime, niterations);
    trans_sinkhorn_autocorr_log(q, mass_b, cost_matrix_B, eta, epsilon_prime, niterations);
    
    
    // const refVecConst & mass_a, const refVecConst & mass_b,
    // const matrix & cost,
    // matrix & Assign,
    // double eta, double epsilon, int niterations,
    // const refVecConst & f_pot, const refVecConst & g_pot
    trans_sinkhorn_log(mass_a, mass_b, cost_matrix, assign_mat, eta, epsilon_prime, niterations,
                   f, g);
    // Rcpp::Rcout << f << "\n";
    // Rcpp::Rcout << g << "\n";
    // Rcpp::Rcout << p << "\n";
    // Rcpp::Rcout << q << "\n";
    f = f - p;
    g = g - q;
    
  } else {
    trans_sinkhorn_log(mass_a, mass_b, cost_matrix, assign_mat, eta, epsilon_prime, niterations,
                       f, g);
  }
  
  return(Rcpp::List::create(Rcpp::Named("f") = Rcpp::wrap(f),
                            Rcpp::Named("g") = Rcpp::wrap(g)));
}
