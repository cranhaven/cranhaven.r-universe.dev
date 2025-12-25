#ifndef BINOMIAL_SOLVER_H
#define BINOMIAL_SOLVER_H

#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <vector>
#include <string>

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

#include "Solver.h"
#include "GaussianSolver.h"
#include "SolverTypes.h"

namespace {
inline double xlogx(const double x) {
  return log(x) * x;
}
  
  double sigmoid_scalar(const double z) {
    /*if (std::fabs(z) > 9) {
     return z < 0 ? 0 : 1;
     }*/
    // https://timvieira.github.io/blog/post/2014/02/11/exp-normalize-trick
    if (z >= 0) {
      return 1 / (1 + std::exp(-z));
    } else {
      const double exp_z = std::exp(z);
      return exp_z / (1 + exp_z);
    }
  }
  
  VecXd sigmoid(const VecXd& z) {
    return z.unaryExpr(std::ref(sigmoid_scalar));
  }
  
  double log_one_plus_exp_scalar(const double z) {
    // http://sachinashanbhag.blogspot.com/2014/05/numerically-approximation-of-log-1-expy.html
    if (z > 35) {
      return z;
    } else if (z > -10) {
      return std::log1p(std::exp(z));
    } else {
      return std::exp(z);
    }
  }
  
  VecXd log_one_plus_exp(const VecXd& z) {
    return z.unaryExpr(std::ref(log_one_plus_exp_scalar));
  }  
}

template <typename TG>
class BinomialSolver : public Solver<TG> {
  
private:
  using Solver<TG>::n;
  using Solver<TG>::p;
  using Solver<TG>::G;
  using Solver<TG>::E;
  using Solver<TG>::Y;
  using Solver<TG>::C;
  using Solver<TG>::weights_user;
  using Solver<TG>::normalize;
  
  using Solver<TG>::normalize_weights_g;
  using Solver<TG>::normalize_weights_e;
  
  using Solver<TG>::b_0;
  using Solver<TG>::b_e;
  using Solver<TG>::b_g;
  using Solver<TG>::b_c;
  using Solver<TG>::b_gxe;
  using Solver<TG>::delta;
  
  using Solver<TG>::weights;
  using Solver<TG>::xbeta;
  using Solver<TG>::Z_w;
  
  using Solver<TG>::safe_set_g;
  using Solver<TG>::safe_set_gxe;
  using Solver<TG>::safe_set_zero;
  using Solver<TG>::working_set;
  
  using Solver<TG>::abs_nu_by_G_uptodate;
  
  using Solver<TG>::norm_G;
  using Solver<TG>::norm_GxE;
  
  using Solver<TG>::active_set;
  
  using Solver<TG>::temp_p;
  using Solver<TG>::temp_n;
  
  using Solver<TG>::update_b_for_working_set;
  using Solver<TG>::update_weighted_variables;
  
protected:
  double primal_objective;
  VecXd abs_nu_by_G;
  VecXd abs_nu_by_GxE;
  double x_opt;
  VecXd upperbound_nu_by_G;
  VecXd upperbound_nu_by_GxE;
  
  VecXd abs_inner_nu_by_G;
  VecXd abs_inner_nu_by_GxE;
  
  VecXd nu;
  VecXd inner_nu;
  
  MatXd X_1_E_C, X_1_E_C_w_t;  
  VecXd mu, minus_grad, b_diff;
  MatXd H;
  
  public: BinomialSolver(const MapMat& G_,
                         const Eigen::Map<Eigen::VectorXd>& E_,
                         const Eigen::Map<Eigen::VectorXd>& Y_,
                         const Eigen::Map<Eigen::MatrixXd>& C_,
                         const Eigen::Map<Eigen::VectorXd>& weights_,
                         bool normalize_) :
    Solver<TG>(G_, E_, Y_, C_, weights_, normalize_),
    abs_nu_by_G(p),
    abs_nu_by_GxE(p),
    upperbound_nu_by_G(p),
    upperbound_nu_by_GxE(p),
    abs_inner_nu_by_G(p),
    abs_inner_nu_by_GxE(p),
    nu(n),
    inner_nu(n),
    X_1_E_C(G_.rows(), 2 + C_.cols()),
    X_1_E_C_w_t(2 + C_.cols(), G_.rows()),
    mu(n),
    minus_grad(2 + C_.cols()), 
    b_diff(2 + C_.cols()),
    H(2 + C_.cols(), 2 + C_.cols()) {
      init();
    }
    
    BinomialSolver(const MapSparseMat& G_,
                   const Eigen::Map<Eigen::VectorXd>& E_,
                   const Eigen::Map<Eigen::VectorXd>& Y_,
                   const Eigen::Map<Eigen::MatrixXd>& C_,
                   const Eigen::Map<Eigen::VectorXd>& weights_,
                   bool normalize_) :
    Solver<TG>(G_, E_, Y_, C_, weights_, normalize_),
    abs_nu_by_G(p),
    abs_nu_by_GxE(p),
    upperbound_nu_by_G(p),
    upperbound_nu_by_GxE(p),
    abs_inner_nu_by_G(p),
    abs_inner_nu_by_GxE(p),
    nu(n),
    inner_nu(n),
    X_1_E_C(G_.rows(), 2 + C_.cols()),
    X_1_E_C_w_t(2 + C_.cols(), G_.rows()),
    mu(n),
    minus_grad(2 + C_.cols()), 
    b_diff(2 + C_.cols()),
    H(2 + C_.cols(), 2 + C_.cols()) {
      init();
    }
    
    virtual ~BinomialSolver() {}
    
    void init() {
      MatXd X_1_E_C_w(n, 2 + C.cols());
      X_1_E_C.col(0).setOnes();
      X_1_E_C_w.col(0) = weights_user;
      X_1_E_C.col(1) = E * normalize_weights_e;
      X_1_E_C_w.col(1) = E.cwiseProduct(weights_user) * normalize_weights_e;
      for (int i = 0; i < C.cols(); ++i) {
        X_1_E_C.col(i + 2) = C.col(i);
        X_1_E_C_w.col(i + 2) = C.col(i).cwiseProduct(weights_user);
      }
      X_1_E_C_w_t = X_1_E_C_w.transpose();
    }
    
    int solve(double lambda_1, double lambda_2, double tolerance, int max_iterations, int min_working_set_size) {
      safe_set_g.setOnes(p);
      safe_set_gxe.setOnes(p);
      safe_set_zero.setOnes(p);
      
      int num_passes = 0;
      int working_set_size = 0;
      for (int i = 0; i < p; ++i) {
        working_set_size += int(b_g[i] != 0 || b_gxe[i] != 0);
      }
      if (working_set_size == 0) {
        working_set_size = min_working_set_size;
      }
      working_set.resize(0);
      
      double duality_gap, inner_duality_gap, max_diff_tolerance, max_diff;
      while (num_passes < max_iterations) {
        if (!abs_nu_by_G_uptodate) {
          num_passes = update_intercept(num_passes, max_iterations, nu);
        }
        duality_gap = check_duality_gap(lambda_1, lambda_2);
        if (duality_gap < tolerance) {
          break;
        }

        update_working_set(lambda_1, lambda_2, duality_gap, working_set_size);
        working_set_size = std::min(2 * working_set_size, p);

        active_set.setZero(p);
        max_diff_tolerance = tolerance; //* 4;
        int num_updates_b_for_working_set = 0;
        bool is_first_iteration = true;
        while (num_passes < max_iterations) {
          num_passes = update_intercept(num_passes, max_iterations, inner_nu);
          inner_duality_gap = check_inner_duality_gap(lambda_1, lambda_2);
          if (inner_duality_gap < tolerance) {
            break;
          } else {
            if (!is_first_iteration && num_updates_b_for_working_set <= 1) {
             // max_diff_tolerance = std::max(max_diff_tolerance / 4, tolerance / 4);
              max_diff_tolerance = max_diff_tolerance / 4;
            }
            update_quadratic_approximation();
            update_weighted_variables(true);

            num_updates_b_for_working_set = 0;
            is_first_iteration = false;
          }
          
          while (num_passes < max_iterations) {
            max_diff = update_b_for_working_set(lambda_1, lambda_2, false);
            ++num_passes;
            ++num_updates_b_for_working_set;
            
            if (max_diff < max_diff_tolerance) {
              break;
            }
            while (num_passes < max_iterations && max_diff >= max_diff_tolerance) {
              max_diff = update_b_for_working_set(lambda_1, lambda_2, true);
              ++num_passes;
            }
          }
        }
      }
      return num_passes;
    }
    
    void update_quadratic_approximation() {
      temp_n = sigmoid(xbeta); // probabilities
      weights.array() = temp_n.array() * (1 - temp_n.array()) * weights_user.array();
      /*for (int i = 0; i < n; ++i) {
       if (weights[i] != 0 && std::fabs(weights[i]) < 1e-5) {
       weights[i] = 1e-5;
       }
      }*/
      Z_w = xbeta.cwiseProduct(weights) + (Y - temp_n).cwiseProduct(weights_user);
    }
    
    double compute_dual_objective(const VecXd& nu) {
      double result = 0;
      temp_n = Y - nu;
      for (int i = 0; i < n; ++i) {
        // temp_n should be between 0 and 1 as long as the nu = Y - sigmoid(xbeta)
        if (weights_user[i] != 0 && temp_n[i] > 0 && temp_n[i] < 1) {
          result -= weights_user[i] * (xlogx(temp_n[i]) + xlogx(1 - temp_n[i]));
        }
      }
      return result;
    }
    
    double naive_projection(double lambda_1,
                            double lambda_2,
                            const Eigen::Ref<VecXd>& abs_nu_by_G,
                            const Eigen::Ref<VecXd>& abs_nu_by_GxE,
                            const VecXd& nu) {
      double M = std::numeric_limits<double>::infinity();
      for (int i = 0; i < abs_nu_by_G.size(); ++i) {
        if (abs_nu_by_GxE[i] * lambda_1 - abs_nu_by_G[i] * lambda_2 <= 0) {
          // delta = 0
          if (abs_nu_by_GxE[i] > 0) {
            M = std::min(M, lambda_2 / abs_nu_by_GxE[i]);
          }
          if (abs_nu_by_G[i] > 0) {
            M = std::min(M, lambda_1 / abs_nu_by_G[i]);
          }
        } else {
          // delta = (B * lambda_1 - A * lambda_1) / (A + B)
          if (abs_nu_by_G[i] + abs_nu_by_GxE[i] > 0) {
            M = std::min(M, (lambda_1 + lambda_2) / (abs_nu_by_G[i] + abs_nu_by_GxE[i]));
          }
        }
      }
      double x_hat = 1;
      //double x_hat = triple_dot_product(nu, Y.array() - 0.5, weights_user) / triple_dot_product(nu, nu, weights_user);
      //x_hat = std::min(x_hat, 1.0);
      double x_opt;
      if (std::abs(x_hat) <= M) {
        x_opt = x_hat;
      } else {
        x_opt = sign(x_hat) * M;
      }
      return x_opt;
    }
    
    void update_nu(double lambda_1, double lambda_2) {
      if (!abs_nu_by_G_uptodate) {
        abs_nu_by_G = (nu.cwiseProduct(weights_user).transpose() * G).cwiseAbs().transpose().cwiseProduct(normalize_weights_g);
        abs_nu_by_GxE = (nu.cwiseProduct(weights_user).cwiseProduct(E).transpose() * G).cwiseAbs().transpose().cwiseProduct(normalize_weights_g) * normalize_weights_e;
        abs_nu_by_G_uptodate = false;
      }
      x_opt = naive_projection(lambda_1, lambda_2, abs_nu_by_G, abs_nu_by_GxE, nu);
      nu *= x_opt;
      abs_nu_by_G *= x_opt;
      abs_nu_by_GxE *= x_opt;
    }
    
    void update_inner_nu(double lambda_1, double lambda_2) {
      //    abs_inner_nu_by_G = (inner_nu_res * G(Eigen::all, working_set)).cwiseAbs();
      //    abs_inner_nu_by_GxE = (inner_nu_res.cwiseProduct(E) * G(Eigen::all, working_set)).cwiseAbs();
      // For details on resize vs conservativeResize see
      // https://stackoverflow.com/questions/34449805/reserve-dense-eigen-matrix
      //abs_inner_nu_by_G.conservativeResize(working_set.size());
      //abs_inner_nu_by_GxE.conservativeResize(working_set.size());
      abs_inner_nu_by_G.setZero(working_set.size());
      abs_inner_nu_by_GxE.setZero(working_set.size());
      VecXd inner_nu_weighted = inner_nu.cwiseProduct(weights_user);
      
      for (int i = 0; i < working_set.size(); ++i) {
        temp_n = inner_nu_weighted.cwiseProduct(G.col(working_set[i])) * normalize_weights_g[working_set[i]];
        abs_inner_nu_by_G[i] = std::abs(temp_n.sum());
        abs_inner_nu_by_GxE[i] = std::abs(temp_n.dot(E) * normalize_weights_e);
      }
      double x_opt = naive_projection(lambda_1, lambda_2, abs_inner_nu_by_G, abs_inner_nu_by_GxE, inner_nu);
      inner_nu *= x_opt;
    }
    
    double check_duality_gap(double lambda_1, double lambda_2) {
      update_nu(lambda_1, lambda_2); 
      double dual_objective = compute_dual_objective(nu);
      primal_objective = (-Y.cwiseProduct(xbeta) + log_one_plus_exp(xbeta)).dot(weights_user) + lambda_1 * (b_g.cwiseAbs().cwiseMax(b_gxe.cwiseAbs())).sum() + lambda_2 * b_gxe.cwiseAbs().sum();
      return primal_objective - dual_objective;
    }
    
    double check_inner_duality_gap(double lambda_1, double lambda_2) {
      update_inner_nu(lambda_1, lambda_2);  
      double dual_objective = compute_dual_objective(inner_nu);
      primal_objective = (-Y.cwiseProduct(xbeta) + log_one_plus_exp(xbeta)).dot(weights_user) + lambda_1 * (b_g.cwiseAbs().cwiseMax(b_gxe.cwiseAbs())).sum() + lambda_2 * b_gxe.cwiseAbs().sum();
      return primal_objective - dual_objective;
    }
    
    int update_intercept(int num_passes, int max_iterations, VecXd& nu) {
      while (num_passes < max_iterations) {
        mu = sigmoid(xbeta);
        nu = Y - mu;
        minus_grad = X_1_E_C_w_t * nu;
        if (minus_grad.cwiseAbs().maxCoeff() < 1e-10) {
          break;
        }
        temp_n = mu.array() * (1 - mu.array());        
        H = X_1_E_C_w_t * temp_n.asDiagonal() * X_1_E_C;
        b_diff = H.fullPivHouseholderQr().solve(minus_grad);
        xbeta += X_1_E_C * b_diff;
        b_0 += b_diff(0);
        b_e += b_diff(1);
        if (C.cols() > 0) {
          b_c += b_diff.tail(C.cols());
        }
        abs_nu_by_G_uptodate = false;
        ++num_passes;
      }
      return num_passes;
    }
    
    void update_working_set(double lambda_1, double lambda_2, double dual_gap, int working_set_size) {
      double r = std::sqrt(dual_gap / 2);
      //ArrayXd d_j = ((lambda_1 - abs_nu_by_G.array()) + (r * norm_GxE).array().max(lambda_2 - abs_nu_by_GxE.array())) / (norm_GxE + norm_G).array();
      ArrayXd d_j = (lambda_1 - lambda_2 - (abs_nu_by_G).array() - (lambda_2 - r * norm_GxE.array()).max((abs_nu_by_GxE).array())) / (norm_GxE + norm_G).array();
      
      upperbound_nu_by_G = abs_nu_by_G + r * norm_G;
      upperbound_nu_by_GxE = abs_nu_by_GxE + r * norm_GxE;
      safe_set_zero = (upperbound_nu_by_GxE.array() - lambda_2).max(0) < (lambda_1 - upperbound_nu_by_G.array());
      for (int i = 0; i < p; ++i) {
        safe_set_gxe[i] = safe_set_gxe[i] && (!safe_set_zero[i]) && (upperbound_nu_by_GxE[i] >= lambda_2);
        safe_set_g[i] = safe_set_g[i] && (!safe_set_zero[i]) && (upperbound_nu_by_G[i] >= lambda_1 || safe_set_gxe[i]);      
      }
      
      for (int i = 0; i < p; ++i) {
        if (b_gxe[i] != 0 && (!safe_set_gxe[i])) {
          xbeta -= normalize_weights_e * normalize_weights_g[i] * G.col(i).cwiseProduct(E) * b_gxe[i];
          abs_nu_by_G_uptodate = false;
          b_gxe[i] = 0;
        }
        if (b_g[i] != 0 && (!safe_set_g[i])) {
          xbeta -= normalize_weights_g[i] * G.col(i) * b_g[i];
          abs_nu_by_G_uptodate = false;
          b_g[i] = 0;
        }      
      }
      std::vector<int> working_set_tmp = argsort<ArrayXd>(d_j);
      int index;
      working_set.resize(0);
      for (int i = 0; i < p; ++i) {
        if (b_g[i] != 0 || b_gxe[i] != 0) {
          working_set.push_back(i);
        }
      }
      for (int i = 0; i < p; ++i) {
        index = working_set_tmp[i];
        if (b_g[index] == 0 && b_gxe[index] == 0 && safe_set_g[index] && working_set.size() < working_set_size) {
          working_set.push_back(index);  
        }
      }
    }
    
    virtual double get_value() {
      return primal_objective;
    }
    
    virtual double get_test_loss(const std::vector<int>& test_idx) {
      double test_loss = 0;
      int index;
      for (int i = 0; i < test_idx.size(); ++i) {
        index = test_idx[i];
        test_loss += -Y[index] * xbeta[index] + log_one_plus_exp_scalar(xbeta[index]);
      }
      return test_loss;
    }
};

#endif // BINOMIAL_SOLVER_H
