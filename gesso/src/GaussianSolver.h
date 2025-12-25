#ifndef GAUSSIAN_SOLVER_H
#define GAUSSIAN_SOLVER_H

#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

#include "Solver.h"
#include "SolverTypes.h"


template <typename TG>
class GaussianSolver : public Solver<TG> {

private:
  using Solver<TG>::n;
  using Solver<TG>::p;
  using Solver<TG>::G;
  using Solver<TG>::E;
  using Solver<TG>::Y;
  using Solver<TG>::C;
  using Solver<TG>::normalize;
  using Solver<TG>::weights_user;
  
  using Solver<TG>::normalize_weights_g;
  using Solver<TG>::normalize_weights_e;
  
  using Solver<TG>::b_0;
  using Solver<TG>::b_e;
  using Solver<TG>::b_g;
  using Solver<TG>::b_gxe;
  using Solver<TG>::b_c;
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
  VecXd abs_res_by_G;
  VecXd abs_res_by_GxE;
  double x_opt;
  VecXd upperbound_nu_by_G;
  VecXd upperbound_nu_by_GxE;
  
  VecXd abs_inner_res_by_G;
  VecXd abs_inner_res_by_GxE;
  
  // X^TX where X = (1 | E | C)
  MatXd intercept_system_A;
  VecXd intercept_system_B;
  Eigen::FullPivHouseholderQR<MatXd> intercept_system_A_qr;
  
  public:
    GaussianSolver(const MapMat& G_,
                   const Eigen::Map<Eigen::VectorXd>& E_,
                   const Eigen::Map<Eigen::VectorXd>& Y_,
                   const Eigen::Map<Eigen::MatrixXd>& C_,
                   const Eigen::Map<Eigen::VectorXd>& weights_,
                   bool normalize_) :
      Solver<TG>(G_, E_, Y_, C_, weights_, normalize_),
      abs_res_by_G(p),
      abs_res_by_GxE(p),
      upperbound_nu_by_G(p),
      upperbound_nu_by_GxE(p),
      abs_inner_res_by_G(p),
      abs_inner_res_by_GxE(p),
      intercept_system_A(2 + C_.cols(), 2 + C_.cols()),
      intercept_system_B(2 + C_.cols()) {
      init();
    }
    
    GaussianSolver(const MapSparseMat& G_,
                   const Eigen::Map<Eigen::VectorXd>& E_,
                   const Eigen::Map<Eigen::VectorXd>& Y_,
                   const Eigen::Map<Eigen::MatrixXd>& C_,
                   const Eigen::Map<Eigen::VectorXd>& weights_,
                   bool normalize_) :
    Solver<TG>(G_, E_, Y_, C_, weights_, normalize_),
    abs_res_by_G(p),
    abs_res_by_GxE(p),
    upperbound_nu_by_G(p),
    upperbound_nu_by_GxE(p),
    abs_inner_res_by_G(p),
    abs_inner_res_by_GxE(p),
    intercept_system_A(2 + C_.cols(), 2 + C_.cols()),
    intercept_system_B(2 + C_.cols()) {
      init();
    } 

    void init() {
      weights = weights_user;
      update_weighted_variables(false);
      Z_w = Y.cwiseProduct(weights);
      
      intercept_system_A(0, 0) = weights.sum();
      intercept_system_A(0, 1) = normalize_weights_e * E.dot(weights);
      intercept_system_A(1, 0) = intercept_system_A(0, 1);
      intercept_system_A(1, 1) = sqr(normalize_weights_e) * E.cwiseProduct(E).dot(weights);
      for (int i = 0; i < C.cols(); ++i) {
        intercept_system_A(0, i + 2) = C.col(i).dot(weights);
        intercept_system_A(i + 2, 0) = intercept_system_A(0, i + 2);
        intercept_system_A(1, i + 2) = normalize_weights_e * triple_dot_product(E, C.col(i), weights);
        intercept_system_A(i + 2, 1) = intercept_system_A(1, i + 2);
        for (int j = 0; j <= i; ++j) {
          intercept_system_A(i + 2, j + 2) = triple_dot_product(C.col(i), C.col(j), weights);
          if (i != j) {
            intercept_system_A(j + 2, i + 2) = intercept_system_A(i + 2, j + 2);
          }
        }
      }
      
      intercept_system_A_qr = intercept_system_A.fullPivHouseholderQr();
    }
    
    virtual ~GaussianSolver() {}
    
    virtual int solve(double lambda_1, double lambda_2, double tolerance, int max_iterations, int min_working_set_size) {
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
        duality_gap = check_duality_gap(lambda_1, lambda_2, false);
        if (duality_gap < tolerance) {
          break;
        }
        update_working_set(lambda_1, lambda_2, duality_gap, working_set_size);
        working_set_size = std::min(2 * working_set_size, p);
        
        active_set.setZero(p);
        max_diff_tolerance = tolerance * 4;
        while (num_passes < max_iterations) {
          inner_duality_gap = check_duality_gap(lambda_1, lambda_2, true);
          if (inner_duality_gap < tolerance) {
            break;
          } else {
            max_diff_tolerance /= 4;
          }
          
          while (num_passes < max_iterations) {
            max_diff = update_intercept();
            max_diff = std::max(max_diff, update_b_for_working_set(lambda_1, lambda_2, false));
            num_passes += 1;
            if (max_diff < max_diff_tolerance) {
              break;
            }
            while (num_passes < max_iterations && max_diff >= max_diff_tolerance) {
              max_diff = update_intercept();
              max_diff = std::max(max_diff, update_b_for_working_set(lambda_1, lambda_2, true));
              num_passes += 1;
            }
          }
        }
      }
      return num_passes;
    }
    
    double compute_dual_objective(double x_opt) {
      temp_n = Y - xbeta;
      return x_opt  * triple_dot_product(temp_n, weights, Y) - x_opt * x_opt * weighted_squared_norm(temp_n, weights) / 2;
    }
    
    double naive_projection(double lambda_1, double lambda_2, const Eigen::Ref<VecXd>& abs_res_by_G, const Eigen::Ref<VecXd>& abs_res_by_GxE) {
      //VecXd dual_delta
      double M = std::numeric_limits<double>::infinity();
      for (int i = 0; i < abs_res_by_G.size(); ++i) {
        if (abs_res_by_GxE[i] * lambda_1 - abs_res_by_G[i] * lambda_2 <= 0) {
          // delta = 0
          if (abs_res_by_GxE[i] > 0) {
            M = std::min(M, lambda_2 / abs_res_by_GxE[i]);
          }
          if (abs_res_by_G[i] > 0) {
            M = std::min(M, lambda_1 / abs_res_by_G[i]);
          }
        } else {
          // delta = (B * lambda_1 - A * lambda_1) / (A + B)
          if (abs_res_by_G[i] + abs_res_by_GxE[i] > 0) {
            M = std::min(M, (lambda_1 + lambda_2) / (abs_res_by_G[i] + abs_res_by_GxE[i]));
          }
        }
      }
      temp_n = Y - xbeta;
      double x_hat = triple_dot_product(temp_n, Y, weights) / triple_dot_product(temp_n, temp_n, weights);
      double x_opt;
      if (std::abs(x_hat) <= M) {
        x_opt = x_hat;
      } else {
        x_opt = sign(x_hat) * M;
      }
      return x_opt;
    }
    
    double update_nu(double lambda_1, double lambda_2) {
      if (!abs_nu_by_G_uptodate) {
        temp_n = (Y - xbeta).cwiseProduct(weights); // weighted residual
        abs_res_by_G = (temp_n.transpose() * G).cwiseAbs().transpose().cwiseProduct(normalize_weights_g);
        abs_res_by_GxE = (temp_n.cwiseProduct(E).transpose() * G).cwiseAbs().transpose().cwiseProduct(normalize_weights_g) * normalize_weights_e;
      }
      x_opt = naive_projection(lambda_1, lambda_2, abs_res_by_G, abs_res_by_GxE);
      double dual_objective = compute_dual_objective(x_opt);
      return dual_objective;
    }
    
    double update_inner_nu(double lambda_1, double lambda_2) {
      //    abs_inner_nu_by_G = (inner_nu_res * G(Eigen::all, working_set)).cwiseAbs();
      //    abs_inner_nu_by_GxE = (inner_nu_res.cwiseProduct(E) * G(Eigen::all, working_set)).cwiseAbs();
      // For details on resize vs conservativeResize see
      // https://stackoverflow.com/questions/34449805/reserve-dense-eigen-matrix
      //abs_inner_nu_by_G.conservativeResize(working_set.size());
      //abs_inner_nu_by_GxE.conservativeResize(working_set.size());
      abs_inner_res_by_G.setZero(working_set.size());
      abs_inner_res_by_GxE.setZero(working_set.size());
      
      temp_n = (Y - xbeta).cwiseProduct(weights); // weighted residual
      for (int i = 0; i < working_set.size(); ++i) {
        abs_inner_res_by_G[i] = std::abs(G.col(working_set[i]).dot(temp_n)) * normalize_weights_g[working_set[i]];
        abs_inner_res_by_GxE[i] = std::abs(G.col(working_set[i]).dot(temp_n.cwiseProduct(E))) * normalize_weights_g[working_set[i]] * normalize_weights_e;
      }
      double x_opt = naive_projection(lambda_1, lambda_2, abs_inner_res_by_G, abs_inner_res_by_GxE);
      return compute_dual_objective(x_opt);
    }
    
    double check_duality_gap(double lambda_1, double lambda_2, bool use_working_set) {
      update_intercept();
      double current_dual_objective;
      if (use_working_set) {
        current_dual_objective = update_inner_nu(lambda_1, lambda_2);  
      } else {
        current_dual_objective = update_nu(lambda_1, lambda_2);  
      }
      primal_objective = weighted_squared_norm(Y - xbeta, weights) / 2.0 + lambda_1 * (b_g.cwiseAbs().cwiseMax(b_gxe.cwiseAbs())).sum() + lambda_2 * b_gxe.cwiseAbs().sum();
      return primal_objective - current_dual_objective;
    }
    
    void update_working_set(double lambda_1, double lambda_2, double dual_gap, int working_set_size) {
      double r = sqrt(2 * dual_gap);
      //ArrayXd d_j = ((lambda_1 - (std::fabs(x_opt) * abs_res_by_G).array()) + (r * norm_GxE).array().max(lambda_2 - (std::fabs(x_opt) * abs_res_by_G).array() )) / (norm_GxE + norm_G).array();
      ArrayXd d_j = (lambda_1 - lambda_2 - (std::fabs(x_opt) * abs_res_by_G).array() - (lambda_2 - r * norm_GxE.array()).max((std::fabs(x_opt) * abs_res_by_GxE).array())) / (norm_GxE + norm_G).array();
      
      upperbound_nu_by_G = std::fabs(x_opt) * abs_res_by_G + r * norm_G;
      upperbound_nu_by_GxE = std::fabs(x_opt) * abs_res_by_GxE + r * norm_GxE;
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
      std::sort(working_set.begin(), working_set.end());
    }
    
    double update_intercept() {
      xbeta -= normalize_weights_e * E * b_e;
      xbeta = xbeta.array() - b_0;
      for (int i = 0; i < C.cols(); ++i) {
        xbeta -= C.col(i) * b_c[i];
      }
      intercept_system_B(0) = Z_w.sum() - xbeta.dot(weights);
      intercept_system_B(1) = normalize_weights_e * E.dot(Z_w) - triple_dot_product(E * normalize_weights_e, xbeta, weights);
      for (int i = 0; i < C.cols(); ++i) {
        intercept_system_B(i + 2) = C.col(i).dot(Z_w) - triple_dot_product(C.col(i), xbeta, weights);
      }
      VecXd x = intercept_system_A_qr.solve(intercept_system_B);
      double max_diff = std::max(intercept_system_A(0, 0) * sqr(b_0 - x(0)), intercept_system_A(1, 1) * sqr(b_e - x(1)));
      b_0 = x(0);
      b_e = x(1);
      xbeta = xbeta.array() + b_0;
      xbeta += E * b_e * normalize_weights_e;
      for (int i = 0; i < C.cols(); ++i) {
        max_diff = std::max(max_diff, intercept_system_A(i + 2, i + 2) * sqr(b_c[i] - x(i + 2)));
        b_c[i] = x(i + 2);
        xbeta += C.col(i) * b_c[i];
      }
      
      if (max_diff > 0) {
        abs_nu_by_G_uptodate = false;
      }
      return max_diff;
    }    
    
    virtual double get_value() {
      return primal_objective;
    }

    virtual double get_test_loss(const std::vector<int>& test_idx) {
      double test_loss = 0;
      int index;
      for (int i = 0; i < test_idx.size(); ++i) {
        index = test_idx[i];
        test_loss += sqr(Y[index] - xbeta[index]);
      }
      return test_loss;
    }
    
    void get_residual(VecXd& nu) {
      nu = Y - xbeta;
    }
};

#endif // GAUSSIAN_SOLVER_H
