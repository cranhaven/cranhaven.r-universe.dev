#ifndef SOLVER_H
#define SOLVER_H

#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

#include "SolverTypes.h"

inline double sqr(double x) {
  return x * x;
}

inline double sign(double x) {
  return (x > 0) ? 1 : ((x < 0) ? -1 : 0);
}

inline double soft_threshold(double x, double lambda) {
  if (x > lambda) {
    return x - lambda;
  }
  if (x < - lambda){
    return x + lambda;
  }
  return 0;
}

template<typename A, typename B, typename C>
double triple_dot_product(const A& a, const B& b, const C& c) {
  double result = 0;
  for (int i = 0; i < a.size(); ++i) {
    result += a[i] * b[i] * c[i];
  }
  return result;
}

template<typename A, typename B>
double weighted_squared_norm(const A& x, const B& weights) {
  double result = 0;
  for (int i = 0; i < x.size(); ++i) {
    result += x[i] * x[i] * weights[i];
  }
  return result;
}

template<typename T>
std::vector<int> argsort(const T& array) {
  std::vector<int> indices(array.size());
  std::iota(indices.begin(), indices.end(), 0);
  std::sort(indices.begin(), indices.end(),
            [&array](int left, int right) -> bool {
              // sort indices according to corresponding array element
              return array[left] < array[right];
            });
  
  return indices;
}

template <typename TG>
class Solver {
  
protected:
  const int n;
  const int p;
  TG G;
  MapVec E;
  MapVec Y;
  MapMat C;
  MapVec weights_user;
  bool normalize;

  VecXd normalize_weights_g;
  double normalize_weights_e;
  
  double b_0;
  double b_e;
  VecXd b_c;
  VecXd b_g;
  VecXd b_gxe;
  VecXd delta;
  
  VecXd weights;
  VecXd xbeta;
  VecXd Z_w;
  
  ArrayXb safe_set_g;
  ArrayXb safe_set_gxe;
  ArrayXb safe_set_zero;
  std::vector<int> working_set;  
  
  bool abs_nu_by_G_uptodate;

  // Pre-computed constants for updating b_g and b_gxe
  VecXd norm2_G;
  VecXd norm2_GxE;
  VecXd G_by_GxE;
  VecXd case1_A22_div_detA;
  VecXd case1_A12_div_detA;  

  // Pre-computed constants for dual computations
  VecXd norm_G;
  VecXd norm_GxE;
  
  ArrayXb active_set;
  
  VecXd temp_p;
  VecXd temp_n;
  
public:

  public: Solver(const MapMat& G_,
                 const Eigen::Map<Eigen::VectorXd>& E_,
                 const Eigen::Map<Eigen::VectorXd>& Y_,
                 const Eigen::Map<Eigen::MatrixXd>& C_,
                 const Eigen::Map<Eigen::VectorXd>& weights_,
                 bool normalize_) :
    n(G_.rows()),
    p(G_.cols()),
    G(G_.data(), G_.rows(), p),
    E(E_.data(), E_.rows()),
    Y(Y_.data(), Y_.rows()),
    C(C_.data(), C_.rows(), C_.cols()),
    weights_user(weights_.data(), weights_.rows()),
    normalize(normalize_),
    normalize_weights_g(p),
    b_0(0), 
    b_e(0),
    b_c(C_.cols()),
    b_g(p),
    b_gxe(p),
    delta(p),
    weights(n),
    xbeta(n),
    Z_w(n),
    safe_set_g(p),
    safe_set_gxe(p),
    safe_set_zero(p),
    norm2_G(p),
    norm2_GxE(p),
    G_by_GxE(p),
    case1_A22_div_detA(p),
    case1_A12_div_detA(p),
    norm_G(p),    
    norm_GxE(p),    
    active_set(p),
    temp_p(p),
    temp_n(n) {
    
    base_init();
  }  
    
  Solver(const MapSparseMat& G_,
         const Eigen::Map<Eigen::VectorXd>& E_,
         const Eigen::Map<Eigen::VectorXd>& Y_,
         const Eigen::Map<Eigen::MatrixXd>& C_,
         const Eigen::Map<Eigen::VectorXd>& weights_,
         bool normalize_) :    
    n(G_.rows()),
    p(G_.cols()),
    G(G_),
    E(E_.data(), E_.rows()),
    Y(Y_.data(), Y_.rows()),
    C(C_.data(), C_.rows(), C_.cols()),
    weights_user(weights_.data(), weights_.rows()),
    normalize(normalize_),
    normalize_weights_g(p),
    b_0(0), 
    b_e(0),
    b_c(C_.cols()),
    b_g(p),
    b_gxe(p),
    delta(p),
    weights(n),
    xbeta(n),
    Z_w(n),
    safe_set_g(p),
    safe_set_gxe(p),
    safe_set_zero(p),
    norm2_G(p),
    norm2_GxE(p),
    G_by_GxE(p),
    case1_A22_div_detA(p),
    case1_A12_div_detA(p),
    norm_G(p),
    norm_GxE(p),
    active_set(p),
    temp_p(p),
    temp_n(n) {

    base_init();
  }     
    
  virtual ~Solver() {}
    
  void base_init() {
    b_c.setZero(C.cols());
    b_g.setZero(p);
    b_gxe.setZero(p);
    delta.setZero(p);
    
    if (normalize) {
      for (int i = 0; i < p; ++i) {
        normalize_weights_g[i] = 1.0 / std::sqrt(G.col(i).cwiseProduct(G.col(i)).dot(weights_user) - sqr(G.col(i).dot(weights_user)));
      }
      normalize_weights_e = 1.0 / std::sqrt(E.cwiseProduct(E).dot(weights_user) - sqr(E.dot(weights_user)));
    } else {
      normalize_weights_g.setOnes(p);
      normalize_weights_e = 1;
    }

    for (int i = 0; i < G.cols(); ++i) {
      temp_n = G.col(i).cwiseProduct(G.col(i)) * sqr(normalize_weights_g[i]);
      norm2_G[i] = temp_n.dot(weights_user);
      temp_n = normalize_weights_e * temp_n.cwiseProduct(E);
      temp_n = normalize_weights_e * temp_n.cwiseProduct(E);
      norm2_GxE[i] = temp_n.dot(weights_user);
    }
    norm_G = norm2_G.cwiseSqrt();
    norm_GxE = norm2_GxE.cwiseSqrt();
    
    xbeta.setZero(n);
    abs_nu_by_G_uptodate = false;
    
    working_set.reserve(p);
  }
  
  virtual int solve(double lambda_1, double lambda_2, double tolerance, int max_iterations, int min_working_set_size) = 0;
    
  void update_weighted_variables(bool working_set_only) {
    int i;
    if (working_set_only) {
      for (int j = 0; j < working_set.size(); ++j) {
        i = working_set[j];
        temp_n = G.col(i).cwiseProduct(G.col(i)) * sqr(normalize_weights_g[i]);
        norm2_G[i] = temp_n.dot(weights);
        temp_n = normalize_weights_e * temp_n.cwiseProduct(E);
        G_by_GxE[i] = temp_n.dot(weights);
        temp_n = normalize_weights_e * temp_n.cwiseProduct(E);
        norm2_GxE[i] = temp_n.dot(weights);
      }
    } else {
      for (int i = 0; i < G.cols(); ++i) {
        temp_n = G.col(i).cwiseProduct(G.col(i)) * sqr(normalize_weights_g[i]);
        norm2_G[i] = temp_n.dot(weights);
        temp_n = normalize_weights_e * temp_n.cwiseProduct(E);
        G_by_GxE[i] = temp_n.dot(weights);
        temp_n = normalize_weights_e * temp_n.cwiseProduct(E);
        norm2_GxE[i] = temp_n.dot(weights);
      }      
    }

    // const VecXd case1_detA
    temp_p = norm2_G.cwiseProduct(norm2_GxE) - G_by_GxE.cwiseProduct(G_by_GxE);
    case1_A22_div_detA = norm2_GxE.cwiseQuotient(temp_p);
    case1_A12_div_detA = G_by_GxE.cwiseQuotient(temp_p);
  }
  
  double update_b_for_working_set(double lambda_1, double lambda_2, bool active_set_iteration) {
    const double plus_minus_one[] = {-1.0, 1.0};
    double curr_diff;
    double max_diff = 0;
    double G_by_res, GxE_by_res;
    double delta_upperbound, delta_lowerbound;
    bool has_solved;
    int index;
    double b_0_old, b_e_old, b_g_old, b_gxe_old, b_g_new;
    double case1_B1_A22_div_detA;
    double case1_B2, s, root_1, root_2, b_gxe_numerator;
    double case_3_C, case_3_D, case_3_E, case_3_F;
    double s_g, s_gxe, case_3_E_D, case_3_C_F, case_3_B_s_g, root_3, b_g_numerator;
    double case5_B2;
    
    for (int k = 0; k < working_set.size(); ++k) {
      index = working_set[k];
      if (active_set_iteration && !active_set[index]) {
        continue;
      }
      b_g_old = b_g[index];
      b_gxe_old = b_gxe[index];
      
      xbeta -= normalize_weights_g[index] * G.col(index).cwiseProduct((b_g[index] + ((normalize_weights_e * b_gxe[index]) * E).array()).matrix());
      
      temp_n = normalize_weights_g[index] * (Z_w - xbeta.cwiseProduct(weights)).cwiseProduct(G.col(index));
      G_by_res = temp_n.sum();
      GxE_by_res = normalize_weights_e * E.dot(temp_n);
      
      if (norm2_GxE[index] == 0.0 || !safe_set_gxe[index]) {
        delta_upperbound = lambda_1 - std::abs(G_by_res);
        delta_lowerbound = std::max(-lambda_2 + std::abs(GxE_by_res), 0.0);
        if (delta_lowerbound <= delta_upperbound) {
          b_g[index] = 0; b_gxe[index] = 0; delta[index] = delta_upperbound;
          curr_diff = norm2_G[index] * sqr(b_g_old);
          max_diff = std::max(max_diff, curr_diff);
          if (curr_diff > 0) {
            abs_nu_by_G_uptodate = false;
            if (!active_set_iteration && !active_set[index]) {
              active_set[index] = true;
            }              
          }
          continue;
        } else {
          b_g_new = soft_threshold(G_by_res, lambda_1) / norm2_G[index];
          b_g[index] = b_g_new; b_gxe[index] = 0; delta[index] = 0;
          xbeta += normalize_weights_g[index] * b_g[index] * G.col(index);
          curr_diff = norm2_G[index] * sqr(b_g_new - b_g_old);
          max_diff = std::max(max_diff, curr_diff);
          if (curr_diff > 0) {
            abs_nu_by_G_uptodate = false;
            if (!active_set_iteration && !active_set[index]) {
              active_set[index] = true;
            }              
          }    
          continue;
        }
      }
      
      // Case 2
      delta_upperbound = lambda_1 - std::abs(G_by_res);
      delta_lowerbound = std::max(-lambda_2 + std::abs(GxE_by_res), 0.0);
      if (delta_lowerbound <= delta_upperbound) {
        b_g[index] = 0; b_gxe[index] = 0; delta[index] = delta_upperbound;
        curr_diff = std::max(norm2_G[index] * sqr(b_g_old), 
                             norm2_GxE[index] * sqr(b_gxe_old));
        max_diff = std::max(max_diff, curr_diff);      
        if (curr_diff > 0) {
          abs_nu_by_G_uptodate = false;
          if (!active_set_iteration && !active_set[index]) {
            active_set[index] = true;
          }              
        }
        continue;
      }
      
      has_solved = false;
      // Case 1
      case1_B1_A22_div_detA = G_by_res * case1_A22_div_detA[index];
      for (int i = 0; i < 2; ++i) {
        s = plus_minus_one[i];
        case1_B2 = GxE_by_res - s * (lambda_1 + lambda_2);
        root_1 = case1_B1_A22_div_detA - case1_B2 * case1_A12_div_detA[index];
        root_2 = (case1_B2 - root_1 * G_by_GxE[index]) / norm2_GxE[index];
        if (std::abs(root_2) > std::abs(root_1)) {
          b_gxe_numerator = GxE_by_res - G_by_GxE[index]  * root_1;
          if (s * b_gxe_numerator > lambda_1 + lambda_2) {
            b_g[index] = root_1; b_gxe[index] = root_2; delta[index] = lambda_1;
            xbeta += normalize_weights_g[index] * (b_g[index] * G.col(index) + normalize_weights_e * G.col(index).cwiseProduct(E) * b_gxe[index]);
            curr_diff = std::max(norm2_G[index] * sqr(b_g_old - root_1), 
                                 norm2_GxE[index] * sqr(b_gxe_old - root_2));
            max_diff = std::max(max_diff, curr_diff);              
            if (curr_diff > 0) {
              abs_nu_by_G_uptodate = false;
              if (!active_set_iteration && !active_set[index]) {
                active_set[index] = true;
              }              
            }     
            has_solved = true;
            break;
          }
        }
      }
      if (has_solved) {
        continue;
      }
      
      // Case 3
      case_3_C = GxE_by_res * G_by_GxE[index] - G_by_res * norm2_GxE[index];
      case_3_D = GxE_by_res * norm2_G[index] - G_by_res * G_by_GxE[index];
      case_3_E = G_by_GxE[index] * (lambda_1 - lambda_2);
      case_3_F = (lambda_1 * norm2_GxE[index] - lambda_2 * norm2_G[index]);
      for (int i = 0; i < 2; ++i) {
        s_g = plus_minus_one[i];
        case_3_E_D = s_g * case_3_E + case_3_D;
        case_3_C_F = s_g * case_3_C + case_3_F;
        case_3_B_s_g = s_g * 2 * G_by_GxE[index];
        for (int j = 0; j < 2; ++j) {
          s_gxe = plus_minus_one[j];
          root_3 = (s_gxe * case_3_E_D + case_3_C_F) / (norm2_G[index] + norm2_GxE[index] + s_gxe * case_3_B_s_g);
          if ((root_3 >= 0) && (root_3 < lambda_1)) {
            root_1 = (G_by_res - s_g * (lambda_1 - root_3)) / (norm2_G[index] + s_g * s_gxe * G_by_GxE[index]);
            root_2 = s_g * s_gxe * root_1;
            b_gxe_numerator = GxE_by_res - G_by_GxE[index] * root_1;
            b_g_numerator = (G_by_res - root_2 * G_by_GxE[index]);
            if ((s_gxe * b_gxe_numerator > lambda_2 + root_3) &&
                (s_g * b_g_numerator > lambda_1 - root_3)) {
              b_g[index] = root_1; b_gxe[index] = root_2; delta[index] = root_3;
              xbeta += normalize_weights_g[index] * (b_g[index] * G.col(index) + normalize_weights_e * G.col(index).cwiseProduct(E) * b_gxe[index]);
              curr_diff = std::max(norm2_G[index] * sqr(b_g_old - root_1), 
                                   norm2_GxE[index] * sqr(b_gxe_old - root_2));
              max_diff = std::max(max_diff, curr_diff);               
              if (curr_diff > 0) {
                abs_nu_by_G_uptodate = false;
                if (!active_set_iteration && !active_set[index]) {
                  active_set[index] = true;
                }              
              }           
              has_solved = true;
              break;
            }          
          }
        }
        if (has_solved) {
          break;
        }      
      }
      if (has_solved) {
        continue;
      }  
      
      // Case 4
      b_g_new = soft_threshold(G_by_res, lambda_1) / norm2_G[index];
      b_gxe_numerator = GxE_by_res - G_by_GxE[index] * b_g_new;
      if (std::abs(b_gxe_numerator) <= lambda_2) {
        b_g[index] = b_g_new; b_gxe[index] = 0; delta[index] = 0;
        xbeta += normalize_weights_g[index] * b_g[index] * G.col(index);
        curr_diff = std::max(norm2_G[index] * sqr(b_g_old - b_g_new), 
                             norm2_GxE[index] * sqr(b_gxe_old));
        max_diff = std::max(max_diff, curr_diff);         
        if (curr_diff > 0) {
          abs_nu_by_G_uptodate = false;
          if (!active_set_iteration && !active_set[index]) {
            active_set[index] = true;
          }              
        }    
        continue;
      }    
      
      // Case 5
      for (int i = 0; i < 2; ++i) {
        s_g = plus_minus_one[i];
        for (int j = 0; j < 2; ++j) {
          s_gxe = plus_minus_one[j];
          case5_B2 = GxE_by_res - s_gxe * lambda_2;
          root_1 = (G_by_res - s_g * lambda_1) * case1_A22_div_detA[index] - case5_B2 * case1_A12_div_detA[index];
          b_gxe_numerator = GxE_by_res - G_by_GxE[index] * root_1;
          if  (s_gxe * b_gxe_numerator > lambda_2) {
            root_2 = (case5_B2 - root_1 * G_by_GxE[index]) / norm2_GxE[index];
            b_g_numerator = (G_by_res - root_2 * G_by_GxE[index]);
            if (s_g * b_g_numerator > lambda_1) {
              b_g[index] = root_1; b_gxe[index] = root_2; delta[index] = 0;
              xbeta += normalize_weights_g[index] * (b_g[index] * G.col(index) + normalize_weights_e * G.col(index).cwiseProduct(E) * b_gxe[index]);
              curr_diff = std::max(norm2_G[index] * sqr(b_g_old - root_1), 
                                   norm2_GxE[index] * sqr(b_gxe_old - root_2));
              max_diff = std::max(max_diff, curr_diff);                     
              if (curr_diff > 0) {
                abs_nu_by_G_uptodate = false;
                if (!active_set_iteration && !active_set[index]) {
                  active_set[index] = true;
                }              
              }       
              has_solved = true;
              break;
            }
          }
        }
        if (has_solved) {
          break;
        }      
      }
    }
    return max_diff;
  }  
    
  double get_b_0() {
    return b_0;
  }
    
  double get_b_e() {
    return b_e * normalize_weights_e;
  }
  
  VecXd get_b_c() {
    return b_c;
  }  
    
  VecXd get_b_g() {
    return b_g.cwiseProduct(normalize_weights_g);
  }  
    
  VecXd get_b_gxe() {
    return b_gxe.cwiseProduct(normalize_weights_g) * normalize_weights_e;
  }
  
  VecXd get_normalize_weights_g() {
    return normalize_weights_g;
  }
  
  double get_normalize_weights_e() {
    return normalize_weights_e;
  }  
    
  int get_b_g_non_zero() {
    int result = 0;
    for (int i = 0; i < p; ++i) {
      result += int((b_g[i] != 0) && (normalize_weights_g[i] != 0));
    }
    return result;
  }
    
  int get_b_gxe_non_zero() {
    int result = 0;
    if (normalize_weights_e == 0) {
      return 0;
    }
    for (int i = 0; i < p; ++i) {
      result += int((b_gxe[i] != 0) && (normalize_weights_g[i] != 0));
    }
    return result;
  }    
  
  double get_xbeta(int index) {
    return xbeta[index];
  }
    
  int get_working_set_size() {
    return working_set.size();
  }
    
  int get_num_fitered_by_safe_g() {
    int num_filtered = 0;
    for (int i = 0; i < safe_set_g.size(); ++i) {
      if (!safe_set_g[i]) {
        ++num_filtered;
      }
    }
    return num_filtered;
  }
    
  int get_num_fitered_by_safe_gxe() {
    int num_filtered = 0;
    for (int i = 0; i < safe_set_g.size(); ++i) {
      if (!safe_set_g[i]) {
        ++num_filtered;
      }
    }
    return num_filtered;
  }
    
  virtual double get_value() = 0;

  virtual double get_test_loss(const std::vector<int>& test_idx) = 0;
};

#endif // SOLVER_H
