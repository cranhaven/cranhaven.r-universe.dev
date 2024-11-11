#ifndef OEM_BASE_H
#define OEM_BASE_H

#include <RcppEigen.h>
#include "utils.h"

template<typename TypeBeta>
class oemBase_gen
{
protected:
  
  const int nvars;                  // dimension of beta
  const int betadim;                // vector space of beta
  int nobs;                         // number of rows
  const int ngroups;                // number of groups for group lasso
  
  bool intercept;                   //
  bool standardize;                 //
  
  double meanY;
  double scaleY;
  
  TypeBeta u;                       // u vector
  
  TypeBeta beta;                 // parameters to be optimized
  TypeBeta beta_prev;            // parameters from previous iteration
  TypeBeta beta_prev_irls;       // parameters from previous irls iteration
  
  Eigen::RowVectorXd colmeans;      // column means of X
  Eigen::RowVectorXd colstd;        // column std devs of X
  
  double tol;                       // tolerance for convergence
  
  virtual void next_beta(TypeBeta &res) = 0;
  
  
  virtual void next_u(TypeBeta &res) = 0;
  
  virtual bool converged()
  {
    return (stopRuleMat(beta, beta_prev, tol)); //diff
  }
  
  
  void print_row(int iter)
  {
    const char sep = ' ';
    
    Rcpp::Rcout << std::left << std::setw(7)  << std::setfill(sep) << iter;
    Rcpp::Rcout << std::endl;
  }
  
  
public:
  oemBase_gen(int n_,
          int p_, 
          int s_, //diff
            int ngroups_,
            bool intercept_,
            bool standardize_,
            double tol_ = 1e-6) :
  nvars(p_*s_), //diff
  betadim(p_),
  nobs(n_),
  ngroups(ngroups_),
  intercept(intercept_),
  standardize(standardize_),
  u(p_, s_),               // allocate space but do not set values //diff
  beta(p_, s_),            // allocate space but do not set values //diff
  beta_prev(p_, s_),       // allocate space but do not set values //diff
  beta_prev_irls(p_, s_),
  colmeans(p_),
  colstd(p_),
  tol(tol_)
  {}
  
  virtual ~oemBase_gen() {}
  
  void update_u()
  {
    //TypeBeta newbeta(nvars);
    next_u(u);
    //beta.swap(newbeta);
  }
  
  void update_beta()
  {
    //TypeBeta newbeta(nvars);
    next_beta(beta);
    //beta.swap(newbeta);
  }
  
  
  virtual int solve(int maxit)
  {
    int i;
    
    for(i = 0; i < maxit; ++i)
    {
      // Rcpp::Rcout << "iteration " << i << "\n";
      if(i % 1000)  Rcpp::checkUserInterrupt(); 
      
      beta_prev = beta;
      
      update_u();
      
      update_beta();
      
      if(converged()) break;
      
    }
    
    
    return i + 1;
  }
  
  virtual void init_oem() {}
  
  virtual void init_xtx(bool add_int_) {}
  virtual void update_xtx(int fold_) {}
  virtual double compute_lambda_zero(std::string penalty_) { return 0; }
  virtual TypeBeta get_beta() { return beta; }
  virtual double get_d() { return 0; }
  
  Eigen::RowVectorXd get_X_colmeans() {return colmeans;}
  Eigen::RowVectorXd get_X_colstd() {return colstd;}
  
  virtual double get_loss() { return 1e99; }
  
  virtual void init(double lambda_, std::string penalty_,
                    double alpha_, double gamma_, double tau_) {}
  virtual void beta_ones() {} //diff
  virtual void beta_zeros() {} //diff
  virtual void init_warm(double lambda_) {} 
  
  virtual void init_warm_xty(){} //diff
  virtual double next_lambda(double lambda_current, int num_act){return 0.0;};
};

#endif // OEM_BASE_H
