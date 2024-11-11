#ifndef OEM_XTX_H
#define OEM_XTX_H


#include "oem_base.h"
#include "Spectra/SymEigsSolver.h"
#include "utils.h"
#include "sort.h"

using Eigen::MatrixXd;
using Eigen::MatrixXi;
using Eigen::ArrayXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::SparseMatrix;
using Eigen::RowVectorXd;
using Eigen::Lower;
using Eigen::Upper;
using Eigen::Ref;
using Eigen::Map;
using Rcpp::as;

// minimize  1/2 * ||y - X * beta||^2 + lambda * ||beta||_1
class oemXTX_gen: public oemBase_gen<MatrixXd> //Eigen::SparseVector<double>
{
protected:
  typedef float Scalar;
  typedef double Double;
  typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Matrix;
  typedef Eigen::Matrix<double, Eigen::Dynamic, 1> Vector;
  typedef Eigen::Map<const Eigen::MatrixXd> MapMat;
  typedef Eigen::Map<const Eigen::VectorXd> MapVec;
  typedef Eigen::Map<const Eigen::MatrixXd> MapMatd;
  typedef Eigen::Map<const Eigen::VectorXd> MapVecd;
  typedef Eigen::Map<Eigen::VectorXi> MapVeci;
  typedef const Eigen::Ref<const Eigen::MatrixXd> ConstGenericMatrix;
  typedef const Eigen::Ref<const Eigen::VectorXd> ConstGenericVector;
  typedef Eigen::Ref<Eigen::VectorXd> GenericVector;
  typedef Eigen::Ref<Eigen::MatrixXd> GenericMatrix;
  typedef Eigen::SparseMatrix<double> SpMat;
  typedef Eigen::SparseVector<double> SparseVector;
  
  const MapMatd XX;           // X'X matrix
  const MapMatd XY_init;             // X'Y vector
  MatrixXd XY;                // X'Y vector/matrix
  VectorXi groups;            // vector of group membersihp indexes
  VectorXi unique_groups;     // vector of all unique groups
  VectorXd penalty_factor;    // penalty multiplication factors
  VectorXd group_weights;     // group lasso penalty multiplication factors
  VectorXd scale_factor;      // scaling factor for columns of X
  VectorXd scale_factor_inv;  // inverse of scaling factor for columns of X
  int penalty_factor_size;    // size of penalty_factor vector
  bool selection;
  
  // Eigen::MatrixXd A;                 // A = d * I - X'X
  Eigen::SparseMatrix<double> A;
  double d;                   // d value (largest eigenvalue of X'X)
  bool default_group_weights; // do we need to compute default group weights?
  
  
  std::vector<std::vector<int> > grp_idx; // vector of vectors of the indexes for all members of each group
  std::string penalty;        // penalty specified
  
  double lambda;              // L1 penalty
  double lambda0;             // minimum lambda to make coefficients all zero
  double alpha;               // alpha = mixing parameter for elastic net
  double gamma;               // extra tuning parameter for mcp/scad
  double tau;                 // mixing parameter for group sparse penalties
  
  double threshval;
  int scale_len;
  
  bool found_grp_idx;
  bool is_projection;
  
  static void soft_threshold_sel(MatrixXd &res, const Eigen::MatrixXd &vec, const double &penalty,
                                 VectorXd &pen_fact, double &d)
  {
    int v_size = vec.size();
    res.setZero();
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
      double total_pen = pen_fact(i) * penalty;
      // Rcpp::Rcout << ptr[i] << ", ";
      if(std::abs(ptr[i]) > total_pen) { //changed from std::abs(ptr[i])
        res(i) = ptr[i]/d;
        // res(i) = 1.0;
      }
    }
    Rcpp::Rcout << "\n\n";
  }
  
  static void soft_threshold(MatrixXd &res, const MatrixXd &vec, const double &penalty,
                             VectorXd &pen_fact, double &d)
  {
    int v_size = vec.size();
    res.setZero();
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
      double total_pen = pen_fact(i) * penalty;
      
      if(ptr[i] > total_pen)
        res(i) = (ptr[i] - total_pen)/d;
      else if(ptr[i] < -total_pen)
        res(i) = (ptr[i] + total_pen)/d;
    }
  }
  
  
  static void soft_threshold_mcp(MatrixXd &res, const MatrixXd &vec, const double &penalty,
                                 VectorXd &pen_fact, double &d, double &gamma)
  {
    int v_size = vec.size();
    res.setZero();
    double gammad = gamma * d;
    double d_minus_gammainv = d - 1.0 / gamma;
    
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
      double total_pen = pen_fact(i) * penalty;
      
      if (std::abs(ptr[i]) > gammad * total_pen){
        res(i) = ptr[i]/d;
      } else if(ptr[i] > total_pen)
        res(i) = (ptr[i] - total_pen)/(d_minus_gammainv);
      else if(ptr[i] < -total_pen)
        res(i) = (ptr[i] + total_pen)/(d_minus_gammainv);
      
    }
    
  }
  
  static void soft_threshold_scad(MatrixXd &res, const MatrixXd &vec, const double &penalty,
                                  VectorXd &pen_fact, double &d, double &gamma)
  {
    int v_size = vec.size();
    res.setZero();
    double gammad = gamma * d;
    double gamma_minus1_d = (gamma - 1.0) * d;
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
      double total_pen = pen_fact(i) * penalty;
      
      if (std::abs(ptr[i]) > gammad * total_pen)
        res(i) = ptr[i]/d;
      else if (std::abs(ptr[i]) > (d + 1.0) * total_pen)
      {
        double gam_ptr = (gamma - 1.0) * ptr[i];
        double gam_pen = gamma * total_pen;
        if(gam_ptr > gam_pen)
          res(i) = (gam_ptr - gam_pen)/(gamma_minus1_d - 1.0);
        else if(gam_ptr < -gam_pen)
          res(i) = (gam_ptr + gam_pen)/(gamma_minus1_d - 1.0);
      }
      else if(ptr[i] > total_pen)
        res(i) = (ptr[i] - total_pen)/d;
      else if(ptr[i] < -total_pen)
        res(i) = (ptr[i] + total_pen)/d;
      
    }
  }
  
  static double soft_threshold_scad_norm(double &b, const double &pen, double &d, double &gamma)
  {
    double retval = 0.0;
    
    double gammad = gamma * d;
    double gamma_minus1_d = (gamma - 1.0) * d;
    
    if (std::abs(b) > gammad * pen)
      retval = 1.0;
    else if (std::abs(b) > (d + 1.0) * pen)
    {
      double gam_ptr = (gamma - 1.0);
      double gam_pen = gamma * pen / b;
      if(gam_ptr > gam_pen)
        retval = d * (gam_ptr - gam_pen)/(gamma_minus1_d - 1.0);
      else if(gam_ptr < -gam_pen)
        retval = d * (gam_ptr + gam_pen)/(gamma_minus1_d - 1.0);
    }
    else if(b > pen)
      retval = (1.0 - pen / b);
    else if(b < -pen)
      retval = (1.0 + pen / b);
    return retval;
  }
  
  static double soft_threshold_mcp_norm(double &b, const double &pen, double &d, double &gamma)
  {
    double retval = 0.0;
    
    double gammad = gamma * d;
    double d_minus_gammainv = d - 1.0 / gamma;
    
    if (std::abs(b) > gammad * pen)
      retval = 1.0;
    else if(b > pen)
      retval = d * (1.0 - pen / b)/(d_minus_gammainv);
    else if(b < -pen)
      retval = d * (1.0 + pen / b)/(d_minus_gammainv);
    
    return retval;
  }
  
  static void block_soft_threshold_scad(MatrixXd &res, const MatrixXd &vec, const double &penalty,
                                        VectorXd &pen_fact, double &d,
                                        std::vector<std::vector<int> > &grp_idx,
                                        const int &ngroups, VectorXi &unique_grps, VectorXi &grps,
                                        double & gamma)
  {
    //int v_size = vec.size();
    res.setZero();
    
    for (int g = 0; g < ngroups; ++g)
    {
      double thresh_factor;
      std::vector<int> gr_idx = grp_idx[g];
      
      if (unique_grps(g) == 0) // the 0 group represents unpenalized variables
      {
        thresh_factor = 1.0;
      } else {
        double ds_norm = 0.0;
        for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
        {
          int c_idx = gr_idx[v];
          double val = vec(c_idx);
          ds_norm += val * val;
        }
        ds_norm = std::sqrt(ds_norm);
        // double grp_wts = sqrt(gr_idx.size());
        double grp_wts = pen_fact(g);
        //thresh_factor = std::max(0.0, 1.0 - penalty * grp_wts / (ds_norm) );
        thresh_factor = soft_threshold_scad_norm(ds_norm, penalty * grp_wts, d, gamma);
      }
      if (thresh_factor != 0.0)
      {
        for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
        {
          int c_idx = gr_idx[v];
          res(c_idx) = vec(c_idx) * thresh_factor / d;
        }
      }
    }
  }
  
  static void block_soft_threshold_mcp(MatrixXd &res, const MatrixXd &vec, const double &penalty,
                                       VectorXd &pen_fact, double &d,
                                       std::vector<std::vector<int> > &grp_idx,
                                       const int &ngroups, VectorXi &unique_grps, VectorXi &grps,
                                       double & gamma)
  {
    //int v_size = vec.size();
    res.setZero();
    
    for (int g = 0; g < ngroups; ++g)
    {
      double thresh_factor;
      std::vector<int> gr_idx = grp_idx[g];
      
      if (unique_grps(g) == 0) // the 0 group represents unpenalized variables
      {
        thresh_factor = 1.0;
      } else {
        double ds_norm = 0.0;
        for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
        {
          int c_idx = gr_idx[v];
          double val = vec(c_idx);
          ds_norm += val * val;
        }
        ds_norm = std::sqrt(ds_norm);
        // double grp_wts = sqrt(gr_idx.size());
        double grp_wts = pen_fact(g);
        //thresh_factor = std::max(0.0, 1.0 - penalty * grp_wts / (ds_norm) );
        thresh_factor = soft_threshold_mcp_norm(ds_norm, penalty * grp_wts, d, gamma);
      } // fi unique grp not equal 0
      if (thresh_factor != 0.0)
      {
        for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
        {
          int c_idx = gr_idx[v];
          res(c_idx) = vec(c_idx) * thresh_factor / d;
        } // for looping through values in particular group
      } //fi thresh_factor != 0.0
    }// for looping through groups
  }
  
  static void block_soft_threshold(MatrixXd &res, const MatrixXd &vec, const double &penalty,
                                   VectorXd &pen_fact, double &d,
                                   std::vector<std::vector<int> > &grp_idx,
                                   const int &ngroups, VectorXi &unique_grps, VectorXi &grps)
  {
    //int v_size = vec.size();
    res.setZero();
    
    for (int g = 0; g < ngroups; ++g)
    {
      double thresh_factor;
      std::vector<int> gr_idx = grp_idx[g];
      /*
      for (int v = 0; v < v_size; ++v)
      {
      if (grps(v) == unique_grps(g))
      {
      gr_idx.push_back(v);
      }
      }
      */
      if (unique_grps(g) == 0)
      {
        thresh_factor = 1.0;
      } else
      {
        double ds_norm = 0.0;
        for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
        {
          int c_idx = gr_idx[v];
          double val = vec(c_idx);
          ds_norm += val * val;
        }
        ds_norm = std::sqrt(ds_norm);
        // double grp_wts = sqrt(gr_idx.size());
        double grp_wts = pen_fact(g);
        thresh_factor = std::max(0.0, 1.0 - penalty * grp_wts / (ds_norm) );
      }
      if (thresh_factor != 0.0)
      {
        for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
        {
          int c_idx = gr_idx[v];
          res(c_idx) = vec(c_idx) * thresh_factor / d;
        }
      }
    }
  }
  static double soft_threshold_norm (double &norm, const double &pen)
  {
    return std::max(0.0, 1.0 - (pen / norm) );
  }
  
  void get_group_indexes();
  
  void compute_XtX_d_update_A();
  
  void next_u(MatrixXd &res);
  
  void next_beta(MatrixXd &res);
  
  public:
    oemXTX_gen(const Eigen::Ref<const MatrixXd>  &XX_,
               ConstGenericMatrix &XY_,
               const VectorXi &groups_,
               const VectorXi &unique_groups_,
               VectorXd &group_weights_,
               VectorXd &penalty_factor_,
               const VectorXd &scale_factor_,
               bool selection_ = false,
               const double tol_ = 1e-6) :
    oemBase_gen<MatrixXd>(XX_.rows(),
                          XX_.cols(), XY_.cols(),
                          unique_groups_.size(),
                          false,
                          false,
                          tol_),
                          XX(XX_.data(), XX_.rows(), XX_.cols()),
                          XY_init(XY_.data(), XY_.rows(), XY_.cols()),
                          XY(XY_.rows(), XY_.cols()),
                          groups(groups_),
                          unique_groups(unique_groups_),
                          penalty_factor(penalty_factor_),
                          group_weights(group_weights_),
                          scale_factor(scale_factor_),
                          scale_factor_inv(XX_.cols()),
                          penalty_factor_size(penalty_factor_.size()),
                          default_group_weights(bool(group_weights_.size() < 1)), // compute default weights if none given
                           grp_idx(unique_groups_.size())
                           {selection = selection_;}
    
    
    void init_oem();
    // {
    //   scale_len = scale_factor.size();
    //   
    //   found_grp_idx = false;
    //   
    //   if (scale_len)
    //   {
    //     scale_factor_inv = 1.0 / scale_factor.array();
    //     XY = XY_init.array().colwise() * scale_factor_inv.array();
    //   } else
    //   {
    //     XY = XY_init;
    //   }
    //   // compute XtX or XXt (depending on if n > p or not)
    //   // and compute A = dI - XtX (if n > p)
    //   compute_XtX_d_update_A();
    // }
    
    
    double compute_lambda_zero(std::string penalty_);
    // { //maybe change this
    //   // lambda0 = XY.cwiseAbs().maxCoeff();
    //   int temp_size = XY.rows();
    //   vector temp(temp_size);
    // 
    //   if (!found_grp_idx) {
    //     penalty = penalty_;
    //     get_group_indexes();
    //   }
    // 
    //   if ( XY.cols() > 1 ) {
    //     temp = XY.rowwise().norm();
    //     temp = temp.cwiseQuotient(penalty_factor);
    //   } else if (found_grp_idx) {
    //     temp.resize(ngroups);
    //     temp.fill(0.0);
    // 
    // 
    //     for ( int g = 0; g < ngroups; g++ ) {
    //       std::vector<int> gr_idx = grp_idx[g];
    //       for ( int i = 0; i < gr_idx.size(); i++ ) {
    //         double val = XY(gr_idx[i]);
    //         temp(g) += val * val;
    //       }
    //     }
    //     temp = temp.cwiseSqrt().eval();
    //     temp = temp.cwiseQuotient(group_weights);
    //   } else {
    //     temp = XY.cwiseQuotient(penalty_factor).cwiseAbs().eval();
    //   }
    //   if ( penalty_factor.cwiseEqual(0.0).any() ) {
    //     for ( int i = 0; i < temp_size; i ++ ) {
    //       if ( penalty_factor(i) < Eigen::NumTraits<double>::dummy_precision() ) temp(i) = 0.0;
    //     }
    //   }
    //   lambda0 = temp.maxCoeff();
    // 
    //   return lambda0;
    // }
    
    double get_d() { return d; }
    
    // init() is a cold start for the first lambda
    void init(double lambda_, std::string penalty_,
              double alpha_, double gamma_, double tau_);
    // {
    //   beta.setZero();
    //   
    //   lambda = lambda_;
    //   penalty = penalty_;
    //   
    //   alpha = alpha_;
    //   gamma = gamma_;
    //   tau   = tau_;
    //   
    //   std::string projectionstxt("projection.");
    //   is_projection = penalty.find(projectionstxt) != std::string::npos;
    //   
    //   // get indexes of members of each group.
    //   // best to do just once in the beginning
    //   if (!found_grp_idx)
    //   {
    //     get_group_indexes();
    //   }
    //   
    // }
    
    void beta_ones() //diff
    {
      beta.setOnes(); // set to ones for OLS solution.
      //start with OLS since reverse order of lambdas is faster
      //also guaranteed to start with approximate correct solution.
    }
    void beta_zeros() //diff
    {
      beta.setZero();
    }
    
    // when computing for the next lambda, we can use the
    // current main_x, aux_z, dual_y and rho as initial values
    void init_warm(double lambda_)
    {
      lambda = lambda_;
      
    }
    
    void init_warm_xty() // right now still points to original xty. If this changes, function will break down!!!
    {
      if (scale_len)
      {
        // scale_factor_inv = 1.0 / scale_factor.array();
        XY = XY_init.array().colwise() * scale_factor_inv.array();
      } else
      {
        XY = XY_init;
      }
      // if( nonFinite(beta)) {
      //   beta.setZero();
      //   beta_prev.setZero();
      // }
      // if(nonFiniteDist(beta) ) {
      //   beta.setZero();
      //   beta_prev.setZero();
      // }
      // if( beta.hasNaN()) {
      //   beta.setZero();
      //   beta_prev.setZero();
      // }
    }
    
    MatrixXd get_beta()
    {
      matrix res = beta;
      if (scale_len) {
        res.array().colwise() *= scale_factor_inv.array();
      }
      
      if (selection) {
        // for( int i = 0; i < beta.size(); i ++) {
        //   if(beta(i) != 0) {
        //     res(i) = 1.0;
        //   } else {
        //     res(i) = 0.0;
        //   }
        // }
        // Rcpp::Rcout << "\nProject\n";
        // Rcpp::Rcout << "beta.size(): " << beta.size() << "\n";
        for( int i = 0; i < beta.size(); i ++) {
          // Rcpp::Rcout << res(i) << ", ";
          if(res(i) >= 0.5) {
            res(i) = 1.0;
          } else {
            res(i) = 0.0;
          }
        }
        beta = res.array().colwise() * scale_factor.array();
        u = beta.array() * d;
      }
      // Rcpp::Rcout << beta << "\n";
      
      
      return res;
    }
    
    virtual double get_loss()
    {
      return 1e99;
    }
    
    };


#endif // OEM_XTX_H
