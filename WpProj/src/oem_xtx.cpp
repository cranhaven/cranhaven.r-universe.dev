#include "oem_xtx.h"

void oemXTX_gen::get_group_indexes()
{
  // if the group is any group penalty
  std::string grptxt("grp");
  if (penalty.find(grptxt) != std::string::npos)
  {
    found_grp_idx = true;
    grp_idx.reserve(ngroups);
    for (int g = 0; g < ngroups; ++g)
    {
      // find all variables in group number g
      std::vector<int> idx_tmp;
      for (int v = 0; v < betadim; ++v)
      {
        if (groups(v) == unique_groups(g))
        {
          idx_tmp.push_back(v);
        }
      }
      grp_idx[g] = idx_tmp;
    }
    // if group weights were not specified,
    // then set the group weight for each
    // group to be the sqrt of the size of the
    // group
    if (default_group_weights)
    {
      group_weights.resize(ngroups);
      for (int g = 0; g < ngroups; ++g)
      {
        if(unique_groups(g) == 0){
          group_weights(g) = 0;
        } else {
          group_weights(g) = std::sqrt(double(grp_idx[g].size()));
        }
      }
    }
  }
}

void oemXTX_gen::compute_XtX_d_update_A()
{
  MatrixXd XXmat(XX.rows(), XX.cols());
  if (scale_len)
  {
    XXmat = scale_factor_inv.asDiagonal() * XX * scale_factor_inv.asDiagonal();
  } else
  {
    XXmat = XX;
  }
  // Rcpp::Rcout << "init obj"<< "\n";
  Spectra::DenseSymMatProd<double> op(XXmat);
  int ncv = 4;
  if (XX.cols() < 4)
  {
    ncv = XX.cols();
  }
  // Rcpp::Rcout << ncv << "\n";
  // Rcpp::stop("safety");
  Spectra::SymEigsSolver< double, Spectra::LARGEST_ALGE, Spectra::DenseSymMatProd<double> > eigs(&op, 1, ncv); //object, number eig val (nev), convergence speed >= 2*nev
  // Rcpp::Rcout << "what's happening??\n";
  eigs.init();
  eigs.compute(10000, 1e-10); // values are iterations and tolerance
  Vector eigenvals = eigs.eigenvalues();
  d = eigenvals[0] * 1.005; // multiply by an increasing factor to be safe
  
  Eigen::MatrixXd temp_A = -XXmat;
  
  
  temp_A.diagonal().array() += d;
  A = temp_A.sparseView();
  
}

void oemXTX_gen::next_u(MatrixXd &res)
{
  res = XY;
  res += A * beta_prev.sparseView();
  // res.noalias() += A * beta_prev;
}

void oemXTX_gen::next_beta(MatrixXd &res)
{
  if (penalty == "lasso")
  {
    soft_threshold(beta, u, lambda, penalty_factor, d);
  } else if (penalty == "ols")
  {
    beta = u / d;
  } else if (penalty == "elastic.net")
  {
    double denom = d + (1.0 - alpha) * lambda;
    double lam = lambda * alpha;
    
    soft_threshold(beta, u, lam, penalty_factor, denom);
  } else if (penalty == "scad")
  {
    soft_threshold_scad(beta, u, lambda, penalty_factor, d, gamma);
    
  } else if (penalty == "scad.net")
  {
    double denom = d + (1.0 - alpha) * lambda;
    double lam = lambda * alpha;
    
    if (alpha == 0)
    {
      lam   = 0;
      denom = d + lambda;
    }
    
    soft_threshold_scad(beta, u, lam, penalty_factor, denom, gamma);
    
  } else if (penalty == "mcp")
  {
    soft_threshold_mcp(beta, u, lambda, penalty_factor, d, gamma);
  } else if (penalty == "mcp.net")
  {
    double denom = d + (1.0 - alpha) * lambda;
    double lam = lambda * alpha;
    
    soft_threshold_mcp(beta, u, lam, penalty_factor, denom, gamma);
    
  } else if (penalty == "grp.lasso")
  {
    block_soft_threshold(beta, u, lambda, group_weights,
                         d, grp_idx, ngroups,
                         unique_groups, groups);
  } else if (penalty == "grp.lasso.net")
  {
    double denom = d + (1.0 - alpha) * lambda;
    double lam = lambda * alpha;
    
    block_soft_threshold(beta, u, lam, group_weights,
                         denom, grp_idx, ngroups,
                         unique_groups, groups);
    
  } else if (penalty == "grp.mcp")
  {
    block_soft_threshold_mcp(beta, u, lambda, group_weights,
                             d, grp_idx, ngroups,
                             unique_groups, groups, gamma);
  } else if (penalty == "grp.scad")
  {
    block_soft_threshold_scad(beta, u, lambda, group_weights,
                              d, grp_idx, ngroups,
                              unique_groups, groups, gamma);
  } else if (penalty == "grp.mcp.net")
  {
    double denom = d + (1.0 - alpha) * lambda;
    double lam = lambda * alpha;
    
    
    block_soft_threshold_mcp(beta, u, lam, group_weights,
                             denom, grp_idx, ngroups,
                             unique_groups, groups, gamma);
  } else if (penalty == "grp.scad.net")
  {
    double denom = d + (1.0 - alpha) * lambda;
    double lam = lambda * alpha;
    
    block_soft_threshold_scad(beta, u, lam, group_weights,
                              denom, grp_idx, ngroups,
                              unique_groups, groups, gamma);
  } else if (penalty == "sparse.grp.lasso") {
    double lam_grp = (1.0 - tau) * lambda;
    double lam_l1  = tau * lambda;
    
    double fact = 1.0;
    
    // first apply soft thresholding
    // but don't divide by d
    soft_threshold(beta, u, lam_l1, penalty_factor, fact);
    
    MatrixXd beta_tmp = beta;
    
    // then apply block soft thresholding
    block_soft_threshold(beta, beta_tmp, lam_grp,
                         group_weights,
                         d, grp_idx, ngroups,
                         unique_groups, groups);
  } else if ( penalty == "selection.lasso" )
  { //added elastic net penalty
    // soft_threshold_sel(beta, u, lambda, penalty_factor, d);
    // double denom = d + (1.0 - alpha) * lambda;
    // double lam = lambda * alpha; // comment out cause not divided by alpha earlier
    double denom = d + (1.0 - alpha) * lambda / alpha; //correct for fact not div alpha earlier
    double lam = lambda;
    soft_threshold(beta, u, lam, penalty_factor, denom);
  } else if (is_projection) {
    int P = beta.rows();
    vector norms = u.rowwise().norm();
    vector thresh(P);
    double denom = d;
    if ( penalty == "projection.elastic.net" || penalty == "projection.scad.net"  || penalty == "projection.mcp.net") {
      denom += (1.0 - alpha) * lambda;
    }
    if (found_grp_idx) {
      P = ngroups;
      vector gnorm_vec(P);
      for (int g = 0; g < ngroups; g++) {
        double gnorm = 0.0;
        std::vector<int> grp_tmp = grp_idx[g];
        
        for(std::vector<int>::iterator it = std::begin(grp_tmp); it != std::end(grp_tmp); ++it) {
          // Rcpp::Rcout << *it << ", ";
          gnorm += norms(*it) * norms(*it);
        }
        gnorm_vec(g) = std::sqrt(gnorm);
        // for(std::vector<int>::iterator it = std::begin(grp_tmp); it != std::end(grp_tmp); ++it) {
        //   norms(*it) = gnorm;
        //   // Rcpp::Rcout << gnorm << ", " << norms(*it) << ", ";
        // }
        // Rcpp::Rcout << "\n";
      }
      norms.resize(P);
      norms = gnorm_vec;
    }
    // Rcpp::Rcout << "\n"<< P <<","<<thresh.size() << ", " << penalty_factor.size() << ", " << norms.size() <<"\n";
    // Rcpp::stop("before loop");
    for(int p = 0; p < P; p++) {
      if (penalty_factor(p) != 0) {
        double pen = lambda * penalty_factor(p);
        double val = norms(p);
        if (penalty == "projection.lasso") {
          thresh(p) = soft_threshold_norm(val, pen);
        } else if (penalty == "projection.scad") {
          thresh(p) = soft_threshold_scad_norm(val, pen, denom, gamma);
        } else if (penalty == "projection.mcp") {
          thresh(p) = soft_threshold_mcp_norm(val, pen, denom, gamma);
        } else if ( penalty == "projection.elastic.net") {
          pen *= alpha;
          thresh(p) = soft_threshold_norm(val, pen);
        } else if ( penalty == "projection.scad.net" ){
          pen *= alpha;
          thresh(p) = soft_threshold_scad_norm(val, pen, denom, gamma);
        } else if (penalty == "projection.mcp.net") {
          pen *= alpha;
          thresh(p) = soft_threshold_mcp_norm(val, pen, denom, gamma);
        }
        // Rcpp::Rcout << thresh(p) <<", ";
      } else {
        thresh(p) = 1.0;
      }
      
    }
    if (found_grp_idx) {
      vector thresh_temp(P);
      for(int i = 0; i <P ; i++) thresh_temp(i) = thresh(i);
      for(int g = 0; g < ngroups; g++){
        double tt_copy = thresh_temp(g);
        std::vector<int> grp_tmp = grp_idx[g];
        for(std::vector<int>::iterator it = std::begin(grp_tmp); it != std::end(grp_tmp); ++it) {
          // Rcpp::Rcout << *it << ", ";
          thresh(*it) =  tt_copy;
        }
      }
      
    }
    // Rcpp::Rcout <<"\n\n";
    // Rcpp::stop("ok through loop");
    beta = u.array().colwise() * thresh.array();
    beta.array() /= denom;
    // Rcpp::Rcout << u(7,0) << "," <<  thresh(7,0) << "," <<  thresh(17,0) << beta(7,0)<< "\n";
  } else {
    Rcpp::stop("Penalty factor not found!");
  }
}

void oemXTX_gen::init_oem()
{
  scale_len = scale_factor.size();
  
  found_grp_idx = false;
  
  if (scale_len)
  {
    scale_factor_inv = 1.0 / scale_factor.array();
    XY = XY_init.array().colwise() * scale_factor_inv.array();
  } else
  {
    XY = XY_init;
  }
  // compute XtX or XXt (depending on if n > p or not)
  // and compute A = dI - XtX (if n > p)
  compute_XtX_d_update_A();
}

double oemXTX_gen::compute_lambda_zero(std::string penalty_)
{ //maybe change this
  // lambda0 = XY.cwiseAbs().maxCoeff();
  int temp_size = XY.rows();
  vector temp(temp_size);
  
  if (!found_grp_idx) {
    penalty = penalty_;
    get_group_indexes();
  }
  
  if (XY.cols() > 1 && found_grp_idx) {
    vector xy_temp = XY.rowwise().squaredNorm();
    temp.resize(ngroups);
    temp.fill(0.0);
    
    for ( int g = 0; g < ngroups; g++ ) {
      std::vector<int> gr_idx = grp_idx[g];
      for ( int i = 0; i < gr_idx.size(); i++ ) {
        temp(g) += xy_temp(g);
      }
    }
    
    temp = temp.cwiseSqrt().eval();
    temp = temp.cwiseQuotient(group_weights);
  } else if ( XY.cols() > 1 ) {
    temp = XY.rowwise().norm();
    temp = temp.cwiseQuotient(penalty_factor);
  }  else if (found_grp_idx) {
    temp.resize(ngroups);
    temp.fill(0.0);
    
    
    for ( int g = 0; g < ngroups; g++ ) {
      std::vector<int> gr_idx = grp_idx[g];
      for ( int i = 0; i < gr_idx.size(); i++ ) {
        double val = XY(gr_idx[i]);
        temp(g) += val * val;
      }
    }
    temp = temp.cwiseSqrt().eval();
    temp = temp.cwiseQuotient(group_weights);
  } else {
    temp = XY.cwiseQuotient(penalty_factor).cwiseAbs().eval();
  }
  if ( penalty_factor.cwiseEqual(0.0).any() ) {
    for ( int i = 0; i < temp_size; i ++ ) {
      if ( penalty_factor(i) < Eigen::NumTraits<double>::dummy_precision() ) temp(i) = 0.0;
    }
  }
  lambda0 = temp.maxCoeff();
  
  return lambda0;
}

void oemXTX_gen::init(double lambda_, std::string penalty_,
          double alpha_, double gamma_, double tau_)
{
  beta.setZero();
  
  lambda = lambda_;
  penalty = penalty_;
  
  alpha = alpha_;
  gamma = gamma_;
  tau   = tau_;
  
  std::string projectionstxt("projection.");
  is_projection = penalty.find(projectionstxt) != std::string::npos;
  
  // get indexes of members of each group.
  // best to do just once in the beginning
  if (!found_grp_idx)
  {
    get_group_indexes();
  }
  // Rcpp::Rcout << penalty <<"\n";
  if(found_grp_idx && is_projection) {
    penalty.erase(11,4);
    penalty_factor.resize(ngroups);
    for(int i = 0; i < ngroups; i ++) penalty_factor(i) = group_weights(i);
  }
  // Rcpp::Rcout << penalty <<"\n";
}
