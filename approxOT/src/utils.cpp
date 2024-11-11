#include "utils.h"

double threshold(double num) 
{
  return num > 0 ? num : 0;
}

// computes cumulative sum of vector x
VectorXd cumsum(const VectorXd& x) {
  const int n(x.size());
  VectorXd cmsm(n);
  //cmsm = std::partial_sum(x.data(), x.data() + x.size(), cmsm.data(), std::plus<double>());
  cmsm(0) = x(0);
  
  for (int i = 1; i < n; i++) {
    cmsm(i) = cmsm(i-1) + x(i);
  }
  return (cmsm);
}

// computes reverse cumulative sum of vector x
VectorXd cumsumrev(const VectorXd& x) {
  const int n(x.size());
  VectorXd cmsm(n);
  //std::reverse(x.data(), x.data() + x.size());
  //cmsm = std::partial_sum(x.data(), x.data() + x.size(), cmsm.data(), std::plus<double>());
  cmsm(0) = x(n-1);
  //double tmpsum = 0;
  
  for (int i = 1; i < n; i++) {
    //tmpsum += cmsm(i-1);
    cmsm(i) = cmsm(i-1) + x(n-i-1);
  }
  std::reverse(cmsm.data(), cmsm.data() + cmsm.size());
  return (cmsm);
}

VectorXd sliced_matvecprod(const MatrixXd& A, const VectorXd& b, const std::vector<int>& idx)
{
  const int nn(A.rows());
  const int rr(idx.size());
  VectorXd retvec(nn);
  retvec.setZero();
  
  
  for (int cl = 0; cl < rr; ++cl)
  {
    for (int r = 0; r < nn; ++r)
    {
      retvec(r) += A(r, idx[cl] - 1) * b( idx[cl] - 1 );
    }
  }
  return(retvec);
}

// computes X[,idx]'y
VectorXd sliced_crossprod(const MatrixXd& X, const VectorXd& y, const VectorXi& idx)
{
  const int rr(idx.size());
  VectorXd retvec(rr);
  
  for (int cl = 0; cl < rr; ++cl)
  {
    retvec(cl) = X.col(idx(cl)).dot(y);
  }
  return(retvec);
}

// computes X[,idx]'y
void sliced_crossprod_inplace(VectorXd &res, const MatrixXd& X, const VectorXd& y, const std::vector<int>& idx)
{
  const int rr(idx.size());
  //VectorXd retvec(rr);
  res.setZero();
  
  for (int cl = 0; cl < rr; ++cl)
  {
    res(idx[cl]) = X.col(idx[cl]).dot(y);
  }
  
}
/*
void soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty)
{
  int v_size = vec.size();
  res.setZero();
  res.reserve(v_size);
  
  const double *ptr = vec.data();
  for(int i = 0; i < v_size; i++)
  {
    if(ptr[i] > penalty)
      res.insertBack(i) = ptr[i] - penalty;
    else if(ptr[i] < -penalty)
      res.insertBack(i) = ptr[i] + penalty;
  }
}

void soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty)
{
  int v_size = vec.size();
  res.setZero();
  
  const double *ptr = vec.data();
  for(int i = 0; i < v_size; i++)
  {
    if(ptr[i] > penalty)
      res(i) = ptr[i] - penalty;
    else if(ptr[i] < -penalty)
      res(i) = ptr[i] + penalty;
  }
  
}

void soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact)
{
    int v_size = vec.size();
    res.setZero();
    res.reserve(v_size);
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
        double total_pen = pen_fact(i) * penalty;
        
        if(ptr[i] > total_pen)
            res.insertBack(i) = ptr[i] - total_pen;
        else if(ptr[i] < -total_pen)
            res.insertBack(i) = ptr[i] + total_pen;
    }
}

void soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact, double &d)
{
    int v_size = vec.size();
    res.setZero();
    res.reserve(v_size);
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
        double total_pen = pen_fact(i) * penalty;
        
        if(ptr[i] > total_pen)
            res.insertBack(i) = (ptr[i] - total_pen)/d;
        else if(ptr[i] < -total_pen)
            res.insertBack(i) = (ptr[i] + total_pen)/d;
    }
}

void soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact)
{
    int v_size = vec.size();
    res.setZero();
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
        double total_pen = pen_fact(i) * penalty;
        
        if(ptr[i] > total_pen)
            res(i) = ptr[i] - total_pen;
        else if(ptr[i] < -total_pen)
            res(i) = ptr[i] + total_pen;
    }
}


void soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty, VectorXd &pen_fact, double &d)
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

void soft_threshold_mcp(VectorXd &res, const VectorXd &vec, const double &penalty, 
                        VectorXd &pen_fact, double &d, double &gamma)
{
    int v_size = vec.size();
    res.setZero();
    double gammad = gamma * d;
    double d_minus_gammainv = d - 1 / gamma;
    
    
    const double *ptr = vec.data();
    for(int i = 0; i < v_size; i++)
    {
        double total_pen = pen_fact(i) * penalty;
        
        if (std::abs(ptr[i]) > gammad * total_pen)
            res(i) = ptr[i]/d;
        else if(ptr[i] > total_pen)
            res(i) = (ptr[i] - total_pen)/(d_minus_gammainv);
        else if(ptr[i] < -total_pen)
            res(i) = (ptr[i] + total_pen)/(d_minus_gammainv);
        
    }
     
}

*/

// void update_active_set(VectorXd &u, std::vector<int> &active, std::vector<int> &inactive,
//                        double &lambdak, double &lambdakminus1, const int &penalty)
// {
//   for(std::vector<int>::iterator it = inactive.begin(); it != inactive.end(); ) {
//     // the sequential strong rule
//     // https://statweb.stanford.edu/~tibs/ftp/strong.pdf
//     //std::cout << "var idx: " << *it << std::endl;
//     if (std::abs(u(*it)) >= 2 *lambdak - lambdakminus1){
//       active.push_back(*it);
//       it = inactive.erase(it);
//     } else {
//       ++it;
//     }
//   }
// }
// 
// void initiate_active_set(VectorXd &u, std::vector<int> &active, std::vector<int> &inactive,
//                          double &lambdak, double &lambdamax, const int &nvars, const int &penalty)
// {
//   for (int cl = 0; cl < nvars; ++cl) {
//     // the basic strong rule
//     if (std::abs(u(cl)) >= 2 * lambdak - lambdamax){
//       active.push_back(cl);
//     } else {
//       inactive.push_back(cl);
//     }
//   }
// }
// 
// void block_soft_threshold(SpVec &res, const VectorXd &vec, const double &penalty,
//                                  const int &ngroups, VectorXi &unique_grps, VectorXi &grps)
// {
//   int v_size = vec.size();
//   res.setZero();
//   res.reserve(v_size);
//   
//   for (int g = 0; g < ngroups; ++g) 
//   {
//     double thresh_factor;
//     std::vector<int> gr_idx;
//     for (int v = 0; v < v_size; ++v) 
//     {
//       if (grps(v) == unique_grps(g)) 
//       {
//         gr_idx.push_back(v);
//       }
//     }
//     if (unique_grps(g) == 0) 
//     {
//       thresh_factor = 1;
//     } else 
//     {
//       double ds_norm = 0;
//       for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
//       {
//         int c_idx = gr_idx[v];
//         ds_norm += std::pow(vec(c_idx), 2);
//       }
//       ds_norm = std::sqrt(ds_norm);
//       double grp_wts = std::sqrt(gr_idx.size());
//       thresh_factor = std::max(0.0, 1 - penalty * grp_wts / (ds_norm) );
//     }
//     if (thresh_factor != 0.0)
//     {
//       for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
//       {
//         int c_idx = gr_idx[v];
//         res.insertBack(c_idx) = vec(c_idx) * thresh_factor;
//       }
//     }
//   }
// }
// 
// 
// void block_soft_threshold(VectorXd &res, const VectorXd &vec, const double &penalty,
//                           const int &ngroups, VectorXi &unique_grps, VectorXi &grps)
// {
//   int v_size = vec.size();
//   res.setZero();
//   
//   for (int g = 0; g < ngroups; ++g) 
//   {
//     double thresh_factor;
//     std::vector<int> gr_idx;
//     for (int v = 0; v < v_size; ++v) 
//     {
//       if (grps(v) == unique_grps(g)) 
//       {
//         gr_idx.push_back(v);
//       }
//     }
//     if (unique_grps(g) == 0) 
//     {
//       thresh_factor = 1;
//     } else 
//     {
//       double ds_norm = 0;
//       for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
//       {
//         int c_idx = gr_idx[v];
//         ds_norm += std::pow(vec(c_idx), 2);
//       }
//       ds_norm = std::sqrt(ds_norm);
//       double grp_wts = std::sqrt(gr_idx.size());
//       thresh_factor = std::max(0.0, 1 - penalty * grp_wts / (ds_norm) );
//     }
//     if (thresh_factor != 0.0)
//     {
//       for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
//       {
//         int c_idx = gr_idx[v];
//         res(c_idx) = vec(c_idx) * thresh_factor;
//       }
//     }
//   }
// }


/*
static void block_soft_threshold(SparseVector &res, const VectorXd &vec, const double &penalty,
                                 const int &ngroups, const MapVeci &unique_grps, const MapVeci &grps)
{
  int v_size = vec.size();
  res.setZero();
  res.reserve(v_size);
  
  for (int g = 0; g < ngroups; ++g) 
  {
    double thresh_factor;
    std::vector<int> gr_idx;
    for (int v = 0; v < v_size; ++v) 
    {
      if (grps(v) == unique_grps(g)) 
      {
        gr_idx.push_back(v);
      }
    }
    if (unique_grps(g) == 0) 
    {
      thresh_factor = 1;
    } else 
    {
      double ds_norm = 0;
      for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
      {
        int c_idx = gr_idx[v];
        ds_norm += pow(vec(c_idx), 2);
      }
      ds_norm = sqrt(ds_norm);
      double grp_wts = sqrt(gr_idx.size());
      thresh_factor = std::max(0.0, 1 - penalty * grp_wts / (ds_norm) );
    }
    if (thresh_factor != 0.0)
    {
      for (std::vector<int>::size_type v = 0; v < gr_idx.size(); ++v)
      {
        int c_idx = gr_idx[v];
        res.insertBack(c_idx) = vec(c_idx) * thresh_factor;
      }
    }
  }
}
 */

//computes X'WX where W is diagonal (input w as vector)
MatrixXd XtWX(const MapMatd& xx, const MatrixXd& ww) {
    const int n(xx.cols());
    MatrixXd AtWA(MatrixXd(n, n).setZero().
                      selfadjointView<Lower>().rankUpdate(xx.adjoint() * ww.array().sqrt().matrix().asDiagonal()));
    return (AtWA);
}

//computes X'WX where W is diagonal (input w as vector)
MatrixXd XWXt(const MapMatd& xx, const MatrixXd& ww) {
    const int n(xx.rows());
    MatrixXd AWAt(MatrixXd(n, n).setZero().
                      selfadjointView<Lower>().rankUpdate(xx * ww.array().sqrt().matrix().asDiagonal()));
    return (AWAt);
}

/*
//computes X'X
MatrixXd XtX(const MapMatd &xx) {
    const int n(xx.cols());
    MatrixXd AtA(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx.adjoint()));
    return (AtA);
}


//computes X'X
MatrixXd XtX(MapMat &xx) {
    const int n(xx.cols());
    MatrixXd AtA(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx.adjoint()));
    return (AtA);
}
*/
//computes X'X
MatrixXd XtX(const MatrixXd &xx) {
    const int n(xx.cols());
    MatrixXd AtA(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx.adjoint()));
    return (AtA);
}

void XtX(MatrixXd &xTx, const MatrixXd &xx) {
    xTx.setZero().selfadjointView<Lower>().rankUpdate(xx.adjoint());
}


/*
//computes X'X
MatrixXd XtX(MatrixXd &xx) {
    const int n(xx.cols());
    MatrixXd AtA(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx.adjoint()));
    return (AtA);
}


//computes XX'
MatrixXd XXt(const MapMatd& xx) {
    const int n(xx.rows());
    MatrixXd AAt(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx));
    return (AAt);
}


//computes XX'
MatrixXd XXt(MapMat& xx) {
    const int n(xx.rows());
    MatrixXd AAt(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx));
    return (AAt);
}

//computes XX'
MatrixXd XXt(const MapMat& xx) {
    const int n(xx.rows());
    MatrixXd AAt(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx));
    return (AAt);
}
*/

//computes XX'
MatrixXd XXt(const MapMat& xx) {
    const int n(xx.rows());
    MatrixXd AAt(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx));
    return (AAt);
}

/*

//computes XX'
MatrixXd XXt(MatrixXd& xx) {
    const int n(xx.rows());
    MatrixXd AAt(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(xx));
    return (AAt);
}
 
 */


MatrixXd XtX_scaled(const MapMatd &xx, RowVectorXd &colmeans, RowVectorXd &colstd) {
    const int p(xx.cols());
    // this currently induces a copy, need to fix if possible
    MatrixXd AtA(MatrixXd(p, p).setZero().
                     selfadjointView<Lower>().rankUpdate(((xx.rowwise() - colmeans).array().rowwise() / 
                     colstd.array()).array().matrix().adjoint() ));
    
    return (AtA);
}


MatrixXd XXt_scaled(const MapMatd &xx, RowVectorXd &colmeans, RowVectorXd &colstd) {
    const int n(xx.rows());
    // this currently induces a copy, need to fix if possible
    MatrixXd AAt(MatrixXd(n, n).setZero().
                     selfadjointView<Lower>().rankUpdate(((xx.rowwise() - colmeans).array().rowwise() / 
                     colstd.array()).matrix() ));
    
    return (AAt);
}



//computes X'X
SpMat XtX(const MSpMat& xx) {
    const int n(xx.cols());
    SpMat AtA(SpMat(n, n).selfadjointView<Upper>().rankUpdate(xx.adjoint()));
    return (AtA);
}

//computes XX'
SpMat XXt(const MSpMat& xx) {
    const int n(xx.rows());
    SpMat AAt(SpMat(n, n).selfadjointView<Upper>().rankUpdate(xx));
    return (AAt);
}

//computes X'WX where W is diagonal (input w as vector)
SpMat XtWX(const MSpMat& xx, const MatrixXd& ww) {
    const int n(xx.cols());
    SpMat AtWA(SpMat(n, n).
                   selfadjointView<Lower>().rankUpdate(xx.adjoint() * ww.array().sqrt().matrix().asDiagonal()));
    return (AtWA);
}

//computes X'WX where W is diagonal (input w as vector)
SpMat XWXt(const MSpMat& xx, const MatrixXd& ww) {
    const int n(xx.rows());
    SpMat AWAt(SpMat(n, n).
                   selfadjointView<Lower>().rankUpdate(xx * ww.array().sqrt().matrix().asDiagonal()));
    return (AWAt);
}


bool stopRule(const VectorXd& cur, const VectorXd& prev, const double& tolerance) {
  for (unsigned i = 0; i < cur.rows(); i++) {
    if ( (std::abs(cur(i)) > 1e-13 && std::abs(prev(i)) <= 1e-13) || 
         (std::abs(cur(i)) <= 1e-13 && std::abs(prev(i)) > 1e-13) ) {
      return 0;
    }
    if (std::abs(cur(i)) > 1e-13 && std::abs(prev(i)) > 1e-13 && 
        std::abs( (cur(i) - prev(i)) / prev(i)) > tolerance) {
  	  return 0;
    }
  }
  return 1;
}

bool stopRule(const SpVec& cur, const SpVec& prev, const double& tolerance) {
  
  
  SpVec diff = cur - prev;
    
  for(SpVec::InnerIterator iter(diff); iter; ++iter)
  {
    double prevval = prev.coeff(iter.index());
    double curval  = cur.coeff(iter.index());
    
    if ( (curval != 0 && prevval == 0) || (curval == 0 && prevval != 0) ) {
      return 0;
    }
    
    if (prevval != 0 && curval != 0 && 
        std::abs(iter.value() / prevval) > tolerance)
    {
      return 0;
    }
  }
  return 1;
}

bool stopRuleMat(const MatrixXd& cur, const MatrixXd& prev, const double& tolerance) {
  for (unsigned j = 0; j < cur.cols(); j++) {
    for (unsigned i = 0; i < cur.rows(); i++) {
      // if (j >= prev.cols() ) stop("Out of bounds in column");
      // if (i >= prev.rows() ) stop("Out of bounds in row");
      if ( (cur(i, j) != 0.0 && prev(i, j) == 0.0) 
        || (cur(i, j) == 0.0 && prev(i, j) != 0.0) ) {
        return false;
      }
      if (cur(i, j) != 0.0 && prev(i, j) != 0.0 && std::abs( (cur(i, j) - prev(i, j)) / prev(i, j)) > tolerance) {
    	  return false;
      }
    }
  }
  return true;
}

// bool nonZero(const VectorXd & v) {
//   if( (v.cwiseAbs().array() >= Eigen::NumTraits<double>::dummy_precision()).any()) return 1;
//   // for(int i = 0; i < v.rows(); i ++) {
//   //   if(std::fabs(v(i)) >= Eigen::NumTraits<double>::dummy_precision() ) return 1;
//   // }
//   
//   return 0;
// }

bool nonZero(const refMatConst & v) {
  if( (v.cwiseAbs().array() >= Eigen::NumTraits<double>::dummy_precision()).any()) return 1;
  // for(int i = 0; i < v.rows(); i ++) {
  //   if(std::fabs(v(i)) >= Eigen::NumTraits<double>::dummy_precision() ) return 1;
  // }
  
  return 0;
}

int countNonZero(const refMatConst & v) {
  // Rcpp::Rcout << v.cwiseAbs().rowwise().sum() << std::endl;
  // Rcpp::Rcout << ( v.cwiseAbs().rowwise().sum().array() >= Eigen::NumTraits<double>::dummy_precision()) << std::endl;
  // Rcpp::Rcout << ( v.cwiseAbs().rowwise().sum().array() >= Eigen::NumTraits<double>::dummy_precision()).count();
  return ( ( v.cwiseAbs().rowwise().sum().array() >= Eigen::NumTraits<double>::dummy_precision()).count() );
}

bool nonFinite(const refMatConst & v) {
  if( (v.cwiseAbs().array() >= Eigen::NumTraits<double>::highest()).any()) return 1;
  // for(int i = 0; i < v.rows(); i ++) {
  //   if(std::fabs(v(i)) >= Eigen::NumTraits<double>::dummy_precision() ) return 1;
  // }
  
  return 0;
}

bool nonFiniteDist(const refMatConst & v) {
  if( (v.colwise().squaredNorm().array() >= Eigen::NumTraits<double>::highest()).any()) return 1;
  // for(int i = 0; i < v.rows(); i ++) {
  //   if(std::fabs(v(i)) >= Eigen::NumTraits<double>::dummy_precision() ) return 1;
  // }
  
  return 0;
}

int compare(const MatrixXi & a, const MatrixXi & b, VectorXi & idx_col){
  VectorXi temp = a.cwiseNotEqual(b).colwise().any().cast<int>();
  
  if ((temp.array() > 0).any()) {
    int count = 0;
    for(int i = 0; i < temp.size(); i++){
      if(temp(i)) {
        idx_col(count) = i;
        count++;
        // Rcpp::Rcout << i <<", ";
      }
    }
  }
  // Rcpp::Rcout << std::endl;
  return(temp.sum());
}

void mu_update(const refMatConst & X, 
               const refMatConst & result, 
               const refMatConst & theta, 
               matrix & mu,
               const Rcpp::CharacterVector & method) {
  if( (method(0) == "scale") || (method(0) == "selection.variable")) {
    mu = X.transpose() * result.asDiagonal() * theta;
  } else if (method(0) == "location.scale") {
    //pull data from theta in to centered and mean matrix
    int P = theta.rows()/2;
    int S = theta.cols();
    vector c_res = result.block(0,0,P,1);
    vector m_res = result.block(P,0,P,1);
    matrix c_theta = theta.block(0,0,P,S);
    matrix theta_mean = theta.block(P,0,P,S);
    
    //calculate mu
    mu = c_theta.transpose() * c_res.asDiagonal() * X + 
      theta_mean.transpose() * m_res.asDiagonal() * X;
  } else if (method(0) == "projection") {
    mu = X.transpose() * result;
  } else {
    Rcpp::stop("Method not found in updating mu!");
  }
}


//computes X'WX where W is diagonal (input w as vector)
/*SparseMatrix<double> XtWX_sparse(const SparseMatrix<double>& xx, const MatrixXd& ww) {
  const int n(xx.cols());
  SparseMatrix<double> AtWA(n, n);
  AtWA = AtWA.selfadjointView<Lower>().rankUpdate(xx.adjoint() * ww.asDiagonal());
  return (AtWA);
}*/

matrix covariance(const refMatConst & samples, const refVecConst & mean) {
  int S = samples.cols();
  int d = samples.rows();
  matrix c_samples(d, S);
  
  if(d != mean.rows()) Rcpp::stop("Dimension of mean vector not match dimension of samples vector!");
  for(int i = 0 ; i < S; i++){
    c_samples.col(i) = samples.col(i) - mean;
  }
  return matrix(d, d).setZero().selfadjointView<Eigen::Lower>().rankUpdate(c_samples);
}

matrix covariance(const refMatConst & samples) {
  int S = samples.cols();
  int d = samples.rows();
  matrix c_samples(d, S);
  vector mean = samples.colwise().mean();
  
  if(d != mean.rows()) Rcpp::stop("Dimension of mean vector not match dimension of samples vector!");
  for(int i = 0 ; i < S; i++){
    c_samples.col(i) = samples.col(i) - mean;
  }
  return matrix(d, d).setZero().selfadjointView<Eigen::Lower>().rankUpdate(c_samples);
}

void which(const matrixI & basis, int N, int M, matrixI & index) { //check which function is working
  if ( (N*M) != index.rows() ) Rcpp::stop("Index matrix rows don't match number of possible assignments");
  int count = 0;
  // Rcpp::Rcout << index(0,0) << std::endl;
  // Rcpp::Rcout << index(0,1) << std::endl;
  if (basis.rows() != (N) ) Rcpp::stop("Basis matrix rows don't match cost matrix rows");
  if (basis.cols() != (M) ) Rcpp::stop("Basis matrix columns don't match cost matrix cols");
  
  for (int j = 0; j < M; j++) {
    for (int i = 0; i < N; i ++) {
      if(basis(i,j) == 1 ) { 
        index(count, 0) = i;
        index(count, 1) = j;
        count++;
      }
    }
  }
  if(count == 0) Rcpp::stop("No matchings found!");
  index.conservativeResize( count,	Eigen::NoChange );
}

void which_nonzero(const matrix & basis, int N, int M, matrixI & index) { //check which function is working
  if ( (N*M) != index.rows() ) Rcpp::stop("Index matrix rows don't match number of possible assignments");
  int count = 0;
  // Rcpp::Rcout << index(0,0) << std::endl;
  // Rcpp::Rcout << index(0,1) << std::endl;
  if (basis.rows() != (N) ) Rcpp::stop("Assignment matrix rows don't match cost matrix rows");
  if (basis.cols() != (M) ) Rcpp::stop("Assignment matrix columns don't match cost matrix cols");
  
  for (int j = 0; j < M; j++) {
    for (int i = 0; i < N; i ++) {
      if(basis(i,j) != 0.0 ) { 
        index(count, 0) = i;
        index(count, 1) = j;
        count++;
      }
    }
  }
  if(count == 0) Rcpp::stop("No matchings found!");
  index.conservativeResize( count,	Eigen::NoChange );
}
// template <typename Derived>
// double median(const Eigen::EigenBase<Derived>& X) {
//   vecMapConst x(X.data());
//   int N = x.size();
//   double med;
//   
//   if (N == 0){
//     Rcpp::stop("vector/matrix taking median of has length 0")
//   }
//   int half = (N + 1)/2;
//   if (N % 2 == 1){
//     vector x_sort = sort_partial(x, half-1);
//     med = x_sort;
//   } else {
//     vector x_sort = sort_partial(x, half);
//     med = 0.5 * ( x_sort(half) + x_sort(half - 1) );
//   }
//   return(med);
// }

double sinkhorn_converge(const vector & u, const vector & u_old) {
  vector f = u.array().log();
  vector f_old = u_old.array().log();
  vector diff = (f - f_old).array().abs();
  double out = (diff.array()/ f_old.array().abs()).sum();
  
  return(out);
}

double sinkhorn_converge_log(const vector & f, const vector & f_old) {
  vector diff = (f - f_old).array().abs();
  double out = (diff.array()/ f_old.array().abs()).sum();
  
  return(out);
}

double dist_approx_ot(const refVecConst & mass_a, const refVecConst & mass_b,
                const vector & r, const vector & c, int p) {
  vector rdiff = r - mass_a;
  vector cdiff = c - mass_b;
  double out = 0.0;
  if(p == 2) {
    out = rdiff.norm() + cdiff.norm();
  } else if ( p == 1) {
    out = rdiff.lpNorm<1>() + cdiff.lpNorm<1>();
  } else {
    Rcpp::stop("Other norms not supported");
  }
  return (out);
}

double rho_ot(const vector & a, const vector & b) {
  return ( (b  - a).sum() +  (a.array() *( a.array().log() - b.array().log() )).sum());
}

vector rho_vec(const vector & a, const vector & b) {
  return ( b.array()  - a.array() +  a.array() * ( a.array().log() - b.array().log() ) );
}

double rho(double a, double b) {
  return(b  - a + a * (std::log(a) - std::log(b)));
}

double f_randk(const refVecConst & mass_a, const refVecConst & mass_b, const matrix & exp_cost,
         vector & u, vector & v) {
  return ( ((u.array().exp()).matrix().asDiagonal() * exp_cost * (v.array().exp()).matrix().asDiagonal() ).sum() - 
           u.dot(mass_a) - v.dot(mass_b) );
}

void argmin_f(const refVecConst & mass_a, const refVecConst & mass_b, const matrix & exp_cost,
              vector & u, vector & v, vector & y_u, vector & y_v, vector & u_hat, vector & v_hat) 
{
  double f_hat = f_randk(mass_a, mass_b, exp_cost, u_hat, v_hat);
  double f_y = f_randk(mass_a, mass_b, exp_cost, y_u, y_v);
  
  if (f_hat <= f_y) {
    u = u_hat;
    v = v_hat;
  } else {
    u = y_u;
    v = y_v;
  }
}


// from https://stackoverflow.com/questions/1719070/what-is-the-right-approach-when-using-stl-container-for-median-calculation
// template <class RandAccessIter>
// double median(RandAccessIter begin, RandAccessIter end) {
//   if ( begin == end ) {
//     Rcpp:stop("Can't take the median of an empty list.");
//   }
//   std::size_t size = end - begin;
//   std::size_t middleIdx = size/2;
//   RandAccessIter target = begin + middleIdx;
//   std::nth_element(begin, target, end);
//   double a = *target;
//   //
//   if (size % 2 != 0) { //Odd number of elements
//     return (a);
//   } else {            //Even number of elements
//     RandAccessIter targetNeighbor= target-1;
//     std::nth_element(begin, targetNeighbor, end);
//     double an = *targetNeighbor;
//     return ( (a + an)/2.0);
//   }
//   return(0.0);
// }

double median(refMat A) {
  if ( A.size() == 0) {
    Rcpp::stop("Can't take the median of an empty matrix.");
  }
  int size = A.size();
  int middleIdx = size/2;
  matrix A_copy = A;
  double * begin = A_copy.data();
  double * end = begin + size;
  double * target = begin + middleIdx;
  std::nth_element( begin, target, end);
  double a = *target;
  //
  if (size % 2 != 0) { //Odd number of elements
    return (a);
  } else {            //Even number of elements
    double * targetNeighbor = target-1;
    std::nth_element(begin, targetNeighbor, end);
    double an = *targetNeighbor;
    return ( (a + an)/2.0);
  }
  // return(0.0);
}

double median(const matrix & A) {
  if ( A.size() == 0) {
    Rcpp::stop("Can't take the median of an empty matrix.");
  }
  int size = A.size();
  int middleIdx = size/2;
  matrix A_copy = A;
  double * begin = A_copy.data();
  double * end = begin + size;
  double * target = begin + middleIdx;
  std::nth_element( begin, target, end);
  double a = *target;
  //
  if (size % 2 != 0) { //Odd number of elements
    return (a);
  } else {            //Even number of elements
    double * targetNeighbor = target-1;
    std::nth_element(begin, targetNeighbor, end);
    double an = *targetNeighbor;
    return ( (a + an)/2.0);
  }
  // return(0.0);
}


double median( matrix & A) {
  if ( A.size() == 0) {
    Rcpp::stop("Can't take the median of an empty matrix.");
  }
  int size = A.size();
  int middleIdx = size/2;
  matrix A_copy = A;
  double * begin = A_copy.data();
  double * end = begin + size;
  double * target = begin + middleIdx;
  std::nth_element( begin, target, end);
  double a = *target;
  //
  if (size % 2 != 0) { //Odd number of elements
    return (a);
  } else {            //Even number of elements
    double * targetNeighbor = target-1;
    std::nth_element(begin, targetNeighbor, end);
    double an = *targetNeighbor;
    return ( (a + an)/2.0);
  }
  // return(0.0);
}


