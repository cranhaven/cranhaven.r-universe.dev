#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::Vector3d;

//' Compute Normal Vectors Using Eigen Decomposition (C++ Implementation)
//'
//' Fast C++ implementation for computing normal vectors from point cloud data
//' using eigenvalue decomposition. This function is called internally by
//' \code{add_normals} and should not typically be called directly.
//'
//' @param coords Matrix of point coordinates (n x 3: X, Y, Z)
//' @param neighbors Matrix of neighbor indices (n x k) - 1-based indexing
//' @param always_up Logical, ensure normals point upward (positive Z)
//' @param num_threads Number of OpenMP threads to use (0 = auto-detect)
//' @return Matrix of normal vectors (n x 3)
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix compute_normals_cpp(
    const Eigen::Map<Eigen::MatrixXd> coords,
    const Eigen::Map<Eigen::MatrixXd> neighbors,
    bool always_up = true,
    int num_threads = 0
) {
  const int n_points = static_cast<int>(neighbors.rows());
  const int k         = static_cast<int>(neighbors.cols());
  
  // Set number of threads
#ifdef _OPENMP
  if (num_threads > 0) {
    omp_set_num_threads(num_threads);
  }
#endif
  
  Rcpp::NumericMatrix normals(n_points, 3);
  
  // Parallel computation with OpenMP
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i = 0; i < n_points; i++) {
    // Count valid neighbors
    int valid_count = 0;
    for (int j = 0; j < k; j++) {
      if (neighbors(i, j) > 0) {
        valid_count++;
      }
    }
    
    // Need at least 3 points for PCA
    if (valid_count < 3) {
      normals(i, 0) = 0.0;
      normals(i, 1) = 0.0;
      normals(i, 2) = 1.0;
      continue;
    }
    
    // Gather neighbor coordinates
    MatrixXd pts(valid_count, 3);
    int pt_idx = 0;
    for (int j = 0; j < k; j++) {
      int idx = static_cast<int>(neighbors(i, j));
      if (idx > 0) {
        // Convert from 1-based (R) to 0-based (C++) indexing
        idx = idx - 1;
        pts(pt_idx, 0) = coords(idx, 0);
        pts(pt_idx, 1) = coords(idx, 1);
        pts(pt_idx, 2) = coords(idx, 2);
        pt_idx++;
      }
    }
    
    // Compute centroid
    Vector3d centroid = pts.colwise().mean();
    
    // Center the points
    MatrixXd centered = pts.rowwise() - centroid.transpose();
    
    // Compute covariance matrix
    MatrixXd cov = (centered.transpose() * centered) / static_cast<double>(valid_count - 1);
    
    // Eigen decomposition (Eigen library automatically sorts eigenvalues in ascending order)
    Eigen::SelfAdjointEigenSolver<MatrixXd> es(cov);
    
    // Get normal (eigenvector corresponding to smallest eigenvalue)
    // For SelfAdjointEigenSolver, eigenvalues are in ascending order
    Vector3d normal = es.eigenvectors().col(0);
    
    // Ensure normal points upward if requested
    if (always_up && normal(2) < 0.0) {
      normal = -normal;
    }
    
    normals(i, 0) = normal(0);
    normals(i, 1) = normal(1);
    normals(i, 2) = normal(2);
  }
  
  return normals;
}


//' Compute Normal Vectors and Geometric Features (C++ Implementation)
//'
//' Fast C++ implementation for computing normal vectors and eigenvalue-based
//' geometric features from point cloud data. Features include linearity,
//' planarity, sphericity, and curvature.
//'
//' @param coords Matrix of point coordinates (n x 3: X, Y, Z)
//' @param neighbors Matrix of neighbor indices (n x k) - 1-based indexing
//' @param always_up Logical, ensure normals point upward (positive Z)
//' @param num_threads Number of OpenMP threads to use (0 = auto-detect)
//' @return List with normal vectors (n x 3) and features (n x 4)
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List compute_normals_with_features_cpp(
    const Eigen::Map<Eigen::MatrixXd> coords,
    const Eigen::Map<Eigen::MatrixXd> neighbors,
    bool always_up = true,
    int num_threads = 0
) {
  const int n_points = static_cast<int>(neighbors.rows());
  const int k         = static_cast<int>(neighbors.cols());
  
  // Set number of threads
#ifdef _OPENMP
  if (num_threads > 0) {
    omp_set_num_threads(num_threads);
  }
#endif
  
  Rcpp::NumericMatrix normals(n_points, 3);
  Rcpp::NumericMatrix features(n_points, 4);
  
  // Parallel computation with OpenMP
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int i = 0; i < n_points; i++) {
    // Count valid neighbors
    int valid_count = 0;
    for (int j = 0; j < k; j++) {
      if (neighbors(i, j) > 0) {
        valid_count++;
      }
    }
    
    // Need at least 3 points for PCA
    if (valid_count < 3) {
      normals(i, 0) = 0.0;
      normals(i, 1) = 0.0;
      normals(i, 2) = 1.0;
      features(i, 0) = 0.0;  // linearity
      features(i, 1) = 0.0;  // planarity
      features(i, 2) = 0.0;  // sphericity
      features(i, 3) = 0.0;  // curvature
      continue;
    }
    
    // Gather neighbor coordinates
    MatrixXd pts(valid_count, 3);
    int pt_idx = 0;
    for (int j = 0; j < k; j++) {
      int idx = static_cast<int>(neighbors(i, j));
      if (idx > 0) {
        // Convert from 1-based (R) to 0-based (C++) indexing
        idx = idx - 1;
        pts(pt_idx, 0) = coords(idx, 0);
        pts(pt_idx, 1) = coords(idx, 1);
        pts(pt_idx, 2) = coords(idx, 2);
        pt_idx++;
      }
    }
    
    // Compute centroid
    Vector3d centroid = pts.colwise().mean();
    
    // Center the points
    MatrixXd centered = pts.rowwise() - centroid.transpose();
    
    // Compute covariance matrix
    MatrixXd cov = (centered.transpose() * centered) / static_cast<double>(valid_count - 1);
    
    // Eigen decomposition
    Eigen::SelfAdjointEigenSolver<MatrixXd> es(cov);
    VectorXd eigenvalues = es.eigenvalues();  // Ascending order
    
    // Get normal (eigenvector corresponding to smallest eigenvalue)
    Vector3d normal = es.eigenvectors().col(0);
    
    // Ensure normal points upward if requested
    if (always_up && normal(2) < 0.0) {
      normal = -normal;
    }
    
    normals(i, 0) = normal(0);
    normals(i, 1) = normal(1);
    normals(i, 2) = normal(2);
    
    // Compute geometric features
    // Eigenvalues are in ascending order: λ3 <= λ2 <= λ1
    double lambda1 = eigenvalues(2);  // Largest
    double lambda2 = eigenvalues(1);  // Middle
    double lambda3 = eigenvalues(0);  // Smallest
    double sum_eig = lambda1 + lambda2 + lambda3;
    
    if (lambda1 > 1e-10 && sum_eig > 1e-10) {
      features(i, 0) = (lambda1 - lambda2) / lambda1;  // Linearity
      features(i, 1) = (lambda2 - lambda3) / lambda1;  // Planarity
      features(i, 2) = lambda3 / lambda1;              // Sphericity
      features(i, 3) = lambda3 / sum_eig;              // Curvature
    } else {
      features(i, 0) = 0.0;
      features(i, 1) = 0.0;
      features(i, 2) = 0.0;
      features(i, 3) = 0.0;
    }
  }
  
  return Rcpp::List::create(
    Rcpp::Named("normals") = normals,
    Rcpp::Named("features") = features
  );
}
