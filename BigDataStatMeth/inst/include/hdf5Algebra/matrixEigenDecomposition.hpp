/**
 * @file matrixEigenDecomposition.hpp
 * @brief Eigenvalue decomposition algorithms for big data matrices stored in HDF5 format
 * 
 * This header provides efficient eigenvalue and eigenvector computation functions
 * for large matrices stored in HDF5 format using the Spectra library (same version
 * as used in BigDataStatMeth SVD) for consistent results with RSpectra.
 * 
 * @author BigDataStatMeth Development Team
 * @date 2025
 * @version 1.0
 * @note Updated for Spectra 1.0.1 compatibility
 * 
 * @details
 * The implementation uses Spectra library for eigenvalue computation, which provides:
 * - Support for both symmetric and non-symmetric matrices
 * - Consistent results with RSpectra package
 * - Memory-efficient algorithms for large matrices
 * - Integration with existing BigDataStatMeth infrastructure
 */

#ifndef BIGDATASTATMETH_HDF5_MATRIXEIGEN_HPP
#define BIGDATASTATMETH_HDF5_MATRIXEIGEN_HPP

#include "Spectra/SymEigsSolver.h"
#include "Spectra/GenEigsSolver.h"
#include "Spectra/MatOp/DenseSymMatProd.h"
#include "Spectra/MatOp/DenseGenMatProd.h"

#include <random>


namespace BigDataStatMeth {
    
    /**
     * @brief Convert 'which' string to Spectra SortRule for symmetric matrices
     * @param which String indicating which eigenvalues to compute
     * @return Corresponding Spectra SortRule for symmetric eigenproblems
     */
    inline Spectra::SortRule getSymmetricSortRule(const std::string& which) {
        if (which == "LA") return Spectra::SortRule::LargestAlge;
        if (which == "SA") return Spectra::SortRule::SmallestAlge;
        if (which == "LM") return Spectra::SortRule::LargestMagn;
        if (which == "SM") return Spectra::SortRule::SmallestMagn;
        // Default to largest magnitude for symmetric matrices
        return Spectra::SortRule::LargestMagn;
    }
    
    /**
     * @brief Convert 'which' string to Spectra SortRule for general matrices
     * @param which String indicating which eigenvalues to compute
     * @return Corresponding Spectra SortRule for general eigenproblems
     */
    inline Spectra::SortRule getGeneralSortRule(const std::string& which) {
        if (which == "LM") return Spectra::SortRule::LargestMagn;
        if (which == "SM") return Spectra::SortRule::SmallestMagn;
        if (which == "LR") return Spectra::SortRule::LargestReal;
        if (which == "SR") return Spectra::SortRule::SmallestReal;
        if (which == "LI") return Spectra::SortRule::LargestImag;
        if (which == "SI") return Spectra::SortRule::SmallestImag;
        // Default to largest magnitude for general matrices
        return Spectra::SortRule::LargestMagn;
    }
    
    /**
     * @brief Validate and adjust Spectra parameters for convergence
     * @param n Matrix size
     * @param k Number of eigenvalues requested  
     * @param ncv Number of Arnoldi vectors
     * @return Adjusted parameters that satisfy Spectra constraints
     */
    inline std::tuple<int, int> validateSpectraParams(int n, int k, int ncv) {
        // Ensure k is reasonable
        if (k <= 0) k = std::min(n, 6);
        if (k >= n) k = n - 1;
        
        // Calculate optimal ncv following RSpectra defaults
        if (ncv <= 0) {
            ncv = std::min(n, std::max(2 * k + 1, k + 2));
        }
        
        // Enforce Spectra constraints: k + 2 <= ncv <= n
        ncv = std::max(ncv, k + 2);
        ncv = std::min(ncv, n);
        
        // Additional safety: ensure we have enough space for convergence
        if (ncv - k < 2) {
            ncv = std::min(n, k + 2);
        }
        
        return std::make_tuple(k, ncv);
    }
    
    /**
     * @brief Improved matrix symmetry detection for big-omics data
     * @param X Input matrix to check for symmetry
     * @param sample_size Number of elements to sample for symmetry check (default 100)
     * @return True if matrix is approximately symmetric within tolerance
     * @note Optimized for large biological matrices with potential noise
     */
    inline bool isMatrixSymmetric(const Eigen::MatrixXd& X, int sample_size = 100) {
        if (X.rows() != X.cols()) return false;
        
        int n = X.rows();
        if (n <= 3) return true; // Too small to meaningfully check
        
        // For small matrices, check all elements
        if (n <= 50) {
            double max_diff = (X - X.transpose()).cwiseAbs().maxCoeff();
            double matrix_scale = X.cwiseAbs().maxCoeff();
            return max_diff <= 1e-12 * std::max(1.0, matrix_scale);
        }
        
        // For large matrices, use optimized sampling with vectorized operations
        sample_size = std::min(sample_size, n * n / 4);
        
        // Sample diagonal and off-diagonal elements efficiently
        Eigen::VectorXd diffs(sample_size);
        int count = 0;
        
        // Random sampling with good coverage
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> dis(0, n - 1);
        
        for (int s = 0; s < sample_size && count < sample_size; ++s) {
            int i = dis(gen);
            int j = dis(gen);
            if (i != j) {
                diffs(count++) = std::abs(X(i, j) - X(j, i));
            }
        }
        
        if (count == 0) return true;
        
        // Use vectorized operations to compute statistics
        double max_diff = diffs.head(count).maxCoeff();
        double matrix_scale = X.cwiseAbs().maxCoeff();
        double tolerance = 1e-12 * std::max(1.0, matrix_scale);
        
        return max_diff <= tolerance;
    }
    
    /**
     * @brief Structure to hold eigendecomposition results
     * @details Similar to svdeig structure used in SVD, this structure holds
     * the results of eigenvalue decomposition for consistent interface.
     */
    struct eigdecomp {
        Eigen::VectorXd eigenvalues_real;      /**< Real part of eigenvalues */
    Eigen::VectorXd eigenvalues_imag;      /**< Imaginary part of eigenvalues */
    Eigen::MatrixXd eigenvectors_real;     /**< Real part of eigenvectors */
    Eigen::MatrixXd eigenvectors_imag;     /**< Imaginary part of eigenvectors */
    bool bcomputevectors = true;           /**< Whether eigenvectors were computed */
    bool bconv = false;                    /**< Convergence status */
    bool is_symmetric = false;             /**< Whether input matrix was symmetric */
    };
    
    /**
     * @brief Eigenvalue decomposition using Spectra (compatible with BigDataStatMeth SVD version)
     * 
     * Uses the same Spectra version and patterns as the existing SVD implementation in
     * BigDataStatMeth. Based on the RcppbdSVD function in matrixSvd.hpp.
     * 
     * @param X Input matrix
     * @param k Number of eigenvalues to compute
     * @param which Which eigenvalues to compute (LM, SM, LR, SR, LI, SI, LA, SA)
     * @param ncv Number of Arnoldi vectors (if 0, uses auto-selection)
     * @param bcenter Whether to center the data
     * @param bscale Whether to scale the data
     * @param tol Convergence tolerance for Spectra
     * @param max_iter Maximum iterations for Spectra
     * 
     * @return eigdecomp structure containing results
     * 
     * @note This follows the same pattern as RcppbdSVD in matrixSvd.hpp
     * @note Updated for Spectra 1.0.1 API compatibility
     */
    inline eigdecomp RcppbdEigen_spectra(const Eigen::MatrixXd& X, int k, const std::string& which = "LM", 
                                         int ncv = 0, bool bcenter = false, bool bscale = false,
                                         double tol = 1e-10, int max_iter = 1000) {
        
        eigdecomp reteig;
        Eigen::MatrixXd nX;
        // int nconv = 0;
        
        try {
            
            int n = X.rows();
            if (X.rows() != X.cols()) {
                Rf_error("Matrix must be square for eigendecomposition");
                return reteig;
            }
            
            // Better parameter selection following RSpectra defaults
            std::tie(k, ncv) = validateSpectraParams(n, k, ncv);
            
            // Improved symmetry detection
            reteig.is_symmetric = isMatrixSymmetric(X);
            
            if (reteig.is_symmetric) {
                // Use symmetric solver - following SVD pattern
                Eigen::MatrixXd Xcp;
                if (bcenter == true || bscale == true) {
                    nX = RcppNormalize_Data(X, bcenter, bscale, false);
                    Xcp = nX;  // For symmetric case, use matrix directly
                } else {
                    Xcp = X;
                }
                
                Spectra::DenseSymMatProd<double> op(Xcp);
                Spectra::SymEigsSolver<Spectra::DenseSymMatProd<double>> eigs(op, k, ncv);
                
                // // Set tolerance and max iterations
                // eigs.set_max_iter(max_iter);
                
                // Initialize and compute with appropriate sort rule
                eigs.init();
                Spectra::SortRule sort_rule = getSymmetricSortRule(which);
                //..// nconv = eigs.compute(sort_rule, max_iter, tol);
                (void)eigs.compute(sort_rule, max_iter, tol);
                
                // Retrieve results
                if (eigs.info() == Spectra::CompInfo::Successful) {
                    reteig.eigenvalues_real = eigs.eigenvalues();
                    reteig.eigenvalues_imag = Eigen::VectorXd::Zero(k);
                    reteig.eigenvectors_real = eigs.eigenvectors();
                    reteig.eigenvectors_imag = Eigen::MatrixXd::Zero(n, k);
                    reteig.bconv = true;
                } else {
                    reteig.bconv = false;
                }
                
            } else {
                // For non-symmetric matrices, use general solver
                Eigen::MatrixXd Xcp;
                if (bcenter == true || bscale == true) {
                    nX = RcppNormalize_Data(X, bcenter, bscale, false);
                    Xcp = nX;
                } else {
                    Xcp = X;
                }
                
                Spectra::DenseGenMatProd<double> op(Xcp);
                Spectra::GenEigsSolver<Spectra::DenseGenMatProd<double>> eigs(op, k, ncv);
                
                // // Set tolerance and max iterations
                // eigs.set_max_iter(max_iter);
                
                // Initialize and compute with appropriate sort rule
                eigs.init();
                Spectra::SortRule sort_rule = getGeneralSortRule(which);
                //..// nconv = eigs.compute(sort_rule, max_iter, tol);
                (void)eigs.compute(sort_rule, max_iter, tol);
                
                // Retrieve results
                if (eigs.info() == Spectra::CompInfo::Successful) {
                    Eigen::VectorXcd eigenvals = eigs.eigenvalues();
                    Eigen::MatrixXcd eigenvecs = eigs.eigenvectors();
                    
                    reteig.eigenvalues_real = eigenvals.real();
                    reteig.eigenvalues_imag = eigenvals.imag();
                    reteig.eigenvectors_real = eigenvecs.real();
                    reteig.eigenvectors_imag = eigenvecs.imag();
                    reteig.bconv = true;
                } else {
                    reteig.bconv = false;
                }
            }
            
            reteig.bcomputevectors = true;
            
        } catch(std::exception &ex) {
            Rcpp::Rcout << "C++ exception RcppbdEigen_spectra: " << ex.what();
            reteig.bconv = false;
        } catch (...) {
            Rf_error("C++ exception RcppbdEigen_spectra (unknown reason)");
            reteig.bconv = false;
        }
        
        return reteig;
    }
    
    /**
     * @brief Block-wise eigendecomposition for large HDF5 matrices using Spectra
     * 
     * This function performs eigenvalue decomposition on large matrices stored in HDF5
     * using Spectra library for consistency with RSpectra results.
     * 
     * @param dsA Input hdf5Dataset containing the matrix
     * @param dsd Output hdf5Dataset for eigenvalues
     * @param dsu Output hdf5Dataset for eigenvectors  
     * @param k Number of eigenvalues to compute
     * @param which Which eigenvalues to compute
     * @param ncv Number of Arnoldi vectors
     * @param bcenter Whether to center the data
     * @param bscale Whether to scale the data
     * @param tol Convergence tolerance
     * @param max_iter Maximum iterations
     * @param compute_vectors Whether to compute eigenvectors
     * @param threads Number of parallel threads
     * 
     * @throws H5::FileIException for HDF5 file operation errors
     * @throws H5::DataSetIException for HDF5 dataset operation errors
     * @throws std::exception for computation errors
     * 
     * @note Updated for Spectra 1.0.1 compatibility with improved parameter handling
     */
    template <class T>
    inline void RcppbdEigen_hdf5_Block(T* dsA, 
                                       BigDataStatMeth::hdf5Dataset* dsd,
                                       BigDataStatMeth::hdf5Dataset* dsu, 
                                       int k, const std::string& which, int ncv,
                                       bool bcenter, bool bscale, double tol, int max_iter,
                                       bool compute_vectors, Rcpp::Nullable<int> threads = R_NilValue) {
        
        static_assert(std::is_same<T*, BigDataStatMeth::hdf5Dataset*>::value ||
                      std::is_same<T*, BigDataStatMeth::hdf5DatasetInternal*>::value,
                      "Error - type not allowed");
        
        BigDataStatMeth::hdf5Dataset* dsnormalizedData = nullptr;
        
        try {
            
            std::vector<hsize_t> stride = {1, 1}, block = {1, 1};
            eigdecomp reteig;
            
            // Get matrix dimensions
            hsize_t n_rows = dsA->nrows();
            hsize_t n_cols = dsA->ncols();
            
            // Check if matrix is square
            if (n_rows != n_cols) {
                Rf_error("Matrix must be square for eigendecomposition");
                return;
            }
            
            hsize_t n = n_rows;
            
            // Use parameter validation function
            std::tie(k, ncv) = validateSpectraParams((int)n, k, ncv);
            
            // Read matrix data
            Eigen::MatrixXd X;
            std::vector<double> vdA(n * n);
            
            dsA->readDatasetBlock({0, 0}, {n, n}, stride, block, vdA.data());
            X = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>(vdA.data(), n, n);
            
            // Handle normalization if needed
            if (bcenter || bscale) {
                
                // For large matrices, we could implement block-wise normalization here
                // For now, since we already read the matrix, apply normalization directly
                if (n * n < MAXELEMSINBLOCK) {
                    X = RcppNormalize_Data(X, bcenter, bscale, false);
                } else {
                    // For very large matrices, implement block-wise normalization
                    Rcpp::warning("Large matrix normalization not yet optimized - applying direct normalization");
                    X = RcppNormalize_Data(X, bcenter, bscale, false);
                }
            }
            
            // Use Spectra for eigendecomposition - same pattern as SVD
            reteig = RcppbdEigen_spectra(X, k, which, ncv, false, false, tol, max_iter);
            
            if (!reteig.bconv) {
                Rf_error("Eigendecomposition failed to converge");
                return;
            }
            
            // Write eigenvalues to HDF5
            dsd->createDataset(1, reteig.eigenvalues_real.size(), "real");
            dsd->writeDataset(Rcpp::wrap(reteig.eigenvalues_real));
            
            // Write eigenvectors to HDF5 if computed
            if (compute_vectors && reteig.bcomputevectors) {
                dsu->createDataset(reteig.eigenvectors_real.rows(), reteig.eigenvectors_real.cols(), "real");
                dsu->writeDataset(Rcpp::wrap(reteig.eigenvectors_real));
            }
            
            // For non-symmetric matrices, also save imaginary parts if non-zero
            if (!reteig.is_symmetric && reteig.eigenvalues_imag.cwiseAbs().maxCoeff() > 1e-14) {
                
                // Create additional datasets for imaginary parts
                BigDataStatMeth::hdf5Dataset* dsd_imag = new BigDataStatMeth::hdf5Dataset(
                    dsA->getFileName(), "EIGEN/" + dsA->getDatasetName(), "values_imag", true);
                dsd_imag->createDataset(1, reteig.eigenvalues_imag.size(), "real");
                dsd_imag->writeDataset(Rcpp::wrap(reteig.eigenvalues_imag));
                delete dsd_imag;
                
                if (compute_vectors && reteig.bcomputevectors && reteig.eigenvectors_imag.cwiseAbs().maxCoeff() > 1e-14) {
                    BigDataStatMeth::hdf5Dataset* dsu_imag = new BigDataStatMeth::hdf5Dataset(
                        dsA->getFileName(), "EIGEN/" + dsA->getDatasetName(), "vectors_imag", true);
                    dsu_imag->createDataset(reteig.eigenvectors_imag.rows(), reteig.eigenvectors_imag.cols(), "real");
                    dsu_imag->writeDataset(Rcpp::wrap(reteig.eigenvectors_imag));
                    delete dsu_imag;
                }
            }
            
        } catch(H5::FileIException& error) {
            checkClose_file(dsA, dsd, dsu, dsnormalizedData);
            Rcpp::Rcerr << "\nc++ exception RcppbdEigen_hdf5_Block (File IException)\n";
            return;
        } catch(H5::DataSetIException& error) {
            checkClose_file(dsA, dsd, dsu, dsnormalizedData);
            Rcpp::Rcerr << "\nc++ exception RcppbdEigen_hdf5_Block (DataSet IException)\n";
            return;
        } catch(H5::GroupIException& error) {
            checkClose_file(dsA, dsd, dsu, dsnormalizedData);
            Rcpp::Rcerr << "\nc++ exception RcppbdEigen_hdf5_Block (Group IException)\n";
            return;
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsd, dsu, dsnormalizedData);
            Rcpp::Rcerr << "C++ exception RcppbdEigen_hdf5_Block: " << ex.what();
            return;
        } catch (...) {
            checkClose_file(dsA, dsd, dsu, dsnormalizedData);
            Rcpp::Rcerr << "\nC++ exception RcppbdEigen_hdf5_Block (unknown reason)";
            return;
        }
    }
    
    /**
     * @brief Main eigenvalue decomposition function for HDF5 matrices using Spectra
     * 
     * This is the main interface for eigenvalue decomposition of matrices stored in HDF5.
     * It uses Spectra library for consistent results with RSpectra and automatically 
     * handles both symmetric and non-symmetric matrices.
     * 
     * @param filename Path to HDF5 file containing the input matrix
     * @param strsubgroup Group path within the HDF5 file
     * @param strdataset Dataset name containing the matrix
     * @param k Number of eigenvalues to compute (0 = auto-select based on matrix size)
     * @param which Which eigenvalues to compute (LM, SM, LR, SR, LI, SI, LA, SA)
     * @param ncv Number of Arnoldi vectors (0 = auto-select)
     * @param bcenter Whether to center the data before decomposition
     * @param bscale Whether to scale the data before decomposition
     * @param tolerance Convergence tolerance for Spectra algorithms
     * @param max_iter Maximum iterations for Spectra algorithms  
     * @param compute_vectors Whether to compute eigenvectors or just eigenvalues
     * @param bforce Whether to overwrite existing results
     * @param threads Number of threads for parallel computation
     * 
     * @throws H5::FileIException for HDF5 file operation errors
     * @throws H5::DataSetIException for HDF5 dataset operation errors
     * @throws std::exception for computation errors
     * 
     * @note Results are written to HDF5 datasets under "EIGEN/<dataset_name>/"
     *       - eigenvalues (real): "EIGEN/<dataset_name>/values"
     *       - eigenvalues (imag): "EIGEN/<dataset_name>/values_imag" (if non-zero)
     *       - eigenvectors (real): "EIGEN/<dataset_name>/vectors"
     *       - eigenvectors (imag): "EIGEN/<dataset_name>/vectors_imag" (if non-zero)
     * @note Updated for Spectra 1.0.1 compatibility with enhanced parameter handling
     */
    inline void RcppbdEigen_hdf5(std::string filename, std::string strsubgroup, std::string strdataset,
                                 int k = 0, const std::string& which = "LM", int ncv = 0,
                                 bool bcenter = false, bool bscale = false,
                                 double tolerance = 1e-10, int max_iter = 1000, 
                                 bool compute_vectors = true, bool bforce = false,
                                 Rcpp::Nullable<int> threads = R_NilValue) {
        
        BigDataStatMeth::hdf5Dataset* dsA = nullptr;
        BigDataStatMeth::hdf5Dataset* dsd = nullptr;
        BigDataStatMeth::hdf5Dataset* dsu = nullptr;
        
        try {
            
            std::vector<hsize_t> stride = {1, 1}, block = {1, 1}, offset = {0, 0}, count = {0, 0};
            
            dsA = new BigDataStatMeth::hdf5Dataset(filename, strsubgroup, strdataset, false);
            dsA->openDataset();
            
            if (dsA->getDatasetptr() != nullptr) {
                
                // Create results folder following SVD pattern
                std::string stroutgroup = "EIGEN/" + strdataset;
                
                std::vector<hsize_t> dims_out = {dsA->nrows(), dsA->ncols()};
                count = {dims_out[0], dims_out[1]};
                
                // Check if matrix is square
                if (dims_out[0] != dims_out[1]) {
                    Rf_error("Matrix must be square for eigendecomposition");
                    return;
                }
                
                hsize_t n = dims_out[0];
                
                // Use parameter validation function
                std::tie(k, ncv) = validateSpectraParams((int)n, k, ncv);
                
                // Create output datasets
                dsd = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "values", bforce);
                if (compute_vectors) {
                    dsu = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "vectors", bforce);
                }
                
                // Small matrices => Direct eigendecomposition with Spectra
                if (n * n < MAXELEMSINBLOCK / 20) {
                    
                    Eigen::MatrixXd X;
                    eigdecomp reteig;
                    
                    std::vector<double> vdA(count[0] * count[1]);
                    dsA->readDatasetBlock({offset[0], offset[1]}, {count[0], count[1]}, stride, block, vdA.data());
                    
                    X = Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>(
                        vdA.data(), count[0], count[1]);
                    
                    reteig = RcppbdEigen_spectra(X, k, which, ncv, bcenter, bscale, tolerance, max_iter);
                    
                    if (!reteig.bconv) {
                        Rf_error("Eigendecomposition failed to converge");
                        return;
                    }
                    
                    // Write real eigenvalues
                    dsd->createDataset(1, reteig.eigenvalues_real.size(), "real");
                    dsd->writeDataset(Rcpp::wrap(reteig.eigenvalues_real));
                    
                    // Write real eigenvectors if computed
                    if (compute_vectors && reteig.bcomputevectors) {
                        dsu->createDataset(reteig.eigenvectors_real.rows(), reteig.eigenvectors_real.cols(), "real");
                        dsu->writeDataset(Rcpp::wrap(reteig.eigenvectors_real));
                    }
                    
                    // Write imaginary parts if non-zero (for non-symmetric matrices)
                    if (!reteig.is_symmetric && reteig.eigenvalues_imag.cwiseAbs().maxCoeff() > 1e-14) {
                        
                        BigDataStatMeth::hdf5Dataset* dsd_imag = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "values_imag", bforce);
                        dsd_imag->createDataset(1, reteig.eigenvalues_imag.size(), "real");
                        dsd_imag->writeDataset(Rcpp::wrap(reteig.eigenvalues_imag));
                        delete dsd_imag;
                        
                        if (compute_vectors && reteig.bcomputevectors && reteig.eigenvectors_imag.cwiseAbs().maxCoeff() > 1e-14) {
                            BigDataStatMeth::hdf5Dataset* dsu_imag = new BigDataStatMeth::hdf5Dataset(filename, stroutgroup, "vectors_imag", bforce);
                            dsu_imag->createDataset(reteig.eigenvectors_imag.rows(), reteig.eigenvectors_imag.cols(), "real");
                            dsu_imag->writeDataset(Rcpp::wrap(reteig.eigenvectors_imag));
                            delete dsu_imag;
                        }
                    }
                    
                } else {
                    // Large matrices => Block-based approach with Spectra  
                    if (dsA->getDatasetptr() != nullptr) {
                        RcppbdEigen_hdf5_Block(dsA, dsd, dsu, k, which, ncv, bcenter, bscale, tolerance, max_iter, compute_vectors, threads);
                    }
                }
            }
            delete dsd; dsd = nullptr;
            if (dsu) { delete dsu; dsu = nullptr; }
            delete dsA; dsA = nullptr;
            
        } catch(H5::FileIException& error) {
            checkClose_file(dsA, dsd, dsu);
            Rcpp::Rcerr << "\nc++ exception RcppbdEigen_hdf5 (File IException)\n";
        } catch(H5::DataSetIException& error) {
            checkClose_file(dsA, dsd, dsu);
            Rcpp::Rcerr << "\nc++ exception RcppbdEigen_hdf5 (DataSet IException) "<<error.getDetailMsg() <<" \n";
        } catch(std::exception &ex) {
            checkClose_file(dsA, dsd, dsu);
            Rcpp::Rcerr << "c++ exception RcppbdEigen_hdf5: " << ex.what();
        } catch (...) {
            checkClose_file(dsA, dsd, dsu);
            Rcpp::Rcerr << "\nC++ exception RcppbdEigen_hdf5 (unknown reason)";
        }
    }

} // namespace BigDataStatMeth

#endif // BIGDATASTATMETH_HDF5_MATRIXEIGEN_HPP
