#include <BigDataStatMeth.hpp>
// #include "hdf5Algebra/matrixEquationSolver.hpp"
// #include "hdf5Algebra/matrixPseudoinverse.hpp"
// #include "Utilities/Utilities.hpp"

/**
 * @file hdf5_SolveEqation.cpp
 * @brief Implementation of linear equation solvers for HDF5-stored matrices
 * @details This file provides functionality for solving linear systems of equations
 * of the form AX = B, where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
 * The implementation supports:
 * - In-memory and HDF5-stored matrices
 * - Symmetric and non-symmetric systems
 * - Multiple right-hand sides
 * - Efficient LAPACK-based solvers
 * 
 * Key features:
 * - Automatic detection of matrix symmetry
 * - Optimized solvers for symmetric systems
 * - Memory-efficient HDF5 I/O
 * - Error handling and validation
 */

/**
 * @brief Solves linear system AX = B for in-memory matrices
 * 
 * @details Implements an efficient solver for linear systems using LAPACK routines.
 * The function automatically detects if A is symmetric and uses the appropriate
 * solver (dsysv for symmetric, dgesv for non-symmetric systems).
 * 
 * Implementation details:
 * - Uses LAPACK's dsysv_ for symmetric systems
 * - Uses LAPACK's dgesv_ for non-symmetric systems
 * - Performs automatic workspace optimization
 * - Handles multiple right-hand sides efficiently
 * 
 * @param A Input matrix A (N-by-N)
 * @param B Right-hand side matrix B (N-by-NRHS)
 * @return Solution matrix X (N-by-NRHS)
 * 
 * @throws std::exception if computation fails
 */

//' Solve Linear System AX = B (In-Memory)
//'
//' @description
//' Solves the linear system AX = B where A is an N-by-N matrix and X and B are
//' N-by-NRHS matrices. The function automatically detects if A is symmetric and
//' uses the appropriate solver.
//'
//' @details
//' This function provides an efficient implementation for solving linear systems
//' using LAPACK routines. Key features:
//' 
//' * Automatic detection of matrix properties:
//'   - Checks for matrix symmetry
//'   - Selects optimal solver based on matrix structure
//' 
//' * Solver selection:
//'   - Symmetric systems: Uses LAPACK's dsysv routine
//'   - Non-symmetric systems: Uses LAPACK's dgesv routine
//' 
//' * Performance optimizations:
//'   - Automatic workspace sizing
//'   - Efficient memory management
//'   - Support for multiple right-hand sides
//'
//' The implementation ensures:
//' * Robust error handling
//' * Efficient memory usage
//' * Numerical stability
//' * Support for various matrix sizes
//'
//' @param A Numeric matrix. The coefficient matrix (must be square).
//' @param B Numeric matrix. The right-hand side matrix (must have same number of
//'   rows as A).
//'
//' @return Numeric matrix X, the solution to AX = B.
//'
//' @examples
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' n <- 500
//' m <- 500
//' 
//' A <- matrix(runif(n*m), nrow = n, ncol = m)
//' B <- matrix(runif(n), nrow = n)
//' AS <- A %*% t(A)  # Create symmetric matrix
//' 
//' # Solve using bdSolve
//' X <- bdSolve(A, B)
//' 
//' # Compare with R's solve
//' XR <- solve(A, B)
//' all.equal(X, XR, check.attributes=FALSE)
//'
//' @references
//' * Anderson, E. et al. (1999). LAPACK Users' Guide, 3rd Edition.
//'   SIAM, Philadelphia.
//' * Golub, G. H., & Van Loan, C. F. (2013). Matrix Computations, 4th Edition.
//'   Johns Hopkins University Press.
//'
//' @seealso
//' * \code{\link{bdSolve_hdf5}} for solving systems with HDF5-stored matrices
//' * \code{\link{solve}} for R's built-in solver
//'
//' @export
// [[Rcpp::export]]
Rcpp::RObject bdSolve(const Rcpp::RObject A, const Rcpp::RObject B) 
{
    
    try {
        
        H5::Exception::dontPrint();
        
        Eigen::MatrixXd a, b;
        
        char Uchar = 'U';
        int info = 0;
        
        a = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(A);
        b = Rcpp::as<Eigen::Map<Eigen::MatrixXd> >(B);

        // Declare matrix variables
        int n = a.rows();
        int nrhs = b.cols();
        int lwork = std::max( 1, n );
        int lda = std::max( 1, n );
        int ldb = std::max( 1, n );
        std::vector<int> ipiv(n);
        std::vector<double> work(lwork);
        
        // Solve matrix equation
        if( a == a.transpose()  )
        {
            
            // dsysv_( char* UPLO, int* N , int* NRHS, double* A, int* LDA, int* IPIV, double* B, int* LDB, double* WORK, int* LWORK, int* INFO);
            BigDataStatMeth::dsysv_( & Uchar, &n, &nrhs, a.data(), &lda, ipiv.data(), b.data(), &ldb, work.data(), &lwork, &info);
        } else {
            
            // dgesv( int N, int NRHS, double A, int LDA, int IPIV, double B, int LDB, int INFO);
            BigDataStatMeth::dgesv_( &n, &nrhs, a.data(), &lda, ipiv.data(), b.data(), &ldb, &info );
        }
    
        return(Rcpp::wrap(b));
        
    } catch(std::exception &ex) {
        Rcpp::Rcerr << "c++ exception bdSolve" << ex.what(); 
        return Rcpp::wrap(-1);
    }
    
}


/**
 * @brief Solves linear system AX = B for HDF5-stored matrices
 * 
 * @details Implements a solver for linear systems where matrices are stored in
 * HDF5 format. The function handles I/O efficiently and uses appropriate LAPACK
 * routines for the solution.
 * 
 * Implementation features:
 * - Memory-efficient HDF5 I/O
 * - Automatic solver selection
 * - Support for large matrices
 * - Flexible output options
 * 
 * @param filename HDF5 file path
 * @param groupA Group containing matrix A
 * @param datasetA Dataset name for matrix A
 * @param groupB Group containing matrix B
 * @param datasetB Dataset name for matrix B
 * @param outgroup Output group for solution
 * @param outdataset Output dataset name
 * @param overwrite Whether to overwrite existing results
 * 
 * @throws H5::FileIException if there are HDF5 file operation errors
 * @throws H5::GroupIException if there are HDF5 group operation errors
 * @throws H5::DataSetIException if there are HDF5 dataset operation errors
 * @throws std::exception for other errors
 */

//' Solve Linear System AX = B (HDF5-Stored)
//'
//' @description
//' Solves the linear system AX = B where matrices A and B are stored in HDF5
//' format. The solution X is written back to the HDF5 file.
//'
//' @details
//' This function provides an HDF5-based implementation for solving large linear
//' systems. Key features:
//' 
//' * HDF5 Integration:
//'   - Efficient reading of input matrices
//'   - Memory-efficient processing
//'   - Direct output to HDF5 format
//' 
//' * Implementation Features:
//'   - Automatic solver selection
//'   - Support for large matrices
//'   - Flexible output options
//'   - Memory-efficient processing
//'
//' The function handles:
//' * Data validation
//' * Memory management
//' * Error handling
//' * HDF5 file operations
//'
//' @param filename String. Path to the HDF5 file.
//' @param groupA String. Group containing matrix A.
//' @param datasetA String. Dataset name for matrix A.
//' @param groupB String. Group containing matrix B.
//' @param datasetB String. Dataset name for matrix B.
//' @param outgroup Optional string. Output group name (defaults to "Solved").
//' @param outdataset Optional string. Output dataset name (defaults to "A_B").
//' @param overwrite Logical. Whether to overwrite existing results.
//'
//' @return List with components. If an error occurs, all string values are returned as empty strings (""):
//' \describe{
//'   \item{fn}{Character string with the HDF5 filename}
//'   \item{ds}{Character string with the full dataset path to the solution of the linear system (group/dataset)}
//' }
//'
//' @examples
//' library(BigDataStatMeth)
//' 
//' # Create test matrices
//' N <- 1000
//' M <- 1000
//' fn <- "test_temp.hdf5"
//' 
//' set.seed(555)
//' Y <- matrix(rnorm(N*M), N, M)
//' X <- matrix(rnorm(N), N, 1)
//' Ycp <- crossprod(Y)
//' 
//' # Compare with in-memory solution
//' resm <- bdSolve(Ycp, X)
//' resr <- solve(Ycp, X)
//' all.equal(resm, resr)
//' 
//' # Save matrices to HDF5
//' bdCreate_hdf5_matrix(filename = fn,
//'                      object = Ycp,
//'                      group = "data",
//'                      dataset = "A",
//'                      transp = FALSE,
//'                      overwriteFile = TRUE,
//'                      overwriteDataset = TRUE,
//'                      unlimited = FALSE)
//' 
//' bdCreate_hdf5_matrix(filename = fn,
//'                      object = X,
//'                      group = "data",
//'                      dataset = "B",
//'                      transp = FALSE,
//'                      overwriteFile = FALSE,
//'                      overwriteDataset = TRUE,
//'                      unlimited = FALSE)
//' 
//' # Solve using HDF5-stored matrices
//' bdSolve_hdf5(filename = fn,
//'              groupA = "data",
//'              datasetA = "A",
//'              groupB = "data",
//'              datasetB = "B",
//'              outgroup = "Solved",
//'              outdataset = "A_B",
//'              overwrite = TRUE)
//' 
//' # Cleanup
//' if (file.exists(fn)) {
//'     file.remove(fn)
//' }
//'
//' @references
//' * Anderson, E. et al. (1999). LAPACK Users' Guide, 3rd Edition.
//'   SIAM, Philadelphia.
//' * The HDF Group. (2000-2010). HDF5 User's Guide.
//'
//' @seealso
//' * \code{\link{bdSolve}} for in-memory matrix solving
//' * \code{\link{bdCreate_hdf5_matrix}} for creating HDF5 matrices
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bdSolve_hdf5(std::string filename, std::string groupA, std::string datasetA,
                      std::string groupB, std::string datasetB,
                      Rcpp::Nullable<std::string> outgroup = R_NilValue, 
                      Rcpp::Nullable<std::string> outdataset = R_NilValue, 
                      Rcpp::Nullable<bool> overwrite = R_NilValue) 
{
    
    BigDataStatMeth::hdf5Dataset* dsA = nullptr;
    BigDataStatMeth::hdf5Dataset* dsB = nullptr;
    BigDataStatMeth::hdf5Dataset* dsRes = nullptr;
    
    Rcpp::List lst_return = Rcpp::List::create(Rcpp::Named("fn") = "",
                                               Rcpp::Named("ds") = "");
    
    try {
        
        H5::Exception::dontPrint();
        
        std::string strOutgroup, strOutdataset;
        bool bforce;
        
        if(outgroup.isNull()) { strOutgroup = "Solved"; } 
        else {   strOutgroup = Rcpp::as<std::string>(outgroup); }
        
        if(outdataset.isNull()) { strOutdataset = datasetA+"_"+datasetB; } 
        else { strOutdataset = Rcpp::as<std::string>(outdataset); }
        
        if(overwrite.isNull()) { bforce = false ; }
        else { bforce = Rcpp::as<bool>(overwrite); }
        
        dsA = new BigDataStatMeth::hdf5Dataset(filename, groupA, datasetA, false);
        dsA->openDataset();
        
        dsB = new BigDataStatMeth::hdf5Dataset(filename, groupB, datasetB, false);
        dsB->openDataset();
        
        dsRes = new BigDataStatMeth::hdf5Dataset(filename, strOutgroup, strOutdataset, bforce);
        dsRes->createDataset( dsB->nrows(), dsB->ncols(), "real" );
        
        if( dsA->getDatasetptr() != nullptr &&  dsB->getDatasetptr() != nullptr && dsRes->getDatasetptr() != nullptr ) {
            RcppSolveHdf5( dsA, dsB, dsRes );    
        } else {
            checkClose_file(dsA, dsB, dsRes);
            Rcpp::Rcerr << "c++ exception bdSolve_hdf5: " << "Error creating dataset";
            return(lst_return);
        }
        
        delete dsRes; dsRes = nullptr;
        delete dsB; dsB = nullptr;
        delete dsA; dsA = nullptr;
        
        lst_return["fn"] = filename;
        lst_return["ds"] = strOutgroup + "/" + strOutdataset;
        
    } catch( H5::FileIException& error ) { 
        checkClose_file(dsA, dsB, dsRes);
        Rcpp::Rcerr<<"\nc++ exception bdSolve_hdf5 (File IException)";
        return(lst_return);
    } catch( H5::GroupIException & error ) { 
        checkClose_file(dsA, dsB, dsRes);
        Rcpp::Rcerr<<"\nc++ exception bdSolve_hdf5 (Group IException)";
        return(lst_return);
    } catch( H5::DataSetIException& error ) { 
        checkClose_file(dsA, dsB, dsRes);
        Rcpp::Rcerr<<"\nc++ exception bdSolve_hdf5 (DataSet IException)";
        return(lst_return);
    } catch(std::exception& ex) {
        checkClose_file(dsA, dsB, dsRes);
        Rcpp::Rcerr<<"\nc++ exception bdSolve_hdf5" << ex.what();
        return(lst_return);
    } catch (...) {
        checkClose_file(dsA, dsB, dsRes);
        Rcpp::Rcerr<<"\nC++ exception bdSolve_hdf5 (unknown reason)";
        return(lst_return);
    }
    
    return(lst_return);
     
 }
