#include "EigenR.h"

/* Cholesky ----------------------------------------------------------------- */
template <typename Number>
struct Cholesky {
  Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> U;
  Number determinant;
};

template <typename Number>
Cholesky<Number> chol(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::LLT<Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    lltOfM(M);
  if(lltOfM.info() != Eigen::Success) {
    throw Rcpp::exception("The matrix is not positive definite.");
  }
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> U =
    lltOfM.matrixU();
  Cholesky<Number> out;
  out.U = U;
  out.determinant = pow(U.diagonal().prod(), 2);
  return out;
}

/*
 template <typename Matrix, typename Number>
 Matrix chol_sparse(
 Eigen::SparseMatrix<Number>& M) {
 Eigen::SimplicialLLT<Eigen::SparseMatrix<Number>> solver;
 M.makeCompressed();
 solver.analyzePattern(M);
 solver.factorize(M);
 if(solver.info() != Eigen::Success) {
 throw Rcpp::exception("LU factorization has failed.");
 }
 //Matrix U;
 
 if(std::is_same<Number, std::complex<double>>::value) {
 Rcomplex I;
 I.r = 0.0; I.i = 1.0;
 Rcpp::NumericMatrix Ureal0 = Rcpp::wrap(solver.matrixU().real());
 Rcpp::NumericMatrix Uimag0 = Rcpp::wrap(solver.matrixU().imag());
 Matrix Ureal(Ureal0); Matrix Uimag(Uimag0);
 Matrix U(Ureal + I * Uimag);
 return U;
 }else {
 Matrix U = Rcpp::wrap(solver.matrixU());
 return U;
 }
 U.attr("determinant") = solver.determinant();
 return U;
 }
 */

template <typename Number>
Cholesky<Number> chol_sparse(Eigen::SparseMatrix<Number>& M) {
  Eigen::SimplicialLLT<Eigen::SparseMatrix<Number>> solver;
  M.makeCompressed();
  solver.analyzePattern(M);
  solver.factorize(M);
  if(solver.info() != Eigen::Success) {
    throw Rcpp::exception("LU factorization has failed.");
  }
  Cholesky<Number> out;
  out.U = solver.matrixU();
  out.determinant = solver.determinant();
  return out;
}

/*
 Rcpp::NumericMatrix chol_sparse_real(Eigen::SparseMatrix<double>& M) {
 Eigen::SimplicialLLT<Eigen::SparseMatrix<double>> solver;
 M.makeCompressed();
 solver.analyzePattern(M);
 solver.factorize(M);
 if(solver.info() != Eigen::Success) {
 throw Rcpp::exception("LU factorization has failed.");
 }
 Eigen::MatrixXd U = solver.matrixU();
 SEXP s = Rcpp::wrap(U);
 Rcpp::NumericMatrix out(s);
 out.attr("determinant") = solver.determinant();
 return out;
 }
 
 Rcpp::ComplexVector chol_sparse_cplx(
 Eigen::SparseMatrix<std::complex<double>>& M) {
 Eigen::SimplicialLLT<Eigen::SparseMatrix<std::complex<double>>> solver;
 M.makeCompressed();
 solver.analyzePattern(M);
 solver.factorize(M);
 if(solver.info() != Eigen::Success) {
 throw Rcpp::exception("LU factorization has failed.");
 }
 Eigen::MatrixXcd U = solver.matrixU();
 Eigen::MatrixXd Ureal = U.real();
 Eigen::MatrixXd Uimag = U.imag();
 SEXP UrealS = Rcpp::wrap(Ureal);
 SEXP UimagS = Rcpp::wrap(Uimag);
 Rcpp::NumericMatrix outReal(UrealS);
 Rcpp::NumericMatrix outImag(UimagS);
 Rcpp::ComplexMatrix outRealCplx(outReal);
 Rcpp::ComplexMatrix outImagCplx(outImag);
 Rcomplex I;
 I.r = 0.0; I.i = 1.0;
 Rcpp::ComplexVector out = outRealCplx + I * outImagCplx;
 out.attr("dim") = Rcpp::Dimension(U.rows(), U.cols());
 out.attr("determinant") = solver.determinant();
 return out;
 }
 */

// [[Rcpp::export]]
Rcpp::NumericMatrix EigenR_chol_real(const Eigen::MatrixXd& M) {
  Cholesky<double> cholesky = chol<double>(M);
  Rcpp::NumericMatrix U = dblMatrixToRcpp(cholesky.U);
  U.attr("determinant") = cholesky.determinant;
  return U;
}

// [[Rcpp::export]]
Rcpp::ComplexVector EigenR_chol_cplx(const Eigen::MatrixXd& Re,
                                     const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  Cholesky<std::complex<double>> cholesky = chol<std::complex<double>>(M);
  Rcpp::ComplexVector U = cplxMatrixToRcpp(cholesky.U);
  U.attr("determinant") = cholesky.determinant;
  return U;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix EigenR_chol_sparse_real(const std::vector<size_t>& i,
                                            const std::vector<size_t>& j,
                                            const std::vector<double>& Mij,
                                            const size_t nrows,
                                            const size_t ncols) {
  Eigen::SparseMatrix<double> M = realSparseMatrix(i, j, Mij, nrows, ncols);
  Cholesky<double> cholesky = chol_sparse<double>(M);
  Rcpp::NumericMatrix U = dblMatrixToRcpp(cholesky.U);
  U.attr("determinant") = cholesky.determinant;
  return U;
}

// [[Rcpp::export]]
Rcpp::ComplexVector EigenR_chol_sparse_cplx(
    const std::vector<size_t>& i,
    const std::vector<size_t>& j,
    const std::vector<std::complex<double>>& Mij,
    const size_t nrows,
    const size_t ncols) {
  Eigen::SparseMatrix<std::complex<double>> M =
    cplxSparseMatrix(i, j, Mij, nrows, ncols);
  Cholesky<std::complex<double>> cholesky =
    chol_sparse<std::complex<double>>(M);
  Rcpp::ComplexVector U = cplxMatrixToRcpp(cholesky.U);
  U.attr("determinant") = cholesky.determinant;
  return U;
}

/* UtDU --------------------------------------------------------------------- */
template <typename Number>
Rcpp::List UtDU(
    const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) {
  const Eigen::LDLT<Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>>
    ldltOfM(M);
  if(ldltOfM.info() != Eigen::Success) {
    throw Rcpp::exception("Factorization has failed.");
  }
  const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> U =
    ldltOfM.matrixU();
  const Eigen::Matrix<Number, Eigen::Dynamic, 1> D = ldltOfM.vectorD();
  const Eigen::Transpositions<Eigen::Dynamic> T = ldltOfM.transpositionsP();
  Eigen::VectorXi perm(T.size());
  for(auto i = 0; i < T.size(); i++) {
    perm(i) = i;
  }
  Rcpp::List out =
    Rcpp::List::create(Rcpp::Named("U") = U, Rcpp::Named("D") = D,
                       Rcpp::Named("perm") = T * perm);
  bool positive = ldltOfM.isPositive();
  double rcond = ldltOfM.rcond();
  out.attr("positive") = positive;
  out.attr("rcond") = rcond;
  return out;
}

/*
 template <typename Number>
 Rcpp::List UtDU_sparse(
 Eigen::SparseMatrix<Number>& M) {
 Eigen::SimplicialLDLT<Eigen::SparseMatrix<Number>> ldltOfM;
 M.makeCompressed();
 ldltOfM.analyzePattern(M);
 ldltOfM.factorize(M);
 if(ldltOfM.info() != Eigen::Success) {
 throw Rcpp::exception("Factorization has failed.");
 }
 const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic> U =
 ldltOfM.matrixU();
 const Eigen::Matrix<Number, Eigen::Dynamic, 1> D = ldltOfM.vectorD();
 const Eigen::Transpositions<Eigen::Dynamic> T = ldltOfM.transpositionsP();
 Eigen::VectorXi perm(T.size());
 for(auto i = 0; i < T.size(); i++) {
 perm(i) = i;
 }
 const Rcpp::List out =
 Rcpp::List::create(Rcpp::Named("U") = U, Rcpp::Named("D") = D,
 Rcpp::Named("perm") = T * perm);
 return out;
 }
 */

// [[Rcpp::export]]
Rcpp::List EigenR_UtDU_real(const Eigen::MatrixXd& M) {
  return UtDU<double>(M);
}

// [[Rcpp::export]]
Rcpp::List EigenR_UtDU_cplx(const Eigen::MatrixXd& Re,
                            const Eigen::MatrixXd& Im) {
  const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
  const Rcpp::List utdu = UtDU<std::complex<double>>(M);
  Rcpp::List out =
    Rcpp::List::create(Rcpp::Named("U") = cplxMatrixToList(utdu["U"]),
                       Rcpp::Named("D") = cplxVectorToList(utdu["D"]),
                       Rcpp::Named("perm") = utdu["perm"]);
  out.attr("positive") = utdu.attr("positive");
  out.attr("rcond") = utdu.attr("rcond");
  return out;
}

/*
 // [[Rcpp::export]]
 Rcpp::List EigenR_UtDU_sparse_real(const std::vector<size_t>& i,
 const std::vector<size_t>& j,
 const std::vector<double>& Mij,
 const size_t nrows,
 const size_t ncols) {
 Eigen::SparseMatrix<double> M = realSparseMatrix(i, j, Mij, nrows, ncols);
 return UtDU_sparse<double>(M);
 }
 
 // [[Rcpp::export]]
 Rcpp::List EigenR_UtDU_sparse_cplx(const std::vector<size_t>& i,
 const std::vector<size_t>& j,
 const std::vector<std::complex<double>>& Mij,
 const size_t nrows,
 const size_t ncols) {
 Eigen::SparseMatrix<std::complex<double>> M =
 cplxSparseMatrix(i, j, Mij, nrows, ncols);
 const Rcpp::List utdu = UtDU_sparse<std::complex<double>>(M);
 Rcpp::List out =
 Rcpp::List::create(Rcpp::Named("U") = cplxMatrixToList(utdu["U"]),
 Rcpp::Named("D") = cplxVectorToList(utdu["D"]),
 Rcpp::Named("perm") = utdu["perm"]);
 return out;
 }
 */
