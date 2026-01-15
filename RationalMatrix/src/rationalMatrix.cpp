#include <RcppEigen.h>
#include <boost/multiprecision/gmp.hpp>
#include "RationalMatrix_types.h"
using namespace boost::multiprecision;
typedef Eigen::Matrix<mpq_rational, Eigen::Dynamic, Eigen::Dynamic> QMatrix;
typedef Eigen::Matrix<mpq_rational, Eigen::Dynamic, 1>              QVector;

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
std::string q2str(mpq_rational r) {
  mpz_int numer = numerator(r);
  mpz_int denom = denominator(r);
  mpz_t p;
  mpz_init(p);
  mpz_set(p, numer.backend().data());
  mpz_t q;
  mpz_init(q);
  mpz_set(q, denom.backend().data());
  size_t n = mpz_sizeinbase(p, 10) + 2;
  size_t d = mpz_sizeinbase(q, 10) + 2;
  char* cnumer = new char[n];
  char* cdenom = new char[d];
  cnumer = mpz_get_str(cnumer, 10, p);
  cdenom = mpz_get_str(cdenom, 10, q);
  std::string snumer = cnumer;
  std::string sdenom = cdenom;
  delete[] cnumer;
  delete[] cdenom;
  mpz_clear(p);
  mpz_clear(q);
  return snumer + "/" + sdenom;
}

// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
QMatrix charMatrix2qMatrix(CharMatrix M) {
  const int m = M.nrow();
  const int n = M.ncol();
  QMatrix Mq(m, n);
  for(int i = 0; i < m; i++) {
    for(int j = 0; j < n; j++) {
      Mq(i, j) = mpq_rational(Rcpp::as<std::string>(M(i, j)));
    }
  }
  return Mq;
}


// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
CharMatrix qMatrix2charMatrix(const QMatrix& Mq) {
  const int m = Mq.rows();
  const int n = Mq.cols();
  CharMatrix M(m, n);
  for(int i = 0; i < m; i++) {
    for(int j = 0; j < n; j++) {
      M(i, j) = q2str(Mq.coeff(i, j));
    }
  }
  return M;
}


// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
Rcpp::CharacterVector qVector2charVector(const QVector& Vq) {
  const int m = Vq.rows();
  Rcpp::CharacterVector V(m);
  for(int i = 0; i < m; i++) {
    V(i) = q2str(Vq.coeff(i, 0));
  }
  return V;
}


// ------------------------------------------------------------------------- //
// ------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::String det_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  mpq_rational d = Mq.determinant();
  return q2str(d);
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
/* image LU ----------------------------------------------------------------- */
// [[Rcpp::export]]
CharMatrix image_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  return qMatrix2charMatrix(lu.image(Mq));
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
/* injective, surjective, invertible ---------------------------------------- */
// [[Rcpp::export]]
bool isInjective_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  return lu.isInjective();
}

// [[Rcpp::export]]
bool isSurjective_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  return lu.isSurjective();
}

// [[Rcpp::export]]
bool isInvertible_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  return lu.isInvertible();
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
/* inverse ------------------------------------------------------------------ */
// [[Rcpp::export]]
CharMatrix inverse_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  if(lu.isInvertible()) {
    return qMatrix2charMatrix(lu.inverse());
  } else {
    throw Rcpp::exception("The matrix is not invertible.");
  }
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
/* kernel LU ----------------------------------------------------------------- */
// [[Rcpp::export]]
CharMatrix kernel_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  return qMatrix2charMatrix(lu.kernel());
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
/* rank LU ----------------------------------------------------------------- */
// [[Rcpp::export]]
unsigned rank_rcpp(CharMatrix M) {
  QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::FullPivLU<QMatrix> lu(Mq);
  return lu.rank();
}


// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
/* UtDU --------------------------------------------------------------------- */
// [[Rcpp::export]]
Rcpp::List UtDU_rcpp(CharMatrix M) {
  const QMatrix Mq = charMatrix2qMatrix(M);
  const Eigen::LDLT<QMatrix> ldltOfM(Mq);
  if(ldltOfM.info() != Eigen::Success) {
    throw Rcpp::exception("Factorization has failed.");
  }
  const QMatrix U = ldltOfM.matrixU();
  const QVector D = ldltOfM.vectorD();
  const Eigen::Transpositions<Eigen::Dynamic> T = ldltOfM.transpositionsP();
  Eigen::VectorXi perm(T.size());
  for(auto i = 0; i < T.size(); i++) {
    perm(i) = i;
  }
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("U") = qMatrix2charMatrix(U), 
    Rcpp::Named("D") = qVector2charVector(D),
    Rcpp::Named("perm") = T * perm
  );
  bool positive = ldltOfM.isPositive();
  out.attr("positive") = positive;
  return out;
}
