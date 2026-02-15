#ifndef MAPFIT_TRAITS_H
#define MAPFIT_TRAITS_H

#include <Rcpp.h>

// tags
struct TRANS{};
struct NOTRANS{};

struct ArrayT{};
struct DenseMatrixT{};
struct SparseMatrixT{};
struct CSRMatrixT{};
struct CSCMatrixT{};
struct COOMatrixT{};

template <class T>
struct matrix_category;

template <class T, class S = double>
struct vector_traits {
  using ValueT = S;
  static int size(const T& v);
  static const ValueT* value(const T& v);
  static ValueT* value(T& v);
  static int inc(const T& v);
};

template <class T, class S = double>
struct stride_vector_traits {
  using ValueT = S;
  static int size(const T& v);
  static const ValueT* value(const T& v);
  static ValueT* value(T& v);
};

template <class T>
struct dense_matrix_traits {
  static int nrow(const T& m);
  static int ncol(const T& m);
  static const double* value(const T& m);
  static double* value(T& m);
  static int ld(const T& m);
};

template <class T>
struct csr_matrix_traits {
  static int nrow(const T& m);
  static int ncol(const T& m);
  static int nnz(const T& m);
  static int base(const T& m);
  static double* value(T& m);
  static const double* value(const T& m);
  static const int* rowptr(const T& m);
  static const int* colind(const T& m);
};

template <class T>
struct csc_matrix_traits {
  static int nrow(const T& m);
  static int ncol(const T& m);
  static int nnz(const T& m);
  static int base(const T& m);
  static double* value(T& m);
  static const double* value(const T& m);
  static const int* colptr(const T& m);
  static const int* rowind(const T& m);
};

template <class T>
struct coo_matrix_traits {
  static int nrow(const T& m);
  static int ncol(const T& m);
  static int nnz(const T& m);
  static int base(const T& m);
  static double* value(T& m);
  static const double* value(const T& m);
  static const int* rowind(const T& m);
  static const int* colind(const T& m);
};

template <>
struct vector_traits<std::vector<double>,double> {
  static int size(const std::vector<double>& v) { return v.size(); }
  static const double* value(const std::vector<double>& v) { return &v[0]; }
  static double* value(std::vector<double>& v) { return &v[0]; }
  static int inc(const std::vector<double>& v) { return 1; }
};

template <>
struct vector_traits<std::vector<int>,int> {
  static int size(const std::vector<int>& v) { return v.size(); }
  static const int* value(const std::vector<int>& v) { return &v[0]; }
  static int* value(std::vector<int>& v) { return &v[0]; }
  static int inc(const std::vector<int>& v) { return 1; }
};

template <>
struct stride_vector_traits<std::vector<double>,double> {
  static int size(const std::vector<double>& v) { return v.size(); }
  static const double* value(const std::vector<double>& v) { return &v[0]; }
  static double* value(std::vector<double>& v) { return &v[0]; }
};

template <>
struct stride_vector_traits<std::vector<int>,int> {
  static int size(const std::vector<int>& v) { return v.size(); }
  static const int* value(const std::vector<int>& v) { return &v[0]; }
  static int* value(std::vector<int>& v) { return &v[0]; }
};

// s4 matrix
template <typename T>
struct S4matrix {
  int m;
  int n;
  Rcpp::NumericVector x;
  Rcpp::IntegerVector ptr;
  Rcpp::IntegerVector rowind;
  Rcpp::IntegerVector colind;
  
  S4matrix(Rcpp::S4& s4);
};

template<>
inline
S4matrix<DenseMatrixT>::S4matrix(Rcpp::S4& s4) :
  m(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[0]),
  n(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[1]),
  x(Rcpp::as<Rcpp::NumericVector>(s4.slot("x"))),
  ptr(0),
  rowind(0),
  colind(0) {}

template<>
inline
S4matrix<CSRMatrixT>::S4matrix(Rcpp::S4& s4) :
  m(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[0]),
  n(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[1]),
  x(Rcpp::as<Rcpp::NumericVector>(s4.slot("x"))),
  ptr(Rcpp::as<Rcpp::IntegerVector>(s4.slot("p"))),
  rowind(0),
  colind(Rcpp::as<Rcpp::IntegerVector>(s4.slot("j"))) {}

template<>
inline
S4matrix<CSCMatrixT>::S4matrix(Rcpp::S4& s4) :
  m(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[0]),
  n(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[1]),
  x(Rcpp::as<Rcpp::NumericVector>(s4.slot("x"))),
  ptr(Rcpp::as<Rcpp::IntegerVector>(s4.slot("p"))),
  rowind(Rcpp::as<Rcpp::IntegerVector>(s4.slot("i"))),
  colind(0) {}

template<>
inline
S4matrix<COOMatrixT>::S4matrix(Rcpp::S4& s4) :
  m(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[0]),
  n(Rcpp::as<Rcpp::IntegerVector>(s4.slot("Dim"))[1]),
  x(Rcpp::as<Rcpp::NumericVector>(s4.slot("x"))),
  ptr(0),
  rowind(Rcpp::as<Rcpp::IntegerVector>(s4.slot("i"))),
  colind(Rcpp::as<Rcpp::IntegerVector>(s4.slot("j"))) {}

template <>
struct vector_traits<Rcpp::NumericVector,double> {
  static int size(const Rcpp::NumericVector& v) { return v.size(); }
  static const double* value(const Rcpp::NumericVector& v) { return &v[0]; }
  static double* value(Rcpp::NumericVector& v) { return &v[0]; }
  static int inc(const Rcpp::NumericVector& v) { return 1; }
};

template <>
struct vector_traits<Rcpp::IntegerVector,int> {
  static int size(const Rcpp::IntegerVector& v) { return v.size(); }
  static const int* value(const Rcpp::IntegerVector& v) { return &v[0]; }
  static int* value(Rcpp::IntegerVector& v) { return &v[0]; }
  static int inc(const Rcpp::IntegerVector& v) { return 1; }
};

template <>
struct stride_vector_traits<Rcpp::NumericVector,double> {
  static int size(const Rcpp::NumericVector& v) { return v.size(); }
  static const double* value(const Rcpp::NumericVector& v) { return &v[0]; }
  static double* value(Rcpp::NumericVector& v) { return &v[0]; }
};

template <>
struct stride_vector_traits<Rcpp::IntegerVector,int> {
  static int size(const Rcpp::IntegerVector& v) { return v.size(); }
  static const int* value(const Rcpp::IntegerVector& v) { return &v[0]; }
  static int* value(Rcpp::IntegerVector& v) { return &v[0]; }
};

template <>
struct matrix_category<Rcpp::NumericMatrix> {
  using type = DenseMatrixT;
};

template <typename T>
struct matrix_category<S4matrix<T>> {
  using type = T;
};

template <>
struct vector_traits<Rcpp::NumericMatrix> {
  static int size(const Rcpp::NumericMatrix& v) { return v.size(); }
  static const double* value(const Rcpp::NumericMatrix& v) { return &v[0]; }
  static double* value(Rcpp::NumericMatrix& v) { return &v[0]; }
  static int inc(const Rcpp::NumericMatrix& v) { return 1; }
};

template <>
struct dense_matrix_traits<Rcpp::NumericMatrix> {
  static int nrow(const Rcpp::NumericMatrix& m) { return m.nrow(); }
  static int ncol(const Rcpp::NumericMatrix& m) { return m.ncol(); }
  static const double* value(const Rcpp::NumericMatrix& m) { return &m[0]; }
  static double* value(Rcpp::NumericMatrix& m) { return &m[0]; }
  static int ld(const Rcpp::NumericMatrix& m) { return nrow(m); }
};

template <typename T>
struct vector_traits<S4matrix<T>> {
  static int size(const S4matrix<T>& v) { return v.x.length(); }
  static const double* value(const S4matrix<T>& v) { return &v.x[0]; }
  static double* value(S4matrix<T>& v) { return &v.x[0]; }
  static int inc(const S4matrix<T>& v) { return 1; }
};

template <>
struct dense_matrix_traits<S4matrix<DenseMatrixT>> {
  static int nrow(const S4matrix<DenseMatrixT>& m) { return m.m; }
  static int ncol(const S4matrix<DenseMatrixT>& m) { return m.n; }
  static const double* value(const S4matrix<DenseMatrixT>& m) { return &m.x[0]; }
  static double* value(S4matrix<DenseMatrixT>& m) { return &m.x[0]; }
  static int ld(const S4matrix<DenseMatrixT>& m) { return m.m; }
};

template <>
struct csr_matrix_traits<S4matrix<CSRMatrixT>> {
  static int nrow(const S4matrix<CSRMatrixT>& m) { return m.m; }
  static int ncol(const S4matrix<CSRMatrixT>& m) { return m.n; }
  static int nnz(const S4matrix<CSRMatrixT>& m) { return m.x.length(); }
  static int base(const S4matrix<CSRMatrixT>& m) { return 0; }
  static double* value(S4matrix<CSRMatrixT>& m) { return &m.x[0]; }
  static const double* value(const S4matrix<CSRMatrixT>& m) { return &m.x[0]; }
  static const int* rowptr(const S4matrix<CSRMatrixT>& m) { return &m.ptr[0]; }
  static const int* colind(const S4matrix<CSRMatrixT>& m) { return &m.colind[0]; }
};

template <>
struct csc_matrix_traits<S4matrix<CSCMatrixT>> {
  static int nrow(const S4matrix<CSCMatrixT>& m) { return m.m; }
  static int ncol(const S4matrix<CSCMatrixT>& m) { return m.n; }
  static int nnz(const S4matrix<CSCMatrixT>& m) { return m.x.length(); }
  static int base(const S4matrix<CSCMatrixT>& m) { return 0; }
  static double* value(S4matrix<CSCMatrixT>& m) { return &m.x[0]; }
  static const double* value(const S4matrix<CSCMatrixT>& m) { return &m.x[0]; }
  static const int* colptr(const S4matrix<CSCMatrixT>& m) { return &m.ptr[0]; }
  static const int* rowind(const S4matrix<CSCMatrixT>& m) { return &m.rowind[0]; }
};

template <>
struct coo_matrix_traits<S4matrix<COOMatrixT>> {
  static int nrow(const S4matrix<COOMatrixT>& m) { return m.m; }
  static int ncol(const S4matrix<COOMatrixT>& m) { return m.n; }
  static int nnz(const S4matrix<COOMatrixT>& m) { return m.x.length(); }
  static int base(const S4matrix<COOMatrixT>& m) { return 0; }
  static double* value(S4matrix<COOMatrixT>& m) { return &m.x[0]; }
  static const double* value(const S4matrix<COOMatrixT>& m) { return &m.x[0]; }
  static const int* rowind(const S4matrix<COOMatrixT>& m) { return &m.rowind[0]; }
  static const int* colind(const S4matrix<COOMatrixT>& m) { return &m.colind[0]; }
};

#endif
