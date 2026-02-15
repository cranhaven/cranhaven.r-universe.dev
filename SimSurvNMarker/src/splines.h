#ifndef SPLINES_H
#define SPLINES_H
#include <RcppArmadillo.h>
#include <limits> // quiet_NaN
#include <stdexcept> // invalid_argument
#define DEFAULT_ORDER 4L
#define DEFAULT_DERS 0L
#define DEFAULT_INTERCEPT false

namespace splines {
using namespace arma;

class basisMixin {
public:
  virtual uword get_n_basis() const = 0;

  virtual void operator()(
      vec &out, double const x, const int ders = DEFAULT_DERS) const = 0;
  vec operator()(
      double const x, int const ders = DEFAULT_DERS) const;

  mat basis(
      const vec &x, const int ders = DEFAULT_DERS,
      const double centre = std::numeric_limits<double>::quiet_NaN())
    const;

  virtual ~basisMixin() = default;
};

class SplineBasis : public basisMixin {
public:
  int const order = DEFAULT_ORDER, /* order of the spline */
            ordm1 = order - 1;     /* order - 1 (3 for cubic splines) */
  vec const knots;	               /* knot vector */
  int const nknots = knots.n_elem, /* number of knots
                                      except for the boundary case */
             ncoef =               /* number of coefficients */
        nknots > order ? nknots - order : 0L;

private:
  int mutable curs,		/* current position in knots vector */
          boundary;		/* must have knots[(curs) <= x < knots(curs+1) */
  vec mutable ldel = vec(ordm1), /* differences from knots on the left */
              rdel = vec(ordm1), /* differences from knots on the right */
              a    = vec(order), /* scratch array */
              wrk  = vec(order); /* working memory */

public:
  SplineBasis(const int order);
  SplineBasis(const vec knots, const int order = DEFAULT_ORDER);

  uword get_n_basis() const {
    return ncoef;
  }

  using basisMixin::operator();
  void operator()(
      vec &out, double const x, const int ders = DEFAULT_DERS) const;

  virtual ~SplineBasis() = default;

private:
  int set_cursor(const double x) const;
  void diff_table(const double x, const int ndiff) const;
  void derivs(vec &b, const double x, int const ders) const;
  /* fast evaluation of basis functions */
  void basis_funcs(vec &b, const double x) const;
};

class bs final : public SplineBasis {
public:
  vec const boundary_knots, interior_knots;
  bool const intercept;
  int const df;

private:
  /* working memory */
  vec mutable wrk = arma::vec(SplineBasis::get_n_basis()),
             wrks = arma::vec(wrk.begin(), get_n_basis(), false);

public:
  bs(const vec &bk, const vec &ik,
     const bool inter = DEFAULT_INTERCEPT,
     const int ord = DEFAULT_ORDER);

  uword get_n_basis() const {
    return SplineBasis::get_n_basis() - (!intercept);
  }

  using SplineBasis::operator();
  void operator()(
      vec &out, double const x, const int ders = DEFAULT_DERS) const;
};

class ns final : public basisMixin {
public:
  bs const bspline; // composition cf. inheritance
  bool const intercept;
  mat const q_matrix = ([&](){
    // calculate the Q matrix
    mat const_basis = bspline.basis(bspline.boundary_knots, 2);
    if (!intercept)
      const_basis = const_basis.cols(1, const_basis.n_cols - 1);
    mat qd, rd;
    if(!qr(qd, rd, const_basis.t()))
#ifdef DO_CHECKS
      throw std::invalid_argument("ns: QR decomposition failed");
#else
      { }
#endif
    // enforce positive diagonal entries of R and hence unique Q matrix
    {
      size_t const nr = qd.n_rows,
                   nc = rd.n_cols;
      for(size_t i = 0; i < nc; ++i)
        if(rd.at(i, i) < 0){
          double * const qc = qd.colptr(i);
          for(size_t j = 0; j < nr; ++j)
            qc[j] *= -1;
        }
    }

    inplace_trans(qd);
    return qd;
  })();
  vec const tl0, tl1, tr0, tr1;

  ns(const vec &boundary_knots, const vec &interior_knots,
     const bool intercept = DEFAULT_INTERCEPT,
     const int order = DEFAULT_ORDER);

  uword get_n_basis() const {
    return q_matrix.n_rows - 2;
  }

  using basisMixin::operator();
  void operator()(
      vec &out, double const x, const int ders = DEFAULT_DERS) const;

private:
  vec trans(const vec &x) const;
}; // class ns

class iSpline final : public basisMixin {
public:
  bool const intercept;
  int const order;
  bs const bspline; // composition cf. inheritance

private:
  vec mutable wrk = arma::vec(bspline.get_n_basis());

public:
  iSpline(const vec &boundary_knots, const vec &interior_knots,
          const bool intercept = DEFAULT_INTERCEPT,
          const int order = DEFAULT_ORDER);

  uword get_n_basis() const {
    return bspline.get_n_basis() - (!intercept);
  }

  using basisMixin::operator();
  void operator()(
      vec &out, double const x, const int der = DEFAULT_DERS) const;
}; // class iSpline

class mSpline final : public basisMixin {
public:
  bs const bspline;
  bool const intercept;

private:
  vec mutable wrk = vec(bspline.get_n_basis());

public:
  mSpline(const vec &boundary_knots, const vec &interior_knots,
          const bool intercept = DEFAULT_INTERCEPT,
          const int order = DEFAULT_ORDER);

  uword get_n_basis() const {
    return bspline.get_n_basis() - (!intercept);
  }

  using basisMixin::operator();
  void operator()(
      vec &out, double const x, const int der = DEFAULT_DERS) const;
}; // class mSpline

} // namespace splines

#undef DEFAULT_ORDER
#undef DEFAULT_DERS
#undef DEFAULT_INTERCEPT

#endif
