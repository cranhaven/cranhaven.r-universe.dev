/*
 Double exponential formula
 */

#include <Rcpp.h>

#include <vector>
#include <cfloat>
#include <cmath>
#include <algorithm>

// [[Rcpp::plugins("cpp11")]]

namespace deformula {

const static int kStartd = 8;
const static double kDeformulaZero = DBL_EPSILON;
const static double kDeformulaReltol = DBL_EPSILON;
const static int kDeformulaMaxIteration = 31;  // max length of t is 2^31
const static double pi = 4.0 * atan(1.0);

class Deformula {
protected:
  double m_lower;
  double m_upper;
  double m_h;

  double m_zero;
  double m_reltol;
  int m_maxiter;
  int m_dstart;

  int m_info;
  int m_iter;
  double m_aerror;
  double m_rerror;
  double m_sum;

  class DeformulaElement {
  public:
    double t;
    double x;
    double w;

    DeformulaElement(double t_, double x_, double w_)
      : t(t_), x(x_), w(w_) { }

    virtual ~DeformulaElement() {}

    bool operator<(const DeformulaElement& right) const {
      return x < right.x;
    }

    bool operator>(const DeformulaElement& right) const {
      return x > right.x;
    }
  };

  std::vector<DeformulaElement> m_data;

  virtual double phi(double t) const = 0;
  virtual double phidash(double t) const = 0;

  template <class FUNCTION>
  void calcWeight(double t, FUNCTION& func);

  template <class FUNCTION>
  void calcWeight(
      std::vector<double>::iterator b,
      std::vector<double>::iterator e,
      FUNCTION& func);

  double sumw() const;

public:
  // constructor
  Deformula(double lower, double upper);
  virtual ~Deformula() {};

  template <class FUNCTION>
  void getWeight(FUNCTION& func,
                 double zero = kDeformulaZero, double reltol = kDeformulaReltol,
                 int startd = kStartd, int maxiter = kDeformulaMaxIteration);

  // getter & setter
  int getInfo() const { return m_info; }
  double getSum() const { return m_sum; }
  double getH() const { return m_h; }
  double getAbsError() const { return m_aerror; }
  double getRelError() const { return m_rerror; }
  int getIteration() const { return m_iter; }

  std::vector<double> getTValue() const;
  std::vector<double> getXValue() const;
  std::vector<double> getWValue() const;

  int getSize() const;

  template <class Iterator>
  void getTValue(Iterator first, Iterator last) const;

  template <class Iterator>
  void getXValue(Iterator first, Iterator last) const;

  template <class Iterator>
  void getWValue(Iterator first, Iterator last) const;

};

class DeformulaZeroToInf : public Deformula {
public:
  DeformulaZeroToInf();

protected:
  double phi(double t) const;
  double phidash(double t) const;
};

class DeformulaMinusOneToOne : public Deformula {
public:
  DeformulaMinusOneToOne();

protected:
  double phi(double t) const;
  double phidash(double t) const;
};

// template methods
template <class FUNCTION>
void Deformula::calcWeight(double t, FUNCTION& func) {
  double xtmp = phi(t);
  double y = Rcpp::as<double>(func(Rcpp::wrap(xtmp)));
  double wtmp = phidash(t) * y;
  if (!std::isnan(wtmp) && wtmp > m_zero) {
    if (!std::isfinite(wtmp)) {
      m_info = 2;
      return;
    }
    m_data.push_back(DeformulaElement(t, xtmp, wtmp));
  }
}

template <class FUNCTION>
void Deformula::calcWeight(
    std::vector<double>::iterator b,
    std::vector<double>::iterator e,
    FUNCTION& func) {
  for (std::vector<double>::iterator it = b; it != e; it++) {
    calcWeight(*it, func);
  }
}

template <class FUNCTION>
void Deformula::getWeight(FUNCTION& func, double zero, double reltol, int startd, int maxiter) {

  m_zero = zero;
  m_reltol = reltol;
  m_maxiter = maxiter;
  m_dstart = startd;

  int i, d;
  double v, prev;

  // 1st iteration
  m_iter = 1;
  d = m_dstart;
  m_h = (m_upper - m_lower) / d;
  std::vector<double> t(d+1);
  for (i=0, v=m_lower; i<static_cast<int>(t.size()); i++, v+=m_h) {
    t[i] = v;
  }
  calcWeight(t.begin(), t.end(), func);
  m_sum = sumw() * m_h;

  m_info = 0;
  while(1) {
    m_iter++;
    prev = m_sum;

    if (m_iter >= m_maxiter) {
      m_info = 1;
      break;
    }

    d *= 2;
    m_h /= 2.0;
    //	h = (m_upper - m_lower) / d;
    int dsize = t.size();
    for (i=0, v=m_lower; i<=d; i++, v+=m_h) {
      if (i % 2 == 1) {
        t.push_back(v);
      }
    }
    std::vector<double>::iterator bb = t.begin();
    calcWeight(bb+dsize, t.end(), func);
    //			calcWeight(std::next(t.begin(), dsize), t.end(), func);
    m_sum = sumw() * m_h;

    m_aerror = m_sum - prev;
    m_rerror = m_aerror / prev;
    if (std::abs(m_rerror) < m_reltol) {
      m_info = 0;
      break;
    }

    if (m_info == 2) {
      break;
    }
  }

  // post processing
  std::sort(m_data.begin(), m_data.end());
}

template <class Iterator>
void Deformula::getTValue(Iterator first, Iterator last) const {
  std::vector<DeformulaElement>::const_iterator it = m_data.begin();
  Iterator p = first;
  while (it != m_data.end() && p != last) {
    *p = (*it).t;
    p++;
    it++;
  }
}

template <class Iterator>
void Deformula::getXValue(Iterator first, Iterator last) const {
  std::vector<DeformulaElement>::const_iterator it = m_data.begin();
  Iterator p = first;
  while (it != m_data.end() && p != last) {
    *p = (*it).x;
    p++;
    it++;
  }
}

template <class Iterator>
void Deformula::getWValue(Iterator first, Iterator last) const {
  std::vector<DeformulaElement>::const_iterator it = m_data.begin();
  Iterator p = first;
  while (it != m_data.end() && p != last) {
    *p = (*it).w;
    p++;
    it++;
  }
}

Deformula::Deformula(double lower, double upper)
  : m_lower(lower), m_upper(upper) {}

double Deformula::sumw() const {
  double sum = 0.0;
  for (std::vector<DeformulaElement>::const_iterator it = m_data.begin(); it != m_data.end(); it++) {
    sum += (*it).w;
  }
  return sum;
}

int Deformula::getSize() const {
  return m_data.size();
}

std::vector<double> Deformula::getTValue() const {
  std::vector<double> res(getSize());
  getTValue(res.begin(), res.end());
  return res;
}

std::vector<double> Deformula::getXValue() const {
  std::vector<double> res(getSize());
  getXValue(res.begin(), res.end());
  return res;
}

std::vector<double> Deformula::getWValue() const {
  std::vector<double> res(getSize());
  getWValue(res.begin(), res.end());
  return res;
}

// Integral over (0, infinity)

DeformulaZeroToInf::DeformulaZeroToInf()
  : Deformula(-6.8, 6.8) {}

double DeformulaZeroToInf::phi(double t) const {
  return exp(pi * sinh(t) / 2.0);
}

double DeformulaZeroToInf::phidash(double t) const {
  return pi * cosh(t) * exp(pi * sinh(t) / 2.0) / 2.0;
}

// Integral over (-1, 1)

DeformulaMinusOneToOne::DeformulaMinusOneToOne()
  : Deformula(-3.0, 3.0) {}

double DeformulaMinusOneToOne::phi(double t) const {
  return tanh(pi * sinh(t) / 2.0);
}

double DeformulaMinusOneToOne::phidash(double t) const {
  return pi * cosh(t) * (1.0 / cosh(pi * sinh(t) / 2.0)) * (1.0 / cosh(pi * sinh(t) / 2.0)) / 2.0;
}
}

