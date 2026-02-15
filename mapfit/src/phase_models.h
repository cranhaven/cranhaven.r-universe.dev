#ifndef MAPFIT_MODEL_TRAITS_H
#define MAPFIT_MODEL_TRAITS_H

#include "traits.h"

template <typename V, typename I>
struct HErlang {
  V alpha;
  I shape;
  V rate;
  
  HErlang(const V& _alpha,
          const I& _shape,
          const V& _rate)
    : alpha(_alpha), shape(_shape), rate(_rate) {}

  inline int size() const {
    return vector_traits<V>::size(alpha);
  }
};

template <typename V, typename I>
struct HErlangPoi {
  V alpha;
  I shape;
  V rate;
  double omega;
  
  HErlangPoi(const V& _alpha,
          const I& _shape,
          const V& _rate,
          double _omega)
    : alpha(_alpha), shape(_shape), rate(_rate), omega(_omega) {}
  
  inline int size() const {
    return vector_traits<V>::size(alpha);
  }
};

template <typename V>
struct HErlangEres {
  double etotal;
  V eb;
  V ew;
  
  inline
    HErlangEres(const V& _eb, const V& _ew) : etotal(0), eb(_eb), ew(_ew) {}
};

template <typename T1, typename T2, typename T0>
struct GPH {
  T1 alpha;
  T2 Q;
  T2 P;
  T1 xi;
  double qv;
  T0 diag;

  GPH(const T1& _alpha,
      const T2& _Q,
      const T2& _P,
      const T1& _xi,
      double _qv,
      const T0& _diag)
    : alpha(_alpha), Q(_Q), P(_P), xi(_xi), qv(_qv), diag(_diag) {}
  
  inline int size() const {
    return vector_traits<T1>::size(alpha);
  }
};

template <typename GPHT>
struct GPHPoi {
  GPHT gph;
  double omega;
  
  GPHPoi(const GPHT& _gph, double _omega)
    : gph(_gph), omega(_omega) {}
};

template <typename T1, typename GPHT>
struct CF1 {
  T1 alpha;
  T1 rate;
  GPHT gph;

  CF1(const T1& _alpha, const T1& _rate, const GPHT& _gph)
    : alpha(_alpha), rate(_rate), gph(_gph) {}
};

template <typename V, typename M>
struct GPHEres {
  double etotal;
  V eb;
  V ey;
  V ez;
  M en;
  
  inline
    GPHEres(const V& _eb, const V& _ey, const V& _ez, const M& _en)
    : etotal(0),
      eb(_eb),
      ey(_ey),
      ez(_ez),
      en(_en) {}
};

struct GPHWorkSpace1 {
  std::vector<std::vector<double>> vf;
  std::vector<std::vector<double>> vb;
  std::vector<std::vector<double>> vc;
  
  GPHWorkSpace1(int m, int n) :
    vf(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    vb(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    vc(std::vector<std::vector<double>>(m+1, std::vector<double>(n)))
    {}
};

struct GPHWorkSpace2 {
  std::vector<std::vector<double>> barvf;
  std::vector<std::vector<double>> barvb;
  std::vector<std::vector<double>> vb;
  std::vector<std::vector<double>> vc;
  
  GPHWorkSpace2(int m, int n) :
    barvf(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    barvb(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    vb(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    vc(std::vector<std::vector<double>>(m+1, std::vector<double>(n)))
  {}
};

struct HErlangWorkSpace1 {
  std::vector<std::vector<double>> perl0;
  std::vector<std::vector<double>> perl1;
  
  HErlangWorkSpace1(int m, int n) :
    perl0(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    perl1(std::vector<std::vector<double>>(m+1, std::vector<double>(n)))
  {}
};

struct HErlangWorkSpace2 {
  std::vector<std::vector<double>> perl0;
  std::vector<std::vector<double>> perl1;
  std::vector<std::vector<double>> cerl0;
  std::vector<std::vector<double>> cerl1;
  
  HErlangWorkSpace2(int m, int n) :
    perl0(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    perl1(std::vector<std::vector<double>>(m+1, std::vector<double>(n))),
    cerl0(std::vector<std::vector<double>>(m+2, std::vector<double>(n))),
    cerl1(std::vector<std::vector<double>>(m+2, std::vector<double>(n)))
  {}
};

#endif
